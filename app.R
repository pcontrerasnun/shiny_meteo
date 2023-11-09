# TODO:
# - active style para todos scripts
# - documentar funcion YearlyPcpPlot
# - versiones librerias, rproj?
# - añadir cabecera doc al archivo app.R
# - doc MonthlyPcpPlot
# - añadir en leyenda very wet season (p80-p100), wet season (P60-P80)...
# - doc SeasonRankingPcpPlot
# - revisar torrencialidad, otoño 2023 deberia ser el max de la distrubcion
# - cajita en grafico temperatura con top 3 días mas cálidos y más frios
# - intentar separar letra gamma grafico anual
# - cajita tmp minima mas baja
# - cajita tmp maxima mas alta
# - mapa evolucion tmax a lo largo años, tmin a lo largo años, tmean a lo largo años
# - en gráfico 1 y 2 meses están en minus y en español
# - theme temperatura dominic royé
# - doc MonthlyTmeanPlot
# - leyenda dotted y solid en graficos 2 y 9
# - doc DailyRollingTmeanPlot
# - nuevo gráfico, num dias por año con mas de 30mm/dia desglosado por estación
# - poner misma escala eje y en gráficos season
# - en leyenda cumulativa monthly precip <selected_year>
# - arreglar gráfico 2 para año 2022 y todo historico
# - arreglar gráfico 10 para año 2022 y todo historico (creo q va a ser por missings)
# - en leyendas, poner circulos en vez de cuadrados
# - doc HighPcpDaysPlot
# - grafico rolling mean temp añadir label con temp media del año y en leyenda meter periodo ref
# - en gráfico pcp > 25mm si 2023 vs 1981-2010, poner 2023 junto a 2010 no a tomar por culo

library(shiny)
library(shinyjs)
library(shinythemes)
library(lubridate)
library(roxygen2)
library(here)
library(climaemet)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(ggh4x)
library(stringr)
library(mlr3misc)
library(MASS)
library(zoo)
library(ggtext)

# Code outside 'ui' and 'server' only runs once when app is launched
source(here::here("src", "data_cleaning.R"))

source(here::here("src", "plot_daily_cum_pcts_pcp.R"))
source(here::here("src", "plot_daily_cum_pcp.R"))
source(here::here("src", "plot_daily_25mm_pcp.R"))
source(here::here("src", "plot_seasonly_pcp.R"))
source(here::here("src", "plot_seasonly_ranking_pcp.R"))
source(here::here("src", "plot_monthly_pcp.R"))
source(here::here("src", "plot_monthly_ranking_pcp.R"))
source(here::here("src", "plot_seasonly_intensity_pcp.R"))
source(here::here("src", "plot_yearly_pcp.R"))

source(here::here("src", "plot_monthly_tmean.R"))
source(here::here("src", "plot_daily_rolling_tmean.R"))

# Parameters
station <- 3195
ref_start_year <- 1920
ref_end_year <- 2023
selected_year <- 2023
# aemet_api_key('eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJwY29udHJlcjk1QGdtYWlsLmNvbSIsImp0aSI6ImQ0ODYzZWIzLWRmOWQtNDg4YS04OGFmLTU2NTlmZWE3MDBkNyIsImlzcyI6IkFFTUVUIiwiaWF0IjoxNjIzMTc0ODAwLCJ1c2VySWQiOiJkNDg2M2ViMy1kZjlkLTQ4OGEtODhhZi01NjU5ZmVhNzAwZDciLCJyb2xlIjoiIn0.YTDbbuMFmA-ygIvjqwqRbCEt2JRlE9V05iYZjhmX9lA',
#              install = TRUE)

#data <- aemet_daily_period(station = station, start = ref_start_year, end = ref_end_year)
data_clean <- DataCleaning(data)[[1]]
max_date <- paste(DataCleaning(data)[[2]], "UTC")
data_pcp <- data_clean |> # Remove years with more than 50% of pcp values missing
  dplyr::group_by(year) |> dplyr::mutate(missing_pcp = mean(is.na(pcp))) |> 
  dplyr::ungroup() |> dplyr::filter(missing_pcp < 0.5) |> 
  dplyr::select(date, day, month, year, pcp)
data_temp <- data_clean |> # Remove years with more than 50% of tmean values missing
  dplyr::group_by(year) |> dplyr::mutate(missing_tmean = mean(is.na(tmean))) |> 
  dplyr::ungroup() |> dplyr::filter(missing_tmean < 0.5) |> 
  dplyr::select(date, day, month, year, tmin, tmax, tmean)

# Define UI ----
# fluidPage creates a display that automatically adjusts to the dimensions of your user’s
# browser window
ui <- shiny::fluidPage(
  theme = shinythemes::shinytheme("spacelab"),

  # Application title
  shiny::titlePanel("AEMET OpenData"),

  # Sidebar panel
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      width = 3,
      shiny::fluidRow(
        column(
          6,
          shiny::textInput(
            inputId = "year",
            label = "Year of study",
            value = lubridate::year(Sys.Date())
          )
        ),
        column(
          6,
          shiny::selectInput(
            inputId = "ref_period",
            label = "Reference period",
            choices = c(
              "1991-2020" = "1991-2020",
              "1981-2010" = "1981-2010",
              "1971-2000" = "1971-2000",
              "1961-1990" = "1961-1990",
              "1951-1980" = "1951-1980",
              "1941-1970" = "1941-1970",
              "1931-1960" = "1931-1960",
              "1921-1950" = "1921-1950",
              "All available data" = paste0(
                min(lubridate::year(data_clean$date)), "-",
                max(lubridate::year(data_clean$date))
              )
            )
          )
        )
      ),
      shiny::selectInput(
        inputId = "plot",
        label = "Metric",
        choices = c(
          "1. Cumulative daily precip. vs. percentiles" = "1",
          "2. Cumulative daily precip. vs. avg" = "2",
          "3. Number of days with more than 25mm of precip." = "3",
          "4. Seasonal precip." = "4",
          "5. Seasonal precip. (ranking)" = "5",
          "6. Monthly precip." = "6",
          "7. Monthly precip. (ranking)" = "7",
          "8. Precip. intensity" = "8",
          "9. Annual precip." = "9",
          "10. Monthly mean temp. anomalies" = "10",
          "11. Rolling daily mean temp." = "11"
        )
      ),
      shiny::actionButton(
        inputId = "updatePlot",
        label = "Paint!"
      )
    ),
    shiny::mainPanel(
      width = 9,
      shiny::plotOutput("plot", height = "800px")
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  plot <- eventReactive(input$updatePlot, { # Do not change plot until button is pressed

    # Draw plot
    switch(input$plot,
      "1" = DailyCumPcpPctsPlot(
        data = data_pcp, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "2" = DailyCumPcpPlot(
        data = data_pcp, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "3" = HighPcpDaysPlot(
        data = data_pcp, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "4" = SeasonPcpPlot(
        data = data_pcp, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "5" = SeasonRankingPcpPlot(
        data = data_pcp, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "6" = MonthlyPcpPlot(
        data = data_pcp, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "7" = MonthlyRankingPcpPlot(
        data = data_pcp, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "8" = IntensityPcpPlot(
        data = data_pcp, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "9" = YearlyPcpPlot(
        data = data_pcp, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "10" = MonthlyTmeanPlot(
        data = data_temp, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "11" = DailyRollingTmeanPlot(
        data = data_temp, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      )
    )
  })

  output$plot <- shiny::renderPlot({
    plot()
  })

}

# Run the app ----
shiny::shinyApp(ui = ui, server = server)

# shiny::runApp(display.mode="showcase") # To execute and see what chunk of code is executing

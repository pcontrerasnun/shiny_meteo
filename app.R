# TODO:
# - active style para todos scripts
# - gráfico de ranking estaciones
# - grafico lluvia anual todo histoico y recta de minimos cuadrados
# - nuevo gráfico: torrecialidad estacional evolucion para ver si está aumentando
# - documentar funcion YearlyPcpPlot
# - en gráfico anual, añadir curva distrib normal con valores media y desv tipica
# - en grafico anual, que años dentro de barras estén de mayor a menor
# - en gráfico anual, pintar en eje X desviaciones típicias
# - en gráfico estacional, añadir +- mm y +- % mm
# - cambiar todo a inglés
# - versiones librerias, rproj?
# - algunos meses siguen en español
# - add leyenda en grafico 2, linea media historica dotted y linea fija actual
# - añadir cabecera doc al archivo app.R
# - En updated poner hora UTC
# - doc MonthlyPcpPlot
# - crear un dataframe de lluvia y otro para temp y eliminar del de lluvia años 1928 y 1938?
# - añadir very wet season (p80-p100), wet season (P60-P80)...
# - tener en cuenta dato selected_year a la hora de calcular max/min in season ranking plot?
# - doc SeasonRankingPcpPlot


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

# Code outside 'ui' and 'server' only runs once when app is launched
source(here::here("src", "data_cleaning.R"))
source(here::here("src", "plot_daily_cum_pcts_pcp.R"))
source(here::here("src", "plot_daily_cum_pcp.R"))
source(here::here("src", "plot_seasonly_pcp.R"))
source(here::here("src", "plot_seasonly_ranking_pcp.R"))
source(here::here("src", "plot_monthly_pcp.R"))
source(here::here("src", "plot_monthly_ranking_pcp.R"))
source(here::here("src", "plot_seasonly_intensity_pcp.R"))
source(here::here("src", "plot_yearly_pcp.R"))

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
          "3. Seasonal precip." = "3",
          "4. Seasonal precip. (ranking)" = "4",
          "5. Monthly precip." = "5",
          "6. Monthly precip. (ranking)" = "6",
          "7. Precip. intensity" = "7",
          "8. Annual precip." = "8"
        )
      ),
      shiny::actionButton(
        inputId = "updatePlot",
        label = "Paint!"
      )
    ),
    shiny::mainPanel(
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
        data = data_clean, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "2" = DailyCumPcpPlot(
        data = data_clean, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "3" = SeasonPcpPlot(
        data = data_clean, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "4" = SeasonRankingPcpPlot(
        data = data_clean, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "5" = MonthlyPcpPlot(
        data = data_clean, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "6" = MonthlyRankingPcpPlot(
        data = data_clean, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "7" = IntensityPcpPlot(
        data = data_clean, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "8" = YearlyPcpPlot(
        data = data_clean, selected_year = input$year,
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

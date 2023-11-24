# TODO:
# - <freq> <stat> <metric>
# - nombre var plot_datas sin guiones bajos
# - vignette('ggplot2-specs')   
# - versiones librerias, rproj?
# - añadir cabecera doc al archivo app.R
# - acordarse quitar cargar funciones graficos de la parte de server
# - docmumentar funciones sin documentar
# - revisar torrencialidad, otoño 2023 deberia ser el max de la distrubcion, darle una vuelta (total lluvia / num dias con lluvia)
# - cajita en grafico temperatura con top 3 días mas cálidos y más frios
# - cajita tmp minima mas baja
# - cajita tmp maxima mas alta
# - theme temperatura dominic royé
# - en gráfico pcp > 25mm si 2023 vs 1981-2010, poner 2023 junto a 2010 no a tomar por culo
# - en algun sitio meter num total de dias con lluvia en el año, o gráfico evolutivo (tbn mensual?)
# - en gráfico torrencialidad winter, summer, spring, autumn primera letra en mayus
# - estudiar hover sobre gráfico annual mean temp/pcp anomalies para conocer temp media exacta en un año en concreto
# - n days < p40, n days > p60
# - cambiar medias por medianas
# - cambiar +/- por +/- de plotmath
# - "percentiles" por "values" en titulos?
# - gráfico 19 me chirrian p5 y p95 (daily mean temp anomalies)
# - preguntar SO por annotate box junto a leyenda
# - nuevo grafico temp perceetniles hoy extremo
# - nuevo grafico cuanto dura invierno
# - nuevo grafico overview twitter grafico dividido en 4
# - a gráficos anomalias añadir lineas desv tipicas
# - seasonly graficos para tmean
# - amplitud termicas
# - ranking año más seco?


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
library(wesanderson)

# Code outside 'ui' and 'server' only runs once when app is launched
# Load all functions
# invisible(lapply(list.files(path = here::here("src"), full.names = TRUE), source))

# Parameters
station <- 3195
ref_start_year <- 1920
ref_end_year <- 2023
selected_year <- 2023
# aemet_api_key('eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJwY29udHJlcjk1QGdtYWlsLmNvbSIsImp0aSI6ImQ0ODYzZWIzLWRmOWQtNDg4YS04OGFmLTU2NTlmZWE3MDBkNyIsImlzcyI6IkFFTUVUIiwiaWF0IjoxNjIzMTc0ODAwLCJ1c2VySWQiOiJkNDg2M2ViMy1kZjlkLTQ4OGEtODhhZi01NjU5ZmVhNzAwZDciLCJyb2xlIjoiIn0.YTDbbuMFmA-ygIvjqwqRbCEt2JRlE9V05iYZjhmX9lA',
#              install = TRUE)

# data <- aemet_daily_period(station = station, start = ref_start_year, end = ref_end_year)
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
        inputId = "variable",
        label = "Variable",
        choices = c("Mean temperature (00h-24h)", 
                    paste0("Precipitation (07h-07h", "\u207A", "\u00B9", ")"))
      ),
      shiny::selectInput(
        inputId = "plot",
        label = "Metric",
        choices = NULL
      ),
      shiny::actionButton(
        inputId = "updatePlot",
        label = "Paint!"
      )
    ),
    shiny::mainPanel(
      width = 9,
      shiny::plotOutput("plot", height = "800px", click = "plot_click"),
      shiny::verbatimTextOutput("info")
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  invisible(lapply(list.files(path = here::here("src"), full.names = TRUE), source))
  # Update plot selection depending on variable selection
  observe({
    if (input$variable == "Mean temperature (00h-24h)") {
      plot_choices <- c(
        "1. Overview" = "1",
        "2. Daily rolling mean temp." = "2-tmean",
        "3. Daily mean temp. (vs. percentiles)" = "3-tmean",
        "4. Daily mean temp. (anomalies)" = "4-tmean",
        "5. Monthly mean temp. (anomalies)" = "5-tmean",
        "6. Monthly mean temp. (historical)" = "6-tmean",
        "7. Monthly mean temp. (ranking)" = "7-tmean",
        "8. Annual mean temp. (anomalies)" = "8-tmean",
        "9. Annual mean temp. (distribution)" = "9-tmean"
      )
    } else if (input$variable == paste0("Precipitation (07h-07h", "\u207A", "\u00B9", ")")) {
      plot_choices <- c(
        "1. Overview" = "1",
        "2. Daily cumulative precip. (vs. percentiles)" = "2-pcp",
        "3. Daily cumulative precip. (vs. mean)" = "3-pcp",
        "4. Number of days with more than 25mm of precip." = "4-pcp",
        "5. Monthly precip. (vs. percentiles)" = "5-pcp",
        "6. Monthly precip. (ranking)" = "6-pcp",
        "7. Monthly precip. (historical)" = "7-pcp",
        "8. Seasonal precip. (vs. percentiles)" = "8-pcp",
        "9. Seasonal precip. (ranking)" = "9-pcp",
        "10. Seasonal precip. intensity" = "10-pcp",
        "11. Annual precip. (anomalies)" = "11-pcp",
        "12. Annual precip. (distribution)" = "12-pcp",
        "13. Annual precip. (days with precip.)" = "13-pcp"
      )
    }
    
    updateSelectInput(session, "plot", choices = plot_choices)
  })
  
  plot <- eventReactive(input$updatePlot, { # Do not change plot until button is pressed

    # Draw plot
    switch(input$plot,
      "2-pcp" = DailyCumPcpPctsPlot(
        data = data_pcp, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "3-pcp" = DailyCumPcpPlot(
        data = data_pcp, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "4-pcp" = HighPcpDaysPlot(
        data = data_pcp, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "8-pcp" = SeasonPcpPlot(
        data = data_pcp, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "9-pcp" = SeasonRankingPcpPlot(
        data = data_pcp, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "5-pcp" = MonthlyPcpPlot(
        data = data_pcp, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "6-pcp" = MonthlyRankingPcpPlot(
        data = data_pcp, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "7-pcp" = MonthlyAnomaliesPcpPlot(
        data = data_pcp,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "10-pcp" = IntensityPcpPlot(
        data = data_pcp, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "12-pcp" = AnnualPcpDistributionPlot(
        data = data_pcp, max_date = max_date
      ),
      "5-tmean" = MonthlyTmeanAnomaliesPlot(
        data = data_temp, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "2-tmean" = DailyRollingTmeanPlot(
        data = data_temp, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "1" = OverviewPcpTempPlot(
        data = data_clean, selected_year = input$year,
        max_date = max_date
      ),
      "8-tmean" = AnnualTmeanAnomaliesPlot(
        data = data_temp,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "9-tmean" = AnnualTmeanDistributionPlot(
        data = data_temp, max_date = max_date
      ),
      "11-pcp" = AnnualPcpAnomaliesPlot(
        data = data_pcp,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "6-tmean" = MonthlyHistoricalTmeanPlot(
        data = data_temp,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "3-tmean" = DailyTmeanPlot(
        data = data_temp, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "4-tmean" = DailyTmeanAnomaliesPlot(
        data = data_temp, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "13-pcp" = AnnualDaysWithPcpPlot(
        data = data_pcp, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "7-tmean" = MonthlyRankingTmeanPlot(
        data = data_temp, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      )
    )
  })

  # Display plot
  output$plot <- shiny::renderPlot({
    #plot()[[1]]
    plot()
  })
  
  # Display values
#  output$info <- shiny::renderPrint({
#    #input$plot_click
#    nearPoints(plot()[[2]], input$plot_click, maxpoints = 1, xvar = plot()[[3]], yvar = plot()[[4]])
#  })

}

# Run the app ----
shiny::shinyApp(ui = ui, server = server)

# shiny::runApp(display.mode="showcase") # To execute and see what chunk of code is executing

#' ---
#' title: Meteo app
#' author: Pablo Contreras
#' date: 2023-12-26
#' description: Shiny app to visualize meteorological data
#' ---


# TODO:
# - <freq> <stat> <metric>
# - nombre var plot_datas sin guiones bajos
# - vignette('ggplot2-specs')   
# - versiones librerias, rproj?
# - añadir cabecera doc al archivo app.R
# - acordarse quitar cargar funciones graficos de la parte de server
# - docmumentar funciones sin documentar
# - nuevo grafico cuanto dura invierno
# - a gráficos anomalias añadir lineas desv tipicas
# - renv
# - year of study not included in calculations - info message
# - reiniciar googleanalytics
# - info plot tmintmax anomalies, to see data press tmin line, not tmax
# - que hacer con 29 feb
# - doc scripts api, que ficheros generan y como lo hacen, que luego no te acuerdas
# - pensar politica borrados ficheros maquina y de logs
# - investigar porque navacerrada tarda 30 min mas que retiro
# - leyenda grafico cumtmean un poco mas a la derecha, a ppo de año hay mucha dispersion
# - cache plots
# - nuevo grafico temp y pcp doble eje
# - leyenda en graficos 5 y 6 y 8 y 9 pcp para pto navacerrada
# - investigar porque tantos decimales en data
# - meter Izaña

library(shiny, warn.conflicts = FALSE, quietly = TRUE)
library(shinyjs, warn.conflicts = FALSE, quietly = TRUE)
library(shinythemes, warn.conflicts = FALSE, quietly = TRUE)
library(lubridate, warn.conflicts = FALSE, quietly = TRUE)
library(roxygen2, warn.conflicts = FALSE, quietly = TRUE)
library(here, warn.conflicts = FALSE, quietly = TRUE)
library(climaemet, warn.conflicts = FALSE, quietly = TRUE)
library(dtplyr, warn.conflicts = FALSE, quietly = TRUE)
library(dplyr, warn.conflicts = FALSE, quietly = TRUE)
library(tidyr, warn.conflicts = FALSE, quietly = TRUE)
library(ggplot2, warn.conflicts = FALSE, quietly = TRUE)
library(ggrepel, warn.conflicts = FALSE, quietly = TRUE)
library(ggthemes, warn.conflicts = FALSE, quietly = TRUE)
library(ggh4x, warn.conflicts = FALSE, quietly = TRUE)
library(stringr, warn.conflicts = FALSE, quietly = TRUE)
library(mlr3misc, warn.conflicts = FALSE, quietly = TRUE)
library(MASS, warn.conflicts = FALSE, quietly = TRUE)
library(zoo, warn.conflicts = FALSE, quietly = TRUE)
library(ggtext, warn.conflicts = FALSE, quietly = TRUE)
library(wesanderson, warn.conflicts = FALSE, quietly = TRUE)
library(ggridges, warn.conflicts = FALSE, quietly = TRUE)
library(ggforce, warn.conflicts = FALSE, quietly = TRUE)
library(DT, warn.conflicts = FALSE, quietly = TRUE)

# Code outside 'ui' and 'server' only runs once when app is launched
# Plot dictionaries
plot_choices_overview <- c(
  "1. Overview" = "1",
  "2. Overview (2)" = "2"
)

plot_choices_tmean <- c(
  "1. Daily mean temp. (cumulative)" = "1-tmean",
  "2. Daily mean temp. (vs. percentiles)" = "2-tmean",
  "3. Daily mean temp. (anomalies)" = "3-tmean",
  "4. Daily mean temp. (heatmap) " = "4-tmean",
  "5. Monthly mean temp. (anomalies)" = "5-tmean",
  "6. Monthly mean temp. (historical)" = "6-tmean",
  "7. Monthly mean temp. (ranking)" = "7-tmean",
  "8. Seasonal mean temp. (ranking)" = "8-tmean",
  "9. Annual mean temp. (anomalies)" = "9-tmean",
  "10. Annual mean temp. (distribution)" = "10-tmean",
  "11. Annual mean temp. (density)" = "11-tmean"
)

plot_choices_tminmax <- c(
  "1. Daily min/max temp. (vs. percentiles)" = "1-tminmax",
  "2. Daily min/max temp. (anomalies)" = "2-tmintmax",
  "3. Daily max temp. (vs. percentiles)" = "1-tmax",
  "4. Daily min temp. (vs. percentiles)" = "1-tmin",
  "5. Monthly max temp. (historical)" = "2-tmax",
  "6. Monthly min temp. (historical)" = "2-tmin", 
  "7. Monthly max min temp. (historical)" = "3-tmin", 
  "8. Monthly min max temp. (historical)" = "3-tmax", 
  "9. Number of days with max temp. above 35ºC" = "4-tmax", 
  "10. Number of days with min temp. above 25ºC" = "4-tmin", 
  "11. Number of days with min temp. above 20ºC" = "5-tmin", 
  "12. Annual number of days with frost" = "6-tmin"
)

plot_choices_pcp <- c(
  "1. Daily precip. (heatmap)" = "1-pcp",
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

plot_choices_daylight <- c(
  "1. Daily gained daylight minutes" = "1-daylight",
  "2. Sunlight times" = "2-daylight"
)

# Info messages dictionary
info_messages <- list(
  "2" = "Anomaly value refers to the difference from the median. It doesn't make sense to look 
                  at this graph at the beginning of a year since the variations will be large, and 
                  possibly the year's data will be outside the chart's limits",
  "2-tmean" = p("To calculate the percentiles for each day, a time window of +- 15 days (1 month) 
                       is taken with respect to the day in question.", strong("Year of study"), "not 
                       included in percentiles calculation"),
  "3-tmean" = p("To calculate the percentiles for each day, a time window of +- 15 days (1 month) 
                       is taken with respect to the day in question.", strong("Year of study"), "not 
                       included in percentiles calculation"),
  "4-tmean" = "Percentiles are calculated using the empirical (observed) distribution, without 
                        fitting the series to a normal distribution",
  "7-tmean" = p("Ranking is calculated only with years included in", strong("Reference period")),
  "8-tmean" = p("Ranking is calculated only with years included in ", strong("Reference period."), 
                 "Anomaly labels refers to the difference from the median"),
  "10-tmean" = "Years at the top of a bar are hotter than the ones at the bottom",
  "6-pcp" = p("Ranking is calculated only with years included in", strong("Reference period")),
  "9-pcp" = p("Ranking is calculated only with years included in", strong("Reference period")),
  "10-pcp" = "Intensity is calculated as total precipitation in season divided by total days
                      with precipitation in season",
  "11-pcp" = "Anomaly labels refers to the difference from the median",
  "12-pcp" = "Years at the top of a bar have more precipitation than the ones at the bottom",
  "No extra info provided",
  "1-tminmax" = p("To calculate the percentiles for each day, a time window of +- 15 days (1 month) 
                         is taken with respect to the day in question.", strong("Year of study"), "not 
                         included in percentiles calculation. To see data, click on daily max temperature"),
  "3-tmin" = "Frost: when minimum temperature of the day is below 0ºC",
  "2-tmintmax" = "Red labels refer to max and min anomalies for max temperature. Blue labels refer
                          to min temperature"
)

# Stations dictionary
stations_dict <- list(
  "3195" = list(title = "Madrid - Retiro", mun_code = 28079),
  "3129" = list(title = "Madrid - Aeropuerto", mun_code = 28079),
  "2462" = list(title = "Madrid - Pto Navacerrada", mun_code = 28093),
  "C430E" = list(title = "Sta Cruz de Tenerife - Izaña", mun_code = 38026)
)

# Load all functions
invisible(lapply(list.files(path = here::here("src"), full.names = TRUE), source))
invisible(lapply(list.files(path = here::here("helpers"), full.names = TRUE), source))

# Load AEMET OpenData API key
Sys.getenv("AEMET_API_KEY")

# Define UI ----
# fluidPage creates a display that automatically adjusts to the dimensions of your user’s
# browser window
ui <- shiny::fluidPage(
  # Google Analytics
  tags$head(includeHTML("google-analytics.html")),
  
  theme = shinythemes::shinytheme("spacelab"),

  # Application title
  shiny::titlePanel("Meteo app"),
  
  # Position of loading notification
  tags$head(tags$style(".shiny-notification {position: fixed; top: 20% ;left: 50%; width: 420px")),

  # Sidebar panel
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      width = 3,
      shiny::fluidRow(
        column(
          width = 12,
          shiny::selectInput(
            inputId = "station_id",
            label = "Station",
            choices = c(
              "Madrid - Retiro" = "3195",
              "Madrid - Pto Navacerrada" = "2462",
              "Madrid - Aeropuerto" = "3129",
              "Sta Cruz de Tenerife - Izaña" = "C430E"
            ),
            selected = "3195"
          )
        ),
        column(
          width = 6,
          shiny::textInput(
            inputId = "year",
            label = "Year of study",
            value = lubridate::year(Sys.Date())
          )
        ),
        column(
          width = 6,
          shiny::selectInput(
            inputId = "ref_period",
            label = "Reference period",
            choices = NULL
          )
        )
      ),
      shiny::selectInput(
        inputId = "variable",
        label = "Variable",
        choices = c("Temperature & precipiation overviews",
                    "Mean temperature (00h-24h)", 
                    "Min/max temperatures (00h-24h)", 
                    paste0("Precipitation (07h-07h", "\u207A", "\u00B9", ")"),
                    "Sunlight")
      ),
      shiny::selectInput(
        inputId = "plot",
        label = "Metric",
        choices = NULL
      ),
      shiny::fluidRow(
        column(
          width = 2,
          shiny::actionButton(
            inputId = "updatePlot",
            label = "Paint!"
          )
        ),
        column(
          width = 2,
          shiny::actionButton(
            inputId = "nextPlot",
            label = "Next"
          )
        ),
        div(
          style = "position:relative; left:calc(54%);",
          shiny::actionButton(
            inputId = 'info', 
            label = NULL, 
            icon = icon('info-circle'))
        )
      )
    ),
    shiny::mainPanel(
      width = 9,
      # Show plot
      shiny::plotOutput("plot", height = "800px", click = "plot_click"),
      # Show data
      DT::dataTableOutput("info") # shiny::verbatimTextOutput
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  # Update data depending on station selection
  newData <- shiny::reactive({
    withProgress(message = "Loading data...", value = 0, {
      n <- 2
      
      data_clean <- FetchAEMETData(station = input$station_id)
      incProgress(1/n)
      data_temp <- CleanTempData(data = data_clean[[1]])
      data_pcp <- CleanPcpData(data = data_clean[[1]])
      max_date <- data_clean[[2]]
      data_forecast <- FetchForecastData(mun_code = stations_dict[[input$station_id]]$mun_code)
      data_sunlight <- FetchSunlightData(station = input$station_id)
      incProgress(2/n)
      
      return(list(data_temp, data_pcp, max_date, data_forecast, data_sunlight, data_clean))
    })
  }) %>%
    shiny::bindCache(input$station_id) # Cache data
  
  # Update reference period selection choices depending on data
  shiny::observe({
    min_year <- min(newData()[[6]][[1]]$year)
    max_year <- max(newData()[[6]][[1]]$year)
    
    # ROund min_year and max_year to nearest decena
    min_year_floor <- floor(min_year / 10) * 10
    max_year_floor <- floor(max_year / 10) * 10

    years <- seq(min_year_floor, max_year_floor, by = 10)
  
    grupos <- list()
    for (i in 1:(length(years) - 3)) {
      # Get first and last year of group
      year_ini <- years[i]
      year_fin <- years[i + 3]
      nombre <- paste0(year_ini + 1, "-", year_fin)
      grupos[[nombre]] <- nombre
    }
    # Add "All available data" group
    grupos[["All available data"]] <- paste0(min_year, "-", max_year)
    # Transform list to vector
    grupos <- unlist(grupos)
    
    updateSelectInput(session, "ref_period", choices = grupos, selected = grupos[-1])
  })
  
  # Update plot selection choices depending on variable (tmean, pcp...) selection
  shiny::observe({
    if (input$variable == "Mean temperature (00h-24h)") {
      plot_choices <- plot_choices_tmean
    } else if (input$variable == paste0("Precipitation (07h-07h", "\u207A", "\u00B9", ")")) {
      plot_choices <- plot_choices_pcp
    } else if (input$variable == "Min/max temperatures (00h-24h)") {
      plot_choices <- plot_choices_tminmax
    } else if (input$variable == "Sunlight") {
      plot_choices <- plot_choices_daylight
    } else if (input$variable == "Temperature & precipiation overviews") {
      plot_choices <- plot_choices_overview
    }
    
    updateSelectInput(session, "plot", choices = plot_choices)
  })
  
  # Configure "Next" button
  shiny::observeEvent(input$nextPlot, {
    if (input$variable == "Mean temperature (00h-24h)") {
      plot_choices <- plot_choices_tmean
    } else if (input$variable == paste0("Precipitation (07h-07h", "\u207A", "\u00B9", ")")) {
      plot_choices <- plot_choices_pcp
    } else if (input$variable == "Min/max temperatures (00h-24h)") {
      plot_choices <- plot_choices_tminmax
    } else if (input$variable == "Sunlight") {
      plot_choices <- plot_choices_daylight
    } else if (input$variable == "Temperature & precipiation overviews") {
      plot_choices <- plot_choices_overview
    }
    current_index <- match(input$plot, plot_choices)
    
    # Find next index within choices
    next_index <- current_index %% length(plot_choices) + 1
    current_plot_index <- current_index %% length(plot_choices) + 1
    
    # Establish next plot in the list
    updateSelectInput(session, "plot", selected = plot_choices[next_index][[1]])
  })
  
  # Configure "info" message depending on selected plot
  info_message <- shiny::reactive({
    #switch(input$plot, info_messages)
    do.call(switch, c(list(input$plot), info_messages))
  })
  
  # Configure "info" button
  shiny::observeEvent(input$info, {
    shiny::showModal(shiny::modalDialog(
      title = "Help!",
      h4(strong('Using the app')),
      p("Every time", strong("Year of study"), " is changed, ", strong("Paint!"), 
        " or ", strong("Next"), " button need to be pressed. No need when changing ",
        strong("Reference period"), " or ", strong("Metric"), " or ", strong("Variable"),
        " - chart will update automatically. Data is shown below the chart 
        after clicking on the chart"),
      br(), h4(strong('Chart info')),
      p(info_message()),
      easyClose = TRUE,
      footer = modalButton("Capito")
    ))
  })
  
  # Do not change plot until "Paint!" or "Next" button is pressed
  plot <- shiny::eventReactive({input$updatePlot
                         input$nextPlot
                         input$ref_period
                         input$plot
                         input$station_id}, ignoreInit = TRUE, { 
    
    withProgress(message = "Drawing plot...", value = 0, {
      incProgress(1/2) 
      
      # Draw plot
      switch(input$plot,
        "1" = OverviewPcpTempPlot(
           data_temp = newData()[[1]], data_pcp = newData()[[2]], selected_year = input$year,
           max_date = newData()[[3]], title = stations_dict[[input$station_id]]$title
        ),
        "2-pcp" = DailyCumPcpPctsPlot(
          data = newData()[[2]], selected_year = input$year,
          ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
          ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
          max_date = newData()[[3]], title = stations_dict[[input$station_id]]$title
        ),
        "3-pcp" = DailyCumPcpPlot(
          data = newData()[[2]], selected_year = input$year,
          ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
          ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
          max_date = newData()[[3]], title = stations_dict[[input$station_id]]$title
        ),
        "4-pcp" = HighPcpDaysPlot(
          data = newData()[[2]],
          ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
          ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
          max_date = newData()[[3]], title = stations_dict[[input$station_id]]$title
        ),
        "8-pcp" = SeasonPcpPlot(
          data = newData()[[2]], selected_year = input$year,
          ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
          ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
          max_date = newData()[[3]], title = stations_dict[[input$station_id]]$title
        ),
        "9-pcp" = SeasonRankingPcpPlot(
          data = newData()[[2]], selected_year = input$year,
          ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
          ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
          max_date = newData()[[3]], title = stations_dict[[input$station_id]]$title
        ),
        "5-pcp" = MonthlyPcpPlot(
          data = newData()[[2]], selected_year = input$year,
          ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
          ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
          max_date = newData()[[3]], title = stations_dict[[input$station_id]]$title
        ),
        "6-pcp" = MonthlyRankingPcpPlot(
          data = newData()[[2]], selected_year = input$year,
          ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
          ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
          max_date = newData()[[3]], title = stations_dict[[input$station_id]]$title
        ),
        "7-pcp" = MonthlyAnomaliesPcpPlot(
          data = newData()[[2]],
          ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
          ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
          max_date = newData()[[3]], title = stations_dict[[input$station_id]]$title
        ),
        "10-pcp" = IntensityPcpPlot(
          data = newData()[[2]], selected_year = input$year,
          ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
          ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
          max_date = newData()[[3]], title = stations_dict[[input$station_id]]$title
        ),
        "12-pcp" = AnnualPcpDistributionPlot(
          data = newData()[[2]], max_date = newData()[[3]], title = stations_dict[[input$station_id]]$title
        ),
        "5-tmean" = MonthlyTmeanAnomaliesPlot(
          data = newData()[[1]], selected_year = input$year,
          ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
          ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
          max_date = newData()[[3]], title = stations_dict[[input$station_id]]$title
        ),
        "1-tmean" = DailyCumulativeTmeanPlot(
          data = newData()[[1]], selected_year = input$year,
          ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
          ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
          max_date = newData()[[3]], title = stations_dict[[input$station_id]]$title
        ),
        "9-tmean" = AnnualTmeanAnomaliesPlot(
          data = newData()[[1]],
          ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
          ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
          max_date = newData()[[3]], title = stations_dict[[input$station_id]]$title
        ),
        "10-tmean" = AnnualTmeanDistributionPlot(
          data = newData()[[1]], max_date = newData()[[3]], title = stations_dict[[input$station_id]]$title
        ),
        "11-pcp" = AnnualPcpAnomaliesPlot(
          data = newData()[[2]],
          ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
          ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
          max_date = newData()[[3]], title = stations_dict[[input$station_id]]$title
        ),
        "6-tmean" = MonthlyHistoricalTmeanPlot(
          data = newData()[[1]],
          ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
          ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
          max_date = newData()[[3]], title = stations_dict[[input$station_id]]$title
        ),
        "2-tmean" = DailyTmeanPlot(
          data = newData()[[1]], data_forecast = newData()[[4]], selected_year = input$year,
          ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
          ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
          max_date = newData()[[3]], title = stations_dict[[input$station_id]]$title
        ),
        "3-tmean" = DailyTmeanAnomaliesPlot(
          data = newData()[[1]], selected_year = input$year,
          ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
          ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
          max_date = newData()[[3]], title = stations_dict[[input$station_id]]$title
        ),
        "13-pcp" = AnnualDaysWithPcpPlot(
          data = newData()[[2]], selected_year = input$year,
          ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
          ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
          max_date = newData()[[3]], title = stations_dict[[input$station_id]]$title
        ),
        "7-tmean" = MonthlyRankingTmeanPlot(
          data = newData()[[1]], selected_year = input$year,
          ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
          ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
          max_date = newData()[[3]], title = stations_dict[[input$station_id]]$title
        ),
        "4-tmean" = DailyHeatmapTmeanPlot(
          data = newData()[[1]], selected_year = input$year,
          ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
          ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
          max_date = newData()[[3]], title = stations_dict[[input$station_id]]$title
        ),
        "8-tmean" = SeasonRankingTmeanPlot(
          data = newData()[[1]], selected_year = input$year,
          ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
          ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
          max_date = newData()[[3]], title = stations_dict[[input$station_id]]$title
        ),
        "2" = OverviewPcpTempPlot2(
          data_temp = newData()[[1]], data_pcp = newData()[[2]], selected_year = input$year,
          ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
          ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
          max_date = newData()[[3]], title = stations_dict[[input$station_id]]$title
        ),
        "11-tmean" = DensityTmeanPlot(
          data = newData()[[1]], selected_year = input$year,
          ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
          ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
          max_date = newData()[[3]], title = stations_dict[[input$station_id]]$title
        ),
        "1-tminmax" = DailyTminTmaxPlot(
          data = newData()[[1]], data_forecast = newData()[[4]], selected_year = input$year,
          ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
          ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
          max_date = newData()[[3]], title = stations_dict[[input$station_id]]$title
        ),
        "6-tmin" = YearlyFrostDaysPlot(
          data = newData()[[1]],
          ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
          ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
          max_date = newData()[[3]], title = stations_dict[[input$station_id]]$title
        ),
        "2-tmax" = MonthlyHistoricalTmaxPlot(
          data = newData()[[1]],
          ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
          ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
          max_date = newData()[[3]], title = stations_dict[[input$station_id]]$title
        ),
        "4-tmax" = HighTmaxDaysPlot(
          data = newData()[[1]], selected_year = input$year,
          ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
          ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
          max_date = newData()[[3]], title = stations_dict[[input$station_id]]$title
        ),
        "2-tmintmax" = DailyTminTmaxAnomaliesPlot(
          data = newData()[[1]], selected_year = input$year,
          ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
          ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
          max_date = newData()[[3]], title = stations_dict[[input$station_id]]$title
        ),
        "1-daylight" = DailyDaylightGainedPlot(
          data = newData()[[5]], selected_year = input$year,
          max_date = newData()[[3]], title = stations_dict[[input$station_id]]$title
        ),
        "2-daylight" = DailySunlightTimesPlot(
          data = newData()[[5]], selected_year = input$year,
          max_date = newData()[[3]], title = stations_dict[[input$station_id]]$title
        ),
        "4-tmin" = EcuatorialNightsPlot(
          data = newData()[[1]], selected_year = input$year,
          ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
          ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
          max_date = newData()[[3]], title = stations_dict[[input$station_id]]$title
        ),
        "5-tmin" = TropicalNightsPlot(
          data = newData()[[1]], selected_year = input$year,
          ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
          ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
          max_date = newData()[[3]], title = stations_dict[[input$station_id]]$title
        ),
        "2-tmin" = MonthlyHistoricalTminPlot(
          data = newData()[[1]],
          ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
          ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
          max_date = newData()[[3]], title = stations_dict[[input$station_id]]$title
        ),
        "3-tmin" = MonthlyHistoricalMaxTminPlot(
          data = newData()[[1]],
          ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
          ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
          max_date = newData()[[3]], title = stations_dict[[input$station_id]]$title
        ),
        "3-tmax" = MonthlyHistoricalMinTmaxPlot(
          data = newData()[[1]],
          ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
          ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
          max_date = newData()[[3]], title = stations_dict[[input$station_id]]$title
        ),
        "1-pcp" = DailyHeatmapPcpPlot(
          data = newData()[[2]], selected_year = input$year,
          ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
          ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
          max_date = newData()[[3]], title = stations_dict[[input$station_id]]$title
        ),
        "1-tmax" = DailyTmaxPlot(
          data = newData()[[1]], data_forecast = newData()[[4]], selected_year = input$year,
          ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
          ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
          max_date = newData()[[3]], title = stations_dict[[input$station_id]]$title
        ),
        "1-tmin" = DailyTminPlot(
          data = newData()[[1]], data_forecast = newData()[[4]], selected_year = input$year,
          ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
          ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
          max_date = newData()[[3]], title = stations_dict[[input$station_id]]$title
        )
      )
    })
  }) 

  # Display plot
  output$plot <- shiny::renderPlot({ # renderCachedPlot
      plot()[[1]]
  } # ,
#  cacheKeyExpr = {
#    list(input$year, input$variable, input$ref_period, input$plot, input$station_id)
#    }
  )
  
  # Display values/data
  output$info <- DT::renderDataTable({ # shiny::renderPrint
    datatable((nearPoints(plot()[[2]], input$plot_click, maxpoints = 1, threshold = 25,
               xvar = plot()[[3]], yvar = plot()[[4]])), options = list(dom = 't'))
  })

}

# Run the app ----
shiny::shinyApp(ui = ui, server = server)

# To execute and see what chunk of code is executing
#shiny::shinyApp(ui = ui, server = server, options = list(display.mode = 'showcase')) 


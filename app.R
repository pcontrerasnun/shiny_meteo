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
# - amplitud termicas
# - renv
# - calendar map con lluvia en cada día
# - year of study not included in calculations - info message
# - reiniciar googleanalytics
# - añadir en caption direccion pagweb
# - info plot tmintmax anomalies, to see data press tmin line, not tmax
# - que hacer con 29 feb
# - ranking maxtmean y mintmean date
# - forecast cuando eliges año diff al actual
# - decimales diffdayduration
# - linea vertical cuando año diff al actual diffdayduration
# - date with max pcp in selected year 

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
plot_choices_tmean <- c(
  "1. Overview" = "1",
  "2. Overview (2)" = "2",
  "3. Daily mean temp. (cumulative)" = "3-tmean",
  "4. Daily mean temp. (vs. percentiles)" = "4-tmean",
  "5. Daily mean temp. (anomalies)" = "5-tmean",
  "6. Daily mean temp. (heatmap) " = "6-tmean",
  "7. Monthly mean temp. (anomalies)" = "7-tmean",
  "8. Monthly mean temp. (historical)" = "8-tmean",
  "9. Monthly mean temp. (ranking)" = "9-tmean",
  "10. Seasonal mean temp. (ranking)" = "10-tmean",
  "11. Annual mean temp. (anomalies)" = "11-tmean",
  "12. Annual mean temp. (distribution)" = "12-tmean",
  "13. Annual mean temp. (density)" = "13-tmean"
)

plot_choices_tminmax <- c(
  "1. Daily min/max temp. (vs. percentiles)" = "1-tminmax",
  "2. Daily min/max temp. (anomalies)" = "2-tmintmax",
  "3. Annual number of days with frost" = "3-tmin",
  "4. Monthly max temp. (historical)" = "4-tmax",
  "5. Monthly min temp. (historical)" = "5-tmin",
  "6. Monthly max min temp. (historical)" = "6-tmin",
  "7. Monthly min max temp. (historical)" = "7-tmax",
  "8. Number of days with max temp. above 35ºC" = "8-tmax",
  "9. Number of days with min temp. above 25ºC" = "9-tmin",
  "10. Number of days with min temp. above 20ºC" = "10-tmin"
)

plot_choices_pcp <- c(
  "1. Overview" = "1",
  "2. Overview (2)" = "2",
  "3. Daily precip. (heatmap)" = "3-pcp",
  "4. Daily cumulative precip. (vs. percentiles)" = "4-pcp",
  "5. Daily cumulative precip. (vs. mean)" = "5-pcp",
  "6. Number of days with more than 25mm of precip." = "6-pcp",
  "7. Monthly precip. (vs. percentiles)" = "7-pcp",
  "8. Monthly precip. (ranking)" = "8-pcp",
  "9. Monthly precip. (historical)" = "9-pcp",
  "10. Seasonal precip. (vs. percentiles)" = "10-pcp",
  "11. Seasonal precip. (ranking)" = "11-pcp",
  "12. Seasonal precip. intensity" = "12-pcp",
  "13. Annual precip. (anomalies)" = "13-pcp",
  "14. Annual precip. (distribution)" = "14-pcp",
  "15. Annual precip. (days with precip.)" = "15-pcp"
)

plot_choices_daylight <- c(
  "1. Daily gained daylight minutes" = "1-daylight",
  "2. Sunlight times" = "2-daylight"
)

# Load all functions
# invisible(lapply(list.files(path = here::here("src"), full.names = TRUE), source))

# Parameters
station <- 3195
mun_code <- 28079
ref_start_year <- 1920
ref_end_year <- 2023
selected_year <- 2024

# Get sunlight times from Dropbox
search <- rdrop2::drop_search("sunlighttimes")
file <- search$matches[[1]]$metadata$path_lower # Get last historical file
print(paste0("Downloading from Dropbox sunlight data for station ", station, ": ", file))
data_sunlight <- rdrop2::drop_read_csv(
  file, colClasses = c(date = "Date", sunrise = "POSIXct", sunset = "POSIXct", solarNoon = "POSIXct",
                       dawn = "POSIXct", dusk = "POSIXct"))

# Get historical data from Dropbox
search <- rdrop2::drop_search("complete")
file <- search$matches[[1]]$metadata$path_lower # Get last historical file
print(paste0("Downloading from Dropbox historical data for station ", station, ": ", file))
data_clean <- rdrop2::drop_read_csv(
  file, colClasses = c(day = "character", month = "character", date = "Date"))

# Calculate datetime last data based on file name
max_date <- paste(format(strptime(sub(".*/(\\d{8}_\\d{6}).*", "\\1", file), format = "%Y%m%d_%H%M%S"),
                         "%Y-%m-%d %H:%M"), "UTC")

# Get forecast data
forecast <- climaemet::aemet_forecast_daily(mun_code)

# Clean data
data_pcp <- data_clean |> # Remove years with more than 50% of pcp values missing
  dplyr::group_by(year) |> dplyr::mutate(missing_pcp = mean(is.na(pcp))) |> 
  dplyr::ungroup() |> dplyr::filter(missing_pcp < 0.5) |> 
  dplyr::select(date, day, month, year, pcp)

data_temp <- data_clean |> # Remove years with more than 50% of tmean values missing
  dplyr::group_by(year) |> dplyr::mutate(missing_tmean = mean(is.na(tmean))) |> 
  dplyr::ungroup() |> dplyr::filter(missing_tmean < 0.5) |> 
  dplyr::select(date, day, month, year, tmin, tmax, tmean)

data_forecast <- climaemet::aemet_forecast_tidy(forecast, "temperatura") |> 
  dplyr::rename(tmax = temperatura_maxima, tmin = temperatura_minima, date = fecha) |> 
  dplyr::mutate(tmean = (tmax + tmin)/2) |> 
  dplyr::select(date, tmin, tmax, tmean)

# Define UI ----
# fluidPage creates a display that automatically adjusts to the dimensions of your user’s
# browser window
ui <- shiny::fluidPage(
  # Google Analytics
  tags$head(includeHTML("google-analytics.html")),
  
  theme = shinythemes::shinytheme("spacelab"),

  # Application title
  shiny::titlePanel("Meteo app"),

  # Sidebar panel
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      width = 3,
      shiny::fluidRow(
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
            ),
            selected = "1981-2010"
          )
        )
      ),
      shiny::selectInput(
        inputId = "variable",
        label = "Variable",
        choices = c("Mean temperature (00h-24h)", 
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
  invisible(lapply(list.files(path = here::here("src"), full.names = TRUE), source))
  
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
    switch(input$plot,
           "2" = "Anomaly value refers to the difference from the median. It doesn't make sense to look 
                  at this graph at the beginning of a year since the variations will be large, and 
                  possibly the year's data will be outside the chart's limits",
           "4-tmean" = p("To calculate the percentiles for each day, a time window of +- 15 days (1 month) 
                       is taken with respect to the day in question.", strong("Year of study"), "not 
                       included in percentiles calculation"),
           "5-tmean" = p("To calculate the percentiles for each day, a time window of +- 15 days (1 month) 
                       is taken with respect to the day in question.", strong("Year of study"), "not 
                       included in percentiles calculation"),
           "6-tmean" = "Percentiles are calculated using the empirical (observed) distribution, without 
                        fitting the series to a normal distribution",
           "9-tmean" = p("Ranking is calculated only with years included in", strong("Reference period")),
           "10-tmean" = p("Ranking is calculated only with years included in ", strong("Reference period."), 
                        "Anomaly labels refers to the difference from the median"),
           "12-tmean" = "Years at the top of a bar are hotter than the ones at the bottom",
           "8-pcp" = p("Ranking is calculated only with years included in", strong("Reference period")),
           "11-pcp" = p("Ranking is calculated only with years included in", strong("Reference period")),
           "12-pcp" = "Intensity is calculated as total precipitation in season divided by total days
                      with precipitation in season",
           "13-pcp" = "Anomaly labels refers to the difference from the median",
           "14-pcp" = "Years at the top of a bar have more precipitation than the ones at the bottom",
                      "No extra info provided",
           "1-tminmax" = p("To calculate the percentiles for each day, a time window of +- 15 days (1 month) 
                         is taken with respect to the day in question.", strong("Year of study"), "not 
                         included in percentiles calculation. To see data, click on daily max temperature"),
           "3-tmin" = "Frost: when minimum temperature of the day is below 0ºC"
    )
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
                         input$plot}, ignoreInit = TRUE, { 
    # Draw plot
    switch(input$plot,
      "4-pcp" = DailyCumPcpPctsPlot(
        data = data_pcp, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "5-pcp" = DailyCumPcpPlot(
        data = data_pcp, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "6-pcp" = HighPcpDaysPlot(
        data = data_pcp, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "10-pcp" = SeasonPcpPlot(
        data = data_pcp, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "11-pcp" = SeasonRankingPcpPlot(
        data = data_pcp, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "7-pcp" = MonthlyPcpPlot(
        data = data_pcp, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "8-pcp" = MonthlyRankingPcpPlot(
        data = data_pcp, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "9-pcp" = MonthlyAnomaliesPcpPlot(
        data = data_pcp,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "12-pcp" = IntensityPcpPlot(
        data = data_pcp, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "14-pcp" = AnnualPcpDistributionPlot(
        data = data_pcp, max_date = max_date
      ),
      "7-tmean" = MonthlyTmeanAnomaliesPlot(
        data = data_temp, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "3-tmean" = DailyCumulativeTmeanPlot(
        data = data_temp, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "1" = OverviewPcpTempPlot(
        data_temp = data_temp, data_pcp = data_pcp, selected_year = input$year,
        max_date = max_date
      ),
      "11-tmean" = AnnualTmeanAnomaliesPlot(
        data = data_temp,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "12-tmean" = AnnualTmeanDistributionPlot(
        data = data_temp, max_date = max_date
      ),
      "13-pcp" = AnnualPcpAnomaliesPlot(
        data = data_pcp,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "8-tmean" = MonthlyHistoricalTmeanPlot(
        data = data_temp,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "4-tmean" = DailyTmeanPlot(
        data = data_temp, data_forecast = data_forecast, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "5-tmean" = DailyTmeanAnomaliesPlot(
        data = data_temp, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "15-pcp" = AnnualDaysWithPcpPlot(
        data = data_pcp, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "9-tmean" = MonthlyRankingTmeanPlot(
        data = data_temp, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "6-tmean" = DailyHeatmapTmeanPlot(
        data = data_temp, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "10-tmean" = SeasonRankingTmeanPlot(
        data = data_temp, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "2" = OverviewPcpTempPlot2(
        data_temp = data_temp, data_pcp = data_pcp, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "13-tmean" = DensityTmeanPlot(
        data = data_temp, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "1-tminmax" = DailyTminTmaxPlot(
        data = data_temp, data_forecast = data_forecast, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "3-tmin" = YearlyFrostDaysPlot(
        data = data_temp,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "4-tmax" = MonthlyHistoricalTmaxPlot(
        data = data_temp,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "8-tmax" = HighTmaxDaysPlot(
        data = data_temp, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "2-tmintmax" = DailyTminTmaxAnomaliesPlot(
        data = data_temp, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "1-daylight" = DailyDaylightGainedPlot(
        data = data_sunlight, selected_year = input$year,
        max_date = max_date
      ),
      "2-daylight" = DailySunlightTimesPlot(
        data = data_sunlight, selected_year = input$year,
        max_date = max_date
      ),
      "9-tmin" = EcuatorialNightsPlot(
        data = data_temp, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "10-tmin" = TropicalNightsPlot(
        data = data_temp, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "5-tmin" = MonthlyHistoricalTminPlot(
        data = data_temp,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "6-tmin" = MonthlyHistoricalMaxTminPlot(
        data = data_temp,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "7-tmax" = MonthlyHistoricalMinTmaxPlot(
        data = data_temp,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "3-pcp" = DailyHeatmapPcpPlot(
        data = data_pcp, selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      )
    )
  })

  # Display plot
  output$plot <- shiny::renderPlot({
    plot()[[1]]
    #plot()
  })
  
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


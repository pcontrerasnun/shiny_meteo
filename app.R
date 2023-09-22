# TODO:
# - poner bien caption fecha dato actualizado
# - grafico percentiles histórico se sale de margen etiqueta de max
# - documentar funcion PlotCumPcpPcts
# - documentar funcion PlotCumPcp
# - revisar expand grafico 1, mucho espacio en blanco arriba
# - comentar el codigo de plot_season_pcp.R porque tiene telita
# - documentar funcion SeasonPcpPlot
# - salto de linea caption grafico interactivo
# - docu max_date plot_cum_pcp_pcts

library(shiny)
library(shinythemes)
library(lubridate)
library(roxygen2)
library(here)
library(climaemet)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(ggh4x)
library(stringr)

# Code outside 'ui' and 'server' only runs once when app is launched
source(here::here("src", "data_cleaning.R"))
source(here::here("src", "plot_cum_pcp_pcts.R"))
source(here::here("src", "plot_cum_pcp.R"))
source(here::here("src", "plot_season_pcp.R"))

# Parameters
station <- 3195
ref_start_year <- 1920
ref_end_year <- 2023
selected_year <- 2023
# aemet_api_key('eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJwY29udHJlcjk1QGdtYWlsLmNvbSIsImp0aSI6ImQ0ODYzZWIzLWRmOWQtNDg4YS04OGFmLTU2NTlmZWE3MDBkNyIsImlzcyI6IkFFTUVUIiwiaWF0IjoxNjIzMTc0ODAwLCJ1c2VySWQiOiJkNDg2M2ViMy1kZjlkLTQ4OGEtODhhZi01NjU5ZmVhNzAwZDciLCJyb2xlIjoiIn0.YTDbbuMFmA-ygIvjqwqRbCEt2JRlE9V05iYZjhmX9lA',
#              install = TRUE)

# data <- aemet_daily_period(station = station, start = ref_start_year, end = ref_end_year)
max_date <- max(data$fecha)

# Define UI ----
# fluidPage creates a display that automatically adjusts to the dimensions of your user’s
# browser window
ui <- shiny::fluidPage(
  theme = shinythemes::shinytheme("spacelab"),

  # Application title
  shiny::titlePanel("AEMET Open Data"),

  # Sidebar panel
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::fluidRow(
        column(
          6,
          shiny::textInput(
            inputId = "year",
            label = "Año de estudio",
            value = lubridate::year(Sys.Date())
          )
        ),
        column(
          6,
          shiny::selectInput(
            inputId = "ref_period",
            label = "Periodo de referencia",
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
                min(lubridate::year(data$fecha)), "-",
                max(lubridate::year(data$fecha))
              )
            )
          )
        )
      ),
      shiny::selectInput(
        inputId = "plot",
        label = "Gráfico",
        choices = c(
          "1. Precip. diaria acumulada" = "1",
          "2. Precip. diaria acumulada (interactivo)" = "2",
          "3. Precip. estacional" = "3"
        )
      )
    ),
    shiny::mainPanel(
      shiny::plotOutput("plot", height = "800px")
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  output$plot <- shiny::renderPlot({
    # Get plot selected by user
    type <- input$plot

    # Draw plot
    switch(type,
      "1" = CumPcpPctsPlot(
        data = DataCleaning(data), selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "2" = CumPcpPlot(
        data = DataCleaning(data), selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      ),
      "3" = SeasonPcpPlot(
        data = DataCleaning(data), selected_year = input$year,
        ref_start_year = as.numeric(strsplit(input$ref_period, "-")[[1]][1]),
        ref_end_year = as.numeric(strsplit(input$ref_period, "-")[[1]][2]),
        max_date = max_date
      )
    )
  })
}

# Run the app ----
shiny::shinyApp(ui = ui, server = server)

# shiny::runApp(display.mode="showcase") # To execute and see what chunk of code is executing

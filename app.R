library(shiny)
library(shinythemes)
library(lubridate)
library(roxygen2)

# Define UI ----
ui <- fluidPage( # fluidPage creates a display that automatically adjusts to the dimensions 
  # of your user’s browser window
  
  theme = shinytheme("spacelab"),
  
  # Application title
  titlePanel("AEMET Open Data"),
  
  # Sidebar panel
  sidebarLayout(
    sidebarPanel(
      textInput("date", 
                h3("Year input"), 
                value = year(Sys.Date()))
    ),
    mainPanel("main panel")
  )
)

# Define server logic ----
server <- function(input, output) { 
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
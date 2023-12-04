
library(shiny)

plot_choices_temp <- c(
  "1. Overview" = "1. Overview",
  "2. Overview (2)" = "2. Overview (2)",
  "3. Daily cumulative mean temp." = "3. Daily cumulative mean temp."
)

plot_choices_pcp <- c(
  "1. Daily cumulative precip. (vs. percentiles)" = "1. Daily cumulative precip. (vs. percentiles)",
  "2. Daily cumulative precip. (vs. mean)" = "2. Daily cumulative precip. (vs. mean)",
  "3. Number of days with more than 25mm of precip." = "3. Number of days with more than 25mm of precip."
)

ui <- shiny::fluidPage(
  theme = shinythemes::shinytheme("spacelab"),
  
  # Application title
  shiny::titlePanel("AEMET OpenData"),
  
  # Sidebar panel
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      width = 3,
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
      shiny::fluidRow(
        column(
          6,
          shiny::actionButton(
            inputId = "updatePlot",
            label = "Paint!"
          )
        ),
        column(
          6,
          shiny::actionButton(
            inputId = "nextPlot",
            label = "Next"
          )
        )
      )
    ),
    shiny::mainPanel(
      width = 9,
      shiny::textOutput("plot")
    )
  )
)

server <- function(input, output, session) {
  
  # Configure "Next" button
  observeEvent(input$nextPlot, {
    if (input$variable == "Mean temperature (00h-24h)") {
      plot_choices <- plot_choices_temp
    } else if (input$variable == paste0("Precipitation (07h-07h", "\u207A", "\u00B9", ")")) {
      plot_choices <- plot_choices_pcp
    }
    current_index <- match(input$plot, plot_choices)
    
    # Find next index within choices
    next_index <- current_index %% length(plot_choices) + 1
    
    # Establish next plot in the list
    updateSelectInput(session, "plot", selected = plot_choices[next_index][[1]])
  })
  
  # Update plot selection choices depending on variable (tmean, pcp...) selection
  observe({
    if (input$variable == "Mean temperature (00h-24h)") {
      plot_choices <- plot_choices_temp
    } else if (input$variable == paste0("Precipitation (07h-07h", "\u207A", "\u00B9", ")")) {
      plot_choices <- plot_choices_pcp
    }
    
    updateSelectInput(session, "plot", choices = plot_choices)
  })
  
  # Do not change plot until button is pressed
  text <- eventReactive(list(input$updatePlot, input$nextPlot), ignoreInit = TRUE, { 
    # Draw plot
    return(input$plot)
  })
  
  # Display plot
  output$plot <- shiny::renderPrint({
    print(text())
  })
  
}

shiny::shinyApp(ui = ui, server = server)
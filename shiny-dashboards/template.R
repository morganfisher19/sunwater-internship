# Load necessary libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(ggiraph) # For interactive charts
library(gdtools) # IF using ggiraph
#library(extrafont) # IF not using ggiraph


# Register font
register_gfont("Roboto") # IF using ggiraph

# Load the dataset
df <- read.csv("*INSERT DATA HERE*.csv")

# Define UI for the application
ui <- fluidPage(
  
  # Link to external CSS file
  includeCSS("styles.css"),
  
  # Title section
  tags$div(
    id = "custom-html-section",
    tags$h1("*INSERT NAME* Dashboard"),
  ),
  
  fluidRow(
    # Selection Panel
    column(
      width = 12,  # Initial width
      addGFontHtmlDependency(family = c("Roboto")), # IF using ggiraph
      id = "sidebar",
      tags$div(
        id = "sidebar-content",
        tags$div(
          class = "grid-container",
          selectInput("*variable*",
                      "Select variable to display:",
                      choices = c("option 1", "option 2", 
                                  "option 3"),
                      selected = "option 1"),
          ),
        sliderInput("num_variable",
                    "Number of *variable* to display:",
                    min = 1,
                    max = length(unique(df$variable)),
                    value = 15,
                    step = 1,
                    width = "70%",
                    ticks = FALSE)
      )
    ),
    
    # Main Panel
    column(
      width = 12,  # Initial width
      id = "main",
      titlePanel(textOutput("main_title")),
      tags$h3(id = "subtitle", textOutput("subtitle")),
      girafeOutput("plot", width = "1000px"),
      #plotOutput("plot", height = "700px"), # if NOT using ggiraph
      tags$script(
        "Shiny.addCustomMessageHandler('openURL', function(url) { window.open(url); });"
      )
    )
  )
)


# Define server logic
server <- function(input, output, session) {
  
  output$main_title <- renderText({
    "TITLE"
  })
  output$subtitle <- renderText({
    paste(input$variable)
  })
  
  #output$plot <- renderPlot({ # if NOT using ggirafe
  output$plot <- renderGirafe({
    
    # Filter data based on selected settings
    filtered_df <- df %>%
      filter(variable == input$vairable)
    
    # Create custom theme
    # Do NOT use custom theme if using ggiraph
    theme_custom <- function() {
      theme_classic() +
        theme(
          text = element_text(family = "Arial"),
          plot.title = element_text(color = "#333333", size = 18, face = "bold"),
          plot.subtitle = element_text(color = "#777777", size = 16),
          axis.title = element_text(color = "#333333",size = 16),
          axis.text.y = element_text(color = "#333333", size = 12),
          legend.title = element_text(color = "#333333",size = 16),
          legend.text = element_text(color = "#333333", size = 12),
          axis.line = element_line(color = "#777777"),
          axis.ticks.x = element_line(color = "#777777")
          
        )
    }
    
    # Plot graph
    p <- ggplot(filtered_df, aes(x = variable, fill = variable)) +
      geom_bar(position = "fill") +
      labs(title = "TITLE",
           subtitle = "SUBTITLE",
           x = "variable",
           y = "variable") +
      theme_classic(base_family = "Roboto")  +
      theme(
        axis.title = element_text(color="black", size = 8),
        axis.text = element_text(color="black", size = 6),
        legend.title = element_text(color="black", size = 8),
        legend.text = element_text(color="black", size = 7))
    
    # Adding interactivity with hyperlinks
    girafe(ggobj = p,
           options = list(
             opts_hover(css = "cursor:pointer;fill:yellow;stroke:gray;"),
             opts_selection(type = "single",
                            css = "fill:yellow;stroke:gray;")
           ))
  })
  observeEvent(input$plot_selected, {
    selected_id <- input$plot_selected
    if (!is.null(selected_id)) {
      url <- paste0("https://app.legis1.com/lawmaker/detail?id=", selected_id, "#communications")
      session$sendCustomMessage(type = 'openURL', message = url)
    }
  })
}


# Run the application
shinyApp(ui = ui, server = server)

# Load necessary libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(ggiraph)
library(gdtools)

# Register font
register_gfont("Roboto") # IF using ggiraph

# Load the dataset
data <- read.csv("updated_staffer_experience.csv")

#Modify party column
data$party_name <- gsub(",.*", "", data$party_name)

# Define UI for the application
ui <- fluidPage(
  # Link to external CSS file
  includeCSS("styles.css"),
  
  # Title section
  tags$div(
    id = "custom-html-section",
    tags$h1("Staff Experience by Office"),
  ),
  
  fluidRow(
    # Sidebar Panel
    column(
      width = 12,  # Initial width
      addGFontHtmlDependency(family = c("Roboto")), # IF using ggiraph
      id = "sidebar",
      tags$div(
        id = "sidebar-content",
        tags$div(
          class = "grid-container",
          selectInput("filter", "Filter",
                      choices = c("No Filter", "By Chamber", "By Party"),
                      selected = "No Filter"),
          selectInput("selected_state", "Select State:",
                      choices = c("All States", unique(data$state_name)),
                      selected = "All States"),
          selectInput("lawmaker", "Select Lawmaker:",
                      choices = c("None", unique(data$display_name)),
                      selected = "None")
        )
      )
    ),
    
    # Main Panel
    column(
      width = 12,  # Initial width
      id = "main",
      titlePanel(textOutput("main_title")),
      tags$h3(id = "subtitle", textOutput("subtitle")),
      girafeOutput("plot", width = "800px"),
      tags$script(
        "Shiny.addCustomMessageHandler('openURL', function(url) { window.open(url); });"
      )
    )
  )
)


# Define server logic
server <- function(input, output, session) {
  
  output$main_title <- renderText({
    if (input$filter == "By Chamber") {
      "Distribution of Staff Experience Score by Chamber"
    }
    else if (input$filter == "By Party") {
      "Distribution of Staff Experience Score by Party"
    }
    else {
      "Distribution of Staff Experience Score"
    }
  })
  output$subtitle <- renderText({
    paste("118th Congress")
  })
  
  output$plot <- renderGirafe({
    
    #Filter by user input
    if (input$selected_state != "All States") { 
      data <- data %>% filter(state_name == input$selected_state) }
    
    if (input$filter == "By Chamber") {
      #Calculate max density
      max_density_per_chamber <- data %>%
        group_by(Chamber) %>%
        summarize(max_density = max(density(AverageStafferExperience)$y))
      max_density <- max(max_density_per_chamber$max_density)
      
      #Calculate medians
      medians <- data %>%
        group_by(Chamber) %>%
        summarize(median_experience = median(AverageStafferExperience, na.rm = TRUE))
      
      #Create tooltip
      tooltip <- paste0("Chamber: ", medians$Chamber, "<br>",
                        "Median: ", medians$median_experience)
      
      #Add to plot
      density_plot <- ggplot() + 
        geom_density(data = data, aes(x = AverageStafferExperience, fill = Chamber), alpha = 0.5) +
        scale_fill_manual(values = c("Senate" = "#4CAF50", "House" = "#F7E07C"))
      
      
      
    } else if (input$filter == "By Party") {
      #Filter out independents
      data <- data %>%
        filter(party_name != "Independent")
      
      #Calculate max density
      max_density_per_party <- data %>%
        group_by(party_name) %>%
        summarize(max_density = max(density(AverageStafferExperience)$y))
      max_density <- max(max_density_per_party$max_density)
      
      #Calculate medians
      medians <- data %>%
        group_by(party_name) %>%
        summarize(median_experience = median(AverageStafferExperience, na.rm = TRUE))
      
      #Create tooltip
      tooltip <- paste0("Party: ", medians$party_name, "<br>",
                        "Median: ", medians$median_experience)
      
      #Add to plot
      density_plot <- ggplot() + 
        geom_density(data = data, aes(x = AverageStafferExperience, fill = party_name), alpha = 0.5) +
        scale_fill_manual(values = c("Democrat" = "#5AB1E0", "Republican" = "#F28C8C")) +
        labs(fill = 'Party')
      
    }  else {
      
      #Calculate max density
      max_density <- max(density(data$AverageStafferExperience)$y)
      
      medians <- data %>%
        summarize(median_experience = median(AverageStafferExperience, na.rm = TRUE)) %>%
        mutate(x_position = median_experience,
               y_position = 0.0001)
      
      #Create tooltip
      tooltip <- paste0("Median: ", medians$median_experience)
      
      density_plot <- ggplot() + 
        geom_density(data = data, aes(x = AverageStafferExperience), fill = "#6B56AA", alpha = 0.6) +
        geom_segment_interactive(data = medians,
                                 mapping = aes(x = median_experience, xend = median_experience,
                                               y = 0, yend = max_density + .0005,
                                               tooltip = paste0("Median: ", median_experience)),
                                 color = "#333333", linetype = "dashed", size = .8)
      
    }
    
    
    # Creating plot details (lines, labels, etc.)
    density_plot_w_labels <- density_plot +
      #xlim(0, 500) +
      geom_segment_interactive(data = medians,
                               mapping = aes(x = median_experience, xend = median_experience,
                                             y = 0, yend = max_density + .0005,
                                             tooltip = tooltip),
                               color = "#333333", linetype = "dashed", size = .8) +
      labs(x = "Total Years of Staff Experience",
           y = "Frequency") +
      theme_classic(base_family = "Roboto") +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank()
      )
    
    p <- density_plot_w_labels
    
    # To show a specific lawmaker
    if (input$lawmaker != "None") {
      selected_lawmaker <- data %>% filter(display_name == input$lawmaker)
      
      if (input$filter == "By Chamber") {
        lawmaker_chamber <- selected_lawmaker %>% pull(Chamber)
        data <- data %>% 
          filter(Chamber == lawmaker_chamber) }
      else if (input$filter == "By Party") {
        lawmaker_party <- selected_lawmaker %>% pull(party_name)
        data <- data %>% 
          filter(party_name == lawmaker_party) }
      
      
      
      density_data <- density(data$AverageStafferExperience)
      lawmaker_density <- approx(density_data$x, density_data$y, xout = selected_lawmaker$AverageStafferExperience)$y
      
      
      
      p <- density_plot_w_labels + 
        geom_point_interactive(aes(x = selected_lawmaker$AverageStafferExperience, y = lawmaker_density,
                                   tooltip = paste0(selected_lawmaker$display_name, "<br>",
                                                    "Experience: ", selected_lawmaker$AverageStafferExperience)),
                               # ADD data_id = person_id HERE
                               size = 3, shape = 21, fill = "lightgray", color = "#333333")
    } 
    else {
      p <- density_plot_w_labels
    }
    
    
    
    # Adding interactivity with hyperlinks
    girafe(ggobj = p)
    # options = list(
    #   opts_hover(css = "cursor:pointer;fill:yellow;stroke:gray;"),
    #   opts_selection(type = "single",
    #                  css = "fill:yellow;stroke:gray;")
    # ))
  })
  
  # observeEvent(input$plot_selected, {
  #   selected_id <- input$plot_selected
  #   if (!is.null(selected_id)) {
  #     url <- paste0("https://app.legis1.com/lawmaker/detail?id=", selected_id, "#experience-analytics")
  #     session$sendCustomMessage(type = 'openURL', message = url)
  #   }
  # })
}


# Run the application
shinyApp(ui = ui, server = server)

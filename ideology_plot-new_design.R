# Load necessary libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggiraph)
library(gdtools)

# Register font
register_gfont("Roboto")
addGFontHtmlDependency(family = "Roboto")

# Load the dataset
data <- read.csv("cosponsors_all.csv")

#MAKING MATRIX (will probably get deleted later):
# Get a list of unique lawmakers
lawmakers <- unique(data$full_name_display)
# Create an empty matrix
lawmaker_matrix <- matrix(0, nrow = length(lawmakers), ncol = length(lawmakers))
# Set row and column names
rownames(lawmaker_matrix) <- lawmakers
colnames(lawmaker_matrix) <- lawmakers
# Loop through each bill
for(bill in unique(data$bill_number)) {
  # Find all lawmakers for this bill
  cosponsors <- data$full_name_display[data$bill_number == bill]
  # Initialize indices for while loop
  i <- 1
  while(i < length(cosponsors)) {
    j <- i + 1
    while(j <= length(cosponsors)) {
      lawmaker_matrix[cosponsors[i], cosponsors[j]] <- lawmaker_matrix[cosponsors[i], cosponsors[j]] + 1
      lawmaker_matrix[cosponsors[j], cosponsors[i]] <- lawmaker_matrix[cosponsors[j], cosponsors[i]] + 1
      j <- j + 1
    }
    i <- i + 1
  }
}
# center and scale the matrix
lawmaker_matrix_scaled <- scale(lawmaker_matrix)
pca_result <- prcomp(lawmaker_matrix_scaled, center = FALSE, scale. = FALSE)
pc1 <- pca_result$x[, 1]
pc2 <- pca_result$x[, 2]
# prepare data to plot
plot_data <- data.frame(Lawmaker = rownames(pca_result$x), PC1 = pc1*-1, PC2 = pc2*-1)
# pull out party info
plot_data$Party <- ifelse(grepl("\\(D", plot_data$Lawmaker), "D",
                          ifelse(grepl("\\(R", plot_data$Lawmaker), "R", "I"))
# View the updated data frame to verify correct tagging



# Define UI for the application
ui <- fluidPage(
  
  # Link to external CSS file
  includeCSS("styles_new.css"),
  
  # Title section
  tags$div(
    id = "custom-html-section",
    tags$h1("Congressional Ideology Dashboard"),
  ),
  
  fluidRow(
    # Sidebar Panel
    column(
      width = 3,  # Adjust width as needed
      id = "sidebar",
      tags$div(
        id = "sidebar-content",
        tags$div(
          class = "grid-container",
          selectInput("issue-1",
                      "Select issue #1 to display:",
                      choices = c("Issue Area 1", "Issue Area 2", 
                                  "Issue Area 3", "Issue Area 4", "Issue Area 5"),
                      selected = "Issue Area 1"),
          selectInput("issue-2",
                      "Select issue #2 to display:",
                      choices = c("Issue Area 1", "Issue Area 2", 
                                  "Issue Area 3", "Issue Area 4", "Issue Area 5"),
                      selected = "Issue Area 2"),
          selectInput("congress_version",
                      "Select Congress:",
                      choices = c("114", "115", 
                                  "116", "117", "118"),
                      selected = "117"),
          selectInput("chamber",
                      "Select the chamber to display:",
                      choices = c("House", "Senate", 
                                  "Both"),
                      selected = "Senate"),
          selectizeInput("lawmaker_search",
                         "Search Lawmakers:",
                         choices = unique(plot_data$Lawmaker),
                         multiple = FALSE,
                         options = list(
                           placeholder = "Enter lawmaker name...",
                           onInitialize = I('function() { this.setValue(""); }')
                         )
          )
        )
      )
    ),
    
    # Main Panel
    column(
      width = 9,  # Adjust width as needed
      id = "main",
      titlePanel(textOutput("main_title")),
      tags$h3(id = "subtitle", textOutput("subtitle")),
      girafeOutput("plot", height = "700px")
    )
  )
)


# Define server logic
server <- function(input, output) {
  
  output$main_title <- renderText({
    "Ideology of US Lawmakers"
  })
  output$subtitle <- renderText({
    paste0(input$congress_version, "th Congress")
  })
  
  output$plot <- renderGirafe({
    
    # Filter based on selected congress person
    plot_data <- plot_data %>%
      mutate(highlight = ifelse(Lawmaker == input$lawmaker_search, TRUE, FALSE))
    
    # Create new column 'tooltip' containing the info to display with ggirpah
    plot_data <- plot_data %>%
      mutate(
        tooltip = paste("Lawmaker: ", Lawmaker, "<br>PC1: ", round(PC1 * -1, 2), "<br>PC2: ", round(PC2 * -1, 2))
      )
    
    # Determine which variable is selected and process accordingly
    gg <- ggplot(plot_data, aes(x = PC1, y = PC2)) +
      geom_point_interactive(
        aes(
          tooltip = tooltip,
          data_id = Lawmaker),
        shape = 18, size = 2.5) +
      geom_point(color = ifelse(plot_data$highlight == 'TRUE', 'gold',
                                ifelse(plot_data$Party == 'R', '#810000',
                                       ifelse(plot_data$Party == 'D', '#2E598E', '#6B56AA'))),
                 shape = 18, size = 2.5) +
      geom_hline(yintercept = 0, color = "gray", linewidth = 0.5, linetype = "dashed") +  # Horizontal line at y=0
      geom_vline(xintercept = 0, color = "gray", linewidth = 0.5, linetype = "dashed") +  # Vertical line at x=0
      labs(x = "Issue Area 1",
           y = "Issue Area 2") +
      theme_classic(base_family = "Roboto")
    
    girafe(ggobj = gg)
  })
}


# Run the application
shinyApp(ui = ui, server = server)

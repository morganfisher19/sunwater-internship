# Load necessary libraries
library(shiny)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(extrafont)

# Load the dataset
df <- read.csv("congress_member_demographics.csv")

# Define UI for the application
ui <- fluidPage(
  
  # Link to external CSS file
  includeCSS("styles_new.css"),
  
  # Title section
  tags$div(
    id = "custom-html-section",
    tags$h1("Committee Member Demographics Dashboard"),
  ),
  
  fluidRow(
    # Sidebar Panel
    column(
      width = 12,  # Initial width
      id = "sidebar",
      tags$div(
        id = "sidebar-content",
        tags$div(
          class = "grid-container",
          selectInput("variable",
                      "Select variable to display:",
                      choices = c("Political Affiliation", "Gender", 
                                  "Age", "Experience"),
                      selected = "Political Affiliation"),
          selectInput("congress_version",
                      "Select Congress:",
                      choices = sort(unique(df$congress)),
                      selected = max(df$congress),
                      multiple = TRUE)
        ),
        sliderInput("num_committees",
                    "Number of committees to display:",
                    min = 1,
                    max = length(unique(df$committee_name)),
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
      plotOutput("plot", height = "700px")
    )
  )
)


# Define server logic
server <- function(input, output) {
  
  # Render title & subtitle text based on input
  output$main_title <- renderText({
    paste(input$variable, "Breakdown by Committee")
  })
  output$subtitle <- renderText({
    paste0(input$congress_version, "th Congress")
  })
  
  output$plot <- renderPlot({
    
    # Filter data based on selected Congress version
    filtered_df <- df %>%
      filter(congress == input$congress_version)
    
    # Select top committees based on user input
    top_committees <- filtered_df %>%
      group_by(committee_name) %>%
      summarize(count = n()) %>%
      top_n(input$num_committees, count) %>%
      pull(committee_name)
    
    # Filter the dataset for the selected committees
    filtered_df <- filtered_df %>% filter(committee_name %in% top_committees)
    
    # Determine a responsive width based on the number of categories
    width_factor <- 0.7 / length(input$num_committees)  # Adjust as needed
    
    # Create custom theme
    theme_custom <- function() {
      theme_classic() +
        theme(
          text = element_text(family = "Roboto"),
          axis.title = element_text(color="black", size = 16),
          axis.text = element_text(color="black", size = 13),
          legend.title = element_text(color="black", size = 16),
          legend.text = element_text(color="black", size = 13)
        )
    }
    
    # Determine which variable is selected and process accordingly
    if (input$variable == "Political Affiliation") {
      # Delete anything following a comma
      filtered_df$party_name <- gsub(",.*", "", filtered_df$party_name)
      
      # Calculate the proportion of Republicans for each committee
      proportion_df <- filtered_df %>%
        group_by(committee_name) %>%
        summarize(proportion_republican = sum(party_name == "Republican") / n()) %>%
        arrange(desc(proportion_republican))
      
      # Reorder the committee names based on the proportion of Republicans
      filtered_df <- filtered_df %>%
        mutate(committee_name = factor(committee_name, levels = proportion_df$committee_name))
      
      # Set colors
      party_colors <- c("Democrat" = "#2E598E", "Republican" = "#810000", "Independent" = "#6B56AA")
      
      # Create the plot
      ggplot(filtered_df, aes(x = committee_name, fill = party_name)) +
        geom_bar(position = "fill", width = width_factor) +
        scale_fill_manual(values = party_colors) +
        coord_flip() +
        labs(x = "Committee",
             y = "Proportion",
             fill = "Party Affiliation") +
        theme_custom() +
        theme(legend.position = "top") +
        guides(fill = guide_legend(title.position = "top", title.hjust = 0.5))
      
    } else if (input$variable == "Gender") {
      # Calculate the proportion of Republicans for each committee
      proportion_df <- filtered_df %>%
        group_by(committee_name) %>%
        summarize(proportion_male = sum(gender == "Male") / n()) %>%
        arrange(desc(proportion_male))
      
      # Reorder the committee names based on the proportion of Republicans
      filtered_df <- filtered_df %>%
        mutate(committee_name = factor(committee_name, levels = proportion_df$committee_name))
      
      # Set colors
      gender_colors <- c("Male" = "#579DB9", "Female" = "#973D7D")
      
      # Create the plot
      # Create the plot
      ggplot(filtered_df, aes(x = committee_name, fill = gender)) +
        geom_bar(position = "fill", width = width_factor) +
        scale_fill_manual(values = gender_colors) +
        coord_flip() +
        labs(x = "Committee",
             y = "Proportion",
             fill = "Gender") +
        theme_custom() +
        theme(legend.position = "top") +
        guides(fill = guide_legend(title.position = "top", title.hjust = 0.5))
      
    } else if (input$variable == "Age") {
      # Step 1: Calculate summary statistics for age by committee
      summary_df <- filtered_df %>%
        group_by(committee_name) %>%
        summarize(
          min_age = min(age, na.rm = TRUE),
          q25 = quantile(age, probs = 0.25, na.rm = TRUE),
          median_age = median(age, na.rm = TRUE),
          q75 = quantile(age, probs = 0.75, na.rm = TRUE),
          max_age = max(age, na.rm = TRUE)
        ) %>%
        arrange(median_age) # Order by median age
      
      # Step 2: Create the boxplot
      ggplot(summary_df, aes(x = reorder(committee_name, median_age), ymin = min_age, lower = q25, middle = median_age, upper = q75, ymax = max_age)) +
        geom_boxplot(stat = "identity", fill = "#89C1C4", color = "#0C2041", width = 0.5) +
        labs(x = "Committee",
             y = "Age") +
        coord_flip() +
        theme_custom()
      
    } else if (input$variable == "Experience") {
      # Step 1: Calculate summary statistics for age by committee
      summary_df <- filtered_df %>%
        group_by(committee_name) %>%
        summarize(
          min = min(experience_years, na.rm = TRUE),
          q25 = quantile(experience_years, probs = 0.25, na.rm = TRUE),
          median = median(experience_years, na.rm = TRUE),
          q75 = quantile(experience_years, probs = 0.75, na.rm = TRUE),
          max = max(experience_years, na.rm = TRUE)
        ) %>%
        arrange(median) # Order by median age
      
      # Step 2: Create the boxplot
      ggplot(summary_df, aes(x = reorder(committee_name, median), ymin = min, lower = q25, middle = median, upper = q75, ymax = max)) +
        geom_boxplot(stat = "identity", fill = "#89C1C4", color = "#0C2041", width = 0.5) +
        labs(x = "Committee",
             y = "Experience in Congress") +
        coord_flip() +
        theme_custom()
      
    }
  })
}


# Run the application
shinyApp(ui = ui, server = server)

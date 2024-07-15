# Load necessary libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(extrafont)



# Load the dataset

df <- read.csv("tweet_issue_lawmaker_dates.csv")

# Modify date column
df$date <- as.Date(df$pub_date, format= "%Y-%m-%d")
df$date_display <- format(df$date, "%B %Y")
df$date_sort <- as.Date(paste0(format(df$date, "%Y-%m"), "-01"))

df <- df %>%
  group_by(date_sort, date_display, member_id, person_id, chamber, issue_name, swi_issue_id, display_name, formal_title, first_name, last_name, party_name, us_state_id, district_no) %>%
  summarise(posts = n(), .groups = "drop") %>%
  ungroup() %>%
  arrange(date_sort)


#Modify party column
df$party_name <- gsub(",.*", "", df$party_name)


# Define UI for the application
ui <- fluidPage(
  
  # Link to external CSS file
  includeCSS("styles_new.css"),
  
  # Title section
  tags$div(
    id = "custom-html-section",
    tags$h1("Lawmaker X (Twitter) Activity by Issue")
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
          selectInput("selected_issue",
                      "Issue Area",
                      choices = c("All", "Government Operations" , "Law, Crime, and Family Issues","Education", "Health", 
                                  "International Affairs and Foreign Aid","Economy", "Immigration and Refugee Issues", 
                                  "Civil Rights, Minority Issues, and Civil Liberties", "Social Welfare", "Energy", 
                                  "Community Development and Housing Issues", "Agriculture", "Defense", 
                                  "Banking, Finance, and Domestic Commerce", "Labor and Employment", "Environment", 
                                  "Space, Science, Technology, and Communications", "Transportation", "Cultural Issues", 
                                  "Public Lands, Water Management, and Territorial Issues", "Foreign Trade", 
                                  "Space, Science, Technology, and Communications"),
                      selected = "All"),
          selectInput("selected_chamber",
                      "Chamber",
                      choices = c("Both Chambers", "House", "Senate"),
                      selected = "Both Chambers"),
          selectInput("selected_party",
                      "Party",
                      choices = c("All Parties", "Democrat", "Republican", "Independent"),
                      selected = "All Parties"),
          selectInput("num_lawmakers",
                      "Number of lawmakers",
                      choices = c("10", "20", "50"),
                      selected = "20"),
          selectInput("start_date",
                      "Start Date",
                      choices = unique(df$date_display),
                      selected = head(df$date_display, 1)),
          selectInput("end_date",
                      "End Date",
                      choices = unique(df$date_display),
                      selected = tail(df$date_display, 1))
        ),
      ),
    ),
    
    
    # Main Panel
    column(
      width = 12,  # Initial width
      id = "main",
      titlePanel(textOutput("main_title")),
      tags$h3(id = "subtitle", textOutput("subtitle")),
      plotOutput("plot", height = "800px")
    )
  )
)


# Define server logic
server <- function(input, output) {
  
  # Render title & subtitle text based on slider input
  output$main_title <- renderText({
    if (input$selected_issue == "All") {
      paste("Top", input$num_lawmakers, "Lawmakers to tweet") 
    } else {
      paste("Top", input$num_lawmakers, "Lawmakers to tweet about", input$selected_issue) 
    }
  })
  output$subtitle <- renderText({
    paste0(input$selected_chamber, ", ", input$selected_party)
  })
  
  output$plot <- renderPlot({
    
    # Filter by date
    start_date <- df %>% filter(date_display == input$start_date) %>% pull(date_sort)
    end_date <- df %>% filter(date_display == input$end_date) %>% pull(date_sort)
    
    filtered_df <- subset(df, date_sort >= unique(start_date) & date_sort <= unique(end_date))
    
    
    # Filter by selected issue
    if (input$selected_issue != "All") {
      filtered_df <-
        filtered_df %>%
        filter(issue_name == input$selected_issue) }
    
    # Filter by selected party
    if (input$selected_party != "All Parties") {
      filtered_df <-
        filtered_df %>%
        filter(party_name == input$selected_party) }
    
    # Filter by selected chamber
    if (input$selected_chamber != "Both Chambers") {
      filtered_df <-
        filtered_df %>%
        filter(chamber == input$selected_chamber) }
    
    #Prepare data for plotting
    filtered_df <- filtered_df %>%
      select(-date_sort, -date_display)
    if (input$selected_issue == "All") {
      plot_df <- filtered_df %>%
        group_by(display_name, party_name) %>%
        summarise(posts = sum(posts), .groups = "drop")
    } else {
      plot_df <- filtered_df %>%
        group_by(issue_name, display_name, party_name) %>%
        summarise(posts = sum(posts), .groups = "drop")
    }
    
    # Select top lawmakers based on user input
    if (input$num_lawmakers != "All") {
      top_lawmakers <- plot_df %>%
        arrange(desc(posts)) %>%
        head(as.numeric(input$num_lawmakers))
      # Filter the dataset for the selected lawmakers
      plot_df <- plot_df %>% filter(display_name %in% top_lawmakers$display_name)
    }
    
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
    
    # Set colors
    party_colors <- c("Democrat" = "#2E598E", "Republican" = "#810000", "Independent" = "#B19CD9")
    
    
    # Plot graph
    ggplot(plot_df, aes(x = reorder(display_name, posts), y = posts, fill = party_name)) +
      geom_bar(stat = "identity") +
      labs(x = "Lawmaker",
           y = "Number of Twitter Posts",
           fill = "Party Name") +
      coord_flip() +
      scale_fill_manual(values = party_colors) +
      theme_custom()
  })
  
}


# Run the application
shinyApp(ui = ui, server = server)

# Load necessary libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(ggiraph) # For interactive charts
library(gdtools) # IF using ggiraph


# Register font
register_gfont("Roboto") # IF using ggiraph

# Load the dataset
df <- read.csv("all_leg.csv")
# Data currently has the following variables
    # bill_id	
    # bill_url	
    # bill_number	
    # bill_type	
    # bill_chamber	
    # bill_no	
    # bill_name	
    # congress	
    # swi_issue_id	
    # issue_name	
    # sponsor_name	
    # sponsor_person_id	
    # sponsor_party	
    # cosponsor_name	
    # cosponsor_person_id	
    # cosponsor_party

# Create bill_search_item column
df <- df %>%
  mutate(bill_search_item = paste0(bill_number, " - ", congress, "th congress"))

# Define UI for the application
ui <- fluidPage(
  
  # Link to external CSS file
  includeCSS("styles.css"),
  
  # Title section
  tags$div(
    id = "custom-html-section",
    tags$h1("Bill Cosponsorship"),
  ),
  
  fluidRow(
    # Selection Panel
    column(
      width = 12,  # Initial width
      addGFontHtmlDependency(family = c("Roboto")),
      id = "sidebar",
      tags$div(
        id = "sidebar-content",
        tags$div(
          class = "grid-container",
          
          # Filter by Congress Version
          selectInput("congress_version",
                      "Congress",
                      choices = sort(unique(df$congress), decreasing = TRUE),
                      selected = max(df$congress)),
          # Filter by Chamber
          selectInput("selected_chamber",
                      "Chamber",
                      choices = c("House", "Senate"),
                      selected = "House"),
          # Filter by Issue
          selectInput("selected_issue",
                      "Issue Area",
                      choices = c("All", sort(unique(df$issue_name))),
                      selected = "All"),
          # Search for specific bills
          selectizeInput("search_bills",
                         "Search Bills",
                         choices = c("All", sort(unique(df$bill_search_item))),
                         selected = "All"),
          # Search for specific sponsors
          selectizeInput("search_sponsor",
                         "Search Sponsor",
                         choices = c("All", unique(df$sponsor_name)),
                         selected = "All"),
          # Search for specific cosponsors
          selectizeInput("search_cosponsor",
                         "Search Cosponsor",
                         choices = c("All", unique(df$cosponsor_name)),
                         selected = "All"),
        )
      )
    ),
    
    # Main Panel
    column(
      width = 12,  # Initial width
      id = "main",
      titlePanel(textOutput("main_title")),
      tags$h3(id = "subtitle", textOutput("subtitle")),
      girafeOutput("plot", width = "1000px"),
      tags$p (HTML("<b>Methodology</b><br>
                   Data is provided by <a href='https://www.congress.gov' target='_blank'>congress.gov</a>. Cosponsor data is derived from each bill’s Republican and Democratic cosponsors.")),
      tags$script(
        "Shiny.addCustomMessageHandler('openURL', function(url) { window.open(url); });"
      )
    )
  )
)


# Define server logic
server <- function(input, output, session) {
  
  # Chart title
  output$main_title <- renderText({
    "Scatterplot of Bill Cosponsorship"
  })
  
  # Chart subtitle
  output$subtitle <- renderText({
    paste0(input$selected_chamber, ", ", input$congress_version, "th Congress")
  })
  
  # Chart
  output$plot <- renderGirafe({
    
    # Filter data based on selected settings
    df_filtered <- df
    
    if (input$search_bills == "All" && input$search_sponsor == "All" && input$search_cosponsor == "All") {
      
      # Filter by congress version
      df_filtered <- df_filtered %>%
        filter(congress == input$congress_version) %>%
        filter(bill_chamber == input$selected_chamber) %>%
        mutate(
          tooltip = paste(bill_number, bill_name, "<br>", "Sponsor: ", sponsor_name))
      
      # Filter by selected issue
      if (input$selected_issue != "All") {
        df_filtered <- df_filtered %>%
          filter(issue_name == input$selected_issue) 
      }
      
    } else if (input$search_bills != "All") {
      df_filtered <- df_filtered %>%
        filter(bill_search_item == input$search_bills) %>%
        mutate(
          tooltip = paste(bill_number, "<br>", bill_name, "<br>", "Sponsor: ", sponsor_name)) }
    
    #For joint sponsor & cosponsor search
    else if (input$search_sponsor != "All" && input$search_cosponsor != "All") {
      cosponsor_bill_ids <- df %>%
        filter(cosponsor_name == input$search_cosponsor) %>%
        pull(bill_id) %>%
        unique()
      
      df_filtered <- df %>%
        filter(bill_id %in% cosponsor_bill_ids) %>%
        filter(sponsor_name == input$search_sponsor) %>%
        mutate(
          tooltip = paste(congress, "th Congress", "<br>", bill_number, bill_name, "<br>", "Sponsor: ", sponsor_name)) }
    
    #For sponsor search
    else if (input$search_sponsor != "All") {
      df_filtered <- df_filtered %>%
        filter(sponsor_name == input$search_sponsor) %>%
        mutate(
          tooltip = paste(congress, "th Congress", "<br>", bill_number, bill_name, "<br>")) }
    
    #For cosponsor search
    else if (input$search_cosponsor != "All") {
      cosponsor_bill_ids <- df %>%
        filter(cosponsor_name == input$search_cosponsor) %>%
        pull(bill_id) %>%
        unique()
      
      df_filtered <- df %>%
        filter(bill_id %in% cosponsor_bill_ids) %>%
        mutate(
          tooltip = paste(congress, "th Congress", "<br>", bill_number, bill_name, "<br>", "Sponsor: ", sponsor_name))
    }
    
    # Ensure cosponsor_party column has correct values
    df_filtered <- df_filtered %>%
      mutate(cosponsor_party = trimws(cosponsor_party)) %>%
      filter(cosponsor_party %in% c("Democrat", "Republican"))
    
    # Create plot_df with the required columns and calculations
    plot_df <- df_filtered %>%
      group_by(bill_url, bill_name, bill_number, sponsor_name, sponsor_person_id, sponsor_party, tooltip) %>%
      summarize(
        num_dem_cosponsors = n_distinct(cosponsor_person_id[cosponsor_party == "Democrat"]),
        num_rep_cosponsors = n_distinct(cosponsor_person_id[cosponsor_party == "Republican"])
      ) %>%
      ungroup()
    
    # Set axis limits
    if (input$selected_chamber == "Senate" ||
        startsWith(input$search_bills, "S") ||
        startsWith(input$search_sponsor, "S") ||
        startsWith(input$search_cosponsor, "S")) 
    {axis_limit <- 50} 
    else 
    {axis_limit <- 220}
    
    # Plot graph
    p <- ggplot(plot_df, aes(x = num_dem_cosponsors, y = num_rep_cosponsors)) +
      geom_point_interactive(aes(tooltip = tooltip, data_id = bill_url),
                             color = ifelse(plot_df$sponsor_party == 'Republican', '#810000',
                                            ifelse(plot_df$sponsor_party == 'Democrat', '#2E598E', '#6B56AA')),
                             size = 2.5, alpha = 0.5) +
      geom_abline(intercept = 0, slope = 1, color = "gray", linewidth = 0.5, linetype = "dashed") +
      coord_cartesian(
        xlim = c(0, axis_limit),
        ylim = c(0, axis_limit)
      ) +
      #add conditional for different ranges for different chambers
      labs(x = "Democratic Cosponsors",
           y = "Republican Cosponsors") +
      theme_classic(base_family = "Roboto")
    
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
      url <- paste0("https://app.legis1.com/bill/detail?id=", selected_id, "#summary")
      session$sendCustomMessage(type = 'openURL', message = url)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)



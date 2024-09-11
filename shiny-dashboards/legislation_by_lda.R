# Load necessary libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(ggiraph) 
library(gdtools)

# Register font
register_gfont("Roboto")
addGFontHtmlDependency(family = "Roboto")

# Load the dataset
df <- read.csv("lda_orgs.csv")
# Data currently has the following variables
    # client_lobby_actor_id
    # organization_id
    # bill_id	
    # organization_name	
    # chamber	
    # bill_number	
    # bill_name	
    # count_firms	
    # count_lda	
    # congress	
    # title	
    # cosponsor_count	
    # status	
    # display_name	
    # party_name

# Replace NA values in cosponsor_count with 0
df$cosponsor_count[is.na(df$cosponsor_count)] <- 0

#Modify party column
df$party_name <- gsub(",.*", "", df$party_name)

#Creating bill_url column
df <- df %>%
  mutate(
    bill_number_suffix = gsub(".* (\\d+)$", "\\1", bill_number),
    chamber_format = case_when(
      chamber == "House" ~ "HR",
      chamber == "Senate" ~ "S",
    ),
    bill_url = paste0(bill_number_suffix, "_", congress, "_", chamber_format)
  ) %>%
  select(-bill_number_suffix, -chamber_format) 


# Load the issues dataset
df_issue <- read.csv("lda_bill_issues.csv")
df_issue <- df_issue %>% distinct()

# Data currently has the following variables
    # bill_id	
    # swi_issue_id	
    # issue_name


# Define UI for the application
ui <- fluidPage(
  
  # Link to external CSS file
  includeCSS("styles.css"),
  
  # Title section
  tags$div(
    id = "custom-html-section",
    tags$h1("Legislation by LDA Dashboard"),
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
          
          # Filter by Bills to display
          selectInput("num_bills",
                      "Number of Bills to Display",
                      choices = c(10, 20, 40), 
                      selected = 20),
          
          # Filter by Congress Version(s)
          selectInput("congress_version",
                      "Filter by Congress Version",
                      choices = sort(unique(df$congress), decreasing = TRUE),
                      selected = 118,
                      multiple = TRUE),
          
          # Filter by Organization(s)
          selectInput("selected_organizations",
                      "Filter by Organization",
                      choices = sort(unique(df$organization_name)), 
                      selected = NULL,
                      multiple = TRUE),
          
          # Filter by only displaying bills sponsored by indicated lawmakers
          selectInput("selected_sponsors",
                      "Filter by Sponsor",
                      choices = unique(df$display_name),
                      selected = NULL,
                      multiple = TRUE),
          
          # Filter by the sponsor's party
          selectInput("selected_party",
                      "Filter by Sponsor's Party",
                      choices = unique(df$party_name),
                      selected = NULL,
                      multiple = TRUE),
          
          # Filter by Bill Status
          selectizeInput("selected_status",
                         "Filter by bill status",
                         choices = sort(setdiff(unique(df$status), "NULL")),
                         selected = NULL,
                         multiple = TRUE),
          
          # Filter by Issue(s)
          selectInput("selected_issues",
                      "Filter by issue",
                      choices = sort(unique(df_issue$issue_name)),
                      selected = NULL,
                      multiple = TRUE)
        )
      )
    ),
    
    # Main Panel
    column(
      width = 12,  # Initial width
      id = "main",
      titlePanel(textOutput("main_title")),
      tags$h3(id = "subtitle", textOutput("subtitle")),
      girafeOutput("plot", height = "800px"),
      tags$script(
        "Shiny.addCustomMessageHandler('openURL', function(url) { window.open(url); });"
      )
    )
  )
)


# Define server logic
server <- function(input, output, session) {
  
  #Chart title
  output$main_title <- renderText({
    "Legislation by LDA"
  })
  
  #Chart subtitle
  output$subtitle <- renderText({
    if (is.null(input$selected_organizations)) {
      paste("All Organizations, Categorized by Party Color")
    } else {
      paste(input$selected_organizations, collapse = ", ")
    }
  })
  
  #Plot
  output$plot <- renderGirafe({
    
    # Filter data based on selected settings
    filtered_df <- df
    
    # Filter by selected organizations
    if (!is.null(input$selected_organizations)) {
      filtered_df <- filtered_df %>%
        filter(organization_name %in% input$selected_organizations)
    }
    
    # Filter by selected congress
    if (!is.null(input$congress_version)) {
      filtered_df <- filtered_df %>%
        filter(congress %in% input$congress_version)
    }
    
    # Filter by selected sponsors
    if (!is.null(input$selected_sponsors)) {
      filtered_df <- filtered_df %>%
        filter(display_name %in% input$selected_sponsors)
    }
    
    # Filter by selected sponsor party
    if (!is.null(input$selected_party)) {
      filtered_df <- filtered_df %>%
        filter(party_name %in% input$selected_party)
    }
    
    
    # Filter by selected status
    if (!is.null(input$selected_status)) {
      filtered_df <- filtered_df %>%
        filter(status %in% input$selected_status)
    }
    
    # Filter by selected issue
    if (!is.null(input$selected_issues)) {
      filtered_df_issue <- df_issue %>%
        filter(issue_name %in% input$selected_issues)
      filtered_df <- filtered_df %>%
        semi_join(filtered_df_issue, by = "bill_id")
    }
    
    
    # Prepare data for plot
    filtered_df <- filtered_df %>%
      select(organization_name, bill_id, bill_name, bill_number, bill_url, count_lda, display_name, party_name, cosponsor_count, status, congress)  %>%
      distinct()
    
    plot_df <- filtered_df %>%
      group_by(bill_id, bill_name, bill_number, bill_url, display_name, party_name, cosponsor_count, status, congress) %>%
      summarize(
        total_count_lda = sum(count_lda)
      ) %>%
      ungroup()
    
    top_bills <- plot_df %>%
      arrange(desc(total_count_lda)) %>%
      head(as.numeric(input$num_bills))
    
    # For all organizations
    if (is.null(input$selected_organizations)) {
      # Prep data
      plot_df <- plot_df %>% 
        filter(bill_id %in% top_bills$bill_id) %>% 
        mutate(tooltip = paste0(bill_number, " ", bill_name, "<br>", 
                                "Bill Status: ", status, "<br>",
                                "Congress: ", congress, "th", "<br>", 
                                "Sponsor: ", display_name, "<br>", 
                                "Number of Coponsors: ", cosponsor_count, "<br>",
                                "Number of LDAs: ", total_count_lda))
      
      plot_df <- plot_df %>%
        select(bill_id, bill_number, bill_url, total_count_lda, party_name, tooltip)
      
      party_colors <- c("Democrat" = "#2E598E", "Republican" = "#810000", "Independent" = "#B19CD9")
      
      
      # Plot graph
      p <- ggplot(plot_df, aes(x = reorder(bill_id, total_count_lda), y = total_count_lda, fill = party_name)) +
        geom_bar_interactive(stat = "identity", aes(tooltip = tooltip, 
                                                    data_id = bill_url)) +
        labs(x = "Bill",
             y = "Number of LDAs",
             fill = "Party Name") +
        coord_flip() +
        scale_x_discrete(labels = plot_df$bill_number) +
        scale_fill_manual(values = party_colors) +
        theme_classic(base_family = "Roboto") +
        theme(
          axis.text.y = element_text(size = 8)
        )
    }
    
    # For specified organizations
    else {
      # Prep data
      org_plot_df <- filtered_df %>%
        group_by(bill_id) %>%
        mutate(total_count_lda = sum(count_lda)) %>%
        ungroup()
      
      org_plot_df <- org_plot_df %>% 
        filter(bill_id %in% top_bills$bill_id) %>% 
        mutate(tooltip = paste0(bill_number, " ", bill_name, "<br>", 
                                "Bill Status: ", status, "<br>",
                                "Congress: ", congress, "th", "<br>", 
                                "Sponsor: ", display_name, "<br>", 
                                "Number of Coponsors: ", cosponsor_count, "<br>",
                                "Number of LDAs: ", total_count_lda))
      
      org_plot_df <- org_plot_df %>%
        select(organization_name, bill_id, bill_number, bill_url, total_count_lda, count_lda, party_name, tooltip)
      
      
      # Define colors
      color_palette <- c("#0C2041", "#2E598E", "#E37A61", "#EFA982", "#579DB9", 
                         "#89C1C4", "#4A001E", "#810000", "#973D7D", "#6B56AA")
      
      # Plot graph
      p <- ggplot(org_plot_df, aes(x = reorder(bill_id, total_count_lda), y = count_lda, fill = organization_name)) +
        geom_bar_interactive(stat = "identity", position = "stack", 
                             aes(tooltip = tooltip, data_id = bill_url)) +
        labs(x = "Bill",
             y = "Number of LDAs",
             fill = "Organization Name") +
        coord_flip() +
        scale_x_discrete(labels = plot_df$bill_number) +
        scale_fill_manual(values = color_palette) +
        theme_classic(base_family = "Roboto") +
        theme(
          axis.text.y = element_text(size = 8)
        )
    }
    
    # Adding interactivity with hyperlinks
    girafe(ggobj = p,
           options = list(
             opts_hover(css = "cursor:pointer;fill:gray;stroke:gray;"),
             opts_selection(type = "single",
                            css = "fill:gray;stroke:gray;")
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

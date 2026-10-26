library(shiny)
library(bslib)
library(tidyverse)
library(DT)
library(shinycssloaders)

# Load data
census_data <- read_csv("data/nc_census_pums.csv")

#  variables
cat_vars <- c("education", "sex")
num_vars <- c("gas_cost", "rent_burden", "age")

ui <- page_sidebar(
  title = "North Carolina PUMS Census Explorer",
  
  # --- SIDEBAR PANEL ---
  sidebar = sidebar(
    title = "Data Subsetting",
    
    # Categorical Filter 1
    selectInput(
      "cat_filter_1",
      "Filter Education Level:",
      choices = c("All", levels(as.factor(census_data$education))),
      selected = "All",
      multiple = TRUE
    ),
    
    # Categorical Filter 2
    selectInput(
      "cat_filter_2",
      "Filter Sex:",
      choices = c("All", levels(as.factor(census_data$sex))),
      selected = "All",
      multiple = TRUE
    ),
    
    hr(),
    
    # Numeric Filter 1
    selectInput(
      "num_choice_1", 
      "Choose First Numeric Filter:", 
      choices = num_vars, 
      selected = "age"
    ),
    uiOutput("slider_1_ui"),
    
    # Numeric Filter 2
    selectInput(
      "num_choice_2", 
      "Choose Second Numeric Filter:", 
      choices = num_vars, 
      selected = "gas_cost"
    ),
    uiOutput("slider_2_ui"),
    
    hr(),
    
    # Button
    actionButton("apply_btn", "Apply Filters", class = "btn-primary w-100")
  ),
  
  # --- MAIN PANEL---
  navset_tab(
    id = "main_tabs",
    nav_panel("About", h3("North Carolina Census PUMS Exploration")),
    nav_panel("Data Download", h3("Data Download Placeholder")),
    nav_panel("Data Exploration", h3("Data Exploration Placeholder"))
  )
)


server <- function(input, output, session) {
  
  # - DYNAMIC SLIDER 1 
  output$slider_1_ui <- renderUI({
    req(input$num_choice_1)
    vals <- census_data[[input$num_choice_1]]
    min_val <- floor(min(vals, na.rm = TRUE))
    max_val <- ceiling(max(vals, na.rm = TRUE))
    
    sliderInput(
      "slider_1",
      label = paste("Range for", input$num_choice_1),
      min = min_val,
      max = max_val,
      value = c(min_val, max_val)
    )
  })
  
  # - DYNAMIC SLIDER 2
  output$slider_2_ui <- renderUI({
    req(input$num_choice_2)
    vals <- census_data[[input$num_choice_2]]
    min_val <- floor(min(vals, na.rm = TRUE))
    max_val <- ceiling(max(vals, na.rm = TRUE))
    
    sliderInput(
      "slider_2",
      label = paste("Range for", input$num_choice_2),
      min = min_val,
      max = max_val,
      value = c(min_val, max_val)
    )
  })
  
  # --- SUBSETTING
  # Only runs when buttin is clicked
  filtered_census <- eventReactive(input$apply_btn, {
    req(input$slider_1, input$slider_2)
    
    data <- census_data
    
    # Filter Categorical 1
    if (!("All" %in% input$cat_filter_1) && length(input$cat_filter_1) > 0) {
      data <- data %>% filter(education %in% input$cat_filter_1)
    }
    
    # Filter Categorical 2
    if (!("All" %in% input$cat_filter_2) && length(input$cat_filter_2) > 0) {
      data <- data %>% filter(sex %in% input$cat_filter_2)
    }
    
    # Filter Dynamic Numeric 1
    v1 <- input$num_choice_1
    data <- data %>%
      filter(!is.na(.data[[v1]]), 
             .data[[v1]] >= input$slider_1[1], 
             .data[[v1]] <= input$slider_1[2])
     
    # Filter Dynamic Numeric 2
    v2 <- input$num_choice_2
    data <- data %>%
      filter(!is.na(.data[[v2]]), 
             .data[[v2]] >= input$slider_2[1], 
             .data[[v2]] <= input$slider_2[2])
    
    data
  }, ignoreNULL = FALSE) 
  # the app initializes with the unfiltered dataset rather than an empty screen.
}
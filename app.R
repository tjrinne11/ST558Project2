library(shiny)
library(bslib)
library(tidyverse)
library(DT)
library(shinycssloaders)

# Load data
census_data <- read_csv("data/nc_census_pums.csv")

# Rename the survey weight column so it matches the rest of the app
census_data <- census_data |> rename(PWGTP = weight)

# Available variables
cat_vars <- c("education", "sex")
num_vars <- c("gas_cost", "rent_burden", "age")

# Variables for the exploration tab (every categorical / numeric column except weights and IDs)
explore_cat_vars <- names(census_data)[map_lgl(census_data, ~ (is.character(.x) || is.factor(.x)) && n_distinct(.x) <= 30)]
explore_num_vars <- names(census_data)[map_lgl(census_data, is.numeric)]
explore_num_vars <- explore_num_vars[!str_detect(explore_num_vars, "WGTP|SERIALNO|SPORDER|PUMA|^ST$")]

# Weighted median
weighted_median <- function(x, w) {
  o <- order(x)
  x[o][which(cumsum(w[o]) >= sum(w) / 2)[1]]
}

# Weighted summary of one numeric variable, by group
make_summary <- function(data, var) {
  data |>
    group_by(group) |>
    summarise(
      n = n(),
      w_mean = weighted.mean(.data[[var]], PWGTP),
      w_median = weighted_median(.data[[var]], PWGTP),
      w_sd = sqrt(weighted.mean((.data[[var]] - w_mean)^2, PWGTP)),
      min = min(.data[[var]]),
      max = max(.data[[var]]),
      .groups = "drop"
    ) |>
    mutate(variable = var, .before = 1)
}

ui <- page_sidebar(
  title = "North Carolina PUMS Census Explorer",
  
  # --- SIDEBAR PANEL ---
  sidebar = sidebar(
    title = "Data Subsetting",
    
    # Categorical Filter 1 (Allows 'All')
    selectInput(
      "cat_filter_1",
      "Filter Education Level:",
      choices = c("All", levels(as.factor(census_data$education))),
      selected = "All",
      multiple = TRUE
    ),
    
    # Categorical Filter 2 (Allows 'All')
    selectInput(
      "cat_filter_2",
      "Filter Sex:",
      choices = c("All", levels(as.factor(census_data$sex))),
      selected = "All",
      multiple = TRUE
    ),
    
    hr(),
    
    # Dynamic Numeric Filter 1 Selection
    selectInput(
      "num_choice_1", 
      "Choose First Numeric Filter:", 
      choices = num_vars, 
      selected = "age"
    ),
    uiOutput("slider_1_ui"),
    
    # Dynamic Numeric Filter 2 Selection
    selectInput(
      "num_choice_2", 
      "Choose Second Numeric Filter:", 
      choices = num_vars, 
      selected = "gas_cost"
    ),
    uiOutput("slider_2_ui"),
    
    hr(),
    
    # Action button for subset isolation
    actionButton("apply_btn", "Apply Filters", class = "btn-primary w-100")
  ),
  
  # --- MAIN PANEL TABS ---
  navset_tab(
    id = "main_tabs",
    
    # TAB 1: ABOUT
    nav_panel(
      "About",
      h3("North Carolina Census PUMS Exploration"),
      p(
        "This interactive dashboard enables users to analyze demographic and financial patterns in North Carolina ",
        "using microdata from the U.S. Census Bureau's American Community Survey (ACS) Public Use Microdata Sample (PUMS)."
      ),
      h4("Data Provenance & Source"),
      p(
        "Data is queried from the 2024 1-Year ACS PUMS API endpoint for North Carolina. ",
        a("Explore Official Census PUMS API Documentation", 
          href = "https://www.census.gov/data/developers/data-sets/pums.html", 
          target = "_blank")
      ),
      h4("How to Navigate the Application"),
      tags$ul(
        tags$li(strong("Sidebar:"), " Subset by education, sex, and custom numeric ranges. Click 'Apply Filters' to execute updates."),
        tags$li(strong("About Tab:"), " App background, data links, and user directions."),
        tags$li(strong("Data Download Tab:"), " Interactive DT table displaying the filtered subset with CSV export."),
        tags$li(strong("Data Exploration Tab:"), " Dynamic tables, weighted statistics, and plots.")
      ),
      br(),
      div(
        style = "text-align: left;",
        img(src = "census_logo.svg", style = "max-width: 200px; height: auto;")
      )
    ),
    
    # TAB 2: DATA DOWNLOAD
    nav_panel(
      "Data Download",
      div(
        style = "padding: 15px 0;",
        downloadButton("download_csv", "Download Filtered CSV", class = "btn-success"),
        br(), br(),
        DT::dataTableOutput("census_table")
      )
    ),
    
    # TAB 3: DATA EXPLORATION
    nav_panel(
      "Data Exploration",
      div(
        style = "padding: 15px 0;",
        radioButtons(
          "explore_mode",
          "Summary View:",
          choices = c("Categorical Summaries", "Numeric Summaries"),
          inline = TRUE
        ),
        
        # Categorical Controls
        conditionalPanel(
          condition = "input.explore_mode == 'Categorical Summaries'",
          layout_columns(
            selectInput("cat_x_select", "Primary Categorical Variable:", choices = explore_cat_vars, selected = "education"),
            selectInput("cat_fill_select", "Grouping / Fill Variable:", choices = c("None", explore_cat_vars), selected = "sex")
          )
        ),
        
        # Numeric Controls
        conditionalPanel(
          condition = "input.explore_mode == 'Numeric Summaries'",
          layout_columns(
            selectInput("num_x_select", "Numeric Variable (X):", choices = explore_num_vars, selected = "gas_cost"),
            selectInput("num_y_select", "Second Variable (Y - Optional):", choices = c("None", explore_num_vars), selected = "rent_burden"),
            selectInput("group_select", "Group / Color By:", choices = c("None", explore_cat_vars), selected = "None"),
            selectInput("facet_select", "Facet By:", choices = c("None", explore_cat_vars), selected = "sex"),
            selectInput("num_plot_type", "Plot Type:", choices = c("Histogram", "Density", "Box plot", "Scatter", "Heatmap"))
          )
        ),
        
        hr(),
        
        card(
          card_header("Visual Plot Display"),
          withSpinner(plotOutput("explore_plot", height = "450px"), type = 6)
        ),
        
        br(),
        
        card(
          card_header("Numerical & Tabular Metrics"),
          verbatimTextOutput("summary_metrics")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Dynamic Slider 1
  output$slider_1_ui <- renderUI({
    req(input$num_choice_1)
    vals <- census_data[[input$num_choice_1]]
    min_val <- floor(min(vals, na.rm = TRUE))
    max_val <- ceiling(max(vals, na.rm = TRUE))
    
    sliderInput(
      "slider_1",
      label = paste("Range for", input$num_choice_1),
      min = min_val, max = max_val,
      value = c(min_val, max_val)
    )
  })
  
  # Dynamic Slider 2
  output$slider_2_ui <- renderUI({
    req(input$num_choice_2)
    vals <- census_data[[input$num_choice_2]]
    min_val <- floor(min(vals, na.rm = TRUE))
    max_val <- ceiling(max(vals, na.rm = TRUE))
    
    sliderInput(
      "slider_2",
      label = paste("Range for", input$num_choice_2),
      min = min_val, max = max_val,
      value = c(min_val, max_val)
    )
  })
  
  # Filtered data (only updates when the Apply button is pressed)
  filtered_data <- eventReactive(input$apply_btn, {
    req(input$slider_1, input$slider_2)
    data <- census_data
    
    # Categorical filters ('All' keeps everything)
    if (!"All" %in% input$cat_filter_1) {
      data <- data |> filter(education %in% input$cat_filter_1)
    }
    if (!"All" %in% input$cat_filter_2) {
      data <- data |> filter(sex %in% input$cat_filter_2)
    }
    
    # Numeric filters (missing values are kept so rows aren't lost by accident)
    data <- data |>
      filter(
        is.na(.data[[input$num_choice_1]]) |
          (.data[[input$num_choice_1]] >= input$slider_1[1] & .data[[input$num_choice_1]] <= input$slider_1[2]),
        is.na(.data[[input$num_choice_2]]) |
          (.data[[input$num_choice_2]] >= input$slider_2[1] & .data[[input$num_choice_2]] <= input$slider_2[2])
      )
    
    data
  }, ignoreNULL = FALSE)
  
  # Numeric data with a grouping column
  numeric_data <- reactive({
    data <- filtered_data()
    data$group <- if (input$group_select == "None") "All" else data[[input$group_select]]
    data |> drop_na(any_of(c(input$num_x_select, input$num_y_select)))
  })
  
  # --- DATA DOWNLOAD TAB ---
  output$census_table <- DT::renderDataTable({
    filtered_data()
  }, options = list(pageLength = 10, scrollX = TRUE))
  
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("nc_pums_filtered_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(filtered_data(), file)
    }
  )
  
  # --- DATA EXPLORATION TAB ---
  output$explore_plot <- renderPlot({
    
    if (input$explore_mode == "Categorical Summaries") {
      data <- filtered_data()
      x <- input$cat_x_select
      fill <- input$cat_fill_select
      validate(need(nrow(data) > 0, "No rows match your filters."))
      validate(need(x != fill, "Please pick two different variables."))
      
      # Weighted bar chart
      if (fill == "None") {
        p <- ggplot(data, aes(x = .data[[x]], weight = PWGTP)) +
          geom_bar(fill = "steelblue") +
          labs(title = paste("Weighted Counts by", x), x = x, y = "Weighted count")
      } else {
        p <- ggplot(data, aes(x = .data[[x]], fill = .data[[fill]], weight = PWGTP)) +
          geom_bar(position = "dodge") +
          labs(title = paste("Weighted Counts of", x, "by", fill), x = x, y = "Weighted count", fill = fill)
      }
      
      p + theme_minimal(base_size = 14) + 
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
      
    } else {
      data <- numeric_data()
      x <- input$num_x_select
      y <- input$num_y_select
      validate(need(nrow(data) > 0, "No rows match your filters."))
      
      if (input$num_plot_type == "Histogram") {
        p <- ggplot(data, aes(x = .data[[x]], fill = group, weight = PWGTP)) +
          geom_histogram(bins = 30, position = "identity", alpha = 0.6) +
          labs(title = paste("Weighted Histogram of", x), x = x, y = "Weighted count", fill = input$group_select)
        
      } else if (input$num_plot_type == "Density") {
        p <- ggplot(data, aes(x = .data[[x]], fill = group, weight = PWGTP)) +
          geom_density(alpha = 0.4) +
          labs(title = paste("Weighted Density of", x), x = x, y = "Density", fill = input$group_select)
        
      } else if (input$num_plot_type == "Box plot") {
        p <- ggplot(data, aes(x = group, y = .data[[x]], fill = group)) +
          geom_boxplot() +
          labs(title = paste("Box Plot of", x), x = input$group_select, y = x, fill = input$group_select)
        
      } else {
        # Scatter and Heatmap both need a Y variable
        validate(need(y != "None" && y != x, "Please choose a Y variable that is different from X."))
        
        if (input$num_plot_type == "Scatter") {
          plot_data <- slice_sample(data, n = min(5000, nrow(data)))
          p <- ggplot(plot_data, aes(x = .data[[x]], y = .data[[y]], color = group)) +
            geom_point(alpha = 0.4) +
            geom_smooth(se = FALSE) +
            labs(title = paste(y, "vs.", x), x = x, y = y, color = input$group_select)
        } else {
          p <- ggplot(data, aes(x = .data[[x]], y = .data[[y]], weight = PWGTP)) +
            geom_bin2d(bins = 30) +
            scale_fill_viridis_c() +
            labs(title = paste("Weighted Heatmap of", y, "vs.", x), x = x, y = y, fill = "Weighted count")
        }
      }
      
      # Facet if the user picked a variable
      if (input$facet_select != "None") {
        p <- p + facet_wrap(input$facet_select)
      }
      
      # Hide the legend when there is no grouping
      p <- p + theme_minimal(base_size = 14)
      if (input$group_select == "None" && input$num_plot_type != "Heatmap") {
        p <- p + theme(legend.position = "none")
      }
      p
    }
  })
  
  output$summary_metrics <- renderPrint({
    
    if (input$explore_mode == "Categorical Summaries") {
      data <- filtered_data()
      x <- input$cat_x_select
      fill <- input$cat_fill_select
      validate(need(nrow(data) > 0, "No rows match your filters."))
      validate(need(x != fill, "Please pick two different variables."))
      
      if (fill == "None") {
        cat("One-way table (weighted counts):\n")
        print(round(xtabs(as.formula(paste("PWGTP ~", x)), data = data)))
      } else {
        cat("Two-way table (weighted counts):\n")
        print(round(xtabs(as.formula(paste("PWGTP ~", x, "+", fill)), data = data)))
      }
      
    } else {
      data <- numeric_data()
      validate(need(nrow(data) > 0, "No rows match your filters."))
      
      results <- make_summary(data, input$num_x_select)
      if (input$num_y_select != "None") {
        results <- bind_rows(results, make_summary(data, input$num_y_select))
      }
      
      cat("Weighted numeric summaries:\n")
      results |>
        mutate(across(where(is.numeric), ~ round(.x, 2))) |>
        as.data.frame() |>
        print(row.names = FALSE)
    }
  })
}

shinyApp(ui, server)

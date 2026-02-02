# Server Module for Iowa Cities Dashboard
# Reactive logic, outputs, and event handlers
# ============================================

library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(tidyr)
library(stringr)
library(plotly)
library(DT)
library(here)

# =============================================================================
# SERVER FUNCTION BUILDER
# =============================================================================

#' Build server function
#' 
#' @param data List of data frames
#' @param scores Data frame of city scores
#' @param cities Character vector of city names
#' @param cache_time POSIXct cache timestamp
#' @return Shiny server function
#' @export
build_server <- function(data, scores, cities, cache_time = NULL) {
  
  # Pre-calculate state average
  state_avg <- calculate_state_average(scores)
  
  function(input, output, session) {
    
    # =========================================================================
    # INITIALIZATION
    # =========================================================================
    
    # Session logging
    logger <- create_session_logger(session)
    logger$log("SESSION_START", paste0("User agent: ", session$request$HTTP_USER_AGENT %||% "Unknown"))
    
    session$onSessionEnded(function() {
      logger$log("SESSION_END", "")
    })
    
    # Security context (load from security.R if available)
    security_ctx <- tryCatch({
      source(here("scripts/security.R"), local = TRUE)
      create_security_context(session)
    }, error = function(e) NULL)
    
    # =========================================================================
    # THEME HANDLING
    # =========================================================================
    
    # Apply saved theme on load
    observeEvent(input$saved_theme, {
      if (!is.null(input$saved_theme)) {
        updateSelectInput(session, "app_theme", selected = input$saved_theme)
      }
    }, ignoreNULL = TRUE, once = TRUE)
    
    # Theme change
    observeEvent(input$app_theme, {
      theme_name <- input$app_theme
      session$sendCustomMessage("applyTheme", theme_name)
      logger$log("THEME_CHANGE", theme_name)
    })
    
    # Current theme reactive
    current_theme <- reactive({
      input$app_theme %||% "light"
    })
    
    # =========================================================================
    # LIFESTYLE MODE HANDLING
    # =========================================================================
    
    # Track if user manually changed sliders
    user_modified_weights <- reactiveVal(FALSE)
    
    # Update sliders when lifestyle mode changes
    observeEvent(input$lifestyle_mode, {
      if (input$lifestyle_mode != "custom") {
        weights <- get_lifestyle_weights(input$lifestyle_mode)
        
        # Convert normalized weights to 0-10 scale
        max_weight <- max(weights)
        if (max_weight > 0) {
          scale_factor <- 10 / max_weight
        } else {
          scale_factor <- 1
        }
        
        updateSliderInput(session, "weight_safety", value = round(weights["safety"] * scale_factor))
        updateSliderInput(session, "weight_housing", value = round(weights["housing"] * scale_factor))
        updateSliderInput(session, "weight_education", value = round(weights["education"] * scale_factor))
        updateSliderInput(session, "weight_economy", value = round(weights["economic"] * scale_factor))
        updateSliderInput(session, "weight_healthcare", value = round(weights["healthcare"] * scale_factor))
        updateSliderInput(session, "weight_livability", value = round(weights["livability"] * scale_factor))
        updateSliderInput(session, "weight_connectivity", value = round(weights["connectivity"] * scale_factor))
        updateSliderInput(session, "weight_family", value = round(weights["family"] * scale_factor))
        updateSliderInput(session, "weight_climate", value = round(weights["climate"] * scale_factor))
        updateSliderInput(session, "weight_senior", value = round(weights["senior"] * scale_factor))
        updateSliderInput(session, "weight_pet", value = round(weights["pet"] * scale_factor))
        
        user_modified_weights(FALSE)
        logger$log("LIFESTYLE_MODE_CHANGE", input$lifestyle_mode)
      }
    }, ignoreInit = TRUE)
    
    # Switch to custom mode when user changes sliders
    observe({
      # Trigger on any weight slider change
      input$weight_safety; input$weight_housing; input$weight_education
      input$weight_economy; input$weight_healthcare; input$weight_livability
      input$weight_connectivity; input$weight_family; input$weight_climate
      input$weight_senior; input$weight_pet
      
      if (!user_modified_weights()) {
        # First change after mode selection - ignore
        user_modified_weights(TRUE)
      } else if (input$lifestyle_mode != "custom") {
        # User manually changed a slider - switch to custom
        updateSelectInput(session, "lifestyle_mode", selected = "custom")
      }
    })
    
    # Lifestyle description
    output$lifestyle_description <- renderUI({
      mode <- get_lifestyle_mode(input$lifestyle_mode)
      tags$p(class = "text-muted", icon(mode$icon), mode$description)
    })
    
    # =========================================================================
    # DATA FRESHNESS
    # =========================================================================
    
    output$data_freshness <- renderUI({
      tags$span(get_cache_freshness(cache_time))
    })
    
    # =========================================================================
    # OVERVIEW TAB
    # =========================================================================
    
    output$total_cities <- renderValueBox({
      valueBox(nrow(scores), "Cities Analyzed", icon = icon("city"), color = "blue")
    })
    
    output$total_population <- renderValueBox({
      pop <- sum(scores$population, na.rm = TRUE)
      valueBox(format_number(pop), "Total Population", icon = icon("users"), color = "green")
    })
    
    output$avg_income <- renderValueBox({
      inc <- mean(scores$median_household_income, na.rm = TRUE)
      valueBox(format_currency(inc), "Avg Income", icon = icon("dollar-sign"), color = "yellow")
    })
    
    output$top_city <- renderValueBox({
      top <- scores$city[1]
      valueBox(top, "Top Ranked City", icon = icon("trophy"), color = "purple")
    })
    
    output$overview_chart <- renderPlotly({
      theme_layout <- get_plotly_theme(current_theme())
      palette <- get_chart_palette(current_theme(), nrow(scores))
      
      p <- scores %>%
        plot_ly(x = ~reorder(city, overall_score), y = ~overall_score, 
                type = "bar", marker = list(color = ~overall_score, colorscale = "Viridis")) %>%
        layout(
          xaxis = list(title = "", tickangle = 45),
          yaxis = list(title = "Overall Score"),
          showlegend = FALSE
        ) %>%
        layout(!!!theme_layout)
      p
    }) %>% bindCache("overview_chart", current_theme())
    
    output$score_distribution <- renderPlotly({
      theme_layout <- get_plotly_theme(current_theme())
      
      scores %>%
        plot_ly(x = ~overall_score, type = "histogram", nbinsx = 10,
                marker = list(color = get_chart_palette(current_theme(), 1)[1])) %>%
        layout(xaxis = list(title = "Overall Score"), yaxis = list(title = "Count")) %>%
        layout(!!!theme_layout)
    })
    
    # =========================================================================
    # RANKINGS TAB
    # =========================================================================
    
    output$rankings_table <- renderDT({
      cat_col <- input$ranking_category
      
      # Filter out NA values for optional categories
      display_scores <- scores
      if (all(is.na(scores[[cat_col]]))) {
        return(datatable(
          tibble(Message = paste0("No data available for ", cat_col, ". This category requires additional data files.")),
          options = list(dom = 't')
        ))
      }
      
      display_scores %>%
        filter(!is.na(.data[[cat_col]])) %>%
        arrange(desc(.data[[cat_col]])) %>%
        mutate(Rank = row_number()) %>%
        select(Rank, City = city, Region = region, Population = population,
               Score = all_of(cat_col)) %>%
        mutate(Score = round(Score, 1),
               Population = format_number(Population)) %>%
        datatable(options = list(pageLength = 20))
    })
    
    # =========================================================================
    # COMPARISON TAB
    # =========================================================================
    
    output$radar_chart <- renderPlotly({
      selected_cities <- c(input$city1, input$city2)
      if (input$city3 != "None") selected_cities <- c(selected_cities, input$city3)
      
      # Include state average if checkbox is checked
      if (isTRUE(input$compare_to_state_avg)) {
        radar_data <- bind_rows(
          filter(scores, city %in% selected_cities),
          state_avg
        )
      } else {
        radar_data <- filter(scores, city %in% selected_cities)
      }
      
      radar_data <- radar_data %>%
        select(city, safety_score, housing_score, education_score, 
               economic_score, healthcare_score, livability_score_calc)
      
      categories <- c("Safety", "Housing", "Education", "Economy", "Healthcare", "Livability")
      theme_layout <- get_plotly_theme(current_theme())
      palette <- get_chart_palette(current_theme(), nrow(radar_data))
      
      p <- plot_ly(type = 'scatterpolar', mode = 'lines+markers', fill = 'toself')
      
      for (i in seq_len(nrow(radar_data))) {
        p <- p %>% add_trace(
          r = as.numeric(radar_data[i, 2:7]),
          theta = categories,
          name = radar_data$city[i],
          line = list(color = palette[i]),
          fillcolor = paste0(palette[i], "40")
        )
      }
      
      p %>%
        layout(polar = list(radialaxis = list(visible = TRUE, range = c(0, 100)))) %>%
        layout(!!!theme_layout)
    }) %>% bindCache(input$city1, input$city2, input$city3, input$compare_to_state_avg, current_theme())
    
    output$comparison_table <- renderDT({
      selected_cities <- c(input$city1, input$city2)
      if (input$city3 != "None") selected_cities <- c(selected_cities, input$city3)
      
      comp_data <- scores %>%
        filter(city %in% selected_cities) %>%
        select(city, overall_score, safety_score, housing_score, education_score,
               economic_score, healthcare_score) %>%
        pivot_longer(-city, names_to = "Metric", values_to = "Score") %>%
        pivot_wider(names_from = city, values_from = Score) %>%
        mutate(Metric = str_replace_all(Metric, "_score", "") %>% str_to_title(),
               across(where(is.numeric), ~round(., 1)))
      
      datatable(comp_data, options = list(dom = 't'))
    })
    
    output$category_winners <- renderUI({
      selected_cities <- c(input$city1, input$city2)
      if (input$city3 != "None") selected_cities <- c(selected_cities, input$city3)
      
      comp <- scores %>% filter(city %in% selected_cities)
      
      winners <- tibble(
        Category = c("Safety", "Housing", "Education", "Economy", "Healthcare", "Livability", "OVERALL"),
        Winner = c(
          comp$city[which.max(comp$safety_score)],
          comp$city[which.max(comp$housing_score)],
          comp$city[which.max(comp$education_score)],
          comp$city[which.max(comp$economic_score)],
          comp$city[which.max(comp$healthcare_score)],
          comp$city[which.max(comp$livability_score_calc)],
          comp$city[which.max(comp$overall_score)]
        )
      )
      
      tags$table(class = "table table-striped",
        tags$thead(tags$tr(tags$th("Category"), tags$th("Winner"))),
        tags$tbody(
          lapply(1:nrow(winners), function(i) {
            tags$tr(
              tags$td(winners$Category[i]),
              tags$td(tags$strong(winners$Winner[i]))
            )
          })
        )
      )
    })
    
    # =========================================================================
    # PROFILES TAB
    # =========================================================================
    
    output$profile_rank <- renderValueBox({
      city_data <- filter(scores, city == input$profile_city)
      valueBox(paste0("#", city_data$rank), "Overall Rank", icon = icon("trophy"), color = "purple")
    })
    
    output$profile_pop <- renderValueBox({
      city_data <- filter(scores, city == input$profile_city)
      valueBox(format_number(city_data$population), "Population", icon = icon("users"), color = "blue")
    })
    
    output$profile_income <- renderValueBox({
      city_data <- filter(scores, city == input$profile_city)
      valueBox(format_currency(city_data$median_household_income), 
               "Median Income", icon = icon("dollar-sign"), color = "green")
    })
    
    output$profile_safety <- renderValueBox({
      city_data <- filter(scores, city == input$profile_city)
      valueBox(round(city_data$safety_score, 1), "Safety Score", icon = icon("shield-alt"), color = "yellow")
    })
    
    output$profile_scores <- renderPlotly({
      city_data <- filter(scores, city == input$profile_city)
      theme_layout <- get_plotly_theme(current_theme())
      
      # Build score data, including optional categories if available
      score_cols <- c(
        "Safety" = "safety_score",
        "Housing" = "housing_score", 
        "Education" = "education_score",
        "Economy" = "economic_score",
        "Healthcare" = "healthcare_score",
        "Livability" = "livability_score_calc"
      )
      
      # Add optional categories if they have data
      if (!is.na(city_data$family_score)) score_cols <- c(score_cols, "Family" = "family_score")
      if (!is.na(city_data$climate_score)) score_cols <- c(score_cols, "Climate" = "climate_score")
      if (!is.na(city_data$senior_score)) score_cols <- c(score_cols, "Senior" = "senior_score")
      if (!is.na(city_data$pet_score)) score_cols <- c(score_cols, "Pet" = "pet_score")
      
      score_df <- tibble(
        Category = names(score_cols),
        Score = sapply(score_cols, function(col) city_data[[col]])
      )
      
      plot_ly(score_df, x = ~Category, y = ~Score, type = "bar",
              marker = list(color = ~Score, colorscale = "Viridis")) %>%
        layout(yaxis = list(range = c(0, 100))) %>%
        layout(!!!theme_layout)
    })
    
    output$profile_stats <- renderDT({
      city <- input$profile_city
      city_scores <- filter(scores, city == !!city)
      
      # Load benchmarks
      source(here("scripts/constants.R"), local = TRUE)
      
      stats <- tibble(
        Metric = c("Population", "Median Home Value", "Graduation Rate", 
                   "Unemployment Rate", "Life Expectancy", "Violent Crime Rate"),
        Value = c(
          format_number(city_scores$population),
          format_currency(city_scores$median_home_value),
          format_pct(city_scores$graduation_rate),
          format_pct(city_scores$unemployment_rate),
          paste0(city_scores$life_expectancy, " years"),
          round(city_scores$violent_crime_rate, 1)
        ),
        `Iowa Avg` = c(
          "—",
          format_currency(IOWA_BENCHMARKS$median_home_value),
          format_pct(IOWA_BENCHMARKS$high_school_graduation_rate),
          format_pct(IOWA_BENCHMARKS$unemployment_rate),
          paste0(IOWA_BENCHMARKS$life_expectancy, " yrs"),
          round(IOWA_BENCHMARKS$violent_crime_rate / 100, 1)
        )
      )
      
      datatable(stats, options = list(dom = 't', pageLength = 10), rownames = FALSE)
    })
    
    # =========================================================================
    # MAPS TAB
    # =========================================================================
    
    output$city_map <- renderPlotly({
      metric <- input$map_metric
      theme_layout <- get_plotly_theme(current_theme())
      
      # Filter out NA values for optional metrics
      map_data <- scores %>% filter(!is.na(.data[[metric]]))
      
      if (nrow(map_data) == 0) {
        return(plotly_empty() %>% 
                 layout(title = "No data available for this metric"))
      }
      
      plot_ly(map_data, lat = ~latitude, lon = ~longitude, 
              type = "scattergeo", mode = "markers",
              marker = list(size = ~sqrt(population) / 10, 
                            color = ~.data[[metric]], 
                            colorscale = "Viridis",
                            showscale = TRUE),
              text = ~paste(city, "<br>", metric, ":", round(.data[[metric]], 1)),
              hoverinfo = "text") %>%
        layout(geo = list(
          scope = "usa",
          projection = list(type = "albers usa"),
          center = list(lon = -93.5, lat = 42),
          lonaxis = list(range = c(-97, -90)),
          lataxis = list(range = c(40, 44)),
          bgcolor = get_theme(current_theme())$chart$bg
        )) %>%
        layout(!!!theme_layout)
    }) %>% bindCache(input$map_metric, current_theme())
    
    # =========================================================================
    # RECOMMENDATIONS TAB
    # =========================================================================
    
    # Collect current weights
    current_weights <- reactive({
      list(
        safety = input$weight_safety,
        housing = input$weight_housing,
        education = input$weight_education,
        economic = input$weight_economy,
        healthcare = input$weight_healthcare,
        livability = input$weight_livability,
        connectivity = input$weight_connectivity,
        family = input$weight_family,
        climate = input$weight_climate,
        senior = input$weight_senior,
        pet = input$weight_pet
      )
    })
    
    recommendations <- eventReactive(input$get_recommendations, {
      weights <- sliders_to_weights(current_weights())
      
      logger$log("RECOMMENDATION_REQUEST", paste0(
        "Mode: ", input$lifestyle_mode,
        " Weights: ", paste(names(weights), round(weights, 2), sep = "=", collapse = ", ")
      ))
      
      scores %>%
        mutate(custom_score = calculate_custom_score(., weights)) %>%
        arrange(desc(custom_score)) %>%
        head(5)
    }) %>%
      bindCache(
        input$weight_safety, input$weight_housing, input$weight_education,
        input$weight_economy, input$weight_healthcare, input$weight_livability,
        input$weight_connectivity, input$weight_family, input$weight_climate,
        input$weight_senior, input$weight_pet
      )
    
    output$recommendations_table <- renderDT({
      req(input$get_recommendations)
      
      recommendations() %>%
        mutate(Rank = row_number()) %>%
        select(Rank, City = city, Region = region, 
               `Your Score` = custom_score, Population = population) %>%
        mutate(`Your Score` = round(`Your Score`, 1),
               Population = format_number(Population)) %>%
        datatable(options = list(dom = 't'))
    })
    
    output$top_recommendation <- renderUI({
      req(input$get_recommendations)
      top <- recommendations()$city[1]
      score <- round(recommendations()$custom_score[1], 1)
      
      div(class = "alert alert-success",
          tags$h4(icon("star"), " Your Best Match: ", tags$strong(top)),
          tags$p(paste0("Based on your priorities, ", top, " scores ", score, " points."))
      )
    })
    
    # Share functionality
    share_url <- reactiveVal(NULL)
    
    observeEvent(input$share_results, {
      req(input$get_recommendations)
      
      base_url <- session$clientData$url_protocol
      base_url <- paste0(base_url, "//", session$clientData$url_hostname)
      if (!is.null(session$clientData$url_port) && session$clientData$url_port != "") {
        base_url <- paste0(base_url, ":", session$clientData$url_port)
      }
      base_url <- paste0(base_url, session$clientData$url_pathname)
      
      url <- generate_share_url(base_url, current_weights(), input$lifestyle_mode)
      share_url(url)
      
      logger$log("SHARE_URL_GENERATED", url)
    })
    
    output$share_url_display <- renderUI({
      req(share_url())
      div(class = "share-url",
          tags$strong("Share this link: "),
          tags$br(),
          tags$code(share_url()),
          actionButton("copy_url", "Copy", class = "btn-sm btn-default", 
                       onclick = sprintf("navigator.clipboard.writeText('%s')", share_url()))
      )
    })
    
    # =========================================================================
    # TRENDS TAB
    # =========================================================================
    
    output$trend_chart <- renderPlotly({
      city <- input$trend_city
      metric <- input$trend_metric
      theme_layout <- get_plotly_theme(current_theme())
      
      hist <- data$historical %>% filter(city == !!city)
      
      if (metric == "population") {
        years <- c(1980, 1990, 2000, 2005, 2010, 2015, 2020)
        values <- c(hist$population_1980, hist$population_1990, hist$population_2000,
                    hist$population_2005, hist$population_2010, hist$population_2015, hist$population_2020)
        title <- "Population Trend"
      } else if (metric == "home_value") {
        years <- c(2010, 2015, 2020)
        values <- c(hist$median_home_value_2010, hist$median_home_value_2015, hist$median_home_value_2020)
        title <- "Median Home Value Trend"
      } else if (metric == "income") {
        years <- c(2010, 2015, 2020)
        values <- c(hist$median_income_2010, hist$median_income_2015, hist$median_income_2020)
        title <- "Median Income Trend"
      } else {
        years <- c(2010, 2015, 2020)
        values <- c(hist$unemployment_2010, hist$unemployment_2015, hist$unemployment_2020)
        title <- "Unemployment Rate Trend"
      }
      
      palette <- get_chart_palette(current_theme(), 1)
      
      plot_ly(x = years, y = values, type = "scatter", mode = "lines+markers",
              line = list(color = palette[1], width = 3),
              marker = list(size = 10, color = palette[1])) %>%
        layout(title = title, xaxis = list(title = "Year"), yaxis = list(title = "")) %>%
        layout(!!!theme_layout)
    })
    
    output$trend_summary <- renderUI({
      city <- input$trend_city
      hist <- data$historical %>% filter(city == !!city)
      
      div(
        tags$h4("Growth Summary"),
        tags$ul(
          tags$li(paste0("10-Year Pop Growth: ", hist$population_growth_10yr_pct, "%")),
          tags$li(paste0("20-Year Pop Growth: ", hist$population_growth_20yr_pct, "%")),
          tags$li(paste0("Home Value Growth: ", hist$home_value_growth_10yr_pct, "%")),
          tags$li(paste0("Income Growth: ", hist$income_growth_10yr_pct, "%")),
          tags$li(paste0("Crime Change: ", hist$crime_change_10yr_pct, "%"))
        )
      )
    })
    
    # =========================================================================
    # DOWNLOAD HANDLERS
    # =========================================================================
    
    output$download_data <- downloadHandler(
      filename = function() {
        city <- input$profile_city
        logger$log("DOWNLOAD_CSV", paste0("City: ", city))
        paste0(gsub(" ", "_", tolower(city)), "_profile_", Sys.Date(), ".csv")
      },
      content = function(file) {
        city <- input$profile_city
        city_data <- filter(scores, city == !!city)
        write_csv(city_data, file)
      }
    )
    
    output$download_report <- downloadHandler(
      filename = function() {
        city <- input$profile_city
        logger$log("DOWNLOAD_REPORT", paste0("City: ", city))
        paste0(gsub(" ", "_", tolower(city)), "_report_", Sys.Date(), ".html")
      },
      content = function(file) {
        city <- input$profile_city
        city_data <- filter(scores, city == !!city)
        
        # Generate simple HTML report
        html_content <- paste0(
          "<!DOCTYPE html><html><head><style>",
          "body { font-family: Arial, sans-serif; margin: 40px; }",
          "h1 { color: #2c3e50; } h2 { color: #3498db; }",
          "table { border-collapse: collapse; width: 100%; margin: 20px 0; }",
          "th, td { border: 1px solid #ddd; padding: 12px; text-align: left; }",
          "th { background-color: #3498db; color: white; }",
          "</style></head><body>",
          "<h1>", city, " City Profile</h1>",
          "<p>Generated: ", format(Sys.Date(), "%B %d, %Y"), "</p>",
          "<h2>Scores</h2><table><tr><th>Category</th><th>Score</th></tr>",
          "<tr><td>Overall</td><td>", round(city_data$overall_score, 1), "</td></tr>",
          "<tr><td>Safety</td><td>", round(city_data$safety_score, 1), "</td></tr>",
          "<tr><td>Housing</td><td>", round(city_data$housing_score, 1), "</td></tr>",
          "<tr><td>Education</td><td>", round(city_data$education_score, 1), "</td></tr>",
          "<tr><td>Economy</td><td>", round(city_data$economic_score, 1), "</td></tr>",
          "<tr><td>Healthcare</td><td>", round(city_data$healthcare_score, 1), "</td></tr>",
          "</table></body></html>"
        )
        writeLines(html_content, file)
      }
    )
    
    output$download_profile_pdf <- downloadHandler(
      filename = function() {
        city <- input$profile_city
        logger$log("DOWNLOAD_PDF", paste0("City: ", city))
        paste0(gsub(" ", "_", tolower(city)), "_profile_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        city <- input$profile_city
        city_data <- filter(scores, city == !!city)
        
        # Render the PDF template
        tryCatch({
          rmarkdown::render(
            here("notebooks/city_profile_export.Rmd"),
            output_file = file,
            params = list(
              city = city,
              city_data = city_data,
              scores = scores
            ),
            envir = new.env(parent = globalenv())
          )
        }, error = function(e) {
          # Fallback to simple text file if Rmd fails
          writeLines(paste0("City Profile: ", city, "\n\nError generating PDF. Please try HTML export."), file)
        })
      }
    )
    
    output$download_rankings <- downloadHandler(
      filename = function() {
        cat_name <- gsub("_score", "", input$ranking_category)
        logger$log("DOWNLOAD_RANKINGS", paste0("Category: ", cat_name))
        paste0("iowa_rankings_", cat_name, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        cat_col <- input$ranking_category
        
        rankings <- scores %>%
          filter(!is.na(.data[[cat_col]])) %>%
          arrange(desc(.data[[cat_col]])) %>%
          mutate(Rank = row_number()) %>%
          select(Rank, City = city, Region = region, Population = population,
                 Score = all_of(cat_col)) %>%
          mutate(Score = round(Score, 1))
        
        write_csv(rankings, file)
      }
    )
    
    output$download_all_data <- downloadHandler(
      filename = function() {
        logger$log("DOWNLOAD_BULK", "All cities data")
        paste0("iowa_cities_complete_", Sys.Date(), ".csv")
      },
      content = function(file) {
        export_data <- scores %>%
          select(
            City = city, Region = region, Population = population, Rank = rank,
            Overall_Score = overall_score, Safety_Score = safety_score,
            Housing_Score = housing_score, Education_Score = education_score,
            Economic_Score = economic_score, Healthcare_Score = healthcare_score,
            Livability_Score = livability_score_calc
          ) %>%
          mutate(across(where(is.numeric), ~round(., 2)))
        
        write_csv(export_data, file)
      }
    )
    
  }
}

cat("✓ Server module loaded\n")

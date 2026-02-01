# Iowa Cities Interactive Dashboard
# Shiny app for exploring Iowa city data
# ========================================

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(here)

# =============================================================================
# LOAD DATA
# =============================================================================

load_all_data <- function() {
  list(
    crime = read_csv(here("data/raw/iowa_crime_data.csv"), show_col_types = FALSE),
    housing = read_csv(here("data/raw/iowa_housing_data.csv"), show_col_types = FALSE),
    education = read_csv(here("data/raw/iowa_education_data.csv"), show_col_types = FALSE),
    economic = read_csv(here("data/raw/iowa_economic_data.csv"), show_col_types = FALSE),
    healthcare = read_csv(here("data/raw/iowa_healthcare_data.csv"), show_col_types = FALSE),
    demographics = read_csv(here("data/raw/iowa_demographics_data.csv"), show_col_types = FALSE),
    environment = read_csv(here("data/raw/iowa_environment_data.csv"), show_col_types = FALSE),
    amenities = read_csv(here("data/raw/iowa_amenities_data.csv"), show_col_types = FALSE),
    historical = read_csv(here("data/raw/iowa_historical_data.csv"), show_col_types = FALSE),
    major_cities = read_csv(here("data/raw/iowa_major_cities.csv"), show_col_types = FALSE)
  )
}

data <- load_all_data()
cities <- sort(unique(data$major_cities$city))

# Helper function to normalize scores
normalize <- function(x, reverse = FALSE) {
  if (all(is.na(x))) return(x)
  scaled <- (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) * 100
  if (reverse) scaled <- 100 - scaled
  return(scaled)
}

# Calculate scores for all cities
calculate_scores <- function(data) {
  scores <- data$major_cities %>%
    select(city, county, population = population_2020, latitude, longitude, region) %>%
    left_join(data$crime %>% select(city, violent_crime_rate, property_crime_rate), by = "city") %>%
    left_join(data$housing %>% select(city, median_home_value, owner_occupied_pct), by = "city") %>%
    left_join(data$education %>% select(city, graduation_rate, college_readiness_pct, pct_bachelors), by = "city") %>%
    left_join(data$economic %>% select(city, median_household_income, unemployment_rate, poverty_rate), by = "city") %>%
    left_join(data$healthcare %>% select(city, life_expectancy, health_insurance_coverage_pct), by = "city") %>%
    left_join(data$amenities %>% select(city, livability_score, cost_of_living_index), by = "city") %>%
    mutate(
      safety_score = (normalize(violent_crime_rate, TRUE) + normalize(property_crime_rate, TRUE)) / 2,
      housing_score = (normalize(median_home_value, TRUE) + normalize(owner_occupied_pct)) / 2,
      education_score = (normalize(graduation_rate) + normalize(college_readiness_pct) + normalize(pct_bachelors)) / 3,
      economic_score = (normalize(median_household_income) + normalize(unemployment_rate, TRUE) + normalize(poverty_rate, TRUE)) / 3,
      healthcare_score = (normalize(life_expectancy) + normalize(health_insurance_coverage_pct)) / 2,
      livability_score_calc = normalize(livability_score),
      overall_score = (safety_score + housing_score + education_score + economic_score + healthcare_score + livability_score_calc) / 6
    ) %>%
    arrange(desc(overall_score)) %>%
    mutate(rank = row_number())
  
  return(scores)
}

scores <- calculate_scores(data)

# =============================================================================
# UI
# =============================================================================

ui <- dashboardPage(
  dashboardHeader(title = "Iowa Cities Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("City Rankings", tabName = "rankings", icon = icon("trophy")),
      menuItem("City Comparison", tabName = "compare", icon = icon("balance-scale")),
      menuItem("City Profiles", tabName = "profiles", icon = icon("city")),
      menuItem("Maps", tabName = "maps", icon = icon("map")),
      menuItem("Recommendations", tabName = "recommend", icon = icon("star")),
      menuItem("Trends", tabName = "trends", icon = icon("chart-line"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
        fluidRow(
          valueBoxOutput("total_cities", width = 3),
          valueBoxOutput("total_population", width = 3),
          valueBoxOutput("avg_income", width = 3),
          valueBoxOutput("top_city", width = 3)
        ),
        fluidRow(
          box(title = "Overall City Scores", status = "primary", solidHeader = TRUE, width = 8,
              plotlyOutput("overview_chart", height = 400)),
          box(title = "Score Distribution", status = "info", solidHeader = TRUE, width = 4,
              plotlyOutput("score_distribution", height = 400))
        )
      ),
      
      # Rankings Tab
      tabItem(tabName = "rankings",
        fluidRow(
          box(title = "City Rankings by Category", status = "primary", solidHeader = TRUE, width = 12,
              selectInput("ranking_category", "Select Category:",
                          choices = c("Overall" = "overall_score",
                                      "Safety" = "safety_score",
                                      "Housing" = "housing_score",
                                      "Education" = "education_score",
                                      "Economy" = "economic_score",
                                      "Healthcare" = "healthcare_score",
                                      "Livability" = "livability_score_calc")),
              DTOutput("rankings_table"))
        )
      ),
      
      # Comparison Tab
      tabItem(tabName = "compare",
        fluidRow(
          box(title = "Select Cities to Compare", status = "primary", solidHeader = TRUE, width = 12,
              fluidRow(
                column(4, selectInput("city1", "City 1:", choices = cities, selected = "Bettendorf")),
                column(4, selectInput("city2", "City 2:", choices = cities, selected = "Ankeny")),
                column(4, selectInput("city3", "City 3:", choices = c("None", cities), selected = "None"))
              ))
        ),
        fluidRow(
          box(title = "Radar Comparison", status = "info", solidHeader = TRUE, width = 6,
              plotlyOutput("radar_chart", height = 400)),
          box(title = "Side-by-Side Metrics", status = "success", solidHeader = TRUE, width = 6,
              DTOutput("comparison_table"))
        ),
        fluidRow(
          box(title = "Category Winners", status = "warning", solidHeader = TRUE, width = 12,
              uiOutput("category_winners"))
        )
      ),
      
      # Profiles Tab
      tabItem(tabName = "profiles",
        fluidRow(
          box(title = "City Profile", status = "primary", solidHeader = TRUE, width = 12,
              selectInput("profile_city", "Select City:", choices = cities, selected = "Des Moines"))
        ),
        fluidRow(
          valueBoxOutput("profile_rank", width = 3),
          valueBoxOutput("profile_pop", width = 3),
          valueBoxOutput("profile_income", width = 3),
          valueBoxOutput("profile_safety", width = 3)
        ),
        fluidRow(
          box(title = "Score Breakdown", status = "info", solidHeader = TRUE, width = 6,
              plotlyOutput("profile_scores", height = 300)),
          box(title = "Key Statistics", status = "success", solidHeader = TRUE, width = 6,
              DTOutput("profile_stats"))
        )
      ),
      
      # Maps Tab
      tabItem(tabName = "maps",
        fluidRow(
          box(title = "Iowa Cities Map", status = "primary", solidHeader = TRUE, width = 12,
              selectInput("map_metric", "Color by:",
                          choices = c("Overall Score" = "overall_score",
                                      "Population" = "population",
                                      "Median Income" = "median_household_income",
                                      "Safety Score" = "safety_score",
                                      "Education Score" = "education_score")),
              plotlyOutput("city_map", height = 500))
        )
      ),
      
      # Recommendations Tab
      tabItem(tabName = "recommend",
        fluidRow(
          box(title = "What's Important to You?", status = "primary", solidHeader = TRUE, width = 4,
              sliderInput("weight_safety", "Safety:", 1, 10, 5),
              sliderInput("weight_education", "Education:", 1, 10, 5),
              sliderInput("weight_economy", "Economy/Jobs:", 1, 10, 5),
              sliderInput("weight_housing", "Housing Affordability:", 1, 10, 5),
              sliderInput("weight_healthcare", "Healthcare:", 1, 10, 5),
              sliderInput("weight_livability", "Livability/Amenities:", 1, 10, 5),
              actionButton("get_recommendations", "Get Recommendations", class = "btn-primary")
          ),
          box(title = "Your Top 5 Cities", status = "success", solidHeader = TRUE, width = 8,
              DTOutput("recommendations_table"),
              br(),
              uiOutput("top_recommendation"))
        )
      ),
      
      # Trends Tab
      tabItem(tabName = "trends",
        fluidRow(
          box(title = "Historical Trends", status = "primary", solidHeader = TRUE, width = 12,
              selectInput("trend_city", "Select City:", choices = cities, selected = "Des Moines"),
              selectInput("trend_metric", "Select Metric:",
                          choices = c("Population" = "population",
                                      "Home Value" = "home_value",
                                      "Income" = "income",
                                      "Unemployment" = "unemployment")))
        ),
        fluidRow(
          box(title = "Trend Chart", status = "info", solidHeader = TRUE, width = 8,
              plotlyOutput("trend_chart", height = 400)),
          box(title = "Growth Summary", status = "success", solidHeader = TRUE, width = 4,
              uiOutput("trend_summary"))
        )
      )
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {
  
  # Overview Value Boxes
  output$total_cities <- renderValueBox({
    valueBox(nrow(scores), "Cities Analyzed", icon = icon("city"), color = "blue")
  })
  
  output$total_population <- renderValueBox({
    pop <- sum(scores$population, na.rm = TRUE)
    valueBox(format(pop, big.mark = ","), "Total Population", icon = icon("users"), color = "green")
  })
  
  output$avg_income <- renderValueBox({
    inc <- mean(scores$median_household_income, na.rm = TRUE)
    valueBox(paste0("$", format(round(inc), big.mark = ",")), "Avg Income", icon = icon("dollar-sign"), color = "yellow")
  })
  
  output$top_city <- renderValueBox({
    top <- scores$city[1]
    valueBox(top, "Top Ranked City", icon = icon("trophy"), color = "purple")
  })
  
  # Overview Chart
  output$overview_chart <- renderPlotly({
    p <- scores %>%
      plot_ly(x = ~reorder(city, overall_score), y = ~overall_score, 
              type = "bar", marker = list(color = ~overall_score, colorscale = "Viridis")) %>%
      layout(xaxis = list(title = "", tickangle = 45),
             yaxis = list(title = "Overall Score"),
             showlegend = FALSE)
    p
  })
  
  # Score Distribution
  output$score_distribution <- renderPlotly({
    scores %>%
      plot_ly(x = ~overall_score, type = "histogram", nbinsx = 10,
              marker = list(color = "steelblue")) %>%
      layout(xaxis = list(title = "Overall Score"),
             yaxis = list(title = "Count"))
  })
  
  # Rankings Table
  output$rankings_table <- renderDT({
    cat_col <- input$ranking_category
    
    scores %>%
      arrange(desc(.data[[cat_col]])) %>%
      mutate(Rank = row_number()) %>%
      select(Rank, City = city, Region = region, Population = population,
             Score = all_of(cat_col)) %>%
      mutate(Score = round(Score, 1),
             Population = format(Population, big.mark = ",")) %>%
      datatable(options = list(pageLength = 20))
  })
  
  # Comparison Radar Chart
  output$radar_chart <- renderPlotly({
    selected_cities <- c(input$city1, input$city2)
    if (input$city3 != "None") selected_cities <- c(selected_cities, input$city3)
    
    radar_data <- scores %>%
      filter(city %in% selected_cities) %>%
      select(city, safety_score, housing_score, education_score, 
             economic_score, healthcare_score, livability_score_calc)
    
    categories <- c("Safety", "Housing", "Education", "Economy", "Healthcare", "Livability")
    
    plot_ly(type = 'scatterpolar', mode = 'lines+markers', fill = 'toself') %>%
      add_trace(
        r = as.numeric(radar_data[1, 2:7]),
        theta = categories,
        name = radar_data$city[1]
      ) %>%
      add_trace(
        r = as.numeric(radar_data[2, 2:7]),
        theta = categories,
        name = radar_data$city[2]
      ) %>%
      {if (nrow(radar_data) > 2) add_trace(., r = as.numeric(radar_data[3, 2:7]), theta = categories, name = radar_data$city[3]) else .} %>%
      layout(polar = list(radialaxis = list(visible = TRUE, range = c(0, 100))))
  })
  
  # Comparison Table
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
  
  # Category Winners
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
  
  # Profile Value Boxes
  output$profile_rank <- renderValueBox({
    city_data <- scores %>% filter(city == input$profile_city)
    valueBox(paste0("#", city_data$rank), "Overall Rank", icon = icon("trophy"), color = "purple")
  })
  
  output$profile_pop <- renderValueBox({
    city_data <- scores %>% filter(city == input$profile_city)
    valueBox(format(city_data$population, big.mark = ","), "Population", icon = icon("users"), color = "blue")
  })
  
  output$profile_income <- renderValueBox({
    city_data <- scores %>% filter(city == input$profile_city)
    valueBox(paste0("$", format(city_data$median_household_income, big.mark = ",")), 
             "Median Income", icon = icon("dollar-sign"), color = "green")
  })
  
  output$profile_safety <- renderValueBox({
    city_data <- scores %>% filter(city == input$profile_city)
    valueBox(round(city_data$safety_score, 1), "Safety Score", icon = icon("shield-alt"), color = "yellow")
  })
  
  # Profile Scores Chart
  output$profile_scores <- renderPlotly({
    city_data <- scores %>% filter(city == input$profile_city)
    
    score_df <- tibble(
      Category = c("Safety", "Housing", "Education", "Economy", "Healthcare", "Livability"),
      Score = c(city_data$safety_score, city_data$housing_score, city_data$education_score,
                city_data$economic_score, city_data$healthcare_score, city_data$livability_score_calc)
    )
    
    plot_ly(score_df, x = ~Category, y = ~Score, type = "bar",
            marker = list(color = ~Score, colorscale = "Viridis")) %>%
      layout(yaxis = list(range = c(0, 100)))
  })
  
  # Profile Stats Table
  output$profile_stats <- renderDT({
    city <- input$profile_city
    
    stats <- tibble(
      Metric = c("Population", "Median Home Value", "Graduation Rate", 
                 "Unemployment Rate", "Life Expectancy", "Violent Crime Rate"),
      Value = c(
        format(scores$population[scores$city == city], big.mark = ","),
        paste0("$", format(scores$median_home_value[scores$city == city], big.mark = ",")),
        paste0(scores$graduation_rate[scores$city == city], "%"),
        paste0(scores$unemployment_rate[scores$city == city], "%"),
        paste0(scores$life_expectancy[scores$city == city], " years"),
        scores$violent_crime_rate[scores$city == city]
      )
    )
    
    datatable(stats, options = list(dom = 't', pageLength = 10))
  })
  
  # City Map
  output$city_map <- renderPlotly({
    metric <- input$map_metric
    
    plot_ly(scores, lat = ~latitude, lon = ~longitude, 
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
        lataxis = list(range = c(40, 44))
      ))
  })
  
  # Recommendations
  recommendations <- eventReactive(input$get_recommendations, {
    weights <- c(
      safety = input$weight_safety,
      education = input$weight_education,
      economy = input$weight_economy,
      housing = input$weight_housing,
      healthcare = input$weight_healthcare,
      livability = input$weight_livability
    )
    weights <- weights / sum(weights)
    
    scores %>%
      mutate(
        custom_score = safety_score * weights["safety"] +
                       education_score * weights["education"] +
                       economic_score * weights["economy"] +
                       housing_score * weights["housing"] +
                       healthcare_score * weights["healthcare"] +
                       livability_score_calc * weights["livability"]
      ) %>%
      arrange(desc(custom_score)) %>%
      head(5)
  })
  
  output$recommendations_table <- renderDT({
    req(input$get_recommendations)
    
    recommendations() %>%
      mutate(Rank = row_number()) %>%
      select(Rank, City = city, Region = region, 
             `Your Score` = custom_score, Population = population) %>%
      mutate(`Your Score` = round(`Your Score`, 1),
             Population = format(Population, big.mark = ",")) %>%
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
  
  # Trends
  output$trend_chart <- renderPlotly({
    city <- input$trend_city
    metric <- input$trend_metric
    
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
    
    plot_ly(x = years, y = values, type = "scatter", mode = "lines+markers",
            line = list(color = "steelblue", width = 3),
            marker = list(size = 10)) %>%
      layout(title = title, xaxis = list(title = "Year"), yaxis = list(title = ""))
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
}

# =============================================================================
# RUN APP
# =============================================================================

shinyApp(ui, server)

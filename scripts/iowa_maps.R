# Iowa Cities Geographic Visualizations
# Interactive maps using leaflet
# =====================================

library(tidyverse)
library(leaflet)
library(htmlwidgets)
library(here)
library(RColorBrewer)

cat("
╔═══════════════════════════════════════════════════════════════╗
║              IOWA CITIES MAP VISUALIZATIONS                   ║
╚═══════════════════════════════════════════════════════════════╝
\n")

# =============================================================================
# 1. LOAD AND PREPARE DATA
# =============================================================================

cat("[1/5] Loading data...\n")

major_cities <- read_csv(here("data/raw/iowa_major_cities.csv"), show_col_types = FALSE)
economic <- read_csv(here("data/raw/iowa_economic_data.csv"), show_col_types = FALSE)
crime <- read_csv(here("data/raw/iowa_crime_data.csv"), show_col_types = FALSE)
education <- read_csv(here("data/raw/iowa_education_data.csv"), show_col_types = FALSE)
housing <- read_csv(here("data/raw/iowa_housing_data.csv"), show_col_types = FALSE)
healthcare <- read_csv(here("data/raw/iowa_healthcare_data.csv"), show_col_types = FALSE)

# Merge data
map_data <- major_cities %>%
  left_join(economic %>% select(city, median_household_income, unemployment_rate, poverty_rate), by = "city") %>%
  left_join(crime %>% select(city, violent_crime_rate, property_crime_rate), by = "city") %>%
  left_join(education %>% select(city, graduation_rate, pct_bachelors), by = "city") %>%
  left_join(housing %>% select(city, median_home_value), by = "city") %>%
  left_join(healthcare %>% select(city, life_expectancy), by = "city")

# =============================================================================
# 2. POPULATION MAP
# =============================================================================

cat("[2/5] Creating population map...\n")

# Color palette based on population
pop_pal <- colorNumeric(
  palette = "YlOrRd",
  domain = map_data$population_2020
)

population_map <- leaflet(map_data) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng = -93.5, lat = 42, zoom = 7) %>%
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    radius = ~sqrt(population_2020) / 50,
    color = ~pop_pal(population_2020),
    fillOpacity = 0.7,
    stroke = TRUE,
    weight = 2,
    popup = ~paste0(
      "<strong>", city, "</strong><br>",
      "Population: ", format(population_2020, big.mark = ","), "<br>",
      "County: ", county, "<br>",
      "Region: ", region
    ),
    label = ~city
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pop_pal,
    values = ~population_2020,
    title = "Population",
    opacity = 0.7
  )

# Save map
saveWidget(population_map, here("outputs/iowa_population_map.html"), selfcontained = TRUE)
cat("  ✓ Saved iowa_population_map.html\n")

# =============================================================================
# 3. INCOME MAP
# =============================================================================

cat("[3/5] Creating income map...\n")

income_pal <- colorNumeric(
  palette = "Greens",
  domain = map_data$median_household_income
)

income_map <- leaflet(map_data) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng = -93.5, lat = 42, zoom = 7) %>%
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    radius = 12,
    color = ~income_pal(median_household_income),
    fillOpacity = 0.8,
    stroke = TRUE,
    weight = 2,
    popup = ~paste0(
      "<strong>", city, "</strong><br>",
      "Median Income: $", format(median_household_income, big.mark = ","), "<br>",
      "Poverty Rate: ", poverty_rate, "%<br>",
      "Unemployment: ", unemployment_rate, "%"
    ),
    label = ~paste0(city, ": $", format(median_household_income, big.mark = ","))
  ) %>%
  addLegend(
    position = "bottomright",
    pal = income_pal,
    values = ~median_household_income,
    title = "Median Income",
    labFormat = labelFormat(prefix = "$"),
    opacity = 0.7
  )

saveWidget(income_map, here("outputs/iowa_income_map.html"), selfcontained = TRUE)
cat("  ✓ Saved iowa_income_map.html\n")

# =============================================================================
# 4. SAFETY MAP
# =============================================================================

cat("[4/5] Creating safety map...\n")

# Reverse palette - lower crime = greener
safety_pal <- colorNumeric(
  palette = "RdYlGn",
  domain = c(max(map_data$violent_crime_rate), min(map_data$violent_crime_rate)),
  reverse = TRUE
)

safety_map <- leaflet(map_data) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng = -93.5, lat = 42, zoom = 7) %>%
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    radius = 12,
    color = ~safety_pal(violent_crime_rate),
    fillOpacity = 0.8,
    stroke = TRUE,
    weight = 2,
    popup = ~paste0(
      "<strong>", city, "</strong><br>",
      "Violent Crime Rate: ", violent_crime_rate, " per 100k<br>",
      "Property Crime Rate: ", property_crime_rate, " per 100k"
    ),
    label = ~paste0(city, ": ", violent_crime_rate)
  ) %>%
  addLegend(
    position = "bottomright",
    pal = safety_pal,
    values = ~violent_crime_rate,
    title = "Violent Crime<br>(per 100k)",
    opacity = 0.7
  )

saveWidget(safety_map, here("outputs/iowa_safety_map.html"), selfcontained = TRUE)
cat("  ✓ Saved iowa_safety_map.html\n")

# =============================================================================
# 5. COMPREHENSIVE MAP WITH ALL LAYERS
# =============================================================================

cat("[5/5] Creating comprehensive multi-layer map...\n")

# Create icon for markers
city_icon <- awesomeIcons(
  icon = "home",
  iconColor = "white",
  markerColor = "blue",
  library = "fa"
)

comprehensive_map <- leaflet(map_data) %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  addProviderTiles(providers$OpenStreetMap, group = "Streets") %>%
  setView(lng = -93.5, lat = 42, zoom = 7) %>%
  
  # Population layer
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    radius = ~sqrt(population_2020) / 40,
    color = ~pop_pal(population_2020),
    fillOpacity = 0.6,
    stroke = TRUE,
    weight = 1,
    group = "Population",
    popup = ~paste0(
      "<h4>", city, "</h4>",
      "<table style='width:100%'>",
      "<tr><td><b>Population:</b></td><td>", format(population_2020, big.mark = ","), "</td></tr>",
      "<tr><td><b>Median Income:</b></td><td>$", format(median_household_income, big.mark = ","), "</td></tr>",
      "<tr><td><b>Home Value:</b></td><td>$", format(median_home_value, big.mark = ","), "</td></tr>",
      "<tr><td><b>Graduation Rate:</b></td><td>", graduation_rate, "%</td></tr>",
      "<tr><td><b>Crime Rate:</b></td><td>", violent_crime_rate, "</td></tr>",
      "<tr><td><b>Life Expectancy:</b></td><td>", life_expectancy, " years</td></tr>",
      "</table>"
    )
  ) %>%
  
  # Income layer
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    radius = 10,
    color = ~income_pal(median_household_income),
    fillOpacity = 0.7,
    stroke = TRUE,
    weight = 1,
    group = "Income",
    label = ~paste0(city, ": $", format(median_household_income, big.mark = ","))
  ) %>%
  
  # Safety layer
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    radius = 10,
    color = ~safety_pal(violent_crime_rate),
    fillOpacity = 0.7,
    stroke = TRUE,
    weight = 1,
    group = "Safety",
    label = ~paste0(city, ": ", violent_crime_rate, " violent crimes/100k")
  ) %>%
  
  # Region markers
  addAwesomeMarkers(
    lng = ~longitude,
    lat = ~latitude,
    icon = city_icon,
    group = "City Labels",
    label = ~city
  ) %>%
  
  # Layer controls
  addLayersControl(
    baseGroups = c("Light", "Satellite", "Streets"),
    overlayGroups = c("Population", "Income", "Safety", "City Labels"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  # Legends
  addLegend(
    position = "bottomright",
    pal = pop_pal,
    values = ~population_2020,
    title = "Population",
    group = "Population",
    opacity = 0.7
  ) %>%
  
  hideGroup(c("Income", "Safety", "City Labels"))

saveWidget(comprehensive_map, here("outputs/iowa_comprehensive_map.html"), selfcontained = TRUE)
cat("  ✓ Saved iowa_comprehensive_map.html\n")

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n")
cat(rep("═", 50), "\n", sep = "")
cat("MAP GENERATION COMPLETE\n")
cat(rep("═", 50), "\n", sep = "")
cat("Created 4 interactive maps:\n")
cat("  1. iowa_population_map.html\n")
cat("  2. iowa_income_map.html\n")
cat("  3. iowa_safety_map.html\n")
cat("  4. iowa_comprehensive_map.html (multi-layer)\n")
cat("\nOpen in browser to explore!\n")
cat(rep("═", 50), "\n", sep = "")

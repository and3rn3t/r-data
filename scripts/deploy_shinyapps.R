# Deploy Iowa Cities Dashboard to shinyapps.io
# 
# Prerequisites:
# 1. Create a free account at https://www.shinyapps.io/
# 2. Go to Account > Tokens to get your token and secret
# 3. Run the setAccountInfo() command below with your credentials

library(rsconnect)

# ============================================
# STEP 1: Configure your shinyapps.io account
# ============================================
# Uncomment and run this ONCE with your credentials from shinyapps.io:
#
# rsconnect::setAccountInfo(
#   name   = "YOUR_ACCOUNT_NAME",
#   token  = "YOUR_TOKEN",
#   secret = "YOUR_SECRET"
# )

# ============================================
# STEP 2: Verify account is configured
# ============================================
accounts <- rsconnect::accounts()
if (nrow(accounts) == 0) {
  stop("No shinyapps.io account configured. Please run setAccountInfo() first.")
}
cat("Configured accounts:\n")
print(accounts)

# ============================================
# STEP 3: Deploy the application
# ============================================
# The app will be deployed to: https://YOUR_ACCOUNT.shinyapps.io/iowa-cities-dashboard

deploy_app <- function(app_name = "iowa-cities-dashboard") {
  cat("\nDeploying Iowa Cities Dashboard to shinyapps.io...\n")
  cat("This may take several minutes.\n\n")
  
  rsconnect::deployApp(
    appDir = here::here(),
    appName = app_name,
    appTitle = "Iowa Cities Dashboard",
    appFiles = c(
      # Main app
      "app.R",
      
      # R modules
      "R/helpers.R",
      "R/scoring.R",
      "R/server.R",
      "R/themes.R",
      "R/ui.R",
      
      # Data files
      "data/raw/iowa_amenities_data.csv",
      "data/raw/iowa_cities_census.csv",
      "data/raw/iowa_cities_raw.csv",
      "data/raw/iowa_climate_data.csv",
      "data/raw/iowa_crime_data.csv",
      "data/raw/iowa_demographics_data.csv",
      "data/raw/iowa_economic_data.csv",
      "data/raw/iowa_education_data.csv",
      "data/raw/iowa_environment_data.csv",
      "data/raw/iowa_family_data.csv",
      "data/raw/iowa_healthcare_data.csv",
      "data/raw/iowa_historical_data.csv",
      "data/raw/iowa_housing_data.csv",
      "data/raw/iowa_major_cities.csv",
      "data/raw/iowa_pets_data.csv",
      "data/raw/iowa_senior_data.csv",
      "data/processed/iowa_cities_analyzed.csv",
      "data/processed/iowa_cities_clean.csv",
      
      # Config
      "config.yml",
      
      # Assets
      "www/themes.css"
    ),
    forceUpdate = TRUE,
    launch.browser = TRUE
  )
  
  cat("\n✓ Deployment complete!\n")
  cat("\nNext steps for custom domain (iowa.andernet.dev):\n")
  cat("1. Go to your Cloudflare dashboard\n")
  cat("2. Add a CNAME record:\n")
  cat("   - Name: iowa\n")
  cat("   - Target: YOUR_ACCOUNT.shinyapps.io\n")
  cat("   - Proxy status: Proxied (orange cloud)\n")
  cat("3. In shinyapps.io dashboard, go to your app settings\n")
  cat("4. Under 'URLs', add custom domain: iowa.andernet.dev\n")
}

# Run deployment
# deploy_app()

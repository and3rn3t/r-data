# Iowa Cities Dashboard - Dockerfile
# ==================================
# Multi-stage build for efficient R Shiny deployment

# Stage 1: Base R image with dependencies
FROM rocker/shiny:4.3.2 AS base

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c( \
    'shiny', \
    'shinydashboard', \
    'shinyjs', \
    'shinycssloaders', \
    'tidyverse', \
    'plotly', \
    'DT', \
    'here', \
    'scales', \
    'leaflet' \
), repos='https://cloud.r-project.org/')"

# Stage 2: Production image
FROM base AS production

# Set working directory
WORKDIR /srv/shiny-server/iowa-cities

# Copy application files
COPY app.R .
COPY scripts/ scripts/
COPY data/ data/

# Set permissions
RUN chown -R shiny:shiny /srv/shiny-server/iowa-cities

# Expose port
EXPOSE 3838

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD curl -f http://localhost:3838/ || exit 1

# Run as shiny user
USER shiny

# Start Shiny Server
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/iowa-cities', host='0.0.0.0', port=3838)"]

# ==================================
# Build Instructions:
# ==================================
# 
# Build the image:
#   docker build -t iowa-cities-dashboard .
#
# Run the container:
#   docker run -d -p 3838:3838 --name iowa-dashboard iowa-cities-dashboard
#
# Access the app:
#   http://localhost:3838
#
# Development mode with live reload:
#   docker run -d -p 3838:3838 -v $(pwd):/srv/shiny-server/iowa-cities iowa-cities-dashboard
#
# View logs:
#   docker logs iowa-dashboard
#
# Stop container:
#   docker stop iowa-dashboard && docker rm iowa-dashboard

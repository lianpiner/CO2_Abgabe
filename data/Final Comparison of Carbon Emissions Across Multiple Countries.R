# Install and load necessary packages
install.packages(c("readr", "dplyr", "ggplot2", "rworldmap"))
library(readr)
library(dplyr)
library(ggplot2)
library(rworldmap)

# Set the working directory
setwd("C:/Users/shang/Desktop/Praktikum/11.1")

# Read the CO2 data
co2_data <- read_csv("NEW_CO2DATA(3).csv")

# Create a list of European countries
european_countries <- c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE")

# Add a new column 'Region' based on whether the country is in Europe or not
co2_data <- co2_data %>%
  mutate(Region = ifelse(Country_code_A3 %in% european_countries, "Europe", "Non-Europe"))

# Define time periods
time_periods <- list(
  "1990-1999" = c(1990, 1999),
  "2000-2009" = c(2000, 2009),
  "2010-2019" = c(2010, 2019)
)

# Function to create a heatmap for a given time period
create_heatmap <- function(period, start_year, end_year) {
  # Filter data for the specified time period
  period_data <- co2_data %>%
    filter(Year >= start_year & Year <= end_year)
  
  # Calculate mean CO2 emissions by country for the period
  country_emissions <- period_data %>%
    group_by(Country_code_A3, Name, Region) %>%
    summarise(mean_emission = mean(Emission, na.rm = TRUE)) %>%
    ungroup()
  
  # Create a world map dataframe
  worldmap_data <- joinCountryData2Map(country_emissions, joinCode = "ISO3", nameJoinColumn = "Country_code_A3")
  
  # Create a heatmap showing CO2 emissions by country, highlighting Europe vs Non-Europe
  mapCountryData(worldmap_data, nameColumnToPlot = "mean_emission", catMethod = "fixedWidth", mapTitle = paste("Average CO2 Emissions by Country (", period, ")"), colourPalette = "heat")
  
  # Save the map
  png(paste0("co2_emissions_map_", period, ".png"), width = 1200, height = 800, res = 150)
  mapCountryData(worldmap_data, nameColumnToPlot = "mean_emission", catMethod = "fixedWidth", mapTitle = paste("Average CO2 Emissions by Country (", period, ")"), colourPalette = "heat")
  dev.off()
}

# Create heatmaps for each time period
for (period in names(time_periods)) {
  start_year <- time_periods[[period]][1]
  end_year <- time_periods[[period]][2]
  create_heatmap(period, start_year, end_year)
}
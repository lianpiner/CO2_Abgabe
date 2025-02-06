# Load required packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(scales)

# Read CSV file
merged_data <- read.csv("MERGED_POLICY_CO2_DATA.csv", stringsAsFactors = FALSE)

# Countries to exclude from analysis
excluded_countries <- c("CHN", "NKR", "USA")

# Define sectors for analysis
sectors <- c("Buildings", "Transport", "Industry", "Electricity")

# Custom color palette - using standard base colors
custom_palette <- c(
  "#FFFF00",    # Yellow
  "#00FF00",    # Green
  "#00FFFF",    # Cyan
  "#FFA500",    # Orange
  "#800080",    # Purple
  "#FF1493"     # Deep Pink
)

# Function to analyze individual sector
analyze_sector <- function(merged_data, sector_name, complete_data_countries) {
  sector_data <- merged_data %>%
    filter(
      Sector == sector_name, 
      Year >= 2000 & Year <= 2020,
      !is.na(Emission),
      !is.na(OBS_VALUE),
      !Country_code_A3 %in% excluded_countries,
      Name %in% complete_data_countries
    )
  
  # Calculate annual data for each country
  sector_data_complete <- sector_data %>%
    # First group by country, year, and policy to calculate averages
    group_by(Name, Year, CLIM_ACT_POL) %>%
    summarise(
      Emissions = mean(Emission, na.rm = TRUE),
      PolicyValue = mean(OBS_VALUE, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    # Then group by country and year for final values
    group_by(Name, Year) %>%
    summarise(
      Emissions = sum(Emissions, na.rm = TRUE),
      PolicyIntensity = mean(PolicyValue, na.rm = TRUE),
      .groups = 'drop'
    )
  
  return(sector_data_complete)
}

# Function to create plots for each sector
create_sector_plots <- function(sector_data, sector_name) {
  emissions_plot <- ggplot(sector_data, aes(x = Year, y = Emissions/1000, color = Name)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    scale_color_manual(values = custom_palette) +
    scale_y_continuous(labels = comma) +
    labs(
      title = paste(sector_name, "Sector CO2 Emissions (2000-2020)"),
      x = "Year",
      y = "CO2 Emissions (Million tonnes)",
      color = "Country"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 12),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  policy_plot <- ggplot(sector_data, aes(x = Year, y = PolicyIntensity, color = Name)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    scale_color_manual(values = custom_palette) +
    scale_y_continuous(limits = c(0, 10)) +
    labs(
      title = paste(sector_name, "Sector Policy Intensity (2000-2020)"),
      x = "Year",
      y = "Policy Intensity (0-10 scale)",
      color = "Country"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 12),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  return(list(emissions = emissions_plot, policy = policy_plot))
}

# Main analysis workflow
main_analysis <- function() {
  # Find countries with complete data for each sector
  complete_countries_by_sector <- lapply(sectors, function(sector) {
    merged_data %>%
      filter(
        Sector == sector,
        Year >= 2000 & Year <= 2020,
        !is.na(Emission),
        !is.na(OBS_VALUE),
        !Country_code_A3 %in% excluded_countries
      ) %>%
      group_by(Name) %>%
      summarise(
        unique_years = n_distinct(Year),
        total_years = 21,
        .groups = 'drop'
      ) %>%
      filter(unique_years == total_years) %>%
      pull(Name)
  })
  
  # Find countries with complete data in main sectors
  common_countries <- Reduce(intersect, complete_countries_by_sector[1:3])
  
  # Calculate total emissions for these countries
  total_emissions_by_country <- merged_data %>%
    filter(
      Name %in% common_countries,
      Year >= 2000 & Year <= 2020
    ) %>%
    group_by(Name) %>%
    summarise(
      total_emissions = sum(Emission, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    arrange(desc(total_emissions))
  
  # Select top 6 countries by emissions
  top_countries <- total_emissions_by_country %>%
    head(6) %>%
    pull(Name)
  
  # Analyze each sector and create visualizations
  sectors_data <- lapply(sectors, function(sector) {
    sector_data <- analyze_sector(merged_data, sector, top_countries)
    plots <- create_sector_plots(sector_data, sector)
    combined_plots <- grid.arrange(plots$emissions, plots$policy, ncol = 1)
    ggsave(
      paste0(tolower(sector), "_trends.png"), 
      combined_plots, 
      width = 12, 
      height = 14, 
      dpi = 300
    )
    return(sector_data)
  })
}

# Run the analysis
main_analysis()
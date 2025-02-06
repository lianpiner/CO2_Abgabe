# Install and load necessary packages
install.packages(c("readr", "dplyr", "ggplot2"))
library(readr)
library(dplyr)
library(ggplot2)

# Set the working directory
setwd("C:/Users/shang/Desktop/Praktikum/11.1")

# Read the CO2 data
co2_data <- read_csv("NEW_CO2DATA(3).csv")

# Create a list of European Union countries (as of 2021)
eu_countries <- c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE")

# Add a new column 'Region' based on whether the country is in the EU or not
co2_data <- co2_data %>%
  mutate(Region = ifelse(Country_code_A3 %in% eu_countries, "European Union", "Non-EU"))

# Filter data for the selected sectors
selected_sectors <- c("Buildings", "Electricity", "Transport", "Industry")
filtered_data <- co2_data %>%
  filter(Sector %in% selected_sectors)

# Group data by Year, Region, and Sector, calculate total emissions and number of countries
emission_summary <- filtered_data %>%
  group_by(Year, Region, Sector) %>%
  summarise(
    total_emission = sum(Emission, na.rm = TRUE),
    num_countries = n_distinct(Country_code_A3)
  ) %>%
  ungroup()

# Calculate average emissions per country
emission_summary <- emission_summary %>%
  mutate(avg_emission_per_country = total_emission / num_countries)

# Create a line plot with four facets, one for each sector
ggplot(emission_summary, aes(x = Year, y = avg_emission_per_country, color = Region)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ Sector, ncol = 2, scales = "free_y") +
  labs(x = "Year", y = "Average CO2 Emissions per Country", color = "Region",
       title = "Average CO2 Emissions per Country Over Time: EU vs Non-EU by Sector") +
  scale_color_manual(values = c("European Union" = "blue", "Non-EU" = "red")) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Save the plot
ggsave("avg_co2_emissions_per_country_eu_vs_non_eu_by_sector.png", width = 12, height = 8, dpi = 300)
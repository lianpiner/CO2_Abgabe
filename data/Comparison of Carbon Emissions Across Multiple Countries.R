# Install and load necessary packages
install.packages(c("readr", "dplyr", "ggplot2", "scales"))
library(readr)
library(dplyr)
library(ggplot2)
library(scales)

# Set the working directory
setwd("C:/Users/shang/Desktop/Praktikum/11.1")

# Read the CO2 data
co2_data <- read_csv("NEW_CO2DATA(3).csv")

# Select the countries of interest
selected_countries <- c("CHN", "USA", "DEU", "IND", "JPN", "RUS")

# Filter data for the selected countries
filtered_data <- co2_data %>%
  filter(Country_code_A3 %in% selected_countries)

# Group data by Year and Country, and calculate total CO2 emissions
emission_summary <- filtered_data %>%
  group_by(Year, Country_code_A3) %>%
  summarise(total_emission = sum(Emission, na.rm = TRUE)) %>%
  ungroup()

# Create a line plot
ggplot(emission_summary, aes(x = Year, y = total_emission, color = Country_code_A3)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  labs(x = "Year", y = "Total CO2 Emissions", color = "Country",
       title = "CO2 Emissions Over Time for Selected Countries") +
  scale_color_manual(values = c("CHN" = "#FFFF00", "USA" = "#56B4E9", "DEU" = "#800080",
                                "IND" = "black", "JPN" = "#D55E00", "RUS" = "#CC79A7"),
                     labels = c("CHN" = "China", "USA" = "United States", "DEU" = "Germany",
                                "IND" = "India", "JPN" = "Japan", "RUS" = "Russia")) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Save the plot
ggsave("co2_emissions_selected_countries.png", width = 12, height = 8, dpi = 300)

# Print summary statistics
summary_stats <- emission_summary %>%
  group_by(Country_code_A3) %>%
  summarise(
    avg_emission = mean(total_emission),
    min_emission = min(total_emission),
    max_emission = max(total_emission),
    total_change = (last(total_emission) - first(total_emission))/first(total_emission) * 100
  )

print("Emission Summary by Country:")
print(summary_stats)
# Load required libraries
library(readr)
library(dplyr)
library(ggplot2)
library(scales)

# Read data from a single CSV file
all_data <- read.csv("C:/学习/24sta/Prak/merged_policy_co2_data.csv")

# First identify countries with complete data
country_completeness <- all_data %>%
  filter(Year >= 2000 & Year <= 2020) %>%
  group_by(Country_code_A3, Name, Year) %>%
  summarise(
    has_emission = !all(is.na(Emission)),
    has_obs_value = !all(is.na(OBS_VALUE)),
    .groups = 'drop'
  ) %>%
  group_by(Country_code_A3, Name) %>%
  summarise(
    complete_years = sum(has_emission & has_obs_value),
    all_complete = all(has_emission & has_obs_value),
    .groups = 'drop'
  )

# Get countries with complete data
complete_countries <- country_completeness %>%
  filter(all_complete & complete_years == 21)

# Print the countries used in the analysis
print("Countries included in the analysis:")
print(complete_countries)

# Process data for complete countries only
final_data <- all_data %>%
  filter(Year >= 2000 & Year <= 2020,
         Country_code_A3 %in% complete_countries$Country_code_A3) %>%
  group_by(Year, Sector) %>%
  summarise(
    avg_emission = mean(Emission, na.rm = TRUE),
    avg_obs_value = mean(OBS_VALUE, na.rm = TRUE),
    .groups = 'drop'
  )

# Standardize data within each sector
standardized_data <- final_data %>%
  group_by(Sector) %>%
  mutate(
    std_emission = (avg_emission - mean(avg_emission)) / sd(avg_emission),
    std_obs_value = (avg_obs_value - mean(avg_obs_value)) / sd(avg_obs_value)
  ) %>%
  ungroup()

# Create plot
p <- ggplot(standardized_data, aes(x = Year)) +
  # CO2 Emissions line (blue line)
  geom_line(aes(y = std_emission, color = "CO2 Emissions"), linewidth = 1) +
  geom_point(aes(y = std_emission, color = "CO2 Emissions"), size = 2) +
  # Policy Intensity line (red line)
  geom_line(aes(y = std_obs_value, color = "Policy Intensity"), linewidth = 1) +
  geom_point(aes(y = std_obs_value, color = "Policy Intensity"), size = 2) +
  # Zero reference line
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey50", size = 0.5) +
  # Facet by sector
  facet_wrap(~Sector, scales = "fixed", ncol = 2) +
  # Formatting
  theme_minimal() +
  labs(title = "Standardized CO2 Emissions and Policy Intensity (2000-2020)",
       subtitle = "Values standardized within each sector",
       x = "Year",
       y = "Standard Deviations from Mean",
       color = "Metric") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    strip.text = element_text(size = 12, face = "bold"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey95")
  ) +
  scale_color_manual(
    values = c("CO2 Emissions" = "blue", "Policy Intensity" = "#E41A1C")
  )

# Save plot
ggsave("C:/学习/24sta/Prak/standardized_sector_analysis.png", p, width = 15, height = 10, dpi = 300)

# Calculate correlation between standardized values
correlations <- standardized_data %>%
  group_by(Sector) %>%
  summarise(
    correlation = cor(std_emission, std_obs_value),
    start_to_end_emission = last(std_emission) - first(std_emission),
    start_to_end_policy = last(std_obs_value) - first(std_obs_value)
  )

# Save summary statistics
write.csv(correlations, "C:/学习/24sta/Prak/sector_correlations.csv", row.names = FALSE)
print("Correlations between standardized emissions and policy intensity by sector:")
print(correlations)
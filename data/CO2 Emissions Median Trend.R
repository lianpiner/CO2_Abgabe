library(readr)
library(dplyr)
library(ggplot2)
library(scales)


co2_data <- read_csv("D:/sta3/praxis/shujuku/NEW_CO2DATA.csv")

# Remove missing and invalid values
co2_data_clean <- co2_data %>%
  filter(!is.na(Emission) & Emission > 0)  

# Compute median CO₂ emissions per year
co2_median <- co2_data_clean %>%
  group_by(Year) %>%
  summarise(Median_Emission = median(Emission, na.rm = TRUE), .groups = "drop")

# Plot median CO₂ emissions trend
ggplot(co2_median, aes(x = Year, y = Median_Emission)) +
  geom_line(color = "blue", size = 1.2) +  
  geom_point(color = "blue", size = 2) +  
  scale_y_continuous(labels = comma) +  
  scale_x_continuous(breaks = seq(min(co2_median$Year), max(co2_median$Year), by = 5)) + 
  labs(
    title = "CO2 Emissions Median Trend",
    x = "Year",
    y = "CO2 Median Emissions (Tons)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey40"),
    axis.text.x = element_text(angle = 45, hjust = 1)  
  )

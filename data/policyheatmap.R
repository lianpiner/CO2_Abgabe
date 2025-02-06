library(readr)
library(dplyr)
library(ggplot2)
library(scales)

policy_data <- read_csv("D:/sta3/praxis/shujuku/NEW_POLICYDATA.csv")

# Clean data: Remove missing and invalid values, keep only positive values, and compute yearly mean per country
policy_data_clean <- policy_data %>%
  filter(!is.na(OBS_VALUE) & OBS_VALUE > 0) %>%
  group_by(Year, Name) %>%
  summarise(Mean_OBS_VALUE = mean(OBS_VALUE, na.rm = TRUE), .groups = 'drop')

# Plot heatmap with improved red color gradient
ggplot(policy_data_clean, aes(x = Year, y = Name, fill = Mean_OBS_VALUE)) +
  geom_tile(color = "white") +  # Add white grid lines for clarity
  scale_fill_gradient(low = "#FFE5E5", high = "#B30000", na.value = "grey90") +  
  scale_x_continuous(breaks = seq(min(policy_data_clean$Year), max(policy_data_clean$Year), by = 5)) +  # Show every 5 years
  labs(
    x = "Year",
    fill = "Avg. Policy Intensity"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 8),  
    axis.text.x = element_text(angle = 45, hjust = 1)  
  )

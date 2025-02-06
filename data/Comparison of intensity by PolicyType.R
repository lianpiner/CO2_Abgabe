library(ggplot2)
library(dplyr)


data <- read.csv("C:/Users/mrxuk/Desktop/pra/FINAL DATA/merged_policy_co2_data(2).csv")


data_clean <- data %>%
  filter(!is.na(OBS_VALUE), !is.na(PolicyType))

# Daten berechnen
policy_summary <- data_clean %>%
  group_by(PolicyType) %>%
  summarise(mean_obs_value = mean(OBS_VALUE, na.rm = TRUE),
            sd_obs_value = sd(OBS_VALUE, na.rm = TRUE),
            count = n())

print(policy_summary)

# vergleichen policy intensity mit Boxplot
ggplot(data_clean, aes(x = PolicyType, y = OBS_VALUE, fill = PolicyType)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#1976D2", "orange"))+
  theme_minimal() +
  labs(title = "Comparison of Intensity by PolicyType",
       x = "Policy Type",
       y = "Policy intensity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



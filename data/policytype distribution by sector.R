
library(ggplot2)
library(dplyr)

df <- read.csv("C:/Users/mrxuk/Desktop/Data/NEW_POLICYDATA(4).csv", stringsAsFactors = FALSE)

policy_sector_data <- df %>%
  group_by(Sector, PolicyType) %>%
  summarise(count = n(), .groups = "drop")  

# die Farben bestimmen
custom_colors <- c("market-based instrument" = "#1976D2",  # 蓝色
                   "non market-based instrument" = "orange")  # 橙色

# Zeichnen  ein  Säulendiagramm
ggplot(policy_sector_data, aes(x = Sector, y = count, fill = PolicyType)) +
  geom_bar(stat = "identity", position = "stack") +  
  labs(title = "Policy Type Distribution by Sector",
       x = "Sector",
       y = "Count",
       fill = "Policy Type") +
  scale_fill_manual(values = custom_colors) +  # 使用自定义颜色
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
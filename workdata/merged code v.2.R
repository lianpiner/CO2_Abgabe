# Install and load necessary packages
install.packages("readr")
library(dplyr)
library(readr)

# Read both CSV files
policy_data <- read_csv("NEW_POLICYDATA.csv")
co2_data <- read_csv("NEW_CO2DATA.csv")

# Select and rename relevant columns from CO2 data
co2_data_clean <- co2_data %>%
  select(Country_code_A3, Sector, Year, Emission)

# Merge the datasets with many-to-many relationship specified
merged_data <- policy_data %>%
  inner_join(co2_data_clean, 
             by = c("Country_code_A3", "Sector", "Year"),
             relationship = "many-to-many")

# Save the merged data to a new CSV file
write_csv(merged_data, "MERGED_POLICY_CO2_DATA.csv")

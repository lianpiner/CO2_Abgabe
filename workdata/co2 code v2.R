# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)

# Read the Excel file with correct path
file_path <- "C:/学习/24sta/Prak/ORIGIN_CO2DATA.xlsx"
ipcc2006_data <- read_excel(file_path, sheet = "IPCC 2006")

# Use the 10th row as column names and adjust the data structure
colnames(ipcc2006_data) <- ipcc2006_data[10, ]
ipcc2006_data <- ipcc2006_data[-(1:10), ]

# Filter out rows where 'fossil_bio' column has the value 'bio'
filtered_data <- ipcc2006_data %>% filter(fossil_bio != "bio")

# Select columns for the years 1990 to 2021
year_columns <- paste0("Y_", 1990:2021)
columns_to_keep <- c("IPCC_annex", "C_group_IM24_sh", "Country_code_A3", "Name",
                     "fossil_bio", "ipcc_code_2006_for_standard_report",
                     "ipcc_code_2006_for_standard_report_name", year_columns)
filtered_data_years <- filtered_data %>% select(all_of(columns_to_keep))

# Define sector mapping for specific codes
sector_mapping <- c(
  "1.A.1.a" = "Electricity",
  "1.A.2" = "Industry",
  "1.A.3.b_noRES" = "Transport",
  "1.A.3.c" = "Transport",
  "1.A.4" = "Buildings",
  "2.A.1" = "Industry",
  "2.A.2" = "Industry",
  "2.A.3" = "Industry",
  "2.A.4" = "Industry",
  "2.B" = "Industry",
  "2.C" = "Industry",
  "2.D" = "Industry"
)

# Add a new column 'Sector' based on the mapping, with default value for unmapped codes
filtered_data_years <- filtered_data_years %>%
  mutate(Sector = ifelse(is.na(sector_mapping[ipcc_code_2006_for_standard_report]), 
                         "Other", 
                         sector_mapping[ipcc_code_2006_for_standard_report]))

# Convert to long format while preserving NA values
data_long <- filtered_data_years %>%
  pivot_longer(
    cols = starts_with("Y_"),
    names_to = "Year",
    values_to = "Emission",
    values_drop_na = FALSE  # Preserve NA values
  ) %>%
  mutate(
    Year = as.numeric(sub("Y_", "", Year)),
    Emission = as.numeric(Emission)  # Ensure emission values are numeric
  )

# Export the data including NA values
write.csv(data_long, "C:/学习/24sta/Prak/NEW_CO2DATA.csv", row.names = FALSE)

# View the first few rows including NA values
head(data_long, 10)
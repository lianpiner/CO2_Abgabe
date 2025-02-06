# Install and load necessary packages
install.packages("readr")
library(dplyr)
library(readr)

# Set the working directory and file path
setwd("C:/学习/24sta/Prak")
file_path <- "ORIGIN_POLICYDATA.csv"

# Read the CSV file
oecd_data <- read_csv(file_path)

# Rename columns using base R
names(oecd_data)[names(oecd_data) == "REF_AREA"] <- "Country_code_A3"
names(oecd_data)[names(oecd_data) == "Reference area"] <- "Name"
names(oecd_data)[names(oecd_data) == "TIME_PERIOD"] <- "Year"

# Define classification rules with updated policy types and details
classification <- data.frame(
  CLIM_ACT_POL = c("LEV3_FFS_E", "LEV3_EXCISETAX_E", "LEV3_FFS_I", "LEV3_EXCISETAX_I",
                   "LEV3_FFS_B", "LEV3_EXCISETAX_B", "LEV3_CONG_CHARG", "LEV3_FFS_T",
                   "LEV3_EXCISETAX_T", "LEV3_EXP_RAIL", "LEV3_SPEED", "LEV4_FFS_E",
                   "LEV4_EXCISETAX_E_COAL", "LEV4_EXCISETAX_E_NATGAS", "LEV4_FFS_I",
                   "LEV4_EXCISETAX_I_COAL", "LEV4_EXCISETAX_I_NATGAS", "LEV4_EXCISETAX_I_DIESEL",
                   "LEV4_FFS_B", "LEV4_EXCISETAX_B_C_COAL", "LEV4_EXCISETAX_B_R_COAL",
                   "LEV4_EXCISETAX_B_C_DIESEL", "LEV4_EXCISETAX_B_R_DIESEL",
                   "LEV4_EXCISETAX_B_C_LPG", "LEV4_EXCISETAX_B_R_LPG",
                   "LEV4_EXCISETAX_B_C_NATGAS", "LEV4_EXCISETAX_B_R_NATGAS", "LEV4_CONG_CHARG",
                   "LEV4_FFS_T", "LEV4_EXCISETAX_T_DIESEL", "LEV4_EXCISETAX_T_GASO", "LEV4_EXP_RAIL",
                   "LEV4_SPEED"),
  Sector = c("Electricity", "Electricity", "Industry", "Industry",
             "Buildings", "Buildings", "Transport", "Transport",
             "Transport", "Transport", "Transport", "Electricity",
             "Electricity", "Electricity", "Industry", "Industry",
             "Industry", "Industry", "Buildings", "Buildings",
             "Buildings", "Buildings", "Buildings", "Buildings",
             "Buildings", "Buildings", "Buildings", "Transport",
             "Transport", "Transport", "Transport", "Transport", "Transport"),
  PolicyType = c(rep("market-based instrument", 9), 
                 "non market-based instrument", "non market-based instrument",  # LEV3_EXP_RAIL, LEV3_SPEED
                 rep("market-based instrument", 20),
                 "non market-based instrument", "non market-based instrument"), # LEV4_EXP_RAIL, LEV4_SPEED
  PolicyDetail = c(rep("Taxes and fees", 9),
                   "Other NMBI", "Other NMBI",  # LEV3_EXP_RAIL, LEV3_SPEED
                   rep("Taxes and fees", 20),
                   "Other NMBI", "Other NMBI"),  # LEV4_EXP_RAIL, LEV4_SPEED
  ClimateRelevant = c(rep("Yes", 33))
)

# Merge classification rules with the original dataset
classified_data <- oecd_data %>%
  left_join(classification, by = c("CLIM_ACT_POL" = "CLIM_ACT_POL"))

# Remove rows where the 'Sector' column is NA
# Also filter out rows where 'Year' is 2022 (note the updated column name)
cleaned_data <- classified_data %>%
  filter(!is.na(Sector)) %>%
  filter(Year != 2022)

# Save the cleaned data to a new CSV file
write_csv(cleaned_data, "NEW_POLICYDATA.csv")
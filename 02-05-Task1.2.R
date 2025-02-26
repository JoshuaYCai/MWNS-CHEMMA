# Load necessary libraries
library(dplyr)       # For data manipulation
library(readr)       # For reading CSV files

# Set the file path
file_path <- "c:/workbench/MWNSdata/chem.csv"

# Load the dataset
data <- read.csv(file_path)

# Filter data for the CUUT parameter
cuut_data <- data %>% filter(parm == "CUUT")

# Calculate the maximum observed value of CUUT for each site
max_cuut_by_site <- cuut_data %>%
  group_by(site) %>%
  summarise(
    max_value = max(value, na.rm = TRUE),  # Calculate the maximum value
    .groups = 'drop'
  )

# Print the results to the console
print(max_cuut_by_site)
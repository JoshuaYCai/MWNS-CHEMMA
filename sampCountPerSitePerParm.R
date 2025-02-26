# Load necessary libraries
library(readr)       # For reading CSV files efficiently
library(dplyr)       # For data manipulation

# Set the file path
file_path <- "c:/workbench/MWNSdata/chem.csv"

# Read the CSV file
data <- read_csv(file_path)


# Count the number of samples per site per parameter
sample_counts <- data %>%
  group_by(site, parm) %>%
  summarise(
    num_samples = n(),  # Count the number of rows (samples)
    .groups = 'drop'
  )

# Print the results
print(sample_counts)

# Save the results to a CSV file (optional)
write.csv(sample_counts, "sample_counts_per_site_per_parameter.csv", row.names = FALSE)
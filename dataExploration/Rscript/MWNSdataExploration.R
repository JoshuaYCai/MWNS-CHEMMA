# Version: 1.0
# Date: 20250119
# Author: Joshua Cai


# Load necessary libraries
library(readr)       # For reading CSV files efficiently
library(dplyr)       # For data manipulation
library(ggplot2)     # For data visualization
library(lubridate)   # For handling date-time data
library(stringr)     # For string manipulation
library(fs)


# Set the file path
file_path <- "c:/workbench/MWNSdata/chem.csv"

# Read the CSV file
data <- read_csv(file_path)

# Inspect the original datetime column
# Verify the datetime column
print(head(data$datetime))  # Check the first few rows
print(class(data$datetime))  # Confirm the class is POSIXct

# Check for missing values in the datetime column
missing_values <- sum(is.na(data$datetime))
print(paste("Number of missing values in datetime:", missing_values))

# Check the structure of the data
str(data)

# Rename the first column to "IDch"
names(data)[1] <- "IDch"

# Create a new column "IDn" and populate it with extracted numerical values from "IDch"
data <- data %>% mutate(IDn = as.numeric(str_extract(IDch, "\\d+")))

# Check the first few rows
head(data)

# Summary statistics for numeric columns
summary(data)
 
# Check for missing values
missing_values <- colSums(is.na(data))
print(missing_values) # parm 'pH' has no unit thus 5468 are missing values under 'units'

# Inspect the first few rows of the datetime column
head(data$datetime)








# --------------------------------------------------------------------------
# OVERVIEW OF THIS XLSX
# --------------------------------------------------------------------------


# Count the number of unique rivers (sites)
unique_rivers <- unique(data$site)
num_rivers <- length(unique_rivers)

print(paste("Number of rivers covered:", num_rivers))

# List parameters for each river
parameters_by_river <- data %>%
  group_by(site) %>%
  summarize(
    parameters = paste(unique(parm), collapse = ", "),  # Combine unique parameters into a single string
    num_parameters = n_distinct(parm)                   # Count the number of unique parameters
  )

# Save the results to a CSV file
write_csv(parameters_by_river, "MWNS_DataOverview.csv")




# --------------------------------------------------------------------------
# VISUALIZE EACH PARM FOR EACH RIVER
# --------------------------------------------------------------------------


# Create a folder to store the plots
output_folder <- "C:/workbench/MWNSexport/MWNS_Plots"
dir_create(output_folder)  # Create the folder if it doesn't exist

# Get the list of unique rivers and parameters
unique_rivers <- unique(data$site)
unique_parameters <- unique(data$parm)

# Loop through each river and parameter to generate plots
for (river in unique_rivers) {
  # Create a subfolder for the river
  river_folder <- path(output_folder, river)
  dir_create(river_folder)
  
  for (parameter in unique_parameters) {
    # Filter data for the current river and parameter
    filtered_data <- data %>%
      filter(site == river & parm == parameter)
    
    # Skip if no data is available for the current river and parameter
    if (nrow(filtered_data) == 0) {
      next
    }
    
    # Create the plot
    plot <- ggplot(filtered_data, aes(x = datetime, y = value)) +
      geom_line(color = "blue") +
      labs(
        title = paste("Time-Series of", parameter, "at", river),
        x = "Date",
        y = paste(parameter, "Concentration (UG/L)")
      ) +
      theme_minimal()
    
    # Save the plot as a PNG file
    plot_file <- path(river_folder, paste0(parameter, ".png"))
    ggsave(plot_file, plot, width = 8, height = 6, dpi = 300)
  }
}

print(paste("All plots saved to:", output_folder))




# --------------------------------------------------------------------------
# PROBE SILVER CONCENTRATION IN BIG CREEK
# --------------------------------------------------------------------------

silver_big_creek <- data %>% filter(parm == "AGUT" & site == "big creek")


# Summary statistics for silver concentrations at "big creek"
silver_summary <- silver_big_creek %>%
  summarize(
    mean_silver = mean(value, na.rm = TRUE),
    median_silver = median(value, na.rm = TRUE),
    max_silver = max(value, na.rm = TRUE),
    min_silver = min(value, na.rm = TRUE)
  )
print(silver_summary)


# Plot silver concentrations over time for "big creek"
ggplot(silver_big_creek, aes(x = datetime, y = value)) +
  geom_line(color = "blue") +
  labs(
    title = "Silver Concentrations at Big Creek Over Time",
    x = "Date",
    y = "Silver Concentration (UG/L)"
  )

# Optionally, group by season and calculate summary statistics
silver_by_season <- silver_big_creek %>%
  group_by(season) %>%
  summarize(
    mean_silver = mean(value, na.rm = TRUE),
    median_silver = median(value, na.rm = TRUE)
  )
print(silver_by_season)



# # Explore data by site
# # Example: Summarize silver concentrations by site
# silver_by_site <- silver_data %>%
#   group_by(site) %>%
#   summarize(
#     mean_silver = mean(value, na.rm = TRUE),
#     median_silver = median(value, na.rm = TRUE),
#     max_silver = max(value, na.rm = TRUE),
#     min_silver = min(value, na.rm = TRUE)
#   )
# print(silver_by_site)
# 
# # Save processed data to a new CSV file (optional)
# write_csv(silver_data, "silver_data.csv")
# 
# # Additional exploration: Check for correlations between variables
# # Example: Correlation between "value" and other numeric variables
# correlation_matrix <- data %>%
#   select_if(is.numeric) %>%
#   cor(use = "complete.obs")  # Handle missing values
# print(correlation_matrix)
# 
# # Visualize correlations using a heatmap
# library(reshape2)  # For reshaping data for heatmap
# melted_corr <- melt(correlation_matrix)
# ggplot(melted_corr, aes(x = Var1, y = Var2, fill = value)) +
#   geom_tile() +
#   scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
#   labs(title = "Correlation Heatmap", x = "", y = "") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# # End of script

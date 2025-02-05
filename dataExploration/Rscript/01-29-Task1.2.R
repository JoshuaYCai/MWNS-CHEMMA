# Load necessary libraries
library(dplyr)       # For data manipulation
library(ggplot2)     # For plotting
library(readr)       # For reading CSV files
library(tidyr)       # For reshaping data
library(lubridate)   # For handling dates and times

# Set the file path
file_path <- "c:/workbench/sample_counts_per_site_per_parameter.csv"

# Load the sample counts data
sample_counts <- read_csv(file_path)

# Convert 'datetime' to a proper date-time format
data$datetime <- as.POSIXct(data$datetime, format = "%Y-%m-%d %H:%M:%S")

# Define meteorological seasons (NOAA definition)
data <- data %>%
  mutate(
    month = month(datetime),
    season = case_when(
      month %in% 3:5 ~ "spring",
      month %in% 6:8 ~ "summer",
      month %in% 9:11 ~ "fall",
      TRUE ~ "winter"  # December, January, February
    )
  )

# Ensure 'season' is a factor with the correct order
data$season <- factor(data$season, levels = c("spring", "summer", "fall", "winter"))

# Count the number of samples per site, per parameter, and per season
seasonal_sample_counts <- data %>%
  group_by(site, parm, season) %>%
  summarise(
    num_samples = n(),  # Count the number of rows (samples)
    .groups = 'drop'
  )

# Get the list of unique parameters
unique_parameters <- unique(seasonal_sample_counts$parm)

# Define the output directory
output_dir <- "C:/workbench/MWNSexport/01-29-task1.2/"

# Create the output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Create a plot for each parameter
for (parameter in unique_parameters) {
  # Filter data for the current parameter
  parameter_data <- seasonal_sample_counts %>% filter(parm == parameter)
  
  # Create the plot
  p <- ggplot(parameter_data, aes(x = season, y = num_samples, fill = site)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~ site, ncol = 3) +  # Arrange sites in a grid
    labs(
      title = paste("Number of Samples per Season for", parameter),
      x = "Season",
      y = "Number of Samples",
      fill = "Site"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
      plot.title = element_text(hjust = 0.5),             # Center the title
      strip.text = element_text(size = 8)                 # Adjust facet label size
    )
  
  # Save the plot as a PNG file in the specified directory
  file_name <- paste0("seasonal_sample_counts_", gsub(" ", "_", parameter), ".png")
  ggsave(file.path(output_dir, file_name), plot = p, width = 12, height = 8, dpi = 300)
}
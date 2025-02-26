# Load necessary libraries
library(dplyr)       # For data manipulation
library(ggplot2)     # For plotting
library(readr)       # For reading CSV files
library(lubridate)   # For handling dates and times

# Set the file path
file_path <- "c:/workbench/MWNSdata/chem.csv"

# Load the dataset
data <- read.csv(file_path)

# Convert 'datetime' to a proper date-time format
data$datetime <- as.POSIXct(data$datetime, format = "%Y-%m-%d %H:%M:%S")

# Define the Priority List of parameters
priority_parameters <- c("ALKT", "CAUT", "CLIDUR", "CUUT", "FEUT", "KKUT", "MGUT", "MNUT", "NAUT", "SIO3UR", "SSO4UR")

# Define the output directory
output_dir <- "C:/workbench/MWNSexport/02-05-task1/"

# Create the output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Function to calculate whisker limits
calculate_whiskers <- function(values) {
  q1 <- quantile(values, 0.25, na.rm = TRUE)
  q3 <- quantile(values, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower_whisker <- max(min(values, na.rm = TRUE), q1 - 1.5 * iqr)
  upper_whisker <- min(max(values, na.rm = TRUE), q3 + 1.5 * iqr)
  return(list(lower_whisker = lower_whisker, upper_whisker = upper_whisker))
}

# Function to create boxplots for a given parameter
create_boxplot <- function(parameter) {
  # Filter data for the current parameter
  parameter_data <- data %>% filter(parm == parameter)
  
  # Calculate whisker limits for each site
  whisker_data <- parameter_data %>%
    group_by(site) %>%
    summarise(
      lower_whisker = calculate_whiskers(value)$lower_whisker,
      upper_whisker = calculate_whiskers(value)$upper_whisker,
      .groups = 'drop'
    )
  
  # Calculate min and max values for each site
  min_max_data <- parameter_data %>%
    group_by(site) %>%
    summarise(
      min_value = min(value, na.rm = TRUE),
      max_value = max(value, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Create the boxplot
  p <- ggplot(parameter_data, aes(x = site, y = value)) +
    geom_boxplot(outlier.shape = NA) +  # Hide outliers
    geom_segment(data = whisker_data, aes(x = site, xend = site, y = lower_whisker, yend = upper_whisker), 
                 color = "blue", linewidth = 0.5) +  # Add whisker-min and whisker-max lines
    geom_point(data = min_max_data, aes(x = site, y = min_value), shape = 4, size = 3, color = "green") +  # Add green cross for min value
    geom_point(data = min_max_data, aes(x = site, y = max_value), shape = 4, size = 3, color = "red") +  # Add red cross for max value
    stat_summary(fun = mean, geom = "point", shape = 4, size = 3, color = "black") +  # Add black cross for mean
    labs(
      title = paste("Boxplot of", parameter, "Concentrations by Site"),
      x = "Site",
      y = paste(parameter, "Concentration")
    ) +
    scale_y_continuous(limits = c(NA, max(parameter_data$value, na.rm = TRUE) * 1.1)) +  # Adjust y-axis to reduce white space
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
      plot.title = element_text(hjust = 0.5)              # Center the title
    )
  
  # Save the plot as a PNG file
  file_name <- paste0("boxplot_", parameter, ".png")
  ggsave(file.path(output_dir, file_name), plot = p, width = 10, height = 6, dpi = 300)
}

# Generate boxplots for each parameter in the Priority List
for (parameter in priority_parameters) {
  create_boxplot(parameter)
}

# Print a message indicating completion
cat("Boxplots have been saved to:", output_dir, "\n")
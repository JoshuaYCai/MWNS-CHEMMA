# Load necessary libraries
library(dplyr)       # For data manipulation
library(ggplot2)     # For plotting
library(readr)       # For reading CSV files
library(RColorBrewer) # For color palettes

# Set the file path
file_path <- "c:/workbench/sample_counts_per_site_per_parameter.csv"

# Load the sample counts data
sample_counts <- read_csv(file_path)

# Get the list of unique sites
unique_sites <- unique(sample_counts$site)

# Define the output directory
output_dir <- "C:/workbench/MWNSexport/01-29-task1.1/"

# Create a bar chart for each site
for (site_name in unique_sites) {
  # Filter data for the current site
  site_data <- sample_counts %>% filter(site == site_name)
  
  # Create the bar chart with different colors for each parameter
  p <- ggplot(site_data, aes(x = parm, y = num_samples, fill = parm)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = colorRampPalette(brewer.pal(9, "Set1"))(nrow(site_data))) +  # Assign unique colors
    labs(
      title = paste("Number of Samples per Parameter for", site_name),
      x = "Parameter",
      y = "Number of Samples"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
      plot.title = element_text(hjust = 0.5),             # Center the title
      legend.position = "none"                           # Hide the legend
    )
  
  # Save the plot as a PNG file in the specified directory
  file_name <- paste0("sample_counts_", gsub(" ", "_", site_name), ".png")
  ggsave(file.path(output_dir, file_name), plot = p, width = 10, height = 6, dpi = 300)
}

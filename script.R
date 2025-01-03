# Install and load necessary libraries
library(dplyr)

library(ggplot2)

library(tidyr)

# Load the dataset
data <- read.csv("HappinessAlcoholConsumption.csv")

# Check column names
colnames(data)

# Clean the data
data_clean <- data %>%
  filter(!is.na(HappinessScore) & 
           !is.na(Beer_PerCapita) & 
           !is.na(Spirit_PerCapita) & 
           !is.na(Wine_PerCapita))

# Define output directory
output_dir <- "D:\\PROJECTS\\test\\output"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
cat("Output directory is set to:", output_dir, "\n")

# Scatter plot: Happiness vs. Beer Consumption
scatter_beer <- ggplot(data_clean, aes(x = Beer_PerCapita, y = HappinessScore)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  theme_minimal() +
  labs(
    title = "Scatter Plot: Happiness vs. Beer Consumption",
    x = "Beer Consumption (Per Capita)",
    y = "Happiness Score"
  )
ggsave(filename = file.path(output_dir, "scatter_happiness_beer.png"), plot = scatter_beer, width = 7, height = 5)

# Box plot: Happiness Score by Alcohol Type
data_melted <- data_clean %>%
  select(HappinessScore, Beer_PerCapita, Spirit_PerCapita, Wine_PerCapita) %>%
  pivot_longer(cols = -HappinessScore, names_to = "Alcohol_Type", values_to = "Consumption")

boxplot_alcohol <- ggplot(data_melted, aes(x = Alcohol_Type, y = Consumption, fill = Alcohol_Type)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Box Plot: Alcohol Consumption by Type",
    x = "Type of Alcohol",
    y = "Consumption (Per Capita)"
  )
ggsave(filename = file.path(output_dir, "boxplot_alcohol_consumption.png"), plot = boxplot_alcohol, width = 7, height = 5)

# Histogram: Distribution of Happiness Score
histogram_happiness <- ggplot(data_clean, aes(x = HappinessScore)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Histogram: Distribution of Happiness Scores",
    x = "Happiness Score",
    y = "Frequency"
  )
ggsave(filename = file.path(output_dir, "histogram_happiness_score.png"), plot = histogram_happiness, width = 7, height = 5)

# Density plot: Happiness Score
density_happiness <- ggplot(data_clean, aes(x = HappinessScore)) +
  geom_density(fill = "lightgreen", alpha = 0.5) +
  theme_minimal() +
  labs(
    title = "Density Plot: Happiness Scores",
    x = "Happiness Score",
    y = "Density"
  )
ggsave(filename = file.path(output_dir, "density_happiness_score.png"), plot = density_happiness, width = 7, height = 5)

# Bar plot: Average Alcohol Consumption
average_consumption <- data_clean %>%
  summarise(
    Beer = mean(Beer_PerCapita, na.rm = TRUE),
    Spirits = mean(Spirit_PerCapita, na.rm = TRUE),
    Wine = mean(Wine_PerCapita, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Alcohol_Type", values_to = "Average_Consumption")

barplot_average <- ggplot(average_consumption, aes(x = Alcohol_Type, y = Average_Consumption, fill = Alcohol_Type)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    title = "Bar Plot: Average Alcohol Consumption",
    x = "Type of Alcohol",
    y = "Average Consumption (Per Capita)"
  )
ggsave(filename = file.path(output_dir, "barplot_average_consumption.png"), plot = barplot_average, width = 7, height = 5)

# Confirm files saved
cat("All plots have been saved in the folder:", output_dir, "\n")

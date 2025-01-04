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

# Define output directories
output_dir <- "output"
stats_file <- file.path(output_dir, "statistical_test_results.txt")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ---- Statistical Tests ----

# Perform correlation tests
cor_beer <- cor.test(data_clean$HappinessScore, data_clean$Beer_PerCapita)
cor_spirits <- cor.test(data_clean$HappinessScore, data_clean$Spirit_PerCapita)
cor_wine <- cor.test(data_clean$HappinessScore, data_clean$Wine_PerCapita)

# Save test results to a text file
cat("Statistical Test Results:\n\n", file = stats_file)
cat("Correlation between Happiness and Beer Consumption:\n", file = stats_file, append = TRUE)
cat(capture.output(cor_beer), sep = "\n", file = stats_file, append = TRUE)
cat("\n\nCorrelation between Happiness and Spirits Consumption:\n", file = stats_file, append = TRUE)
cat(capture.output(cor_spirits), sep = "\n", file = stats_file, append = TRUE)
cat("\n\nCorrelation between Happiness and Wine Consumption:\n", file = stats_file, append = TRUE)
cat(capture.output(cor_wine), sep = "\n", file = stats_file, append = TRUE)

cat("Statistical test results saved to:", stats_file, "\n")

# ---- Plotting Based on Test Results ----

# Scatter plot: Happiness vs. Beer Consumption
if (cor_beer$p.value < 0.05) {
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
}

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

# ---- Output Summary ----
cat("All statistical test results saved to:", stats_file, "\n")
cat("All plots have been saved in the folder:", output_dir, "\n")

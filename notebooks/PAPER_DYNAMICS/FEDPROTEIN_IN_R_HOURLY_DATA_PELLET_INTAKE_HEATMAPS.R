
# Load necessary libraries
library(readr)
library(dplyr)
library(car)
library(multcomp)

# Load the dataset
data <- read_csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R(HEATMAPS)\\PELLETS_HEATMAP\\Transformed_Pellet_Intake_Data.csv")

# Convert relevant columns to factors
data$sex <- as.factor(data$sex)
data$order <- as.factor(data$order)
data$phase <- as.factor(data$phase)
data$hour <- as.factor(data$hour)  # Ensure 'Hour' is a factor


# --- Descriptive statistics for pellet intake by phase ---
descriptive_stats <- data %>%
  group_by(sex, order, phase, hour) %>%
  summarise(
    mean_pellet_intake = mean(pellet_intake, na.rm = TRUE),
    sd_pellet_intake = sd(pellet_intake, na.rm = TRUE),
    n = n()  # Optionally include count of observations
  )



# Save descriptive statistics
write.csv(descriptive_stats, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R(HEATMAPS)\\PELLETS_HEATMAP\\Descriptive_pellets.csv", row.names = FALSE)

# --- ANOVA for each variable (meals, snacks, mega meals) ---
# Compare across Sex, Order, Phase, and Hour (all factors) with interactions

# ANOVA for Meals
anova_pellets <- aov(pellet_intake ~ sex * order * phase * hour, data = data)
capture.output(summary(anova_pellets), file = "C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R(HEATMAPS)\\PELLETS_HEATMAP\\ANOVA_PELLETS.txt")



# --- Posthoc Tukey Test for each ANOVA ---
# Tukey's HSD for main effects and interactions (excluding Hour)

posthoc_pellets <- TukeyHSD(anova_pellets, "sex:order:phase")  # Focus on simpler interactions without 'Hour'
capture.output(posthoc_pellets, file = "C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R(HEATMAPS)\\PELLETS_HEATMAP\\posthoc_pellets.txt")


# --- Pairwise comparisons for PR and NR phases within groups at specific hours ---
# Group-by-hour and diet-phase comparisons for Meals
pairwise_pellets <- pairwise.t.test(data$pellet_intake, interaction(data$sex, data$order, data$phase, data$hour), p.adjust.method = "holm")



# Save pairwise comparisons
capture.output(pairwise_pellets, file = "C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R(HEATMAPS)\\PELLETS_HEATMAP\\pairwise_pellets.txt")



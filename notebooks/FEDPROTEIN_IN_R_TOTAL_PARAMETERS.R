# Load necessary libraries
library(tidyverse)
library(reshape2)
library(car)  # For ANOVA
library(multcomp)  # For Tukey and Holm post-hoc tests

# Load the data
data <- read.csv("C:/Users/hta031/Github/FEDProtein/results/Total_meal_snack_mega_per_phase_stats/total_parameters_per_phase.csv")

# Reshape the data to long format
data_long <- melt(data, id.vars = c("Mouse.ID", "Sex", "Order"),
                  measure.vars = c("Total.NR.Parameters", "Total.PR.Parameters"),
                  variable.name = "Diet_Phase", value.name = "Total_Parameters")

# Step 1: Descriptive statistics
desc_stats <- data_long %>%
  group_by(Sex, Order, Diet_Phase) %>%
  summarise(
    mean_total_parameters = mean(Total_Parameters, na.rm = TRUE),
    sd_total_parameters = sd(Total_Parameters, na.rm = TRUE),
    count = n()
  )

# Save descriptive statistics to CSV
write.csv(desc_stats, "C:/Users/hta031/Github/FEDProtein/results/Total_meal_snack_mega_per_phase_stats/descriptive_stats_total_parameters.csv", row.names = FALSE)

# Step 2: Run ANOVA with grouping information
anova_model <- aov(Total_Parameters ~ Sex * Order * Diet_Phase, data = data_long)
anova_results <- Anova(anova_model, type = 2)

# Create ANOVA table with grouping information
anova_table <- data.frame(
  Factor = rownames(anova_results),
  anova_results
)

# Save ANOVA results to CSV
write.csv(anova_table, "C:/Users/hta031/Github/FEDProtein/results/Total_meal_snack_mega_per_phase_stats/anova_results_total_parameters.csv", row.names = FALSE)

# Step 3: Tukey HSD post-hoc test
# Create interaction term for group comparisons
data_long$interaction_term <- interaction(data_long$Sex, data_long$Order, data_long$Diet_Phase)

# Perform Tukey HSD test on the interaction term
tukey_test <- TukeyHSD(aov(Total_Parameters ~ interaction_term, data = data_long))
tukey_table <- as.data.frame(tukey_test$interaction_term)

# Add group information to the Tukey results
comparison_labels <- str_split_fixed(rownames(tukey_test$interaction_term), "-", 2)
tukey_table$Group1 <- comparison_labels[, 1]
tukey_table$Group2 <- comparison_labels[, 2]

# Save Tukey HSD results to CSV with group labels
write.csv(tukey_table, "C:/Users/hta031/Github/FEDProtein/results/Total_meal_snack_mega_per_phase_stats/tukey_results_total_parameters_with_groups.csv", row.names = FALSE)

# Step 4: Holm post-hoc test
# Perform pairwise t-tests with Holm correction
holm_test <- pairwise.t.test(data_long$Total_Parameters, data_long$interaction_term, p.adjust.method = "holm")

# Extract and save Holm test results to CSV
holm_table <- as.data.frame(holm_test$p.value)
holm_table$Comparison <- rownames(holm_table)
write.csv(holm_table, "C:/Users/hta031/Github/FEDProtein/results/Total_meal_snack_mega_per_phase_stats/holm_results_total_parameters_with_groups.csv", row.names = FALSE)

# Print summary of results
print(desc_stats)
print(anova_table)
print(tukey_table)
print(holm_table)




# #####################
# Order and Diet Phase Matter:

# Males in Order 2 have significantly different total parameters between the PR and NR phases, reinforcing the idea that the diet phase significantly impacts their behavior.
# Males in Order 1 during the NR phase show significant differences when compared with both females in Order 2 (PR phase) and males in Order 2 (PR phase).
# No significant pairwise differences were found for other combinations of groups based on the Tukey test.

# Summary of Significant Findings from the Holm Test:
# Males in Order 1 (NR phase) vs Females in Order 2 (NR phase):
# p = 0.04467 (statistically significant).
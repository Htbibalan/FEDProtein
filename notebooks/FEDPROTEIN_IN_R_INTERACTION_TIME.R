# Load necessary libraries
library(tidyverse)
library(reshape2)
library(car)  # For ANOVA
library(multcomp)  # For Tukey and Holm post-hoc tests

# Load the data
data <- read.csv("C:/Users/hta031/Github/FEDProtein/results/Interaction_time_stats/mice_interaction_times_Updated.csv")

# Reshape the data to long format (ignore grain phase)
data_long <- melt(data, id.vars = c("mouse_id", "sex", "order"),
                  measure.vars = c("pr_interaction_time", "nr_interaction_time"),
                  variable.name = "Diet_Phase", value.name = "Interaction_Time")

# Step 1: Descriptive statistics
desc_stats <- data_long %>%
  group_by(sex, order, Diet_Phase) %>%
  summarise(
    mean_interaction_time = mean(Interaction_Time, na.rm = TRUE),
    sd_interaction_time = sd(Interaction_Time, na.rm = TRUE),
    count = n()
  )

# Save descriptive statistics to CSV
write.csv(desc_stats, "C:/Users/hta031/Github/FEDProtein/results/Interaction_time_stats/descriptive_stats_interaction_times.csv", row.names = FALSE)

# Step 2: Run ANOVA with grouping information
anova_model <- aov(Interaction_Time ~ sex * order * Diet_Phase, data = data_long)
anova_results <- Anova(anova_model, type = 2)

# Create ANOVA table with grouping information
anova_table <- data.frame(
  Factor = rownames(anova_results),
  anova_results
)

# Save ANOVA results to CSV
write.csv(anova_table, "C:/Users/hta031/Github/FEDProtein/results/Interaction_time_stats/anova_results_interaction_times.csv", row.names = FALSE)

# Step 3: Tukey HSD post-hoc test
# Create interaction term for group comparisons
data_long$interaction_term <- interaction(data_long$sex, data_long$order, data_long$Diet_Phase)

# Perform Tukey HSD test on the interaction term
tukey_test <- TukeyHSD(aov(Interaction_Time ~ interaction_term, data = data_long))
tukey_table <- as.data.frame(tukey_test$interaction_term)

# Add group information to the Tukey results
comparison_labels <- str_split_fixed(rownames(tukey_test$interaction_term), "-", 2)
tukey_table$Group1 <- comparison_labels[, 1]
tukey_table$Group2 <- comparison_labels[, 2]

# Save Tukey HSD results to CSV with group labels
write.csv(tukey_table, "C:/Users/hta031/Github/FEDProtein/results/Interaction_time_stats/tukey_results_interaction_times_with_groups.csv", row.names = FALSE)

# Step 4: Holm post-hoc test
# Perform pairwise t-tests with Holm correction
holm_test <- pairwise.t.test(data_long$Interaction_Time, data_long$interaction_term, p.adjust.method = "holm")

# Extract and save Holm test results to CSV
holm_table <- as.data.frame(holm_test$p.value)
holm_table$Comparison <- rownames(holm_table)
write.csv(holm_table, "C:/Users/hta031/Github/FEDProtein/results/Interaction_time_stats/holm_results_interaction_times_with_groups.csv", row.names = FALSE)

# Print summary of results
print(desc_stats)
print(anova_table)
print(tukey_table)
print(holm_table)



#####Order matters overall, as it significantly affects male interaction times, but there is no significant difference between the PR and NR phases for male mice in Order 2 based on the Holm test.
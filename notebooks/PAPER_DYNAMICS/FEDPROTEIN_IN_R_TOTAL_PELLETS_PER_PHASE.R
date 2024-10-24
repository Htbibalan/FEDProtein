



# ###########Key Observations:
# Order 1 (F vs M):

# In the PR phase, females consumed 1401 pellets on average, while males consumed 1338 pellets.
# In the NR phase, females consumed 1352 pellets, and males consumed 1333 pellets.
# The differences between males and females in Order 1 seem relatively small.

# Order 2 (F vs M):

# In the PR phase, females consumed 1366 pellets on average, while males consumed 1469 pellets.
# In the NR phase, females consumed 1442 pellets, and males consumed 1616 pellets.
# The difference between males and females in Order 2 is larger, particularly in the NR phase where males consumed significantly more pellets (1616 pellets) than females (1442 pellets).

# Summary:
# Order 1: Both males and females have fairly similar pellet intake in both PR and NR phases.
# Order 2: Males tend to eat more than females, particularly in the NR phase (1616 vs 1442 pellets).
# Potential Insights:
# The significant Order effect observed in the ANOVA might be largely driven by the difference in pellet intake between males and females in Order 2, especially during the NR phase, where males consumed notably more pellets than females.
# The overall increase in pellet intake in Order 2 NR phase (especially in males) likely contributed to the significant main effect of Order.


# "The sequence of diet presentation (Order) significantly affected overall pellet intake (ANOVA, p < 0.05). While pairwise comparisons between diet phases within groups did not reach statistical significance, male mice in Order 2 showed a trend of increased pellet intake in the NR phase."








###############################################################################################
##################################### LAST UPDATE  #############################################
###############################################################################################



# Load necessary libraries
library(tidyverse)
library(reshape2)
library(car)  # For ANOVA
library(multcomp)  # For Tukey and Holm post-hoc tests

# Load the data
data <- read.csv("C:/Users/hta031/Github/FEDProtein/results/Total_pellets_per_phase_stats/total_pellet_per_phase.csv")

# Reshape the data to long format
data_long <- melt(data, id.vars = c("Mouse.ID", "Sex", "Order"),
                  measure.vars = c("NR", "PR"),
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
write.csv(desc_stats, "C:/Users/hta031/Github/FEDProtein/results/Total_pellets_per_phase_stats/TOTAL_PHASE_STATS/descriptive_TOTAL_PHASE_PELLETS.csv", row.names = FALSE)

# Step 2: Run ANOVA with grouping information
anova_model <- aov(Total_Parameters ~ Sex * Order * Diet_Phase, data = data_long)
anova_results <- Anova(anova_model, type = 2)

# Create ANOVA table with grouping information
anova_table <- data.frame(
  Factor = rownames(anova_results),
  anova_results
)

# Save ANOVA results to CSV
write.csv(anova_table, "C:/Users/hta031/Github/FEDProtein/results/Total_pellets_per_phase_stats/TOTAL_PHASE_STATS/anova_TOTAL_PHASE_PELLETS.csv", row.names = FALSE)

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
write.csv(tukey_table, "C:/Users/hta031/Github/FEDProtein/results/Total_pellets_per_phase_stats/TOTAL_PHASE_STATS/tukey_TOTAL_PHASE.csv", row.names = FALSE)

# Step 4: Holm post-hoc test
# Perform pairwise t-tests with Holm correction
holm_test <- pairwise.t.test(data_long$Total_Parameters, data_long$interaction_term, p.adjust.method = "holm")

# Extract and save Holm test results to CSV
holm_table <- as.data.frame(holm_test$p.value)
holm_table$Comparison <- rownames(holm_table)
write.csv(holm_table, "C:/Users/hta031/Github/FEDProtein/results/Total_pellets_per_phase_stats/TOTAL_PHASE_STATS/holm_TOTAL_PHASE_PELLETS.csv", row.names = FALSE)

# Print summary of results
print(desc_stats)
print(anova_table)
print(tukey_table)
print(holm_table)


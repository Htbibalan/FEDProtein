

################################################################################




# Load necessary libraries
library(tidyverse)  # Includes dplyr and ggplot2
library(reshape2)   # For melt function
library(car)        # For Anova function

# Load the data
data <- read.csv("C:/Users/hta031/Github/FEDProtein/results/Total_pellets_per_phase_stats/total_pellet_per_phase.csv")

# Check column names to ensure they match what is used in the code
colnames(data)

# Step 1: Descriptive statistics for PR and NR phases grouped by Sex and Order
desc_stats <- data %>%
  group_by(Sex, Order) %>%
  summarise(
    PR_mean = mean(`PR Pellets`, na.rm = TRUE),
    PR_sd = sd(`PR Pellets`, na.rm = TRUE),
    PR_min = min(`PR Pellets`, na.rm = TRUE),
    PR_max = max(`PR Pellets`, na.rm = TRUE),
    PR_median = median(`PR Pellets`, na.rm = TRUE),
    NR_mean = mean(`NR Pellets`, na.rm = TRUE),
    NR_sd = sd(`NR Pellets`, na.rm = TRUE),
    NR_min = min(`NR Pellets`, na.rm = TRUE),
    NR_max = max(`NR Pellets`, na.rm = TRUE),
    NR_median = median(`NR Pellets`, na.rm = TRUE)
  )

# Save descriptive stats to CSV
write.csv(desc_stats, "C:/Users/hta031/Github/FEDProtein/results/Total_pellets_per_phase_stats/descriptive_stats.csv", row.names = FALSE)

# Print descriptive stats
print(desc_stats)

# Step 2: Reshape the data to long format for ANOVA
data_long <- melt(data, id.vars = c("Mouse.ID", "Sex", "Order"),
                  measure.vars = c("PR.Pellets", "NR.Pellets"),
                  variable.name = "Diet_Phase", value.name = "Pellets")

# Step 3: Run ANOVA
anova_model <- aov(Pellets ~ Sex * Order * Diet_Phase, data = data_long)
anova_results <- Anova(anova_model, type = 2)  # Type II ANOVA for factorial design

# Save ANOVA results to CSV
anova_table <- as.data.frame(anova_results)
write.csv(anova_table, "C:/Users/hta031/Github/FEDProtein/results/Total_pellets_per_phase_stats/anova_results.csv", row.names = FALSE)

# Print ANOVA results
print(anova_results)

# Step 4: Post-hoc tests using Tukey HSD
tukey_test <- TukeyHSD(anova_model, "Diet_Phase")
tukey_table <- as.data.frame(tukey_test$Diet_Phase)

# Save Tukey test results to CSV
write.csv(tukey_table, "C:/Users/hta031/Github/FEDProtein/results/Total_pellets_per_phase_stats/tukey_results.csv", row.names = FALSE)

# Print Tukey test results
print(tukey_test)

# Step 5: Post-hoc Holm test (pairwise t-tests with Holm adjustment)
pairwise_results <- pairwise.t.test(data_long$Pellets, 
                                    interaction(data_long$Sex, data_long$Order, data_long$Diet_Phase), 
                                    p.adjust.method = "holm")

# Save Holm test results to CSV
holm_table <- as.data.frame(pairwise_results$p.value)
write.csv(holm_table, "C:/Users/hta031/Github/FEDProtein/results/Total_pellets_per_phase_stats/holm_results.csv", row.names = FALSE)

# Print Holm test results
print(pairwise_results)






################################################################################################
# Load necessary libraries
library(tidyverse)  # Includes dplyr and ggplot2
library(reshape2)   # For melt function
library(car)        # For Anova function

# Load the data
data <- read.csv("C:/Users/hta031/Github/FEDProtein/results/Total_pellets_per_phase_stats/total_pellet_per_phase.csv")

# Check column names to ensure they match what is used in the code
colnames(data)

# Step 1: Descriptive statistics for PR and NR phases grouped by Sex and Order
desc_stats <- data %>%
  group_by(Sex, Order) %>%
  summarise(
    PR_mean = mean(`PR Pellets`, na.rm = TRUE),
    PR_sd = sd(`PR Pellets`, na.rm = TRUE),
    PR_min = min(`PR Pellets`, na.rm = TRUE),
    PR_max = max(`PR Pellets`, na.rm = TRUE),
    PR_median = median(`PR Pellets`, na.rm = TRUE),
    NR_mean = mean(`NR Pellets`, na.rm = TRUE),
    NR_sd = sd(`NR Pellets`, na.rm = TRUE),
    NR_min = min(`NR Pellets`, na.rm = TRUE),
    NR_max = max(`NR Pellets`, na.rm = TRUE),
    NR_median = median(`NR Pellets`, na.rm = TRUE)
  )

# Save descriptive stats to CSV
write.csv(desc_stats, "C:/Users/hta031/Github/FEDProtein/results/Total_pellets_per_phase_stats/descriptive_stats.csv", row.names = FALSE)

# Print descriptive stats
print(desc_stats)

# Step 2: Reshape the data to long format for ANOVA
data_long <- melt(data, id.vars = c("Mouse.ID", "Sex", "Order"),
                  measure.vars = c("PR.Pellets", "NR.Pellets"),
                  variable.name = "Diet_Phase", value.name = "Pellets")

# Step 3: Run ANOVA
anova_model <- aov(Pellets ~ Sex * Order * Diet_Phase, data = data_long)
anova_results <- Anova(anova_model, type = 2)  # Type II ANOVA for factorial design

# Include the group labels in the ANOVA table
anova_table <- as.data.frame(anova_results)
anova_table$Factor <- rownames(anova_table)
rownames(anova_table) <- NULL

# Save ANOVA results to CSV
write.csv(anova_table, "C:/Users/hta031/Github/FEDProtein/results/Total_pellets_per_phase_stats/anova_results_with_groups.csv", row.names = FALSE)

# Print ANOVA results
print(anova_results)

# Step 4: Post-hoc tests using Tukey HSD
tukey_test <- TukeyHSD(anova_model, "Diet_Phase")
tukey_table <- as.data.frame(tukey_test$Diet_Phase)

# Add group labels to Tukey results
tukey_table$Comparison <- rownames(tukey_table)
rownames(tukey_table) <- NULL

# Save Tukey test results to CSV
write.csv(tukey_table, "C:/Users/hta031/Github/FEDProtein/results/Total_pellets_per_phase_stats/tukey_results_with_groups.csv", row.names = FALSE)

# Print Tukey test results
print(tukey_test)

# Step 5: Post-hoc Holm test (pairwise t-tests with Holm adjustment)
pairwise_results <- pairwise.t.test(data_long$Pellets, 
                                    interaction(data_long$Sex, data_long$Order, data_long$Diet_Phase), 
                                    p.adjust.method = "holm")

# Extract and format Holm test results with group labels
holm_table <- as.data.frame(pairwise_results$p.value)
holm_table$Comparison <- rownames(holm_table)
rownames(holm_table) <- NULL

# Save Holm test results to CSV
write.csv(holm_table, "C:/Users/hta031/Github/FEDProtein/results/Total_pellets_per_phase_stats/holm_results_with_groups.csv", row.names = FALSE)

# Print Holm test results
print(pairwise_results)




############################# making sure Tukey is conduced correctly###################

# Load necessary libraries
library(tidyverse)
library(reshape2)
library(car)
library(multcomp)

# Load the data
data <- read.csv("C:/Users/hta031/Github/FEDProtein/results/Total_pellets_per_phase_stats/total_pellet_per_phase.csv")

# Reshape the data to long format for ANOVA
data_long <- melt(data, id.vars = c("Mouse.ID", "Sex", "Order"),
                  measure.vars = c("PR.Pellets", "NR.Pellets"),
                  variable.name = "Diet_Phase", value.name = "Pellets")

# Run ANOVA with interaction between Sex, Order, and Diet_Phase
anova_model <- aov(Pellets ~ Sex * Order * Diet_Phase, data = data_long)

# Create an interaction term between Sex, Order, and Diet_Phase
data_long$interaction_term <- interaction(data_long$Sex, data_long$Order, data_long$Diet_Phase)

# Apply Tukey HSD to the interaction term
tukey_test <- TukeyHSD(aov(Pellets ~ interaction_term, data = data_long))

# Break the interaction term into separate components
comparison_labels <- str_split_fixed(rownames(tukey_test$interaction_term), "-", 2)
group1 <- comparison_labels[, 1]
group2 <- comparison_labels[, 2]

# Convert Tukey test results to a data frame and add group labels
tukey_table <- as.data.frame(tukey_test$interaction_term)
tukey_table$Group1 <- group1
tukey_table$Group2 <- group2

# Save the results with group labels to a CSV file
write.csv(tukey_table, "C:/Users/hta031/Github/FEDProtein/results/Total_pellets_per_phase_stats/tukey_results_with_group_labels.csv", row.names = FALSE)

# Print Tukey test results with group labels
print(tukey_table)


######################## ANOVA shows an Order is significant, but posthoc tests do not confrim this.####################
######here is the code just to look at averages########
# Load necessary libraries
library(tidyverse)

# Load the data
data <- read.csv("C:/Users/hta031/Github/FEDProtein/results/Total_pellets_per_phase_stats/total_pellet_per_phase.csv")

# Reshape the data to long format
data_long <- melt(data, id.vars = c("Mouse.ID", "Sex", "Order"),
                  measure.vars = c("PR.Pellets", "NR.Pellets"),
                  variable.name = "Diet_Phase", value.name = "Pellets")

# Calculate the mean pellet intake by Sex, Order, and Diet Phase
order_sex_means <- data_long %>%
  group_by(Sex, Order, Diet_Phase) %>%
  summarise(mean_pellets = mean(Pellets, na.rm = TRUE),
            sd_pellets = sd(Pellets, na.rm = TRUE),
            n = n())

# Print the means for each group
print(order_sex_means)



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
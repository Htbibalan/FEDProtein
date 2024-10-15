# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(car)  # For ANOVA and posthoc tests
library(multcomp)  # For Holm correction

# Step 1: Load the data
data <- read.csv("results\\Protein_intake_per_phase_per_day\\protein_intake_per_mouse.csv")

# Step 2: Filter out the first 3 days (Grain pellets)
data_filtered <- data %>%
  filter(Day > 3)  # Remove Grain days 1, 2, 3

# Step 3: Ensure that Sex, Order, and Day are factors for ANOVA and posthoc tests
data_filtered$Sex <- as.factor(data_filtered$Sex)
data_filtered$Order <- as.factor(data_filtered$Order)
data_filtered$Day <- as.factor(data_filtered$Day)

# Step 4: Descriptive statistics (mean, SD) grouped by Order and Sex
descriptive_stats <- data_filtered %>%
  group_by(Sex, Order, Day) %>%
  summarise(
    Mean_Protein = mean(Protein.Intake..g., na.rm = TRUE),
    SD_Protein = sd(Protein.Intake..g., na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  )

# Save descriptive statistics to CSV
write.csv(descriptive_stats, "results\\Protein_intake_per_phase_per_day\\descriptive_stats_protein.csv")

# Step 5: Two-Way ANOVA (between and within groups)
anova_results <- aov(Protein.Intake..g. ~ Sex * Order * Day, data = data_filtered)

# Print ANOVA summary
anova_summary <- summary(anova_results)
print(anova_summary)

# Save ANOVA results to CSV
anova_table <- as.data.frame(anova_summary[[1]])
write.csv(anova_table, "results\\Protein_intake_per_phase_per_day\\anova_results_protein.csv")

# Step 6: Posthoc Tukey HSD test
# Apply Tukey HSD to the interaction term 'Sex:Order:Day' explicitly
tukey_results <- TukeyHSD(anova_results, "Sex:Order:Day", conf.level = 0.95)

# Save Tukey HSD results in a clear format
tukey_table <- as.data.frame(tukey_results$`Sex:Order:Day`)  # Extract the interaction term results
tukey_table$comparison <- rownames(tukey_table)  # Add the comparison column to show group names

# Save the Tukey results
write.csv(tukey_table, "results\\Protein_intake_per_phase_per_day\\tukey_posthoc_results.csv", row.names = FALSE)

# Step 7: Posthoc Holm correction
# Use glht from multcomp for Holm correction on the interaction term 'Sex:Order:Day'
holm_results <- glht(anova_results, linfct = mcp(`Sex:Order:Day` = "Tukey"))

# Get summary and extract p-values
holm_summary <- summary(holm_results, test = adjusted("holm"))
holm_pvalues <- as.data.frame(holm_summary$test$pvalues)

# Add group comparisons to Holm results
holm_pvalues$comparison <- rownames(holm_summary$test$pvalues)

# Save Holm posthoc results
write.csv(holm_pvalues, "results\\Protein_intake_per_phase_per_day\\holm_posthoc_results.csv", row.names = FALSE)

# Optional: Visualization of mean protein intake per day
ggplot(descriptive_stats, aes(x = as.factor(Day), y = Mean_Protein, color = Sex, group = interaction(Sex, Order))) +
  geom_line() +
  geom_point() +
  facet_wrap(~Order) +
  labs(title = "Mean Protein Intake Per Day by Sex and Order", x = "Day", y = "Mean Protein Intake (g)") +
  theme_minimal()





#########################################
#####################################
########################################
####################################
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(car)  # For ANOVA and posthoc tests
library(multcomp)  # For Holm correction

# Step 1: Load the data
data <- read.csv("results\\Protein_intake_per_phase_per_day\\protein_intake_per_mouse.csv")

# Step 2: Filter out the first 3 days (Grain pellets)
data_filtered <- data %>%
  filter(Day > 3)  # Remove Grain days 1, 2, 3

# Step 3: Ensure that Sex, Order, and Day are factors for ANOVA and posthoc tests
data_filtered$Sex <- as.factor(data_filtered$Sex)
data_filtered$Order <- as.factor(data_filtered$Order)
data_filtered$Day <- as.factor(data_filtered$Day)

# Step 4: Descriptive statistics (mean, SD) grouped by Order and Sex
descriptive_stats <- data_filtered %>%
  group_by(Sex, Order, Day) %>%
  summarise(
    Mean_Protein = mean(Protein.Intake..g., na.rm = TRUE),
    SD_Protein = sd(Protein.Intake..g., na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  )

# Save descriptive statistics to CSV
write.csv(descriptive_stats, "results\\Protein_intake_per_phase_per_day\\descriptive_stats_protein.csv")

# Step 5: Two-Way ANOVA (between and within groups)
anova_results <- aov(Protein.Intake..g. ~ Sex * Order * Day, data = data_filtered)

# Print ANOVA summary
anova_summary <- summary(anova_results)
print(anova_summary)

# Save ANOVA results to CSV
anova_table <- as.data.frame(anova_summary[[1]])
write.csv(anova_table, "results\\Protein_intake_per_phase_per_day\\anova_results_protein.csv")

# Step 6: Posthoc Tukey HSD test
# Apply Tukey HSD to the interaction term 'Sex:Order:Day' explicitly
tukey_results <- TukeyHSD(anova_results, "Sex:Order:Day", conf.level = 0.95)

# Save Tukey HSD results in a clear format
tukey_table <- as.data.frame(tukey_results$`Sex:Order:Day`)
tukey_table$comparison <- rownames(tukey_table)  # Add the comparison column to show group names

# Save the Tukey results
write.csv(tukey_table, "results\\Protein_intake_per_phase_per_day\\tukey_posthoc_results.csv", row.names = FALSE)

# Step 7: Posthoc Holm correction with distinct group comparisons

# Simplified Holm test for each factor separately (Sex, Order, Day)
holm_results_sex <- glht(anova_results, linfct = mcp(Sex = "Tukey"))
holm_summary_sex <- summary(holm_results_sex, test = adjusted("holm"))
holm_table_sex <- data.frame(
  comparison = rownames(holm_summary_sex$test$coefficients),
  p_value = holm_summary_sex$test$pvalues,
  factor = 'Sex'  # Add the factor label
)

holm_results_order <- glht(anova_results, linfct = mcp(Order = "Tukey"))
holm_summary_order <- summary(holm_results_order, test = adjusted("holm"))
holm_table_order <- data.frame(
  comparison = rownames(holm_summary_order$test$coefficients),
  p_value = holm_summary_order$test$pvalues,
  factor = 'Order'  # Add the factor label
)

holm_results_day <- glht(anova_results, linfct = mcp(Day = "Tukey"))
holm_summary_day <- summary(holm_results_day, test = adjusted("holm"))
holm_table_day <- data.frame(
  comparison = rownames(holm_summary_day$test$coefficients),
  p_value = holm_summary_day$test$pvalues,
  factor = 'Day'  # Add the factor label
)

# Combine Holm results from each factor
holm_table <- rbind(holm_table_sex, holm_table_order, holm_table_day)

# Ensure all columns are correctly labeled
colnames(holm_table) <- c("comparison", "p_value", "factor")

# Save Holm posthoc results with clear comparisons
write.csv(holm_table, "results\\Protein_intake_per_phase_per_day\\holm_posthoc_results.csv", row.names = FALSE)

# Optional: Visualization of mean protein intake per day
ggplot(descriptive_stats, aes(x = as.factor(Day), y = Mean_Protein, color = Sex, group = interaction(Sex, Order))) +
  geom_line() +
  geom_point() +
  facet_wrap(~Order) +
  labs(title = "Mean Protein Intake Per Day by Sex and Order", x = "Day", y = "Mean Protein Intake (g)") +
  theme_minimal()





#############################################################################
################ average per phase############################################

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(car)
library(multcomp)

# Read your filtered data
data <- read.csv("results\\Protein_intake_per_phase_per_day\\avg_protein_intake.csv")

# Convert Sex, Order, and Phase to factors
data$Sex <- as.factor(data$Sex)
data$Order <- as.factor(data$Order)
data$Phase <- as.factor(data$Phase)

# Descriptive statistics
descriptive_stats <- data %>%
  group_by(Sex, Order, Phase) %>%
  summarize(
    Mean = mean(Average.Protein.Intake..g.),
    SD = sd(Average.Protein.Intake..g.),
    Min = min(Average.Protein.Intake..g.),
    Max = max(Average.Protein.Intake..g.)
  )

# Print descriptive statistics
print(descriptive_stats)

# Perform ANOVA
anova_result <- aov(Average.Protein.Intake..g. ~ Sex * Order * Phase, data = data)
summary_anova <- summary(anova_result)
print(summary_anova)

# Posthoc Tukey test
tukey_result <- TukeyHSD(anova_result, "Phase")  # Use "Phase" because it's the most significant
print(tukey_result)

# Posthoc Holm test
holm_result <- glht(anova_result, linfct = mcp(Phase = "Tukey"))  # Only Phase is significant
summary_holm <- summary(holm_result)
print(summary_holm)

# Save results to CSV files
write.csv(descriptive_stats, "results\\Protein_intake_per_phase_per_day\\AVdescriptive_stats.csv")
write.csv(as.data.frame(summary(anova_result)[[1]]), "results\\Protein_intake_per_phase_per_day\\AVanova_result.csv")
write.csv(as.data.frame(tukey_result$Phase), "results\\Protein_intake_per_phase_per_day\\AVtukey_result.csv")
write.csv(as.data.frame(summary_holm$test$pvalues), "results\\Protein_intake_per_phase_per_day\\AVholm_result.csv")



######################################fixing holm and tukey lables in Average analysis###########
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(car)
library(multcomp)

# Read your filtered data
data <- read.csv("results\\Protein_intake_per_phase_per_day\\avg_protein_intake.csv")

# Convert Sex, Order, and Phase to factors
data$Sex <- as.factor(data$Sex)
data$Order <- as.factor(data$Order)
data$Phase <- as.factor(data$Phase)

# Filter out the Grain phase
data_filtered <- data %>% filter(Phase != "Grain")

# Descriptive statistics (only NR and PR phases)
descriptive_stats <- data_filtered %>%
  group_by(Sex, Order, Phase) %>%
  summarize(
    Mean = mean(Average.Protein.Intake..g.),
    SD = sd(Average.Protein.Intake..g.),
    Min = min(Average.Protein.Intake..g.),
    Max = max(Average.Protein.Intake..g.)
  )

# Print descriptive statistics
print(descriptive_stats)

# Perform ANOVA including interactions between Sex, Order, and Phase
anova_result <- aov(Average.Protein.Intake..g. ~ Sex * Order * Phase, data = data_filtered)
summary_anova <- summary(anova_result)
print(summary_anova)

# Posthoc Tukey test for the interaction of Order and Phase
tukey_result <- TukeyHSD(anova_result, "Order:Phase")  # Apply to Order and Phase interaction

# Convert Tukey results into dataframe
tukey_df <- as.data.frame(tukey_result$`Order:Phase`)
tukey_df$Comparison <- rownames(tukey_result$`Order:Phase`)
rownames(tukey_df) <- NULL  # Reset rownames for clean output
print(tukey_df)

# Posthoc Holm test for interactions of Order and Phase
holm_result <- glht(anova_result, linfct = mcp(`Order:Phase` = "Tukey"))
summary_holm <- summary(holm_result)

# Convert Holm test results into dataframe
holm_df <- data.frame(Comparison = names(summary_holm$test$tstat), p_value = summary_holm$test$pvalues)
print(holm_df)

# Save results to CSV files
write.csv(descriptive_stats, "results\\Protein_intake_per_phase_per_day\\AVdescriptive_stats.csv")
write.csv(as.data.frame(summary(anova_result)[[1]]), "results\\Protein_intake_per_phase_per_day\\AVanova_result.csv")
write.csv(tukey_df, "results\\Protein_intake_per_phase_per_day\\AVtukey_result.csv")  # Save Tukey with labels
write.csv(holm_df, "results\\Protein_intake_per_phase_per_day\\AVholm_result.csv")  # Save Holm with labels



#######################################################
######################################################
###################################################fixing holm#######################################
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(car)
library(multcomp)

# Read your filtered data
data <- read.csv("results\\Protein_intake_per_phase_per_day\\avg_protein_intake.csv")

# Convert Sex, Order, and Phase to factors
data$Sex <- as.factor(data$Sex)
data$Order <- as.factor(data$Order)
data$Phase <- as.factor(data$Phase)

# Filter out the Grain phase
data_filtered <- data %>% filter(Phase != "Grain")

# Descriptive statistics (only NR and PR phases)
descriptive_stats <- data_filtered %>%
  group_by(Sex, Order, Phase) %>%
  summarize(
    Mean = mean(Average.Protein.Intake..g.),
    SD = sd(Average.Protein.Intake..g.),
    Min = min(Average.Protein.Intake..g.),
    Max = max(Average.Protein.Intake..g.)
  )

# Print descriptive statistics
print(descriptive_stats)

# Perform ANOVA including interactions between Sex, Order, and Phase
anova_result <- aov(Average.Protein.Intake..g. ~ Sex * Order * Phase, data = data_filtered)
summary_anova <- summary(anova_result)
print(summary_anova)

# Posthoc Tukey test for the interaction of Order and Phase
tukey_result <- TukeyHSD(anova_result, "Order:Phase")  # Apply to Order and Phase interaction

# Convert Tukey results into dataframe
tukey_df <- as.data.frame(tukey_result$`Order:Phase`)
tukey_df$Comparison <- rownames(tukey_result$`Order:Phase`)
rownames(tukey_df) <- NULL  # Reset rownames for clean output
print(tukey_df)

# Posthoc Holm test for interactions of Order and Phase
# Manual comparison of Order and Phase interaction using glht
contrasts_order_phase <- rbind(
    "1:NR vs 1:PR" = c(1, -1, 0, 0),
    "2:NR vs 2:PR" = c(0, 0, 1, -1),
    "1:NR vs 2:NR" = c(1, 0, -1, 0),
    "1:PR vs 2:PR" = c(0, 1, 0, -1)
)

# Create custom contrasts for Order and Phase interaction
interaction_model <- interaction(data_filtered$Order, data_filtered$Phase)
anova_result_interaction <- aov(Average.Protein.Intake..g. ~ interaction_model, data = data_filtered)

# Apply Holm correction using glht with custom contrasts
holm_result <- glht(anova_result_interaction, linfct = mcp(interaction_model = contrasts_order_phase))
summary_holm <- summary(holm_result)

# Convert Holm test results into dataframe
holm_df <- data.frame(Comparison = names(summary_holm$test$tstat), p_value = summary_holm$test$pvalues)
print(holm_df)

# Save results to CSV files
write.csv(descriptive_stats, "results\\Protein_intake_per_phase_per_day\\AVdescriptive_stats.csv")
write.csv(as.data.frame(summary(anova_result)[[1]]), "results\\Protein_intake_per_phase_per_day\\AVanova_result.csv")
write.csv(tukey_df, "results\\Protein_intake_per_phase_per_day\\AVtukey_result.csv")  # Save Tukey with labels
write.csv(holm_df, "results\\Protein_intake_per_phase_per_day\\AVholm_result.csv")  # Save Holm with labels

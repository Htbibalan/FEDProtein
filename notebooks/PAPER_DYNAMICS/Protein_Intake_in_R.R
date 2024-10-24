

###############################################################################################
############################# FORMATTED DATA NEW ANALYSIS ####################################
###################################LINE PLOTS#############################################################



library(tidyr)
library(dplyr)
library(afex)
library(emmeans)
library(ggplot2)

# Load the dataset
data <- read.csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\Protein_intake_per_phase_per_day\\LINE_PLOT_STATS\\Protein_intake_trend.csv")

# Step 1: Reshape the data
long_data <- data %>%
  pivot_longer(cols = starts_with(c("NR", "PR")),
               names_to = "time_phase",
               values_to = "value")

# Step 2: Generate Descriptive Statistics
descriptive_stats <- long_data %>%
  group_by(time_phase, Sex, Order) %>%
  summarise(mean_value = mean(value, na.rm = TRUE),
            sd_value = sd(value, na.rm = TRUE),
            n = n())

# Save descriptive statistics to CSV
write.csv(descriptive_stats, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\Protein_intake_per_phase_per_day\\LINE_PLOT_STATS\\protein_intake_trend_descriptive_stats.csv")

# Step 3: Run ANOVA for each time point, sex, and order
anova_results <- aov_car(value ~ time_phase * Sex * Order + Error(Mouse/time_phase), data = long_data)





# Save ANOVA results to CSV
write.csv(as.data.frame(anova(anova_results)), "C:\\Users\\hta031\\Github\\FEDProtein\\results\\Protein_intake_per_phase_per_day\\LINE_PLOT_STATS\\protein_intake_trend_anova_results.csv")

# Step 4: Post-hoc tests to compare each time point with others (including interactions with Sex and Order)
posthoc_results <- emmeans(anova_results, pairwise ~ time_phase * Sex * Order, adjust= "holm")

# Extract the contrasts for time_phase comparisons
posthoc_contrasts <- summary(posthoc_results$contrasts)




# Convert post-hoc results to a data frame including information on time_phase, Sex, and Order
posthoc_df <- as.data.frame(posthoc_contrasts)

# Save post-hoc results to CSV (including time_phase, Sex, and Order)
write.csv(as.data.frame(posthoc_results$contrasts), "C:\\Users\\hta031\\Github\\FEDProtein\\results\\Protein_intake_per_phase_per_day\\LINE_PLOT_STATS\\protein_intake_trend_posthoc_results_HOLM.csv")




###################################################################################################
########################## Scatter_plots_data ####################################################


# Load necessary libraries
library(tidyverse)
library(reshape2
library(car)  # For ANOVA
library(multcomp)  # For Tukey and Holm post-hoc tests

# Load the data
data <- read.csv("C:/Users/hta031/Github/FEDProtein/results/Protein_intake_per_phase_per_day/scatter_plot_average_per_phase/Average_Protein_Intake_per_Phase.csv")

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
write.csv(desc_stats, "C:/Users/hta031/Github/FEDProtein/results/Protein_intake_per_phase_per_day/scatter_plot_average_per_phase/AV_PROTEIN_INTAKE_DESCRIPTIVE.csv", row.names = FALSE)

# Step 2: Run ANOVA with grouping information
anova_model <- aov(Total_Parameters ~ Sex * Order * Diet_Phase, data = data_long)
anova_results <- Anova(anova_model, type = 2)

# Create ANOVA table with grouping information
anova_table <- data.frame(
  Factor = rownames(anova_results),
  anova_results
)

# Save ANOVA results to CSV
write.csv(anova_table, "C:/Users/hta031/Github/FEDProtein/results/Protein_intake_per_phase_per_day/scatter_plot_average_per_phase/anova_results_AV_INTAKE.csv", row.names = FALSE)

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
write.csv(tukey_table, "C:/Users/hta031/Github/FEDProtein/results/Protein_intake_per_phase_per_day/scatter_plot_average_per_phase/tukey_results_AV_INTAKE.csv", row.names = FALSE)

# Step 4: Holm post-hoc test
# Perform pairwise t-tests with Holm correction
holm_test <- pairwise.t.test(data_long$Total_Parameters, data_long$interaction_term, p.adjust.method = "holm")

# Extract and save Holm test results to CSV
holm_table <- as.data.frame(holm_test$p.value)
holm_table$Comparison <- rownames(holm_table)
write.csv(holm_table, "C:/Users/hta031/Github/FEDProtein/results/Protein_intake_per_phase_per_day/scatter_plot_average_per_phase//holm_results_AV_INTAKE.csv", row.names = FALSE)

# Print summary of results
print(desc_stats)
print(anova_table)
print(tukey_table)
print(holm_table)

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





##########################################################################
######################### DYNAMICS OF INTERACTION TIME ####################


library(tidyr)
library(dplyr)
library(afex)
library(emmeans)
library(ggplot2)

# Load the dataset
data <- read.csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\Interaction_time_stats\\PER_DAY_DYNAMICS\\Expanded_Interaction_Times_Data.csv")

# Step 1: Reshape the data
long_data <- data %>%
  pivot_longer(cols = starts_with(c("NR", "PR")),
               names_to = "time_phase",
               values_to = "value")

# Step 2: Generate Descriptive Statistics
descriptive_stats <- long_data %>%
  group_by(time_phase, sex, order) %>%
  summarise(mean_value = mean(value, na.rm = TRUE),
            sd_value = sd(value, na.rm = TRUE),
            n = n())

# Save descriptive statistics to CSV
write.csv(descriptive_stats, "C://Users//hta031//Github//FEDProtein//results//Interaction_time_stats//PER_DAY_DYNAMICS//R_FEDPROTEIN_INTERACTION_trend_descriptive_stats.csv")

# Step 3: Run ANOVA for each time point, sex, and order
anova_results <- aov_car(value ~ time_phase * sex * order + Error(mouse_id/time_phase), data = long_data)





# Save ANOVA results to CSV
write.csv(as.data.frame(anova(anova_results)), "C://Users//hta031//Github//FEDProtein//results//Interaction_time_stats//PER_DAY_DYNAMICS//R_FEDPROTEIN_INTERACTION_TIME_trend_anova_results.csv")

# Step 4: Post-hoc tests to compare each time point with others (including interactions with Sex and Order)
posthoc_results <- emmeans(anova_results, pairwise ~ time_phase * sex * order, adjust= "holm")

# Extract the contrasts for time_phase comparisons
posthoc_contrasts <- summary(posthoc_results$contrasts)




# Convert post-hoc results to a data frame including information on time_phase, Sex, and Order
posthoc_df <- as.data.frame(posthoc_contrasts)

# Save post-hoc results to CSV (including time_phase, Sex, and Order)
write.csv(as.data.frame(posthoc_results$contrasts), "C://Users//hta031//Github//FEDProtein//results//Interaction_time_stats//PER_DAY_DYNAMICS//R_FEDPROTEIN_INTERACTION_TIME_trend_posthoc_results_HOLM.csv")



################################################################################################################
##################################### HOURLY PELLET##############################################



# Load necessary libraries
library(tidyverse)
library(rstatix)

# Load and reshape the dataset
data <- read.csv("C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/HOURLY_PELLET/Averaged_Hourly_Pellet_Intake.csv")
data <- data %>%
  pivot_longer(cols = c("PR", "NR"), names_to = "diet", values_to = "pellet_intake") %>%
  mutate(order = as.factor(order),
         sex = as.factor(sex),
         diet = factor(diet, levels = c("NR", "PR")),
         hour = as.factor(hour))

# Descriptive statistics
desc_stats <- data %>%
  group_by(order, sex, diet, hour) %>%
  summarise(mean_pellet_intake = mean(pellet_intake, na.rm = TRUE),
            sd_pellet_intake = sd(pellet_intake, na.rm = TRUE),
            .groups = 'drop')

# Save descriptive statistics
write.csv(desc_stats, "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/HOURLY_PELLET/Descriptive_Statistics.csv", row.names = FALSE)

# Three-way ANOVA: Effects of sex, order, and diet, and their interactions
anova_results <- data %>%
  anova_test(pellet_intake ~ sex * order * diet)

# Save ANOVA results
write.csv(anova_results, "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/HOURLY_PELLET/Three_Way_ANOVA_Results.csv", row.names = FALSE)

# Post hoc tests for significant interactions
# Let's focus on interactions if they are significant
posthoc_results <- data %>%
  group_by(sex, order, diet) %>%
  tukey_hsd(pellet_intake ~ hour) %>%
  adjust_pvalue(method = "holm")

# Save post hoc results for within-group hour comparisons
write.csv(posthoc_results, "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/HOURLY_PELLET/Posthoc_Interaction_Results.csv", row.names = FALSE)

# Between-group ANOVA comparisons for diet within each hour
between_anova_results <- data %>%
  group_by(order, sex, hour) %>%
  anova_test(pellet_intake ~ diet) %>%
  adjust_pvalue(method = "holm") %>%
  get_anova_table()

# Save between-group ANOVA results
write.csv(between_anova_results, "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/HOURLY_PELLET/Between_Group_ANOVA_Results.csv", row.names = FALSE)

# Between-group post hoc tests for diet comparison within each hour
between_posthoc_results <- data %>%
  group_by(order, sex, hour) %>%
  tukey_hsd(pellet_intake ~ diet) %>%
  adjust_pvalue(method = "holm")

# Save between-group post hoc results
write.csv(between_posthoc_results, "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/HOURLY_PELLET/Between_Group_Posthoc_Results.csv", row.names = FALSE)

# Print completion message
cat("Three-way ANOVA, post hoc interaction tests, and between-group post hoc analyses complete. Results saved in the specified directory.\n")















################################################################################################################
##################################### HOURLY MEAL##############################################



# Load necessary libraries
library(tidyverse)
library(rstatix)

# Load and reshape the dataset
data <- read.csv("C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/MEALS/Meals_Hourly_Separated.csv")
data <- data %>%
  pivot_longer(cols = c("PR", "NR"), names_to = "diet", values_to = "pellet_intake") %>%
  mutate(order = as.factor(order),
         sex = as.factor(sex),
         diet = factor(diet, levels = c("NR", "PR")),
         hour = as.factor(hour))

# Descriptive statistics
desc_stats <- data %>%
  group_by(order, sex, diet, hour) %>%
  summarise(mean_pellet_intake = mean(pellet_intake, na.rm = TRUE),
            sd_pellet_intake = sd(pellet_intake, na.rm = TRUE),
            .groups = 'drop')

# Save descriptive statistics
write.csv(desc_stats, "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/MEALS/Descriptive_Statistics.csv", row.names = FALSE)

# Three-way ANOVA: Effects of sex, order, and diet, and their interactions
anova_results <- data %>%
  anova_test(pellet_intake ~ sex * order * diet)

# Save ANOVA results
write.csv(anova_results, "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/MEALS/Three_Way_ANOVA_Results.csv", row.names = FALSE)

# Post hoc tests for significant interactions
# Let's focus on interactions if they are significant
posthoc_results <- data %>%
  group_by(sex, order, diet) %>%
  tukey_hsd(pellet_intake ~ hour) %>%
  adjust_pvalue(method = "holm")

# Save post hoc results for within-group hour comparisons
write.csv(posthoc_results, "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/MEALS/Posthoc_Interaction_Results.csv", row.names = FALSE)

# Between-group ANOVA comparisons for diet within each hour
between_anova_results <- data %>%
  group_by(order, sex, hour) %>%
  anova_test(pellet_intake ~ diet) %>%
  adjust_pvalue(method = "holm") %>%
  get_anova_table()

# Save between-group ANOVA results
write.csv(between_anova_results, "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/MEALS/Between_Group_ANOVA_Results.csv", row.names = FALSE)

# Between-group post hoc tests for diet comparison within each hour
between_posthoc_results <- data %>%
  group_by(order, sex, hour) %>%
  tukey_hsd(pellet_intake ~ diet) %>%
  adjust_pvalue(method = "holm")

# Save between-group post hoc results
write.csv(between_posthoc_results, "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/MEALS/Between_Group_Posthoc_Results.csv", row.names = FALSE)

# Print completion message
cat("Three-way ANOVA, post hoc interaction tests, and between-group post hoc analyses complete. Results saved in the specified directory.\n")








##########################################################################################################################
###################################### HOURLY FEAST####################################################################

# Load necessary libraries
library(tidyverse)
library(rstatix)

# Load and reshape the dataset
data <- read.csv("C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/MEGA_MEALS/Mega_meals_Hourly_Separated.csv")
data <- data %>%
  pivot_longer(cols = c("PR", "NR"), names_to = "diet", values_to = "pellet_intake") %>%
  mutate(order = as.factor(order),
         sex = as.factor(sex),
         diet = factor(diet, levels = c("NR", "PR")),
         hour = as.factor(hour))

# Descriptive statistics
desc_stats <- data %>%
  group_by(order, sex, diet, hour) %>%
  summarise(mean_pellet_intake = mean(pellet_intake, na.rm = TRUE),
            sd_pellet_intake = sd(pellet_intake, na.rm = TRUE),
            .groups = 'drop')

# Save descriptive statistics
write.csv(desc_stats, "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/MEGA_MEALS/Descriptive_Statistics.csv", row.names = FALSE)

# Three-way ANOVA: Effects of sex, order, and diet, and their interactions
anova_results <- data %>%
  anova_test(pellet_intake ~ sex * order * diet)

# Save ANOVA results
write.csv(anova_results, "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/MEGA_MEALS/Three_Way_ANOVA_Results.csv", row.names = FALSE)

# Post hoc tests for significant interactions
# Let's focus on interactions if they are significant
posthoc_results <- data %>%
  group_by(sex, order, diet) %>%
  tukey_hsd(pellet_intake ~ hour) %>%
  adjust_pvalue(method = "holm")

# Save post hoc results for within-group hour comparisons
write.csv(posthoc_results, "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/MEGA_MEALS/Posthoc_Interaction_Results.csv", row.names = FALSE)

# Between-group ANOVA comparisons for diet within each hour
between_anova_results <- data %>%
  group_by(order, sex, hour) %>%
  anova_test(pellet_intake ~ diet) %>%
  adjust_pvalue(method = "holm") %>%
  get_anova_table()

# Save between-group ANOVA results
write.csv(between_anova_results, "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/MEGA_MEALS/Between_Group_ANOVA_Results.csv", row.names = FALSE)

# Between-group post hoc tests for diet comparison within each hour
between_posthoc_results <- data %>%
  group_by(order, sex, hour) %>%
  tukey_hsd(pellet_intake ~ diet) %>%
  adjust_pvalue(method = "holm")

# Save between-group post hoc results
write.csv(between_posthoc_results, "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/MEGA_MEALS/Between_Group_Posthoc_Results.csv", row.names = FALSE)

# Print completion message
cat("Three-way ANOVA, post hoc interaction tests, and between-group post hoc analyses complete. Results saved in the specified directory.\n")







##########################################################################################################################
###################################### HOURLY SNACK####################################################################

 #Load necessary libraries
library(tidyverse)
library(rstatix)

# Load and reshape the dataset
data <- read.csv("C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/SNACKS/Snacks_Hourly_Separated.csv")
data <- data %>%
  pivot_longer(cols = c("PR", "NR"), names_to = "diet", values_to = "pellet_intake") %>%
  mutate(order = as.factor(order),
         sex = as.factor(sex),
         diet = factor(diet, levels = c("NR", "PR")),
         hour = as.factor(hour))

# Descriptive statistics
desc_stats <- data %>%
  group_by(order, sex, diet, hour) %>%
  summarise(mean_pellet_intake = mean(pellet_intake, na.rm = TRUE),
            sd_pellet_intake = sd(pellet_intake, na.rm = TRUE),
            .groups = 'drop')

# Save descriptive statistics
write.csv(desc_stats, "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/SNACKS/Descriptive_Statistics.csv", row.names = FALSE)

# Three-way ANOVA: Effects of sex, order, and diet, and their interactions
anova_results <- data %>%
  anova_test(pellet_intake ~ sex * order * diet)

# Save ANOVA results
write.csv(anova_results, "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/SNACKS/Three_Way_ANOVA_Results.csv", row.names = FALSE)

# Post hoc tests for significant interactions
# Let's focus on interactions if they are significant
posthoc_results <- data %>%
  group_by(sex, order, diet) %>%
  tukey_hsd(pellet_intake ~ hour) %>%
  adjust_pvalue(method = "holm")

# Save post hoc results for within-group hour comparisons
write.csv(posthoc_results, "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/SNACKS/Posthoc_Interaction_Results.csv", row.names = FALSE)

# Between-group ANOVA comparisons for diet within each hour
between_anova_results <- data %>%
  group_by(order, sex, hour) %>%
  anova_test(pellet_intake ~ diet) %>%
  adjust_pvalue(method = "holm") %>%
  get_anova_table()

# Save between-group ANOVA results
write.csv(between_anova_results, "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/SNACKS/Between_Group_ANOVA_Results.csv", row.names = FALSE)

# Between-group post hoc tests for diet comparison within each hour
between_posthoc_results <- data %>%
  group_by(order, sex, hour) %>%
  tukey_hsd(pellet_intake ~ diet) %>%
  adjust_pvalue(method = "holm")

# Save between-group post hoc results
write.csv(between_posthoc_results, "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/SNACKS/Between_Group_Posthoc_Results.csv", row.names = FALSE)

# Print completion message
cat("Three-way ANOVA, post hoc interaction tests, and between-group post hoc analyses complete. Results saved in the specified directory.\n")


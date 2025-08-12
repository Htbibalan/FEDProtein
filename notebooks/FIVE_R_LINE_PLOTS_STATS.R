
################### code below conducting statistical analysis for the line plot data of pellet intake across days ###################

library(tidyr)
library(dplyr)
library(afex)
library(emmeans)
library(ggplot2)

# Load the dataset
data <- read.csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\PELLET_INTAKE\\Pellets_trend.csv")

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
write.csv(descriptive_stats, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\PELLET_INTAKE\\Pellets_trend_descriptive_stats.csv")

# Step 3: Run ANOVA for each time point, sex, and order
anova_results <- aov_car(value ~ time_phase * Sex * Order + Error(Mouse/time_phase), data = long_data)

# Save ANOVA results to CSV
write.csv(as.data.frame(anova(anova_results)), "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\PELLET_INTAKE\\Pellets_trend_anova_results.csv")

# Step 4: Post-hoc tests with Tukey adjustment
posthoc_tukey <- emmeans(anova_results, pairwise ~ time_phase * Sex * Order, adjust = "tukey")
posthoc_tukey_df <- as.data.frame(summary(posthoc_tukey$contrasts))
write.csv(posthoc_tukey_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\PELLET_INTAKE\\Pellets_trend_posthoc_tukey_results.csv")

# Step 5: Post-hoc tests with Holm adjustment
posthoc_holm <- emmeans(anova_results, pairwise ~ time_phase * Sex * Order, adjust = "holm")
posthoc_holm_df <- as.data.frame(summary(posthoc_holm$contrasts))
write.csv(posthoc_holm_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\PELLET_INTAKE\\Pellets_trend_posthoc_holm_results.csv")

######################################################## PELLET INTAKE COMBINED ###############################
#############################################################################################################

library(tidyr)
library(dplyr)
library(afex)
library(emmeans)
library(ggplot2)

# Load the dataset
data <- read.csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\PELLET_INTAKE\\Pellets_trend.csv")

# Step 1: Reshape the data
long_data <- data %>%
  pivot_longer(cols = starts_with(c("NR", "PR")),
               names_to = "time_phase",
               values_to = "value")

# Step 2: Generate Descriptive Statistics
descriptive_stats <- long_data %>%
  group_by(time_phase,Order) %>%
  summarise(mean_value = mean(value, na.rm = TRUE),
            sd_value = sd(value, na.rm = TRUE),
            n = n())

# Save descriptive statistics to CSV
write.csv(descriptive_stats, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\PELLET_INTAKE\\COMBINE_Pellets_trend_descriptive_stats.csv")

# Step 3: Run ANOVA for each time point, sex, and order
anova_results <- aov_car(value ~ time_phase  * Order + Error(Mouse/time_phase), data = long_data)

# Save ANOVA results to CSV
write.csv(as.data.frame(anova(anova_results)), "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\PELLET_INTAKE\\COMBINE_Pellets_trend_anova_results.csv")

# Step 4: Post-hoc tests with Tukey adjustment
posthoc_tukey <- emmeans(anova_results, pairwise ~ time_phase  * Order, adjust = "tukey")
posthoc_tukey_df <- as.data.frame(summary(posthoc_tukey$contrasts))
write.csv(posthoc_tukey_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\PELLET_INTAKE\\COMBINE_Pellets_trend_posthoc_tukey_results.csv")

# Step 5: Post-hoc tests with Holm adjustment
posthoc_holm <- emmeans(anova_results, pairwise ~ time_phase  * Order, adjust = "holm")
posthoc_holm_df <- as.data.frame(summary(posthoc_holm$contrasts))
write.csv(posthoc_holm_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\PELLET_INTAKE\\COMBINE_Pellets_trend_posthoc_holm_results.csv")


#################################################################################################################################################################################################

#################################################################################################################################################################################################

#################################################################################################################################################################################################


#################################################################################################################################################################################################
###################### SNACKS BELOW ######################

library(tidyr)
library(dplyr)
library(afex)
library(emmeans)
library(ggplot2)

# Load the dataset
data <- read.csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\SNACK\\snacks_per_day_trend.csv")

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
write.csv(descriptive_stats, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\SNACK\\snacks_trend_descriptive_stats.csv")

# Step 3: Run ANOVA for each time point, sex, and order
anova_results <- aov_car(value ~ time_phase * Sex * Order + Error(Mouse/time_phase), data = long_data)

# Save ANOVA results to CSV
write.csv(as.data.frame(anova(anova_results)), "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\SNACK\\snacks_trend_anova_results.csv")

# Step 4: Post-hoc tests with Tukey and Holm adjustments
# Tukey adjustment
posthoc_tukey_results <- emmeans(anova_results, pairwise ~ time_phase * Sex * Order, adjust = "Tukey")
posthoc_tukey_df <- as.data.frame(summary(posthoc_tukey_results$contrasts))

# Save Tukey-adjusted post-hoc results to CSV
write.csv(posthoc_tukey_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\SNACK\\snacks_trend_posthoc_results_Tukey.csv")

# Holm adjustment
posthoc_holm_results <- emmeans(anova_results, pairwise ~ time_phase * Sex * Order, adjust = "holm")
posthoc_holm_df <- as.data.frame(summary(posthoc_holm_results$contrasts))

# Save Holm-adjusted post-hoc results to CSV
write.csv(posthoc_holm_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\SNACK\\snacks_trend_posthoc_results_Holm.csv")
###############################################################################################################################################################################################
############################################################################ SNACKS COMBINED #################################################################################################
###############################################################################################################################################################################################


library(tidyr)
library(dplyr)
library(afex)
library(emmeans)
library(ggplot2)

# Load the dataset
data <- read.csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\SNACK\\snacks_per_day_trend.csv")

# Step 1: Reshape the data
long_data <- data %>%
  pivot_longer(cols = starts_with(c("NR", "PR")),
               names_to = "time_phase",
               values_to = "value")

# Step 2: Generate Descriptive Statistics
descriptive_stats <- long_data %>%
  group_by(time_phase, Order) %>%
  summarise(mean_value = mean(value, na.rm = TRUE),
            sd_value = sd(value, na.rm = TRUE),
            n = n())

# Save descriptive statistics to CSV
write.csv(descriptive_stats, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\SNACK\\COMBINE_snacks_trend_descriptive_stats.csv")

# Step 3: Run ANOVA for each time point, sex, and order
anova_results <- aov_car(value ~ time_phase * Order + Error(Mouse/time_phase), data = long_data)

# Save ANOVA results to CSV
write.csv(as.data.frame(anova(anova_results)), "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\SNACK\\COMBINE_snacks_trend_anova_results.csv")

# Step 4: Post-hoc tests with Tukey and Holm adjustments
# Tukey adjustment
posthoc_tukey_results <- emmeans(anova_results, pairwise ~ time_phase * Order, adjust = "Tukey")
posthoc_tukey_df <- as.data.frame(summary(posthoc_tukey_results$contrasts))

# Save Tukey-adjusted post-hoc results to CSV
write.csv(posthoc_tukey_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\SNACK\\COMBINE_snacks_trend_posthoc_results_Tukey.csv")

# Holm adjustment
posthoc_holm_results <- emmeans(anova_results, pairwise ~ time_phase * Order, adjust = "holm")
posthoc_holm_df <- as.data.frame(summary(posthoc_holm_results$contrasts))

# Save Holm-adjusted post-hoc results to CSV
write.csv(posthoc_holm_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\SNACK\\COMBINE_snacks_trend_posthoc_results_Holm.csv")


###############################################################################################################################################################################################
############################################################################ SNACKS Frequency COMBINED #################################################################################################
###############################################################################################################################################################################################


library(tidyr)
library(dplyr)
library(afex)
library(emmeans)
library(ggplot2)

# Load the dataset
data <- read.csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\SNACK\\snack_freq_per_day\\snack_freq_realigned_FINAL.csv")

# Step 1: Reshape the data
long_data <- data %>%
  pivot_longer(cols = starts_with(c("NR", "PR")),
               names_to = "time_phase",
               values_to = "value")

# Step 2: Generate Descriptive Statistics
descriptive_stats <- long_data %>%
  group_by(time_phase, Order) %>%
  summarise(mean_value = mean(value, na.rm = TRUE),
            sd_value = sd(value, na.rm = TRUE),
            n = n())

# Save descriptive statistics to CSV
write.csv(descriptive_stats, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\SNACK\\snack_freq_per_day\\COMBINE_snacksFreq_trend_descriptive_stats.csv")

# Step 3: Run ANOVA for each time point, sex, and order
anova_results <- aov_car(value ~ time_phase * Order + Error(Mouse/time_phase), data = long_data)

# Save ANOVA results to CSV
write.csv(as.data.frame(anova(anova_results)), "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\SNACK\\snack_freq_per_day\\COMBINE_snacksFreq_trend_anova_results.csv")

# Step 4: Post-hoc tests with Tukey and Holm adjustments
# Tukey adjustment
posthoc_tukey_results <- emmeans(anova_results, pairwise ~ time_phase * Order, adjust = "Tukey")
posthoc_tukey_df <- as.data.frame(summary(posthoc_tukey_results$contrasts))

# Save Tukey-adjusted post-hoc results to CSV
write.csv(posthoc_tukey_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\SNACK\\snack_freq_per_day\\COMBINE_snacksFreq_trend_posthoc_results_Tukey.csv")

# Holm adjustment
posthoc_holm_results <- emmeans(anova_results, pairwise ~ time_phase * Order, adjust = "holm")
posthoc_holm_df <- as.data.frame(summary(posthoc_holm_results$contrasts))

# Save Holm-adjusted post-hoc results to CSV
write.csv(posthoc_holm_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\SNACK\\snack_freq_per_day\\COMBINE_snacksFreq_trend_posthoc_results_Holm.csv")



###############################################################################################################################################################################################
############################################################################ MEALS BELOW #################################################################################################
###############################################################################################################################################################################################
library(tidyr)
library(dplyr)
library(afex)
library(emmeans)
library(ggplot2)

# Load the dataset
data <- read.csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\MEAL\\MEAL_per_day_trend.csv")

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
write.csv(descriptive_stats, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\MEAL\\MEAL_trend_descriptive_stats.csv")

# Step 3: Run ANOVA for each time point, sex, and order
anova_results <- aov_car(value ~ time_phase * Sex * Order + Error(Mouse/time_phase), data = long_data)

# Save ANOVA results to CSV
write.csv(as.data.frame(anova(anova_results)), "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\MEAL\\MEAL_trend_anova_results.csv")

# Step 4: Post-hoc tests with Tukey and Holm adjustments
# Tukey adjustment
posthoc_tukey_results <- emmeans(anova_results, pairwise ~ time_phase * Sex * Order, adjust = "Tukey")
posthoc_tukey_df <- as.data.frame(summary(posthoc_tukey_results$contrasts))

# Save Tukey-adjusted post-hoc results to CSV
write.csv(posthoc_tukey_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\MEAL\\MEAL_trend_posthoc_results_Tukey.csv")

# Holm adjustment
posthoc_holm_results <- emmeans(anova_results, pairwise ~ time_phase * Sex * Order, adjust = "holm")
posthoc_holm_df <- as.data.frame(summary(posthoc_holm_results$contrasts))

# Save Holm-adjusted post-hoc results to CSV
write.csv(posthoc_holm_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\MEAL\\MEAL_trend_posthoc_results_Holm.csv")


###############################################################################################################################################################################################
############################################################################ MEALS COMBINED #################################################################################################
###############################################################################################################################################################################################
library(tidyr)
library(dplyr)
library(afex)
library(emmeans)
library(ggplot2)

# Load the dataset
data <- read.csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\MEAL\\meals_per_day_trend.csv")

# Step 1: Reshape the data
long_data <- data %>%
  pivot_longer(cols = starts_with(c("NR", "PR")),
               names_to = "time_phase",
               values_to = "value")

# Step 2: Generate Descriptive Statistics
descriptive_stats <- long_data %>%
  group_by(time_phase, Order) %>%
  summarise(mean_value = mean(value, na.rm = TRUE),
            sd_value = sd(value, na.rm = TRUE),
            n = n())

# Save descriptive statistics to CSV
write.csv(descriptive_stats, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\MEAL\\COMBINE_MEAL_trend_descriptive_stats.csv")

# Step 3: Run ANOVA for each time point, sex, and order
anova_results <- aov_car(value ~ time_phase * Order + Error(Mouse/time_phase), data = long_data)

# Save ANOVA results to CSV
write.csv(as.data.frame(anova(anova_results)), "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\MEAL\\COMBINE_MEAL_trend_anova_results.csv")

# Step 4: Post-hoc tests with Tukey and Holm adjustments
# Tukey adjustment
posthoc_tukey_results <- emmeans(anova_results, pairwise ~ time_phase  * Order, adjust = "Tukey")
posthoc_tukey_df <- as.data.frame(summary(posthoc_tukey_results$contrasts))

# Save Tukey-adjusted post-hoc results to CSV
write.csv(posthoc_tukey_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\MEAL\\COMBINE_MEAL_trend_posthoc_results_Tukey.csv")

# Holm adjustment
posthoc_holm_results <- emmeans(anova_results, pairwise ~ time_phase * Order, adjust = "holm")
posthoc_holm_df <- as.data.frame(summary(posthoc_holm_results$contrasts))

# Save Holm-adjusted post-hoc results to CSV
write.csv(posthoc_holm_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\MEAL\\COMBINE_MEAL_trend_posthoc_results_Holm.csv")





################################################################################################################################################################################################################# 
######################## MEAL Frequency COMBINED #####################################################################################################
###################################################################################################################################################################

library(tidyr)
library(dplyr)
library(afex)
library(emmeans)
library(ggplot2)

# Load the dataset
data <- read.csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\MEAL\\meal_freq_per_day\\meal_freq_realigned_FINAL.csv")

# Step 1: Reshape the data
long_data <- data %>%
  pivot_longer(cols = starts_with(c("NR", "PR")),
               names_to = "time_phase",
               values_to = "value")

# Step 2: Generate Descriptive Statistics
descriptive_stats <- long_data %>%
  group_by(time_phase, Order) %>%
  summarise(mean_value = mean(value, na.rm = TRUE),
            sd_value = sd(value, na.rm = TRUE),
            n = n())

# Save descriptive statistics to CSV
write.csv(descriptive_stats, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\MEAL\\meal_freq_per_day\\COMBINE_MEALFreq_trend_descriptive_stats.csv")

# Step 3: Run ANOVA for each time point, sex, and order
anova_results <- aov_car(value ~ time_phase * Order + Error(Mouse/time_phase), data = long_data)

# Save ANOVA results to CSV
write.csv(as.data.frame(anova(anova_results)), "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\MEAL\\meal_freq_per_day\\COMBINE_MEALFreq_trend_anova_results.csv")

# Step 4: Post-hoc tests with Tukey and Holm adjustments
# Tukey adjustment
posthoc_tukey_results <- emmeans(anova_results, pairwise ~ time_phase  * Order, adjust = "Tukey")
posthoc_tukey_df <- as.data.frame(summary(posthoc_tukey_results$contrasts))

# Save Tukey-adjusted post-hoc results to CSV
write.csv(posthoc_tukey_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\MEAL\\meal_freq_per_day\\COMBINE_MEALFreq_trend_posthoc_results_Tukey.csv")

# Holm adjustment
posthoc_holm_results <- emmeans(anova_results, pairwise ~ time_phase * Order, adjust = "holm")
posthoc_holm_df <- as.data.frame(summary(posthoc_holm_results$contrasts))

# Save Holm-adjusted post-hoc results to CSV
write.csv(posthoc_holm_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\MEAL\\meal_freq_per_day\\COMBINE_MEALFreq_trend_posthoc_results_Holm.csv")





################################################################################################################################################################################################################# 
######################## MEAL size COMBINED #####################################################################################################
###################################################################################################################################################################

library(tidyr)
library(dplyr)
library(afex)
library(emmeans)
library(ggplot2)

# Load the dataset
data <- read.csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\MEAL\\meal_size_per_day\\meal_size_realigned_FINAL.csv")

# Step 1: Reshape the data
long_data <- data %>%
  pivot_longer(cols = starts_with(c("NR", "PR")),
               names_to = "time_phase",
               values_to = "value")

# Step 2: Generate Descriptive Statistics
descriptive_stats <- long_data %>%
  group_by(time_phase, Order) %>%
  summarise(mean_value = mean(value, na.rm = TRUE),
            sd_value = sd(value, na.rm = TRUE),
            n = n())

# Save descriptive statistics to CSV
write.csv(descriptive_stats, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\MEAL\\meal_size_per_day\\COMBINE_MEALsize_trend_descriptive_stats.csv")

# Step 3: Run ANOVA for each time point, sex, and order
anova_results <- aov_car(value ~ time_phase * Order + Error(Mouse/time_phase), data = long_data)

# Save ANOVA results to CSV
write.csv(as.data.frame(anova(anova_results)), "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\MEAL\\meal_size_per_day\\COMBINE_MEALsize_trend_anova_results.csv")

# Step 4: Post-hoc tests with Tukey and Holm adjustments
# Tukey adjustment
posthoc_tukey_results <- emmeans(anova_results, pairwise ~ time_phase  * Order, adjust = "Tukey")
posthoc_tukey_df <- as.data.frame(summary(posthoc_tukey_results$contrasts))

# Save Tukey-adjusted post-hoc results to CSV
write.csv(posthoc_tukey_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\MEAL\\meal_size_per_day\\COMBINE_MEALsize_trend_posthoc_results_Tukey.csv")

# Holm adjustment
posthoc_holm_results <- emmeans(anova_results, pairwise ~ time_phase * Order, adjust = "holm")
posthoc_holm_df <- as.data.frame(summary(posthoc_holm_results$contrasts))

# Save Holm-adjusted post-hoc results to CSV
write.csv(posthoc_holm_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\MEAL\\meal_size_per_day\\COMBINE_MEALsize_trend_posthoc_results_Holm.csv")






###############################################################################################################################################################################################
############################################################################ FEAST BELOW #################################################################################################
###############################################################################################################################################################################################

library(tidyr)
library(dplyr)
library(afex)
library(emmeans)
library(ggplot2)

# Load the dataset
data <- read.csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\FEAST\\mega_meals_per_day_trend.csv")

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
write.csv(descriptive_stats, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\FEAST\\FEAST_trend_descriptive_stats.csv")

# Step 3: Run ANOVA for each time point, sex, and order
anova_results <- aov_car(value ~ time_phase * Sex * Order + Error(Mouse/time_phase), data = long_data)

# Save ANOVA results to CSV
write.csv(as.data.frame(anova(anova_results)), "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\FEAST\\FEAST_trend_anova_results.csv")

# Step 4: Post-hoc tests with Tukey and Holm adjustments
# Tukey adjustment
posthoc_tukey_results <- emmeans(anova_results, pairwise ~ time_phase * Sex * Order, adjust = "Tukey")
posthoc_tukey_df <- as.data.frame(summary(posthoc_tukey_results$contrasts))

# Save Tukey-adjusted post-hoc results to CSV
write.csv(posthoc_tukey_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\FEAST\\FEAST_trend_posthoc_results_Tukey.csv")

# Holm adjustment
posthoc_holm_results <- emmeans(anova_results, pairwise ~ time_phase * Sex * Order, adjust = "holm")
posthoc_holm_df <- as.data.frame(summary(posthoc_holm_results$contrasts))

# Save Holm-adjusted post-hoc results to CSV
write.csv(posthoc_holm_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\FEAST\\FEAST_trend_posthoc_results_Holm.csv")


###############################################################################################################################################################################################
############################################################################ FEAST Frequency BELOW #################################################################################################
###############################################################################################################################################################################################

library(tidyr)
library(dplyr)
library(afex)
library(emmeans)
library(ggplot2)

# Load the dataset
data <- read.csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\FEAST\\feast_freq_per_day\\mega_meal_freq_realigned_FINAL.csv")

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
write.csv(descriptive_stats, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\FEAST\\feast_freq_per_day\\FEASTFreq_trend_descriptive_stats.csv")

# Step 3: Run ANOVA for each time point, sex, and order
anova_results <- aov_car(value ~ time_phase * Sex * Order + Error(Mouse/time_phase), data = long_data)

# Save ANOVA results to CSV
write.csv(as.data.frame(anova(anova_results)), "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\FEAST\\feast_freq_per_day\\FEASTFreq_trend_anova_results.csv")

# Step 4: Post-hoc tests with Tukey and Holm adjustments
# Tukey adjustment
posthoc_tukey_results <- emmeans(anova_results, pairwise ~ time_phase * Sex * Order, adjust = "Tukey")
posthoc_tukey_df <- as.data.frame(summary(posthoc_tukey_results$contrasts))

# Save Tukey-adjusted post-hoc results to CSV
write.csv(posthoc_tukey_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\FEAST\\feast_freq_per_day\\FEASTFreq_trend_posthoc_results_Tukey.csv")

# Holm adjustment
posthoc_holm_results <- emmeans(anova_results, pairwise ~ time_phase * Sex * Order, adjust = "holm")
posthoc_holm_df <- as.data.frame(summary(posthoc_holm_results$contrasts))

# Save Holm-adjusted post-hoc results to CSV
write.csv(posthoc_holm_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\FEAST\\feast_freq_per_day\\FEASTFreq_trend_posthoc_results_Holm.csv")

###############################################################################################################################################################################################
############################################################################ FEAST SIZE BELOW #################################################################################################
###############################################################################################################################################################################################

library(tidyr)
library(dplyr)
library(afex)
library(emmeans)
library(ggplot2)

# Load the dataset
data <- read.csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\FEAST\\feast_size_per_day\\mega_meal_size_realigned_FINAL.csv")

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
write.csv(descriptive_stats, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\FEAST\\feast_size_per_day\\FEASTSIZE_trend_descriptive_stats.csv")

# Step 3: Run ANOVA for each time point, sex, and order
anova_results <- aov_car(value ~ time_phase * Sex * Order + Error(Mouse/time_phase), data = long_data)

# Save ANOVA results to CSV
write.csv(as.data.frame(anova(anova_results)), "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\FEAST\\feast_size_per_day\\FEASTSIZE_trend_anova_results.csv")

# Step 4: Post-hoc tests with Tukey and Holm adjustments
# Tukey adjustment
posthoc_tukey_results <- emmeans(anova_results, pairwise ~ time_phase * Sex * Order, adjust = "Tukey")
posthoc_tukey_df <- as.data.frame(summary(posthoc_tukey_results$contrasts))

# Save Tukey-adjusted post-hoc results to CSV
write.csv(posthoc_tukey_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\FEAST\\feast_size_per_day\\FEASTSIZE_trend_posthoc_results_Tukey.csv")

# Holm adjustment
posthoc_holm_results <- emmeans(anova_results, pairwise ~ time_phase * Sex * Order, adjust = "holm")
posthoc_holm_df <- as.data.frame(summary(posthoc_holm_results$contrasts))

# Save Holm-adjusted post-hoc results to CSV
write.csv(posthoc_holm_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\FEAST\\feast_size_per_day\\FEASTSIZE_trend_posthoc_results_Holm.csv")

###############################################################################################################################################################################################
############################################################################ FEAST COMBINED #################################################################################################
####################################################################################HOWEVER ANOVA SHOWS Interaction between sex and Timephase###########################################################################################################

library(tidyr)
library(dplyr)
library(afex)
library(emmeans)
library(ggplot2)

# Load the dataset
data <- read.csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\FEAST\\mega_meals_per_day_trend.csv")

# Step 1: Reshape the data
long_data <- data %>%
  pivot_longer(cols = starts_with(c("NR", "PR")),
               names_to = "time_phase",
               values_to = "value")

# Step 2: Generate Descriptive Statistics
descriptive_stats <- long_data %>%
  group_by(time_phase, Order) %>%
  summarise(mean_value = mean(value, na.rm = TRUE),
            sd_value = sd(value, na.rm = TRUE),
            n = n())

# Save descriptive statistics to CSV
write.csv(descriptive_stats, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\FEAST\\COMBINE_FEAST_trend_descriptive_stats.csv")

# Step 3: Run ANOVA for each time point, sex, and order
anova_results <- aov_car(value ~ time_phase * Order + Error(Mouse/time_phase), data = long_data)

# Save ANOVA results to CSV
write.csv(as.data.frame(anova(anova_results)), "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\FEAST\\COMBINE_FEAST_trend_anova_results.csv")

# Step 4: Post-hoc tests with Tukey and Holm adjustments
# Tukey adjustment
posthoc_tukey_results <- emmeans(anova_results, pairwise ~ time_phase  * Order, adjust = "Tukey")
posthoc_tukey_df <- as.data.frame(summary(posthoc_tukey_results$contrasts))

# Save Tukey-adjusted post-hoc results to CSV
write.csv(posthoc_tukey_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\FEAST\\COMBINE_FEAST_trend_posthoc_results_Tukey.csv")

# Holm adjustment
posthoc_holm_results <- emmeans(anova_results, pairwise ~ time_phase * Order, adjust = "holm")
posthoc_holm_df <- as.data.frame(summary(posthoc_holm_results$contrasts))

# Save Holm-adjusted post-hoc results to CSV
write.csv(posthoc_holm_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\FEAST\\COMBINE_FEAST_trend_posthoc_results_Holm.csv")






###############################################################################################################################################################################################
############################################################################ INTERACTION TIME BELOW #################################################################################################
###############################################################################################################################################################################################


library(tidyr)
library(dplyr)
library(afex)
library(emmeans)
library(ggplot2)

# Load the dataset
data <- read.csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\INTERACTION_TIME\\INTER_TIME.csv")

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
write.csv(descriptive_stats, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\INTERACTION_TIME\\INTERTIME_trend_descriptive_stats.csv")

# Step 3: Run ANOVA for each time point, sex, and order
anova_results <- aov_car(value ~ time_phase * sex * order + Error(mouse_id/time_phase), data = long_data)

# Save ANOVA results to CSV
write.csv(as.data.frame(anova(anova_results)), "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\INTERACTION_TIME\\INTERTIME_trend_anova_results.csv")

# Step 4: Post-hoc tests with Tukey and Holm adjustments
# Tukey adjustment
posthoc_tukey_results <- emmeans(anova_results, pairwise ~ time_phase * sex * order, adjust = "Tukey")
posthoc_tukey_df <- as.data.frame(summary(posthoc_tukey_results$contrasts))

# Save Tukey-adjusted post-hoc results to CSV
write.csv(posthoc_tukey_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\INTERACTION_TIME\\INTERTIME_trend_posthoc_results_Tukey.csv")

# Holm adjustment
posthoc_holm_results <- emmeans(anova_results, pairwise ~ time_phase * sex * order, adjust = "holm")
posthoc_holm_df <- as.data.frame(summary(posthoc_holm_results$contrasts))

# Save Holm-adjusted post-hoc results to CSV
write.csv(posthoc_holm_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\INTERACTION_TIME\\INTERTIME_trend_posthoc_results_Holm.csv")



###############################################################################################################################################################################################
############################################################################ INTERACTION TIME COMBINED #################################################################################################    
#######################################################################################ALTHOUGH THERE ARE INTERACTIONS WITH SEX########################################################################################################



library(tidyr)
library(dplyr)
library(afex)
library(emmeans)
library(ggplot2)

# Load the dataset
data <- read.csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\INTERACTION_TIME\\INTER_TIME.csv")

# Step 1: Reshape the data
long_data <- data %>%
  pivot_longer(cols = starts_with(c("NR", "PR")),
               names_to = "time_phase",
               values_to = "value")

# Step 2: Generate Descriptive Statistics
descriptive_stats <- long_data %>%
  group_by(time_phase, order) %>%
  summarise(mean_value = mean(value, na.rm = TRUE),
            sd_value = sd(value, na.rm = TRUE),
            n = n())

# Save descriptive statistics to CSV
write.csv(descriptive_stats, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\INTERACTION_TIME\\COMBINE_INTERTIME_trend_descriptive_stats.csv")

# Step 3: Run ANOVA for each time point, sex, and order
anova_results <- aov_car(value ~ time_phase  * order + Error(mouse_id/time_phase), data = long_data)

# Save ANOVA results to CSV
write.csv(as.data.frame(anova(anova_results)), "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\INTERACTION_TIME\\COMBINE_INTERTIME_trend_anova_results.csv")

# Step 4: Post-hoc tests with Tukey and Holm adjustments
# Tukey adjustment
posthoc_tukey_results <- emmeans(anova_results, pairwise ~ time_phase * order, adjust = "Tukey")
posthoc_tukey_df <- as.data.frame(summary(posthoc_tukey_results$contrasts))

# Save Tukey-adjusted post-hoc results to CSV
write.csv(posthoc_tukey_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\INTERACTION_TIME\\COMBINE_INTERTIME_trend_posthoc_results_Tukey.csv")

# Holm adjustment
posthoc_holm_results <- emmeans(anova_results, pairwise ~ time_phase * order, adjust = "holm")
posthoc_holm_df <- as.data.frame(summary(posthoc_holm_results$contrasts))

# Save Holm-adjusted post-hoc results to CSV
write.csv(posthoc_holm_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\INTERACTION_TIME\\COMBINE_INTERTIME_trend_posthoc_results_Holm.csv")










###############################################################################################################################################################################################
############################################################################ BODYWEIGHT #################################################################################################
###############################################################################################################################################################################################



library(tidyr)
library(dplyr)
library(afex)
library(emmeans)
library(ggplot2)

# Load the dataset
data <- read.csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\Bodyweight\\BODYWEIGHT.csv")

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
write.csv(descriptive_stats, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\Bodyweight\\w_trend_descriptive_stats.csv")

# Step 3: Run ANOVA for each time point, sex, and order
anova_results <- aov_car(value ~ time_phase * Sex * Order + Error(Mouse/time_phase), data = long_data)

# Save ANOVA results to CSV
write.csv(as.data.frame(anova(anova_results)), "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\Bodyweight\\wtrend_anova_results.csv")

# Step 4: Post-hoc tests with Tukey adjustment
posthoc_tukey <- emmeans(anova_results, pairwise ~ time_phase * Sex * Order, adjust = "tukey")
posthoc_tukey_df <- as.data.frame(summary(posthoc_tukey$contrasts))
write.csv(posthoc_tukey_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\Bodyweight\\w_trend_posthoc_tukey_results.csv")

# Step 5: Post-hoc tests with Holm adjustment
posthoc_holm <- emmeans(anova_results, pairwise ~ time_phase * Sex * Order, adjust = "holm")
posthoc_holm_df <- as.data.frame(summary(posthoc_holm$contrasts))
write.csv(posthoc_holm_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\Bodyweight\\w_trend_posthoc_holm_results.csv")






###############################################################################################################################################################################################
############################################################################ HOARDING #################################################################################################
###############################################################################################################################################################################################



library(tidyr)
library(dplyr)
library(afex)
library(emmeans)
library(ggplot2)

# Load the dataset
data <- read.csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\Hoarding\\HOARDING.csv")

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
write.csv(descriptive_stats, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\Hoarding\\h_trend_descriptive_stats.csv")

# Step 3: Run ANOVA for each time point, sex, and order
anova_results <- aov_car(value ~ time_phase * Sex * Order + Error(Mouse/time_phase), data = long_data)

# Save ANOVA results to CSV
write.csv(as.data.frame(anova(anova_results)), "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\Hoarding\\h_trend_anova_results.csv")

# Step 4: Post-hoc tests with Tukey adjustment
posthoc_tukey <- emmeans(anova_results, pairwise ~ time_phase * Sex * Order, adjust = "tukey")
posthoc_tukey_df <- as.data.frame(summary(posthoc_tukey$contrasts))
write.csv(posthoc_tukey_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\Hoarding\\h_trend_posthoc_tukey_results.csv")

# Step 5: Post-hoc tests with Holm adjustment
posthoc_holm <- emmeans(anova_results, pairwise ~ time_phase * Sex * Order, adjust = "holm")
posthoc_holm_df <- as.data.frame(summary(posthoc_holm$contrasts))
write.csv(posthoc_holm_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\Hoarding\\h_trend_posthoc_holm_results.csv")



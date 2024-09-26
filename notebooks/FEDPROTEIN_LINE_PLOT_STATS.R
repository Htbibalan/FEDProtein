
################### code below conducting statistical analysis for the line plot data of pellet intake across days ###################

library(tidyr)
library(dplyr)
library(afex)
library(emmeans)
library(ggplot2)

# Load the dataset
data <- read.csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\Working_on_trend_data\\Pellets_trend.csv")

# Step 1: Reshape the data
long_data <- data %>%
  pivot_longer(cols = starts_with(c("G", "NR", "PR")),
               names_to = "time_phase",
               values_to = "value")

# Step 2: Generate Descriptive Statistics
descriptive_stats <- long_data %>%
  group_by(time_phase, Sex, Order) %>%
  summarise(mean_value = mean(value, na.rm = TRUE),
            sd_value = sd(value, na.rm = TRUE),
            n = n())

# Save descriptive statistics to CSV
write.csv(descriptive_stats, "C://Users//hta031//Github//FEDProtein//results//Working_on_trend_data//R_FEDPROTEIN_Pellets_trend_descriptive_stats.csv")

# Step 3: Run ANOVA for each time point, sex, and order
anova_results <- aov_car(value ~ time_phase * Sex * Order + Error(Mouse/time_phase), data = long_data)





# Save ANOVA results to CSV
write.csv(as.data.frame(anova(anova_results)), "C://Users//hta031//Github//FEDProtein//results//Working_on_trend_data//R_FEDPROTEIN_Pellets_trend_anova_results.csv")

# Step 4: Post-hoc tests to compare each time point with others (including interactions with Sex and Order)
posthoc_results <- emmeans(anova_results, pairwise ~ time_phase * Sex * Order, adjust= "Tukey")

# Extract the contrasts for time_phase comparisons
posthoc_contrasts <- summary(posthoc_results$contrasts)




# Convert post-hoc results to a data frame including information on time_phase, Sex, and Order
posthoc_df <- as.data.frame(posthoc_contrasts)

# Save post-hoc results to CSV (including time_phase, Sex, and Order)
write.csv(as.data.frame(posthoc_results$contrasts), "C://Users//hta031//Github//FEDProtein//results//Working_on_trend_data//R_FEDPROTEIN_Pellets_trend_posthoc_results.csv")





###################### code below for statistical analysis for snacks across days ######################


library(tidyr)
library(dplyr)
library(afex)
library(emmeans)
library(ggplot2)

# Load the dataset
data <- read.csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\Working_on_trend_data\\snacks_trend.csv")

# Step 1: Reshape the data
long_data <- data %>%
  pivot_longer(cols = starts_with(c("G", "NR", "PR")),
               names_to = "time_phase",
               values_to = "value")

# Step 2: Generate Descriptive Statistics
descriptive_stats <- long_data %>%
  group_by(time_phase, Sex, Order) %>%
  summarise(mean_value = mean(value, na.rm = TRUE),
            sd_value = sd(value, na.rm = TRUE),
            n = n())

# Save descriptive statistics to CSV
write.csv(descriptive_stats, "C://Users//hta031//Github//FEDProtein//results//Working_on_trend_data//R_FEDPROTEIN_snacks_trend_descriptive_stats.csv")

# Step 3: Run ANOVA for each time point, sex, and order
anova_results <- aov_car(value ~ time_phase * Sex * Order + Error(Mouse/time_phase), data = long_data)





# Save ANOVA results to CSV
write.csv(as.data.frame(anova(anova_results)), "C://Users//hta031//Github//FEDProtein//results//Working_on_trend_data//R_FEDPROTEIN_snacks_trend_anova_results.csv")

# Step 4: Post-hoc tests to compare each time point with others (including interactions with Sex and Order)
posthoc_results <- emmeans(anova_results, pairwise ~ time_phase * Sex * Order, adjust= "Tukey")

# Extract the contrasts for time_phase comparisons
posthoc_contrasts <- summary(posthoc_results$contrasts)




# Convert post-hoc results to a data frame including information on time_phase, Sex, and Order
posthoc_df <- as.data.frame(posthoc_contrasts)

# Save post-hoc results to CSV (including time_phase, Sex, and Order)
write.csv(as.data.frame(posthoc_results$contrasts), "C://Users//hta031//Github//FEDProtein//results//Working_on_trend_data//R_FEDPROTEIN_Pellets_trend_posthoc_results.csv")




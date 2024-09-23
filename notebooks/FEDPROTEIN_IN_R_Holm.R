library(readr)      # For reading CSV files
library(afex)       # For ANOVA and mixed models
library(emmeans)    # For post-hoc comparisons
library(ggplot2)    # Optional: for data visualization



# Load your data file (replace with the correct path to your CSV file)
data <- read_csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\FED_PROTEIN_FINAL_ULTIMATE_DATA_WITHOUT_IMI_IPI_LISTS.csv")

# View the first few rows of the data to confirm it was loaded correctly
head(data)


# Load necessary libraries
library(tidyr)
library(dplyr)

# Reshape the data to long format
long_data <- data %>%
  pivot_longer(cols = c(grain_meal_size, pr_meal_size, nr_meal_size),
               names_to = "diet_phase",
               values_to = "meal_size") %>%
  mutate(diet_phase = case_when(
    diet_phase == "grain_meal_size" ~ "grain",
    diet_phase == "pr_meal_size" ~ "pr",
    diet_phase == "nr_meal_size" ~ "nr"
  ))

# View the reshaped data
head(long_data)



# Run ANOVA on the reshaped data
result <- aov_car(meal_size ~ sex * order + Error(mouse_id/diet_phase), data = long_data)

# View the summary of the ANOVA results
summary(result)


# Perform post-hoc tests
posthoc <- emmeans(result, pairwise ~ diet_phase)

# View the post-hoc test results
summary(posthoc)


anova_summary <- summary(result)


# Extract the ANOVA table
anova_table <- anova(result)
# Define the file path for the ANOVA results
file_path_anova <- "C:/Users/hta031/Github/FEDProtein/results/R_anova_results.csv"

# Save the ANOVA table to the specified file path
write.csv(as.data.frame(anova_table), file_path_anova)





# Perform post-hoc tests
posthoc <- emmeans(result, pairwise ~ diet_phase)

# Extract the summary of post-hoc results
posthoc_summary <- summary(posthoc)


# Define the file path for post-hoc results
file_path_posthoc <- "C:/Users/hta031/Github/FEDProtein/results/R_posthoc_results.csv"

# Convert and save post-hoc results to CSV
write.csv(as.data.frame(posthoc_summary$contrasts), file_path_posthoc)


###########################################################################################################################################################################################
###########################################################################################################################################################################################
###########################################################################################################################################################################################
###########################################################################################################################################################################################
###########################################################################################################################################################################################
# Load necessary libraries
library(tidyr)
library(dplyr)
library(afex)
library(emmeans)
library(ggplot2)

# Reshape the data to include all meal, snack, and megameal components
long_data <- data %>%
  pivot_longer(cols = c(grain_meal_size, pr_meal_size, nr_meal_size,
                        grain_number_of_meals, pr_number_of_meals, nr_number_of_meals,
                        grain_meal_frequency, pr_meal_frequency, nr_meal_frequency,
                        grain_snack_size, pr_snack_size, nr_snack_size,
                        grain_number_of_snacks, pr_number_of_snacks, nr_number_of_snacks,
                        grain_snack_frequency, pr_snack_frequency, nr_snack_frequency,
                        grain_mega_meal_size, pr_mega_meal_size, nr_mega_meal_size,
                        grain_mega_meal_frequency, pr_mega_meal_frequency, nr_mega_meal_frequency),
               names_to = "meal_component",
               values_to = "component_value") %>%
  separate(meal_component, into = c("diet_phase", "component"), sep = "_", extra = "merge")

# View the reshaped data
head(long_data)


# Generate descriptive statistics for each component by diet phase, sex, and order
descriptive_stats <- long_data %>%
  group_by(component, diet_phase, sex, order) %>%
  summarise(mean_value = mean(component_value, na.rm = TRUE),
            sd_value = sd(component_value, na.rm = TRUE),
            n = n())

# Save the descriptive statistics to a CSV file
write.csv(descriptive_stats, "C:/Users/hta031/Github/FEDProtein/results/R_FEDPROTEIN_descriptive_stats.csv")









# Loop over each meal component and run ANOVA and post-hoc comparisons
meal_components <- unique(long_data$component)

for (component in meal_components) {
  component_data <- long_data %>% filter(component == !!component)
  
  # ANOVA
  result <- aov_car(component_value ~ sex * order + Error(mouse_id/diet_phase), data = component_data)
  write.csv(as.data.frame(anova(result)), paste0("C:/Users/hta031/Github/FEDProtein/results/R_FEDPROTEIN_anova_", component, ".csv"))
  
  # Post-hoc tests
  posthoc <- emmeans(result, pairwise ~ diet_phase * sex * order)
  write.csv(as.data.frame(posthoc$contrasts), paste0("C:/Users/hta031/Github/FEDProtein/results/R_FEDPROTEIN_posthoc_", component, ".csv"))
}




########################################################################################################################################



# Load necessary libraries
library(tidyr)
library(dplyr)
library(afex)
library(emmeans)
library(ggplot2)

# Load the dataset
data <- read.csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\resultallpellets_widePhase.csv")

# Step 1: Reshape the data to long format
long_data <- data %>%
  pivot_longer(cols = starts_with(c("G", "NR", "PR")),
               names_to = "time_phase",
               values_to = "value") %>%
  mutate(diet_phase = case_when(
    grepl("G", time_phase) ~ "Grain",
    grepl("NR", time_phase) ~ "NR",
    grepl("PR", time_phase) ~ "PR"
  ))

# Step 2: Generate Descriptive Statistics
descriptive_stats <- long_data %>%
  group_by(diet_phase, Sex, Order) %>%
  summarise(mean_value = mean(value, na.rm = TRUE),
            sd_value = sd(value, na.rm = TRUE),
            n = n())

# Save descriptive statistics to CSV
write.csv(descriptive_stats, "C://Users//hta031//Github//FEDProtein//results//R_FEDPROTEIN_ALL_DAYS_descriptive_stats.csv")

# Step 3: Run ANOVA
anova_results <- aov_car(value ~ Sex * Order * diet_phase + Error(Mouse/diet_phase), data = long_data)

# Save ANOVA results to CSV
write.csv(as.data.frame(anova(anova_results)), "C://Users//hta031//Github//FEDProtein//results//R_FEDPROTEIN_ALL_DAYS_anova_results.csv")

# Step 4: Post-hoc Tests
posthoc_results <- emmeans(anova_results, pairwise ~ diet_phase * Sex * Order)

# Save post-hoc results to CSV
write.csv(as.data.frame(posthoc_results$contrasts), "C://Users//hta031//Github//FEDProtein//results//R_FEDPROTEIN_ALL_DAYS_posthoc_results.csv")

# Step 5: Optional - Visualize the results using ggplot2
ggplot(long_data, aes(x = diet_phase, y = value, color = Sex)) +
  geom_boxplot() +
  labs(title = "Comparison of Values Across Diet Phases by Sex")





###########################################################################################################################################################################################
##########################the code below should measure day by day comparison of the data#################################################################################################

# Load necessary libraries
library(tidyr)
library(dplyr)
library(afex)
library(emmeans)
library(ggplot2)

# Load the dataset
data <- read.csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\resultallpellets_widePhase.csv")

# Step 1: Reshape the data, keeping each time point (G0, NR0, PR0, etc.) as its own group
long_data <- data %>%
  pivot_longer(cols = starts_with(c("G", "NR", "PR")),
               names_to = "time_phase",
               values_to = "value")

# Step 2: Generate Descriptive Statistics for each time point, sex, and order
descriptive_stats <- long_data %>%
  group_by(time_phase, Sex, Order) %>%
  summarise(mean_value = mean(value, na.rm = TRUE),
            sd_value = sd(value, na.rm = TRUE),
            n = n())

# Save descriptive statistics to CSV
write.csv(descriptive_stats, "C:/Users/hta031/Github/FEDProtein/results/descriptive_stats_per_time_point.csv")

# Step 3: Run ANOVA to compare time points (e.g., NR0 vs NR1 vs NR2)
anova_results <- aov_car(value ~ time_phase * Sex * Order + Error(Mouse/time_phase), data = long_data)

# Save ANOVA results to CSV
write.csv(as.data.frame(anova(anova_results)), "C:/Users/hta031/Github/FEDProtein/results/R_FEDPROTEIN_ALL_DAYS_DAY_BY_DAY_anova_results_per_time_point.csv")

# Step 4: Post-hoc Tests to compare each time point with others (e.g., NR0 vs NR1, NR0 vs NR2)
posthoc_results <- emmeans(anova_results, pairwise ~ time_phase)

# Save post-hoc results to CSV
write.csv(as.data.frame(posthoc_results$contrasts), "C:/Users/hta031/Github/FEDProtein/results/R_FEDPROTEIN_ALL_DAYS_DAY_BY_DAY_posthoc_results_per_time_point.csv")

# Step 5: Optional - Visualize the results using ggplot2
ggplot(long_data, aes(x = time_phase, y = value, color = Sex)) +
  geom_boxplot() +
  labs(title = "Comparison of Values Across Time Points by Sex")



###########################################################################################################################################################################################
########################## code below should do the day by day comparison of the data separated by order and sex#################################################################################################
# Load necessary libraries
library(tidyr)
library(dplyr)
library(afex)
library(emmeans)
library(ggplot2)

# Load the dataset
data <- read.csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\resultallpellets_widePhase.csv")

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
write.csv(descriptive_stats, "C:/Users/hta031/Github/FEDProtein/results/descriptive_stats_per_time_point.csv")

# Step 3: Run ANOVA for each time point, sex, and order
anova_results <- aov_car(value ~ time_phase * Sex * Order + Error(Mouse/time_phase), data = long_data)

# Save ANOVA results to CSV
write.csv(as.data.frame(anova(anova_results)), "C:/Users/hta031/Github/FEDProtein/results/R_FEDPROTEIN_ALLDAYS_SEX_ORDER_anova_results_per_time_point.csv")

# Step 4: Post-hoc tests to compare each time point with others (including interactions with Sex and Order)
posthoc_results <- emmeans(anova_results, pairwise ~ time_phase * Sex * Order)

# Extract the contrasts for time_phase comparisons
posthoc_contrasts <- summary(posthoc_results$contrasts)

# Convert post-hoc results to a data frame including information on time_phase, Sex, and Order
posthoc_df <- as.data.frame(posthoc_contrasts)

# Save post-hoc results to CSV (including time_phase, Sex, and Order)
write.csv(posthoc_df, "C:/Users/hta031/Github/FEDProtein/results/R_FEDPROTEIN_ALLDAYS_SEX_ORDER_posthoc_results_per_time_point_with_group_sex_order.csv")

# Step 5: Optional - Visualize the results using ggplot2
ggplot(long_data, aes(x = time_phase, y = value, color = Sex)) +
  geom_boxplot() +
  labs(title = "Comparison of Values Across Time Points by Sex and Order")




###########################################################################################################################################################################################
########################In case we deccide to use Holm test for post-hoc comparisons######################################################################################################
# Run post-hoc tests with Holm adjustment for multiple comparisons
posthoc_results <- emmeans(anova_results, pairwise ~ time_phase * Sex * Order, adjust = "holm")

# Extract the contrasts
posthoc_contrasts <- summary(posthoc_results$contrasts)

# Save the post-hoc results with Holm adjustment
write.csv(as.data.frame(posthoc_contrasts), "C:/Users/hta031/Github/FEDProtein/results/posthoc_results_with_Holm.csv")








###########################################################################################################################################################################################
########################Number of mega meals######################################################################################################
# Load necessary libraries
library(tidyr)
library(dplyr)
library(afex)
library(emmeans)
library(ggplot2)

# Load the dataset
data <- read_csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\FED_PROTEIN_FINAL_ULTIMATE_DATA_WITHOUT_IMI_IPI_LISTS.csv")

# Step 1: Reshape the data to include the number of mega meals
long_data <- data %>%
  pivot_longer(cols = c(grain_number_of_mega_meals, pr_number_of_mega_meals, nr_number_of_mega_meals),
               names_to = "time_phase",
               values_to = "mega_meal_count")

# Step 2: Generate Descriptive Statistics
descriptive_stats <- long_data %>%
  group_by(time_phase, sex, order) %>%
  summarise(mean_value = mean(mega_meal_count, na.rm = TRUE),
            sd_value = sd(mega_meal_count, na.rm = TRUE),
            n = n())

# Save descriptive statistics to CSV
write.csv(descriptive_stats, "C:/Users/hta031/Github/FEDProtein/results/R_FEDPROTEIN_descriptive_stats_mega_meals.csv")

# Step 3: Run ANOVA for the number of mega meals
anova_results <- aov_car(mega_meal_count ~ time_phase * sex * order + Error(mouse_id/time_phase), data = long_data)

# Save ANOVA results to CSV
write.csv(as.data.frame(anova(anova_results)), "C:/Users/hta031/Github/FEDProtein/results/R_FEDPROTEIN_anova_results_mega_meals.csv")

# Step 4: Post-hoc tests to compare mega meals across time points, sex, and order
posthoc_results <- emmeans(anova_results, pairwise ~ time_phase * sex * order)

# Save post-hoc results to CSV
write.csv(as.data.frame(posthoc_results$contrasts), "C:/Users/hta031/Github/FEDProtein/results/R_FEDPROTEIN_posthoc_results_mega_meals.csv")

# Optional - Visualize the results
ggplot(long_data, aes(x = time_phase, y = mega_meal_count, color = sex)) +
  geom_boxplot() +
  labs(title = "Comparison of Number of Mega Meals Across Time Points by Sex and Order")


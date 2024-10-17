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

# Perform post-hoc tests using Holm adjustment
posthoc <- emmeans(result, pairwise ~ diet_phase, adjust = "holm")

# View the post-hoc test results
summary(posthoc)

anova_summary <- summary(result)

# Extract the ANOVA table
anova_table <- anova(result)
# # Define the file path for the ANOVA results
file_path_anova <- "C:/Users/hta031/Github/FEDProtein/results/ULTIMATE_RESULTS_HOLM/R_anova_results.csv"

# Save the ANOVA table to the specified file path
write.csv(as.data.frame(anova_table), file_path_anova)

# Perform post-hoc tests using Holm adjustment
posthoc <- emmeans(result, pairwise ~ diet_phase, adjust = "holm")

# Extract the summary of post-hoc results
posthoc_summary <- summary(posthoc)

# # Define the file path for post-hoc results
file_path_posthoc <- "C:/Users/hta031/Github/FEDProtein/results/ULTIMATE_RESULTS_HOLM/R_posthoc_results.csv"

# Convert and save post-hoc results to CSV
write.csv(as.data.frame(posthoc_summary$contrasts), file_path_posthoc)

###########################################################################################################################################################################################
###########################################################################################################################################################################################
###########################################################################################################################################################################################
###########################################################################################################################################################################################
###########################################################################################################################################################################################
# Reshape the data to include all meal, snack, and megameal components


# Load your data file (replace with the correct path to your CSV file)
data <- read_csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\FED_PROTEIN_FINAL_ULTIMATE_DATA_WITHOUT_IMI_IPI_LISTS.csv")


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
write.csv(descriptive_stats, "C:/Users/hta031/Github/FEDProtein/results/ULTIMATE_RESULTS_HOLM/R_FEDPROTEIN_descriptive_stats.csv")

# Loop over each meal component and run ANOVA and post-hoc comparisons
meal_components <- unique(long_data$component)

for (component in meal_components) {
  component_data <- long_data %>% filter(component == !!component)
  
  # ANOVA
  result <- aov_car(component_value ~ sex * order + Error(mouse_id/diet_phase), data = component_data)
  write.csv(as.data.frame(anova(result)), paste0("C:/Users/hta031/Github/FEDProtein/results/ULTIMATE_RESULTS_HOLM/R_FEDPROTEIN_anova_", component, ".csv"))
  
  # Post-hoc tests using Holm adjustment
  posthoc <- emmeans(result, pairwise ~ diet_phase * sex * order, adjust = "holm")
  write.csv(as.data.frame(posthoc$contrasts), paste0("C:/Users/hta031/Github/FEDProtein/results/ULTIMATE_RESULTS_HOLM/R_FEDPROTEIN_posthoc_", component, ".csv"))
}


print(paste0("Saving ANOVA results to: ", paste0("C:/Users/hta031/Github/FEDProtein/results/ULTIMATE_RESULTS_HOLM/R_FEDPROTEIN_anova_", component, ".csv")))


########################################################################################################################################
########trying to fix the mega meal number measuemrent ########################
library(readr)
library(afex)
library(emmeans)
library(tidyr)
library(dplyr)

# Load your data file (replace with the correct path to your CSV file)
data <- read_csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\FED_PROTEIN_FINAL_ULTIMATE_DATA_WITHOUT_IMI_IPI_LISTS.csv")

# Reshape the data to long format, focusing on the columns you need
long_data <- data %>%
  pivot_longer(cols = c(grain_meal_size, pr_meal_size, nr_meal_size,
                        grain_number_of_meals, pr_number_of_meals, nr_number_of_meals,
                        grain_meal_frequency, pr_meal_frequency, nr_meal_frequency,
                        grain_snack_size, pr_snack_size, nr_snack_size,
                        grain_number_of_snacks, pr_number_of_snacks, nr_number_of_snacks,
                        grain_snack_frequency, pr_snack_frequency, nr_snack_frequency,
                        grain_mega_meal_size, pr_mega_meal_size, nr_mega_meal_size,
                        grain_mega_meal_frequency, pr_mega_meal_frequency, nr_mega_meal_frequency,
                        grain_number_of_mega_meals, pr_number_of_mega_meals, nr_number_of_mega_meals),
               names_to = "meal_component",
               values_to = "component_value") %>%
  separate(meal_component, into = c("diet_phase", "component"), sep = "_", extra = "merge")

# View the reshaped data to confirm the changes
head(long_data)

# Run ANOVA on the reshaped data
result <- aov_car(component_value ~ sex * order + Error(mouse_id/diet_phase), data = long_data)

# Perform post-hoc tests using Holm adjustment
posthoc <- emmeans(result, pairwise ~ diet_phase * sex * order, adjust = "holm")

# Save the ANOVA table to a CSV file
file_path_anova <- "C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_RESULTS_HOLM2\\R_anova_results.csv"
write.csv(as.data.frame(anova(result)), file_path_anova)

# Save post-hoc results to CSV
file_path_posthoc <- "C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_RESULTS_HOLM2\\R_posthoc_results.csv"
write.csv(as.data.frame(posthoc$contrasts), file_path_posthoc)

# Generate descriptive statistics for each component by diet phase, sex, and order
descriptive_stats <- long_data %>%
  group_by(component, diet_phase, sex, order) %>%
  summarise(mean_value = mean(component_value, na.rm = TRUE),
            sd_value = sd(component_value, na.rm = TRUE),
            n = n())

# Save the descriptive statistics to a CSV file
write.csv(descriptive_stats, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_RESULTS_HOLM2\\R_FEDPROTEIN_descriptive_stats.csv")

# Loop over each meal component and run ANOVA and post-hoc comparisons for each
meal_components <- unique(long_data$component)

for (component in meal_components) {
  tryCatch({
    component_data <- long_data %>% filter(component == !!component)
    
    # Run ANOVA
    result <- aov_car(component_value ~ sex * order + Error(mouse_id/diet_phase), data = component_data)
    
    # Save ANOVA results
    file_path_anova <- paste0("C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_RESULTS_HOLM2\\R_FEDPROTEIN_anova_", component, ".csv")
    write.csv(as.data.frame(anova(result)), file_path_anova)
    
    # Perform post-hoc tests using Holm adjustment
    posthoc <- emmeans(result, pairwise ~ diet_phase * sex * order, adjust = "holm")
    
    # Save post-hoc results
    file_path_posthoc <- paste0("C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_RESULTS_HOLM2\\R_FEDPROTEIN_posthoc_", component, ".csv")
    write.csv(as.data.frame(posthoc$contrasts), file_path_posthoc)
    
  }, error = function(e) {
    print(paste("Error processing component:", component))
    print(e)
  })
}







library(readr)
library(afex)
library(emmeans)
library(tidyr)
library(dplyr)

# Load your data file
data <- read_csv("C:/Users/hta031/Github/FEDProtein/results/FED_PROTEIN_FINAL_ULTIMATE_DATA_WITHOUT_IMI_IPI_LISTS.csv")

# Reshape the data to long format, focusing on the columns you need
long_data <- data %>%
  pivot_longer(cols = c(grain_meal_size, pr_meal_size, nr_meal_size,
                        grain_number_of_meals, pr_number_of_meals, nr_number_of_meals,
                        grain_meal_frequency, pr_meal_frequency, nr_meal_frequency,
                        grain_snack_size, pr_snack_size, nr_snack_size,
                        grain_number_of_snacks, pr_number_of_snacks, nr_number_of_snacks,
                        grain_snack_frequency, pr_snack_frequency, nr_snack_frequency,
                        grain_mega_meal_size, pr_mega_meal_size, nr_mega_meal_size,
                        grain_mega_meal_frequency, pr_mega_meal_frequency, nr_mega_meal_frequency,
                        grain_number_of_mega_meals, pr_number_of_mega_meals, nr_number_of_mega_meals),
               names_to = "meal_component",
               values_to = "component_value") %>%
  separate(meal_component, into = c("diet_phase", "component"), sep = "_", extra = "merge")

# View the reshaped data to confirm the changes
print(head(long_data))

# Run ANOVA on the reshaped data
result <- aov_car(component_value ~ sex * order + Error(mouse_id/diet_phase), data = long_data)

# Perform post-hoc tests using Holm adjustment
posthoc <- emmeans(result, pairwise ~ diet_phase * sex * order, adjust = "holm")

# Save the ANOVA table to a CSV file
file_path_anova <- "C:/Users/hta031/Github/HOLM3/R_anova_results.csv"
write.csv(as.data.frame(anova(result)), file_path_anova)
print(paste("ANOVA results saved to:", file_path_anova))

# Save post-hoc results to CSV
file_path_posthoc <- "C:/Users/hta031/Github/HOLM3/R_posthoc_results.csv"
write.csv(as.data.frame(posthoc$contrasts), file_path_posthoc)
print(paste("Post-hoc results saved to:", file_path_posthoc))

# Generate descriptive statistics for each component by diet phase, sex, and order
descriptive_stats <- long_data %>%
  group_by(component, diet_phase, sex, order) %>%
  summarise(mean_value = mean(component_value, na.rm = TRUE),
            sd_value = sd(component_value, na.rm = TRUE),
            n = n())

# Save the descriptive statistics to a CSV file
descriptive_stats_path <- "C:/Users/hta031/Github/HOLM3/R_FEDPROTEIN_descriptive_stats.csv"
write.csv(descriptive_stats, descriptive_stats_path)
print(paste("Descriptive stats saved to:", descriptive_stats_path))

# Loop over each meal component and run ANOVA and post-hoc comparisons for each
meal_components <- unique(long_data$component)

for (component in meal_components) {
  tryCatch({
    component_data <- long_data %>% filter(component == !!component)
    
    # Run ANOVA
    result <- aov_car(component_value ~ sex * order + Error(mouse_id/diet_phase), data = component_data)
    
    # Save ANOVA results
    file_path_anova <- paste0("C:/Users/hta031/Github/HOLM3/R_FEDPROTEIN_anova_", component, ".csv")
    write.csv(as.data.frame(anova(result)), file_path_anova)
    print(paste("ANOVA results for", component, "saved to:", file_path_anova))
    
    # Perform post-hoc tests using Holm adjustment
    posthoc <- emmeans(result, pairwise ~ diet_phase * sex * order, adjust = "holm")
    
    # Save post-hoc results
    file_path_posthoc <- paste0("C:/Users/hta031/Github/HOLM3/R_FEDPROTEIN_posthoc_", component, ".csv")
    write.csv(as.data.frame(posthoc$contrasts), file_path_posthoc)
    print(paste("Post-hoc results for", component, "saved to:", file_path_posthoc))
    
  }, error = function(e) {
    print(paste("Error processing component:", component))
    print(e)
  })
}



################################################################################################################################
################### Holm including 

library(readr)
library(afex)
library(emmeans)
library(tidyr)
library(dplyr)

# Load your data file
data <- read_csv("C:/Users/hta031/Github/FEDProtein/results/FED_PROTEIN_FINAL_ULTIMATE_DATA_WITHOUT_IMI_IPI_LISTS.csv")

# Reshape the data to long format, focusing on the columns you need
long_data <- data %>%
  pivot_longer(cols = c(grain_meal_size, pr_meal_size, nr_meal_size,
                        grain_number_of_meals, pr_number_of_meals, nr_number_of_meals,
                        grain_meal_frequency, pr_meal_frequency, nr_meal_frequency,
                        grain_snack_size, pr_snack_size, nr_snack_size,
                        grain_number_of_snacks, pr_number_of_snacks, nr_number_of_snacks,
                        grain_snack_frequency, pr_snack_frequency, nr_snack_frequency,
                        grain_mega_meal_size, pr_mega_meal_size, nr_mega_meal_size,
                        grain_mega_meal_frequency, pr_mega_meal_frequency, nr_mega_meal_frequency,
                        grain_number_of_mega_meals, pr_number_of_mega_meals, nr_number_of_mega_meals),
               names_to = "meal_component",
               values_to = "component_value") %>%
  separate(meal_component, into = c("diet_phase", "component"), sep = "_", extra = "merge")

# View the reshaped data to confirm the changes
print(head(long_data))

# Run ANOVA on the reshaped data
result <- aov_car(component_value ~ sex * order + Error(mouse_id/diet_phase), data = long_data)

# Perform post-hoc tests using Holm adjustment
posthoc <- emmeans(result, pairwise ~ diet_phase * sex * order, adjust = "holm")

# Save the ANOVA table to a CSV file
file_path_anova <- "C:/Users/hta031/Github/HOLM3/R_anova_results.csv"
write.csv(as.data.frame(anova(result)), file_path_anova)
print(paste("ANOVA results saved to:", file_path_anova))

# Save post-hoc results to CSV
file_path_posthoc <- "C:/Users/hta031/Github/HOLM3/R_posthoc_results.csv"
write.csv(as.data.frame(posthoc$contrasts), file_path_posthoc)
print(paste("Post-hoc results saved to:", file_path_posthoc))

# Generate descriptive statistics for each component by diet phase, sex, and order
descriptive_stats <- long_data %>%
  group_by(component, diet_phase, sex, order) %>%
  summarise(mean_value = mean(component_value, na.rm = TRUE),
            sd_value = sd(component_value, na.rm = TRUE),
            n = n())

# Save the descriptive statistics to a CSV file
descriptive_stats_path <- "C:/Users/hta031/Github/HOLM3/R_FEDPROTEIN_descriptive_stats.csv"
write.csv(descriptive_stats, descriptive_stats_path)
print(paste("Descriptive stats saved to:", descriptive_stats_path))

# Loop over each meal component and run ANOVA and post-hoc comparisons for each
meal_components <- unique(long_data$component)

for (component in meal_components) {
  tryCatch({
    component_data <- long_data %>% filter(component == !!component)
    
    # Run ANOVA
    result <- aov_car(component_value ~ sex * order + Error(mouse_id/diet_phase), data = component_data)
    
    # Save ANOVA results
    file_path_anova <- paste0("C:/Users/hta031/Github/HOLM3/R_FEDPROTEIN_anova_", component, ".csv")
    write.csv(as.data.frame(anova(result)), file_path_anova)
    print(paste("ANOVA results for", component, "saved to:", file_path_anova))
    
    # Perform post-hoc tests using Holm adjustment
    posthoc <- emmeans(result, pairwise ~ diet_phase * sex * order, adjust = "holm")
    
    # Save post-hoc results
    file_path_posthoc <- paste0("C:/Users/hta031/Github/HOLM3/R_FEDPROTEIN_posthoc_", component, ".csv")
    write.csv(as.data.frame(posthoc$contrasts), file_path_posthoc)
    print(paste("Post-hoc results for", component, "saved to:", file_path_posthoc))
    
  }, error = function(e) {
    print(paste("Error processing component:", component))
    print(e)
  })
}







#######################################################################################




# Load necessary libraries
library(tidyr)
library(dplyr)
library(afex)
library(emmeans)
library(ggplot2)

# Load the dataset
data <- read.csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\Working_on_trend_data\\Pellets_trend.csv")

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
write.csv(descriptive_stats, "C://Users//hta031//Github//FEDProtein//results//Working_on_trend_data//R_FEDPROTEIN_Pellets_trend_descriptive_stats.csv")

# Step 3: Run ANOVA
anova_results <- aov_car(value ~ Sex * Order * diet_phase + Error(Mouse/diet_phase), data = long_data)

# Save ANOVA results to CSV
write.csv(as.data.frame(anova(anova_results)), "C://Users//hta031//Github//FEDProtein//results//Working_on_trend_data//R_FEDPROTEIN_Pellets_trend_anova_results.csv")

# Step 4: Post-hoc Tests using Holm adjustment
posthoc_results <- emmeans(anova_results, pairwise ~ diet_phase * Sex * Order, adjust = "holm")

# Save post-hoc results to CSV
write.csv(as.data.frame(posthoc_results$contrasts), "C://Users//hta031//Github//FEDProtein//results//Working_on_trend_data//R_FEDPROTEIN_Pellets_trend_posthoc_results.csv")





##################################################################################################################################################################

##################################################################################################################################################################
#######################################################################MIXED_MODEL_PELLET_INTAKE_HOURLY###########################################################################################
##################################################################################################################################################################


# Load necessary libraries
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)

# Define the file path
file_path <- "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/HOURLY_PELLET/Averaged_Hourly_Pellet_Intake.csv"

# Load the dataset
data <- read.csv(file_path)

# Check the column names to confirm structure
print(colnames(data))

# Reshape the dataset for analysis
# Use PR and NR as the diet columns
data <- data %>%
  pivot_longer(cols = c("PR", "NR"), names_to = "diet", values_to = "pellet_intake") %>%
  mutate(order = as.factor(order),
         sex = as.factor(sex),
         diet = factor(diet, levels = c("NR", "PR")), # Ensure correct order
         hour = as.factor(hour),                     # Treat hour as a factor
         mouse_id = as.factor(mouse_id))             # Ensure mouse_id is a factor

# Fit the mixed-effects model
model <- lmer(pellet_intake ~ diet * sex * order * hour + (1 | mouse_id), data = data)

# Perform Type III ANOVA to extract F-values and p-values
anova_results <- anova(model)

# Print the ANOVA results
print(anova_results)

# Save the ANOVA results to a CSV file
write.csv(anova_results, "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/HOURLY_PELLET/MIXED_MODEL/Mixed_Model_ANOVA_Results.csv", row.names = TRUE)

# Post hoc pairwise comparisons for significant effects
posthoc_results <- emmeans(model, pairwise ~ diet * sex * order | hour, adjust = "holm")

# Save the post hoc comparisons to a CSV file
posthoc_comparisons <- as.data.frame(posthoc_results$contrasts)
write.csv(posthoc_comparisons, "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/HOURLY_PELLET/MIXED_MODEL/Posthoc_Hourly_Comparisons.csv", row.names = FALSE)

# Marginal means to assess trends
marginal_means <- emmeans(model, ~ diet | hour)

# Save the marginal means to a CSV file
marginal_means_summary <- as.data.frame(marginal_means)
write.csv(marginal_means_summary, "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/HOURLY_PELLET/MIXED_MODEL/Marginal_Means_Hourly.csv", row.names = FALSE)

# Completion message
cat("Analysis complete. Results saved in:", file_path, "\n")





##################################################################################################################################################################
####################################################################### UPDATED_MIXED_MODEL_PELLET_INTAKE_HOURLY #########################################################
##################################################################################################################################################################

# Load necessary libraries
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)

# Define the file path
file_path <- "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/HOURLY_PELLET/Averaged_Hourly_Pellet_Intake.csv"

# Load the dataset
data <- read.csv(file_path)

# Check the column names to confirm structure
print(colnames(data))

# Reshape the dataset for analysis
# Use PR and NR as the diet columns
data <- data %>%
  pivot_longer(cols = c("PR", "NR"), names_to = "diet", values_to = "pellet_intake") %>%
  mutate(order = as.factor(order),
         sex = as.factor(sex),
         diet = factor(diet, levels = c("NR", "PR")), # Ensure correct order
         hour = as.factor(hour),                     # Treat hour as a factor
         mouse_id = as.factor(mouse_id))             # Ensure mouse_id is a factor

# Fit the mixed-effects model
model <- lmer(pellet_intake ~ diet * sex * order * hour + (1 | mouse_id), data = data)

# Perform Type III ANOVA to extract F-values and p-values
anova_results <- anova(model)

# Print the ANOVA results
print(anova_results)

# Save the ANOVA results to a CSV file
write.csv(anova_results, "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/HOURLY_PELLET/MIXED_MODEL/Mixed_Model_ANOVA_Results.csv", row.names = TRUE)

# Post hoc pairwise comparisons for significant effects
posthoc_results <- emmeans(model, pairwise ~ diet * sex * order | hour, adjust = "holm")

# Save the post hoc comparisons to a CSV file
posthoc_comparisons <- as.data.frame(posthoc_results$contrasts)
write.csv(posthoc_comparisons, "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/HOURLY_PELLET/MIXED_MODEL/Posthoc_Hourly_Comparisons.csv", row.names = FALSE)

# Marginal means for diet by sex, order, and hour
marginal_means <- emmeans(model, ~ diet | sex * order * hour)

# Save the marginal means to a CSV file
marginal_means_summary <- as.data.frame(marginal_means)
write.csv(marginal_means_summary, "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/HOURLY_PELLET/MIXED_MODEL/Marginal_Means_Hourly_Grouped.csv", row.names = FALSE)

# Completion message
cat("Analysis complete. Results saved in: C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/HOURLY_PELLET/MIXED_MODEL/\n")



################################################################################################################
##################################### FULL MIXED MODEL ANALYSIS FOR HOURLY PELLET INTAKE #######################
################################################################################################################

# Load necessary libraries
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)

# Define file paths
file_path <- "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/HOURLY_PELLET/Averaged_Hourly_Pellet_Intake.csv"
output_path <- "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/HOURLY_PELLET/MIXED_MODEL"

# Load the dataset
data <- read.csv(file_path)

# Reshape the dataset to long format
data <- data %>%
  pivot_longer(cols = c("PR", "NR"), names_to = "diet", values_to = "pellet_intake") %>%
  mutate(
    order = as.factor(order),
    sex = as.factor(sex),
    diet = factor(diet, levels = c("NR", "PR")), # Ensure NR and PR are ordered correctly
    hour = as.factor(hour),                     # Treat hour as a categorical variable
    mouse_id = as.factor(mouse_id)              # Ensure mouse_id is treated as a factor
  )

# Fit the full mixed-effects model
model <- lmer(pellet_intake ~ diet * sex * order * hour + (1 | mouse_id), data = data)

# Perform Type III ANOVA
anova_results <- anova(model)

# Save ANOVA results to a CSV file
write.csv(anova_results, file.path(output_path, "Mixed_Model_ANOVA_Results.csv"), row.names = TRUE)

# Perform post hoc pairwise comparisons
# Including diet, sex, order, and hour to explore all significant interactions
posthoc_results <- emmeans(model, pairwise ~ diet * sex * order | hour, adjust = "holm")

# Save post hoc comparisons to a CSV file
posthoc_comparisons <- as.data.frame(posthoc_results$contrasts)
write.csv(posthoc_comparisons, file.path(output_path, "Posthoc_Hourly_Comparisons.csv"), row.names = FALSE)

# Calculate marginal means for diet and hour
# Include sex and order only if their interactions are significant
if (any(anova_results$`Pr(>F)`[grepl("sex|order", rownames(anova_results))] < 0.05)) {
  marginal_means <- emmeans(model, ~ diet | sex * order * hour)
  marginal_means_path <- file.path(output_path, "Marginal_Means_Hourly_Grouped.csv")
} else {
  marginal_means <- emmeans(model, ~ diet | hour)
  marginal_means_path <- file.path(output_path, "Marginal_Means_Hourly.csv")
}

# Save marginal means to a CSV file
marginal_means_summary <- as.data.frame(marginal_means)
write.csv(marginal_means_summary, marginal_means_path, row.names = FALSE)

# Completion message
cat("Full mixed model analysis complete. Results saved in:", output_path, "\n")






##################################################################################################################################################################
#######################################################################MIXED_MODEL_MEAL_HOURLY###########################################################################################
##################################################################################################################################################################


# Load necessary libraries
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)

# Define the file path
file_path <- "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/MEALS/Meals_Hourly_Separated.csv"

# Load the dataset
data <- read.csv(file_path)

# Check the column names to confirm structure
print(colnames(data))

# Reshape the dataset for analysis
# Use PR and NR as the diet columns
data <- data %>%
  pivot_longer(cols = c("PR", "NR"), names_to = "diet", values_to = "pellet_intake") %>%
  mutate(order = as.factor(order),
         sex = as.factor(sex),
         diet = factor(diet, levels = c("NR", "PR")), # Ensure correct order
         hour = as.factor(hour),                     # Treat hour as a factor
         mouse_id = as.factor(mouse_id))             # Ensure mouse_id is a factor

# Fit the mixed-effects model
model <- lmer(pellet_intake ~ diet * sex * order * hour + (1 | mouse_id), data = data)

# Perform Type III ANOVA to extract F-values and p-values
anova_results <- anova(model)

# Print the ANOVA results
print(anova_results)

# Save the ANOVA results to a CSV file
write.csv(anova_results, "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/MEALS/MIXED_MODEL/Mixed_Model_ANOVA_Results.csv", row.names = TRUE)

# Post hoc pairwise comparisons for significant effects
posthoc_results <- emmeans(model, pairwise ~ diet * sex * order | hour, adjust = "holm")

# Save the post hoc comparisons to a CSV file
posthoc_comparisons <- as.data.frame(posthoc_results$contrasts)
write.csv(posthoc_comparisons, "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/MEALS/MIXED_MODEL/Posthoc_Hourly_Comparisons.csv", row.names = FALSE)

# Marginal means to assess trends
marginal_means <- emmeans(model, ~ diet | hour)

# Save the marginal means to a CSV file
marginal_means_summary <- as.data.frame(marginal_means)
write.csv(marginal_means_summary, "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/MEALS/MIXED_MODEL/Marginal_Means_Hourly.csv", row.names = FALSE)

# Completion message
cat("Analysis complete. Results saved in:", file_path, "\n")






##################################################################################################################################################################
####################################################################### UPDATED_MIXED_MODEL_MEAL_HOURLY ##################################################################
##################################################################################################################################################################

# Load necessary libraries
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)

# Define the file path
file_path <- "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/MEALS/Meals_Hourly_Separated.csv"

# Load the dataset
data <- read.csv(file_path)

# Check the column names to confirm structure
print(colnames(data))

# Reshape the dataset for analysis
# Use PR and NR as the diet columns
data <- data %>%
  pivot_longer(cols = c("PR", "NR"), names_to = "diet", values_to = "meal_intake") %>% # Changed value column to "meal_intake"
  mutate(order = as.factor(order),
         sex = as.factor(sex),
         diet = factor(diet, levels = c("NR", "PR")), # Ensure correct order
         hour = as.factor(hour),                     # Treat hour as a factor
         mouse_id = as.factor(mouse_id))             # Ensure mouse_id is a factor

# Fit the mixed-effects model
model <- lmer(meal_intake ~ diet * sex * order * hour + (1 | mouse_id), data = data)

# Perform Type III ANOVA to extract F-values and p-values
anova_results <- anova(model)

# Print the ANOVA results
print(anova_results)

# Save the ANOVA results to a CSV file
write.csv(anova_results, "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/MEALS/MIXED_MODEL/Mixed_Model_ANOVA_Results.csv", row.names = TRUE)

# Post hoc pairwise comparisons for significant effects
posthoc_results <- emmeans(model, pairwise ~ diet * sex * order | hour, adjust = "holm")

# Save the post hoc comparisons to a CSV file
posthoc_comparisons <- as.data.frame(posthoc_results$contrasts)
write.csv(posthoc_comparisons, "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/MEALS/MIXED_MODEL/Posthoc_Hourly_Comparisons.csv", row.names = FALSE)

# Marginal means for diet by sex, order, and hour
marginal_means <- emmeans(model, ~ diet | sex * order * hour)

# Save the marginal means to a CSV file
marginal_means_summary <- as.data.frame(marginal_means)
write.csv(marginal_means_summary, "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/MEALS/MIXED_MODEL/Marginal_Means_Hourly_Grouped.csv", row.names = FALSE)

# Completion message
cat("Analysis complete. Results saved in: C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/MEALS/MIXED_MODEL/\n")








##################################################################################################################################################################
#######################################################################MIXED_MODEL_SNACKS_HOURLY###########################################################################################
##################################################################################################################################################################


# Load necessary libraries
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)

# Define the file path
file_path <- "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/SNACKS/Snacks_Hourly_Separated.csv"

# Load the dataset
data <- read.csv(file_path)

# Check the column names to confirm structure
print(colnames(data))

# Reshape the dataset for analysis
# Use PR and NR as the diet columns
data <- data %>%
  pivot_longer(cols = c("PR", "NR"), names_to = "diet", values_to = "pellet_intake") %>%
  mutate(order = as.factor(order),
         sex = as.factor(sex),
         diet = factor(diet, levels = c("NR", "PR")), # Ensure correct order
         hour = as.factor(hour),                     # Treat hour as a factor
         mouse_id = as.factor(mouse_id))             # Ensure mouse_id is a factor

# Fit the mixed-effects model
model <- lmer(pellet_intake ~ diet * sex * order * hour + (1 | mouse_id), data = data)

# Perform Type III ANOVA to extract F-values and p-values
anova_results <- anova(model)

# Print the ANOVA results
print(anova_results)

# Save the ANOVA results to a CSV file
write.csv(anova_results, "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/SNACKS/MIXED_MODEL/Mixed_Model_ANOVA_Results.csv", row.names = TRUE)

# Post hoc pairwise comparisons for significant effects
posthoc_results <- emmeans(model, pairwise ~ diet * sex * order | hour, adjust = "holm")

# Save the post hoc comparisons to a CSV file
posthoc_comparisons <- as.data.frame(posthoc_results$contrasts)
write.csv(posthoc_comparisons, "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/SNACKS/MIXED_MODEL/Posthoc_Hourly_Comparisons.csv", row.names = FALSE)

# Marginal means to assess trends
marginal_means <- emmeans(model, ~ diet | hour)

# Save the marginal means to a CSV file
marginal_means_summary <- as.data.frame(marginal_means)
write.csv(marginal_means_summary, "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/SNACKS/MIXED_MODEL/Marginal_Means_Hourly.csv", row.names = FALSE)

# Completion message
cat("Analysis complete. Results saved in:", file_path, "\n")








##########################################################################################################################
########################################### UPDATED MIXED MODEL ANALYSIS SNACKS #################################################
##########################################################################################################################

# Load necessary libraries
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)

# Define the file path
file_path <- "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/SNACKS/Snacks_Hourly_Separated.csv"

# Load the dataset
data <- read.csv(file_path)

# Check the column names to confirm structure
print(colnames(data))

# Reshape the dataset for analysis
# Use PR and NR as the diet columns
data <- data %>%
  pivot_longer(cols = c("PR", "NR"), names_to = "diet", values_to = "pellet_intake") %>%
  mutate(order = as.factor(order),
         sex = as.factor(sex),
         diet = factor(diet, levels = c("NR", "PR")), # Ensure correct order
         hour = as.factor(hour),                     # Treat hour as a factor
         mouse_id = as.factor(mouse_id))             # Ensure mouse_id is a factor

# Fit the mixed-effects model
model <- lmer(pellet_intake ~ diet * sex * order * hour + (1 | mouse_id), data = data)

# Perform Type III ANOVA to extract F-values and p-values
anova_results <- anova(model)

# Print the ANOVA results
print(anova_results)

# Save the ANOVA results to a CSV file
write.csv(anova_results, "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/SNACKS/MIXED_MODEL/Mixed_Model_ANOVA_Results.csv", row.names = TRUE)

# Post hoc pairwise comparisons for significant effects
posthoc_results <- emmeans(model, pairwise ~ diet * sex * order | hour, adjust = "holm")

# Save the post hoc comparisons to a CSV file
posthoc_comparisons <- as.data.frame(posthoc_results$contrasts)
write.csv(posthoc_comparisons, "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/SNACKS/MIXED_MODEL/Posthoc_Hourly_Comparisons.csv", row.names = FALSE)

# Marginal means for diet by sex, order, and hour
marginal_means <- emmeans(model, ~ diet | sex * order * hour)

# Save the marginal means to a CSV file
marginal_means_summary <- as.data.frame(marginal_means)
write.csv(marginal_means_summary, "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/SNACKS/MIXED_MODEL/Marginal_Means_Hourly_Grouped.csv", row.names = FALSE)

# Completion message
cat("Analysis complete. Results saved in: C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/SNACKS/MIXED_MODEL/\n")



#################################################################################################################################################################
#######################################################################MIXED_MODEL_MEGAMEAL_HOURLY###########################################################################################
##################################################################################################################################################################


# Load necessary libraries
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)

# Define the file path
file_path <- "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/MEGA_MEALS/Mega_meals_Hourly_Separated.csv"

# Load the dataset
data <- read.csv(file_path)

# Check the column names to confirm structure
print(colnames(data))

# Reshape the dataset for analysis
# Use PR and NR as the diet columns
data <- data %>%
  pivot_longer(cols = c("PR", "NR"), names_to = "diet", values_to = "pellet_intake") %>%
  mutate(order = as.factor(order),
         sex = as.factor(sex),
         diet = factor(diet, levels = c("NR", "PR")), # Ensure correct order
         hour = as.factor(hour),                     # Treat hour as a factor
         mouse_id = as.factor(mouse_id))             # Ensure mouse_id is a factor

# Fit the mixed-effects model
model <- lmer(pellet_intake ~ diet * sex * order * hour + (1 | mouse_id), data = data)

# Perform Type III ANOVA to extract F-values and p-values
anova_results <- anova(model)

# Print the ANOVA results
print(anova_results)

# Save the ANOVA results to a CSV file
write.csv(anova_results, "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/MEGA_MEALS/MIXED_MODEL/Mixed_Model_ANOVA_Results.csv", row.names = TRUE)

# Post hoc pairwise comparisons for significant effects
posthoc_results <- emmeans(model, pairwise ~ diet * sex * order | hour, adjust = "holm")

# Save the post hoc comparisons to a CSV file
posthoc_comparisons <- as.data.frame(posthoc_results$contrasts)
write.csv(posthoc_comparisons, "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/MEGA_MEALS/MIXED_MODEL/Posthoc_Hourly_Comparisons.csv", row.names = FALSE)

# Marginal means to assess trends
marginal_means <- emmeans(model, ~ diet | hour)

# Save the marginal means to a CSV file
marginal_means_summary <- as.data.frame(marginal_means)
write.csv(marginal_means_summary, "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/MEGA_MEALS/MIXED_MODEL/Marginal_Means_Hourly.csv", row.names = FALSE)
MEGA_MEALS
# Completion message
cat("Analysis complete. Results saved in:", file_path, "\n")




#################################################################################################################################################################
####################################################################### UPDATE_MIXED_MODEL_MEGAMEAL_HOURLY #############################################################
#################################################################################################################################################################

# Load necessary libraries
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)

# Define the file path
file_path <- "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/MEGA_MEALS/Mega_meals_Hourly_Separated.csv"

# Load the dataset
data <- read.csv(file_path)

# Check the column names to confirm structure
print(colnames(data))

# Reshape the dataset for analysis
# Use PR and NR as the diet columns
data <- data %>%
  pivot_longer(cols = c("PR", "NR"), names_to = "diet", values_to = "mega_meal_intake") %>% # Changed value column to "mega_meal_intake"
  mutate(order = as.factor(order),
         sex = as.factor(sex),
         diet = factor(diet, levels = c("NR", "PR")), # Ensure correct order
         hour = as.factor(hour),                     # Treat hour as a factor
         mouse_id = as.factor(mouse_id))             # Ensure mouse_id is a factor

# Fit the mixed-effects model
model <- lmer(mega_meal_intake ~ diet * sex * order * hour + (1 | mouse_id), data = data)

# Perform Type III ANOVA to extract F-values and p-values
anova_results <- anova(model)

# Print the ANOVA results
print(anova_results)

# Save the ANOVA results to a CSV file
write.csv(anova_results, "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/MEGA_MEALS/MIXED_MODEL/Mixed_Model_ANOVA_Results.csv", row.names = TRUE)

# Post hoc pairwise comparisons for significant effects
posthoc_results <- emmeans(model, pairwise ~ diet * sex * order | hour, adjust = "holm")

# Save the post hoc comparisons to a CSV file
posthoc_comparisons <- as.data.frame(posthoc_results$contrasts)
write.csv(posthoc_comparisons, "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/MEGA_MEALS/MIXED_MODEL/Posthoc_Hourly_Comparisons.csv", row.names = FALSE)

# Marginal means for diet by sex, order, and hour
marginal_means <- emmeans(model, ~ diet | sex * order * hour)

# Save the marginal means to a CSV file
marginal_means_summary <- as.data.frame(marginal_means)
write.csv(marginal_means_summary, "C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/MEGA_MEALS/MIXED_MODEL/Marginal_Means_Hourly_Grouped.csv", row.names = FALSE)

# Completion message
cat("Analysis complete. Results saved in: C:/Users/hta031/Github/FEDProtein/results/FIVE/HEATMAPS/MEGA_MEALS/MIXED_MODEL/\n")




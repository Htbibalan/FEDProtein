


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
#######################################################################MIXED_MODEL_PELLET_MEAL_HOURLY###########################################################################################
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




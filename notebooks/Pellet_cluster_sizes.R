##########################################################################
##        COMBINED MALE AND FEMALE STATS (Repeated-Measures ANOVA)      ##
##########################################################################

############################## ANALYSIS ###################################

######################################################
# Load necessary libraries (install if not present)
######################################################
packages <- c("readr", "tidyr", "dplyr", "ggplot2", 
              "agricolae", "emmeans")  # Added emmeans for post-hoc

installed_packages <- rownames(installed.packages())
for (pkg in packages) {
  if (!(pkg %in% installed_packages)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

######################################################
# Define the file path
######################################################
data_file <- "C:/Users/hta031/Github/FEDProtein/results/MEAL_DEF_PELLET_CLUSTER_STATS/combined_cluster_sizes.csv"

######################################################
# Read the CSV file
######################################################
data <- read_csv(data_file)

# View the data (wide format: one row per MouseID)
print(data)

######################################################
# Reshape data to long format
#  - pivot_longer() creates EventSize factor for cluster sizes (1..10)
#  - Count is the numeric response
######################################################
data_long <- data %>% 
  pivot_longer(
    cols = starts_with("Count_"), 
    names_to = "EventSize", 
    values_to = "Count"
  )

# Convert MouseID to factor (needed for repeated-measures)
data_long$MouseID <- as.factor(data_long$MouseID)

# Convert 'Count_#' to just numeric cluster size or factor
#   For repeated-measures ANOVA, we typically treat EventSize as a factor
data_long$EventSize <- gsub("Count_", "", data_long$EventSize)
data_long$EventSize <- as.factor(data_long$EventSize)

# View the reshaped data (long format)
print(head(data_long))

######################################################
# Perform Repeated-Measures ANOVA
#   Formula: Count ~ EventSize + Error(MouseID / EventSize)
#   This accounts for within-subject variation across EventSize
######################################################
anova_result <- aov(Count ~ EventSize + Error(MouseID / EventSize), data = data_long)

# View ANOVA summary
anova_summary <- summary(anova_result)
print(anova_summary)

######################################################
# Save ANOVA results to a text file
######################################################
anova_output_file <- "C:/Users/hta031/Github/FEDProtein/results/MEAL_DEF_PELLET_CLUSTER_STATS/PELLET_rm_anova_results.txt"
capture.output(print(anova_summary), file = anova_output_file)
cat("Repeated-measures ANOVA results saved to", anova_output_file, "\n")

######################################################
# Post-hoc Pairwise Comparisons via emmeans
#   - We obtain estimated marginal means for each EventSize
#   - Then compare them pairwise with a Holm correction
######################################################
emm <- emmeans(anova_result, ~ EventSize)
posthoc_contrasts <- contrast(emm, method = "pairwise", adjust = "holm")

# Print post-hoc contrast results
print(posthoc_contrasts)

# Save post-hoc results to a text file
posthoc_output_file <- "C:/Users/hta031/Github/FEDProtein/results/MEAL_DEF_PELLET_CLUSTER_STATS/PELLET_posthoc_emmeans.txt"
capture.output(posthoc_contrasts, file = posthoc_output_file)
cat("Post-hoc pairwise comparisons saved to", posthoc_output_file, "\n")

######################################################
# Descriptive Statistics
#   Summary by cluster size across all mice
######################################################
descriptive_stats <- data_long %>% 
  group_by(EventSize) %>% 
  summarise(
    Mean   = mean(Count),
    SD     = sd(Count),
    Median = median(Count),
    Min    = min(Count),
    Max    = max(Count),
    n      = n()
  )

# View descriptive statistics
print(descriptive_stats)

######################################################
# Save Descriptive Statistics to a CSV file
######################################################
descriptive_stats_file <- "C:/Users/hta031/Github/FEDProtein/results/MEAL_DEF_PELLET_CLUSTER_STATS/PELLET_descriptive_statistics.csv"
write.csv(descriptive_stats, descriptive_stats_file, row.names = FALSE)
cat("Descriptive statistics saved to", descriptive_stats_file, "\n")

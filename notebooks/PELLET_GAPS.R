


#########################################################

#########################################################
#########################################################
#########################################################
#########################################################
#########################################################
#########################################################



# Load necessary libraries
library(dplyr)
library(ggplot2)
library(car)
library(multcomp)


# Load the dataset
data_file <- "C:/Users/hta031/Github/FEDProtein/results/MEAL_DEF_PELLET_GAP_TIME/feeding_time_gaps_categorized.csv"
data <- read.csv(data_file)

# Check structure of data
str(data)

# Filter the data for PR and NR phases
anova_data <- data %>% filter(Diet_Phase %in% c("PR", "NR"))

# Create a directory to save results
output_dir <- "C:/Users/hta031/Github/FEDProtein/results/MEAL_DEF_PELLET_GAP_TIME/"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Descriptive statistics
grouped_stats <- anova_data %>%
  group_by(Order, Sex, Diet_Phase, Current_Size, Next_Size) %>%
  summarise(Mean_Time_Gap = mean(`Time_Gap`, na.rm = TRUE),
            SD_Time_Gap = sd(`Time_Gap`, na.rm = TRUE),
            Count = n())

# Save descriptive statistics to CSV
write.csv(grouped_stats, file = paste0(output_dir, "grouped_descriptive_stats.csv"))

# ANOVA
anova_model <- aov(`Time_Gap` ~ Order * Sex * Diet_Phase * Current_Size, data = anova_data)
anova_summary <- summary(anova_model)

# Save ANOVA results to text file
sink(file = paste0(output_dir, "anova_results.txt"))
print(anova_summary)
sink()

# Post-hoc Holm test
pairwise_holm <- glht(anova_model, linfct = mcp(Current_Size = "Tukey"))
pairwise_holm_summary <- summary(pairwise_holm)

# Save post-hoc Holm test results to text file
sink(file = paste0(output_dir, "posthoc_holm_results.txt"))
print(pairwise_holm_summary)
sink()




##############################################################################
#############################################################################
########################## FIXING POSTHOC##################################



# Load necessary libraries
library(dplyr)
library(ggplot2)
library(car)
library(multcomp)

# Load the dataset
data_file <- "C:/Users/hta031/Github/FEDProtein/results/MEAL_DEF_PELLET_GAP_TIME/feeding_time_gaps_categorized.csv"
data <- read.csv(data_file)

# Check structure of data
str(data)

# Convert 'Current_Size' and 'Next_Size' to factors for correct analysis
data$Current_Size <- as.factor(data$Current_Size)
data$Next_Size <- as.factor(data$Next_Size)

# Filter the data for PR and NR phases
anova_data <- data %>% filter(Diet_Phase %in% c("PR", "NR"))

# Create a directory to save results
output_dir <- "C:/Users/hta031/Github/FEDProtein/results/MEAL_DEF_PELLET_GAP_TIME/"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Descriptive statistics
grouped_stats <- anova_data %>%
  group_by(Order, Sex, Diet_Phase, Current_Size, Next_Size) %>%
  summarise(Mean_Time_Gap = mean(Time_Gap, na.rm = TRUE),
            SD_Time_Gap = sd(Time_Gap, na.rm = TRUE),
            Count = n(),
            .groups = "drop")

# Save descriptive statistics to CSV
write.csv(grouped_stats, file = paste0(output_dir, "grouped_descriptive_stats.csv"))

# ANOVA
anova_model <- aov(Time_Gap ~ Order * Sex * Diet_Phase * Current_Size, data = anova_data)
anova_summary <- summary(anova_model)

# Save ANOVA results to text file
sink(file = paste0(output_dir, "anova_results.txt"))
print(anova_summary)
sink()

# Post-hoc Holm test: Make sure 'Current_Size' is treated as a factor
pairwise_holm <- glht(anova_model, linfct = mcp(Current_Size = "Tukey"))
pairwise_holm_summary <- summary(pairwise_holm)

# Save post-hoc Holm test results to text file
sink(file = paste0(output_dir, "posthoc_holm_results.txt"))
print(pairwise_holm_summary)
sink()





##########################################################################################
########################### COMBINED MALE AND FEMALE###############################


# Load necessary libraries
library(dplyr)
library(ggplot2)
library(car)
library(multcomp)

# Load the dataset
data_file <- "C:/Users/hta031/Github/FEDProtein/results/MEAL_DEF_PELLET_GAP_TIME/feeding_time_gaps_categorized.csv"
data <- read.csv(data_file)

# Convert 'Current_Size' and 'Next_Size' to factors for correct analysis
data$Current_Size <- as.factor(data$Current_Size)
data$Next_Size <- as.factor(data$Next_Size)

# Filter the data to combine males and females in Order 1, NR phase
combined_data <- data %>% filter(Order == 1 & Diet_Phase == "NR")

# Create a directory to save results
output_dir <- "C:/Users/hta031/Github/FEDProtein/results/MEAL_DEF_PELLET_GAP_TIME/"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Descriptive statistics for combined male and female data in Order 1, phase NR
grouped_stats <- combined_data %>%
  group_by(Current_Size, Next_Size) %>%
  summarise(Mean_Time_Gap = mean(Time_Gap, na.rm = TRUE),
            SD_Time_Gap = sd(Time_Gap, na.rm = TRUE),
            Count = n(),
            .groups = "drop")

# Save descriptive statistics to CSV
write.csv(grouped_stats, file = paste0(output_dir, "combined_descriptive_stats.csv"))

# ANOVA to compare event size gaps in combined data
anova_model <- aov(Time_Gap ~ Current_Size, data = combined_data)
anova_summary <- summary(anova_model)

# Save ANOVA results to text file
sink(file = paste0(output_dir, "combined_anova_results.txt"))
print(anova_summary)
sink()

# Post-hoc Holm test to compare event sizes
pairwise_holm <- glht(anova_model, linfct = mcp(Current_Size = "Tukey"))
pairwise_holm_summary <- summary(pairwise_holm)

# Save post-hoc Holm test results to text file
sink(file = paste0(output_dir, "combined_posthoc_holm_results.txt"))
print(pairwise_holm_summary)
sink()

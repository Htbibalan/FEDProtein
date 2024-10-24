


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




##########################################################################################
#############################################  4, 5 event sizes shapiro, t-test###############


# Load necessary libraries
library(dplyr)
library(ggplot2)

# Load the dataset
data_file <- "C:/Users/hta031/Github/FEDProtein/results/MEAL_DEF_PELLET_GAP_TIME/feeding_time_gaps_categorized.csv"
data <- read.csv(data_file)

# Convert 'Current_Size' to factor for grouping
data$Current_Size <- as.factor(data$Current_Size)

# Filter the data for specific event sizes (e.g., sizes 4 and 5)
size_4_5_data <- data %>% filter(Current_Size %in% c(4, 5))

# Check for normality using Shapiro-Wilk Test (for each group)
shapiro_test_size_4 <- shapiro.test(size_4_5_data$Time_Gap[size_4_5_data$Current_Size == 4])
shapiro_test_size_5 <- shapiro.test(size_4_5_data$Time_Gap[size_4_5_data$Current_Size == 5])

# Print results of normality tests
print(shapiro_test_size_4)
print(shapiro_test_size_5)

# QQ plot for visual inspection of normality
ggplot(size_4_5_data, aes(sample = Time_Gap)) +
  facet_wrap(~Current_Size) +
  stat_qq() +
  stat_qq_line()

# Histogram for visual inspection of distribution
ggplot(size_4_5_data, aes(x = Time_Gap, fill = Current_Size)) +
  geom_histogram(alpha = 0.6, position = 'identity') +
  facet_wrap(~Current_Size) +
  theme_minimal()


# If data is not normally distributed, proceed with Mann-Whitney U Test
# Mann-Whitney U Test between event size 4 and 5
mann_whitney_test <- wilcox.test(Time_Gap ~ Current_Size, data = size_4_5_data)

# Print the Mann-Whitney test result
print(mann_whitney_test)





###########################################################################
######################### wilcoxon test#####################################
# Load necessary libraries
library(dplyr)

# Load the dataset
data_file <- "C:/Users/hta031/Github/FEDProtein/results/MEAL_DEF_PELLET_GAP_TIME/feeding_time_gaps_categorized.csv"
data <- read.csv(data_file)

# Filter out the GRAIN phase
data <- data %>% filter(Diet_Phase != "GRAIN")

# Convert Current_Size to factor for grouping
data$Current_Size <- as.factor(data$Current_Size)
data$Next_Size <- as.factor(data$Next_Size)

# Perform Wilcoxon Signed-Rank test for each pair of event sizes
output_file <- "C:/Users/hta031/Github/FEDProtein/results/MEAL_DEF_PELLET_GAP_TIME/wilcoxon_results.csv"
results <- data.frame(Current_Size = character(),
                      Next_Size = character(),
                      P_Value = numeric(),
                      stringsAsFactors = FALSE)

# Loop through pairs of Current_Size and Next_Size for Wilcoxon test
for (i in 1:9) {
  for (j in (i + 1):10) {
    subset_data <- data %>% filter(Current_Size == as.character(i) & Next_Size == as.character(j))
    
    if (nrow(subset_data) > 0) {
      test <- wilcox.test(subset_data$Time_Gap)
      results <- rbind(results, data.frame(Current_Size = i,
                                           Next_Size = j,
                                           P_Value = test$p.value))
    }
  }
}

# Save the results to CSV
write.csv(results, file = output_file, row.names = FALSE)
print(paste("Wilcoxon test results saved to", output_file))




######################################################################
#############trying logistic regression################################

# Load necessary libraries
library(dplyr)

# Load the dataset
data <- read.csv("C:/Users/hta031/Github/FEDProtein/results/MEAL_DEF_PELLET_GAP_TIME/feeding_time_gaps_categorized.csv")

# Filter out the 'GRAIN' phase
data <- data %>% filter(Diet_Phase != "GRAIN")

# Convert sizes to factors for grouping
data$Current_Size <- as.factor(data$Current_Size)
data$Next_Size <- as.factor(data$Next_Size)

# Descriptive statistics
descriptive_stats <- data %>%
  group_by(Current_Size, Next_Size) %>%
  summarise(
    Mean_Time_Gap = mean(Time_Gap),
    SD_Time_Gap = sd(Time_Gap),
    Count = n()
  )
print(descriptive_stats)

# Wilcoxon Signed-Rank test: Compare time gaps between event sizes
# For example: Compare 3-pellet events to 4-pellet events
wilcox_test_4_vs_5 <- wilcox.test(
  data$Time_Gap[data$Current_Size == 3 & data$Next_Size == 4],
  data$Time_Gap[data$Current_Size == 4 & data$Next_Size == 5],
  paired = FALSE
)

print(wilcox_test_4_vs_5)

# (Optional) Logistic Regression: Model the probability of taking one more pellet
# Here, we treat 'Next_Size' as a binary outcome (i.e., whether the mouse takes another pellet)

# Convert Next_Size into a binary variable for logistic regression
data$Takes_Another_Pellet <- ifelse(data$Next_Size == as.character(as.numeric(data$Current_Size) + 1), 1, 0)

# Logistic regression model
logistic_model <- glm(Takes_Another_Pellet ~ Current_Size + Time_Gap, data = data, family = "binomial")
summary(logistic_model)






##################################################################################################
#########################################################################
########################logistic regresion################################


# Load necessary libraries
library(dplyr)

# Load the dataset
data <- read.csv("C:/Users/hta031/Github/FEDProtein/results/MEAL_DEF_PELLET_GAP_TIME/feeding_time_gaps_categorized.csv")

# Filter out the 'GRAIN' phase
data <- data %>% filter(Diet_Phase != "GRAIN")

# Convert sizes to factors for grouping
data$Current_Size <- as.factor(data$Current_Size)
data$Next_Size <- as.factor(data$Next_Size)

# Descriptive statistics
descriptive_stats <- data %>%
  group_by(Current_Size, Next_Size) %>%
  summarise(
    Mean_Time_Gap = mean(Time_Gap),
    SD_Time_Gap = sd(Time_Gap),
    Count = n(),
    .groups = 'drop'
  )
print(descriptive_stats)

# Save descriptive statistics to a CSV file
write.csv(descriptive_stats, "C:/Users/hta031/Github/FEDProtein/results/MEAL_DEF_PELLET_GAP_TIME/L_R_descriptive_stats.csv", row.names = FALSE)

# Wilcoxon Signed-Rank test: Compare time gaps between event sizes
# Compare 3-pellet events to 4-pellet events and 4-pellet events to 5-pellet events

# Wilcoxon Test for 3-pellet to 4-pellet comparison
wilcox_test_3_vs_4 <- wilcox.test(
  data$Time_Gap[data$Current_Size == 3 & data$Next_Size == 4],
  data$Time_Gap[data$Current_Size == 4 & data$Next_Size == 5],
  paired = FALSE
)
print(wilcox_test_3_vs_4)

# Save Wilcoxon test results for 3-pellet to 4-pellet comparison
wilcox_results <- data.frame(
  Current_Size = 3,
  Next_Size = 4,
  W_Statistic = wilcox_test_3_vs_4$statistic,
  P_Value = wilcox_test_3_vs_4$p.value
)
write.csv(wilcox_results, "C:/Users/hta031/Github/FEDProtein/results/MEAL_DEF_PELLET_GAP_TIME/L_R_wilcoxon_3_vs_4.csv", row.names = FALSE)

# (Optional) Logistic Regression: Model the probability of taking one more pellet
# Here, we treat 'Next_Size' as a binary outcome (i.e., whether the mouse takes another pellet)

# Convert Next_Size into a binary variable for logistic regression
data$Takes_Another_Pellet <- ifelse(data$Next_Size == as.character(as.numeric(data$Current_Size) + 1), 1, 0)

# Logistic regression model
logistic_model <- glm(Takes_Another_Pellet ~ Current_Size + Time_Gap, data = data, family = "binomial")
logistic_summary <- summary(logistic_model)
print(logistic_summary)

# Save logistic regression results to CSV
logistic_coeffs <- as.data.frame(logistic_summary$coefficients)
write.csv(logistic_coeffs, "C:/Users/hta031/Github/FEDProtein/results/MEAL_DEF_PELLET_GAP_TIME/L_R_logistic_regression_results.csv", row.names = TRUE)


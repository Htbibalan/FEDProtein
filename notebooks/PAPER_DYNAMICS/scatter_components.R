



####################################################### MEAL FREQ #######################################
####################################################################################################################
# Load necessary libraries
library(tidyverse)
library(reshape2)
library(car)
library(multcomp)

# Load the data
data <- read.csv("C:/Users/hta031/Github/FEDProtein/results/Scatter_plots/meal/meal_frequency_PR_NR.csv")

# Reshape the data to long format
data_long <- melt(data, id.vars = c("mouse_id", "sex", "order"),
                  measure.vars = c("NR", "PR"),
                  variable.name = "Diet_Phase", value.name = "Total_Parameters")

# Step 1: Descriptive statistics
desc_stats <- data_long %>%
  group_by(sex, order, Diet_Phase) %>%
  summarise(
    mean_total_parameters = mean(Total_Parameters, na.rm = TRUE),
    sd_total_parameters = sd(Total_Parameters, na.rm = TRUE),
    count = n()
  )

# Save descriptive statistics to CSV
write.csv(desc_stats, "C:/Users/hta031/Github/FEDProtein/results/Scatter_plots/meal/FREQ_desc_stats.csv")

# Step 2: Run ANOVA with grouping information
anova_model <- aov(Total_Parameters ~ sex * order * Diet_Phase, data = data_long)
anova_results <- Anova(anova_model, type = 2)

# Create ANOVA table with grouping information
anova_table <- data.frame(
  Factor = rownames(anova_results),
  anova_results
)

# Save ANOVA results to CSV
write.csv(anova_table, "C:/Users/hta031/Github/FEDProtein/results/Scatter_plots/meal/FREQ_anova_results.csv")

# Step 3: Tukey HSD post-hoc test
# Create interaction term for group comparisons
data_long$interaction_term <- interaction(data_long$sex, data_long$order, data_long$Diet_Phase)

# Perform Tukey HSD test on the interaction term
tukey_test <- TukeyHSD(aov(Total_Parameters ~ interaction_term, data = data_long))
tukey_table <- as.data.frame(tukey_test$interaction_term)

# Add group information to the Tukey results
comparison_labels <- str_split_fixed(rownames(tukey_test$interaction_term), ":", 2)
tukey_table$Group1 <- comparison_labels[, 1]
tukey_table$Group2 <- comparison_labels[, 2]

# Save Tukey HSD results to CSV with group labels
write.csv(tukey_table, "C:/Users/hta031/Github/FEDProtein/results/Scatter_plots/meal/FREQ_tukey_results.csv")

# Step 4: Holm post-hoc test
# Perform pairwise t-tests with Holm correction
holm_test <- pairwise.t.test(data_long$Total_Parameters, data_long$interaction_term, p.adjust.method = "holm")

# Extract and save Holm test results to CSV
holm_table <- as.data.frame(holm_test$p.value)
holm_table$Comparison <- rownames(holm_table)
write.csv(holm_table, "C:/Users/hta031/Github/FEDProtein/results/Scatter_plots/meal/FREQ_holm_results.csv")

# Print summary of results
print(desc_stats)
print(anova_table)
print(tukey_table)
print(holm_table)


####################################################################################################################
####################################################   Meal_number   ################################################
# Load necessary libraries
library(tidyverse)
library(reshape2)
library(car)
library(multcomp)

# Load the data
data <- read.csv("C:/Users/hta031/Github/FEDProtein/results/Scatter_plots/meal/meal_number_PR_NR.csv")

# Reshape the data to long format
data_long <- melt(data, id.vars = c("mouse_id", "sex", "order"),
                  measure.vars = c("NR", "PR"),
                  variable.name = "Diet_Phase", value.name = "Total_Parameters")

# Step 1: Descriptive statistics
desc_stats <- data_long %>%
  group_by(sex, order, Diet_Phase) %>%
  summarise(
    mean_total_parameters = mean(Total_Parameters, na.rm = TRUE),
    sd_total_parameters = sd(Total_Parameters, na.rm = TRUE),
    count = n()
  )

# Save descriptive statistics to CSV
write.csv(desc_stats, "C:/Users/hta031/Github/FEDProtein/results/Scatter_plots/meal/NUMBER_desc_stats.csv")

# Step 2: Run ANOVA with grouping information
anova_model <- aov(Total_Parameters ~ sex * order * Diet_Phase, data = data_long)
anova_results <- Anova(anova_model, type = 2)

# Create ANOVA table with grouping information
anova_table <- data.frame(
  Factor = rownames(anova_results),
  anova_results
)

# Save ANOVA results to CSV
write.csv(anova_table, "C:/Users/hta031/Github/FEDProtein/results/Scatter_plots/meal/NUMBER_anova_results.csv")

# Step 3: Tukey HSD post-hoc test
# Create interaction term for group comparisons
data_long$interaction_term <- interaction(data_long$sex, data_long$order, data_long$Diet_Phase)

# Perform Tukey HSD test on the interaction term
tukey_test <- TukeyHSD(aov(Total_Parameters ~ interaction_term, data = data_long))
tukey_table <- as.data.frame(tukey_test$interaction_term)

# Add group information to the Tukey results
comparison_labels <- str_split_fixed(rownames(tukey_test$interaction_term), ":", 2)
tukey_table$Group1 <- comparison_labels[, 1]
tukey_table$Group2 <- comparison_labels[, 2]

# Save Tukey HSD results to CSV with group labels
write.csv(tukey_table, "C:/Users/hta031/Github/FEDProtein/results/Scatter_plots/meal/NUMBER_tukey_results.csv")

# Step 4: Holm post-hoc test
# Perform pairwise t-tests with Holm correction
holm_test <- pairwise.t.test(data_long$Total_Parameters, data_long$interaction_term, p.adjust.method = "holm")

# Extract and save Holm test results to CSV
holm_table <- as.data.frame(holm_test$p.value)
holm_table$Comparison <- rownames(holm_table)
write.csv(holm_table, "C:/Users/hta031/Github/FEDProtein/results/Scatter_plots/meal/NUMBER_holm_results.csv")

# Print summary of results
print(desc_stats)
print(anova_table)
print(tukey_table)
print(holm_table)




####################################################################################################################
####################################################   MEAL_SIZE   ################################################
# Load necessary libraries
library(tidyverse)
library(reshape2)
library(car)
library(multcomp)

# Load the data
data <- read.csv("C:/Users/hta031/Github/FEDProtein/results/Scatter_plots/meal/meal_size_PR_NR.csv")

# Reshape the data to long format
data_long <- melt(data, id.vars = c("mouse_id", "sex", "order"),
                  measure.vars = c("NR", "PR"),
                  variable.name = "Diet_Phase", value.name = "Total_Parameters")

# Step 1: Descriptive statistics
desc_stats <- data_long %>%
  group_by(sex, order, Diet_Phase) %>%
  summarise(
    mean_total_parameters = mean(Total_Parameters, na.rm = TRUE),
    sd_total_parameters = sd(Total_Parameters, na.rm = TRUE),
    count = n()
  )

# Save descriptive statistics to CSV
write.csv(desc_stats, "C:/Users/hta031/Github/FEDProtein/results/Scatter_plots/meal/SIZE_desc_stats.csv")

# Step 2: Run ANOVA with grouping information
anova_model <- aov(Total_Parameters ~ sex * order * Diet_Phase, data = data_long)
anova_results <- Anova(anova_model, type = 2)

# Create ANOVA table with grouping information
anova_table <- data.frame(
  Factor = rownames(anova_results),
  anova_results
)

# Save ANOVA results to CSV
write.csv(anova_table, "C:/Users/hta031/Github/FEDProtein/results/Scatter_plots/meal/SIZE_anova_results.csv")

# Step 3: Tukey HSD post-hoc test
# Create interaction term for group comparisons
data_long$interaction_term <- interaction(data_long$sex, data_long$order, data_long$Diet_Phase)

# Perform Tukey HSD test on the interaction term
tukey_test <- TukeyHSD(aov(Total_Parameters ~ interaction_term, data = data_long))
tukey_table <- as.data.frame(tukey_test$interaction_term)

# Add group information to the Tukey results
comparison_labels <- str_split_fixed(rownames(tukey_test$interaction_term), ":", 2)
tukey_table$Group1 <- comparison_labels[, 1]
tukey_table$Group2 <- comparison_labels[, 2]

# Save Tukey HSD results to CSV with group labels
write.csv(tukey_table, "C:/Users/hta031/Github/FEDProtein/results/Scatter_plots/meal/SIZE_tukey_results.csv")

# Step 4: Holm post-hoc test
# Perform pairwise t-tests with Holm correction
holm_test <- pairwise.t.test(data_long$Total_Parameters, data_long$interaction_term, p.adjust.method = "holm")

# Extract and save Holm test results to CSV
holm_table <- as.data.frame(holm_test$p.value)
holm_table$Comparison <- rownames(holm_table)
write.csv(holm_table, "C:/Users/hta031/Github/FEDProtein/results/Scatter_plots/meal/SIZE_holm_results.csv")

# Print summary of results
print(desc_stats)
print(anova_table)
print(tukey_table)
print(holm_table)





###############################################################################
###################### testing pooled meal size #####################################


# Load necessary libraries
library(tidyverse)
library(reshape2)
library(car)
library(multcomp)

# Load the data
data <- read.csv("C:/Users/hta031/Github/FEDProtein/results/Scatter_plots/meal/meal_size_PR_NR.csv")

# Reshape the data to long format, removing `sex` from id.vars
data_long <- melt(data, id.vars = c("mouse_id", "order"),
                  measure.vars = c("NR", "PR"),
                  variable.name = "Diet_Phase", value.name = "Total_Parameters")

# Step 1: Descriptive statistics
desc_stats <- data_long %>%
  group_by(order, Diet_Phase) %>%
  summarise(
    mean_total_parameters = mean(Total_Parameters, na.rm = TRUE),
    sd_total_parameters = sd(Total_Parameters, na.rm = TRUE),
    count = n()
  )

# Save descriptive statistics to CSV
write.csv(desc_stats, "C:/Users/hta031/Github/FEDProtein/results/Scatter_plots/meal/POOLSIZE_desc_stats.csv")

# Step 2: Run ANOVA without `sex` in the formula
anova_model <- aov(Total_Parameters ~ order * Diet_Phase, data = data_long)
anova_results <- Anova(anova_model, type = 2)

# Create ANOVA table with grouping information
anova_table <- data.frame(
  Factor = rownames(anova_results),
  anova_results
)

# Save ANOVA results to CSV
write.csv(anova_table, "C:/Users/hta031/Github/FEDProtein/results/Scatter_plots/meal/POOLSIZE_anova_results.csv")

# Step 3: Tukey HSD post-hoc test
# Create interaction term for group comparisons without `sex`
data_long$interaction_term <- interaction(data_long$order, data_long$Diet_Phase)

# Perform Tukey HSD test on the interaction term
tukey_test <- TukeyHSD(aov(Total_Parameters ~ interaction_term, data = data_long))
tukey_table <- as.data.frame(tukey_test$interaction_term)

# Add group information to the Tukey results
comparison_labels <- str_split_fixed(rownames(tukey_test$interaction_term), ":", 2)
tukey_table$Group1 <- comparison_labels[, 1]
tukey_table$Group2 <- comparison_labels[, 2]

# Save Tukey HSD results to CSV with group labels
write.csv(tukey_table, "C:/Users/hta031/Github/FEDProtein/results/Scatter_plots/meal/POOLSIZE_tukey_results.csv")

# Step 4: Holm post-hoc test
# Perform pairwise t-tests with Holm correction
holm_test <- pairwise.t.test(data_long$Total_Parameters, data_long$interaction_term, p.adjust.method = "holm")

# Extract and save Holm test results to CSV
holm_table <- as.data.frame(holm_test$p.value)
holm_table$Comparison <- rownames(holm_table)
write.csv(holm_table, "C:/Users/hta031/Github/FEDProtein/results/Scatter_plots/meal/POOLSIZE_holm_results.csv")

# Print summary of results
print(desc_stats)
print(anova_table)
print(tukey_table)
print(holm_table)

############################################### MEAL OVER#####################################################################
###########################################  SNACK STARTS###############################################################


####################################################################################################################
####################################################   SNACK_NUMBER   ################################################
# Load necessary libraries
library(tidyverse)
library(reshape2)
library(car)
library(multcomp)

# Load the data
data <- read.csv("C:/Users/hta031/Github/FEDProtein/results/Scatter_plots/snack/snack_number_PR_NR.csv")

# Reshape the data to long format
data_long <- melt(data, id.vars = c("mouse_id", "sex", "order"),
                  measure.vars = c("NR", "PR"),
                  variable.name = "Diet_Phase", value.name = "Total_Parameters")

# Step 1: Descriptive statistics
desc_stats <- data_long %>%
  group_by(sex, order, Diet_Phase) %>%
  summarise(
    mean_total_parameters = mean(Total_Parameters, na.rm = TRUE),
    sd_total_parameters = sd(Total_Parameters, na.rm = TRUE),
    count = n()
  )

# Save descriptive statistics to CSV
write.csv(desc_stats, "C:/Users/hta031/Github/FEDProtein/results/Scatter_plots/snack/NUMBER_desc_stats.csv")

# Step 2: Run ANOVA with grouping information
anova_model <- aov(Total_Parameters ~ sex * order * Diet_Phase, data = data_long)
anova_results <- Anova(anova_model, type = 2)

# Create ANOVA table with grouping information
anova_table <- data.frame(
  Factor = rownames(anova_results),
  anova_results
)

# Save ANOVA results to CSV
write.csv(anova_table, "C:/Users/hta031/Github/FEDProtein/results/Scatter_plots/snack/NUMBER_anova_results.csv")

# Step 3: Tukey HSD post-hoc test
# Create interaction term for group comparisons
data_long$interaction_term <- interaction(data_long$sex, data_long$order, data_long$Diet_Phase)

# Perform Tukey HSD test on the interaction term
tukey_test <- TukeyHSD(aov(Total_Parameters ~ interaction_term, data = data_long))
tukey_table <- as.data.frame(tukey_test$interaction_term)

# Add group information to the Tukey results
comparison_labels <- str_split_fixed(rownames(tukey_test$interaction_term), ":", 2)
tukey_table$Group1 <- comparison_labels[, 1]
tukey_table$Group2 <- comparison_labels[, 2]

# Save Tukey HSD results to CSV with group labels
write.csv(tukey_table, "C:/Users/hta031/Github/FEDProtein/results/Scatter_plots/snack/NUMBER_tukey_results.csv")

# Step 4: Holm post-hoc test
# Perform pairwise t-tests with Holm correction
holm_test <- pairwise.t.test(data_long$Total_Parameters, data_long$interaction_term, p.adjust.method = "holm")

# Extract and save Holm test results to CSV
holm_table <- as.data.frame(holm_test$p.value)
holm_table$Comparison <- rownames(holm_table)
write.csv(holm_table, "C:/Users/hta031/Github/FEDProtein/results/Scatter_plots/snack/NUMBER_holm_results.csv")

# Print summary of results
print(desc_stats)
print(anova_table)
print(tukey_table)
print(holm_table)




####################################################################################################################
####################################################   SNACK_FREQ   ################################################
# Load necessary libraries
library(tidyverse)
library(reshape2)
library(car)
library(multcomp)

# Load the data
data <- read.csv("C:/Users/hta031/Github/FEDProtein/results/Scatter_plots/snack/snack_frequency_PR_NR.csv")

# Reshape the data to long format
data_long <- melt(data, id.vars = c("mouse_id", "sex", "order"),
                  measure.vars = c("NR", "PR"),
                  variable.name = "Diet_Phase", value.name = "Total_Parameters")

# Step 1: Descriptive statistics
desc_stats <- data_long %>%
  group_by(sex, order, Diet_Phase) %>%
  summarise(
    mean_total_parameters = mean(Total_Parameters, na.rm = TRUE),
    sd_total_parameters = sd(Total_Parameters, na.rm = TRUE),
    count = n()
  )

# Save descriptive statistics to CSV
write.csv(desc_stats, "C:/Users/hta031/Github/FEDProtein/results/Scatter_plots/snack/FREQ_desc_stats.csv")

# Step 2: Run ANOVA with grouping information
anova_model <- aov(Total_Parameters ~ sex * order * Diet_Phase, data = data_long)
anova_results <- Anova(anova_model, type = 2)

# Create ANOVA table with grouping information
anova_table <- data.frame(
  Factor = rownames(anova_results),
  anova_results
)

# Save ANOVA results to CSV
write.csv(anova_table, "C:/Users/hta031/Github/FEDProtein/results/Scatter_plots/snack/FREQ_anova_results.csv")

# Step 3: Tukey HSD post-hoc test
# Create interaction term for group comparisons
data_long$interaction_term <- interaction(data_long$sex, data_long$order, data_long$Diet_Phase)

# Perform Tukey HSD test on the interaction term
tukey_test <- TukeyHSD(aov(Total_Parameters ~ interaction_term, data = data_long))
tukey_table <- as.data.frame(tukey_test$interaction_term)

# Add group information to the Tukey results
comparison_labels <- str_split_fixed(rownames(tukey_test$interaction_term), ":", 2)
tukey_table$Group1 <- comparison_labels[, 1]
tukey_table$Group2 <- comparison_labels[, 2]

# Save Tukey HSD results to CSV with group labels
write.csv(tukey_table, "C:/Users/hta031/Github/FEDProtein/results/Scatter_plots/snack/FREQ_tukey_results.csv")

# Step 4: Holm post-hoc test
# Perform pairwise t-tests with Holm correction
holm_test <- pairwise.t.test(data_long$Total_Parameters, data_long$interaction_term, p.adjust.method = "holm")

# Extract and save Holm test results to CSV
holm_table <- as.data.frame(holm_test$p.value)
holm_table$Comparison <- rownames(holm_table)
write.csv(holm_table, "C:/Users/hta031/Github/FEDProtein/results/Scatter_plots/snack/FREQ_holm_results.csv")

# Print summary of results
print(desc_stats)
print(anova_table)
print(tukey_table)
print(holm_table)




############################################### SNACK OVER#####################################################################
###########################################  FEAST STARTS###############################################################


####################################################################################################################
####################################################   FEAST_FREQ   ################################################
# Load necessary libraries
library(tidyverse)
library(reshape2)
library(car)
library(multcomp)

# Load the data
data <- read.csv("C:/Users/hta031/Github/FEDProtein/results/Scatter_plots/mega_meal/mega_meal_frequency_PR_NR.csv")

# Reshape the data to long format
data_long <- melt(data, id.vars = c("mouse_id", "sex", "order"),
                  measure.vars = c("NR", "PR"),
                  variable.name = "Diet_Phase", value.name = "Total_Parameters")

# Step 1: Descriptive statistics
desc_stats <- data_long %>%
  group_by(sex, order, Diet_Phase) %>%
  summarise(
    mean_total_parameters = mean(Total_Parameters, na.rm = TRUE),
    sd_total_parameters = sd(Total_Parameters, na.rm = TRUE),
    count = n()
  )

# Save descriptive statistics to CSV
write.csv(desc_stats, "C:/Users/hta031/Github/FEDProtein/results/Scatter_plots/mega_meal/FREQ_desc_stats.csv")

# Step 2: Run ANOVA with grouping information
anova_model <- aov(Total_Parameters ~ sex * order * Diet_Phase, data = data_long)
anova_results <- Anova(anova_model, type = 2)

# Create ANOVA table with grouping information
anova_table <- data.frame(
  Factor = rownames(anova_results),
  anova_results
)

# Save ANOVA results to CSV
write.csv(anova_table, "C:/Users/hta031/Github/FEDProtein/results/Scatter_plots/mega_meal/FREQ_anova_results.csv")

# Step 3: Tukey HSD post-hoc test
# Create interaction term for group comparisons
data_long$interaction_term <- interaction(data_long$sex, data_long$order, data_long$Diet_Phase)

# Perform Tukey HSD test on the interaction term
tukey_test <- TukeyHSD(aov(Total_Parameters ~ interaction_term, data = data_long))
tukey_table <- as.data.frame(tukey_test$interaction_term)

# Add group information to the Tukey results
comparison_labels <- str_split_fixed(rownames(tukey_test$interaction_term), ":", 2)
tukey_table$Group1 <- comparison_labels[, 1]
tukey_table$Group2 <- comparison_labels[, 2]

# Save Tukey HSD results to CSV with group labels
write.csv(tukey_table, "C:/Users/hta031/Github/FEDProtein/results/Scatter_plots/mega_meal/FREQ_tukey_results.csv")

# Step 4: Holm post-hoc test
# Perform pairwise t-tests with Holm correction
holm_test <- pairwise.t.test(data_long$Total_Parameters, data_long$interaction_term, p.adjust.method = "holm")

# Extract and save Holm test results to CSV
holm_table <- as.data.frame(holm_test$p.value)
holm_table$Comparison <- rownames(holm_table)
write.csv(holm_table, "C:/Users/hta031/Github/FEDProtein/results/Scatter_plots/mega_meal/FREQ_holm_results.csv")

# Print summary of results
print(desc_stats)
print(anova_table)
print(tukey_table)
print(holm_table)



####################################################################################################################
####################################################   FEAST_NUMBER  ################################################
# Load necessary libraries
library(tidyverse)
library(reshape2)
library(car)
library(multcomp)

# Load the data
data <- read.csv("C:/Users/hta031/Github/FEDProtein/results/Scatter_plots/mega_meal/mega_meal_number_PR_NR.csv")

# Reshape the data to long format
data_long <- melt(data, id.vars = c("mouse_id", "sex", "order"),
                  measure.vars = c("NR", "PR"),
                  variable.name = "Diet_Phase", value.name = "Total_Parameters")

# Step 1: Descriptive statistics
desc_stats <- data_long %>%
  group_by(sex, order, Diet_Phase) %>%
  summarise(
    mean_total_parameters = mean(Total_Parameters, na.rm = TRUE),
    sd_total_parameters = sd(Total_Parameters, na.rm = TRUE),
    count = n()
  )

# Save descriptive statistics to CSV
write.csv(desc_stats, "C:/Users/hta031/Github/FEDProtein/results/Scatter_plots/mega_meal/NUMBER_desc_stats.csv")

# Step 2: Run ANOVA with grouping information
anova_model <- aov(Total_Parameters ~ sex * order * Diet_Phase, data = data_long)
anova_results <- Anova(anova_model, type = 2)

# Create ANOVA table with grouping information
anova_table <- data.frame(
  Factor = rownames(anova_results),
  anova_results
)

# Save ANOVA results to CSV
write.csv(anova_table, "C:/Users/hta031/Github/FEDProtein/results/Scatter_plots/mega_meal/NUMBER_anova_results.csv")

# Step 3: Tukey HSD post-hoc test
# Create interaction term for group comparisons
data_long$interaction_term <- interaction(data_long$sex, data_long$order, data_long$Diet_Phase)

# Perform Tukey HSD test on the interaction term
tukey_test <- TukeyHSD(aov(Total_Parameters ~ interaction_term, data = data_long))
tukey_table <- as.data.frame(tukey_test$interaction_term)

# Add group information to the Tukey results
comparison_labels <- str_split_fixed(rownames(tukey_test$interaction_term), ":", 2)
tukey_table$Group1 <- comparison_labels[, 1]
tukey_table$Group2 <- comparison_labels[, 2]

# Save Tukey HSD results to CSV with group labels
write.csv(tukey_table, "C:/Users/hta031/Github/FEDProtein/results/Scatter_plots/mega_meal/NUMBER_tukey_results.csv")

# Step 4: Holm post-hoc test
# Perform pairwise t-tests with Holm correction
holm_test <- pairwise.t.test(data_long$Total_Parameters, data_long$interaction_term, p.adjust.method = "holm")

# Extract and save Holm test results to CSV
holm_table <- as.data.frame(holm_test$p.value)
holm_table$Comparison <- rownames(holm_table)
write.csv(holm_table, "C:/Users/hta031/Github/FEDProtein/results/Scatter_plots/mega_meal/NUMBER_holm_results.csv")

# Print summary of results
print(desc_stats)
print(anova_table)
print(tukey_table)
print(holm_table)




####################################################################################################################
####################################################   FEAST_SIZE  ################################################
# Load necessary libraries
library(tidyverse)
library(reshape2)
library(car)
library(multcomp)

# Load the data
data <- read.csv("C:/Users/hta031/Github/FEDProtein/results/Scatter_plots/mega_meal/mega_meal_size_PR_NR.csv")

# Reshape the data to long format
data_long <- melt(data, id.vars = c("mouse_id", "sex", "order"),
                  measure.vars = c("NR", "PR"),
                  variable.name = "Diet_Phase", value.name = "Total_Parameters")

# Step 1: Descriptive statistics
desc_stats <- data_long %>%
  group_by(sex, order, Diet_Phase) %>%
  summarise(
    mean_total_parameters = mean(Total_Parameters, na.rm = TRUE),
    sd_total_parameters = sd(Total_Parameters, na.rm = TRUE),
    count = n()
  )

# Save descriptive statistics to CSV
write.csv(desc_stats, "C:/Users/hta031/Github/FEDProtein/results/Scatter_plots/mega_meal/SIZE_desc_stats.csv")

# Step 2: Run ANOVA with grouping information
anova_model <- aov(Total_Parameters ~ sex * order * Diet_Phase, data = data_long)
anova_results <- Anova(anova_model, type = 2)

# Create ANOVA table with grouping information
anova_table <- data.frame(
  Factor = rownames(anova_results),
  anova_results
)

# Save ANOVA results to CSV
write.csv(anova_table, "C:/Users/hta031/Github/FEDProtein/results/Scatter_plots/mega_meal/SIZE_anova_results.csv")

# Step 3: Tukey HSD post-hoc test
# Create interaction term for group comparisons
data_long$interaction_term <- interaction(data_long$sex, data_long$order, data_long$Diet_Phase)

# Perform Tukey HSD test on the interaction term
tukey_test <- TukeyHSD(aov(Total_Parameters ~ interaction_term, data = data_long))
tukey_table <- as.data.frame(tukey_test$interaction_term)

# Add group information to the Tukey results
comparison_labels <- str_split_fixed(rownames(tukey_test$interaction_term), ":", 2)
tukey_table$Group1 <- comparison_labels[, 1]
tukey_table$Group2 <- comparison_labels[, 2]

# Save Tukey HSD results to CSV with group labels
write.csv(tukey_table, "C:/Users/hta031/Github/FEDProtein/results/Scatter_plots/mega_meal/SIZE_tukey_results.csv")

# Step 4: Holm post-hoc test
# Perform pairwise t-tests with Holm correction
holm_test <- pairwise.t.test(data_long$Total_Parameters, data_long$interaction_term, p.adjust.method = "holm")

# Extract and save Holm test results to CSV
holm_table <- as.data.frame(holm_test$p.value)
holm_table$Comparison <- rownames(holm_table)
write.csv(holm_table, "C:/Users/hta031/Github/FEDProtein/results/Scatter_plots/mega_meal/SIZE_holm_results.csv")

# Print summary of results
print(desc_stats)
print(anova_table)
print(tukey_table)
print(holm_table)






####################################################### MEAL FREQ #######################################
####################################################################################################################
# Load necessary libraries
library(tidyverse)
library(reshape2)
library(car)
library(multcomp)

# Load the data
data <- read.csv("C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/MEAL/MEAL_FREQUENCY_PR_NR.csv")

# Reshape the data to long format
data_long <- melt(data, id.vars = c("Mouse.ID", "Sex", "Order"),
                  measure.vars = c("NR", "PR"),
                  variable.name = "Diet_Phase", value.name = "Total_Parameters")

# Step 1: Descriptive statistics
desc_stats <- data_long %>%
  group_by(Sex, Order, Diet_Phase) %>%
  summarise(
    mean_total_parameters = mean(Total_Parameters, na.rm = TRUE),
    sd_total_parameters = sd(Total_Parameters, na.rm = TRUE),
    count = n()
  )

# Save descriptive statistics to CSV
write.csv(desc_stats, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/MEAL/FREQ/FREQ_desc_stats.csv")

# Step 2: Run ANOVA with grouping information
anova_model <- aov(Total_Parameters ~ Sex * Order * Diet_Phase, data = data_long)
anova_results <- Anova(anova_model, type = 2)

# Create ANOVA table with grouping information
anova_table <- data.frame(
  Factor = rownames(anova_results),
  anova_results
)

# Save ANOVA results to CSV
write.csv(anova_table, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/MEAL/FREQ/FREQ_anova_results.csv")

# Step 3: Tukey HSD post-hoc test
# Create interaction term for group comparisons
data_long$interaction_term <- interaction(data_long$Sex, data_long$Order, data_long$Diet_Phase)

# Perform Tukey HSD test on the interaction term
tukey_test <- TukeyHSD(aov(Total_Parameters ~ interaction_term, data = data_long))
tukey_table <- as.data.frame(tukey_test$interaction_term)

# Add group information to the Tukey results
comparison_labels <- str_split_fixed(rownames(tukey_test$interaction_term), ":", 2)
tukey_table$Group1 <- comparison_labels[, 1]
tukey_table$Group2 <- comparison_labels[, 2]

# Save Tukey HSD results to CSV with group labels
write.csv(tukey_table, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/MEAL/FREQ/FREQ_tukey_results.csv")

# Step 4: Holm post-hoc test
# Perform pairwise t-tests with Holm correction
holm_test <- pairwise.t.test(data_long$Total_Parameters, data_long$interaction_term, p.adjust.method = "holm")

# Extract and save Holm test results to CSV
holm_table <- as.data.frame(holm_test$p.value)
holm_table$Comparison <- rownames(holm_table)
write.csv(holm_table, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/MEAL/FREQ/FREQ_holm_results.csv")

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
data <- read.csv("C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/MEAL/MEAL_NUMBER_PR_NR.csv")

# Reshape the data to long format
data_long <- melt(data, id.vars = c("Mouse.ID", "Sex", "Order"),
                  measure.vars = c("NR", "PR"),
                  variable.name = "Diet_Phase", value.name = "Total_Parameters")

# Step 1: Descriptive statistics
desc_stats <- data_long %>%
  group_by(Sex, Order, Diet_Phase) %>%
  summarise(
    mean_total_parameters = mean(Total_Parameters, na.rm = TRUE),
    sd_total_parameters = sd(Total_Parameters, na.rm = TRUE),
    count = n()
  )

# Save descriptive statistics to CSV
write.csv(desc_stats, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/MEAL/NUMBER/NUMBER_desc_stats.csv")

# Step 2: Run ANOVA with grouping information
anova_model <- aov(Total_Parameters ~ Sex * Order * Diet_Phase, data = data_long)
anova_results <- Anova(anova_model, type = 2)

# Create ANOVA table with grouping information
anova_table <- data.frame(
  Factor = rownames(anova_results),
  anova_results
)

# Save ANOVA results to CSV
write.csv(anova_table, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/MEAL/NUMBER/NUMBER_anova_results.csv")

# Step 3: Tukey HSD post-hoc test
# Create interaction term for group comparisons
data_long$interaction_term <- interaction(data_long$Sex, data_long$Order, data_long$Diet_Phase)

# Perform Tukey HSD test on the interaction term
tukey_test <- TukeyHSD(aov(Total_Parameters ~ interaction_term, data = data_long))
tukey_table <- as.data.frame(tukey_test$interaction_term)

# Add group information to the Tukey results
comparison_labels <- str_split_fixed(rownames(tukey_test$interaction_term), ":", 2)
tukey_table$Group1 <- comparison_labels[, 1]
tukey_table$Group2 <- comparison_labels[, 2]

# Save Tukey HSD results to CSV with group labels
write.csv(tukey_table, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/MEAL/NUMBER/NUMBER_tukey_results.csv")

# Step 4: Holm post-hoc test
# Perform pairwise t-tests with Holm correction
holm_test <- pairwise.t.test(data_long$Total_Parameters, data_long$interaction_term, p.adjust.method = "holm")

# Extract and save Holm test results to CSV
holm_table <- as.data.frame(holm_test$p.value)
holm_table$Comparison <- rownames(holm_table)
write.csv(holm_table, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/MEAL/NUMBER/NUMBER_holm_results.csv")

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
data <- read.csv("C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/MEAL/MEAL_SIZE_PR_NR.csv")

# Reshape the data to long format
data_long <- melt(data, id.vars = c("Mouse.ID", "Sex", "Order"),
                  measure.vars = c("NR", "PR"),
                  variable.name = "Diet_Phase", value.name = "Total_Parameters")

# Step 1: Descriptive statistics
desc_stats <- data_long %>%
  group_by(Sex, Order, Diet_Phase) %>%
  summarise(
    mean_total_parameters = mean(Total_Parameters, na.rm = TRUE),
    sd_total_parameters = sd(Total_Parameters, na.rm = TRUE),
    count = n()
  )

# Save descriptive statistics to CSV
write.csv(desc_stats, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/MEAL/SIZE/SIZE_desc_stats.csv")

# Step 2: Run ANOVA with grouping information
anova_model <- aov(Total_Parameters ~ Sex * Order * Diet_Phase, data = data_long)
anova_results <- Anova(anova_model, type = 2)

# Create ANOVA table with grouping information
anova_table <- data.frame(
  Factor = rownames(anova_results),
  anova_results
)

# Save ANOVA results to CSV
write.csv(anova_table, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/MEAL/SIZE/SIZE_anova_results.csv")

# Step 3: Tukey HSD post-hoc test
# Create interaction term for group comparisons
data_long$interaction_term <- interaction(data_long$Sex, data_long$Order, data_long$Diet_Phase)

# Perform Tukey HSD test on the interaction term
tukey_test <- TukeyHSD(aov(Total_Parameters ~ interaction_term, data = data_long))
tukey_table <- as.data.frame(tukey_test$interaction_term)

# Add group information to the Tukey results
comparison_labels <- str_split_fixed(rownames(tukey_test$interaction_term), ":", 2)
tukey_table$Group1 <- comparison_labels[, 1]
tukey_table$Group2 <- comparison_labels[, 2]

# Save Tukey HSD results to CSV with group labels
write.csv(tukey_table, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/MEAL/SIZE/SIZE_tukey_results.csv")

# Step 4: Holm post-hoc test
# Perform pairwise t-tests with Holm correction
holm_test <- pairwise.t.test(data_long$Total_Parameters, data_long$interaction_term, p.adjust.method = "holm")

# Extract and save Holm test results to CSV
holm_table <- as.data.frame(holm_test$p.value)
holm_table$Comparison <- rownames(holm_table)
write.csv(holm_table, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/MEAL/SIZE/SIZE_holm_results.csv")

# Print summary of results
print(desc_stats)
print(anova_table)
print(tukey_table)
print(holm_table)





#######################################BELOW COMBINED######################## BELOW COMBINED#########################BELOW COMBINED###################################
###################### There is not significant sex difference between groups, so POOLED MEAL DATA BELOW #####################################
####################################################### MEAL FREQ COMBINED #######################################
####################################################################################################################
# Load necessary libraries
library(tidyverse)
library(reshape2)
library(car)
library(multcomp)

# Load the data
data <- read.csv("C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/MEAL/MEAL_FREQUENCY_PR_NR.csv")

# Reshape the data to long format
data_long <- melt(data, id.vars = c("Mouse.ID","Order"),
                  measure.vars = c("NR", "PR"),
                  variable.name = "Diet_Phase", value.name = "Total_Parameters")

# Step 1: Descriptive statistics
desc_stats <- data_long %>%
  group_by(Order, Diet_Phase) %>%
  summarise(
    mean_total_parameters = mean(Total_Parameters, na.rm = TRUE),
    sd_total_parameters = sd(Total_Parameters, na.rm = TRUE),
    count = n()
  )

# Save descriptive statistics to CSV
write.csv(desc_stats, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/MEAL/FREQ/COMBINE_FREQ_desc_stats.csv")

# Step 2: Run ANOVA with grouping information
anova_model <- aov(Total_Parameters ~ Order * Diet_Phase, data = data_long)
anova_results <- Anova(anova_model, type = 2)

# Create ANOVA table with grouping information
anova_table <- data.frame(
  Factor = rownames(anova_results),
  anova_results
)

# Save ANOVA results to CSV
write.csv(anova_table, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/MEAL/FREQ/COMBINE_FREQ_anova_results.csv")

# Step 3: Tukey HSD post-hoc test
# Create interaction term for group comparisons
data_long$interaction_term <- interaction(data_long$Order, data_long$Diet_Phase)

# Perform Tukey HSD test on the interaction term
tukey_test <- TukeyHSD(aov(Total_Parameters ~ interaction_term, data = data_long))
tukey_table <- as.data.frame(tukey_test$interaction_term)

# Add group information to the Tukey results
comparison_labels <- str_split_fixed(rownames(tukey_test$interaction_term), ":", 2)
tukey_table$Group1 <- comparison_labels[, 1]
tukey_table$Group2 <- comparison_labels[, 2]

# Save Tukey HSD results to CSV with group labels
write.csv(tukey_table, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/MEAL/FREQ/COMBINE_FREQ_tukey_results.csv")

# Step 4: Holm post-hoc test
# Perform pairwise t-tests with Holm correction
holm_test <- pairwise.t.test(data_long$Total_Parameters, data_long$interaction_term, p.adjust.method = "holm")

# Extract and save Holm test results to CSV
holm_table <- as.data.frame(holm_test$p.value)
holm_table$Comparison <- rownames(holm_table)
write.csv(holm_table, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/MEAL/FREQ/COMBINE_FREQ_holm_results.csv")

# Print summary of results
print(desc_stats)
print(anova_table)
print(tukey_table)
print(holm_table)




####################################################################################################################
####################################################   Meal_number  COMBINED  ################################################
# Load necessary libraries
library(tidyverse)
library(reshape2)
library(car)
library(multcomp)

# Load the data
data <- read.csv("C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/MEAL/MEAL_NUMBER_PR_NR.csv")

# Reshape the data to long format
data_long <- melt(data, id.vars = c("Mouse.ID", "Order"),
                  measure.vars = c("NR", "PR"),
                  variable.name = "Diet_Phase", value.name = "Total_Parameters")

# Step 1: Descriptive statistics
desc_stats <- data_long %>%
  group_by(Order, Diet_Phase) %>%
  summarise(
    mean_total_parameters = mean(Total_Parameters, na.rm = TRUE),
    sd_total_parameters = sd(Total_Parameters, na.rm = TRUE),
    count = n()
  )

# Save descriptive statistics to CSV
write.csv(desc_stats, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/MEAL/NUMBER/COMBINE_NUMBER_desc_stats.csv")

# Step 2: Run ANOVA with grouping information
anova_model <- aov(Total_Parameters ~ Order * Diet_Phase, data = data_long)
anova_results <- Anova(anova_model, type = 2)

# Create ANOVA table with grouping information
anova_table <- data.frame(
  Factor = rownames(anova_results),
  anova_results
)

# Save ANOVA results to CSV
write.csv(anova_table, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/MEAL/NUMBER/COMBINE_NUMBER_anova_results.csv")

# Step 3: Tukey HSD post-hoc test
# Create interaction term for group comparisons
data_long$interaction_term <- interaction(data_long$Order, data_long$Diet_Phase)

# Perform Tukey HSD test on the interaction term
tukey_test <- TukeyHSD(aov(Total_Parameters ~ interaction_term, data = data_long))
tukey_table <- as.data.frame(tukey_test$interaction_term)

# Add group information to the Tukey results
comparison_labels <- str_split_fixed(rownames(tukey_test$interaction_term), ":", 2)
tukey_table$Group1 <- comparison_labels[, 1]
tukey_table$Group2 <- comparison_labels[, 2]

# Save Tukey HSD results to CSV with group labels
write.csv(tukey_table, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/MEAL/NUMBER/COMBINE_NUMBER_tukey_results.csv")

# Step 4: Holm post-hoc test
# Perform pairwise t-tests with Holm correction
holm_test <- pairwise.t.test(data_long$Total_Parameters, data_long$interaction_term, p.adjust.method = "holm")

# Extract and save Holm test results to CSV
holm_table <- as.data.frame(holm_test$p.value)
holm_table$Comparison <- rownames(holm_table)
write.csv(holm_table, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/MEAL/NUMBER/COMBINE_NUMBER_holm_results.csv")

# Print summary of results
print(desc_stats)
print(anova_table)
print(tukey_table)
print(holm_table)




####################################################################################################################
####################################################   MEAL_SIZE COMBINED   ################################################
# Load necessary libraries
library(tidyverse)
library(reshape2)
library(car)
library(multcomp)

# Load the data
data <- read.csv("C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/MEAL/MEAL_SIZE_PR_NR.csv")

# Reshape the data to long format
data_long <- melt(data, id.vars = c("Mouse.ID", "Order"),
                  measure.vars = c("NR", "PR"),
                  variable.name = "Diet_Phase", value.name = "Total_Parameters")

# Step 1: Descriptive statistics
desc_stats <- data_long %>%
  group_by(Order, Diet_Phase) %>%
  summarise(
    mean_total_parameters = mean(Total_Parameters, na.rm = TRUE),
    sd_total_parameters = sd(Total_Parameters, na.rm = TRUE),
    count = n()
  )

# Save descriptive statistics to CSV
write.csv(desc_stats, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/MEAL/SIZE/COMBINED_SIZE_desc_stats.csv")

# Step 2: Run ANOVA with grouping information
anova_model <- aov(Total_Parameters ~ Order * Diet_Phase, data = data_long)
anova_results <- Anova(anova_model, type = 2)

# Create ANOVA table with grouping information
anova_table <- data.frame(
  Factor = rownames(anova_results),
  anova_results
)

# Save ANOVA results to CSV
write.csv(anova_table, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/MEAL/SIZE/COMBINE_SIZE_anova_results.csv")

# Step 3: Tukey HSD post-hoc test
# Create interaction term for group comparisons
data_long$interaction_term <- interaction(data_long$Order, data_long$Diet_Phase)

# Perform Tukey HSD test on the interaction term
tukey_test <- TukeyHSD(aov(Total_Parameters ~ interaction_term, data = data_long))
tukey_table <- as.data.frame(tukey_test$interaction_term)

# Add group information to the Tukey results
comparison_labels <- str_split_fixed(rownames(tukey_test$interaction_term), ":", 2)
tukey_table$Group1 <- comparison_labels[, 1]
tukey_table$Group2 <- comparison_labels[, 2]

# Save Tukey HSD results to CSV with group labels
write.csv(tukey_table, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/MEAL/SIZE/COMBINE_SIZE_tukey_results.csv")

# Step 4: Holm post-hoc test
# Perform pairwise t-tests with Holm correction
holm_test <- pairwise.t.test(data_long$Total_Parameters, data_long$interaction_term, p.adjust.method = "holm")

# Extract and save Holm test results to CSV
holm_table <- as.data.frame(holm_test$p.value)
holm_table$Comparison <- rownames(holm_table)
write.csv(holm_table, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/MEAL/SIZE/COMBINE_SIZE_holm_results.csv")

# Print summary of results
print(desc_stats)
print(anova_table)
print(tukey_table)
print(holm_table)









############################################### MEAL OVER#####################################################################
###########################################  SNACK STARTS###############################################################


####################################################### SNACK FREQ #######################################
####################################################################################################################
# Load necessary libraries
library(tidyverse)
library(reshape2)
library(car)
library(multcomp)

# Load the data
data <- read.csv("C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/SNACK/SNACK_FREQUENCY_PR_NR.csv")

# Reshape the data to long format
data_long <- melt(data, id.vars = c("Mouse.ID", "Sex", "Order"),
                  measure.vars = c("NR", "PR"),
                  variable.name = "Diet_Phase", value.name = "Total_Parameters")

# Step 1: Descriptive statistics
desc_stats <- data_long %>%
  group_by(Sex, Order, Diet_Phase) %>%
  summarise(
    mean_total_parameters = mean(Total_Parameters, na.rm = TRUE),
    sd_total_parameters = sd(Total_Parameters, na.rm = TRUE),
    count = n()
  )

# Save descriptive statistics to CSV
write.csv(desc_stats, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/SNACK/FREQ/SNACK_FREQ_desc_stats.csv")

# Step 2: Run ANOVA with grouping information
anova_model <- aov(Total_Parameters ~ Sex * Order * Diet_Phase, data = data_long)
anova_results <- Anova(anova_model, type = 2)

# Create ANOVA table with grouping information
anova_table <- data.frame(
  Factor = rownames(anova_results),
  anova_results
)

# Save ANOVA results to CSV
write.csv(anova_table, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/SNACK/FREQ/SNACK_FREQ_anova_results.csv")

# Step 3: Tukey HSD post-hoc test
# Create interaction term for group comparisons
data_long$interaction_term <- interaction(data_long$Sex, data_long$Order, data_long$Diet_Phase)

# Perform Tukey HSD test on the interaction term
tukey_test <- TukeyHSD(aov(Total_Parameters ~ interaction_term, data = data_long))
tukey_table <- as.data.frame(tukey_test$interaction_term)

# Add group information to the Tukey results
comparison_labels <- str_split_fixed(rownames(tukey_test$interaction_term), ":", 2)
tukey_table$Group1 <- comparison_labels[, 1]
tukey_table$Group2 <- comparison_labels[, 2]

# Save Tukey HSD results to CSV with group labels
write.csv(tukey_table, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/SNACK/FREQ/SNACK_FREQ_tukey_results.csv")

# Step 4: Holm post-hoc test
# Perform pairwise t-tests with Holm correction
holm_test <- pairwise.t.test(data_long$Total_Parameters, data_long$interaction_term, p.adjust.method = "holm")

# Extract and save Holm test results to CSV
holm_table <- as.data.frame(holm_test$p.value)
holm_table$Comparison <- rownames(holm_table)
write.csv(holm_table, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/SNACK/FREQ/SNACK_FREQ_holm_results.csv")

# Print summary of results
print(desc_stats)
print(anova_table)
print(tukey_table)
print(holm_table)





####################################################### SNACK NUMBER #######################################
####################################################################################################################
# Load necessary libraries
library(tidyverse)
library(reshape2)
library(car)
library(multcomp)

# Load the data
data <- read.csv("C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/SNACK/SNACK_NUMBER_PR_NR.csv")

# Reshape the data to long format
data_long <- melt(data, id.vars = c("Mouse.ID", "Sex", "Order"),
                  measure.vars = c("NR", "PR"),
                  variable.name = "Diet_Phase", value.name = "Total_Parameters")

# Step 1: Descriptive statistics
desc_stats <- data_long %>%
  group_by(Sex, Order, Diet_Phase) %>%
  summarise(
    mean_total_parameters = mean(Total_Parameters, na.rm = TRUE),
    sd_total_parameters = sd(Total_Parameters, na.rm = TRUE),
    count = n()
  )

# Save descriptive statistics to CSV
write.csv(desc_stats, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/SNACK/NUMBER/SNACK_NUMBER_desc_stats.csv")

# Step 2: Run ANOVA with grouping information
anova_model <- aov(Total_Parameters ~ Sex * Order * Diet_Phase, data = data_long)
anova_results <- Anova(anova_model, type = 2)

# Create ANOVA table with grouping information
anova_table <- data.frame(
  Factor = rownames(anova_results),
  anova_results
)

# Save ANOVA results to CSV
write.csv(anova_table, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/SNACK/NUMBER/SNACK_NUMBER_anova_results.csv")

# Step 3: Tukey HSD post-hoc test
# Create interaction term for group comparisons
data_long$interaction_term <- interaction(data_long$Sex, data_long$Order, data_long$Diet_Phase)

# Perform Tukey HSD test on the interaction term
tukey_test <- TukeyHSD(aov(Total_Parameters ~ interaction_term, data = data_long))
tukey_table <- as.data.frame(tukey_test$interaction_term)

# Add group information to the Tukey results
comparison_labels <- str_split_fixed(rownames(tukey_test$interaction_term), ":", 2)
tukey_table$Group1 <- comparison_labels[, 1]
tukey_table$Group2 <- comparison_labels[, 2]

# Save Tukey HSD results to CSV with group labels
write.csv(tukey_table, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/SNACK/NUMBER/SNACK_NUMBER_tukey_results.csv")

# Step 4: Holm post-hoc test
# Perform pairwise t-tests with Holm correction
holm_test <- pairwise.t.test(data_long$Total_Parameters, data_long$interaction_term, p.adjust.method = "holm")

# Extract and save Holm test results to CSV
holm_table <- as.data.frame(holm_test$p.value)
holm_table$Comparison <- rownames(holm_table)
write.csv(holm_table, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/SNACK/NUMBER/SNACK_NUMBER_holm_results.csv")

# Print summary of results
print(desc_stats)
print(anova_table)
print(tukey_table)
print(holm_table)




#######################################BELOW COMBINED######################## BELOW COMBINED#########################BELOW COMBINED###################################
###################### There is no significant sex difference between groups, so POOLED SNACK DATA BELOW #####################################
####################################################### SNACK FREQ COMBINED #######################################
####################################################################################################################


####################################################### SNACK FREQ COMBINED #######################################
####################################################################################################################
# Load necessary libraries
library(tidyverse)
library(reshape2)
library(car)
library(multcomp)

# Load the data
data <- read.csv("C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/SNACK/SNACK_FREQUENCY_PR_NR.csv")

# Reshape the data to long format
data_long <- melt(data, id.vars = c("Mouse.ID", "Order"),
                  measure.vars = c("NR", "PR"),
                  variable.name = "Diet_Phase", value.name = "Total_Parameters")

# Step 1: Descriptive statistics
desc_stats <- data_long %>%
  group_by(Order, Diet_Phase) %>%
  summarise(
    mean_total_parameters = mean(Total_Parameters, na.rm = TRUE),
    sd_total_parameters = sd(Total_Parameters, na.rm = TRUE),
    count = n()
  )

# Save descriptive statistics to CSV
write.csv(desc_stats, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/SNACK/FREQ/COMBINE_SNACK_FREQ_desc_stats.csv")

# Step 2: Run ANOVA with grouping information
anova_model <- aov(Total_Parameters ~ Order * Diet_Phase, data = data_long)
anova_results <- Anova(anova_model, type = 2)

# Create ANOVA table with grouping information
anova_table <- data.frame(
  Factor = rownames(anova_results),
  anova_results
)

# Save ANOVA results to CSV
write.csv(anova_table, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/SNACK/FREQ/COMBINE_SNACK_FREQ_anova_results.csv")

# Step 3: Tukey HSD post-hoc test
# Create interaction term for group comparisons
data_long$interaction_term <- interaction(data_long$Order, data_long$Diet_Phase)

# Perform Tukey HSD test on the interaction term
tukey_test <- TukeyHSD(aov(Total_Parameters ~ interaction_term, data = data_long))
tukey_table <- as.data.frame(tukey_test$interaction_term)

# Add group information to the Tukey results
comparison_labels <- str_split_fixed(rownames(tukey_test$interaction_term), ":", 2)
tukey_table$Group1 <- comparison_labels[, 1]
tukey_table$Group2 <- comparison_labels[, 2]

# Save Tukey HSD results to CSV with group labels
write.csv(tukey_table, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/SNACK/FREQ/COMBINE_SNACK_FREQ_tukey_results.csv")

# Step 4: Holm post-hoc test
# Perform pairwise t-tests with Holm correction
holm_test <- pairwise.t.test(data_long$Total_Parameters, data_long$interaction_term, p.adjust.method = "holm")

# Extract and save Holm test results to CSV
holm_table <- as.data.frame(holm_test$p.value)
holm_table$Comparison <- rownames(holm_table)
write.csv(holm_table, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/SNACK/FREQ/COMBINE_SNACK_FREQ_holm_results.csv")

# Print summary of results
print(desc_stats)
print(anova_table)
print(tukey_table)
print(holm_table)




####################################################### SNACK NUMBER COMBINE #######################################
####################################################################################################################
# Load necessary libraries
library(tidyverse)
library(reshape2)
library(car)
library(multcomp)

# Load the data
data <- read.csv("C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/SNACK/SNACK_NUMBER_PR_NR.csv")

# Reshape the data to long format
data_long <- melt(data, id.vars = c("Mouse.ID", "Order"),
                  measure.vars = c("NR", "PR"),
                  variable.name = "Diet_Phase", value.name = "Total_Parameters")

# Step 1: Descriptive statistics
desc_stats <- data_long %>%
  group_by(Order, Diet_Phase) %>%
  summarise(
    mean_total_parameters = mean(Total_Parameters, na.rm = TRUE),
    sd_total_parameters = sd(Total_Parameters, na.rm = TRUE),
    count = n()
  )

# Save descriptive statistics to CSV
write.csv(desc_stats, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/SNACK/NUMBER/COMBINE_SNACK_NUMBER_desc_stats.csv")

# Step 2: Run ANOVA with grouping information
anova_model <- aov(Total_Parameters ~ Order * Diet_Phase, data = data_long)
anova_results <- Anova(anova_model, type = 2)

# Create ANOVA table with grouping information
anova_table <- data.frame(
  Factor = rownames(anova_results),
  anova_results
)

# Save ANOVA results to CSV
write.csv(anova_table, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/SNACK/NUMBER/COMBINE_SNACK_NUMBER_anova_results.csv")

# Step 3: Tukey HSD post-hoc test
# Create interaction term for group comparisons
data_long$interaction_term <- interaction(data_long$Order, data_long$Diet_Phase)

# Perform Tukey HSD test on the interaction term
tukey_test <- TukeyHSD(aov(Total_Parameters ~ interaction_term, data = data_long))
tukey_table <- as.data.frame(tukey_test$interaction_term)

# Add group information to the Tukey results
comparison_labels <- str_split_fixed(rownames(tukey_test$interaction_term), ":", 2)
tukey_table$Group1 <- comparison_labels[, 1]
tukey_table$Group2 <- comparison_labels[, 2]

# Save Tukey HSD results to CSV with group labels
write.csv(tukey_table, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/SNACK/NUMBER/COMBINE_SNACK_NUMBER_tukey_results.csv")

# Step 4: Holm post-hoc test
# Perform pairwise t-tests with Holm correction
holm_test <- pairwise.t.test(data_long$Total_Parameters, data_long$interaction_term, p.adjust.method = "holm")

# Extract and save Holm test results to CSV
holm_table <- as.data.frame(holm_test$p.value)
holm_table$Comparison <- rownames(holm_table)
write.csv(holm_table, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/SNACK/NUMBER/COMBINE_SNACK_NUMBER_holm_results.csv")

# Print summary of results
print(desc_stats)
print(anova_table)
print(tukey_table)
print(holm_table)





############################################### SNACK OVER#####################################################################
###########################################  FEAST STARTS###############################################################


####################################################### FEAST FREQ #######################################
####################################################################################################################
# Load necessary libraries
library(tidyverse)
library(reshape2)
library(car)
library(multcomp)

# Load the data
data <- read.csv("C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/FEAST/MEGA_MEAL_FREQUENCY_PR_NR.csv")

# Reshape the data to long format
data_long <- melt(data, id.vars = c("Mouse.ID", "Sex", "Order"),
                  measure.vars = c("NR", "PR"),
                  variable.name = "Diet_Phase", value.name = "Total_Parameters")

# Step 1: Descriptive statistics
desc_stats <- data_long %>%
  group_by(Sex, Order, Diet_Phase) %>%
  summarise(
    mean_total_parameters = mean(Total_Parameters, na.rm = TRUE),
    sd_total_parameters = sd(Total_Parameters, na.rm = TRUE),
    count = n()
  )

# Save descriptive statistics to CSV
write.csv(desc_stats, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/FEAST/FREQ/FEAST_FREQ_desc_stats.csv")

# Step 2: Run ANOVA with grouping information
anova_model <- aov(Total_Parameters ~ Sex * Order * Diet_Phase, data = data_long)
anova_results <- Anova(anova_model, type = 2)

# Create ANOVA table with grouping information
anova_table <- data.frame(
  Factor = rownames(anova_results),
  anova_results
)

# Save ANOVA results to CSV
write.csv(anova_table, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/FEAST/FREQ/FEAST_FREQ_anova_results.csv")

# Step 3: Tukey HSD post-hoc test
# Create interaction term for group comparisons
data_long$interaction_term <- interaction(data_long$Sex, data_long$Order, data_long$Diet_Phase)

# Perform Tukey HSD test on the interaction term
tukey_test <- TukeyHSD(aov(Total_Parameters ~ interaction_term, data = data_long))
tukey_table <- as.data.frame(tukey_test$interaction_term)

# Add group information to the Tukey results
comparison_labels <- str_split_fixed(rownames(tukey_test$interaction_term), ":", 2)
tukey_table$Group1 <- comparison_labels[, 1]
tukey_table$Group2 <- comparison_labels[, 2]

# Save Tukey HSD results to CSV with group labels
write.csv(tukey_table, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/FEAST/FREQ/FEAST_FREQ_tukey_results.csv")

# Step 4: Holm post-hoc test
# Perform pairwise t-tests with Holm correction
holm_test <- pairwise.t.test(data_long$Total_Parameters, data_long$interaction_term, p.adjust.method = "holm")

# Extract and save Holm test results to CSV
holm_table <- as.data.frame(holm_test$p.value)
holm_table$Comparison <- rownames(holm_table)
write.csv(holm_table, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/FEAST/FREQ/FEAST_FREQ_holm_results.csv")

# Print summary of results
print(desc_stats)
print(anova_table)
print(tukey_table)
print(holm_table)


####################################################################################################################
####################################################   FEAST NUMBER   ################################################
# Load necessary libraries
library(tidyverse)
library(reshape2)
library(car)
library(multcomp)

# Load the data
data <- read.csv("C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/FEAST/MEGA_MEAL_NUMBER_PR_NR.csv")

# Reshape the data to long format
data_long <- melt(data, id.vars = c("Mouse.ID", "Sex", "Order"),
                  measure.vars = c("NR", "PR"),
                  variable.name = "Diet_Phase", value.name = "Total_Parameters")

# Step 1: Descriptive statistics
desc_stats <- data_long %>%
  group_by(Sex, Order, Diet_Phase) %>%
  summarise(
    mean_total_parameters = mean(Total_Parameters, na.rm = TRUE),
    sd_total_parameters = sd(Total_Parameters, na.rm = TRUE),
    count = n()
  )

# Save descriptive statistics to CSV
write.csv(desc_stats, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/FEAST/NUMBER/FEAST_NUMBER_desc_stats.csv")

# Step 2: Run ANOVA with grouping information
anova_model <- aov(Total_Parameters ~ Sex * Order * Diet_Phase, data = data_long)
anova_results <- Anova(anova_model, type = 2)

# Create ANOVA table with grouping information
anova_table <- data.frame(
  Factor = rownames(anova_results),
  anova_results
)

# Save ANOVA results to CSV
write.csv(anova_table, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/FEAST/NUMBER/FEAST_NUMBER_anova_results.csv")

# Step 3: Tukey HSD post-hoc test
# Create interaction term for group comparisons
data_long$interaction_term <- interaction(data_long$Sex, data_long$Order, data_long$Diet_Phase)

# Perform Tukey HSD test on the interaction term
tukey_test <- TukeyHSD(aov(Total_Parameters ~ interaction_term, data = data_long))
tukey_table <- as.data.frame(tukey_test$interaction_term)

# Add group information to the Tukey results
comparison_labels <- str_split_fixed(rownames(tukey_test$interaction_term), ":", 2)
tukey_table$Group1 <- comparison_labels[, 1]
tukey_table$Group2 <- comparison_labels[, 2]

# Save Tukey HSD results to CSV with group labels
write.csv(tukey_table, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/FEAST/NUMBER/FEAST_NUMBER_tukey_results.csv")

# Step 4: Holm post-hoc test
# Perform pairwise t-tests with Holm correction
holm_test <- pairwise.t.test(data_long$Total_Parameters, data_long$interaction_term, p.adjust.method = "holm")

# Extract and save Holm test results to CSV
holm_table <- as.data.frame(holm_test$p.value)
holm_table$Comparison <- rownames(holm_table)
write.csv(holm_table, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/FEAST/NUMBER/FEAST_NUMBER_holm_results.csv")

# Print summary of results
print(desc_stats)
print(anova_table)
print(tukey_table)
print(holm_table)



####################################################################################################################
####################################################   FEAST SIZE   ################################################
# Load necessary libraries
library(tidyverse)
library(reshape2)
library(car)
library(multcomp)

# Load the data
data <- read.csv("C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/FEAST/MEGA_MEAL_SIZE_PR_NR.csv")

# Reshape the data to long format
data_long <- melt(data, id.vars = c("Mouse.ID", "Sex", "Order"),
                  measure.vars = c("NR", "PR"),
                  variable.name = "Diet_Phase", value.name = "Total_Parameters")

# Step 1: Descriptive statistics
desc_stats <- data_long %>%
  group_by(Sex, Order, Diet_Phase) %>%
  summarise(
    mean_total_parameters = mean(Total_Parameters, na.rm = TRUE),
    sd_total_parameters = sd(Total_Parameters, na.rm = TRUE),
    count = n()
  )

# Save descriptive statistics to CSV
write.csv(desc_stats, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/FEAST/SIZE/FEAST_SIZE_desc_stats.csv")

# Step 2: Run ANOVA with grouping information
anova_model <- aov(Total_Parameters ~ Sex * Order * Diet_Phase, data = data_long)
anova_results <- Anova(anova_model, type = 2)

# Create ANOVA table with grouping information
anova_table <- data.frame(
  Factor = rownames(anova_results),
  anova_results
)

# Save ANOVA results to CSV
write.csv(anova_table, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/FEAST/SIZE/FEAST_SIZE_anova_results.csv")

# Step 3: Tukey HSD post-hoc test
# Create interaction term for group comparisons
data_long$interaction_term <- interaction(data_long$Sex, data_long$Order, data_long$Diet_Phase)

# Perform Tukey HSD test on the interaction term
tukey_test <- TukeyHSD(aov(Total_Parameters ~ interaction_term, data = data_long))
tukey_table <- as.data.frame(tukey_test$interaction_term)

# Add group information to the Tukey results
comparison_labels <- str_split_fixed(rownames(tukey_test$interaction_term), ":", 2)
tukey_table$Group1 <- comparison_labels[, 1]
tukey_table$Group2 <- comparison_labels[, 2]

# Save Tukey HSD results to CSV with group labels
write.csv(tukey_table, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/FEAST/SIZE/FEAST_SIZE_tukey_results.csv")

# Step 4: Holm post-hoc test
# Perform pairwise t-tests with Holm correction
holm_test <- pairwise.t.test(data_long$Total_Parameters, data_long$interaction_term, p.adjust.method = "holm")

# Extract and save Holm test results to CSV
holm_table <- as.data.frame(holm_test$p.value)
holm_table$Comparison <- rownames(holm_table)
write.csv(holm_table, "C:/Users/hta031/Github/FEDProtein/results/FIVE/SCATTER_PLOTS/FEAST/SIZE/FEAST_SIZE_holm_results.csv")

# Print summary of results
print(desc_stats)
print(anova_table)
print(tukey_table)
print(holm_table)
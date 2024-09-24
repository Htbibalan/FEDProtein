# Load necessary libraries
library(dplyr)
library(car)   # For ANOVA
library(multcomp) # For post-hoc Holm test

# Load the dataset (update the path if needed)
data <- read.csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\Corrected_Average_Hourly_Pellet_Intake_Data.csv")

# Ensure 'hour', 'order', and 'sex' are treated as factors
data$hour <- as.factor(data$hour)
data$order <- as.factor(data$order)
data$sex <- as.factor(data$sex)

# Convert 'sex' from numeric to 'M' and 'F' if needed
data$sex <- recode_factor(data$sex, `1` = "M", `2` = "F")

# Grouping based on 'order' and 'sex'
groups <- unique(data[, c("order", "sex")])

# Loop through each combination of Order and Sex for both NR and PR phases
for (i in 1:nrow(groups)) {
  # Filter the data for the current group (NR phase)
  group_data_nr <- subset(data, order == groups$order[i] & sex == groups$sex[i])
  
  # Filter the data for the current group (PR phase)
  group_data_pr <- subset(data, order == groups$order[i] & sex == groups$sex[i])

  # NR Phase Analysis
  if (nrow(group_data_nr) > 0) {
    # Run ANOVA for the NR phase
    anova_nr <- aov(nr_hourly_pellet_intake ~ hour, data = group_data_nr)
    
    # Pairwise t-test with Holm's correction
    posthoc_nr <- pairwise.t.test(group_data_nr$nr_hourly_pellet_intake, group_data_nr$hour, p.adjust.method = "holm")
    
    # Save the ANOVA summary
    anova_nr_summary <- summary(anova_nr)
    write.csv(as.data.frame(anova_nr_summary[[1]]), paste0("C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\ANOVA_NR_Group_", groups$order[i], "_", groups$sex[i], ".csv"))
    
    # Save the post-hoc test results
    posthoc_nr_pvalues <- as.data.frame(posthoc_nr$p.value)
    write.csv(posthoc_nr_pvalues, paste0("C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\Posthoc_NR_Group_", groups$order[i], "_", groups$sex[i], ".csv"))
  }







  ######################################################################################################
  #####################Compare between groups, order, sex###########################
# Load necessary libraries
# Load necessary libraries
library(dplyr)
library(car)       # For ANOVA
library(multcomp)  # For post-hoc Holm test

# Load the dataset (update with your path)
data <- read.csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\Corrected_Average_Hourly_Pellet_Intake_Data.csv")

# Ensure 'hour', 'order', and 'sex' are treated as factors
data$hour <- as.factor(data$hour)
data$order <- as.factor(data$order)
data$sex <- as.factor(data$sex)

# Step 1: Aggregate data to calculate the average pellet intake per hour across 7 days for each group
group_avg_hourly <- data %>%
  group_by(order, sex, hour) %>%
  summarize(
    avg_nr_intake = mean(nr_hourly_pellet_intake, na.rm = TRUE),
    avg_pr_intake = mean(pr_hourly_pellet_intake, na.rm = TRUE),
    .groups = 'drop'
  )

# Step 2: Subset the data for each group (NR and PR)
# Subset for NR and PR for comparison between Order 1 vs Order 2
nr_data <- subset(group_avg_hourly, !is.na(avg_nr_intake))  # Only NR data
pr_data <- subset(group_avg_hourly, !is.na(avg_pr_intake))  # Only PR data

# Step 3: ANOVA for Order 1 vs Order 2 at each hour (NR phase) for Male and Female separately

# Run ANOVA for NR phase (Male and Female separately)
anova_results_nr <- aov(avg_nr_intake ~ order * hour, data = nr_data)
anova_results_pr <- aov(avg_pr_intake ~ order * hour, data = pr_data)

# Save ANOVA summary to CSV
write.csv(as.data.frame(summary(anova_results_nr)[[1]]), 
          "C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\ANOVA_NR_Results.csv")
write.csv(as.data.frame(summary(anova_results_pr)[[1]]), 
          "C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\ANOVA_PR_Results.csv")

# Step 4: Post-hoc test (Tukey HSD) to compare hourly differences between Order 1 and Order 2

# Tukey HSD for NR phase
if (anova_results_nr$df.residual > 0) {
  posthoc_nr <- TukeyHSD(anova_results_nr)
  write.table(posthoc_nr, file = "C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\Posthoc_NR_Results.txt", sep = "\t")
}

# Tukey HSD for PR phase
if (anova_results_pr$df.residual > 0) {
  posthoc_pr <- TukeyHSD(anova_results_pr)
  write.table(posthoc_pr, file = "C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\Posthoc_PR_Results.txt", sep = "\t")
}





#################################################################
##code below works in measuring posthoc and ANOVA but does not output Male and Female data

# Load necessary libraries
library(dplyr)
library(car)       # For ANOVA
library(multcomp)  # For post-hoc Holm test

# Load the dataset (update with your path)
data <- read.csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\Corrected_Average_Hourly_Pellet_Intake_Data.csv")

# Ensure 'hour', 'order', and 'sex' are treated as factors
data$hour <- as.factor(data$hour)
data$order <- as.factor(data$order)
data$sex <- as.factor(data$sex)

# Step 1: Aggregate data to calculate the average pellet intake per hour across 7 days for each group
group_avg_hourly <- data %>%
  group_by(order, sex, hour) %>%
  summarize(
    avg_nr_intake = mean(nr_hourly_pellet_intake, na.rm = TRUE),
    avg_pr_intake = mean(pr_hourly_pellet_intake, na.rm = TRUE),
    .groups = 'drop'
  )

# Step 2: Subset the data for each group (NR and PR)
# Subset for NR and PR for comparison between Order 1 vs Order 2
nr_data <- subset(group_avg_hourly, !is.na(avg_nr_intake))  # Only NR data
pr_data <- subset(group_avg_hourly, !is.na(avg_pr_intake))  # Only PR data

# Step 3: ANOVA for Order 1 vs Order 2 at each hour (NR phase) for Male and Female separately

# Run ANOVA for NR phase (Male and Female separately)
anova_results_nr <- aov(avg_nr_intake ~ order * hour, data = nr_data)
anova_results_pr <- aov(avg_pr_intake ~ order * hour, data = pr_data)

# Save ANOVA summary to CSV
write.csv(as.data.frame(summary(anova_results_nr)[[1]]), 
          "C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\ANOVA_NR_Results.csv")
write.csv(as.data.frame(summary(anova_results_pr)[[1]]), 
          "C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\ANOVA_PR_Results.csv")

# Step 4: Post-hoc test (Tukey HSD) to compare hourly differences between Order 1 and Order 2

# Tukey HSD for NR phase
if (anova_results_nr$df.residual > 0) {
  posthoc_nr <- TukeyHSD(anova_results_nr)
  capture.output(posthoc_nr, file = "C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\Posthoc_NR_Results.txt")
}

# Tukey HSD for PR phase
if (anova_results_pr$df.residual > 0) {
  posthoc_pr <- TukeyHSD(anova_results_pr)
  capture.output(posthoc_pr, file = "C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\Posthoc_PR_Results.txt")
}






####################################################################################




# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(car)
library(multcomp)

# Load the data from your subfolder
male_order_1 <- read_excel("C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\MALE_ORDER_1.xlsx", sheet = "Sheet1")
male_order_2 <- read_excel("C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\MALE_ORDER_2.xlsx", sheet = "Sheet1")
female_order_1 <- read_excel("C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\FEMALE_ORDER_1.xlsx", sheet = "Sheet1")
female_order_2 <- read_excel("C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\FEMALE_ORDER_2.xlsx", sheet = "Sheet1")

# Ensure pellet intake columns are numeric
male_order_1 <- male_order_1 %>%
  mutate(pr_hourly_pellet_intake = as.numeric(pr_hourly_pellet_intake),
         nr_hourly_pellet_intake = as.numeric(nr_hourly_pellet_intake))
male_order_2 <- male_order_2 %>%
  mutate(pr_hourly_pellet_intake = as.numeric(pr_hourly_pellet_intake),
         nr_hourly_pellet_intake = as.numeric(nr_hourly_pellet_intake))
female_order_1 <- female_order_1 %>%
  mutate(pr_hourly_pellet_intake = as.numeric(pr_hourly_pellet_intake),
         nr_hourly_pellet_intake = as.numeric(nr_hourly_pellet_intake))
female_order_2 <- female_order_2 %>%
  mutate(pr_hourly_pellet_intake = as.numeric(pr_hourly_pellet_intake),
         nr_hourly_pellet_intake = as.numeric(nr_hourly_pellet_intake))

# Add group identifiers
male_order_1$group <- "Male_Order_1"
male_order_2$group <- "Male_Order_2"
female_order_1$group <- "Female_Order_1"
female_order_2$group <- "Female_Order_2"

# Combine the datasets into one
combined_data <- bind_rows(male_order_1, male_order_2, female_order_1, female_order_2)

# Group by sex and order, and calculate average pellet intake per hour across days
average_data <- combined_data %>%
  group_by(group, sex, hour) %>%
  summarise(mean_pr_pellets = mean(pr_hourly_pellet_intake, na.rm = TRUE),
            sd_pr_pellets = sd(pr_hourly_pellet_intake, na.rm = TRUE),
            mean_nr_pellets = mean(nr_hourly_pellet_intake, na.rm = TRUE),
            sd_nr_pellets = sd(nr_hourly_pellet_intake, na.rm = TRUE))

# Save descriptive statistics to CSV
write.csv(average_data, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\descriptive_stats.csv", row.names = FALSE)

# --- ANOVA for PR and NR phases ---
anova_pr <- aov(pr_hourly_pellet_intake ~ group * hour, data = combined_data)
anova_nr <- aov(nr_hourly_pellet_intake ~ group * hour, data = combined_data)

# Save ANOVA results
capture.output(summary(anova_pr), file = "C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\anova_pr_summary.txt")
capture.output(summary(anova_nr), file = "C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\anova_nr_summary.txt")

# --- Posthoc Holm Test for PR and NR phases ---
posthoc_pr <- glht(anova_pr, linfct = mcp(group = "Tukey"))
posthoc_nr <- glht(anova_nr, linfct = mcp(group = "Tukey"))

# Save posthoc test results
capture.output(summary(posthoc_pr, test = adjusted("holm")), file = "C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\posthoc_pr_summary.txt")
capture.output(summary(posthoc_nr, test = adjusted("holm")), file = "C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\posthoc_nr_summary.txt")

# --- Pairwise Comparisons for PR and NR phases by Sex and Order ---

# Filter data by sex and order
male_order_1_data <- average_data %>% filter(sex == "M" & group == "Male_Order_1")
male_order_2_data <- average_data %>% filter(sex == "M" & group == "Male_Order_2")
female_order_1_data <- average_data %>% filter(sex == "F" & group == "Female_Order_1")
female_order_2_data <- average_data %>% filter(sex == "F" & group == "Female_Order_2")

# --- Pairwise Comparisons for PR Phase ---

# Male Order 1 PR
pairwise_hour_pr_male_order_1 <- pairwise.t.test(male_order_1_data$mean_pr_pellets, male_order_1_data$hour, p.adjust.method = "holm")

# Male Order 2 PR
pairwise_hour_pr_male_order_2 <- pairwise.t.test(male_order_2_data$mean_pr_pellets, male_order_2_data$hour, p.adjust.method = "holm")

# Female Order 1 PR
pairwise_hour_pr_female_order_1 <- pairwise.t.test(female_order_1_data$mean_pr_pellets, female_order_1_data$hour, p.adjust.method = "holm")

# Female Order 2 PR
pairwise_hour_pr_female_order_2 <- pairwise.t.test(female_order_2_data$mean_pr_pellets, female_order_2_data$hour, p.adjust.method = "holm")

# --- Pairwise Comparisons for NR Phase ---

# Male Order 1 NR
pairwise_hour_nr_male_order_1 <- pairwise.t.test(male_order_1_data$mean_nr_pellets, male_order_1_data$hour, p.adjust.method = "holm")

# Male Order 2 NR
pairwise_hour_nr_male_order_2 <- pairwise.t.test(male_order_2_data$mean_nr_pellets, male_order_2_data$hour, p.adjust.method = "holm")

# Female Order 1 NR
pairwise_hour_nr_female_order_1 <- pairwise.t.test(female_order_1_data$mean_nr_pellets, female_order_1_data$hour, p.adjust.method = "holm")

# Female Order 2 NR
pairwise_hour_nr_female_order_2 <- pairwise.t.test(female_order_2_data$mean_nr_pellets, female_order_2_data$hour, p.adjust.method = "holm")

# --- Save Pairwise Comparisons for Each Group and Phase ---

# PR Phase
capture.output(pairwise_hour_pr_male_order_1, file = "C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\pairwise_hour_pr_comparisons_male_order_1.txt")
capture.output(pairwise_hour_pr_male_order_2, file = "C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\pairwise_hour_pr_comparisons_male_order_2.txt")
capture.output(pairwise_hour_pr_female_order_1, file = "C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\pairwise_hour_pr_comparisons_female_order_1.txt")
capture.output(pairwise_hour_pr_female_order_2, file = "C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\pairwise_hour_pr_comparisons_female_order_2.txt")

# NR Phase
capture.output(pairwise_hour_nr_male_order_1, file = "C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\pairwise_hour_nr_comparisons_male_order_1.txt")
capture.output(pairwise_hour_nr_male_order_2, file = "C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\pairwise_hour_nr_comparisons_male_order_2.txt")
capture.output(pairwise_hour_nr_female_order_1, file = "C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\pairwise_hour_nr_comparisons_female_order_1.txt")
capture.output(pairwise_hour_nr_female_order_2, file = "C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\pairwise_hour_nr_comparisons_female_order")



##############code below works however fails to compare hour by hour between groups######################
# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(car)
library(multcomp)

# Load the data from your subfolder
male_order_1 <- read_excel("C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\MALE_ORDER_1.xlsx", sheet = "Sheet1")
male_order_2 <- read_excel("C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\MALE_ORDER_2.xlsx", sheet = "Sheet1")
female_order_1 <- read_excel("C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\FEMALE_ORDER_1.xlsx", sheet = "Sheet1")
female_order_2 <- read_excel("C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\FEMALE_ORDER_2.xlsx", sheet = "Sheet1")

# Ensure pellet intake columns are numeric
male_order_1 <- male_order_1 %>%
  mutate(pr_hourly_pellet_intake = as.numeric(pr_hourly_pellet_intake),
         nr_hourly_pellet_intake = as.numeric(nr_hourly_pellet_intake))
male_order_2 <- male_order_2 %>%
  mutate(pr_hourly_pellet_intake = as.numeric(pr_hourly_pellet_intake),
         nr_hourly_pellet_intake = as.numeric(nr_hourly_pellet_intake))
female_order_1 <- female_order_1 %>%
  mutate(pr_hourly_pellet_intake = as.numeric(pr_hourly_pellet_intake),
         nr_hourly_pellet_intake = as.numeric(nr_hourly_pellet_intake))
female_order_2 <- female_order_2 %>%
  mutate(pr_hourly_pellet_intake = as.numeric(pr_hourly_pellet_intake),
         nr_hourly_pellet_intake = as.numeric(nr_hourly_pellet_intake))

# Add group identifiers
male_order_1$group <- "Male_Order_1"
male_order_2$group <- "Male_Order_2"
female_order_1$group <- "Female_Order_1"
female_order_2$group <- "Female_Order_2"

# Combine the datasets into one
combined_data <- bind_rows(male_order_1, male_order_2, female_order_1, female_order_2)

# --- ANOVA for PR and NR phases ---
# Separate ANOVA for PR and NR, comparing group by hour
anova_pr <- aov(pr_hourly_pellet_intake ~ group * hour, data = combined_data)
anova_nr <- aov(nr_hourly_pellet_intake ~ group * hour, data = combined_data)

# Save ANOVA results
capture.output(summary(anova_pr), file = "C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\anova_pr_summary.txt")
capture.output(summary(anova_nr), file = "C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\anova_nr_summary.txt")

# --- Posthoc Tukey Test for PR and NR phases ---
posthoc_pr <- TukeyHSD(anova_pr)
posthoc_nr <- TukeyHSD(anova_nr)

# Save posthoc test results
capture.output(posthoc_pr, file = "C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\posthoc_pr_tukey.txt")
capture.output(posthoc_nr, file = "C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\posthoc_nr_tukey.txt")

# --- Pairwise Comparisons: Compare groups with each other ---
# Compare groups (e.g., Male_Order_1 vs Female_Order_1) for PR and NR phases using t-tests

# PR Phase Pairwise Comparisons
pairwise_pr <- pairwise.t.test(combined_data$pr_hourly_pellet_intake, combined_data$group, p.adjust.method = "holm")

# NR Phase Pairwise Comparisons
pairwise_nr <- pairwise.t.test(combined_data$nr_hourly_pellet_intake, combined_data$group, p.adjust.method = "holm")

# Save Pairwise Comparisons
capture.output(pairwise_pr, file = "C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\pairwise_pr_group_comparisons.txt")
capture.output(pairwise_nr, file = "C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\pairwise_nr_group_comparisons.txt")





####################trying to calculate houyr by hour comparisons between groups####################

# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(car)
library(multcomp)

# Load the data from your subfolder
male_order_1 <- read_excel("C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\MALE_ORDER_1.xlsx", sheet = "Sheet1")
male_order_2 <- read_excel("C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\MALE_ORDER_2.xlsx", sheet = "Sheet1")
female_order_1 <- read_excel("C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\FEMALE_ORDER_1.xlsx", sheet = "Sheet1")
female_order_2 <- read_excel("C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\FEMALE_ORDER_2.xlsx", sheet = "Sheet1")

# Ensure pellet intake columns are numeric
male_order_1 <- male_order_1 %>%
  mutate(pr_hourly_pellet_intake = as.numeric(pr_hourly_pellet_intake),
         nr_hourly_pellet_intake = as.numeric(nr_hourly_pellet_intake))
male_order_2 <- male_order_2 %>%
  mutate(pr_hourly_pellet_intake = as.numeric(pr_hourly_pellet_intake),
         nr_hourly_pellet_intake = as.numeric(nr_hourly_pellet_intake))
female_order_1 <- female_order_1 %>%
  mutate(pr_hourly_pellet_intake = as.numeric(pr_hourly_pellet_intake),
         nr_hourly_pellet_intake = as.numeric(nr_hourly_pellet_intake))
female_order_2 <- female_order_2 %>%
  mutate(pr_hourly_pellet_intake = as.numeric(pr_hourly_pellet_intake),
         nr_hourly_pellet_intake = as.numeric(nr_hourly_pellet_intake))

# Add group identifiers
male_order_1$group <- "Male_Order_1"
male_order_2$group <- "Male_Order_2"
female_order_1$group <- "Female_Order_1"
female_order_2$group <- "Female_Order_2"

# Combine the datasets into one
combined_data <- bind_rows(male_order_1, male_order_2, female_order_1, female_order_2)

# --- Descriptive statistics by group and hour ---
descriptive_stats <- combined_data %>%
  group_by(group, hour) %>%
  summarise(
    mean_pr_pellets = mean(pr_hourly_pellet_intake, na.rm = TRUE),
    sd_pr_pellets = sd(pr_hourly_pellet_intake, na.rm = TRUE),
    mean_nr_pellets = mean(nr_hourly_pellet_intake, na.rm = TRUE),
    sd_nr_pellets = sd(nr_hourly_pellet_intake, na.rm = TRUE)
  )

# Save descriptive statistics to CSV
write.csv(descriptive_stats, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\descriptive_stats_by_group_hour.csv", row.names = FALSE)

# --- ANOVA with interaction (group * hour) ---
anova_pr <- aov(pr_hourly_pellet_intake ~ group * hour, data = combined_data)
anova_nr <- aov(nr_hourly_pellet_intake ~ group * hour, data = combined_data)

# Save ANOVA results
capture.output(summary(anova_pr), file = "C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\anova_pr_with_hour_interaction.txt")
capture.output(summary(anova_nr), file = "C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\anova_nr_with_hour_interaction.txt")

# --- Posthoc Tukey Test to examine group differences at specific hours ---
posthoc_pr <- TukeyHSD(anova_pr, "group:hour")
posthoc_nr <- TukeyHSD(anova_nr, "group:hour")

# Save posthoc test results
capture.output(posthoc_pr, file = "C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\posthoc_pr_tukey_hour_comparison.txt")
capture.output(posthoc_nr, file = "C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\posthoc_nr_tukey_hour_comparison.txt")

# --- Pairwise Comparisons for NR and PR phases at each hour ---
# Group-by-hour comparisons for PR phase
pairwise_pr_hour <- pairwise.t.test(combined_data$pr_hourly_pellet_intake, interaction(combined_data$group, combined_data$hour), p.adjust.method = "holm")

# Group-by-hour comparisons for NR phase
pairwise_nr_hour <- pairwise.t.test(combined_data$nr_hourly_pellet_intake, interaction(combined_data$group, combined_data$hour), p.adjust.method = "holm")

# Save Pairwise Comparisons
capture.output(pairwise_pr_hour, file = "C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\pairwise_pr_group_hour_comparisons.txt")
capture.output(pairwise_nr_hour, file = "C:\\Users\\hta031\\Github\\FEDProtein\\results\\ULTIMATE_HOURLY_R\\pairwise_nr_group_hour_comparisons.txt")



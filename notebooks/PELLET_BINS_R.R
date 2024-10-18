


######################################################

######################################################

######################################################

######################################################
# Load necessary libraries
######################################################
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
# Remove reshape2 or load it before dplyr and tidyr if necessary

# Read the CSV file
data <- read_csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\PELLET_PINS.csv")
# View the data
print(data)

# Melt the data to long format for statistical analysis
data_long <- data %>%
  tidyr::gather(key = "ClusterSize", value = "EventCount", -MouseID)

# Convert ClusterSize to numeric
data_long$ClusterSize <- as.numeric(gsub("Count_", "", data_long$ClusterSize))

# Aggregate counts for 2-4 pellet events
data_long <- data_long %>%
  mutate(EventGroup = case_when(
    ClusterSize == 1 ~ "Count_1",
    ClusterSize >= 2 & ClusterSize <= 4 ~ "Count_2_4",
    ClusterSize >= 5 ~ "Count_5+"
  ))

# Summarize counts per mouse per group
data_grouped <- data_long %>%
  group_by(MouseID, EventGroup) %>%
  summarize(TotalEvents = sum(EventCount), .groups = 'drop') %>%
  tidyr::spread(EventGroup, TotalEvents, fill = 0)

# View the grouped data
print(data_grouped)

# Perform paired t-test between Count_1 and Count_2_4
t_test_result <- t.test(data_grouped$Count_1, data_grouped$Count_2_4, paired = TRUE)
print(t_test_result)

# Perform ANOVA to compare counts of 2, 3, and 4-pellet events
data_2_4 <- data %>%
  dplyr::select(MouseID, Count_2, Count_3, Count_4) %>%
  tidyr::gather(key = "ClusterSize", value = "EventCount", -MouseID)

# Perform repeated measures ANOVA
anova_result <- aov(EventCount ~ ClusterSize + Error(MouseID/ClusterSize), data = data_2_4)
summary(anova_result)




#################################################################################

####################################     PELLET BINS     ############################

######################################################
# Load necessary libraries and install if not present
######################################################
packages <- c("readr", "tidyr", "dplyr", "ggplot2", "agricolae")
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
data_file <- "C:/Users/hta031/Github/FEDProtein/results/PELLET_PINS.csv"

######################################################
# Read the CSV file
######################################################
data <- read_csv(data_file)

# View the data
print(data)

######################################################
# Reshape data to long format
######################################################
data_long <- data %>%
  pivot_longer(cols = starts_with("Count_"), names_to = "EventSize", values_to = "Count")

# View the reshaped data
print(data_long)

######################################################
# Perform One-Way ANOVA without considering MouseID
######################################################
anova_result <- aov(Count ~ EventSize, data = data_long)

# View ANOVA summary
anova_summary <- summary(anova_result)
print(anova_summary)

######################################################
# Save ANOVA results to a text file
######################################################
anova_output_file <- "C:/Users/hta031/Github/FEDProtein/results/PELLET_anova_results.txt"
capture.output(print(anova_summary), file = anova_output_file)
cat("ANOVA results saved to", anova_output_file, "\n")

######################################################
# Perform Post-hoc Tukey HSD Test
######################################################
tukey_result <- HSD.test(anova_result, "EventSize", group = TRUE, console = TRUE)

# View Tukey HSD test results
print(tukey_result)

######################################################
# Save Post-hoc test results to a text file
######################################################
tukey_output_file <- "C:/Users/hta031/Github/FEDProtein/results/PELLET_tukey_hsd_results.txt"

# Capture the output from HSD.test, including groupings and statistics
capture.output({
  print("Tukey HSD Test Results:")
  print(tukey_result$groups)
  print("Details:")
  print(tukey_result$comparison)
}, file = tukey_output_file)
cat("Tukey HSD test results saved to", tukey_output_file, "\n")

######################################################
# Descriptive Statistics
######################################################
descriptive_stats <- data_long %>%
  group_by(EventSize) %>%
  summarise(
    Mean = mean(Count),
    SD = sd(Count),
    Median = median(Count),
    Min = min(Count),
    Max = max(Count),
    n = n()
  )

# View descriptive statistics
print(descriptive_stats)

######################################################
# Save Descriptive Statistics to a CSV file
######################################################
descriptive_stats_file <- "C:/Users/hta031/Github/FEDProtein/results/PELLET_descriptive_statistics.csv"
write.csv(descriptive_stats, descriptive_stats_file, row.names = FALSE)
cat("Descriptive statistics saved to", descriptive_stats_file, "\n")

######################################################
# Plotting the Counts per Event Size
######################################################
plot_file <- "C:/Users/hta031/Github/FEDProtein/results/PELLET_event_counts_plot.png"

# Create the plot
p <- ggplot(data_long, aes(x = EventSize, y = Count)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Counts per Event Size", x = "Event Size (Number of Pellets)", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
print(p)

# Save the plot
ggsave(plot_file, plot = p, width = 8, height = 6, dpi = 300)
cat("Plot saved to", plot_file, "\n")






################################ FINAL APPROACH ####################################

######################################################
# Load necessary libraries and install if not present
######################################################
packages <- c("readr", "tidyr", "dplyr", "ggplot2", "agricolae")
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
data_file <- "C:/Users/hta031/Github/FEDProtein/results/PELLET_PINS.csv"

######################################################
# Read the CSV file
######################################################
data <- read_csv(data_file)

# View the data
print(data)

######################################################
# Reshape data to long format
######################################################
data_long <- data %>%
  pivot_longer(cols = starts_with("Count_"), names_to = "EventSize", values_to = "Count")

# View the reshaped data
print(data_long)

######################################################
# Perform One-Way ANOVA without considering MouseID
######################################################
anova_result <- aov(Count ~ EventSize, data = data_long)

# View ANOVA summary
anova_summary <- summary(anova_result)
print(anova_summary)

######################################################
# Save ANOVA results to a text file
######################################################
anova_output_file <- "C:/Users/hta031/Github/FEDProtein/results/PELLET_anova_results.txt"
capture.output(print(anova_summary), file = anova_output_file)
cat("ANOVA results saved to", anova_output_file, "\n")

######################################################
# Perform Post-hoc Tukey HSD Test and Capture Output
######################################################
tukey_output_file <- "C:/Users/hta031/Github/FEDProtein/results/PELLET_tukey_hsd_results.txt"

# Capture the output from HSD.test, including all console output
tukey_output <- capture.output(
  tukey_result <- HSD.test(anova_result, "EventSize", group = TRUE, console = TRUE)
)

# Write the captured output to a file
writeLines(tukey_output, tukey_output_file)

cat("Tukey HSD test results saved to", tukey_output_file, "\n")

######################################################
# Descriptive Statistics
######################################################
descriptive_stats <- data_long %>%
  group_by(EventSize) %>%
  summarise(
    Mean = mean(Count),
    SD = sd(Count),
    Median = median(Count),
    Min = min(Count),
    Max = max(Count),
    n = n()
  )

# View descriptive statistics
print(descriptive_stats)

######################################################
# Save Descriptive Statistics to a CSV file
######################################################
descriptive_stats_file <- "C:/Users/hta031/Github/FEDProtein/results/PELLET_descriptive_statistics.csv"
write.csv(descriptive_stats, descriptive_stats_file, row.names = FALSE)
cat("Descriptive statistics saved to", descriptive_stats_file, "\n")

######################################################
# Plotting the Counts per Event Size
######################################################
plot_file <- "C:/Users/hta031/Github/FEDProtein/results/PELLET_event_counts_plot.png"

# Create the plot
p <- ggplot(data_long, aes(x = EventSize, y = Count)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Counts per Event Size", x = "Event Size (Number of Pellets)", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
print(p)

# Save the plot
ggsave(plot_file, plot = p, width = 8, height = 6, dpi = 300)
cat("Plot saved to", plot_file, "\n")





Detailed Interpretation
Let's analyze the groupings to understand which event sizes are significantly different.

EventSize Assignments
Count_1 (Mean = 94.67) → Group 'a'
Count_3 (Mean = 62.67) → Groups 'ab'
Count_4 (Mean = 61.50) → Groups 'ab'
Count_2 (Mean = 55.83) → Groups 'bc'
Count_5 (Mean = 49.83) → Groups 'bcd'
Count_6 (Mean = 24.17) → Groups 'cde'
Count_7 (Mean = 15.67) → Groups 'de'
Count_8 (Mean = 9.50) → Group 'e'
Count_10 (Mean = 8.00) → Group 'e'
Count_9 (Mean = 5.50) → Group 'e'
Significant Differences
Count_1 vs. Other Groups:

Significantly Different From: Count_5, Count_6, Count_7, Count_8, Count_9, Count_10 (since they do not share any common letters beyond 'a').
Not Significantly Different From: Count_3 and Count_4 (they share the letter 'a').
Count_2 vs. Other Groups:

Significantly Different From: Count_6, Count_7, Count_8, Count_9, Count_10 (do not share common letters).
Not Significantly Different From: Count_3, Count_4, Count_5 (shares 'b' and 'c').
Count_3 and Count_4:

Not Significantly Different From Each Other (both in groups 'ab').
Count_5:

Acts as a transitional group, sharing letters 'b', 'c', and 'd'.
Not Significantly Different From: Count_2 (shares 'b' and 'c'), Count_6 (shares 'c' and 'd').
Count_6 vs. Count_7:

Not Significantly Different (both share 'd' and 'e').
Count_8, Count_9, Count_10:

All share group 'e', indicating they are not significantly different from each other.
Significantly Different From: Count_1, Count_2, Count_3, Count_4 (do not share any letters with these).
Summary of Significant Differences
Event sizes can be grouped based on statistical similarities:

Group 'a': Count_1 (1-pellet events) – highest mean count.
Group 'ab': Count_3, Count_4 (3 and 4-pellet events) – not significantly different from Count_1 or each other.
Group 'bc': Count_2 (2-pellet events) – not significantly different from Count_3, Count_4, and Count_5.
Group 'bcd': Count_5 (5-pellet events) – transitional, overlaps with higher and lower groups.
Group 'cde': Count_6 (6-pellet events) – overlaps with middle groups.
Group 'de': Count_7 (7-pellet events) – closer to lower counts.
Group 'e': Count_8, Count_9, Count_10 (8 to 10-pellet events) – lowest mean counts.



Frequency of Events is just one aspect of feeding behavior.
Physiological Impact: The amount of food consumed (pellet number) and the associated caloric intake are crucial for understanding feeding microstructure.
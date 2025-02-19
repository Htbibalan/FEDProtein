

##################################################
##################################################
##################################################
##################################################
##################################################
##################################################
##################################################
##################################################
##################################################
##################################################
##################################################
##################################################
##################################################

##################################################
# (1) Install and load the packages as needed
##################################################
# install.packages("afex")    # If not installed
# install.packages("emmeans") # If not installed
# install.packages("dplyr")   # If not installed

library(afex)
library(emmeans)
library(dplyr)

##################################################
# (2) Read the data
##################################################
data_file <- "C:/Users/hta031/Github/FEDProtein/results/FIVE/Barplots/eco_metrics_breakpoints.csv"
df <- read.csv(data_file)

##################################################
# (3) Convert 'order' and 'pellet_type' into factors
#     and ensure 'mouse' is treated as a subject ID
##################################################
df$order       <- factor(df$order, levels = c(1, 2), labels = c("ProteinRestricted", "NonRestricted"))
df$pellet_type <- factor(df$pellet_type, levels = c("PR", "RICH"))
df$mouse       <- factor(df$mouse)  # Ensure 'mouse' is treated as a subject identifier

##################################################
# (4) Descriptive statistics
##################################################
desc_stats <- df %>%
  group_by(order, pellet_type) %>%
  summarize(
    N    = n(),
    Mean = mean(breakpoint, na.rm = TRUE),
    SD   = sd(breakpoint, na.rm = TRUE),
    SEM  = SD / sqrt(N),
    .groups = "drop"
  )

##################################################
# (5) Repeated-measures ANOVA with afex::aov_ez
#     - 'mouse' is the subject identifier
#     - 'pellet_type' is a within-subject factor
#     - 'order' is a between-subjects factor
#     - Type III SS is used
##################################################
res_aov <- aov_ez(
  id     = "mouse",
  dv     = "breakpoint",
  data   = df,
  within = "pellet_type",
  between= "order",
  type   = 3
)
aov_summary <- summary(res_aov)

##################################################
# (6) Post-hoc tests using emmeans
##################################################
# Extract emmeans for pairwise comparisons
em <- emmeans(res_aov, ~ order * pellet_type)
posthoc_pairs <- pairs(em, adjust = "holm")

##################################################
# (7) SAVE OUTPUTS
##################################################
desc_stats_file <- "C:/Users/hta031/Github/FEDProtein/results/FIVE/Barplots/descriptive_stats.csv"
write.csv(desc_stats, desc_stats_file, row.names = FALSE)

results_file <- "C:/Users/hta031/Github/FEDProtein/results/FIVE/Barplots/ANOVA_Posthoc_Results.txt"
sink(results_file)

cat("=== DESCRIPTIVE STATISTICS ===\n")
print(desc_stats)

cat("\n\n=== ANOVA SUMMARY (afex::aov_ez) ===\n")
print(aov_summary)

cat("\n\n=== POST-HOC PAIRWISE COMPARISONS (Holm) ===\n")
print(posthoc_pairs)

sink()
##################################################







##################################################
##################################################
##################################################
##################################################
##################################################
##################################################
########################PELLETS CHOICE##########################
##################################################
##################################################
##################################################
##################################################
##################################################
##################################################

##################################################
# (1) Install and load the packages as needed
##################################################
# install.packages("afex")    # If not installed
# install.packages("emmeans") # If not installed
# install.packages("dplyr")   # If not installed

library(afex)
library(emmeans)
library(dplyr)

##################################################
# (2) Read the data
##################################################
data_file <- "C:/Users/hta031/Github/FEDProtein/results/FIVE/Barplots/Pellets_choice/eco_metrics_pellets.csv"
df <- read.csv(data_file)

##################################################
# (3) Convert 'order' and 'pellet_type' into factors
#     and ensure 'mouse' is treated as a subject ID
##################################################
df$order       <- factor(df$order, levels = c(1, 2), labels = c("ProteinRestricted", "NonRestricted"))
df$pellet_type <- factor(df$pellet_type, levels = c("PR", "RICH"))
df$mouse       <- factor(df$mouse)  # Ensure 'mouse' is treated as a subject identifier

##################################################
# (4) Descriptive statistics
##################################################
desc_stats <- df %>%
  group_by(order, pellet_type) %>%
  summarize(
    N    = n(),
    Mean = mean(pellets, na.rm = TRUE),
    SD   = sd(pellets, na.rm = TRUE),
    SEM  = SD / sqrt(N),
    .groups = "drop"
  )

##################################################
# (5) Repeated-measures ANOVA with afex::aov_ez
#     - 'mouse' is the subject identifier
#     - 'pellet_type' is a within-subject factor
#     - 'order' is a between-subjects factor
#     - Type III SS is used
##################################################
res_aov <- aov_ez(
  id     = "mouse",
  dv     = "pellets",
  data   = df,
  within = "pellet_type",
  between= "order",
  type   = 3
)
aov_summary <- summary(res_aov)

##################################################
# (6) Post-hoc tests using emmeans
##################################################
# Extract emmeans for pairwise comparisons
em <- emmeans(res_aov, ~ order * pellet_type)
posthoc_pairs <- pairs(em, adjust = "holm")

##################################################
# (7) SAVE OUTPUTS
##################################################
desc_stats_file <- "C:/Users/hta031/Github/FEDProtein/results/FIVE/Barplots/Pellet_choice/descriptive_stats.csv"
write.csv(desc_stats, desc_stats_file, row.names = FALSE)

results_file <- "C:/Users/hta031/Github/FEDProtein/results/FIVE/Barplots/Pellets_choice/ANOVA_Posthoc_Results.txt"
sink(results_file)

cat("=== DESCRIPTIVE STATISTICS ===\n")
print(desc_stats)

cat("\n\n=== ANOVA SUMMARY (afex::aov_ez) ===\n")
print(aov_summary)

cat("\n\n=== POST-HOC PAIRWISE COMPARISONS (Holm) ===\n")
print(posthoc_pairs)

sink()
##################################################






############################################################################################
##################################################################################
### ALL METRICS TOGETHER#################################
############################################


##################################################
# (1) Install and load the packages as needed
##################################################
# install.packages("afex")    # If not installed
# install.packages("emmeans") # If not installed
# install.packages("dplyr")   # If not installed

library(afex)
library(emmeans)
library(dplyr)

##################################################
# (2) Read the data
##################################################
data_file <- "C:/Users/hta031/Github/FEDProtein/results/FIVE/Barplots/CHOICE_BOX/eco_metrics.csv"
df <- read.csv(data_file)

##################################################
# (3) Convert 'order' and 'pellet_type' into factors
##################################################
df$order       <- factor(df$order, levels = c(1, 2), labels = c("ProteinRestricted", "NonRestricted"))
df$pellet_type <- factor(df$pellet_type, levels = c("PR", "RICH"))
df$mouse       <- factor(df$mouse)  # Ensure 'mouse' is a subject identifier

##################################################
# (4) Define the metrics to analyze
##################################################
metrics <- c("avg_breakpoint", "num_breakpoints", "max_breakpoint_ratio", "cost")

# Loop through each metric and perform ANOVA + post-hoc tests
for (metric in metrics) {

  # (5) Descriptive Statistics
  desc_stats <- df %>%
    group_by(order, pellet_type) %>%
    summarize(
      N    = n(),
      Mean = mean(.data[[metric]], na.rm = TRUE),
      SD   = sd(.data[[metric]], na.rm = TRUE),
      SEM  = SD / sqrt(N),
      .groups = "drop"
    )

  # (6) Repeated-measures ANOVA with afex::aov_ez
  res_aov <- aov_ez(
    id     = "mouse",
    dv     = metric,
    data   = df,
    within = "pellet_type",
    between= "order",
    type   = 3
  )
  aov_summary <- summary(res_aov)

  # (7) Post-hoc tests using emmeans
  em <- emmeans(res_aov, ~ order * pellet_type)
  posthoc_pairs <- pairs(em, adjust = "holm")

  # (8) Save outputs
  desc_stats_file <- paste0("C:/Users/hta031/Github/FEDProtein/results/FIVE/Barplots/CHOICE_BOX/descriptive_stats_", metric, ".csv")
  write.csv(desc_stats, desc_stats_file, row.names = FALSE)

  results_file <- paste0("C:/Users/hta031/Github/FEDProtein/results/FIVE/Barplots/CHOICE_BOX/ANOVA_Posthoc_Results_", metric, ".txt")
  sink(results_file)

  cat("=== DESCRIPTIVE STATISTICS (", metric, ") ===\n")
  print(desc_stats)

  cat("\n\n=== ANOVA SUMMARY (", metric, ") ===\n")
  print(aov_summary)

  cat("\n\n=== POST-HOC PAIRWISE COMPARISONS (Holm, ", metric, ") ===\n")
  print(posthoc_pairs)

  sink()
}


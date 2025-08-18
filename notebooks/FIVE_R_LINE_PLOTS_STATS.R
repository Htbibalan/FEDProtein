
################### code below conducting statistical analysis for the line plot data of pellet intake across days ###################

library(tidyr)
library(dplyr)
library(afex)
library(emmeans)
library(ggplot2)

# Load the dataset
data <- read.csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\PELLET_INTAKE\\Pellets_trend.csv")

# Step 1: Reshape the data
long_data <- data %>%
  pivot_longer(cols = starts_with(c("NR", "PR")),
               names_to = "time_phase",
               values_to = "value")

# Step 2: Generate Descriptive Statistics
descriptive_stats <- long_data %>%
  group_by(time_phase, Sex, Order) %>%
  summarise(mean_value = mean(value, na.rm = TRUE),
            sd_value = sd(value, na.rm = TRUE),
            n = n())

# Save descriptive statistics to CSV
write.csv(descriptive_stats, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\PELLET_INTAKE\\Pellets_trend_descriptive_stats.csv")

# Step 3: Run ANOVA for each time point, sex, and order
anova_results <- aov_car(value ~ time_phase * Sex * Order + Error(Mouse/time_phase), data = long_data)

# Save ANOVA results to CSV
write.csv(as.data.frame(anova(anova_results)), "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\PELLET_INTAKE\\Pellets_trend_anova_results.csv")

# Step 4: Post-hoc tests with Tukey adjustment
posthoc_tukey <- emmeans(anova_results, pairwise ~ time_phase * Sex * Order, adjust = "tukey")
posthoc_tukey_df <- as.data.frame(summary(posthoc_tukey$contrasts))
write.csv(posthoc_tukey_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\PELLET_INTAKE\\Pellets_trend_posthoc_tukey_results.csv")

# Step 5: Post-hoc tests with Holm adjustment
posthoc_holm <- emmeans(anova_results, pairwise ~ time_phase * Sex * Order, adjust = "holm")
posthoc_holm_df <- as.data.frame(summary(posthoc_holm$contrasts))
write.csv(posthoc_holm_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\PELLET_INTAKE\\Pellets_trend_posthoc_holm_results.csv")

######################################################## PELLET INTAKE COMBINED ###############################
#############################################################################################################

library(tidyr)
library(dplyr)
library(afex)
library(emmeans)
library(ggplot2)

# Load the dataset
data <- read.csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\PELLET_INTAKE\\Pellets_trend.csv")

# Step 1: Reshape the data
long_data <- data %>%
  pivot_longer(cols = starts_with(c("NR", "PR")),
               names_to = "time_phase",
               values_to = "value")

# Step 2: Generate Descriptive Statistics
descriptive_stats <- long_data %>%
  group_by(time_phase,Order) %>%
  summarise(mean_value = mean(value, na.rm = TRUE),
            sd_value = sd(value, na.rm = TRUE),
            n = n())

# Save descriptive statistics to CSV
write.csv(descriptive_stats, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\PELLET_INTAKE\\COMBINE_Pellets_trend_descriptive_stats.csv")

# Step 3: Run ANOVA for each time point, sex, and order
anova_results <- aov_car(value ~ time_phase  * Order + Error(Mouse/time_phase), data = long_data)

# Save ANOVA results to CSV
write.csv(as.data.frame(anova(anova_results)), "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\PELLET_INTAKE\\COMBINE_Pellets_trend_anova_results.csv")

# Step 4: Post-hoc tests with Tukey adjustment
posthoc_tukey <- emmeans(anova_results, pairwise ~ time_phase  * Order, adjust = "tukey")
posthoc_tukey_df <- as.data.frame(summary(posthoc_tukey$contrasts))
write.csv(posthoc_tukey_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\PELLET_INTAKE\\COMBINE_Pellets_trend_posthoc_tukey_results.csv")

# Step 5: Post-hoc tests with Holm adjustment
posthoc_holm <- emmeans(anova_results, pairwise ~ time_phase  * Order, adjust = "holm")
posthoc_holm_df <- as.data.frame(summary(posthoc_holm$contrasts))
write.csv(posthoc_holm_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\PELLET_INTAKE\\COMBINE_Pellets_trend_posthoc_holm_results.csv")





################################################################################ PELLET TRENDS COMBINED, DOUBLE CHECKED FOR CORRECTNESS ##########################################################
#################################################################################################################################################################################################

############################  PELLET INTAKE (RM-ANOVA + TARGET CONTRASTS)  ############################

############################  PELLET INTAKE — RM-ANOVA + SPECIFIC & ALL PAIRWISE CONTRASTS  ############################

# --- Packages & options ---
library(tidyverse)
library(afex)
library(emmeans)
library(broom)

afex::afex_options(type = 3)                  # Type-III SS
options(contrasts = c("contr.sum","contr.poly"))

# --- Paths   ---
infile <- "C:/Users/hta031/Github/FEDProtein/results/FIVE/LINE_PLOTS/PELLET_INTAKE/Pellets_trend.csv"
outdir <- "C:/Users/hta031/Github/FEDProtein/results/FIVE/LINE_PLOTS/PELLET_INTAKE/RE_DO"
infile <- trimws(infile); outdir <- trimws(outdir)
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

# --- Load & reshape (NR/PR + day parsed cleanly) ---
dat <- read.csv(infile)

long <- dat %>%
  pivot_longer(
    cols = matches("^(NR|PR)\\d+$"),
    names_to        = c("phase","day"),
    names_pattern   = "(NR|PR)(\\d+)",
    names_transform = list(day = as.integer),
    values_to       = "value"
  ) %>%
  mutate(
    day   = factor(day, levels = sort(unique(day))),   # categorical for RM design
    phase = factor(phase, levels = c("NR","PR")),
    Order = factor(Order),
    Sex   = factor(Sex),
    Mouse = factor(Mouse)
  ) %>%
  drop_na(value)

# --- Descriptives per (Order × phase × day) ---
descriptive <- long %>%
  group_by(Order, phase, day) %>%
  summarise(n = sum(!is.na(value)),
            mean = mean(value, na.rm = TRUE),
            sd   = sd(value, na.rm = TRUE),
            .groups = "drop")
write.csv(descriptive, file.path(outdir, "COMBINE_Pellets_descriptives.csv"), row.names = FALSE)

# --- Repeated-measures ANOVA ---
include_sex <- FALSE
between_fml <- if (include_sex) c("Order","Sex") else "Order"

fit <- aov_ez(
  id      = "Mouse",
  dv      = "value",
  within  = c("phase","day"),
  between = between_fml,
  data    = long
)

anova_tab <- afex::nice(fit, es = "pes", correction = "GG")
write.csv(anova_tab, file.path(outdir, "COMBINE_Pellets_ANOVA_GG.csv"), row.names = FALSE)

# --- EMMs for all cells (phase × day × Order) ---
em <- emmeans(fit, ~ phase*day*Order)

# Save the grid (helps inspect labels; day may appear as X0..X6)
grid <- as.data.frame(em)[, c("phase","day","Order")]
write.csv(grid, file.path(outdir, "EMM_grid_Pellets.csv"), row.names = FALSE)

# --- Helper to build a contrast vector A - B (robust to 'X' in day labels) ---
# A, B are c(phase, day, Order) with day like "0","1","2"
mkL <- function(em, A, B) {
  g <- as.data.frame(em)[, c("phase","day","Order")]
  g <- data.frame(lapply(g, function(x) trimws(as.character(x))), stringsAsFactors = FALSE)
  names(g) <- c("phase","day","Order")
  g$day_plain <- sub("^X", "", g$day)  # strip leading 'X' if present

  iA <- which(g$phase==A[1] & g$day_plain==A[2] & g$Order==A[3])
  iB <- which(g$phase==B[1] & g$day_plain==B[2] & g$Order==B[3])

  if (length(iA)!=1 || length(iB)!=1) {
    message("Could not find requested cells. First rows of the EMM grid:")
    print(utils::head(g, 12))
    stop(sprintf("Can't find cells:\n  A = %s\n  B = %s",
                 paste(A, collapse=","), paste(B, collapse=",")))
  }
  L <- rep(0, nrow(g)); L[iA] <- 1; L[iB] <- -1; L
}

# --- SPECIFIC CONTRASTS (Holm-adjusted) --------------------------------------

# Helper to format readable labels like "PR0 Order2"
fmt <- function(phase, day, ord) sprintf("%s%s Order%s", phase, day, ord)

# 1) PR0 (Order 2) vs every other day in Order 2
#    -> PR0 vs PR1..PR6 and PR0 vs NR0..NR6
cts_PR0O2_vs_allO2 <- list()
# PR0 vs PR1..PR6 (within PR phase)
for (d in 1:6) {
  name <- paste(fmt("PR","0","2"), "-", fmt("PR", as.character(d), "2"))
  cts_PR0O2_vs_allO2[[name]] <- mkL(em, c("PR","0","2"), c("PR", as.character(d), "2"))
}
# PR0 vs NR0..NR6 (across to NR phase)
for (d in 0:6) {
  name <- paste(fmt("PR","0","2"), "-", fmt("NR", as.character(d), "2"))
  cts_PR0O2_vs_allO2[[name]] <- mkL(em, c("PR","0","2"), c("NR", as.character(d), "2"))
}

res_PR0O2_vs_allO2 <- summary(contrast(em, cts_PR0O2_vs_allO2), adjust = "holm")
write.csv(as.data.frame(res_PR0O2_vs_allO2),
          file.path(outdir, "SPEC_O2_PR0_vs_all_Order2_days.csv"),
          row.names = FALSE)

# 2) NR0 (Order 2) vs every other NR day in Order 2
#    -> NR0 vs NR1..NR6
cts_NR0O2_vs_NR_O2 <- list()
for (d in 1:6) {
  name <- paste(fmt("NR","0","2"), "-", fmt("NR", as.character(d), "2"))
  cts_NR0O2_vs_NR_O2[[name]] <- mkL(em, c("NR","0","2"), c("NR", as.character(d), "2"))
}

res_NR0O2_vs_NR_O2 <- summary(contrast(em, cts_NR0O2_vs_NR_O2), adjust = "holm")
write.csv(as.data.frame(res_NR0O2_vs_NR_O2),
          file.path(outdir, "SPEC_O2_NR0_vs_NR1to6_in_Order2.csv"),
          row.names = FALSE)

# 3) Matched-day cross-order, cross-phase contrasts
#    a) PRk (Order 2) vs NRk (Order 1), for k = 0..6
#    b) NRk (Order 2) vs PRk (Order 1), for k = 0..6
cts_O2PRk_vs_O1NRk <- list()
for (d in 0:6) {
  name <- paste(fmt("PR", as.character(d), "2"), "-", fmt("NR", as.character(d), "1"))
  cts_O2PRk_vs_O1NRk[[name]] <- mkL(em, c("PR", as.character(d), "2"),
                                         c("NR", as.character(d), "1"))
}
res_O2PRk_vs_O1NRk <- summary(contrast(em, cts_O2PRk_vs_O1NRk), adjust = "holm")
write.csv(as.data.frame(res_O2PRk_vs_O1NRk),
          file.path(outdir, "SPEC_O2_PRk_vs_O1_NRk_matched_days.csv"),
          row.names = FALSE)

cts_O2NRk_vs_O1PRk <- list()
for (d in 0:6) {
  name <- paste(fmt("NR", as.character(d), "2"), "-", fmt("PR", as.character(d), "1"))
  cts_O2NRk_vs_O1PRk[[name]] <- mkL(em, c("NR", as.character(d), "2"),
                                         c("PR", as.character(d), "1"))
}
res_O2NRk_vs_O1PRk <- summary(contrast(em, cts_O2NRk_vs_O1PRk), adjust = "holm")
write.csv(as.data.frame(res_O2NRk_vs_O1PRk),
          file.path(outdir, "SPEC_O2_NRk_vs_O1_PRk_matched_days.csv"),
          row.names = FALSE)

# # --- OPTIONAL: treat O1:PR0 as control and compare it to every O2 cell ---
# do_ctrl_vs_all <- TRUE
# if (do_ctrl_vs_all) {
#   g <- as.data.frame(em)[, c("phase","day","Order")]
#   g <- data.frame(lapply(g, function(x) trimws(as.character(x))), stringsAsFactors = FALSE)
#   names(g) <- c("phase","day","Order")
#   g$day_plain <- sub("^X", "", g$day)

#   iRef <- which(g$phase=="PR" & g$day_plain=="0" & g$Order=="1")
#   if (length(iRef)!=1) stop("Reference cell (O1:PR0) not found in EMM grid.")

#   targ_idx <- which(g$Order=="2")
#   cts_list <- setNames(vector("list", length(targ_idx)),
#                        sprintf("O2:%s%s - O1:PR0", g$phase[targ_idx], g$day_plain[targ_idx]))
#   for (k in seq_along(targ_idx)) {
#     r <- targ_idx[k]
#     L <- rep(0, nrow(g)); L[r] <- 1; L[iRef] <- -1
#     cts_list[[k]] <- L
#   }

#   res_ctrl <- summary(contrast(em, cts_list), adjust = "holm")
#   write.csv(as.data.frame(res_ctrl),
#             file.path(outdir, "COMBINE_PR0O1_vs_all_O2.csv"),
#             row.names = FALSE)
# }

# === ALL pairwise comparisons across phase × day × Order (28 cells → 378 tests) ===
em_cells <- emmeans(fit, ~ phase:day:Order)  # each combo as one level

all_pairs_holm  <- summary(pairs(em_cells, adjust = "holm"))
all_pairs_tukey <- summary(pairs(em_cells, adjust = "tukey"))

# Prettify the contrast labels to look like "PR0 Order1 - NR3 Order2"
prettify_contrasts <- function(df) {
  df <- as.data.frame(df)
  lr <- strsplit(df$contrast, " - ", fixed = TRUE)

  pattern <- "phase\\s*=?\\s*(NR|PR).*day\\s*=?\\s*X?([0-9]+).*Order\\s*=?\\s*([12])"

  grab <- function(x) {
    utils::strcapture(
      pattern = pattern,
      x = x,
      proto = data.frame(phase = character(), day = character(), Order = character(),
                         stringsAsFactors = FALSE)
    )
  }

  L <- do.call(rbind, lapply(lr, function(z) grab(z[1])))
  R <- do.call(rbind, lapply(lr, function(z) grab(z[2])))

  df$contrast_pretty <- paste0(L$phase, L$day, " Order", L$Order,
                               " - ",
                               R$phase, R$day, " Order", R$Order)
  df
}

all_pairs_holm_df  <- prettify_contrasts(all_pairs_holm)
all_pairs_tukey_df <- prettify_contrasts(all_pairs_tukey)

write.csv(all_pairs_holm_df,
          file.path(outdir, "ALL_pairwise_phase_day_Order_HOLM.csv"),
          row.names = FALSE)
write.csv(all_pairs_tukey_df,
          file.path(outdir, "ALL_pairwise_phase_day_Order_TUKEY.csv"),
          row.names = FALSE)

############################################################################################################



#################################################################################################################################################################################################

#################################################################################################################################################################################################


#################################################################################################################################################################################################
###################### SNACKS BELOW ######################

library(tidyr)
library(dplyr)
library(afex)
library(emmeans)
library(ggplot2)

# Load the dataset
data <- read.csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\SNACK\\snacks_per_day_trend.csv")

# Step 1: Reshape the data
long_data <- data %>%
  pivot_longer(cols = starts_with(c("NR", "PR")),
               names_to = "time_phase",
               values_to = "value")

# Step 2: Generate Descriptive Statistics
descriptive_stats <- long_data %>%
  group_by(time_phase, Sex, Order) %>%
  summarise(mean_value = mean(value, na.rm = TRUE),
            sd_value = sd(value, na.rm = TRUE),
            n = n())

# Save descriptive statistics to CSV
write.csv(descriptive_stats, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\SNACK\\snacks_trend_descriptive_stats.csv")

# Step 3: Run ANOVA for each time point, sex, and order
anova_results <- aov_car(value ~ time_phase * Sex * Order + Error(Mouse/time_phase), data = long_data)

# Save ANOVA results to CSV
write.csv(as.data.frame(anova(anova_results)), "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\SNACK\\snacks_trend_anova_results.csv")

# Step 4: Post-hoc tests with Tukey and Holm adjustments
# Tukey adjustment
posthoc_tukey_results <- emmeans(anova_results, pairwise ~ time_phase * Sex * Order, adjust = "Tukey")
posthoc_tukey_df <- as.data.frame(summary(posthoc_tukey_results$contrasts))

# Save Tukey-adjusted post-hoc results to CSV
write.csv(posthoc_tukey_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\SNACK\\snacks_trend_posthoc_results_Tukey.csv")

# Holm adjustment
posthoc_holm_results <- emmeans(anova_results, pairwise ~ time_phase * Sex * Order, adjust = "holm")
posthoc_holm_df <- as.data.frame(summary(posthoc_holm_results$contrasts))

# Save Holm-adjusted post-hoc results to CSV
write.csv(posthoc_holm_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\SNACK\\snacks_trend_posthoc_results_Holm.csv")
###############################################################################################################################################################################################
############################################################################ SNACKS COMBINED #################################################################################################
###############################################################################################################################################################################################


library(tidyr)
library(dplyr)
library(afex)
library(emmeans)
library(ggplot2)

# Load the dataset
data <- read.csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\SNACK\\snacks_per_day_trend.csv")

# Step 1: Reshape the data
long_data <- data %>%
  pivot_longer(cols = starts_with(c("NR", "PR")),
               names_to = "time_phase",
               values_to = "value")

# Step 2: Generate Descriptive Statistics
descriptive_stats <- long_data %>%
  group_by(time_phase, Order) %>%
  summarise(mean_value = mean(value, na.rm = TRUE),
            sd_value = sd(value, na.rm = TRUE),
            n = n())

# Save descriptive statistics to CSV
write.csv(descriptive_stats, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\SNACK\\COMBINE_snacks_trend_descriptive_stats.csv")

# Step 3: Run ANOVA for each time point, sex, and order
anova_results <- aov_car(value ~ time_phase * Order + Error(Mouse/time_phase), data = long_data)

# Save ANOVA results to CSV
write.csv(as.data.frame(anova(anova_results)), "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\SNACK\\COMBINE_snacks_trend_anova_results.csv")

# Step 4: Post-hoc tests with Tukey and Holm adjustments
# Tukey adjustment
posthoc_tukey_results <- emmeans(anova_results, pairwise ~ time_phase * Order, adjust = "Tukey")
posthoc_tukey_df <- as.data.frame(summary(posthoc_tukey_results$contrasts))

# Save Tukey-adjusted post-hoc results to CSV
write.csv(posthoc_tukey_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\SNACK\\COMBINE_snacks_trend_posthoc_results_Tukey.csv")

# Holm adjustment
posthoc_holm_results <- emmeans(anova_results, pairwise ~ time_phase * Order, adjust = "holm")
posthoc_holm_df <- as.data.frame(summary(posthoc_holm_results$contrasts))

# Save Holm-adjusted post-hoc results to CSV
write.csv(posthoc_holm_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\SNACK\\COMBINE_snacks_trend_posthoc_results_Holm.csv")






########################################################################################################################################################################
######################################################################## SNACKS COMBINED RE_DO###########################################################################
###########################################################################################################################################################################

# --- Packages & options ---
library(tidyverse)
library(afex)
library(emmeans)
library(broom)

afex::afex_options(type = 3)                  # Type-III SS
options(contrasts = c("contr.sum","contr.poly"))

# --- Paths   ---
infile <- "C:/Users/hta031/Github/FEDProtein/results/FIVE/LINE_PLOTS/SNACK/snacks_per_day_trend.csv"
outdir <- "C:/Users/hta031/Github/FEDProtein/results/FIVE/LINE_PLOTS/SNACK/RE_DO"
infile <- trimws(infile); outdir <- trimws(outdir)
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

# --- Load & reshape (NR/PR + day parsed cleanly) ---
dat <- read.csv(infile)

long <- dat %>%
  pivot_longer(
    cols = matches("^(NR|PR)\\d+$"),
    names_to        = c("phase","day"),
    names_pattern   = "(NR|PR)(\\d+)",
    names_transform = list(day = as.integer),
    values_to       = "value"
  ) %>%
  mutate(
    day   = factor(day, levels = sort(unique(day))),   # categorical for RM design
    phase = factor(phase, levels = c("NR","PR")),
    Order = factor(Order),
    Sex   = factor(Sex),
    Mouse = factor(Mouse)
  ) %>%
  drop_na(value)

# --- Descriptives per (Order × phase × day) ---
descriptive <- long %>%
  group_by(Order, phase, day) %>%
  summarise(n = sum(!is.na(value)),
            mean = mean(value, na.rm = TRUE),
            sd   = sd(value, na.rm = TRUE),
            .groups = "drop")
write.csv(descriptive, file.path(outdir, "COMBINE_Snacks_descriptives.csv"), row.names = FALSE)

# --- Repeated-measures ANOVA ---
include_sex <- FALSE
between_fml <- if (include_sex) c("Order","Sex") else "Order"

fit <- aov_ez(
  id      = "Mouse",
  dv      = "value",
  within  = c("phase","day"),
  between = between_fml,
  data    = long
)

anova_tab <- afex::nice(fit, es = "pes", correction = "GG")
write.csv(anova_tab, file.path(outdir, "COMBINE_Snacks_ANOVA_GG.csv"), row.names = FALSE)

# --- EMMs for all cells (phase × day × Order) ---
em <- emmeans(fit, ~ phase*day*Order)

# Save the grid (helps inspect labels; day may appear as X0..X6)
grid <- as.data.frame(em)[, c("phase","day","Order")]
write.csv(grid, file.path(outdir, "EMM_grid_Pellets.csv"), row.names = FALSE)

# --- Helper to build a contrast vector A - B (robust to 'X' in day labels) ---
# A, B are c(phase, day, Order) with day like "0","1","2"
mkL <- function(em, A, B) {
  g <- as.data.frame(em)[, c("phase","day","Order")]
  g <- data.frame(lapply(g, function(x) trimws(as.character(x))), stringsAsFactors = FALSE)
  names(g) <- c("phase","day","Order")
  g$day_plain <- sub("^X", "", g$day)  # strip leading 'X' if present

  iA <- which(g$phase==A[1] & g$day_plain==A[2] & g$Order==A[3])
  iB <- which(g$phase==B[1] & g$day_plain==B[2] & g$Order==B[3])

  if (length(iA)!=1 || length(iB)!=1) {
    message("Could not find requested cells. First rows of the EMM grid:")
    print(utils::head(g, 12))
    stop(sprintf("Can't find cells:\n  A = %s\n  B = %s",
                 paste(A, collapse=","), paste(B, collapse=",")))
  }
  L <- rep(0, nrow(g)); L[iA] <- 1; L[iB] <- -1; L
}

# --- SPECIFIC CONTRASTS (Holm-adjusted) --------------------------------------

# Helper to format readable labels like "PR0 Order2"
fmt <- function(phase, day, ord) sprintf("%s%s Order%s", phase, day, ord)

# 1) PR0 (Order 2) vs every other day in Order 2
#    -> PR0 vs PR1..PR6 and PR0 vs NR0..NR6
cts_PR0O2_vs_allO2 <- list()
# PR0 vs PR1..PR6 (within PR phase)
for (d in 1:6) {
  name <- paste(fmt("PR","0","2"), "-", fmt("PR", as.character(d), "2"))
  cts_PR0O2_vs_allO2[[name]] <- mkL(em, c("PR","0","2"), c("PR", as.character(d), "2"))
}
# PR0 vs NR0..NR6 (across to NR phase)
for (d in 0:6) {
  name <- paste(fmt("PR","0","2"), "-", fmt("NR", as.character(d), "2"))
  cts_PR0O2_vs_allO2[[name]] <- mkL(em, c("PR","0","2"), c("NR", as.character(d), "2"))
}

res_PR0O2_vs_allO2 <- summary(contrast(em, cts_PR0O2_vs_allO2), adjust = "holm")
write.csv(as.data.frame(res_PR0O2_vs_allO2),
          file.path(outdir, "SPEC_O2_PR0_vs_all_Order2_days.csv"),
          row.names = FALSE)

# 2) NR0 (Order 2) vs every other NR day in Order 2
#    -> NR0 vs NR1..NR6
cts_NR0O2_vs_NR_O2 <- list()
for (d in 1:6) {
  name <- paste(fmt("NR","0","2"), "-", fmt("NR", as.character(d), "2"))
  cts_NR0O2_vs_NR_O2[[name]] <- mkL(em, c("NR","0","2"), c("NR", as.character(d), "2"))
}

res_NR0O2_vs_NR_O2 <- summary(contrast(em, cts_NR0O2_vs_NR_O2), adjust = "holm")
write.csv(as.data.frame(res_NR0O2_vs_NR_O2),
          file.path(outdir, "SPEC_O2_NR0_vs_NR1to6_in_Order2.csv"),
          row.names = FALSE)

# 3) Matched-day cross-order, cross-phase contrasts
#    a) PRk (Order 2) vs NRk (Order 1), for k = 0..6
#    b) NRk (Order 2) vs PRk (Order 1), for k = 0..6
cts_O2PRk_vs_O1NRk <- list()
for (d in 0:6) {
  name <- paste(fmt("PR", as.character(d), "2"), "-", fmt("NR", as.character(d), "1"))
  cts_O2PRk_vs_O1NRk[[name]] <- mkL(em, c("PR", as.character(d), "2"),
                                         c("NR", as.character(d), "1"))
}
res_O2PRk_vs_O1NRk <- summary(contrast(em, cts_O2PRk_vs_O1NRk), adjust = "holm")
write.csv(as.data.frame(res_O2PRk_vs_O1NRk),
          file.path(outdir, "SPEC_O2_PRk_vs_O1_NRk_matched_days.csv"),
          row.names = FALSE)

cts_O2NRk_vs_O1PRk <- list()
for (d in 0:6) {
  name <- paste(fmt("NR", as.character(d), "2"), "-", fmt("PR", as.character(d), "1"))
  cts_O2NRk_vs_O1PRk[[name]] <- mkL(em, c("NR", as.character(d), "2"),
                                         c("PR", as.character(d), "1"))
}
res_O2NRk_vs_O1PRk <- summary(contrast(em, cts_O2NRk_vs_O1PRk), adjust = "holm")
write.csv(as.data.frame(res_O2NRk_vs_O1PRk),
          file.path(outdir, "SPEC_O2_NRk_vs_O1_PRk_matched_days.csv"),
          row.names = FALSE)


# === ALL pairwise comparisons across phase × day × Order (28 cells → 378 tests) ===
em_cells <- emmeans(fit, ~ phase:day:Order)  # each combo as one level

all_pairs_holm  <- summary(pairs(em_cells, adjust = "holm"))
all_pairs_tukey <- summary(pairs(em_cells, adjust = "tukey"))

# Prettify the contrast labels to look like "PR0 Order1 - NR3 Order2"
prettify_contrasts <- function(df) {
  df <- as.data.frame(df)
  lr <- strsplit(df$contrast, " - ", fixed = TRUE)

  pattern <- "phase\\s*=?\\s*(NR|PR).*day\\s*=?\\s*X?([0-9]+).*Order\\s*=?\\s*([12])"

  grab <- function(x) {
    utils::strcapture(
      pattern = pattern,
      x = x,
      proto = data.frame(phase = character(), day = character(), Order = character(),
                         stringsAsFactors = FALSE)
    )
  }

  L <- do.call(rbind, lapply(lr, function(z) grab(z[1])))
  R <- do.call(rbind, lapply(lr, function(z) grab(z[2])))

  df$contrast_pretty <- paste0(L$phase, L$day, " Order", L$Order,
                               " - ",
                               R$phase, R$day, " Order", R$Order)
  df
}

all_pairs_holm_df  <- prettify_contrasts(all_pairs_holm)
all_pairs_tukey_df <- prettify_contrasts(all_pairs_tukey)

write.csv(all_pairs_holm_df,
          file.path(outdir, "ALL_pairwise_phase_day_Order_HOLM.csv"),
          row.names = FALSE)
write.csv(all_pairs_tukey_df,
          file.path(outdir, "ALL_pairwise_phase_day_Order_TUKEY.csv"),
          row.names = FALSE)





###############################################################################################################################################################################################
############################################################################ SNACKS Frequency COMBINED #################################################################################################
###############################################################################################################################################################################################


library(tidyr)
library(dplyr)
library(afex)
library(emmeans)
library(ggplot2)

# Load the dataset
data <- read.csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\SNACK\\snack_freq_per_day\\snack_freq_realigned_FINAL.csv")

# Step 1: Reshape the data
long_data <- data %>%
  pivot_longer(cols = starts_with(c("NR", "PR")),
               names_to = "time_phase",
               values_to = "value")

# Step 2: Generate Descriptive Statistics
descriptive_stats <- long_data %>%
  group_by(time_phase, Order) %>%
  summarise(mean_value = mean(value, na.rm = TRUE),
            sd_value = sd(value, na.rm = TRUE),
            n = n())

# Save descriptive statistics to CSV
write.csv(descriptive_stats, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\SNACK\\snack_freq_per_day\\COMBINE_snacksFreq_trend_descriptive_stats.csv")

# Step 3: Run ANOVA for each time point, sex, and order
anova_results <- aov_car(value ~ time_phase * Order + Error(Mouse/time_phase), data = long_data)

# Save ANOVA results to CSV
write.csv(as.data.frame(anova(anova_results)), "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\SNACK\\snack_freq_per_day\\COMBINE_snacksFreq_trend_anova_results.csv")

# Step 4: Post-hoc tests with Tukey and Holm adjustments
# Tukey adjustment
posthoc_tukey_results <- emmeans(anova_results, pairwise ~ time_phase * Order, adjust = "Tukey")
posthoc_tukey_df <- as.data.frame(summary(posthoc_tukey_results$contrasts))

# Save Tukey-adjusted post-hoc results to CSV
write.csv(posthoc_tukey_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\SNACK\\snack_freq_per_day\\COMBINE_snacksFreq_trend_posthoc_results_Tukey.csv")

# Holm adjustment
posthoc_holm_results <- emmeans(anova_results, pairwise ~ time_phase * Order, adjust = "holm")
posthoc_holm_df <- as.data.frame(summary(posthoc_holm_results$contrasts))

# Save Holm-adjusted post-hoc results to CSV
write.csv(posthoc_holm_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\SNACK\\snack_freq_per_day\\COMBINE_snacksFreq_trend_posthoc_results_Holm.csv")












#######################################################################################################################################################################
#############################################################SNACKS FREQUUENCY RE DO##########################################################################
#####################################################################################################################################################


# --- Packages & options ---
library(tidyverse)
library(afex)
library(emmeans)
library(broom)

afex::afex_options(type = 3)                  # Type-III SS
options(contrasts = c("contr.sum","contr.poly"))

# --- Paths   ---
infile <- "C:/Users/hta031/Github/FEDProtein/results/FIVE/LINE_PLOTS/SNACK/snack_freq_per_day/snack_freq_realigned_FINAL.csv"
outdir <- "C:/Users/hta031/Github/FEDProtein/results/FIVE/LINE_PLOTS/SNACK/snack_freq_per_day/RE_DO"
infile <- trimws(infile); outdir <- trimws(outdir)
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

# --- Load & reshape (NR/PR + day parsed cleanly) ---
dat <- read.csv(infile)

long <- dat %>%
  pivot_longer(
    cols = matches("^(NR|PR)\\d+$"),
    names_to        = c("phase","day"),
    names_pattern   = "(NR|PR)(\\d+)",
    names_transform = list(day = as.integer),
    values_to       = "value"
  ) %>%
  mutate(
    day   = factor(day, levels = sort(unique(day))),   # categorical for RM design
    phase = factor(phase, levels = c("NR","PR")),
    Order = factor(Order),
    Sex   = factor(Sex),
    Mouse = factor(Mouse)
  ) %>%
  drop_na(value)

# --- Descriptives per (Order × phase × day) ---
descriptive <- long %>%
  group_by(Order, phase, day) %>%
  summarise(n = sum(!is.na(value)),
            mean = mean(value, na.rm = TRUE),
            sd   = sd(value, na.rm = TRUE),
            .groups = "drop")
write.csv(descriptive, file.path(outdir, "COMBINE_Snacks_descriptives.csv"), row.names = FALSE)

# --- Repeated-measures ANOVA ---
include_sex <- FALSE
between_fml <- if (include_sex) c("Order","Sex") else "Order"

fit <- aov_ez(
  id      = "Mouse",
  dv      = "value",
  within  = c("phase","day"),
  between = between_fml,
  data    = long
)

anova_tab <- afex::nice(fit, es = "pes", correction = "GG")
write.csv(anova_tab, file.path(outdir, "COMBINE_Snacks_ANOVA_GG.csv"), row.names = FALSE)

# --- EMMs for all cells (phase × day × Order) ---
em <- emmeans(fit, ~ phase*day*Order)

# Save the grid (helps inspect labels; day may appear as X0..X6)
grid <- as.data.frame(em)[, c("phase","day","Order")]
write.csv(grid, file.path(outdir, "EMM_grid_Pellets.csv"), row.names = FALSE)

# --- Helper to build a contrast vector A - B (robust to 'X' in day labels) ---
# A, B are c(phase, day, Order) with day like "0","1","2"
mkL <- function(em, A, B) {
  g <- as.data.frame(em)[, c("phase","day","Order")]
  g <- data.frame(lapply(g, function(x) trimws(as.character(x))), stringsAsFactors = FALSE)
  names(g) <- c("phase","day","Order")
  g$day_plain <- sub("^X", "", g$day)  # strip leading 'X' if present

  iA <- which(g$phase==A[1] & g$day_plain==A[2] & g$Order==A[3])
  iB <- which(g$phase==B[1] & g$day_plain==B[2] & g$Order==B[3])

  if (length(iA)!=1 || length(iB)!=1) {
    message("Could not find requested cells. First rows of the EMM grid:")
    print(utils::head(g, 12))
    stop(sprintf("Can't find cells:\n  A = %s\n  B = %s",
                 paste(A, collapse=","), paste(B, collapse=",")))
  }
  L <- rep(0, nrow(g)); L[iA] <- 1; L[iB] <- -1; L
}

# --- SPECIFIC CONTRASTS (Holm-adjusted) --------------------------------------

# Helper to format readable labels like "PR0 Order2"
fmt <- function(phase, day, ord) sprintf("%s%s Order%s", phase, day, ord)

# 1) PR0 (Order 2) vs every other day in Order 2
#    -> PR0 vs PR1..PR6 and PR0 vs NR0..NR6
cts_PR0O2_vs_allO2 <- list()
# PR0 vs PR1..PR6 (within PR phase)
for (d in 1:6) {
  name <- paste(fmt("PR","0","2"), "-", fmt("PR", as.character(d), "2"))
  cts_PR0O2_vs_allO2[[name]] <- mkL(em, c("PR","0","2"), c("PR", as.character(d), "2"))
}
# PR0 vs NR0..NR6 (across to NR phase)
for (d in 0:6) {
  name <- paste(fmt("PR","0","2"), "-", fmt("NR", as.character(d), "2"))
  cts_PR0O2_vs_allO2[[name]] <- mkL(em, c("PR","0","2"), c("NR", as.character(d), "2"))
}

res_PR0O2_vs_allO2 <- summary(contrast(em, cts_PR0O2_vs_allO2), adjust = "holm")
write.csv(as.data.frame(res_PR0O2_vs_allO2),
          file.path(outdir, "SPEC_O2_PR0_vs_all_Order2_days.csv"),
          row.names = FALSE)

# 2) NR0 (Order 2) vs every other NR day in Order 2
#    -> NR0 vs NR1..NR6
cts_NR0O2_vs_NR_O2 <- list()
for (d in 1:6) {
  name <- paste(fmt("NR","0","2"), "-", fmt("NR", as.character(d), "2"))
  cts_NR0O2_vs_NR_O2[[name]] <- mkL(em, c("NR","0","2"), c("NR", as.character(d), "2"))
}

res_NR0O2_vs_NR_O2 <- summary(contrast(em, cts_NR0O2_vs_NR_O2), adjust = "holm")
write.csv(as.data.frame(res_NR0O2_vs_NR_O2),
          file.path(outdir, "SPEC_O2_NR0_vs_NR1to6_in_Order2.csv"),
          row.names = FALSE)

# 3) Matched-day cross-order, cross-phase contrasts
#    a) PRk (Order 2) vs NRk (Order 1), for k = 0..6
#    b) NRk (Order 2) vs PRk (Order 1), for k = 0..6
cts_O2PRk_vs_O1NRk <- list()
for (d in 0:6) {
  name <- paste(fmt("PR", as.character(d), "2"), "-", fmt("NR", as.character(d), "1"))
  cts_O2PRk_vs_O1NRk[[name]] <- mkL(em, c("PR", as.character(d), "2"),
                                         c("NR", as.character(d), "1"))
}
res_O2PRk_vs_O1NRk <- summary(contrast(em, cts_O2PRk_vs_O1NRk), adjust = "holm")
write.csv(as.data.frame(res_O2PRk_vs_O1NRk),
          file.path(outdir, "SPEC_O2_PRk_vs_O1_NRk_matched_days.csv"),
          row.names = FALSE)

cts_O2NRk_vs_O1PRk <- list()
for (d in 0:6) {
  name <- paste(fmt("NR", as.character(d), "2"), "-", fmt("PR", as.character(d), "1"))
  cts_O2NRk_vs_O1PRk[[name]] <- mkL(em, c("NR", as.character(d), "2"),
                                         c("PR", as.character(d), "1"))
}
res_O2NRk_vs_O1PRk <- summary(contrast(em, cts_O2NRk_vs_O1PRk), adjust = "holm")
write.csv(as.data.frame(res_O2NRk_vs_O1PRk),
          file.path(outdir, "SPEC_O2_NRk_vs_O1_PRk_matched_days.csv"),
          row.names = FALSE)


# === ALL pairwise comparisons across phase × day × Order (28 cells → 378 tests) ===
em_cells <- emmeans(fit, ~ phase:day:Order)  # each combo as one level

all_pairs_holm  <- summary(pairs(em_cells, adjust = "holm"))
all_pairs_tukey <- summary(pairs(em_cells, adjust = "tukey"))

# Prettify the contrast labels to look like "PR0 Order1 - NR3 Order2"
prettify_contrasts <- function(df) {
  df <- as.data.frame(df)
  lr <- strsplit(df$contrast, " - ", fixed = TRUE)

  pattern <- "phase\\s*=?\\s*(NR|PR).*day\\s*=?\\s*X?([0-9]+).*Order\\s*=?\\s*([12])"

  grab <- function(x) {
    utils::strcapture(
      pattern = pattern,
      x = x,
      proto = data.frame(phase = character(), day = character(), Order = character(),
                         stringsAsFactors = FALSE)
    )
  }

  L <- do.call(rbind, lapply(lr, function(z) grab(z[1])))
  R <- do.call(rbind, lapply(lr, function(z) grab(z[2])))

  df$contrast_pretty <- paste0(L$phase, L$day, " Order", L$Order,
                               " - ",
                               R$phase, R$day, " Order", R$Order)
  df
}

all_pairs_holm_df  <- prettify_contrasts(all_pairs_holm)
all_pairs_tukey_df <- prettify_contrasts(all_pairs_tukey)

write.csv(all_pairs_holm_df,
          file.path(outdir, "ALL_pairwise_phase_day_Order_HOLM.csv"),
          row.names = FALSE)
write.csv(all_pairs_tukey_df,
          file.path(outdir, "ALL_pairwise_phase_day_Order_TUKEY.csv"),
          row.names = FALSE)









###############################################################################################################################################################################################
############################################################################ MEALS BELOW #################################################################################################
###############################################################################################################################################################################################
library(tidyr)
library(dplyr)
library(afex)
library(emmeans)
library(ggplot2)

# Load the dataset
data <- read.csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\MEAL\\MEAL_per_day_trend.csv")

# Step 1: Reshape the data
long_data <- data %>%
  pivot_longer(cols = starts_with(c("NR", "PR")),
               names_to = "time_phase",
               values_to = "value")

# Step 2: Generate Descriptive Statistics
descriptive_stats <- long_data %>%
  group_by(time_phase, Sex, Order) %>%
  summarise(mean_value = mean(value, na.rm = TRUE),
            sd_value = sd(value, na.rm = TRUE),
            n = n())

# Save descriptive statistics to CSV
write.csv(descriptive_stats, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\MEAL\\MEAL_trend_descriptive_stats.csv")

# Step 3: Run ANOVA for each time point, sex, and order
anova_results <- aov_car(value ~ time_phase * Sex * Order + Error(Mouse/time_phase), data = long_data)

# Save ANOVA results to CSV
write.csv(as.data.frame(anova(anova_results)), "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\MEAL\\MEAL_trend_anova_results.csv")

# Step 4: Post-hoc tests with Tukey and Holm adjustments
# Tukey adjustment
posthoc_tukey_results <- emmeans(anova_results, pairwise ~ time_phase * Sex * Order, adjust = "Tukey")
posthoc_tukey_df <- as.data.frame(summary(posthoc_tukey_results$contrasts))

# Save Tukey-adjusted post-hoc results to CSV
write.csv(posthoc_tukey_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\MEAL\\MEAL_trend_posthoc_results_Tukey.csv")

# Holm adjustment
posthoc_holm_results <- emmeans(anova_results, pairwise ~ time_phase * Sex * Order, adjust = "holm")
posthoc_holm_df <- as.data.frame(summary(posthoc_holm_results$contrasts))

# Save Holm-adjusted post-hoc results to CSV
write.csv(posthoc_holm_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\MEAL\\MEAL_trend_posthoc_results_Holm.csv")


###############################################################################################################################################################################################
############################################################################ MEALS COMBINED #################################################################################################
###############################################################################################################################################################################################
library(tidyr)
library(dplyr)
library(afex)
library(emmeans)
library(ggplot2)

# Load the dataset
data <- read.csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\MEAL\\meals_per_day_trend.csv")

# Step 1: Reshape the data
long_data <- data %>%
  pivot_longer(cols = starts_with(c("NR", "PR")),
               names_to = "time_phase",
               values_to = "value")

# Step 2: Generate Descriptive Statistics
descriptive_stats <- long_data %>%
  group_by(time_phase, Order) %>%
  summarise(mean_value = mean(value, na.rm = TRUE),
            sd_value = sd(value, na.rm = TRUE),
            n = n())

# Save descriptive statistics to CSV
write.csv(descriptive_stats, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\MEAL\\COMBINE_MEAL_trend_descriptive_stats.csv")

# Step 3: Run ANOVA for each time point, sex, and order
anova_results <- aov_car(value ~ time_phase * Order + Error(Mouse/time_phase), data = long_data)

# Save ANOVA results to CSV
write.csv(as.data.frame(anova(anova_results)), "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\MEAL\\COMBINE_MEAL_trend_anova_results.csv")

# Step 4: Post-hoc tests with Tukey and Holm adjustments
# Tukey adjustment
posthoc_tukey_results <- emmeans(anova_results, pairwise ~ time_phase  * Order, adjust = "Tukey")
posthoc_tukey_df <- as.data.frame(summary(posthoc_tukey_results$contrasts))

# Save Tukey-adjusted post-hoc results to CSV
write.csv(posthoc_tukey_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\MEAL\\COMBINE_MEAL_trend_posthoc_results_Tukey.csv")

# Holm adjustment
posthoc_holm_results <- emmeans(anova_results, pairwise ~ time_phase * Order, adjust = "holm")
posthoc_holm_df <- as.data.frame(summary(posthoc_holm_results$contrasts))

# Save Holm-adjusted post-hoc results to CSV
write.csv(posthoc_holm_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\MEAL\\COMBINE_MEAL_trend_posthoc_results_Holm.csv")


###############################################################################################################################################################################
##################################################################### MEALS NUMBER RE_DO#########################################################################################


# --- Packages & options ---
library(tidyverse)
library(afex)
library(emmeans)
library(broom)

afex::afex_options(type = 3)                  # Type-III SS
options(contrasts = c("contr.sum","contr.poly"))

# --- Paths   ---
infile <- "C:/Users/hta031/Github/FEDProtein/results/FIVE/LINE_PLOTS/MEAL/meals_per_day_trend.csv"
outdir <- "C:/Users/hta031/Github/FEDProtein/results/FIVE/LINE_PLOTS/MEAL/RE_DO_MEAL_NUMBER"
infile <- trimws(infile); outdir <- trimws(outdir)
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

# --- Load & reshape (NR/PR + day parsed cleanly) ---
dat <- read.csv(infile)

long <- dat %>%
  pivot_longer(
    cols = matches("^(NR|PR)\\d+$"),
    names_to        = c("phase","day"),
    names_pattern   = "(NR|PR)(\\d+)",
    names_transform = list(day = as.integer),
    values_to       = "value"
  ) %>%
  mutate(
    day   = factor(day, levels = sort(unique(day))),   # categorical for RM design
    phase = factor(phase, levels = c("NR","PR")),
    Order = factor(Order),
    Sex   = factor(Sex),
    Mouse = factor(Mouse)
  ) %>%
  drop_na(value)

# --- Descriptives per (Order × phase × day) ---
descriptive <- long %>%
  group_by(Order, phase, day) %>%
  summarise(n = sum(!is.na(value)),
            mean = mean(value, na.rm = TRUE),
            sd   = sd(value, na.rm = TRUE),
            .groups = "drop")
write.csv(descriptive, file.path(outdir, "COMBINE_MealsNumber_descriptives.csv"), row.names = FALSE)

# --- Repeated-measures ANOVA ---
include_sex <- FALSE
between_fml <- if (include_sex) c("Order","Sex") else "Order"

fit <- aov_ez(
  id      = "Mouse",
  dv      = "value",
  within  = c("phase","day"),
  between = between_fml,
  data    = long
)

anova_tab <- afex::nice(fit, es = "pes", correction = "GG")
write.csv(anova_tab, file.path(outdir, "COMBINE_MealsNumber_ANOVA_GG.csv"), row.names = FALSE)

# --- EMMs for all cells (phase × day × Order) ---
em <- emmeans(fit, ~ phase*day*Order)

# Save the grid (helps inspect labels; day may appear as X0..X6)
grid <- as.data.frame(em)[, c("phase","day","Order")]
write.csv(grid, file.path(outdir, "EMM_grid_Pellets.csv"), row.names = FALSE)

# --- Helper to build a contrast vector A - B (robust to 'X' in day labels) ---
# A, B are c(phase, day, Order) with day like "0","1","2"
mkL <- function(em, A, B) {
  g <- as.data.frame(em)[, c("phase","day","Order")]
  g <- data.frame(lapply(g, function(x) trimws(as.character(x))), stringsAsFactors = FALSE)
  names(g) <- c("phase","day","Order")
  g$day_plain <- sub("^X", "", g$day)  # strip leading 'X' if present

  iA <- which(g$phase==A[1] & g$day_plain==A[2] & g$Order==A[3])
  iB <- which(g$phase==B[1] & g$day_plain==B[2] & g$Order==B[3])

  if (length(iA)!=1 || length(iB)!=1) {
    message("Could not find requested cells. First rows of the EMM grid:")
    print(utils::head(g, 12))
    stop(sprintf("Can't find cells:\n  A = %s\n  B = %s",
                 paste(A, collapse=","), paste(B, collapse=",")))
  }
  L <- rep(0, nrow(g)); L[iA] <- 1; L[iB] <- -1; L
}

# --- SPECIFIC CONTRASTS (Holm-adjusted) --------------------------------------

# Helper to format readable labels like "PR0 Order2"
fmt <- function(phase, day, ord) sprintf("%s%s Order%s", phase, day, ord)

# 1) PR0 (Order 2) vs every other day in Order 2
#    -> PR0 vs PR1..PR6 and PR0 vs NR0..NR6
cts_PR0O2_vs_allO2 <- list()
# PR0 vs PR1..PR6 (within PR phase)
for (d in 1:6) {
  name <- paste(fmt("PR","0","2"), "-", fmt("PR", as.character(d), "2"))
  cts_PR0O2_vs_allO2[[name]] <- mkL(em, c("PR","0","2"), c("PR", as.character(d), "2"))
}
# PR0 vs NR0..NR6 (across to NR phase)
for (d in 0:6) {
  name <- paste(fmt("PR","0","2"), "-", fmt("NR", as.character(d), "2"))
  cts_PR0O2_vs_allO2[[name]] <- mkL(em, c("PR","0","2"), c("NR", as.character(d), "2"))
}

res_PR0O2_vs_allO2 <- summary(contrast(em, cts_PR0O2_vs_allO2), adjust = "holm")
write.csv(as.data.frame(res_PR0O2_vs_allO2),
          file.path(outdir, "SPEC_O2_PR0_vs_all_Order2_days.csv"),
          row.names = FALSE)

# 2) NR0 (Order 2) vs every other NR day in Order 2
#    -> NR0 vs NR1..NR6
cts_NR0O2_vs_NR_O2 <- list()
for (d in 1:6) {
  name <- paste(fmt("NR","0","2"), "-", fmt("NR", as.character(d), "2"))
  cts_NR0O2_vs_NR_O2[[name]] <- mkL(em, c("NR","0","2"), c("NR", as.character(d), "2"))
}

res_NR0O2_vs_NR_O2 <- summary(contrast(em, cts_NR0O2_vs_NR_O2), adjust = "holm")
write.csv(as.data.frame(res_NR0O2_vs_NR_O2),
          file.path(outdir, "SPEC_O2_NR0_vs_NR1to6_in_Order2.csv"),
          row.names = FALSE)

# 3) Matched-day cross-order, cross-phase contrasts
#    a) PRk (Order 2) vs NRk (Order 1), for k = 0..6
#    b) NRk (Order 2) vs PRk (Order 1), for k = 0..6
cts_O2PRk_vs_O1NRk <- list()
for (d in 0:6) {
  name <- paste(fmt("PR", as.character(d), "2"), "-", fmt("NR", as.character(d), "1"))
  cts_O2PRk_vs_O1NRk[[name]] <- mkL(em, c("PR", as.character(d), "2"),
                                         c("NR", as.character(d), "1"))
}
res_O2PRk_vs_O1NRk <- summary(contrast(em, cts_O2PRk_vs_O1NRk), adjust = "holm")
write.csv(as.data.frame(res_O2PRk_vs_O1NRk),
          file.path(outdir, "SPEC_O2_PRk_vs_O1_NRk_matched_days.csv"),
          row.names = FALSE)

cts_O2NRk_vs_O1PRk <- list()
for (d in 0:6) {
  name <- paste(fmt("NR", as.character(d), "2"), "-", fmt("PR", as.character(d), "1"))
  cts_O2NRk_vs_O1PRk[[name]] <- mkL(em, c("NR", as.character(d), "2"),
                                         c("PR", as.character(d), "1"))
}
res_O2NRk_vs_O1PRk <- summary(contrast(em, cts_O2NRk_vs_O1PRk), adjust = "holm")
write.csv(as.data.frame(res_O2NRk_vs_O1PRk),
          file.path(outdir, "SPEC_O2_NRk_vs_O1_PRk_matched_days.csv"),
          row.names = FALSE)


# === ALL pairwise comparisons across phase × day × Order (28 cells → 378 tests) ===
em_cells <- emmeans(fit, ~ phase:day:Order)  # each combo as one level

all_pairs_holm  <- summary(pairs(em_cells, adjust = "holm"))
all_pairs_tukey <- summary(pairs(em_cells, adjust = "tukey"))

# Prettify the contrast labels to look like "PR0 Order1 - NR3 Order2"
prettify_contrasts <- function(df) {
  df <- as.data.frame(df)
  lr <- strsplit(df$contrast, " - ", fixed = TRUE)

  pattern <- "phase\\s*=?\\s*(NR|PR).*day\\s*=?\\s*X?([0-9]+).*Order\\s*=?\\s*([12])"

  grab <- function(x) {
    utils::strcapture(
      pattern = pattern,
      x = x,
      proto = data.frame(phase = character(), day = character(), Order = character(),
                         stringsAsFactors = FALSE)
    )
  }

  L <- do.call(rbind, lapply(lr, function(z) grab(z[1])))
  R <- do.call(rbind, lapply(lr, function(z) grab(z[2])))

  df$contrast_pretty <- paste0(L$phase, L$day, " Order", L$Order,
                               " - ",
                               R$phase, R$day, " Order", R$Order)
  df
}

all_pairs_holm_df  <- prettify_contrasts(all_pairs_holm)
all_pairs_tukey_df <- prettify_contrasts(all_pairs_tukey)

write.csv(all_pairs_holm_df,
          file.path(outdir, "ALL_pairwise_phase_day_Order_HOLM.csv"),
          row.names = FALSE)
write.csv(all_pairs_tukey_df,
          file.path(outdir, "ALL_pairwise_phase_day_Order_TUKEY.csv"),
          row.names = FALSE)










################################################################################################################################################################################################################# 
######################## MEAL Frequency COMBINED #####################################################################################################
###################################################################################################################################################################

library(tidyr)
library(dplyr)
library(afex)
library(emmeans)
library(ggplot2)

# Load the dataset
data <- read.csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\MEAL\\meal_freq_per_day\\meal_freq_realigned_FINAL.csv")

# Step 1: Reshape the data
long_data <- data %>%
  pivot_longer(cols = starts_with(c("NR", "PR")),
               names_to = "time_phase",
               values_to = "value")

# Step 2: Generate Descriptive Statistics
descriptive_stats <- long_data %>%
  group_by(time_phase, Order) %>%
  summarise(mean_value = mean(value, na.rm = TRUE),
            sd_value = sd(value, na.rm = TRUE),
            n = n())

# Save descriptive statistics to CSV
write.csv(descriptive_stats, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\MEAL\\meal_freq_per_day\\COMBINE_MEALFreq_trend_descriptive_stats.csv")

# Step 3: Run ANOVA for each time point, sex, and order
anova_results <- aov_car(value ~ time_phase * Order + Error(Mouse/time_phase), data = long_data)

# Save ANOVA results to CSV
write.csv(as.data.frame(anova(anova_results)), "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\MEAL\\meal_freq_per_day\\COMBINE_MEALFreq_trend_anova_results.csv")

# Step 4: Post-hoc tests with Tukey and Holm adjustments
# Tukey adjustment
posthoc_tukey_results <- emmeans(anova_results, pairwise ~ time_phase  * Order, adjust = "Tukey")
posthoc_tukey_df <- as.data.frame(summary(posthoc_tukey_results$contrasts))

# Save Tukey-adjusted post-hoc results to CSV
write.csv(posthoc_tukey_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\MEAL\\meal_freq_per_day\\COMBINE_MEALFreq_trend_posthoc_results_Tukey.csv")

# Holm adjustment
posthoc_holm_results <- emmeans(anova_results, pairwise ~ time_phase * Order, adjust = "holm")
posthoc_holm_df <- as.data.frame(summary(posthoc_holm_results$contrasts))

# Save Holm-adjusted post-hoc results to CSV
write.csv(posthoc_holm_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\MEAL\\meal_freq_per_day\\COMBINE_MEALFreq_trend_posthoc_results_Holm.csv")


###############################################################################################################################################################################
##################################################################### MEALS FREQUENCY RE_DO#########################################################################################


# --- Packages & options ---
library(tidyverse)
library(afex)
library(emmeans)
library(broom)

afex::afex_options(type = 3)                  # Type-III SS
options(contrasts = c("contr.sum","contr.poly"))

# --- Paths   ---
infile <- "C:/Users/hta031/Github/FEDProtein/results/FIVE/LINE_PLOTS/MEAL/meal_freq_per_day/meal_freq_realigned_FINAL.csv"
outdir <- "C:/Users/hta031/Github/FEDProtein/results/FIVE/LINE_PLOTS/MEAL/meal_freq_per_day/RE_DO_MEALFREQUENCY"
infile <- trimws(infile); outdir <- trimws(outdir)
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

# --- Load & reshape (NR/PR + day parsed cleanly) ---
dat <- read.csv(infile)

long <- dat %>%
  pivot_longer(
    cols = matches("^(NR|PR)\\d+$"),
    names_to        = c("phase","day"),
    names_pattern   = "(NR|PR)(\\d+)",
    names_transform = list(day = as.integer),
    values_to       = "value"
  ) %>%
  mutate(
    day   = factor(day, levels = sort(unique(day))),   # categorical for RM design
    phase = factor(phase, levels = c("NR","PR")),
    Order = factor(Order),
    Sex   = factor(Sex),
    Mouse = factor(Mouse)
  ) %>%
  drop_na(value)

# --- Descriptives per (Order × phase × day) ---
descriptive <- long %>%
  group_by(Order, phase, day) %>%
  summarise(n = sum(!is.na(value)),
            mean = mean(value, na.rm = TRUE),
            sd   = sd(value, na.rm = TRUE),
            .groups = "drop")
write.csv(descriptive, file.path(outdir, "COMBINE_MealFrequency_descriptives.csv"), row.names = FALSE)

# --- Repeated-measures ANOVA ---
include_sex <- FALSE
between_fml <- if (include_sex) c("Order","Sex") else "Order"

fit <- aov_ez(
  id      = "Mouse",
  dv      = "value",
  within  = c("phase","day"),
  between = between_fml,
  data    = long
)

anova_tab <- afex::nice(fit, es = "pes", correction = "GG")
write.csv(anova_tab, file.path(outdir, "COMBINE_MealFrequency_ANOVA_GG.csv"), row.names = FALSE)

# --- EMMs for all cells (phase × day × Order) ---
em <- emmeans(fit, ~ phase*day*Order)

# Save the grid (helps inspect labels; day may appear as X0..X6)
grid <- as.data.frame(em)[, c("phase","day","Order")]
write.csv(grid, file.path(outdir, "EMM_grid_Pellets.csv"), row.names = FALSE)

# --- Helper to build a contrast vector A - B (robust to 'X' in day labels) ---
# A, B are c(phase, day, Order) with day like "0","1","2"
mkL <- function(em, A, B) {
  g <- as.data.frame(em)[, c("phase","day","Order")]
  g <- data.frame(lapply(g, function(x) trimws(as.character(x))), stringsAsFactors = FALSE)
  names(g) <- c("phase","day","Order")
  g$day_plain <- sub("^X", "", g$day)  # strip leading 'X' if present

  iA <- which(g$phase==A[1] & g$day_plain==A[2] & g$Order==A[3])
  iB <- which(g$phase==B[1] & g$day_plain==B[2] & g$Order==B[3])

  if (length(iA)!=1 || length(iB)!=1) {
    message("Could not find requested cells. First rows of the EMM grid:")
    print(utils::head(g, 12))
    stop(sprintf("Can't find cells:\n  A = %s\n  B = %s",
                 paste(A, collapse=","), paste(B, collapse=",")))
  }
  L <- rep(0, nrow(g)); L[iA] <- 1; L[iB] <- -1; L
}

# --- SPECIFIC CONTRASTS (Holm-adjusted) --------------------------------------

# Helper to format readable labels like "PR0 Order2"
fmt <- function(phase, day, ord) sprintf("%s%s Order%s", phase, day, ord)

# 1) PR0 (Order 2) vs every other day in Order 2
#    -> PR0 vs PR1..PR6 and PR0 vs NR0..NR6
cts_PR0O2_vs_allO2 <- list()
# PR0 vs PR1..PR6 (within PR phase)
for (d in 1:6) {
  name <- paste(fmt("PR","0","2"), "-", fmt("PR", as.character(d), "2"))
  cts_PR0O2_vs_allO2[[name]] <- mkL(em, c("PR","0","2"), c("PR", as.character(d), "2"))
}
# PR0 vs NR0..NR6 (across to NR phase)
for (d in 0:6) {
  name <- paste(fmt("PR","0","2"), "-", fmt("NR", as.character(d), "2"))
  cts_PR0O2_vs_allO2[[name]] <- mkL(em, c("PR","0","2"), c("NR", as.character(d), "2"))
}

res_PR0O2_vs_allO2 <- summary(contrast(em, cts_PR0O2_vs_allO2), adjust = "holm")
write.csv(as.data.frame(res_PR0O2_vs_allO2),
          file.path(outdir, "SPEC_O2_PR0_vs_all_Order2_days.csv"),
          row.names = FALSE)

# 2) NR0 (Order 2) vs every other NR day in Order 2
#    -> NR0 vs NR1..NR6
cts_NR0O2_vs_NR_O2 <- list()
for (d in 1:6) {
  name <- paste(fmt("NR","0","2"), "-", fmt("NR", as.character(d), "2"))
  cts_NR0O2_vs_NR_O2[[name]] <- mkL(em, c("NR","0","2"), c("NR", as.character(d), "2"))
}

res_NR0O2_vs_NR_O2 <- summary(contrast(em, cts_NR0O2_vs_NR_O2), adjust = "holm")
write.csv(as.data.frame(res_NR0O2_vs_NR_O2),
          file.path(outdir, "SPEC_O2_NR0_vs_NR1to6_in_Order2.csv"),
          row.names = FALSE)

# 3) Matched-day cross-order, cross-phase contrasts
#    a) PRk (Order 2) vs NRk (Order 1), for k = 0..6
#    b) NRk (Order 2) vs PRk (Order 1), for k = 0..6
cts_O2PRk_vs_O1NRk <- list()
for (d in 0:6) {
  name <- paste(fmt("PR", as.character(d), "2"), "-", fmt("NR", as.character(d), "1"))
  cts_O2PRk_vs_O1NRk[[name]] <- mkL(em, c("PR", as.character(d), "2"),
                                         c("NR", as.character(d), "1"))
}
res_O2PRk_vs_O1NRk <- summary(contrast(em, cts_O2PRk_vs_O1NRk), adjust = "holm")
write.csv(as.data.frame(res_O2PRk_vs_O1NRk),
          file.path(outdir, "SPEC_O2_PRk_vs_O1_NRk_matched_days.csv"),
          row.names = FALSE)

cts_O2NRk_vs_O1PRk <- list()
for (d in 0:6) {
  name <- paste(fmt("NR", as.character(d), "2"), "-", fmt("PR", as.character(d), "1"))
  cts_O2NRk_vs_O1PRk[[name]] <- mkL(em, c("NR", as.character(d), "2"),
                                         c("PR", as.character(d), "1"))
}
res_O2NRk_vs_O1PRk <- summary(contrast(em, cts_O2NRk_vs_O1PRk), adjust = "holm")
write.csv(as.data.frame(res_O2NRk_vs_O1PRk),
          file.path(outdir, "SPEC_O2_NRk_vs_O1_PRk_matched_days.csv"),
          row.names = FALSE)


# === ALL pairwise comparisons across phase × day × Order (28 cells → 378 tests) ===
em_cells <- emmeans(fit, ~ phase:day:Order)  # each combo as one level

all_pairs_holm  <- summary(pairs(em_cells, adjust = "holm"))
all_pairs_tukey <- summary(pairs(em_cells, adjust = "tukey"))

# Prettify the contrast labels to look like "PR0 Order1 - NR3 Order2"
prettify_contrasts <- function(df) {
  df <- as.data.frame(df)
  lr <- strsplit(df$contrast, " - ", fixed = TRUE)

  pattern <- "phase\\s*=?\\s*(NR|PR).*day\\s*=?\\s*X?([0-9]+).*Order\\s*=?\\s*([12])"

  grab <- function(x) {
    utils::strcapture(
      pattern = pattern,
      x = x,
      proto = data.frame(phase = character(), day = character(), Order = character(),
                         stringsAsFactors = FALSE)
    )
  }

  L <- do.call(rbind, lapply(lr, function(z) grab(z[1])))
  R <- do.call(rbind, lapply(lr, function(z) grab(z[2])))

  df$contrast_pretty <- paste0(L$phase, L$day, " Order", L$Order,
                               " - ",
                               R$phase, R$day, " Order", R$Order)
  df
}

all_pairs_holm_df  <- prettify_contrasts(all_pairs_holm)
all_pairs_tukey_df <- prettify_contrasts(all_pairs_tukey)

write.csv(all_pairs_holm_df,
          file.path(outdir, "ALL_pairwise_phase_day_Order_HOLM.csv"),
          row.names = FALSE)
write.csv(all_pairs_tukey_df,
          file.path(outdir, "ALL_pairwise_phase_day_Order_TUKEY.csv"),
          row.names = FALSE)



################################################################################################################################################################################################################# 
######################## MEAL size COMBINED #####################################################################################################
###################################################################################################################################################################

library(tidyr)
library(dplyr)
library(afex)
library(emmeans)
library(ggplot2)

# Load the dataset
data <- read.csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\MEAL\\meal_size_per_day\\meal_size_realigned_FINAL.csv")

# Step 1: Reshape the data
long_data <- data %>%
  pivot_longer(cols = starts_with(c("NR", "PR")),
               names_to = "time_phase",
               values_to = "value")

# Step 2: Generate Descriptive Statistics
descriptive_stats <- long_data %>%
  group_by(time_phase, Order) %>%
  summarise(mean_value = mean(value, na.rm = TRUE),
            sd_value = sd(value, na.rm = TRUE),
            n = n())

# Save descriptive statistics to CSV
write.csv(descriptive_stats, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\MEAL\\meal_size_per_day\\COMBINE_MEALsize_trend_descriptive_stats.csv")

# Step 3: Run ANOVA for each time point, sex, and order
anova_results <- aov_car(value ~ time_phase * Order + Error(Mouse/time_phase), data = long_data)

# Save ANOVA results to CSV
write.csv(as.data.frame(anova(anova_results)), "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\MEAL\\meal_size_per_day\\COMBINE_MEALsize_trend_anova_results.csv")

# Step 4: Post-hoc tests with Tukey and Holm adjustments
# Tukey adjustment
posthoc_tukey_results <- emmeans(anova_results, pairwise ~ time_phase  * Order, adjust = "Tukey")
posthoc_tukey_df <- as.data.frame(summary(posthoc_tukey_results$contrasts))

# Save Tukey-adjusted post-hoc results to CSV
write.csv(posthoc_tukey_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\MEAL\\meal_size_per_day\\COMBINE_MEALsize_trend_posthoc_results_Tukey.csv")

# Holm adjustment
posthoc_holm_results <- emmeans(anova_results, pairwise ~ time_phase * Order, adjust = "holm")
posthoc_holm_df <- as.data.frame(summary(posthoc_holm_results$contrasts))

# Save Holm-adjusted post-hoc results to CSV
write.csv(posthoc_holm_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\MEAL\\meal_size_per_day\\COMBINE_MEALsize_trend_posthoc_results_Holm.csv")




###############################################################################################################################################################################
##################################################################### MEALS SIZE RE_DO#########################################################################################


# --- Packages & options ---
library(tidyverse)
library(afex)
library(emmeans)
library(broom)

afex::afex_options(type = 3)                  # Type-III SS
options(contrasts = c("contr.sum","contr.poly"))

# --- Paths   ---
infile <- "C:/Users/hta031/Github/FEDProtein/results/FIVE/LINE_PLOTS/MEAL/meal_size_per_day/meal_size_realigned_FINAL.csv"
outdir <- "C:/Users/hta031/Github/FEDProtein/results/FIVE/LINE_PLOTS/MEAL/meal_size_per_day/RE_DO_MEAL_SIZE"
infile <- trimws(infile); outdir <- trimws(outdir)
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

# --- Load & reshape (NR/PR + day parsed cleanly) ---
dat <- read.csv(infile)

long <- dat %>%
  pivot_longer(
    cols = matches("^(NR|PR)\\d+$"),
    names_to        = c("phase","day"),
    names_pattern   = "(NR|PR)(\\d+)",
    names_transform = list(day = as.integer),
    values_to       = "value"
  ) %>%
  mutate(
    day   = factor(day, levels = sort(unique(day))),   # categorical for RM design
    phase = factor(phase, levels = c("NR","PR")),
    Order = factor(Order),
    Sex   = factor(Sex),
    Mouse = factor(Mouse)
  ) %>%
  drop_na(value)

# --- Descriptives per (Order × phase × day) ---
descriptive <- long %>%
  group_by(Order, phase, day) %>%
  summarise(n = sum(!is.na(value)),
            mean = mean(value, na.rm = TRUE),
            sd   = sd(value, na.rm = TRUE),
            .groups = "drop")
write.csv(descriptive, file.path(outdir, "COMBINE_MealSize_descriptives.csv"), row.names = FALSE)

# --- Repeated-measures ANOVA ---
include_sex <- FALSE
between_fml <- if (include_sex) c("Order","Sex") else "Order"

fit <- aov_ez(
  id      = "Mouse",
  dv      = "value",
  within  = c("phase","day"),
  between = between_fml,
  data    = long
)

anova_tab <- afex::nice(fit, es = "pes", correction = "GG")
write.csv(anova_tab, file.path(outdir, "COMBINE_MealSize_ANOVA_GG.csv"), row.names = FALSE)

# --- EMMs for all cells (phase × day × Order) ---
em <- emmeans(fit, ~ phase*day*Order)

# Save the grid (helps inspect labels; day may appear as X0..X6)
grid <- as.data.frame(em)[, c("phase","day","Order")]
write.csv(grid, file.path(outdir, "EMM_grid_Pellets.csv"), row.names = FALSE)

# --- Helper to build a contrast vector A - B (robust to 'X' in day labels) ---
# A, B are c(phase, day, Order) with day like "0","1","2"
mkL <- function(em, A, B) {
  g <- as.data.frame(em)[, c("phase","day","Order")]
  g <- data.frame(lapply(g, function(x) trimws(as.character(x))), stringsAsFactors = FALSE)
  names(g) <- c("phase","day","Order")
  g$day_plain <- sub("^X", "", g$day)  # strip leading 'X' if present

  iA <- which(g$phase==A[1] & g$day_plain==A[2] & g$Order==A[3])
  iB <- which(g$phase==B[1] & g$day_plain==B[2] & g$Order==B[3])

  if (length(iA)!=1 || length(iB)!=1) {
    message("Could not find requested cells. First rows of the EMM grid:")
    print(utils::head(g, 12))
    stop(sprintf("Can't find cells:\n  A = %s\n  B = %s",
                 paste(A, collapse=","), paste(B, collapse=",")))
  }
  L <- rep(0, nrow(g)); L[iA] <- 1; L[iB] <- -1; L
}

# --- SPECIFIC CONTRASTS (Holm-adjusted) --------------------------------------

# Helper to format readable labels like "PR0 Order2"
fmt <- function(phase, day, ord) sprintf("%s%s Order%s", phase, day, ord)

# 1) PR0 (Order 2) vs every other day in Order 2
#    -> PR0 vs PR1..PR6 and PR0 vs NR0..NR6
cts_PR0O2_vs_allO2 <- list()
# PR0 vs PR1..PR6 (within PR phase)
for (d in 1:6) {
  name <- paste(fmt("PR","0","2"), "-", fmt("PR", as.character(d), "2"))
  cts_PR0O2_vs_allO2[[name]] <- mkL(em, c("PR","0","2"), c("PR", as.character(d), "2"))
}
# PR0 vs NR0..NR6 (across to NR phase)
for (d in 0:6) {
  name <- paste(fmt("PR","0","2"), "-", fmt("NR", as.character(d), "2"))
  cts_PR0O2_vs_allO2[[name]] <- mkL(em, c("PR","0","2"), c("NR", as.character(d), "2"))
}

res_PR0O2_vs_allO2 <- summary(contrast(em, cts_PR0O2_vs_allO2), adjust = "holm")
write.csv(as.data.frame(res_PR0O2_vs_allO2),
          file.path(outdir, "SPEC_O2_PR0_vs_all_Order2_days.csv"),
          row.names = FALSE)

# 2) NR0 (Order 2) vs every other NR day in Order 2
#    -> NR0 vs NR1..NR6
cts_NR0O2_vs_NR_O2 <- list()
for (d in 1:6) {
  name <- paste(fmt("NR","0","2"), "-", fmt("NR", as.character(d), "2"))
  cts_NR0O2_vs_NR_O2[[name]] <- mkL(em, c("NR","0","2"), c("NR", as.character(d), "2"))
}

res_NR0O2_vs_NR_O2 <- summary(contrast(em, cts_NR0O2_vs_NR_O2), adjust = "holm")
write.csv(as.data.frame(res_NR0O2_vs_NR_O2),
          file.path(outdir, "SPEC_O2_NR0_vs_NR1to6_in_Order2.csv"),
          row.names = FALSE)

# 3) Matched-day cross-order, cross-phase contrasts
#    a) PRk (Order 2) vs NRk (Order 1), for k = 0..6
#    b) NRk (Order 2) vs PRk (Order 1), for k = 0..6
cts_O2PRk_vs_O1NRk <- list()
for (d in 0:6) {
  name <- paste(fmt("PR", as.character(d), "2"), "-", fmt("NR", as.character(d), "1"))
  cts_O2PRk_vs_O1NRk[[name]] <- mkL(em, c("PR", as.character(d), "2"),
                                         c("NR", as.character(d), "1"))
}
res_O2PRk_vs_O1NRk <- summary(contrast(em, cts_O2PRk_vs_O1NRk), adjust = "holm")
write.csv(as.data.frame(res_O2PRk_vs_O1NRk),
          file.path(outdir, "SPEC_O2_PRk_vs_O1_NRk_matched_days.csv"),
          row.names = FALSE)

cts_O2NRk_vs_O1PRk <- list()
for (d in 0:6) {
  name <- paste(fmt("NR", as.character(d), "2"), "-", fmt("PR", as.character(d), "1"))
  cts_O2NRk_vs_O1PRk[[name]] <- mkL(em, c("NR", as.character(d), "2"),
                                         c("PR", as.character(d), "1"))
}
res_O2NRk_vs_O1PRk <- summary(contrast(em, cts_O2NRk_vs_O1PRk), adjust = "holm")
write.csv(as.data.frame(res_O2NRk_vs_O1PRk),
          file.path(outdir, "SPEC_O2_NRk_vs_O1_PRk_matched_days.csv"),
          row.names = FALSE)


# === ALL pairwise comparisons across phase × day × Order (28 cells → 378 tests) ===
em_cells <- emmeans(fit, ~ phase:day:Order)  # each combo as one level

all_pairs_holm  <- summary(pairs(em_cells, adjust = "holm"))
all_pairs_tukey <- summary(pairs(em_cells, adjust = "tukey"))

# Prettify the contrast labels to look like "PR0 Order1 - NR3 Order2"
prettify_contrasts <- function(df) {
  df <- as.data.frame(df)
  lr <- strsplit(df$contrast, " - ", fixed = TRUE)

  pattern <- "phase\\s*=?\\s*(NR|PR).*day\\s*=?\\s*X?([0-9]+).*Order\\s*=?\\s*([12])"

  grab <- function(x) {
    utils::strcapture(
      pattern = pattern,
      x = x,
      proto = data.frame(phase = character(), day = character(), Order = character(),
                         stringsAsFactors = FALSE)
    )
  }

  L <- do.call(rbind, lapply(lr, function(z) grab(z[1])))
  R <- do.call(rbind, lapply(lr, function(z) grab(z[2])))

  df$contrast_pretty <- paste0(L$phase, L$day, " Order", L$Order,
                               " - ",
                               R$phase, R$day, " Order", R$Order)
  df
}

all_pairs_holm_df  <- prettify_contrasts(all_pairs_holm)
all_pairs_tukey_df <- prettify_contrasts(all_pairs_tukey)

write.csv(all_pairs_holm_df,
          file.path(outdir, "ALL_pairwise_phase_day_Order_HOLM.csv"),
          row.names = FALSE)
write.csv(all_pairs_tukey_df,
          file.path(outdir, "ALL_pairwise_phase_day_Order_TUKEY.csv"),
          row.names = FALSE)



###############################################################################################################################################################################################
############################################################################ FEAST BELOW #################################################################################################
###############################################################################################################################################################################################

library(tidyr)
library(dplyr)
library(afex)
library(emmeans)
library(ggplot2)

# Load the dataset
data <- read.csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\FEAST\\mega_meals_per_day_trend.csv")

# Step 1: Reshape the data
long_data <- data %>%
  pivot_longer(cols = starts_with(c("NR", "PR")),
               names_to = "time_phase",
               values_to = "value")

# Step 2: Generate Descriptive Statistics
descriptive_stats <- long_data %>%
  group_by(time_phase, Sex, Order) %>%
  summarise(mean_value = mean(value, na.rm = TRUE),
            sd_value = sd(value, na.rm = TRUE),
            n = n())

# Save descriptive statistics to CSV
write.csv(descriptive_stats, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\FEAST\\FEAST_trend_descriptive_stats.csv")

# Step 3: Run ANOVA for each time point, sex, and order
anova_results <- aov_car(value ~ time_phase * Sex * Order + Error(Mouse/time_phase), data = long_data)

# Save ANOVA results to CSV
write.csv(as.data.frame(anova(anova_results)), "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\FEAST\\FEAST_trend_anova_results.csv")

# Step 4: Post-hoc tests with Tukey and Holm adjustments
# Tukey adjustment
posthoc_tukey_results <- emmeans(anova_results, pairwise ~ time_phase * Sex * Order, adjust = "Tukey")
posthoc_tukey_df <- as.data.frame(summary(posthoc_tukey_results$contrasts))

# Save Tukey-adjusted post-hoc results to CSV
write.csv(posthoc_tukey_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\FEAST\\FEAST_trend_posthoc_results_Tukey.csv")

# Holm adjustment
posthoc_holm_results <- emmeans(anova_results, pairwise ~ time_phase * Sex * Order, adjust = "holm")
posthoc_holm_df <- as.data.frame(summary(posthoc_holm_results$contrasts))

# Save Holm-adjusted post-hoc results to CSV
write.csv(posthoc_holm_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\FEAST\\FEAST_trend_posthoc_results_Holm.csv")












###############################################################################################################################################################################
##################################################################### FEAST NUMBER RE_DO#########################################################################################

###############################################################################################################################################################################
##################################################################### FEAST NUMBER — with Sex ##################################################################################

# --- Packages & options ---
library(tidyverse)
library(afex)
library(emmeans)
library(broom)

afex::afex_options(type = 3)                  # Type-III SS
options(contrasts = c("contr.sum","contr.poly"))

# --- Paths   ---
infile <- "C:/Users/hta031/Github/FEDProtein/results/FIVE/LINE_PLOTS/FEAST/mega_meals_per_day_trend.csv"
outdir <- "C:/Users/hta031/Github/FEDProtein/results/FIVE/LINE_PLOTS/FEAST/FEAST_NUMBER_RE_DO"
infile <- trimws(infile); outdir <- trimws(outdir)
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

# --- Load & reshape (NR/PR + day parsed cleanly) ---
dat <- read.csv(infile)

long <- dat %>%
  pivot_longer(
    cols = matches("^(NR|PR)\\d+$"),
    names_to        = c("phase","day"),
    names_pattern   = "(NR|PR)(\\d+)",
    names_transform = list(day = as.integer),
    values_to       = "value"
  ) %>%
  mutate(
    day   = factor(day, levels = sort(unique(day))),
    phase = factor(phase, levels = c("NR","PR")),
    Order = factor(Order),
    Sex   = factor(Sex),
    Mouse = factor(Mouse)
  ) %>%
  drop_na(value)

# --- Descriptives per (Order × Sex × phase × day) ---
descriptive <- long %>%
  group_by(Order, Sex, phase, day) %>%
  summarise(n = sum(!is.na(value)),
            mean = mean(value, na.rm = TRUE),
            sd   = sd(value, na.rm = TRUE),
            .groups = "drop")
write.csv(descriptive, file.path(outdir, "FeastNumber_descriptives_by_Order_Sex_phase_day.csv"), row.names = FALSE)

# --- Repeated-measures ANOVA (Sex included) ---
fit <- aov_ez(
  id      = "Mouse",
  dv      = "value",
  within  = c("phase","day"),
  between = c("Order","Sex"),
  data    = long
)

anova_tab <- afex::nice(fit, es = "pes", correction = "GG")
write.csv(anova_tab, file.path(outdir, "FeastNumber_ANOVA_GG_with_Sex.csv"), row.names = FALSE)

# --- EMMs for all cells (phase × day × Order × Sex) ---
em <- emmeans(fit, ~ phase*day*Order*Sex)

# Save the grid (helps inspect labels; day may appear as X0..X6)
grid <- as.data.frame(em)[, c("phase","day","Order","Sex")]
write.csv(grid, file.path(outdir, "EMM_grid_FeastNumber_with_Sex.csv"), row.names = FALSE)

# --- Helper to build a contrast vector A - B (robust to 'X' in day labels) ---
# A, B are c(phase, day, Order, Sex) with day like "0","1","2", Sex like "M"/"F"
mkL4 <- function(em, A, B) {
  g <- as.data.frame(em)[, c("phase","day","Order","Sex")]
  g <- data.frame(lapply(g, function(x) trimws(as.character(x))), stringsAsFactors = FALSE)
  names(g) <- c("phase","day","Order","Sex")
  g$day_plain <- sub("^X", "", g$day)  # strip leading 'X' if present

  iA <- which(g$phase==A[1] & g$day_plain==A[2] & g$Order==A[3] & g$Sex==A[4])
  iB <- which(g$phase==B[1] & g$day_plain==B[2] & g$Order==B[3] & g$Sex==B[4])

  if (length(iA)!=1 || length(iB)!=1) {
    message("Could not find requested cells. First rows of the EMM grid:")
    print(utils::head(g, 12))
    stop(sprintf("Can't find cells:\n  A = %s\n  B = %s",
                 paste(A, collapse=","), paste(B, collapse=",")))
  }
  L <- rep(0, nrow(g)); L[iA] <- 1; L[iB] <- -1; L
}

# --- SPECIFIC CONTRASTS (Holm-adjusted), computed PER SEX -------------------------------------------------------
fmt <- function(phase, day, ord, sex) sprintf("%s%s Order%s Sex%s", phase, day, ord, sex)

sex_levels <- levels(long$Sex)  # typically c("M","F")

# Containers to optionally bind results across sexes (if you want single files later)
all_PR0_vs_allO2  <- list()
all_NR0_vs_NRO2   <- list()
all_PRk_vs_NRk    <- list()
all_NRk_vs_PRk    <- list()

for (sx in sex_levels) {

  # 1) PR0 (Order 2, Sex = sx) vs every other day in Order 2, Sex = sx
  cts_PR0O2_vs_allO2 <- list()
  for (d in 1:6) {
    name <- paste(fmt("PR","0","2", sx), "-", fmt("PR", as.character(d), "2", sx))
    cts_PR0O2_vs_allO2[[name]] <- mkL4(em, c("PR","0","2", sx), c("PR", as.character(d), "2", sx))
  }
  for (d in 0:6) {
    name <- paste(fmt("PR","0","2", sx), "-", fmt("NR", as.character(d), "2", sx))
    cts_PR0O2_vs_allO2[[name]] <- mkL4(em, c("PR","0","2", sx), c("NR", as.character(d), "2", sx))
  }
  res_PR0O2_vs_allO2 <- summary(contrast(em, cts_PR0O2_vs_allO2), adjust = "holm")
  res_PR0O2_vs_allO2 <- transform(as.data.frame(res_PR0O2_vs_allO2), Sex = sx)
  write.csv(res_PR0O2_vs_allO2,
            file.path(outdir, paste0("SPEC_O2_PR0_vs_all_Order2_days_Sex", sx, ".csv")),
            row.names = FALSE)
  all_PR0_vs_allO2[[sx]] <- res_PR0O2_vs_allO2

  # 2) NR0 (Order 2, Sex = sx) vs NR1..NR6 in Order 2, Sex = sx
  cts_NR0O2_vs_NR_O2 <- setNames(lapply(1:6, function(d)
    mkL4(em, c("NR","0","2", sx), c("NR", as.character(d), "2", sx))),
    paste0(fmt("NR","0","2", sx), " - ", fmt("NR","", "2", sx)) # label updated below
  )
  names(cts_NR0O2_vs_NR_O2) <- paste(fmt("NR","0","2", sx), "-", paste0("NR", 1:6, " Order2 Sex", sx))
  res_NR0O2_vs_NR_O2 <- summary(contrast(em, cts_NR0O2_vs_NR_O2), adjust = "holm")
  res_NR0O2_vs_NR_O2 <- transform(as.data.frame(res_NR0O2_vs_NR_O2), Sex = sx)
  write.csv(res_NR0O2_vs_NR_O2,
            file.path(outdir, paste0("SPEC_O2_NR0_vs_NR1to6_in_Order2_Sex", sx, ".csv")),
            row.names = FALSE)
  all_NR0_vs_NRO2[[sx]] <- res_NR0O2_vs_NR_O2

  # 3a) Matched-day: PRk (Order 2, Sex = sx) vs NRk (Order 1, Sex = sx)
  cts_O2PRk_vs_O1NRk <- setNames(lapply(0:6, function(d)
    mkL4(em, c("PR", as.character(d), "2", sx),
             c("NR", as.character(d), "1", sx))),
    paste0("PR", 0:6, " Order2 Sex", sx, " - NR", 0:6, " Order1 Sex", sx)
  )
  res_O2PRk_vs_O1NRk <- summary(contrast(em, cts_O2PRk_vs_O1NRk), adjust = "holm")
  res_O2PRk_vs_O1NRk <- transform(as.data.frame(res_O2PRk_vs_O1NRk), Sex = sx)
  write.csv(res_O2PRk_vs_O1NRk,
            file.path(outdir, paste0("SPEC_O2_PRk_vs_O1_NRk_matched_days_Sex", sx, ".csv")),
            row.names = FALSE)
  all_PRk_vs_NRk[[sx]] <- res_O2PRk_vs_O1NRk

  # 3b) Matched-day: NRk (Order 2, Sex = sx) vs PRk (Order 1, Sex = sx)
  cts_O2NRk_vs_O1PRk <- setNames(lapply(0:6, function(d)
    mkL4(em, c("NR", as.character(d), "2", sx),
             c("PR", as.character(d), "1", sx))),
    paste0("NR", 0:6, " Order2 Sex", sx, " - PR", 0:6, " Order1 Sex", sx)
  )
  res_O2NRk_vs_O1PRk <- summary(contrast(em, cts_O2NRk_vs_O1PRk), adjust = "holm")
  res_O2NRk_vs_O1PRk <- transform(as.data.frame(res_O2NRk_vs_O1PRk), Sex = sx)
  write.csv(res_O2NRk_vs_O1PRk,
            file.path(outdir, paste0("SPEC_O2_NRk_vs_O1_PRk_matched_days_Sex", sx, ".csv")),
            row.names = FALSE)
  all_NRk_vs_PRk[[sx]] <- res_O2NRk_vs_O1PRk
}

# If you also want combined tables across both sexes (each row has a Sex column), uncomment:
# write.csv(bind_rows(all_PR0_vs_allO2), file.path(outdir, "SPEC_O2_PR0_vs_all_Order2_days_ALL_SEX.csv"), row.names = FALSE)
# write.csv(bind_rows(all_NR0_vs_NRO2), file.path(outdir, "SPEC_O2_NR0_vs_NR1to6_in_Order2_ALL_SEX.csv"), row.names = FALSE)
# write.csv(bind_rows(all_PRk_vs_NRk),  file.path(outdir, "SPEC_O2_PRk_vs_O1_NRk_matched_days_ALL_SEX.csv"), row.names = FALSE)
# write.csv(bind_rows(all_NRk_vs_PRk),  file.path(outdir, "SPEC_O2_NRk_vs_O1_PRk_matched_days_ALL_SEX.csv"), row.names = FALSE)

# === ALL pairwise comparisons across phase × day × Order × Sex =================
em_cells <- emmeans(fit, ~ phase:day:Order:Sex)  # each combo as one level

all_pairs_holm  <- summary(pairs(em_cells, adjust = "holm"))
all_pairs_tukey <- summary(pairs(em_cells, adjust = "tukey"))

# Prettify the contrast labels to look like "PR0 Order1 SexM - NR3 Order2 SexF" (if ever cross-sex)
prettify_contrasts4 <- function(df) {
  df <- as.data.frame(df)
  lr <- strsplit(df$contrast, " - ", fixed = TRUE)

  pattern <- "phase\\s*=?\\s*(NR|PR).*day\\s*=?\\s*X?([0-9]+).*Order\\s*=?\\s*([12]).*Sex\\s*=?\\s*([MF])"

  grab <- function(x) {
    utils::strcapture(
      pattern = pattern,
      x = x,
      proto = data.frame(phase = character(), day = character(), Order = character(), Sex = character(),
                         stringsAsFactors = FALSE)
    )
  }

  L <- do.call(rbind, lapply(lr, function(z) grab(z[1])))
  R <- do.call(rbind, lapply(lr, function(z) grab(z[2])))

  df$contrast_pretty <- paste0(L$phase, L$day, " Order", L$Order, " Sex", L$Sex,
                               " - ",
                               R$phase, R$day, " Order", R$Order, " Sex", R$Sex)
  df
}

all_pairs_holm_df  <- prettify_contrasts4(all_pairs_holm)
all_pairs_tukey_df <- prettify_contrasts4(all_pairs_tukey)

write.csv(all_pairs_holm_df,
          file.path(outdir, "ALL_pairwise_phase_day_Order_Sex_HOLM.csv"),
          row.names = FALSE)
write.csv(all_pairs_tukey_df,
          file.path(outdir, "ALL_pairwise_phase_day_Order_Sex_TUKEY.csv"),
          row.names = FALSE)














###############################################################################################################################################################################################
############################################################################ FEAST Frequency BELOW #################################################################################################
###############################################################################################################################################################################################

library(tidyr)
library(dplyr)
library(afex)
library(emmeans)
library(ggplot2)

# Load the dataset
data <- read.csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\FEAST\\feast_freq_per_day\\mega_meal_freq_realigned_FINAL.csv")

# Step 1: Reshape the data
long_data <- data %>%
  pivot_longer(cols = starts_with(c("NR", "PR")),
               names_to = "time_phase",
               values_to = "value")

# Step 2: Generate Descriptive Statistics
descriptive_stats <- long_data %>%
  group_by(time_phase, Sex, Order) %>%
  summarise(mean_value = mean(value, na.rm = TRUE),
            sd_value = sd(value, na.rm = TRUE),
            n = n())

# Save descriptive statistics to CSV
write.csv(descriptive_stats, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\FEAST\\feast_freq_per_day\\FEASTFreq_trend_descriptive_stats.csv")

# Step 3: Run ANOVA for each time point, sex, and order
anova_results <- aov_car(value ~ time_phase * Sex * Order + Error(Mouse/time_phase), data = long_data)

# Save ANOVA results to CSV
write.csv(as.data.frame(anova(anova_results)), "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\FEAST\\feast_freq_per_day\\FEASTFreq_trend_anova_results.csv")

# Step 4: Post-hoc tests with Tukey and Holm adjustments
# Tukey adjustment
posthoc_tukey_results <- emmeans(anova_results, pairwise ~ time_phase * Sex * Order, adjust = "Tukey")
posthoc_tukey_df <- as.data.frame(summary(posthoc_tukey_results$contrasts))

# Save Tukey-adjusted post-hoc results to CSV
write.csv(posthoc_tukey_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\FEAST\\feast_freq_per_day\\FEASTFreq_trend_posthoc_results_Tukey.csv")

# Holm adjustment
posthoc_holm_results <- emmeans(anova_results, pairwise ~ time_phase * Sex * Order, adjust = "holm")
posthoc_holm_df <- as.data.frame(summary(posthoc_holm_results$contrasts))

# Save Holm-adjusted post-hoc results to CSV
write.csv(posthoc_holm_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\FEAST\\feast_freq_per_day\\FEASTFreq_trend_posthoc_results_Holm.csv")

###############################################################################################################################################################################
##################################################################### FEAST FREQ RE_DO#########################################################################################

###############################################################################################################################################################################
##################################################################### FEAST FREQ with Sex ##################################################################################

# --- Packages & options ---
library(tidyverse)
library(afex)
library(emmeans)
library(broom)

afex::afex_options(type = 3)                  # Type-III SS
options(contrasts = c("contr.sum","contr.poly"))

# --- Paths   ---
infile <- "C:/Users/hta031/Github/FEDProtein/results/FIVE/LINE_PLOTS/FEAST/feast_freq_per_day/mega_meal_freq_realigned_FINAL.csv"
outdir <- "C:/Users/hta031/Github/FEDProtein/results/FIVE/LINE_PLOTS/FEAST/feast_freq_per_day/FEAST_FREQ_RE_DO"
infile <- trimws(infile); outdir <- trimws(outdir)
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

# --- Load & reshape (NR/PR + day parsed cleanly) ---
dat <- read.csv(infile)

long <- dat %>%
  pivot_longer(
    cols = matches("^(NR|PR)\\d+$"),
    names_to        = c("phase","day"),
    names_pattern   = "(NR|PR)(\\d+)",
    names_transform = list(day = as.integer),
    values_to       = "value"
  ) %>%
  mutate(
    day   = factor(day, levels = sort(unique(day))),
    phase = factor(phase, levels = c("NR","PR")),
    Order = factor(Order),
    Sex   = factor(Sex),
    Mouse = factor(Mouse)
  ) %>%
  drop_na(value)

# --- Descriptives per (Order × Sex × phase × day) ---
descriptive <- long %>%
  group_by(Order, Sex, phase, day) %>%
  summarise(n = sum(!is.na(value)),
            mean = mean(value, na.rm = TRUE),
            sd   = sd(value, na.rm = TRUE),
            .groups = "drop")
write.csv(descriptive, file.path(outdir, "Feastfreq_descriptives_by_Order_Sex_phase_day.csv"), row.names = FALSE)

# --- Repeated-measures ANOVA (Sex included) ---
fit <- aov_ez(
  id      = "Mouse",
  dv      = "value",
  within  = c("phase","day"),
  between = c("Order","Sex"),
  data    = long
)

anova_tab <- afex::nice(fit, es = "pes", correction = "GG")
write.csv(anova_tab, file.path(outdir, "Feastfreq_ANOVA_GG_with_Sex.csv"), row.names = FALSE)

# --- EMMs for all cells (phase × day × Order × Sex) ---
em <- emmeans(fit, ~ phase*day*Order*Sex)

# Save the grid (helps inspect labels; day may appear as X0..X6)
grid <- as.data.frame(em)[, c("phase","day","Order","Sex")]
write.csv(grid, file.path(outdir, "EMM_grid_Feastfreq_with_Sex.csv"), row.names = FALSE)

# --- Helper to build a contrast vector A - B (robust to 'X' in day labels) ---
# A, B are c(phase, day, Order, Sex) with day like "0","1","2", Sex like "M"/"F"
mkL4 <- function(em, A, B) {
  g <- as.data.frame(em)[, c("phase","day","Order","Sex")]
  g <- data.frame(lapply(g, function(x) trimws(as.character(x))), stringsAsFactors = FALSE)
  names(g) <- c("phase","day","Order","Sex")
  g$day_plain <- sub("^X", "", g$day)  # strip leading 'X' if present

  iA <- which(g$phase==A[1] & g$day_plain==A[2] & g$Order==A[3] & g$Sex==A[4])
  iB <- which(g$phase==B[1] & g$day_plain==B[2] & g$Order==B[3] & g$Sex==B[4])

  if (length(iA)!=1 || length(iB)!=1) {
    message("Could not find requested cells. First rows of the EMM grid:")
    print(utils::head(g, 12))
    stop(sprintf("Can't find cells:\n  A = %s\n  B = %s",
                 paste(A, collapse=","), paste(B, collapse=",")))
  }
  L <- rep(0, nrow(g)); L[iA] <- 1; L[iB] <- -1; L
}

# --- SPECIFIC CONTRASTS (Holm-adjusted), computed PER SEX -------------------------------------------------------
fmt <- function(phase, day, ord, sex) sprintf("%s%s Order%s Sex%s", phase, day, ord, sex)

sex_levels <- levels(long$Sex)  # typically c("M","F")

# Containers to optionally bind results across sexes (if you want single files later)
all_PR0_vs_allO2  <- list()
all_NR0_vs_NRO2   <- list()
all_PRk_vs_NRk    <- list()
all_NRk_vs_PRk    <- list()

for (sx in sex_levels) {

  # 1) PR0 (Order 2, Sex = sx) vs every other day in Order 2, Sex = sx
  cts_PR0O2_vs_allO2 <- list()
  for (d in 1:6) {
    name <- paste(fmt("PR","0","2", sx), "-", fmt("PR", as.character(d), "2", sx))
    cts_PR0O2_vs_allO2[[name]] <- mkL4(em, c("PR","0","2", sx), c("PR", as.character(d), "2", sx))
  }
  for (d in 0:6) {
    name <- paste(fmt("PR","0","2", sx), "-", fmt("NR", as.character(d), "2", sx))
    cts_PR0O2_vs_allO2[[name]] <- mkL4(em, c("PR","0","2", sx), c("NR", as.character(d), "2", sx))
  }
  res_PR0O2_vs_allO2 <- summary(contrast(em, cts_PR0O2_vs_allO2), adjust = "holm")
  res_PR0O2_vs_allO2 <- transform(as.data.frame(res_PR0O2_vs_allO2), Sex = sx)
  write.csv(res_PR0O2_vs_allO2,
            file.path(outdir, paste0("SPEC_O2_PR0_vs_all_Order2_days_Sex", sx, ".csv")),
            row.names = FALSE)
  all_PR0_vs_allO2[[sx]] <- res_PR0O2_vs_allO2

  # 2) NR0 (Order 2, Sex = sx) vs NR1..NR6 in Order 2, Sex = sx
  cts_NR0O2_vs_NR_O2 <- setNames(lapply(1:6, function(d)
    mkL4(em, c("NR","0","2", sx), c("NR", as.character(d), "2", sx))),
    paste0(fmt("NR","0","2", sx), " - ", fmt("NR","", "2", sx)) # label updated below
  )
  names(cts_NR0O2_vs_NR_O2) <- paste(fmt("NR","0","2", sx), "-", paste0("NR", 1:6, " Order2 Sex", sx))
  res_NR0O2_vs_NR_O2 <- summary(contrast(em, cts_NR0O2_vs_NR_O2), adjust = "holm")
  res_NR0O2_vs_NR_O2 <- transform(as.data.frame(res_NR0O2_vs_NR_O2), Sex = sx)
  write.csv(res_NR0O2_vs_NR_O2,
            file.path(outdir, paste0("SPEC_O2_NR0_vs_NR1to6_in_Order2_Sex", sx, ".csv")),
            row.names = FALSE)
  all_NR0_vs_NRO2[[sx]] <- res_NR0O2_vs_NR_O2

  # 3a) Matched-day: PRk (Order 2, Sex = sx) vs NRk (Order 1, Sex = sx)
  cts_O2PRk_vs_O1NRk <- setNames(lapply(0:6, function(d)
    mkL4(em, c("PR", as.character(d), "2", sx),
             c("NR", as.character(d), "1", sx))),
    paste0("PR", 0:6, " Order2 Sex", sx, " - NR", 0:6, " Order1 Sex", sx)
  )
  res_O2PRk_vs_O1NRk <- summary(contrast(em, cts_O2PRk_vs_O1NRk), adjust = "holm")
  res_O2PRk_vs_O1NRk <- transform(as.data.frame(res_O2PRk_vs_O1NRk), Sex = sx)
  write.csv(res_O2PRk_vs_O1NRk,
            file.path(outdir, paste0("SPEC_O2_PRk_vs_O1_NRk_matched_days_Sex", sx, ".csv")),
            row.names = FALSE)
  all_PRk_vs_NRk[[sx]] <- res_O2PRk_vs_O1NRk

  # 3b) Matched-day: NRk (Order 2, Sex = sx) vs PRk (Order 1, Sex = sx)
  cts_O2NRk_vs_O1PRk <- setNames(lapply(0:6, function(d)
    mkL4(em, c("NR", as.character(d), "2", sx),
             c("PR", as.character(d), "1", sx))),
    paste0("NR", 0:6, " Order2 Sex", sx, " - PR", 0:6, " Order1 Sex", sx)
  )
  res_O2NRk_vs_O1PRk <- summary(contrast(em, cts_O2NRk_vs_O1PRk), adjust = "holm")
  res_O2NRk_vs_O1PRk <- transform(as.data.frame(res_O2NRk_vs_O1PRk), Sex = sx)
  write.csv(res_O2NRk_vs_O1PRk,
            file.path(outdir, paste0("SPEC_O2_NRk_vs_O1_PRk_matched_days_Sex", sx, ".csv")),
            row.names = FALSE)
  all_NRk_vs_PRk[[sx]] <- res_O2NRk_vs_O1PRk
}

# If you also want combined tables across both sexes (each row has a Sex column), uncomment:
# write.csv(bind_rows(all_PR0_vs_allO2), file.path(outdir, "SPEC_O2_PR0_vs_all_Order2_days_ALL_SEX.csv"), row.names = FALSE)
# write.csv(bind_rows(all_NR0_vs_NRO2), file.path(outdir, "SPEC_O2_NR0_vs_NR1to6_in_Order2_ALL_SEX.csv"), row.names = FALSE)
# write.csv(bind_rows(all_PRk_vs_NRk),  file.path(outdir, "SPEC_O2_PRk_vs_O1_NRk_matched_days_ALL_SEX.csv"), row.names = FALSE)
# write.csv(bind_rows(all_NRk_vs_PRk),  file.path(outdir, "SPEC_O2_NRk_vs_O1_PRk_matched_days_ALL_SEX.csv"), row.names = FALSE)

# === ALL pairwise comparisons across phase × day × Order × Sex =================
em_cells <- emmeans(fit, ~ phase:day:Order:Sex)  # each combo as one level

all_pairs_holm  <- summary(pairs(em_cells, adjust = "holm"))
all_pairs_tukey <- summary(pairs(em_cells, adjust = "tukey"))

# Prettify the contrast labels to look like "PR0 Order1 SexM - NR3 Order2 SexF" (if ever cross-sex)
prettify_contrasts4 <- function(df) {
  df <- as.data.frame(df)
  lr <- strsplit(df$contrast, " - ", fixed = TRUE)

  pattern <- "phase\\s*=?\\s*(NR|PR).*day\\s*=?\\s*X?([0-9]+).*Order\\s*=?\\s*([12]).*Sex\\s*=?\\s*([MF])"

  grab <- function(x) {
    utils::strcapture(
      pattern = pattern,
      x = x,
      proto = data.frame(phase = character(), day = character(), Order = character(), Sex = character(),
                         stringsAsFactors = FALSE)
    )
  }

  L <- do.call(rbind, lapply(lr, function(z) grab(z[1])))
  R <- do.call(rbind, lapply(lr, function(z) grab(z[2])))

  df$contrast_pretty <- paste0(L$phase, L$day, " Order", L$Order, " Sex", L$Sex,
                               " - ",
                               R$phase, R$day, " Order", R$Order, " Sex", R$Sex)
  df
}

all_pairs_holm_df  <- prettify_contrasts4(all_pairs_holm)
all_pairs_tukey_df <- prettify_contrasts4(all_pairs_tukey)

write.csv(all_pairs_holm_df,
          file.path(outdir, "ALL_pairwise_phase_day_Order_Sex_HOLM.csv"),
          row.names = FALSE)
write.csv(all_pairs_tukey_df,
          file.path(outdir, "ALL_pairwise_phase_day_Order_Sex_TUKEY.csv"),
          row.names = FALSE)






###############################################################################################################################################################################
##################################################################### FEAST SIZE RE_DO#########################################################################################

###############################################################################################################################################################################
##################################################################### FEAST SIZE— with Sex ##################################################################################

# --- Packages & options ---
library(tidyverse)
library(afex)
library(emmeans)
library(broom)

afex::afex_options(type = 3)                  # Type-III SS
options(contrasts = c("contr.sum","contr.poly"))

# --- Paths   ---
infile <- "C:/Users/hta031/Github/FEDProtein/results/FIVE/LINE_PLOTS/FEAST/feast_size_per_day/mega_meal_size_realigned_FINAL.csv"
outdir <- "C:/Users/hta031/Github/FEDProtein/results/FIVE/LINE_PLOTS/FEAST/feast_size_per_day/FEAST_SIZE_RE_DO"
infile <- trimws(infile); outdir <- trimws(outdir)
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

# --- Load & reshape (NR/PR + day parsed cleanly) ---
dat <- read.csv(infile)

long <- dat %>%
  pivot_longer(
    cols = matches("^(NR|PR)\\d+$"),
    names_to        = c("phase","day"),
    names_pattern   = "(NR|PR)(\\d+)",
    names_transform = list(day = as.integer),
    values_to       = "value"
  ) %>%
  mutate(
    day   = factor(day, levels = sort(unique(day))),
    phase = factor(phase, levels = c("NR","PR")),
    Order = factor(Order),
    Sex   = factor(Sex),
    Mouse = factor(Mouse)
  ) %>%
  drop_na(value)

# --- Descriptives per (Order × Sex × phase × day) ---
descriptive <- long %>%
  group_by(Order, Sex, phase, day) %>%
  summarise(n = sum(!is.na(value)),
            mean = mean(value, na.rm = TRUE),
            sd   = sd(value, na.rm = TRUE),
            .groups = "drop")
write.csv(descriptive, file.path(outdir, "Feastsize_descriptives_by_Order_Sex_phase_day.csv"), row.names = FALSE)

# --- Repeated-measures ANOVA (Sex included) ---
fit <- aov_ez(
  id      = "Mouse",
  dv      = "value",
  within  = c("phase","day"),
  between = c("Order","Sex"),
  data    = long
)

anova_tab <- afex::nice(fit, es = "pes", correction = "GG")
write.csv(anova_tab, file.path(outdir, "Feastsize_ANOVA_GG_with_Sex.csv"), row.names = FALSE)

# --- EMMs for all cells (phase × day × Order × Sex) ---
em <- emmeans(fit, ~ phase*day*Order*Sex)

# Save the grid (helps inspect labels; day may appear as X0..X6)
grid <- as.data.frame(em)[, c("phase","day","Order","Sex")]
write.csv(grid, file.path(outdir, "EMM_grid_Feastsize_with_Sex.csv"), row.names = FALSE)

# --- Helper to build a contrast vector A - B (robust to 'X' in day labels) ---
# A, B are c(phase, day, Order, Sex) with day like "0","1","2", Sex like "M"/"F"
mkL4 <- function(em, A, B) {
  g <- as.data.frame(em)[, c("phase","day","Order","Sex")]
  g <- data.frame(lapply(g, function(x) trimws(as.character(x))), stringsAsFactors = FALSE)
  names(g) <- c("phase","day","Order","Sex")
  g$day_plain <- sub("^X", "", g$day)  # strip leading 'X' if present

  iA <- which(g$phase==A[1] & g$day_plain==A[2] & g$Order==A[3] & g$Sex==A[4])
  iB <- which(g$phase==B[1] & g$day_plain==B[2] & g$Order==B[3] & g$Sex==B[4])

  if (length(iA)!=1 || length(iB)!=1) {
    message("Could not find requested cells. First rows of the EMM grid:")
    print(utils::head(g, 12))
    stop(sprintf("Can't find cells:\n  A = %s\n  B = %s",
                 paste(A, collapse=","), paste(B, collapse=",")))
  }
  L <- rep(0, nrow(g)); L[iA] <- 1; L[iB] <- -1; L
}

# --- SPECIFIC CONTRASTS (Holm-adjusted), computed PER SEX -------------------------------------------------------
fmt <- function(phase, day, ord, sex) sprintf("%s%s Order%s Sex%s", phase, day, ord, sex)

sex_levels <- levels(long$Sex)  # typically c("M","F")

# Containers to optionally bind results across sexes (if you want single files later)
all_PR0_vs_allO2  <- list()
all_NR0_vs_NRO2   <- list()
all_PRk_vs_NRk    <- list()
all_NRk_vs_PRk    <- list()

for (sx in sex_levels) {

  # 1) PR0 (Order 2, Sex = sx) vs every other day in Order 2, Sex = sx
  cts_PR0O2_vs_allO2 <- list()
  for (d in 1:6) {
    name <- paste(fmt("PR","0","2", sx), "-", fmt("PR", as.character(d), "2", sx))
    cts_PR0O2_vs_allO2[[name]] <- mkL4(em, c("PR","0","2", sx), c("PR", as.character(d), "2", sx))
  }
  for (d in 0:6) {
    name <- paste(fmt("PR","0","2", sx), "-", fmt("NR", as.character(d), "2", sx))
    cts_PR0O2_vs_allO2[[name]] <- mkL4(em, c("PR","0","2", sx), c("NR", as.character(d), "2", sx))
  }
  res_PR0O2_vs_allO2 <- summary(contrast(em, cts_PR0O2_vs_allO2), adjust = "holm")
  res_PR0O2_vs_allO2 <- transform(as.data.frame(res_PR0O2_vs_allO2), Sex = sx)
  write.csv(res_PR0O2_vs_allO2,
            file.path(outdir, paste0("SPEC_O2_PR0_vs_all_Order2_days_Sex", sx, ".csv")),
            row.names = FALSE)
  all_PR0_vs_allO2[[sx]] <- res_PR0O2_vs_allO2

  # 2) NR0 (Order 2, Sex = sx) vs NR1..NR6 in Order 2, Sex = sx
  cts_NR0O2_vs_NR_O2 <- setNames(lapply(1:6, function(d)
    mkL4(em, c("NR","0","2", sx), c("NR", as.character(d), "2", sx))),
    paste0(fmt("NR","0","2", sx), " - ", fmt("NR","", "2", sx)) # label updated below
  )
  names(cts_NR0O2_vs_NR_O2) <- paste(fmt("NR","0","2", sx), "-", paste0("NR", 1:6, " Order2 Sex", sx))
  res_NR0O2_vs_NR_O2 <- summary(contrast(em, cts_NR0O2_vs_NR_O2), adjust = "holm")
  res_NR0O2_vs_NR_O2 <- transform(as.data.frame(res_NR0O2_vs_NR_O2), Sex = sx)
  write.csv(res_NR0O2_vs_NR_O2,
            file.path(outdir, paste0("SPEC_O2_NR0_vs_NR1to6_in_Order2_Sex", sx, ".csv")),
            row.names = FALSE)
  all_NR0_vs_NRO2[[sx]] <- res_NR0O2_vs_NR_O2

  # 3a) Matched-day: PRk (Order 2, Sex = sx) vs NRk (Order 1, Sex = sx)
  cts_O2PRk_vs_O1NRk <- setNames(lapply(0:6, function(d)
    mkL4(em, c("PR", as.character(d), "2", sx),
             c("NR", as.character(d), "1", sx))),
    paste0("PR", 0:6, " Order2 Sex", sx, " - NR", 0:6, " Order1 Sex", sx)
  )
  res_O2PRk_vs_O1NRk <- summary(contrast(em, cts_O2PRk_vs_O1NRk), adjust = "holm")
  res_O2PRk_vs_O1NRk <- transform(as.data.frame(res_O2PRk_vs_O1NRk), Sex = sx)
  write.csv(res_O2PRk_vs_O1NRk,
            file.path(outdir, paste0("SPEC_O2_PRk_vs_O1_NRk_matched_days_Sex", sx, ".csv")),
            row.names = FALSE)
  all_PRk_vs_NRk[[sx]] <- res_O2PRk_vs_O1NRk

  # 3b) Matched-day: NRk (Order 2, Sex = sx) vs PRk (Order 1, Sex = sx)
  cts_O2NRk_vs_O1PRk <- setNames(lapply(0:6, function(d)
    mkL4(em, c("NR", as.character(d), "2", sx),
             c("PR", as.character(d), "1", sx))),
    paste0("NR", 0:6, " Order2 Sex", sx, " - PR", 0:6, " Order1 Sex", sx)
  )
  res_O2NRk_vs_O1PRk <- summary(contrast(em, cts_O2NRk_vs_O1PRk), adjust = "holm")
  res_O2NRk_vs_O1PRk <- transform(as.data.frame(res_O2NRk_vs_O1PRk), Sex = sx)
  write.csv(res_O2NRk_vs_O1PRk,
            file.path(outdir, paste0("SPEC_O2_NRk_vs_O1_PRk_matched_days_Sex", sx, ".csv")),
            row.names = FALSE)
  all_NRk_vs_PRk[[sx]] <- res_O2NRk_vs_O1PRk
}

# If you also want combined tables across both sexes (each row has a Sex column), uncomment:
# write.csv(bind_rows(all_PR0_vs_allO2), file.path(outdir, "SPEC_O2_PR0_vs_all_Order2_days_ALL_SEX.csv"), row.names = FALSE)
# write.csv(bind_rows(all_NR0_vs_NRO2), file.path(outdir, "SPEC_O2_NR0_vs_NR1to6_in_Order2_ALL_SEX.csv"), row.names = FALSE)
# write.csv(bind_rows(all_PRk_vs_NRk),  file.path(outdir, "SPEC_O2_PRk_vs_O1_NRk_matched_days_ALL_SEX.csv"), row.names = FALSE)
# write.csv(bind_rows(all_NRk_vs_PRk),  file.path(outdir, "SPEC_O2_NRk_vs_O1_PRk_matched_days_ALL_SEX.csv"), row.names = FALSE)

# === ALL pairwise comparisons across phase × day × Order × Sex =================
em_cells <- emmeans(fit, ~ phase:day:Order:Sex)  # each combo as one level

all_pairs_holm  <- summary(pairs(em_cells, adjust = "holm"))
all_pairs_tukey <- summary(pairs(em_cells, adjust = "tukey"))

# Prettify the contrast labels to look like "PR0 Order1 SexM - NR3 Order2 SexF" (if ever cross-sex)
prettify_contrasts4 <- function(df) {
  df <- as.data.frame(df)
  lr <- strsplit(df$contrast, " - ", fixed = TRUE)

  pattern <- "phase\\s*=?\\s*(NR|PR).*day\\s*=?\\s*X?([0-9]+).*Order\\s*=?\\s*([12]).*Sex\\s*=?\\s*([MF])"

  grab <- function(x) {
    utils::strcapture(
      pattern = pattern,
      x = x,
      proto = data.frame(phase = character(), day = character(), Order = character(), Sex = character(),
                         stringsAsFactors = FALSE)
    )
  }

  L <- do.call(rbind, lapply(lr, function(z) grab(z[1])))
  R <- do.call(rbind, lapply(lr, function(z) grab(z[2])))

  df$contrast_pretty <- paste0(L$phase, L$day, " Order", L$Order, " Sex", L$Sex,
                               " - ",
                               R$phase, R$day, " Order", R$Order, " Sex", R$Sex)
  df
}

all_pairs_holm_df  <- prettify_contrasts4(all_pairs_holm)
all_pairs_tukey_df <- prettify_contrasts4(all_pairs_tukey)

write.csv(all_pairs_holm_df,
          file.path(outdir, "ALL_pairwise_phase_day_Order_Sex_HOLM.csv"),
          row.names = FALSE)
write.csv(all_pairs_tukey_df,
          file.path(outdir, "ALL_pairwise_phase_day_Order_Sex_TUKEY.csv"),
          row.names = FALSE)



















###############################################################################################################################################################################################
############################################################################ FEAST SIZE BELOW #################################################################################################
###############################################################################################################################################################################################

library(tidyr)
library(dplyr)
library(afex)
library(emmeans)
library(ggplot2)

# Load the dataset
data <- read.csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\FEAST\\feast_size_per_day\\mega_meal_size_realigned_FINAL.csv")

# Step 1: Reshape the data
long_data <- data %>%
  pivot_longer(cols = starts_with(c("NR", "PR")),
               names_to = "time_phase",
               values_to = "value")

# Step 2: Generate Descriptive Statistics
descriptive_stats <- long_data %>%
  group_by(time_phase, Sex, Order) %>%
  summarise(mean_value = mean(value, na.rm = TRUE),
            sd_value = sd(value, na.rm = TRUE),
            n = n())

# Save descriptive statistics to CSV
write.csv(descriptive_stats, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\FEAST\\feast_size_per_day\\FEASTSIZE_trend_descriptive_stats.csv")

# Step 3: Run ANOVA for each time point, sex, and order
anova_results <- aov_car(value ~ time_phase * Sex * Order + Error(Mouse/time_phase), data = long_data)

# Save ANOVA results to CSV
write.csv(as.data.frame(anova(anova_results)), "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\FEAST\\feast_size_per_day\\FEASTSIZE_trend_anova_results.csv")

# Step 4: Post-hoc tests with Tukey and Holm adjustments
# Tukey adjustment
posthoc_tukey_results <- emmeans(anova_results, pairwise ~ time_phase * Sex * Order, adjust = "Tukey")
posthoc_tukey_df <- as.data.frame(summary(posthoc_tukey_results$contrasts))

# Save Tukey-adjusted post-hoc results to CSV
write.csv(posthoc_tukey_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\FEAST\\feast_size_per_day\\FEASTSIZE_trend_posthoc_results_Tukey.csv")

# Holm adjustment
posthoc_holm_results <- emmeans(anova_results, pairwise ~ time_phase * Sex * Order, adjust = "holm")
posthoc_holm_df <- as.data.frame(summary(posthoc_holm_results$contrasts))

# Save Holm-adjusted post-hoc results to CSV
write.csv(posthoc_holm_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\FEAST\\feast_size_per_day\\FEASTSIZE_trend_posthoc_results_Holm.csv")

###############################################################################################################################################################################################
############################################################################ FEAST COMBINED #################################################################################################
####################################################################################HOWEVER ANOVA SHOWS Interaction between sex and Timephase###########################################################################################################

library(tidyr)
library(dplyr)
library(afex)
library(emmeans)
library(ggplot2)

# Load the dataset
data <- read.csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\FEAST\\mega_meals_per_day_trend.csv")

# Step 1: Reshape the data
long_data <- data %>%
  pivot_longer(cols = starts_with(c("NR", "PR")),
               names_to = "time_phase",
               values_to = "value")

# Step 2: Generate Descriptive Statistics
descriptive_stats <- long_data %>%
  group_by(time_phase, Order) %>%
  summarise(mean_value = mean(value, na.rm = TRUE),
            sd_value = sd(value, na.rm = TRUE),
            n = n())

# Save descriptive statistics to CSV
write.csv(descriptive_stats, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\FEAST\\COMBINE_FEAST_trend_descriptive_stats.csv")

# Step 3: Run ANOVA for each time point, sex, and order
anova_results <- aov_car(value ~ time_phase * Order + Error(Mouse/time_phase), data = long_data)

# Save ANOVA results to CSV
write.csv(as.data.frame(anova(anova_results)), "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\FEAST\\COMBINE_FEAST_trend_anova_results.csv")

# Step 4: Post-hoc tests with Tukey and Holm adjustments
# Tukey adjustment
posthoc_tukey_results <- emmeans(anova_results, pairwise ~ time_phase  * Order, adjust = "Tukey")
posthoc_tukey_df <- as.data.frame(summary(posthoc_tukey_results$contrasts))

# Save Tukey-adjusted post-hoc results to CSV
write.csv(posthoc_tukey_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\FEAST\\COMBINE_FEAST_trend_posthoc_results_Tukey.csv")

# Holm adjustment
posthoc_holm_results <- emmeans(anova_results, pairwise ~ time_phase * Order, adjust = "holm")
posthoc_holm_df <- as.data.frame(summary(posthoc_holm_results$contrasts))

# Save Holm-adjusted post-hoc results to CSV
write.csv(posthoc_holm_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\FEAST\\COMBINE_FEAST_trend_posthoc_results_Holm.csv")






###############################################################################################################################################################################################
############################################################################ INTERACTION TIME BELOW #################################################################################################
###############################################################################################################################################################################################


library(tidyr)
library(dplyr)
library(afex)
library(emmeans)
library(ggplot2)

# Load the dataset
data <- read.csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\INTERACTION_TIME\\INTER_TIME.csv")

# Step 1: Reshape the data
long_data <- data %>%
  pivot_longer(cols = starts_with(c("NR", "PR")),
               names_to = "time_phase",
               values_to = "value")

# Step 2: Generate Descriptive Statistics
descriptive_stats <- long_data %>%
  group_by(time_phase, sex, order) %>%
  summarise(mean_value = mean(value, na.rm = TRUE),
            sd_value = sd(value, na.rm = TRUE),
            n = n())

# Save descriptive statistics to CSV
write.csv(descriptive_stats, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\INTERACTION_TIME\\INTERTIME_trend_descriptive_stats.csv")

# Step 3: Run ANOVA for each time point, sex, and order
anova_results <- aov_car(value ~ time_phase * sex * order + Error(mouse_id/time_phase), data = long_data)

# Save ANOVA results to CSV
write.csv(as.data.frame(anova(anova_results)), "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\INTERACTION_TIME\\INTERTIME_trend_anova_results.csv")

# Step 4: Post-hoc tests with Tukey and Holm adjustments
# Tukey adjustment
posthoc_tukey_results <- emmeans(anova_results, pairwise ~ time_phase * sex * order, adjust = "Tukey")
posthoc_tukey_df <- as.data.frame(summary(posthoc_tukey_results$contrasts))

# Save Tukey-adjusted post-hoc results to CSV
write.csv(posthoc_tukey_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\INTERACTION_TIME\\INTERTIME_trend_posthoc_results_Tukey.csv")

# Holm adjustment
posthoc_holm_results <- emmeans(anova_results, pairwise ~ time_phase * sex * order, adjust = "holm")
posthoc_holm_df <- as.data.frame(summary(posthoc_holm_results$contrasts))

# Save Holm-adjusted post-hoc results to CSV
write.csv(posthoc_holm_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\INTERACTION_TIME\\INTERTIME_trend_posthoc_results_Holm.csv")



###############################################################################################################################################################################################
############################################################################ INTERACTION TIME COMBINED #################################################################################################    
#######################################################################################ALTHOUGH THERE ARE INTERACTIONS WITH SEX########################################################################################################



library(tidyr)
library(dplyr)
library(afex)
library(emmeans)
library(ggplot2)

# Load the dataset
data <- read.csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\INTERACTION_TIME\\INTER_TIME.csv")

# Step 1: Reshape the data
long_data <- data %>%
  pivot_longer(cols = starts_with(c("NR", "PR")),
               names_to = "time_phase",
               values_to = "value")

# Step 2: Generate Descriptive Statistics
descriptive_stats <- long_data %>%
  group_by(time_phase, order) %>%
  summarise(mean_value = mean(value, na.rm = TRUE),
            sd_value = sd(value, na.rm = TRUE),
            n = n())

# Save descriptive statistics to CSV
write.csv(descriptive_stats, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\INTERACTION_TIME\\COMBINE_INTERTIME_trend_descriptive_stats.csv")

# Step 3: Run ANOVA for each time point, sex, and order
anova_results <- aov_car(value ~ time_phase  * order + Error(mouse_id/time_phase), data = long_data)

# Save ANOVA results to CSV
write.csv(as.data.frame(anova(anova_results)), "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\INTERACTION_TIME\\COMBINE_INTERTIME_trend_anova_results.csv")

# Step 4: Post-hoc tests with Tukey and Holm adjustments
# Tukey adjustment
posthoc_tukey_results <- emmeans(anova_results, pairwise ~ time_phase * order, adjust = "Tukey")
posthoc_tukey_df <- as.data.frame(summary(posthoc_tukey_results$contrasts))

# Save Tukey-adjusted post-hoc results to CSV
write.csv(posthoc_tukey_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\INTERACTION_TIME\\COMBINE_INTERTIME_trend_posthoc_results_Tukey.csv")

# Holm adjustment
posthoc_holm_results <- emmeans(anova_results, pairwise ~ time_phase * order, adjust = "holm")
posthoc_holm_df <- as.data.frame(summary(posthoc_holm_results$contrasts))

# Save Holm-adjusted post-hoc results to CSV
write.csv(posthoc_holm_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\INTERACTION_TIME\\COMBINE_INTERTIME_trend_posthoc_results_Holm.csv")



###############################################################################################################################################################################
##################################################################### INTERACTION TIME RE_DO#########################################################################################


# --- Packages & options ---
library(tidyverse)
library(afex)
library(emmeans)
library(broom)

afex::afex_options(type = 3)                  # Type-III SS
options(contrasts = c("contr.sum","contr.poly"))

# --- Paths   ---
infile <- "C:/Users/hta031/Github/FEDProtein/results/FIVE/LINE_PLOTS/INTERACTION_TIME/INTER_TIME.csv"
outdir <- "C:/Users/hta031/Github/FEDProtein/results/FIVE/LINE_PLOTS/INTERACTION_TIME/RE_DO"
infile <- trimws(infile); outdir <- trimws(outdir)
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

# --- Load & reshape (NR/PR + day parsed cleanly) ---
dat <- read.csv(infile)

long <- dat %>%
  pivot_longer(
    cols = matches("^(NR|PR)\\d+$"),
    names_to        = c("phase","day"),
    names_pattern   = "(NR|PR)(\\d+)",
    names_transform = list(day = as.integer),
    values_to       = "value"
  ) %>%
  mutate(
    day   = factor(day, levels = sort(unique(day))),   # categorical for RM design
    phase = factor(phase, levels = c("NR","PR")),
    Order = factor(Order),
    Sex   = factor(Sex),
    Mouse = factor(Mouse)
  ) %>%
  drop_na(value)

# --- Descriptives per (Order × phase × day) ---
descriptive <- long %>%
  group_by(Order, phase, day) %>%
  summarise(n = sum(!is.na(value)),
            mean = mean(value, na.rm = TRUE),
            sd   = sd(value, na.rm = TRUE),
            .groups = "drop")
write.csv(descriptive, file.path(outdir, "COMBINE_InteractionTime_descriptives.csv"), row.names = FALSE)

# --- Repeated-measures ANOVA ---
include_sex <- FALSE
between_fml <- if (include_sex) c("Order","Sex") else "Order"

fit <- aov_ez(
  id      = "Mouse",
  dv      = "value",
  within  = c("phase","day"),
  between = between_fml,
  data    = long
)

anova_tab <- afex::nice(fit, es = "pes", correction = "GG")
write.csv(anova_tab, file.path(outdir, "COMBINE_InteractionTime_ANOVA_GG.csv"), row.names = FALSE)

# --- EMMs for all cells (phase × day × Order) ---
em <- emmeans(fit, ~ phase*day*Order)

# Save the grid (helps inspect labels; day may appear as X0..X6)
grid <- as.data.frame(em)[, c("phase","day","Order")]
write.csv(grid, file.path(outdir, "EMM_grid_Pellets.csv"), row.names = FALSE)

# --- Helper to build a contrast vector A - B (robust to 'X' in day labels) ---
# A, B are c(phase, day, Order) with day like "0","1","2"
mkL <- function(em, A, B) {
  g <- as.data.frame(em)[, c("phase","day","Order")]
  g <- data.frame(lapply(g, function(x) trimws(as.character(x))), stringsAsFactors = FALSE)
  names(g) <- c("phase","day","Order")
  g$day_plain <- sub("^X", "", g$day)  # strip leading 'X' if present

  iA <- which(g$phase==A[1] & g$day_plain==A[2] & g$Order==A[3])
  iB <- which(g$phase==B[1] & g$day_plain==B[2] & g$Order==B[3])

  if (length(iA)!=1 || length(iB)!=1) {
    message("Could not find requested cells. First rows of the EMM grid:")
    print(utils::head(g, 12))
    stop(sprintf("Can't find cells:\n  A = %s\n  B = %s",
                 paste(A, collapse=","), paste(B, collapse=",")))
  }
  L <- rep(0, nrow(g)); L[iA] <- 1; L[iB] <- -1; L
}

# --- SPECIFIC CONTRASTS (Holm-adjusted) --------------------------------------

# Helper to format readable labels like "PR0 Order2"
fmt <- function(phase, day, ord) sprintf("%s%s Order%s", phase, day, ord)

# 1) PR0 (Order 2) vs every other day in Order 2
#    -> PR0 vs PR1..PR6 and PR0 vs NR0..NR6
cts_PR0O2_vs_allO2 <- list()
# PR0 vs PR1..PR6 (within PR phase)
for (d in 1:6) {
  name <- paste(fmt("PR","0","2"), "-", fmt("PR", as.character(d), "2"))
  cts_PR0O2_vs_allO2[[name]] <- mkL(em, c("PR","0","2"), c("PR", as.character(d), "2"))
}
# PR0 vs NR0..NR6 (across to NR phase)
for (d in 0:6) {
  name <- paste(fmt("PR","0","2"), "-", fmt("NR", as.character(d), "2"))
  cts_PR0O2_vs_allO2[[name]] <- mkL(em, c("PR","0","2"), c("NR", as.character(d), "2"))
}

res_PR0O2_vs_allO2 <- summary(contrast(em, cts_PR0O2_vs_allO2), adjust = "holm")
write.csv(as.data.frame(res_PR0O2_vs_allO2),
          file.path(outdir, "SPEC_O2_PR0_vs_all_Order2_days.csv"),
          row.names = FALSE)

# 2) NR0 (Order 2) vs every other NR day in Order 2
#    -> NR0 vs NR1..NR6
cts_NR0O2_vs_NR_O2 <- list()
for (d in 1:6) {
  name <- paste(fmt("NR","0","2"), "-", fmt("NR", as.character(d), "2"))
  cts_NR0O2_vs_NR_O2[[name]] <- mkL(em, c("NR","0","2"), c("NR", as.character(d), "2"))
}

res_NR0O2_vs_NR_O2 <- summary(contrast(em, cts_NR0O2_vs_NR_O2), adjust = "holm")
write.csv(as.data.frame(res_NR0O2_vs_NR_O2),
          file.path(outdir, "SPEC_O2_NR0_vs_NR1to6_in_Order2.csv"),
          row.names = FALSE)

# 3) Matched-day cross-order, cross-phase contrasts
#    a) PRk (Order 2) vs NRk (Order 1), for k = 0..6
#    b) NRk (Order 2) vs PRk (Order 1), for k = 0..6
cts_O2PRk_vs_O1NRk <- list()
for (d in 0:6) {
  name <- paste(fmt("PR", as.character(d), "2"), "-", fmt("NR", as.character(d), "1"))
  cts_O2PRk_vs_O1NRk[[name]] <- mkL(em, c("PR", as.character(d), "2"),
                                         c("NR", as.character(d), "1"))
}
res_O2PRk_vs_O1NRk <- summary(contrast(em, cts_O2PRk_vs_O1NRk), adjust = "holm")
write.csv(as.data.frame(res_O2PRk_vs_O1NRk),
          file.path(outdir, "SPEC_O2_PRk_vs_O1_NRk_matched_days.csv"),
          row.names = FALSE)

cts_O2NRk_vs_O1PRk <- list()
for (d in 0:6) {
  name <- paste(fmt("NR", as.character(d), "2"), "-", fmt("PR", as.character(d), "1"))
  cts_O2NRk_vs_O1PRk[[name]] <- mkL(em, c("NR", as.character(d), "2"),
                                         c("PR", as.character(d), "1"))
}
res_O2NRk_vs_O1PRk <- summary(contrast(em, cts_O2NRk_vs_O1PRk), adjust = "holm")
write.csv(as.data.frame(res_O2NRk_vs_O1PRk),
          file.path(outdir, "SPEC_O2_NRk_vs_O1_PRk_matched_days.csv"),
          row.names = FALSE)


# === ALL pairwise comparisons across phase × day × Order (28 cells → 378 tests) ===
em_cells <- emmeans(fit, ~ phase:day:Order)  # each combo as one level

all_pairs_holm  <- summary(pairs(em_cells, adjust = "holm"))
all_pairs_tukey <- summary(pairs(em_cells, adjust = "tukey"))

# Prettify the contrast labels to look like "PR0 Order1 - NR3 Order2"
prettify_contrasts <- function(df) {
  df <- as.data.frame(df)
  lr <- strsplit(df$contrast, " - ", fixed = TRUE)

  pattern <- "phase\\s*=?\\s*(NR|PR).*day\\s*=?\\s*X?([0-9]+).*Order\\s*=?\\s*([12])"

  grab <- function(x) {
    utils::strcapture(
      pattern = pattern,
      x = x,
      proto = data.frame(phase = character(), day = character(), Order = character(),
                         stringsAsFactors = FALSE)
    )
  }

  L <- do.call(rbind, lapply(lr, function(z) grab(z[1])))
  R <- do.call(rbind, lapply(lr, function(z) grab(z[2])))

  df$contrast_pretty <- paste0(L$phase, L$day, " Order", L$Order,
                               " - ",
                               R$phase, R$day, " Order", R$Order)
  df
}

all_pairs_holm_df  <- prettify_contrasts(all_pairs_holm)
all_pairs_tukey_df <- prettify_contrasts(all_pairs_tukey)

write.csv(all_pairs_holm_df,
          file.path(outdir, "ALL_pairwise_phase_day_Order_HOLM.csv"),
          row.names = FALSE)
write.csv(all_pairs_tukey_df,
          file.path(outdir, "ALL_pairwise_phase_day_Order_TUKEY.csv"),
          row.names = FALSE)







###############################################################################################################################################################################################
############################################################################ BODYWEIGHT #################################################################################################
###############################################################################################################################################################################################



library(tidyr)
library(dplyr)
library(afex)
library(emmeans)
library(ggplot2)

# Load the dataset
data <- read.csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\Bodyweight\\BODYWEIGHT.csv")

# Step 1: Reshape the data
long_data <- data %>%
  pivot_longer(cols = starts_with(c("NR", "PR")),
               names_to = "time_phase",
               values_to = "value")

# Step 2: Generate Descriptive Statistics
descriptive_stats <- long_data %>%
  group_by(time_phase, Sex, Order) %>%
  summarise(mean_value = mean(value, na.rm = TRUE),
            sd_value = sd(value, na.rm = TRUE),
            n = n())

# Save descriptive statistics to CSV
write.csv(descriptive_stats, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\Bodyweight\\w_trend_descriptive_stats.csv")

# Step 3: Run ANOVA for each time point, sex, and order
anova_results <- aov_car(value ~ time_phase * Sex * Order + Error(Mouse/time_phase), data = long_data)

# Save ANOVA results to CSV
write.csv(as.data.frame(anova(anova_results)), "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\Bodyweight\\wtrend_anova_results.csv")

# Step 4: Post-hoc tests with Tukey adjustment
posthoc_tukey <- emmeans(anova_results, pairwise ~ time_phase * Sex * Order, adjust = "tukey")
posthoc_tukey_df <- as.data.frame(summary(posthoc_tukey$contrasts))
write.csv(posthoc_tukey_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\Bodyweight\\w_trend_posthoc_tukey_results.csv")

# Step 5: Post-hoc tests with Holm adjustment
posthoc_holm <- emmeans(anova_results, pairwise ~ time_phase * Sex * Order, adjust = "holm")
posthoc_holm_df <- as.data.frame(summary(posthoc_holm$contrasts))
write.csv(posthoc_holm_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\Bodyweight\\w_trend_posthoc_holm_results.csv")



###############################################################################################################################################################################
##################################################################### Bodyweight RE DO#########################################################################################

###############################################################################################################################################################################
##################################################################### ##################################################################################

# --- Packages & options ---
library(tidyverse)
library(afex)
library(emmeans)
library(broom)

afex::afex_options(type = 3)                  # Type-III SS
options(contrasts = c("contr.sum","contr.poly"))

# --- Paths   ---
infile <- "C:/Users/hta031/Github/FEDProtein/results/FIVE/LINE_PLOTS/Bodyweight/BODYWEIGHT.csv"
outdir <- "C:/Users/hta031/Github/FEDProtein/results/FIVE/LINE_PLOTS/Bodyweight/RE_DO_BODYWEIGHT"
infile <- trimws(infile); outdir <- trimws(outdir)
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

# --- Load & reshape (NR/PR + day parsed cleanly) ---
dat <- read.csv(infile)

long <- dat %>%
  pivot_longer(
    cols = matches("^(NR|PR)\\d+$"),
    names_to        = c("phase","day"),
    names_pattern   = "(NR|PR)(\\d+)",
    names_transform = list(day = as.integer),
    values_to       = "value"
  ) %>%
  mutate(
    day   = factor(day, levels = sort(unique(day))),
    phase = factor(phase, levels = c("NR","PR")),
    Order = factor(Order),
    Sex   = factor(Sex),
    Mouse = factor(Mouse)
  ) %>%
  drop_na(value)

# --- Descriptives per (Order × Sex × phase × day) ---
descriptive <- long %>%
  group_by(Order, Sex, phase, day) %>%
  summarise(n = sum(!is.na(value)),
            mean = mean(value, na.rm = TRUE),
            sd   = sd(value, na.rm = TRUE),
            .groups = "drop")
write.csv(descriptive, file.path(outdir, "Bodyweight_descriptives_by_Order_Sex_phase_day.csv"), row.names = FALSE)

# --- Repeated-measures ANOVA (Sex included) ---
fit <- aov_ez(
  id      = "Mouse",
  dv      = "value",
  within  = c("phase","day"),
  between = c("Order","Sex"),
  data    = long
)

anova_tab <- afex::nice(fit, es = "pes", correction = "GG")
write.csv(anova_tab, file.path(outdir, "Bodyweight_ANOVA_GG_with_Sex.csv"), row.names = FALSE)

# --- EMMs for all cells (phase × day × Order × Sex) ---
em <- emmeans(fit, ~ phase*day*Order*Sex)

# Save the grid (helps inspect labels; day may appear as X0..X6)
grid <- as.data.frame(em)[, c("phase","day","Order","Sex")]
write.csv(grid, file.path(outdir, "EMM_grid_Bodyweight_with_Sex.csv"), row.names = FALSE)

# --- Helper to build a contrast vector A - B (robust to 'X' in day labels) ---
# A, B are c(phase, day, Order, Sex) with day like "0","1","2", Sex like "M"/"F"
mkL4 <- function(em, A, B) {
  g <- as.data.frame(em)[, c("phase","day","Order","Sex")]
  g <- data.frame(lapply(g, function(x) trimws(as.character(x))), stringsAsFactors = FALSE)
  names(g) <- c("phase","day","Order","Sex")
  g$day_plain <- sub("^X", "", g$day)  # strip leading 'X' if present

  iA <- which(g$phase==A[1] & g$day_plain==A[2] & g$Order==A[3] & g$Sex==A[4])
  iB <- which(g$phase==B[1] & g$day_plain==B[2] & g$Order==B[3] & g$Sex==B[4])

  if (length(iA)!=1 || length(iB)!=1) {
    message("Could not find requested cells. First rows of the EMM grid:")
    print(utils::head(g, 12))
    stop(sprintf("Can't find cells:\n  A = %s\n  B = %s",
                 paste(A, collapse=","), paste(B, collapse=",")))
  }
  L <- rep(0, nrow(g)); L[iA] <- 1; L[iB] <- -1; L
}

# --- SPECIFIC CONTRASTS (Holm-adjusted), computed PER SEX -------------------------------------------------------
fmt <- function(phase, day, ord, sex) sprintf("%s%s Order%s Sex%s", phase, day, ord, sex)

sex_levels <- levels(long$Sex)  # typically c("M","F")

# Containers to optionally bind results across sexes (if you want single files later)
all_PR0_vs_allO2  <- list()
all_NR0_vs_NRO2   <- list()
all_PRk_vs_NRk    <- list()
all_NRk_vs_PRk    <- list()

for (sx in sex_levels) {

  # 1) PR0 (Order 2, Sex = sx) vs every other day in Order 2, Sex = sx
  cts_PR0O2_vs_allO2 <- list()
  for (d in 1:6) {
    name <- paste(fmt("PR","0","2", sx), "-", fmt("PR", as.character(d), "2", sx))
    cts_PR0O2_vs_allO2[[name]] <- mkL4(em, c("PR","0","2", sx), c("PR", as.character(d), "2", sx))
  }
  for (d in 0:6) {
    name <- paste(fmt("PR","0","2", sx), "-", fmt("NR", as.character(d), "2", sx))
    cts_PR0O2_vs_allO2[[name]] <- mkL4(em, c("PR","0","2", sx), c("NR", as.character(d), "2", sx))
  }
  res_PR0O2_vs_allO2 <- summary(contrast(em, cts_PR0O2_vs_allO2), adjust = "holm")
  res_PR0O2_vs_allO2 <- transform(as.data.frame(res_PR0O2_vs_allO2), Sex = sx)
  write.csv(res_PR0O2_vs_allO2,
            file.path(outdir, paste0("SPEC_O2_PR0_vs_all_Order2_days_Sex", sx, ".csv")),
            row.names = FALSE)
  all_PR0_vs_allO2[[sx]] <- res_PR0O2_vs_allO2

  # 2) NR0 (Order 2, Sex = sx) vs NR1..NR6 in Order 2, Sex = sx
  cts_NR0O2_vs_NR_O2 <- setNames(lapply(1:6, function(d)
    mkL4(em, c("NR","0","2", sx), c("NR", as.character(d), "2", sx))),
    paste0(fmt("NR","0","2", sx), " - ", fmt("NR","", "2", sx)) # label updated below
  )
  names(cts_NR0O2_vs_NR_O2) <- paste(fmt("NR","0","2", sx), "-", paste0("NR", 1:6, " Order2 Sex", sx))
  res_NR0O2_vs_NR_O2 <- summary(contrast(em, cts_NR0O2_vs_NR_O2), adjust = "holm")
  res_NR0O2_vs_NR_O2 <- transform(as.data.frame(res_NR0O2_vs_NR_O2), Sex = sx)
  write.csv(res_NR0O2_vs_NR_O2,
            file.path(outdir, paste0("SPEC_O2_NR0_vs_NR1to6_in_Order2_Sex", sx, ".csv")),
            row.names = FALSE)
  all_NR0_vs_NRO2[[sx]] <- res_NR0O2_vs_NR_O2

  # 3a) Matched-day: PRk (Order 2, Sex = sx) vs NRk (Order 1, Sex = sx)
  cts_O2PRk_vs_O1NRk <- setNames(lapply(0:6, function(d)
    mkL4(em, c("PR", as.character(d), "2", sx),
             c("NR", as.character(d), "1", sx))),
    paste0("PR", 0:6, " Order2 Sex", sx, " - NR", 0:6, " Order1 Sex", sx)
  )
  res_O2PRk_vs_O1NRk <- summary(contrast(em, cts_O2PRk_vs_O1NRk), adjust = "holm")
  res_O2PRk_vs_O1NRk <- transform(as.data.frame(res_O2PRk_vs_O1NRk), Sex = sx)
  write.csv(res_O2PRk_vs_O1NRk,
            file.path(outdir, paste0("SPEC_O2_PRk_vs_O1_NRk_matched_days_Sex", sx, ".csv")),
            row.names = FALSE)
  all_PRk_vs_NRk[[sx]] <- res_O2PRk_vs_O1NRk

  # 3b) Matched-day: NRk (Order 2, Sex = sx) vs PRk (Order 1, Sex = sx)
  cts_O2NRk_vs_O1PRk <- setNames(lapply(0:6, function(d)
    mkL4(em, c("NR", as.character(d), "2", sx),
             c("PR", as.character(d), "1", sx))),
    paste0("NR", 0:6, " Order2 Sex", sx, " - PR", 0:6, " Order1 Sex", sx)
  )
  res_O2NRk_vs_O1PRk <- summary(contrast(em, cts_O2NRk_vs_O1PRk), adjust = "holm")
  res_O2NRk_vs_O1PRk <- transform(as.data.frame(res_O2NRk_vs_O1PRk), Sex = sx)
  write.csv(res_O2NRk_vs_O1PRk,
            file.path(outdir, paste0("SPEC_O2_NRk_vs_O1_PRk_matched_days_Sex", sx, ".csv")),
            row.names = FALSE)
  all_NRk_vs_PRk[[sx]] <- res_O2NRk_vs_O1PRk
}

# If you also want combined tables across both sexes (each row has a Sex column), uncomment:
# write.csv(bind_rows(all_PR0_vs_allO2), file.path(outdir, "SPEC_O2_PR0_vs_all_Order2_days_ALL_SEX.csv"), row.names = FALSE)
# write.csv(bind_rows(all_NR0_vs_NRO2), file.path(outdir, "SPEC_O2_NR0_vs_NR1to6_in_Order2_ALL_SEX.csv"), row.names = FALSE)
# write.csv(bind_rows(all_PRk_vs_NRk),  file.path(outdir, "SPEC_O2_PRk_vs_O1_NRk_matched_days_ALL_SEX.csv"), row.names = FALSE)
# write.csv(bind_rows(all_NRk_vs_PRk),  file.path(outdir, "SPEC_O2_NRk_vs_O1_PRk_matched_days_ALL_SEX.csv"), row.names = FALSE)

# === ALL pairwise comparisons across phase × day × Order × Sex =================
em_cells <- emmeans(fit, ~ phase:day:Order:Sex)  # each combo as one level

all_pairs_holm  <- summary(pairs(em_cells, adjust = "holm"))
all_pairs_tukey <- summary(pairs(em_cells, adjust = "tukey"))

# Prettify the contrast labels to look like "PR0 Order1 SexM - NR3 Order2 SexF" (if ever cross-sex)
prettify_contrasts4 <- function(df) {
  df <- as.data.frame(df)
  lr <- strsplit(df$contrast, " - ", fixed = TRUE)

  pattern <- "phase\\s*=?\\s*(NR|PR).*day\\s*=?\\s*X?([0-9]+).*Order\\s*=?\\s*([12]).*Sex\\s*=?\\s*([MF])"

  grab <- function(x) {
    utils::strcapture(
      pattern = pattern,
      x = x,
      proto = data.frame(phase = character(), day = character(), Order = character(), Sex = character(),
                         stringsAsFactors = FALSE)
    )
  }

  L <- do.call(rbind, lapply(lr, function(z) grab(z[1])))
  R <- do.call(rbind, lapply(lr, function(z) grab(z[2])))

  df$contrast_pretty <- paste0(L$phase, L$day, " Order", L$Order, " Sex", L$Sex,
                               " - ",
                               R$phase, R$day, " Order", R$Order, " Sex", R$Sex)
  df
}

all_pairs_holm_df  <- prettify_contrasts4(all_pairs_holm)
all_pairs_tukey_df <- prettify_contrasts4(all_pairs_tukey)

write.csv(all_pairs_holm_df,
          file.path(outdir, "ALL_pairwise_phase_day_Order_Sex_HOLM.csv"),
          row.names = FALSE)
write.csv(all_pairs_tukey_df,
          file.path(outdir, "ALL_pairwise_phase_day_Order_Sex_TUKEY.csv"),
          row.names = FALSE)



###############################################################################################################################################################################################
############################################################################ HOARDING #################################################################################################
###############################################################################################################################################################################################



library(tidyr)
library(dplyr)
library(afex)
library(emmeans)
library(ggplot2)

# Load the dataset
data <- read.csv("C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\Hoarding\\HOARDING.csv")

# Step 1: Reshape the data
long_data <- data %>%
  pivot_longer(cols = starts_with(c("NR", "PR")),
               names_to = "time_phase",
               values_to = "value")

# Step 2: Generate Descriptive Statistics
descriptive_stats <- long_data %>%
  group_by(time_phase, Sex, Order) %>%
  summarise(mean_value = mean(value, na.rm = TRUE),
            sd_value = sd(value, na.rm = TRUE),
            n = n())

# Save descriptive statistics to CSV
write.csv(descriptive_stats, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\Hoarding\\h_trend_descriptive_stats.csv")

# Step 3: Run ANOVA for each time point, sex, and order
anova_results <- aov_car(value ~ time_phase * Sex * Order + Error(Mouse/time_phase), data = long_data)

# Save ANOVA results to CSV
write.csv(as.data.frame(anova(anova_results)), "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\Hoarding\\h_trend_anova_results.csv")

# Step 4: Post-hoc tests with Tukey adjustment
posthoc_tukey <- emmeans(anova_results, pairwise ~ time_phase * Sex * Order, adjust = "tukey")
posthoc_tukey_df <- as.data.frame(summary(posthoc_tukey$contrasts))
write.csv(posthoc_tukey_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\Hoarding\\h_trend_posthoc_tukey_results.csv")

# Step 5: Post-hoc tests with Holm adjustment
posthoc_holm <- emmeans(anova_results, pairwise ~ time_phase * Sex * Order, adjust = "holm")
posthoc_holm_df <- as.data.frame(summary(posthoc_holm$contrasts))
write.csv(posthoc_holm_df, "C:\\Users\\hta031\\Github\\FEDProtein\\results\\FIVE\\LINE_PLOTS\\Hoarding\\h_trend_posthoc_holm_results.csv")



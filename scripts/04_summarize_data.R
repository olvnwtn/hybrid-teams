# Authors: Olivia B. Newton and Tyler R. Talbot
# Purpose: Compute descriptive statistics and correlations, and visualize
#          variable distributions.

# Read data ---------------------------------------------------------------

team_data <- read_csv(here("data", "processed", "merged_team.csv"))
survey_processed <- read_csv(here("data", "processed", "survey_individual.csv"))

# Demographics ------------------------------------------------------------

summary(survey_processed$age)
summary(factor(survey_processed$sex)). # 1 == male, 2 == female

# Correlation table with means and SDs ------------------------------------

corr_vars <- team_data |>
  select(expert_influence,
         demon_team_median,
         performance_score,
         openness_team_mean, openness_team_sd,
         conscientiousness_team_mean, conscientiousness_team_sd,
         agreeableness_team_mean, agreeableness_team_sd,
         neuroticism_team_mean, neuroticism_team_sd,
         extraversion_team_mean, extraversion_team_sd,
         remain_team_mean, remain_team_sd,
         viability_team_mean, viability_team_sd)

corr_means_sds <- corr_vars |>
  summarise(across(everything(),
                   list(mean = ~ mean(.x, na.rm = TRUE),
                        sd = ~ sd(.x, na.rm = TRUE))))

correlations_sig <- Hmisc::rcorr(as.matrix(corr_vars),
                                 type = "spearman")


# Descriptives by condition -----------------------------------------------

condition_descriptives <- team_data |>
  group_by(condition) |>
  summarise(across(c(performance_score, expert_influence,
                     demon_team_median,
                     extraversion_team_mean, agreeableness_team_mean,
                     conscientiousness_team_mean, neuroticism_team_mean,
                     openness_team_mean, remain_team_mean,
                     viability_team_mean),
                   list(mean = mean, sd = sd, median = median),
                   .names = "{.col}_{.fn}"))

# Descriptives by virtuality ----------------------------------------------

virtuality_descriptives <- team_data |>
  group_by(virtuality_condition) |>
  summarise(across(c(performance_score, expert_influence,
                     demon_team_median,
                     extraversion_team_mean, agreeableness_team_mean,
                     conscientiousness_team_mean, neuroticism_team_mean,
                     openness_team_mean, remain_team_mean,
                     viability_team_mean),
                   list(mean = mean, sd = sd, median = median),
                   .names = "{.col}_{.fn}"))

# Descriptives by expert location -----------------------------------------

location_descriptives <- team_data |>
  group_by(expert_location) |>
  summarise(across(c(performance_score, expert_influence,
                     demon_team_median,
                     extraversion_team_mean, agreeableness_team_mean,
                     conscientiousness_team_mean, neuroticism_team_mean,
                     openness_team_mean, remain_team_mean,
                     viability_team_mean),
                   list(mean = mean, sd = sd, median = median),
                   .names = "{.col}_{.fn}"))

# Sample-level distributions ----------------------------------------------

hist(team_data$performance_score, main = "Performance Score", xlab = "Score")
hist(team_data$demon_team_median, main = "Demonstrability", xlab = "Median")
hist(team_data$expert_influence, main = "Expert Influence", xlab = "Proportion")
hist(team_data$exp_part_in_zoom, main = "Expert Speaking Time", xlab = "Seconds")
hist(team_data$exp_part_in_app, main = "Expert In-App Actions", xlab = "Count")

barplot(table(team_data$condition), main = "Teams by Condition")
barplot(table(team_data$virtuality_condition), main = "Teams by Virtuality")
barplot(table(team_data$expert_location), main = "Teams by Expert Location")

# Distributions by condition ----------------------------------------------

boxplot(performance_score ~ condition, data = team_data, main = "Performance by Condition")
boxplot(demon_team_median ~ condition, data = team_data, main = "Demonstrability by Condition")
boxplot(expert_influence ~ condition, data = team_data, main = "Expert Influence by Condition")
boxplot(exp_part_in_zoom ~ condition, data = team_data, main = "Speaking Time by Condition")
boxplot(exp_part_in_app ~ condition, data = team_data, main = "In-App Actions by Condition")

# Distributions by virtuality ---------------------------------------------

boxplot(performance_score ~ virtuality_condition, data = team_data, main = "Performance by Virtuality")
boxplot(demon_team_median ~ virtuality_condition, data = team_data, main = "Demonstrability by Virtuality")
boxplot(expert_influence ~ virtuality_condition, data = team_data, main = "Expert Influence by Virtuality")

# Distributions by expert location ----------------------------------------

boxplot(performance_score ~ expert_location, data = team_data, main = "Performance by Expert Location")
boxplot(demon_team_median ~ expert_location, data = team_data, main = "Demonstrability by Expert Location")
boxplot(expert_influence ~ expert_location, data = team_data, main = "Expert Influence by Expert Location")

# Descriptives and correlations table (manuscript table format) -----------

table_vars <- team_data |>
  select(demon_team_median,
         performance_score,
         exp_part_in_zoom,
         exp_part_in_app,
         expert_influence)

rc <- Hmisc::rcorr(as.matrix(table_vars), type = "spearman")

r_mat <- round(rc$r, 2)
p_mat <- rc$P

stars <- ifelse(p_mat < .001, "***", ifelse(p_mat < .01, "**", ifelse(p_mat < .05, "*", "")))
r_star <- matrix(paste0(format(r_mat, nsmall = 2), stars),
                 nrow = nrow(r_mat), dimnames = dimnames(r_mat))
r_star[upper.tri(r_star, diag = TRUE)] <- ""

k <- ncol(table_vars)
desc_table <- data.frame(
  Variable = paste0(seq_len(k), ". ", names(table_vars)),
  M  = round(colMeans(table_vars, na.rm = TRUE), 2),
  SD = round(apply(table_vars, 2, sd, na.rm = TRUE), 2),
  N  = diag(rc$n),
  r_star[, -k],
  check.names = FALSE
)
names(desc_table)[-(1:4)] <- seq_len(k - 1)

write.csv(desc_table,
          here("output", "tables", "descriptives_correlations.csv"),
          row.names = FALSE)

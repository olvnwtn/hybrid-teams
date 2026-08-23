# Authors: Olivia B. Newton and Tyler R. Talbot
# Purpose: Compute descriptive statistics and correlations, and visualize
#          variable distributions.

# Read data ---------------------------------------------------------------

merged_team <- read_csv(here("data", "processed", "merged_team.csv"))
survey_processed <- read_csv(here("data", "processed", "survey_individual.csv"))

# Demographics ------------------------------------------------------------

summary(survey_processed$age)
summary(factor(survey_processed$sex))

# Correlation table with means and SDs ------------------------------------

corr_vars <- merged_team |>
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

condition_descriptives <- merged_team |>
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

virtuality_descriptives <- merged_team |>
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

location_descriptives <- merged_team |>
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

hist(merged_team$performance_score, main = "Performance Score", xlab = "Score")
hist(merged_team$demon_team_median, main = "Demonstrability", xlab = "Median")
hist(merged_team$expert_influence, main = "Expert Influence", xlab = "Proportion")
hist(merged_team$exp_part_in_zoom, main = "Expert Speaking Time", xlab = "Seconds")
hist(merged_team$exp_part_in_app, main = "Expert In-App Actions", xlab = "Count")

barplot(table(merged_team$condition), main = "Teams by Condition")
barplot(table(merged_team$virtuality_condition), main = "Teams by Virtuality")
barplot(table(merged_team$expert_location), main = "Teams by Expert Location")

# Distributions by condition ----------------------------------------------

boxplot(performance_score ~ condition, data = merged_team, main = "Performance by Condition")
boxplot(demon_team_median ~ condition, data = merged_team, main = "Demonstrability by Condition")
boxplot(expert_influence ~ condition, data = merged_team, main = "Expert Influence by Condition")
boxplot(exp_part_in_zoom ~ condition, data = merged_team, main = "Speaking Time by Condition")
boxplot(exp_part_in_app ~ condition, data = merged_team, main = "In-App Actions by Condition")

# Distributions by virtuality ---------------------------------------------

boxplot(performance_score ~ virtuality_condition, data = merged_team, main = "Performance by Virtuality")
boxplot(demon_team_median ~ virtuality_condition, data = merged_team, main = "Demonstrability by Virtuality")
boxplot(expert_influence ~ virtuality_condition, data = merged_team, main = "Expert Influence by Virtuality")

# Distributions by expert location ----------------------------------------

boxplot(performance_score ~ expert_location, data = merged_team, main = "Performance by Expert Location")
boxplot(demon_team_median ~ expert_location, data = merged_team, main = "Demonstrability by Expert Location")
boxplot(expert_influence ~ expert_location, data = merged_team, main = "Expert Influence by Expert Location")

# Authors: Olivia B. Newton and Tyler R. Talbot
# Purpose: Compute descriptive statistics and correlations.

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

correlations_sig <- Hmisc::rcorr(as.matrix(corr_vars))

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

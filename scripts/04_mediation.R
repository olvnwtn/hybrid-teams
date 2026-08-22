# Authors: Olivia B. Newton and Tyler R. Talbot
# Purpose: Test whether demonstrability mediates the effect of team
#          configuration on performance (H3).

# Read data ---------------------------------------------------------------

team_data <- read_csv(here("data", "processed", "merged_team.csv"))

# Set factor levels -------------------------------------------------------

team_data <- team_data |>
  mutate(condition = factor(condition,
                            levels = c("F2F", "Remote",
                                       "Hybrid RE", "Hybrid CE")))

# Total effect: condition -> performance ----------------------------------

mod_total <- lm(performance_score ~ condition, data = team_data)
summary(mod_total)

# a path: condition -> demonstrability ------------------------------------

mod_mediator <- lm(demon_team_median ~ condition, data = team_data)
summary(mod_mediator)

# b path + direct effect: condition + demonstrability -> performance ------

mod_outcome <- lm(performance_score ~ condition + demon_team_median,
                  data = team_data)
summary(mod_outcome)

# Bootstrap indirect effects ----------------------------------------------

set.seed(1692576000)

# H3A: F2F vs Remote, mediated by demonstrability

med_f2f_remote <- mediation::mediate(mod_mediator, mod_outcome,
                                     treat = "condition",
                                     mediator = "demon_team_median",
                                     boot = TRUE, sims = 10000,
                                     boot.ci.type = "bca",
                                     control.value = "F2F",
                                     treat.value = "Remote")

# H3B: Remote vs Hybrid, mediated by demonstrability

med_remote_hybrid_re <- mediation::mediate(mod_mediator, mod_outcome,
                                           treat = "condition",
                                           mediator = "demon_team_median",
                                           boot = TRUE, sims = 10000,
                                           boot.ci.type = "bca",
                                           control.value = "Remote",
                                           treat.value = "Hybrid RE")
summary(med_remote_hybrid_re)

med_remote_hybrid_ce <- mediation::mediate(mod_mediator, mod_outcome,
                                           treat = "condition",
                                           mediator = "demon_team_median",
                                           boot = TRUE, sims = 10000,
                                           boot.ci.type = "bca",
                                           control.value = "Remote",
                                           treat.value = "Hybrid CE")
summary(med_remote_hybrid_ce)

# Unmeasured confounding test ---------------------------------------------

sens_f2f_remote <- mediation::medsens(med_f2f_remote, rho.by = 0.05)
summary(sens_f2f_remote)
plot(sens_f2f_remote)

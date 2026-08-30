# Authors: Olivia B. Newton and Tyler R. Talbot
# Purpose: Test all preregistered hypotheses (H1-H7).

# Read data ---------------------------------------------------------------

team_data <- read_csv(here("data", "processed", "merged_team.csv"))

# Set factor levels -------------------------------------------------------

team_data <- team_data |>
  mutate(condition = factor(condition,
                            levels = c("F2F", "Remote",
                                       "Hybrid RE", "Hybrid CE")),
         virtuality_condition = factor(virtuality_condition,
                                       levels = c("Matched", "Mixed")),
         expert_location = factor(expert_location,
                                  levels = c("Co-located", "Remote")))

# H1: Demonstrability ~ virtuality x expert location ---------------------

# Two way ANOVA
mod_demon <- aov(demon_team_median ~ virtuality_condition * expert_location,
                 data = team_data)

# Model assumptions

car::leveneTest(mod_demon)                # homogeneity of variance
shapiro.test(resid(mod_demon))            # normality of residuals
plot(density(resid(mod_demon)))
qqnorm(resid(mod_demon))
qqline(resid(mod_demon))
outlierTest(mod_demon)                    # outliers

# Model assumptions violated, utilizing Aligned Rank Transform (ART) procedure

art_mod <- art(demon_team_median ~ virtuality_condition * expert_location,
               data = team_data)
art_anova <- anova(art_mod)
print(art_anova, verbose = TRUE)

# Effect sizes

art_anova$eta_sq_part <- with(art_anova, `Sum Sq` / (`Sum Sq` + `Sum Sq.res`))
art_anova

# Confidence intervals for effect sizes

effectsize::F_to_eta2(
  f = art_anova$`F value`,
  df = art_anova$Df,
  df_error = art_anova$Df.res,
  ci = 0.90,
  alternative = "two.sided"
)

# Planned contrasts for H1A and H1B with Holm correction
# Cell order: Matched/Co-located, Matched/Remote,
#             Mixed/Co-located, Mixed/Remote

art_lm <- artlm.con(art_mod, "virtuality_condition:expert_location")

h1_contrasts <- emmeans::contrast(
  emmeans::emmeans(art_lm, ~ virtuality_conditionexpert_location),
  method = list(
    "H1A: F2F vs Remote" = c(1, -1, 0, 0),
    "H1B: Remote vs Hybrid" = c(0, 1, -0.5, -0.5)
  ),
  adjust = "holm"
)
summary(h1_contrasts)
confint(h1_contrasts)

# H2: Performance ~ virtuality x expert location -------------------------

# Two way ANOVA

mod_perf <- lm(performance_score ~ virtuality_condition * expert_location,
               data = team_data,
               contrasts = list(virtuality_condition = contr.sum,
                                expert_location = contr.sum))

# Model assumptions

car::leveneTest(mod_perf)
shapiro.test(resid(mod_perf))
plot(density(resid(mod_perf)))
qqnorm(resid(mod_perf))
qqline(resid(mod_perf))
outlierTest(mod_perf)

# Model results

car::Anova(mod_perf, type = 3)

# Effect sizes and confidence intervals

effectsize::eta_squared(car::Anova(mod_perf, type = 3), partial = TRUE,
                        ci = 0.90, alternative = "two.sided")

# Post hoc pairwise comparisons with Tukey adjustment

emmeans::emmeans(mod_perf,
                 pairwise ~ virtuality_condition * expert_location,
                 adjust = "tukey")

confint(emmeans::emmeans(mod_perf,
                         pairwise ~ virtuality_condition * expert_location,
                         adjust = "tukey")$contrasts)

# Planned contrasts for H2A, H2B, H2C with Holm correction
# Cell order: Matched/Co-located, Mixed/Co-located,
#             Matched/Remote, Mixed/Remote

h2_contrasts <- emmeans::contrast(
  emmeans::emmeans(mod_perf,
                   ~ virtuality_condition * expert_location),
  method = list(
    "H2A: F2F vs Remote" = c(1, 0, -1, 0),
    "H2B: Remote vs Hybrid" = c(0, -0.5, 1, -0.5),
    "H2C: Hybrid CE vs Hybrid RE" = c(0, 1, 0, -1)
  ),
  adjust = "holm"
)
summary(h2_contrasts)
confint(h2_contrasts)

# H3: Mediation analyses --------------------------------------------------

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

summary(med_f2f_remote)

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

# H4: Expert participation and demonstrability ----------------------------

cor.test(team_data$exp_part_in_zoom, team_data$demon_team_median,
         method = "spearman")

cor.test(team_data$exp_part_in_app, team_data$demon_team_median,
         method = "spearman")

# H5: Expert participation and expert influence --------------------------

cor.test(team_data$exp_part_in_zoom, team_data$expert_influence,
         method = "spearman")

cor.test(team_data$exp_part_in_app, team_data$expert_influence,
         method = "spearman")

# H6: Demonstrability and expert influence -------------------------------

cor.test(team_data$demon_team_median, team_data$expert_influence,
         method = "spearman")

# H7: Expert influence and performance ----------------------------------

cor.test(team_data$expert_influence, team_data$performance_score,
         method = "spearman")

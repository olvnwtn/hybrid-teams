# Authors: Olivia B. Newton and Tyler R. Talbot
# Purpose: Effects of virtuality and expert location on demonstrability
#          and performance (H1, H2).

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

art_mod <- art(performance_score ~ virtuality_condition * expert_location,
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

# Post hoc pairwise comparisons with Tukey adjustment

art.con(art_mod, "virtuality_condition:expert_location")

# H2: Performance ~ virtuality x expert location -------------------------

# Two way ANOVA

mod_perf <- lm(performance_score ~ virtuality_condition * expert_location,
               data = merged_team,
               contrasts = list(virtuality_condition = contr.sum,
                                expert_location = contr.sum))
car::Anova(mod_perf, type = 3)

# Model assumptions

car::leveneTest(mod_perf)
shapiro.test(resid(mod_perf))
plot(density(resid(mod_perf)))
outlierTest(mod_perf)
qqnorm(resid(mod_perf))
qqline(resid(mod_perf))
outlierTest(mod_perf)

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

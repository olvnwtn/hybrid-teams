# Authors: Olivia B. Newton and Tyler R. Talbot
# Purpose: Compute within-group agreement (rwg) and intraclass correlations
#          (ICC) to justify aggregating demonstrability to team level.

# Read individual-level data ----------------------------------------------

survey_processed <- read_csv(
  here("data", "processed", "survey_individual.csv")
) |>
  as.data.frame()

survey_processed <- survey_processed |>
  mutate(team_id = factor(team_id))

# rwg(j): overall demonstrability ----------------------------------------

rwg_overall <- multilevel::rwg.j(
  survey_processed[, c("demonstrability_1", "demonstrability_2",
                       "demonstrability_3", "demonstrability_4",
                       "demonstrability_5", "demonstrability_6",
                       "demonstrability_7", "demonstrability_8",
                       "demonstrability_9", "demonstrability_10",
                       "demonstrability_11", "demonstrability_12")],
  survey_processed$team_id
)
median(rwg_overall$rwg.j)

# rwg(j): shared conceptual system ----------------------------------------

rwg_scs <- multilevel::rwg.j(
  survey_processed[, c("demonstrability_1", "demonstrability_2",
                       "demonstrability_3", "demonstrability_6")],
  survey_processed$team_id
)
median(rwg_scs$rwg.j)

# rwg(j): willing and able to listen to others ----------------------------

rwg_listen <- multilevel::rwg.j(
  survey_processed[, c("demonstrability_11", "demonstrability_10",
                       "demonstrability_7", "demonstrability_9")],
  survey_processed$team_id
)
median(rwg_listen$rwg.j)

# rwg(j): sufficiency of information --------------------------------------

rwg_sufficient <- multilevel::rwg.j(
  survey_processed[, c("demonstrability_5", "demonstrability_4")],
  survey_processed$team_id
)
median(rwg_sufficient$rwg.j)

# rwg(j): willing and able to share information ---------------------------

rwg_willing <- multilevel::rwg.j(
  survey_processed[, c("demonstrability_12", "demonstrability_8")],
  survey_processed$team_id
)
median(rwg_willing$rwg.j)

# ICC(1) and ICC(2): item level -------------------------------------------

item_correlations <- multilevel::mult.icc(
  survey_processed[, c("demonstrability_1", "demonstrability_2",
                       "demonstrability_3", "demonstrability_4",
                       "demonstrability_5", "demonstrability_6",
                       "demonstrability_7", "demonstrability_8",
                       "demonstrability_9", "demonstrability_10",
                       "demonstrability_11", "demonstrability_12")],
  survey_processed$team_id
)

# ICC(1) and ICC(2): scale level ------------------------------------------

scale_correlations <- multilevel::mult.icc(
  survey_processed[, c("demon_individual_mean", "shared_concept",
                       "listen_others", "info_sufficient",
                       "share_others")],
  survey_processed$team_id
)

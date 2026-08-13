# Authors: Olivia B. Newton and Tyler R. Talbot
# Purpose: Clean the raw Empirica frames and derive the expert's in-app
#          action count. Produces one Empirica dataset for the merge step.

# Clean stages ------------------------------------------------------------

stages_clean <- stages_raw |>
  filter(startTimeAt > "2023-08-22") |>   # drop pre-experiment test runs
  select(stageId = X_id,
         performance_score = data.score,
         violated_constraints = data.violatedConstraints,
         participation = data.log)

# Clean players -----------------------------------------------------------

players_clean <- players_raw |>
  select(playerId = X_id, id) |>
  mutate(team_id = str_extract(id, "[^_]*_[^_]*"))

# Link stages to teams ----------------------------------------------------

stage_team <- player_stages_raw |>
  inner_join(players_clean, by = "playerId") |>
  distinct(stageId, team_id) |>
  mutate(
    condition = case_when(
      str_detect(team_id, "_1") ~ "F2F",
      str_detect(team_id, "_2") ~ "Remote",
      str_detect(team_id, "_3") ~ "Hybrid CE",
      str_detect(team_id, "_4") ~ "Hybrid RE",
      .default = ""
    ),
    virtuality_condition = if_else(
      condition %in% c("F2F", "Remote"), "Matched", "Mixed"
    )
  ) |>
  filter(condition != "")   # empty condition means test data

empirica_clean <- stages_clean |>
  right_join(stage_team, by = "stageId")

# Derive expert in-app action count ---------------------------------------
# The participation log is a packed text field untangled here with fragile
# string operations ported from the original. Validate exp_part_inApp per
# team against the original output before relying on it.

expert_actions <- empirica_clean |>
  select(team_id, participation) |>
  mutate(across(everything(), ~ map_chr(.x, ~ gsub("\"", "", .x)))) |>
  separate_rows(participation, sep = "}") |>
  filter(participation != "",
         !str_detect(participation, "roundStarted")) |>
  mutate(participation = gsub("\\{|\\]", "", str_trim(participation)),
         participation = sub(".", "", participation))

expert_actions[c("action", "playerId", "student", "time")] <-
  str_split_fixed(expert_actions$participation, ",", 4)

expert_actions <- expert_actions |>
  select(-participation, -time) |>
  mutate(across(everything(), ~ map_chr(.x, ~ gsub(".*:", "", .x)))) |>
  inner_join(players_clean, by = c("team_id", "playerId")) |>
  filter(action != "playerSatisfaction",
         str_detect(id, "A")) |>   # "A" marks the team's expert
  count(id, name = "exp_part_inApp") |>
  mutate(team_id = str_extract(id, "[^_]*_[^_]*")) |>
  select(team_id, exp_part_inApp)

empirica_clean <- empirica_clean |>
  left_join(expert_actions, by = "team_id") |>
  select(-participation)   # raw log no longer needed
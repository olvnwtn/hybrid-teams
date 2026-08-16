# Authors: Olivia B. Newton and Tyler R. Talbot
# Purpose: Clean the raw Empirica data and derive the expert's in-app
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

id_list <- merge(players_clean, player_stages_raw, by = "playerId") |>
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
  subset(condition != "")

empirica_clean <- merge(stages_clean, id_list, by = "stageId", all.y = TRUE)

# Derive expert in-app action count ---------------------------------------
# The participation log column holds multiple events nested in each cell.
# The code below separates those events into rows and splits each into
# columns.

expert_actions <- empirica_clean |>
  select(team_id, participation) |>
  mutate(across(everything(), ~ map_chr(.x, ~ gsub("\"", "", .x)))) |>
  separate_rows(participation, sep = "}") |>
  subset(participation != "")

expert_actions <- expert_actions[!grepl("roundStarted", expert_actions$participation), ]

expert_actions["participation"] <- gsub("\\{|\\]", "", str_trim(expert_actions[["participation"]], "both"))
expert_actions["participation"] <- sub(".", "", expert_actions[["participation"]])

expert_actions[c("action", "playerId", "student", "time")] <-
  str_split_fixed(expert_actions$participation, ",", 4)

expert_actions <- expert_actions |>
  select(-participation, -time) |>
  mutate(across(everything(), ~ map_chr(.x, ~ gsub(".*:", "", .x)))) |>
  select(team_id, playerId, action, student)

expert_actions <- merge(expert_actions, players_clean, by = c("team_id", "playerId")) |>
  subset(action != "playerSatisfaction") |>
  filter(str_detect(id, "A"))

expert_actions <- expert_actions |>
  count(id, name = "exp_part_inApp") |>
  mutate(team_id = str_extract(id, "[^_]*_[^_]*")) |>
  select(team_id, exp_part_inApp)

empirica_clean <- empirica_clean |>
  left_join(expert_actions, by = "team_id") |>
  select(-participation)   # raw log no longer needed

# Save processed data -----------------------------------------------------

write_csv(empirica_clean, here("data", "processed", "empirica_clean.csv"))

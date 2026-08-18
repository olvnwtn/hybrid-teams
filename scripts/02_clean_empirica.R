# Authors: Olivia B. Newton and Tyler R. Talbot
# Purpose: Clean the raw Empirica data and sum the expert's in-app actions.
#          Produces one data frame containing processed Empirica data.

# Clean stages ------------------------------------------------------------

stages_clean <- stages_raw |>
  filter(start_time_at > "2023-08-22") |>   # drop pre-experiment test runs
  select(stage_id = x_id,
         performance_score = data_score,
         violated_constraints = data_violated_constraints,
         participation = data_log)

# Clean players -----------------------------------------------------------

players_clean <- players_raw |>
  select(player_id = x_id, id) |>
  mutate(team_id = str_extract(id, "[^_]*_[^_]*"))

# Link stages to teams ----------------------------------------------------

id_list <- players_clean |>
  inner_join(player_stages_raw, by = "player_id") |>
  distinct(stage_id, team_id) |>
  mutate(
    condition = factor(case_when(
      str_detect(team_id, "_1") ~ "F2F",
      str_detect(team_id, "_2") ~ "Remote",
      str_detect(team_id, "_3") ~ "Hybrid CE",  # Co-located Expert
      str_detect(team_id, "_4") ~ "Hybrid RE"   # Remote Expert
    ), levels = c("F2F", "Remote", "Hybrid RE", "Hybrid CE")),
    virtuality_condition = factor(if_else(
      condition %in% c("F2F", "Remote"), "Matched", "Mixed"
    ), levels = c("Matched", "Mixed"))
  ) |>
  filter(!is.na(condition))

empirica_clean <- stages_clean |>
  right_join(id_list, by = "stage_id")

# Derive expert in-app action count ---------------------------------------
# The participation log column holds multiple events nested in each cell.
# The code below separates those events into rows and splits each into
# columns.

expert_actions <- empirica_clean |>
  select(team_id, participation) |>
  mutate(across(everything(), ~ map_chr(.x, ~ gsub("\"", "", .x)))) |>
  separate_rows(participation, sep = "}") |>
  filter(participation != "",
         !str_detect(participation, "roundStarted")) |>
  mutate(participation = gsub("\\{|\\]", "", str_trim(participation)),
         participation = sub(".", "", participation))

expert_actions[c("action", "player_id", "student", "time")] <-
  str_split_fixed(expert_actions$participation, ",", 4)

expert_actions <- expert_actions |>
  select(-participation, -time) |>
  mutate(across(everything(), ~ map_chr(.x, ~ gsub(".*:", "", .x)))) |>
  select(team_id, player_id, action, student)

empirica_clean <- empirica_clean |>
  left_join(expert_actions, by = "team_id") |>
  select(-participation)

# Save processed data -----------------------------------------------------

write_csv(empirica_clean, here("data", "processed", "empirica_team.csv"))

# Authors: Olivia B. Newton and Tyler R. Talbot
# Purpose: Clean the raw Empirica data and sum the expert's in-app actions.
#          Produces one data frame containing processed Empirica data.

# Excluded teams ----------------------------------------------------------
# 2_1, 4_2, 7_3 sessions unusable due to tech or human error
# 4_1 and 24_2 participants did not understand the task per survey comments

excluded_team_ids <- c("2_1", "4_2", "7_3", "4_1", "24_2")

# Clean stages ------------------------------------------------------------
# Pre-experiment test runs conducted before August 23, 2023

stages_clean <- stages_raw |>
  filter(!is.na(start_time_at),
         start_time_at != "",
         start_time_at > "2023-08-22") |>
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
    ), levels = c("Matched", "Mixed")),
    expert_location = factor(if_else(
      condition %in% c("F2F", "Hybrid CE"), "Co-located", "Remote"
    ), levels = c("Co-located", "Remote")),
  ) |>
  filter(!is.na(condition))

empirica_clean <- stages_clean |>
  right_join(id_list, by = "stage_id") |>
  filter(!(team_id %in% excluded_team_ids))

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

expert_actions <- expert_actions |>
  inner_join(players_clean, by = c("team_id", "player_id")) |>
  filter(action != "playerSatisfaction",
         str_detect(id, "A")) |>
  count(id, name = "exp_part_in_app") |>
  mutate(team_id = str_extract(id, "[^_]*_[^_]*")) |>
  select(team_id, exp_part_in_app)

empirica_clean <- empirica_clean |>
  left_join(expert_actions, by = "team_id") |>
  select(-participation)


# Save processed data -----------------------------------------------------

write_csv(empirica_clean, here("data", "processed", "empirica_team.csv"))

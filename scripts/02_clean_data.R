# Authors: Olivia B. Newton and Tyler R. Talbot
# Purpose: Clean all raw data sources and save processed outputs.

# Exclusions ---------------------------------------------------------------
# Player IDs excluded: pilot and failed sessions (tech or human error)
excluded_player_ids <- c("test", "TEST", "Test", "", "BAD", "bad", "BAD DATA",
                         "2_2_c_BAD", "2_2_B_BAD", "2_2_A_BAD", "28_1_BAD",
                         "25_3_A_BAD", "25_3_B_BAD", "25_3_C_BAD")

# Response IDs excluded: first run of session 15 failed due to tech error
excluded_response_ids <- c("R_3fftMsJAxh9YZqH",
                           "R_1roec7ndCgHrw38",
                           "R_1GQ8bxl4UEHex8k")

# Team IDs excluded: 2_1, 4_2, 7_3 sessions unusable due to tech or human error
# Members of 4_1 and 24_2 did not understand the task per survey comments
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


# Clean survey ------------------------------------------------------------

survey_clean <- survey_raw |>
  slice(-c(1, 2)) |>
  mutate(team_id = str_extract(player_id, "[^_]*_[^_]*")) |>
  filter(start_date > "2023-08-22",
         !(response_id %in% excluded_response_ids),
         !(player_id %in% excluded_player_ids),
         !(team_id %in% excluded_team_ids)) |>
  select(team_id, player_id:color) |>
  mutate(
    condition = factor(case_when(
      str_detect(player_id, "_1") ~ "F2F",
      str_detect(player_id, "_2") ~ "Remote",
      str_detect(player_id, "_3") ~ "Hybrid CE",  # Co-located Expert
      str_detect(player_id, "_4") ~ "Hybrid RE",  # Remote Expert
    ), levels = c("F2F", "Remote", "Hybrid RE", "Hybrid CE")),
    player_exp = factor(if_else(str_detect(player_id, "_A"),
                                "Expert", "Non-Expert"),
                        levels = c("Expert", "Non-Expert")),
    virtuality_condition = factor(if_else(condition %in% c("F2F", "Remote"),
                                          "Matched", "Mixed"),
                                  levels = c("Matched", "Mixed")),
    expert_location = factor(if_else(
      condition %in% c("F2F", "Hybrid CE"), "Co-located", "Remote"
    ), levels = c("Co-located", "Remote")),
    age = as.numeric(age),
    sex = factor(sex),
    across(c(starts_with("demonstrability"),
             starts_with("big5"),
             starts_with("intent_remain"),
             starts_with("satis")),
           as.numeric)
  )

# Reverse-code items ------------------------------------------------------
# big5 and intent_remain items are on a 1-5 scale, so 6 - x reverses them.
# satis3 is on a 1-7 scale, so 8 - x reverses it.

reverse_code_cols <- c("big5_1", "big5_3", "big5_4", "big5_5", "big5_7",
                       "intent_remain_1", "intent_remain_3")

survey_clean <- survey_clean |>
  mutate(across(all_of(reverse_code_cols), ~ 6 - .x),
         satis3 = 8 - satis3)


# Compute individual-level measures ---------------------------------------

survey_clean <- survey_clean |>
  mutate(
    demon_individual_mean = rowMeans(pick(starts_with("demonstrability"))),
    shared_concept = (demonstrability_1 + demonstrability_2 +
                        demonstrability_3 + demonstrability_6) / 4,
    listen_others = (demonstrability_9 + demonstrability_11 +
                       demonstrability_7 + demonstrability_10) / 4,
    info_sufficient = (demonstrability_5 + demonstrability_4) / 2,
    share_others = (demonstrability_12 + demonstrability_8) / 2,
    extraversion = rowMeans(pick(c("big5_1", "big5_6"))),
    agreeableness = rowMeans(pick(c("big5_2", "big5_7"))),
    conscientiousness = rowMeans(pick(c("big5_3", "big5_8"))),
    neuroticism = rowMeans(pick(c("big5_4", "big5_9"))),
    openness = rowMeans(pick(c("big5_5", "big5_10"))),
    remain = rowMeans(pick(c("intent_remain_1", "intent_remain_2",
                             "intent_remain_3"))),
    viability = rowMeans(pick(c("satis1", "satis2", "satis3", "satis4")))
  )

# Aggregate to team level -------------------------------------------------

team_data <- survey_clean |>
  group_by(team_id) |>
  summarise(across(c(demon_individual_mean, extraversion, agreeableness,
                     conscientiousness, neuroticism, openness,
                     remain, viability),
                   list(team_median = median, team_mean = mean, team_sd = sd),
                   .names = "{.col}_{.fn}"),
            .groups = "drop") |>
  rename(demon_team_median = demon_individual_mean_team_median,
         demon_team_mean   = demon_individual_mean_team_mean,
         demon_team_sd     = demon_individual_mean_team_sd)

team_conditions <- survey_clean |>
  distinct(team_id, condition)

team_data <- team_data |>
  left_join(team_conditions, by = "team_id")

# Clean Zoom data ---------------------------------------------------------

zoom_clean <- zoom_raw |>
  separate(session_id, into = c("condition_no", "team_no"), sep = "_") |>
  mutate(team_id = paste(team_no, condition_no, sep = "_")) |>
  select(team_id, exp_part_in_zoom = participation_secs)


# Clean team condition assignments ----------------------------------------

design_clean <- team_assign_raw |>
  pivot_longer(
    cols = -condition,
    names_to = "team_order",
    values_to = "team_constraints"
  ) |>
  mutate(
    team_order = str_remove(team_order, "v"),
    condition = factor(case_when(
      condition == "conditionOne_F2F"            ~ "F2F",
      condition == "conditionTwo_Virtual"        ~ "Remote",
      condition == "conditionThree_Hybrid_CE"    ~ "Hybrid CE",
      condition == "conditionFour_Hybrid_RE"     ~ "Hybrid RE"
    ), levels = c("F2F", "Remote", "Hybrid RE", "Hybrid CE"))
  )

# Correct constraint assignments that differed from the design (human error)

design_clean <- design_clean |>
  mutate(team_constraints = case_when(
    team_order == "5" & condition == "Remote"  ~ 5L,
    team_order == "16" & condition == "Remote" ~ 4L,
    .default = team_constraints
  ))

# Clean constraint combos -------------------------------------------------

constraint_combos_clean <- constraint_combos_raw |>
  select(team_constraints = team,
         expert_constraint1a_id, expert_constraint1b_id,
         expert_constraint2a_id, expert_constraint2b_id,
         expert_constraint3a_id, expert_constraint3b_id) |>
  rowwise() |>
  mutate(experts_unique_constraints = paste(
    expert_constraint1a_id, expert_constraint1b_id,
    expert_constraint2a_id, expert_constraint2b_id,
    expert_constraint3a_id, expert_constraint3b_id,
    sep = ","
  )) |>
  ungroup() |>
  select(team_constraints, experts_unique_constraints)

# Join design to constraints ----------------------------------------------

design_clean <- design_clean |>
  inner_join(constraint_combos_clean, by = "team_constraints")

# Save processed data -----------------------------------------------------

write_csv(empirica_clean, here("data", "processed", "empirica_team.csv"))
write_csv(survey_clean, here("data", "processed", "survey_individual.csv"))
write_csv(team_data, here("data", "processed", "survey_team.csv"))
write_csv(zoom_clean, here("data", "processed", "zoom_team.csv"))
write_csv(design_clean, here("data", "processed", "study_design.csv"))

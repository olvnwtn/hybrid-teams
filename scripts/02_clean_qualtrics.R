# Authors: Olivia B. Newton and Tyler R. Talbot
# Purpose: Clean and score measures, then aggregate to team level for analysis.
#          Produces two data frames: one individual-level and one team-level.


# Specify excluded IDs ----------------------------------------------------

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

# Save processed data -----------------------------------------------------

write_csv(survey_clean, here("data", "processed", "survey_individual.csv"))
write_csv(team_data, here("data", "processed", "survey_team.csv"))

# Authors: Olivia B. Newton and Tyler R. Talbot
# Purpose: Read raw data from data/raw/ and standardize column names.

# Empirica ----------------------------------------------------------------
# The platform generates multiple CSV files per session (one session per team).
# Three are used in this pipeline:
# stages.csv: team performance scores, violated constraints, participation log
# players.csv: player IDs and participant IDs
# player-stages.csv: links players to stages

# Each experimental session has its own subfolder holding the same CSV files 
# Read every matching file across sessions and stack them into one data frame

empirica_dir <- here("data", "raw", "empirica")

stages_raw <- list.files(empirica_dir, pattern = "^stages\\.csv$",
                         recursive = TRUE, full.names = TRUE) |>
  map(\(f) read.csv(f, colClasses = "character")) |>
  list_rbind() |>
  clean_names()

players_raw <- list.files(empirica_dir, pattern = "^players\\.csv$",
                          recursive = TRUE, full.names = TRUE) |>
  map(\(f) read.csv(f, colClasses = "character")) |>
  list_rbind() |>
  clean_names()

player_stages_raw <- list.files(empirica_dir, pattern = "^player-stages\\.csv$",
                                recursive = TRUE, full.names = TRUE) |>
  map(\(f) read.csv(f, colClasses = "character")) |>
  list_rbind() |>
  clean_names()

# Qualtrics ---------------------------------------------------------------
# Numeric survey responses: task demonstrability, big five personality traits,
# team satisfaction, intent to remain, perceived influence, and demographics

survey_raw <- read.csv(here("data", "raw", "qualtrics",
                            "survey_numeric.csv")) |>
  clean_names()

# Zoom --------------------------------------------------------------------
# Expert participation: utterance count per participant, derived from
# session audio and video recordings

zoom_raw <- read.csv(here("data", "raw", "zoom",
                          "expert_participation.csv")) |>
  clean_names()

# Experiment design -------------------------------------------------------
# team_condition_assignments.csv: maps team order to experimental condition
# room_assignment_constraint_combos.xlsx: expert-specific constraint sets

team_assign_raw <- read.csv(here("data", "raw", "design",
                                 "team_condition_assignments.csv")) |>
  clean_names()

constraint_combos_raw <- read_xlsx(
  here("data", "raw", "design", "room_assignment_constraint_combos.xlsx"),
  sheet = "Experts_Contraints_All") |>
  clean_names()

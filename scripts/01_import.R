# Authors: Olivia B. Newton and Tyler R. Talbot
# Purpose: Read raw data from data/raw/ and standardize column names.
#          Cleaning and transformation happen in the 02_clean_*.R scripts.

# Empirica ----------------------------------------------------------------

# Each Empirica session lives in its own subfolder under data/raw/empirica/,
# holding the same CSV files. Read every matching file across sessions and
# stack them into one data frame per file type.

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

survey_raw <- read.csv(here("data", "raw", "qualtrics",
                            "survey_numeric.csv")) |>
  clean_names()

# Zoom --------------------------------------------------------------------

zoom_raw <- read.csv(here("data", "raw", "zoom",
                          "expert_participation.csv")) |>
  clean_names()

# Experiment design -------------------------------------------------------

team_assign_raw <- read.csv(here("data", "raw", "design",
                                 "team_condition_assignments.csv")) |>
  clean_names()

constraint_combos_raw <- read_xlsx(
  here("data", "raw", "design", "room_assignment_constraint_combos.xlsx"),
  sheet = "Experts_Contraints_All") |>
  clean_names()

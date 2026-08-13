# Authors: Olivia B. Newton and Tyler R. Talbot
# Purpose: Read raw data from data/raw/ into R without modifying it.
#          Cleaning and transformation happen in the 02_clean_*.R scripts.

# Empirica ----------------------------------------------------------------

# Each Empirica session lives in its own subfolder under data/raw/empirica/,
# holding the same CSV files. Read every matching file across sessions and
# stack them into one data frame per file type.

empirica_dir <- here("data", "raw", "empirica")

stages_raw <- list.files(empirica_dir, pattern = "^stages\\.csv$",
                         recursive = TRUE, full.names = TRUE) |>
  map(read.csv) |>
  list_rbind()

players_raw <- list.files(empirica_dir, pattern = "^players\\.csv$",
                          recursive = TRUE, full.names = TRUE) |>
  map(read.csv) |>
  list_rbind()

player_stages_raw <- list.files(empirica_dir, pattern = "^player-stages\\.csv$",
                                recursive = TRUE, full.names = TRUE) |>
  map(read.csv) |>
  list_rbind()

# Qualtrics ---------------------------------------------------------------

survey_raw <- read.csv(here("data", "raw", "qualtrics",
                            "ttalbs_diss_qualtrics_final_numeric.csv"))

# Zoom --------------------------------------------------------------------

zoom_raw <- read.csv(here("data", "raw", "zoom",
                          "expert_participation.csv"))

# Experiment design -------------------------------------------------------

team_assign_raw <- read.csv(here("data", "raw", "design",
                                 "team_condition_assignments.csv"))

constraint_combos_raw <- read_xlsx(
  here("data", "raw", "design", "room_assignment_constraint_combos.xlsx"),
  sheet = "Experts_Contraints_All"
)
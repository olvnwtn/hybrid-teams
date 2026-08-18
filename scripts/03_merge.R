# Authors: Olivia B. Newton and Tyler R. Talbot
# Purpose: Merge all cleaned data into single data frame for analysis. 
#          Additionally, derive expert influence measures.

# Read processed data -----------------------------------------------------

empirica_clean <- read_csv(here("data", "processed", "empirica_team.csv"))
team_data <- read_csv(here("data", "processed", "survey_team.csv"))
design_clean <- read_csv(here("data", "processed", "study_design.csv"))
zoom_clean <- read_csv(here("data", "processed", "zoom_team.csv"))

# Set factor levels -------------------------------------------------------

condition_levels <- c("F2F", "Remote", "Hybrid RE", "Hybrid CE")
virtuality_levels <- c("Matched", "Mixed")

empirica_clean <- empirica_clean |>
  mutate(condition = factor(condition, levels = condition_levels),
         virtuality_condition = factor(virtuality_condition, levels = virtuality_levels))

design_clean <- design_clean |>
  mutate(condition = factor(condition, levels = condition_levels),
         team_order = as.character(team_order))

# Merge Empirica and survey -----------------------------------------------

merged_team <- empirica_clean |>
  inner_join(team_data, by = c("team_id", "condition"))

# Merge with design -------------------------------------------------------

merged_team <- merged_team |>
  mutate(team_order = str_extract(team_id, "[^_]*")) |>
  inner_join(design_clean, by = c("condition", "team_order"))

# Merge with Zoom ---------------------------------------------------------

merged_team <- merged_team |>
  left_join(zoom_clean, by = "team_id")

# Derive expert influence -------------------------------------------------

merged_team <- merged_team |>
  mutate(
    experts_unique_constraints = str_split(experts_unique_constraints, ","),
    violated_constraints = str_split(violated_constraints, ",")
  ) |>
  rowwise() |>
  mutate(
    violated_count_all = length(violated_constraints),
    violated_count_expert = sum(experts_unique_constraints %in% violated_constraints),
    expert_influence = (6 - violated_count_expert) / 6,
    proportional_exp_inf = violated_count_expert / violated_count_all
  ) |>
  ungroup()

# Order columns and drop intermediates ------------------------------------

merged_team <- merged_team |>
  select(team_id, condition, virtuality_condition, everything(),
         -stage_id, -violated_constraints, -experts_unique_constraints,
         -team_constraints, -team_order)

# Save processed data -----------------------------------------------------

write_csv(merged_team, here("data", "processed", "merged_team.csv"))

# Authors: Olivia B. Newton and Tyler R. Talbot
# Purpose: Clean the Zoom expert participation data.

# Clean Zoom data ---------------------------------------------------------

zoom_clean <- zoom_raw |>
  separate(session_id, into = c("condition_no", "team_no"), sep = "_") |>
  mutate(team_id = paste(team_no, condition_no, sep = "_")) |>
  select(team_id, exp_part_in_zoom = participation_secs)

# Save processed data -----------------------------------------------------

write_csv(zoom_clean, here("data", "processed", "zoom_team.csv"))

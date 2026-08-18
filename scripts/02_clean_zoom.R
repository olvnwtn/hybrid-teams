# Authors: Olivia B. Newton and Tyler R. Talbot
# Purpose: Clean the Zoom expert participation data.

# Clean Zoom data ---------------------------------------------------------

zoom_clean <- zoom_raw |>
  rename(team_id = session_id,
         exp_part_in_zoom = participation_secs)

# Save processed data -----------------------------------------------------

write_csv(zoom_clean, here("data", "processed", "zoom_team.csv"))

# Authors: Olivia B. Newton and Tyler R. Talbot
# Purpose: Clean experiment design files. Produces one data frame linking
#          team order and condition to expert-specific constraint sets.

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

write_csv(design_clean, here("data", "processed", "study_design.csv"))

# Author: Olivia B. Newton
# Purpose: Generate manuscript tables from analysis objects.

# Planned contrasts table (Hypotheses 1 and 2) ----------------------------

contrast_labels <- c(
  "H1A: F2F vs Remote"          = "F2F vs. Remote (H1A)",
  "H1B: Remote vs Hybrid"       = "Remote vs. Hybrid (H1B)",
  "H2A: F2F vs Remote"          = "F2F vs. Remote (H2A)",
  "H2B: Remote vs Hybrid"       = "Remote vs. Hybrid (H2B)",
  "H2C: Hybrid CE vs Hybrid RE" = "Hybrid CE vs. Hybrid RE (H2C)"
)

contrast_table <- rbind(
  as.data.frame(summary(h1_contrasts)),
  as.data.frame(summary(h2_contrasts))
) |>
  transmute(
    Contrast   = contrast_labels[as.character(contrast)],
    t          = round(t.ratio, 2),
    df         = df,
    p          = round(p.value, 3),
    r_contrast = round(t.ratio / sqrt(t.ratio^2 + df), 2)
  )

write.csv(contrast_table,
          here("output", "tables", "planned_contrasts.csv"),
          row.names = FALSE)

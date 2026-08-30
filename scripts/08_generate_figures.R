# Authors: Olivia B. Newton and Tyler R. Talbot
# Purpose: Generate figures for manuscript.

# Read data ---------------------------------------------------------------

team_data <- read_csv(here("data", "processed", "merged_team.csv"))

team_data <- team_data |>
  mutate(condition = factor(condition,
                            levels = c("F2F", "Remote",
                                       "Hybrid RE", "Hybrid CE")),
         virtuality_condition = factor(virtuality_condition,
                                       levels = c("Matched", "Mixed")),
         expert_location = factor(expert_location,
                                  levels = c("Co-located", "Remote")))

# H1: Demonstrability by virtuality ---------------------------------------

ggplot(team_data,
       aes(x = virtuality_condition, y = demon_team_median,
           fill = virtuality_condition)) +
  geom_boxplot(notch = T) +
  labs(x = "", y = "Median Group Demonstrability") +
  scale_x_discrete(labels = c("Matched" = "Matched Virtuality",
                              "Mixed" = "Mixed Virtuality")) +
  #scale_y_continuous(limits = c(4, 7)) +
  guides(fill = "none") +
  scale_fill_manual(values = c("Matched" = "white", "Mixed" = "gray65")) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = rel(1.15)))

ggsave(here("output", "figures", "demon_by_virtuality.png"),
       width = 6, height = 6, dpi = 330)

# Demonstrability by condition: faceted notched boxplots with a cross-panel
# significance bracket for the virtuality main effect.

# Faceted base -----------------------------------------------------------

p <- ggboxplot(team_data,
               x = "expert_location",
               y = "demon_team_median",
               fill = "expert_location",
               notch = TRUE,
               width = 0.8,
               facet.by = "virtuality_condition",
               short.panel.labs = FALSE,
               panel.labs = list(virtuality_condition =
                                   c("Matched Virtuality", "Mixed Virtuality"))) +
  scale_fill_manual(values = c("white", "gray65"), guide = "none") +
  scale_x_discrete(expand = expansion(add = 0.3)) +
  labs(x = "Expert Location", y = "Median Group Demonstrability") +
  coord_cartesian(xlim = c(0.5, 2.5), ylim = c(4, 7.7), clip = "off") +
  theme(text = element_text(family = "Times New Roman"),
        panel.spacing = unit(0, "pt"),
        panel.grid.major.y = element_line(color = "gray95"),
        panel.grid.minor.y = element_line(color = "gray95"))

# Cross-panel bracket (virtuality main effect) plus
# within-panel bracket (H1A: F2F vs Remote) ------------------------------

y_bar <- unit(1, "npc") - unit(9, "mm")  # virtuality
y_tip <- y_bar - unit(3, "mm")
y_lab <- unit(1, "npc") - unit(6, "mm")

y_bar2 <- unit(1, "npc") - unit(20, "mm") # h1a
y_tip2 <- y_bar2 - unit(3, "mm")
y_lab2 <- unit(1, "npc") - unit(17, "mm")

lab <- textGrob(expression(italic(p)~"= .013"), x = 0.5, y = y_lab,
                gp = gpar(fontfamily = "Times New Roman", fontsize = 10))

lab2 <- textGrob(expression(italic(p)~"= .039"), x = 0.25, y = y_lab2,
                 gp = gpar(fontfamily = "Times New Roman", fontsize = 10))

bracket <- grobTree(
  segmentsGrob(x0 = 0.25, x1 = 0.75, y0 = y_bar, y1 = y_bar),
  segmentsGrob(x0 = 0.25, x1 = 0.25, y0 = y_bar, y1 = y_tip),
  segmentsGrob(x0 = 0.75, x1 = 0.75, y0 = y_bar, y1 = y_tip),
  rectGrob(x = 0.5, y = y_lab,
           width = grobWidth(lab) + unit(2, "mm"),
           height = grobHeight(lab) + unit(1.5, "mm"),
           gp = gpar(fill = "white", col = NA)),
  lab,
  segmentsGrob(x0 = 0.154, x1 = 0.346, y0 = y_bar2, y1 = y_bar2),
  segmentsGrob(x0 = 0.154, x1 = 0.154, y0 = y_bar2, y1 = y_tip2),
  segmentsGrob(x0 = 0.346, x1 = 0.346, y0 = y_bar2, y1 = y_tip2),
  rectGrob(x = 0.25, y = y_lab2,
           width = grobWidth(lab2) + unit(2, "mm"),
           height = grobHeight(lab2) + unit(1.5, "mm"),
           gp = gpar(fill = "white", col = NA)),
  lab2,
  segmentsGrob(x0 = -0.008, x1 = 0.008, y0 = 0.012, y1 = 0.022),
  segmentsGrob(x0 = -0.008, x1 = 0.008, y0 = 0.028, y1 = 0.038)
)

g <- ggplotGrob(p)
panels <- g$layout[grepl("^panel", g$layout$name), ]
g <- gtable_add_grob(g, bracket,
                     t = min(panels$t),
                     l = min(panels$l), r = max(panels$r),
                     name = "bracket", clip = "off")

grid.newpage()
grid.draw(g)

ggsave(here("output", "figures", "demon_by_condition.png"), plot = g,
       width = 2300, height = 2000, units = "px", dpi = 330)


# H2: Performance by virtuality ------------------------------------------

ggplot(team_data,
       aes(x = virtuality_condition, y = performance_score,
           fill = virtuality_condition)) +
  geom_boxplot(notch = T) +
  labs(x = "", y = "Performance Score") +
  scale_x_discrete(labels = c("Matched" = "Matched Virtuality",
                              "Mixed" = "Mixed Virtuality")) +
  guides(fill = "none") +
  scale_fill_manual(values = c("Matched" = "white", "Mixed" = "gray65")) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = rel(1.15)))

ggsave(here("output", "figures", "perf_by_virtuality.png"),
       width = 6, height = 6, dpi = 330)

# Performance by condition: faceted notched boxplots with a cross-panel
# significance bracket for the virtuality main effect.
# The bracket is added post hoc as a grid grob spanning both panel cells
# of the gtable (ggplot2 internals; gtable::gtable_add_grob), so it cannot
# affect scales and draws inside the plot grid.

# Faceted base -----------------------------------------------------------

p <- ggboxplot(team_data,
               x = "expert_location",
               y = "performance_score",
               fill = "expert_location",
               notch = TRUE,
               width = 0.8,
               facet.by = "virtuality_condition",
               short.panel.labs = FALSE,
               panel.labs = list(virtuality_condition =
                                   c("Matched Virtuality", "Mixed Virtuality"))) +
  scale_fill_manual(values = c("white", "gray65"), guide = "none") +
  scale_x_discrete(expand = expansion(add = 0.3)) +
  labs(x = "Expert Location", y = "Performance Score") +
  coord_cartesian(xlim = c(0.5, 2.5), ylim = c(0, 1050), clip = "off") +
  theme(text = element_text(family = "Times New Roman"),
        panel.spacing = unit(0, "pt"),
        panel.grid.major.y = element_line(color = "gray95"),
        panel.grid.minor.y = element_line(color = "gray95"))

# Cross-panel bracket ----------------------------------------------------
# npc coordinates within the region spanning both panels:
# each panel's center (between its two boxes) sits at ~0.25 and ~0.75

y_bar <- unit(1, "npc") - unit(8, "mm")
y_tip <- y_bar - unit(3, "mm")
y_lab <- unit(1, "npc") - unit(5, "mm")

lab <- textGrob(expression(italic(p)~"= .019"), x = 0.5, y = y_lab,
                gp = gpar(fontfamily = "Times New Roman", fontsize = 10))

bracket <- grobTree(
  segmentsGrob(x0 = 0.25, x1 = 0.75, y0 = y_bar, y1 = y_bar),
  segmentsGrob(x0 = 0.25, x1 = 0.25, y0 = y_bar, y1 = y_tip),
  segmentsGrob(x0 = 0.75, x1 = 0.75, y0 = y_bar, y1 = y_tip),
  rectGrob(x = 0.5, y = y_lab,
           width = grobWidth(lab) + unit(2, "mm"),
           height = grobHeight(lab) + unit(1.5, "mm"),
           gp = gpar(fill = "white", col = NA)),
  lab
)

g <- ggplotGrob(p)
panels <- g$layout[grepl("^panel", g$layout$name), ]
g <- gtable_add_grob(g, bracket,
                     t = min(panels$t),
                     l = min(panels$l), r = max(panels$r),
                     name = "bracket", clip = "off")

grid.newpage()
grid.draw(g)

ggsave(here("output", "figures", "perf_by_condition.png"), plot = g,
       width = 2300, height = 2000, units = "px", dpi = 330)

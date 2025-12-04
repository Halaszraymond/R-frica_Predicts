## =========================================
## plotting_tournament_table.R
## - Build AFCON bracket + group tables
## - Uses res (from simulation.R)
## =========================================

library(dplyr)
library(ggplot2)
library(cowplot)
library(grid)

# 1. Extract data from res ------------------------------

r16_pairs <- res$round_of_16_pairs
r16_w     <- res$r16_winners
qf_w      <- res$quarter_finalists
sf_w      <- res$semi_finalists
final_p   <- res$final_pair
champion  <- res$champion

group_df  <- res$group_results %>%
  mutate(label = sprintf("%s (%d pts, GD %d)", team, points, gd))

# 2. Coordinates for the bracket ------------------------

r16_y   <- c(17, 13,  9,  5,  5,  9, 13, 17)
qf_y    <- c(15,  7,  7, 15)
sf_y    <- c(13,  9)
final_y <- 11

r16_df <- r16_pairs %>%
  mutate(
    round  = "R16",
    match  = row_number(),
    winner = r16_w[match],
    x      = if_else(match <= 4, 1, 5),
    y      = r16_y
  )

qf_df <- tibble::tibble(
  team1 = r16_w[c(1, 3, 5, 7)],
  team2 = r16_w[c(2, 4, 6, 8)]
) %>%
  mutate(
    round  = "QF",
    match  = row_number(),
    winner = qf_w[match],
    x      = if_else(match <= 2, 2, 4),
    y      = qf_y
  )

sf_df <- tibble::tibble(
  team1 = qf_w[c(1, 3)],
  team2 = qf_w[c(2, 4)]
) %>%
  mutate(
    round  = "SF",
    match  = row_number(),
    winner = sf_w[match],
    x      = 3,
    y      = sf_y
  )

final_df <- final_p %>%
  mutate(
    round  = "Final",
    match  = 1L,
    winner = champion,
    x      = 3,
    y      = final_y
  )

bracket_df <- bind_rows(r16_df, qf_df, sf_df, final_df) %>%
  mutate(
    label = if_else(
      round != "Final",
      sprintf("%s\nvs\n%s\nWinner: %s", team1, team2, winner),
      sprintf("%s\nvs\n%s\nCHAMPION: %s", team1, team2, winner)
    )
  )

# 3. Bracket plot ---------------------------------------

p_bracket <- ggplot(bracket_df, aes(x = x, y = y)) +
  geom_rect(
    aes(
      xmin = x - 0.6, xmax = x + 0.6,
      ymin = y - 1.0, ymax = y + 1.0,
      fill = round
    ),
    colour    = "white",
    linewidth = 0.6
  ) +
  geom_text(
    aes(label = label),
    colour     = "white",
    size       = 3,
    lineheight = 1.1
  ) +
  scale_fill_manual(values = c(
    "R16"   = "#1E88E5",
    "QF"    = "#43A047",
    "SF"    = "#FB8C00",
    "Final" = "#8E24AA"
  )) +
  coord_cartesian(
    xlim   = c(0.5, 5.5),
    ylim   = c(3, 19),
    expand = FALSE
  ) +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "#0D47A1", colour = NA),
    panel.background = element_rect(fill = "#0D47A1", colour = NA),
    legend.position  = "none",
    plot.margin      = unit(c(5, 5, 5, 5), "pt")
  ) +
  annotate(
    "text",
    x      = 3,
    y      = 18.2,
    label  = "AFCON 2025 – Simulated Bracket",
    colour = "white",
    fontface = "bold",
    size   = 6
  )

# 4. Group table helper --------------------------------

make_group_table <- function(groups_vec) {
  group_df %>%
    filter(group %in% groups_vec) %>%
    ggplot(aes(x = 0.5, y = -position)) +
    geom_rect(
      aes(
        xmin = 0.05, xmax = 0.95,
        ymin = -position - 0.45,
        ymax = -position + 0.45
      ),
      fill    = "#2196F3",
      colour  = "white",
      linewidth = 0.4
    ) +
    geom_text(
      aes(label = label),
      hjust    = 0,
      nudge_x  = -0.35,
      colour   = "white",
      size     = 3
    ) +
    facet_wrap(~ group, ncol = 1, strip.position = "top") +
    coord_cartesian(xlim = c(0, 1.0), expand = FALSE) +
    theme_void() +
    theme(
      strip.background = element_rect(fill = "#FF9800"),
      strip.text       = element_text(face = "bold", colour = "white", size = 10),
      plot.background  = element_rect(fill = "#0D47A1", colour = NA),
      panel.background = element_rect(fill = "#0D47A1", colour = NA),
      panel.spacing    = unit(3, "pt"),
      plot.margin      = unit(c(5, 5, 5, 5), "pt")
    )
}

p_left  <- make_group_table(c("A", "B", "C"))
p_right <- make_group_table(c("D", "E", "F"))

# 5. Combine plots --------------------------------------

final_plot <- cowplot::plot_grid(
  p_left, p_bracket, p_right,
  nrow       = 1,
  rel_widths = c(0.65, 1.7, 0.65),
  align      = "h"
)

final_plot

## Optional:
# ggsave("afcon_2025_bracket_equal_sides.png",
#        final_plot, width = 11, height = 5, dpi = 100)
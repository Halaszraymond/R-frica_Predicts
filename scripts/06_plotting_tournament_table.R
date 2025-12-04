## =========================================
## 06_plotting_tournament_table.R
## - Load saved AFCON simulation result
## - Build knockout bracket (R16 → QF → SF → Final)
## - Build group stage tables
## - Arrange everything into one combined tournament graphic
## - Save the plot to disk and display the saved PNG
## =========================================

library(dplyr)
library(ggplot2)
library(cowplot)  # arranging multiple ggplots
library(grid)     # low-level grid graphics (for raster image display)
library(png)      # readPNG() for loading the saved PNG

# -------------------------------------------------------
# 0. Load simulation result
# -------------------------------------------------------

# The simulation result 'res' is produced and saved in 05_simulation.R
# as: results/afcon_2025_sim_seed123.rds.
# Here we load that object and use it to build our visuals.

# Ensure the results folder exists; if not, the simulation has not been run.
if (!dir.exists("results")) {
  stop("results/ folder not found. Run 05_simulation.R first to create it.")
}

# Load the specific simulation run (with seed 123).
# If you later have multiple simulations, you could generalize this.
res <- readRDS("results/afcon_2025_sim_seed123.rds")

# -------------------------------------------------------
# 1. Extract data from res
# -------------------------------------------------------

# These objects were created in simulate_afcon_2025() and stored in res:
#  - round_of_16_pairs : who plays whom in R16
#  - r16_winners       : winners of R16
#  - quarter_finalists : winners of QFs
#  - semi_finalists    : winners of SFs
#  - final_pair        : teams in the final
#  - champion          : tournament winner
#  - group_results     : final standings in each group

r16_pairs <- res$round_of_16_pairs
r16_w     <- res$r16_winners
qf_w      <- res$quarter_finalists
sf_w      <- res$semi_finalists
final_p   <- res$final_pair
champion  <- res$champion

# For group tables:
# group_results contains: team, points, gd, group, position
# Create a label combining team name, points, and goal difference.
group_df  <- res$group_results %>%
  mutate(label = sprintf("%s (%d pts, GD %d)", team, points, gd))

# -------------------------------------------------------
# 2. Coordinates for the bracket
# -------------------------------------------------------

# We manually define y-coordinates for each round so the boxes
# appear in a pleasing bracket structure:
#  - R16: 8 matches spread vertically
#  - QF : 4 matches
#  - SF : 2 matches
#  - Final: 1 match in the center

r16_y   <- c(17, 13,  9,  5,  5,  9, 13, 17)
qf_y    <- c(15,  7,  7, 15)
sf_y    <- c(13,  9)
final_y <- 11

# Round of 16 data frame:
#  - Add 'round' label
#  - 'match' index
#  - 'winner' from r16_w
#  - x placement: left side x = 1, right side x = 5
r16_df <- r16_pairs %>%
  mutate(
    round  = "R16",
    match  = dplyr::row_number(),
    winner = r16_w[match],
    x      = dplyr::if_else(match <= 4, 1, 5),
    y      = r16_y
  )

# Quarter-finals:
#  - Construct QF pairs from R16 winners in bracket order
qf_df <- tibble::tibble(
  team1 = r16_w[c(1, 3, 5, 7)],
  team2 = r16_w[c(2, 4, 6, 8)]
) %>%
  mutate(
    round  = "QF",
    match  = dplyr::row_number(),
    winner = qf_w[match],
    # QF matches: left QFs at x = 2, right QFs at x = 4
    x      = dplyr::if_else(match <= 2, 2, 4),
    y      = qf_y
  )

# Semi-finals:
#  - Winners of QFs arranged into two SF matches
sf_df <- tibble::tibble(
  team1 = qf_w[c(1, 3)],
  team2 = qf_w[c(2, 4)]
) %>%
  mutate(
    round  = "SF",
    match  = dplyr::row_number(),
    winner = sf_w[match],
    x      = 3,       # both SF matches located in the central column
    y      = sf_y
  )

# Final:
#  - Final pair and champion at center
final_df <- final_p %>%
  mutate(
    round  = "Final",
    match  = 1L,
    winner = champion,
    x      = 3,
    y      = final_y
  )

# Combine all rounds into a single data frame for plotting.
# Create a text label showing the fixture and winner for each match,
# and for the final highlight the champion.
bracket_df <- dplyr::bind_rows(r16_df, qf_df, sf_df, final_df) %>%
  mutate(
    label = dplyr::if_else(
      round != "Final",
      sprintf("%s\nvs\n%s\nWinner: %s", team1, team2, winner),
      sprintf("%s\nvs\n%s\nCHAMPION: %s", team1, team2, winner)
    )
  )

# -------------------------------------------------------
# 3. Bracket plot
# -------------------------------------------------------

# This plot shows all knockout rounds on a dark background.
# Each game is a colored rectangle with text inside describing:
#   - the two teams
#   - the winner (or champion)

p_bracket <- ggplot(bracket_df, aes(x = x, y = y)) +
  # Draw rectangles for each game
  geom_rect(
    aes(
      xmin = x - 0.6, xmax = x + 0.6,
      ymin = y - 1.0, ymax = y + 1.0,
      fill = round
    ),
    colour    = "white",
    linewidth = 0.6
  ) +
  # Add the match labels inside the boxes
  geom_text(
    aes(label = label),
    colour     = "white",
    size       = 3,
    lineheight = 1.1
  ) +
  # Distinct colors per round
  scale_fill_manual(values = c(
    "R16"   = "#1E88E5",
    "QF"    = "#43A047",
    "SF"    = "#FB8C00",
    "Final" = "#8E24AA"
  )) +
  # Fix the coordinate space to frame all rounds nicely
  coord_cartesian(
    xlim   = c(0.5, 5.5),
    ylim   = c(3, 19),
    expand = FALSE
  ) +
  # Remove default axes, gridlines, etc.
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "#0D47A1", colour = NA),
    panel.background = element_rect(fill = "#0D47A1", colour = NA),
    legend.position  = "none",
    plot.margin      = unit(c(5, 5, 5, 5), "pt")
  ) +
  # Title annotation at the top of the bracket panel
  annotate(
    "text",
    x      = 3,
    y      = 18.2,
    label  = "AFCON 2025 – Simulated Bracket",
    colour = "white",
    fontface = "bold",
    size   = 6
  )

# -------------------------------------------------------
# 4. Group table helper
# -------------------------------------------------------

# make_group_table():
#   Builds a vertical strip of group tables (A–C on the left, D–F on the right).
#   Each row in a group shows:
#     - team name
#     - total points
#     - goal difference (GD)
#   The groups get colored headers.

make_group_table <- function(groups_vec) {
  group_df %>%
    filter(group %in% groups_vec) %>%
    ggplot(aes(x = 0.5, y = -position)) +
    # Background rectangle for each team row
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
    # Team label (team + points + GD)
    geom_text(
      aes(label = label),
      hjust    = 0,        # left-justify text
      nudge_x  = -0.35,    # move text slightly left inside the rectangle
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

# Build left and right columns of groups:
#  - Left: Groups A, B, C
#  - Right: Groups D, E, F
p_left  <- make_group_table(c("A", "B", "C"))
p_right <- make_group_table(c("D", "E", "F"))

# -------------------------------------------------------
# 5. Combine plots (groups + bracket)
# -------------------------------------------------------

# Use cowplot::plot_grid to place:
#   [p_left]   [p_bracket]   [p_right]
# with relative widths so the bracket gets more horizontal space.

final_plot <- cowplot::plot_grid(
  p_left, p_bracket, p_right,
  nrow       = 1,
  rel_widths = c(0.65, 1.7, 0.65),
  align      = "h"
)

# Show the combined ggplot object in the current device
final_plot

# -------------------------------------------------------
# 6. Save to plots/ folder
# -------------------------------------------------------

# We also save the figure to disk as a PNG so it can be used in
# reports, presentations, or shared as an image.

# Ensure the plots folder exists
if (!dir.exists("plots")) {
  dir.create("plots", recursive = TRUE)
}

out_file <- "plots/afcon_2025_prediction.png"

ggsave(
  filename = out_file,
  plot     = final_plot,
  width    = 11,
  height   = 5,
  dpi      = 100
)

# -------------------------------------------------------
# 7. Display the *saved* PNG image
# -------------------------------------------------------

# To confirm that what we saved matches what we see, we:
#   1. Read the saved PNG from disk
#   2. Draw it as a raster image on a new grid page

img <- png::readPNG(out_file)
grid::grid.newpage()
grid::grid.raster(img)

print("Plotting AFCON 2025 Prediction: Done")




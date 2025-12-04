## ==============================
## 05_simulation.R
## - simulate_afcon_2025()
## - define groups + name_map
## - run one simulation -> res
## - save result to results/
## ==============================
##
## Uses:
## - rf_model.rds  (pre-trained Random Forest from models/)
## - ranks         (FIFA rankings from feature_engineer.R; must be
##                  available in the environment as a data frame)
##
## This script:
##  1. Loads the trained Random Forest model used to predict match outcomes
##  2. Defines AFCON 2025 groups (or accepts custom groups)
##  3. Uses the latest FIFA rankings to build features for each matchup
##  4. Simulates:
##      - Group stage (round-robin)
##      - Qualification of top 2 + best 4 third-placed teams
##      - Knockout rounds (R16 → QF → SF → Final)
##  5. Returns a detailed result object and saves it to results/

library(dplyr)
library(lubridate)
library(tibble)
library(stringr)
library(randomForest)  # for predict.randomForest

# ---------- Ensure results/ folder exists ----------

# We store simulation output (res object) as an .rds file here.
# If the folder doesn't exist yet, create it.
if (!dir.exists("results")) {
  dir.create("results", recursive = TRUE)
}

# ---------- Load Random Forest model from models/ ----------

# The model was trained in modeling.R and saved as "models/rf_model.rds".
# It predicts the outcome of a match (H/D/A) from engineered features.
rf_fit <- readRDS("models/rf_model.rds")

# Name map for FIFA rankings
# --------------------------
# Some country names in the AFCON context differ from how they appear
# in the FIFA rankings data (ranks_df$country_full). This map allows
# us to translate AFCON team names to their corresponding FIFA names.
name_map <- c(
  "DR Congo"    = "Congo DR",
  "Ivory Coast" = "Côte d'Ivoire"
)

# -------------------------------------------------------------------
# simulate_afcon_2025()
#
# Main function to simulate the AFCON 2025 tournament.
#
# Arguments:
#   model         : fitted classification model with predict(type = "prob"),
#                   default is the RF loaded above (rf_fit)
#   ranks_df      : data frame with FIFA rankings, must contain:
#                     - country_full
#                     - rank
#                     - total_points
#                     - rank_date
#   afcon_groups  : named list of groups (A–F), each with 4 teams. If NULL,
#                   a default AFCON 2025 group setup is used.
#   year_for_model: numeric year to set in the feature 'year' for predictions
#   seed          : optional seed for reproducibility of the tournament draw
#   return_details: if TRUE, returns a list with champion and full details;
#                   if FALSE, returns only the champion (team name)
#
# Returns:
#   Either:
#   - champion (character), if return_details = FALSE
#   - list with:
#       champion
#       final_pair
#       semi_finalists
#       quarter_finalists
#       r16_winners
#       group_results
#       knockout_seeding
#       round_of_16_pairs
# -------------------------------------------------------------------

simulate_afcon_2025 <- function(model        = rf_fit,
                                ranks_df,
                                afcon_groups = NULL,
                                year_for_model = 2025,
                                seed = NULL,
                                return_details = TRUE) {
  # Control randomness for reproducibility, if seed is provided
  if (!is.null(seed)) set.seed(seed)
  
  # 1. Default groups ------------------------------------------------
  # If no custom groups are supplied, use a default AFCON 2025 layout.
  if (is.null(afcon_groups)) {
    afcon_groups <- list(
      A = c("Morocco", "Mali", "Zambia", "Comoros"),
      B = c("Egypt", "South Africa", "Angola", "Zimbabwe"),
      C = c("Nigeria", "Tunisia", "Uganda", "Tanzania"),
      D = c("Senegal", "DR Congo", "Benin", "Botswana"),
      E = c("Algeria", "Burkina Faso", "Equatorial Guinea", "Sudan"),
      F = c("Ivory Coast", "Cameroon", "Gabon", "Mozambique")
    )
  }
  
  # Group size sanity check: each group must have at least 2 teams.
  group_sizes <- vapply(afcon_groups, length, integer(1))
  if (any(group_sizes < 2)) {
    bad <- names(afcon_groups)[group_sizes < 2]
    stop("These groups have fewer than 2 teams: ",
         paste(bad, collapse = ", "),
         "\nCheck 'afcon_groups'.")
  }
  
  # 2. Latest FIFA ranking snapshot ---------------------------------
  # We use a *single* ranking snapshot: the most recent rank_date
  # available in ranks_df. This serves as the "current strength" of
  # each team going into the tournament.
  ranks_df$rank_date <- as.Date(ranks_df$rank_date)
  latest_rank_date <- max(ranks_df$rank_date, na.rm = TRUE)
  
  ranks_latest <- ranks_df |>
    filter(rank_date == latest_rank_date) |>
    dplyr::select(country_full, rank, total_points)
  
  # --- Helper functions --------------------------------------------
  
  # Get ranking info for a given team:
  #  - Apply name_map if needed (e.g. "Ivory Coast" → "Côte d'Ivoire")
  #  - Fetch its rank and total_points from ranks_latest
  get_team_rank <- function(team_name) {
    canonical_name <- if (team_name %in% names(name_map)) {
      name_map[[team_name]]
    } else {
      team_name
    }
    
    rec <- ranks_latest |> filter(country_full == canonical_name)
    if (nrow(rec) == 0) {
      stop(
        "Missing ranking for team: ", team_name,
        " (mapped as '", canonical_name, "')",
        "\nCheck spelling vs 'country_full' in ranks_df or extend name_map."
      )
    }
    c(rank = rec$rank[1], total_points = rec$total_points[1])
  }
  
  # Build feature vector for a match between team1 (home) and team2 (away).
  # This mirrors the structure used in modeling.R:
  #   - rank_diff   : away_rank - home_rank
  #   - points_diff : home_points - away_points
  #   - is_afcon    : TRUE for AFCON tournament
  #   - is_qual     : FALSE (we treat this as main tournament)
  #   - is_friendly : FALSE
  #   - year        : year_for_model
  build_match_features <- function(team1, team2) {
    home <- get_team_rank(team1)
    away <- get_team_rank(team2)
    
    tibble(
      rank_diff   = away["rank"] - home["rank"],
      points_diff = home["total_points"] - away["total_points"],
      is_afcon    = TRUE,
      is_qual     = FALSE,
      is_friendly = FALSE,
      year        = year_for_model
    )
  }
  
  # Predict outcome probabilities for a match:
  # returns a named vector with classes (H, D, A) and their probabilities.
  predict_match_prob <- function(team1, team2) {
    feats <- build_match_features(team1, team2)
    probs <- predict(model, newdata = feats, type = "prob")[1, ]
    probs
  }
  
  # Simulate a single group-stage match based on predicted probabilities.
  #  - We sample an outcome (H/D/A) according to the model probabilities.
  #  - We award points: win = 3, draw = 1, loss = 0
  #  - We approximate goal difference as +1 / -1 / 0 for simplicity.
  simulate_match <- function(team1, team2) {
    probs <- predict_match_prob(team1, team2)
    outcome <- sample(c("H", "D", "A"), size = 1, prob = as.numeric(probs))
    
    if (outcome == "H") {
      tibble(team = c(team1, team2),
             points = c(3, 0),
             gd     = c(1, -1))
    } else if (outcome == "A") {
      tibble(team = c(team1, team2),
             points = c(0, 3),
             gd     = c(-1, 1))
    } else {
      tibble(team = c(team1, team2),
             points = c(1, 1),
             gd     = c(0, 0))
    }
  }
  
  # Generate all pairwise fixtures within a group (round-robin).
  # For N teams, this creates N*(N-1)/2 matches.
  group_fixtures <- function(teams) {
    if (length(teams) < 2) {
      stop("group_fixtures called with < 2 teams: ",
           paste(teams, collapse = ", "))
    }
    combn(teams, 2) |>
      t() |>
      as_tibble(.name_repair = "minimal") |>
      rlang::set_names(c("team1", "team2"))
  }
  
  # Simulate an entire group:
  #  1. Generate fixtures
  #  2. Simulate each match
  #  3. Aggregate points and goal difference
  #  4. Rank teams within the group
  simulate_group <- function(group_name, teams) {
    fixtures <- group_fixtures(teams)
    
    results <- do.call(
      rbind,
      lapply(seq_len(nrow(fixtures)), function(i) {
        t1 <- fixtures$team1[i]
        t2 <- fixtures$team2[i]
        simulate_match(t1, t2)
      })
    ) |>
      as_tibble()
    
    standings <- results |>
      group_by(team) |>
      summarise(points = sum(points),
                gd     = sum(gd),
                .groups = "drop") |>
      arrange(desc(points), desc(gd)) |>
      mutate(group = group_name,
             position = row_number())
    
    standings
  }
  
  # Simulate one knockout round (e.g., R16, QF, SF, Final).
  # pairs_df should have columns team1, team2.
  #  - We sample match result (H/D/A).
  #  - If draw, we break tie using only H vs A probabilities (no draws).
  simulate_knockout_round <- function(pairs_df) {
    winners <- sapply(seq_len(nrow(pairs_df)), function(i) {
      t1 <- pairs_df$team1[i]
      t2 <- pairs_df$team2[i]
      probs <- predict_match_prob(t1, t2)
      
      res <- sample(c("H", "D", "A"), size = 1, prob = as.numeric(probs))
      if (res == "H") return(t1)
      if (res == "A") return(t2)
      
      # If draw: decide winner by sampling again between H and A only
      pH <- probs["H"]; pA <- probs["A"]
      sample(c(t1, t2), size = 1, prob = c(pH, pA))
    })
    
    tibble(winner = winners)
  }
  
  # 4. Group stage ---------------------------------------------------
  # Simulate all groups independently and combine into one table.
  group_names <- names(afcon_groups)
  group_list <- lapply(seq_along(afcon_groups), function(i) {
    gname <- group_names[i]
    teams <- afcon_groups[[i]]
    simulate_group(gname, teams)
  })
  group_results <- bind_rows(group_list)
  
  # 5. Qualifiers for knockout stage --------------------------------
  # Standard AFCON-style logic:
  #  - Top 2 from each group qualify automatically (6 groups → 12 teams)
  #  - Best 4 third-placed teams qualify as well → total 16 teams
  auto_qualified <- group_results |> filter(position <= 2)
  third_placed   <- group_results |>
    filter(position == 3) |>
    arrange(desc(points), desc(gd))
  best_thirds    <- third_placed |> slice(1:4)
  
  knockout_teams <- bind_rows(auto_qualified, best_thirds)
  
  # 6. Seeding + Round of 16 pairings -------------------------------
  # We seed teams based on group performance (points, then GD),
  # then pair 1 vs 16, 2 vs 15, ..., 8 vs 9.
  seeded <- knockout_teams |>
    arrange(desc(points), desc(gd)) |>
    mutate(seed = row_number())
  
  round_of_16_pairs <- tibble(
    team1_seed = c(1, 2, 3, 4, 5, 6, 7, 8),
    team2_seed = c(16, 15, 14, 13, 12, 11, 10, 9)
  ) |>
    left_join(seeded |> dplyr::select(seed, team1 = team),
              by = c("team1_seed" = "seed")) |>
    left_join(seeded |> dplyr::select(seed, team2 = team),
              by = c("team2_seed" = "seed"))
  
  # 7. Knockout rounds ----------------------------------------------
  # Round of 16
  r16        <- simulate_knockout_round(round_of_16_pairs)
  r16_w      <- r16$winner
  
  # Quarter-finals: (1 vs 2), (3 vs 4), (5 vs 6), (7 vs 8) among R16 winners
  qf_pairs <- tibble(
    team1 = r16_w[c(1, 3, 5, 7)],
    team2 = r16_w[c(2, 4, 6, 8)]
  )
  qf        <- simulate_knockout_round(qf_pairs)
  qf_w      <- qf$winner
  
  # Semi-finals: (1 vs 2), (3 vs 4) among QF winners
  sf_pairs <- tibble(
    team1 = qf_w[c(1, 3)],
    team2 = qf_w[c(2, 4)]
  )
  sf        <- simulate_knockout_round(sf_pairs)
  sf_w      <- sf$winner
  
  # Final: winner SF1 vs winner SF2
  final_pair <- tibble(team1 = sf_w[1], team2 = sf_w[2])
  final_res  <- simulate_knockout_round(final_pair)
  champion   <- final_res$winner[1]
  
  # 8. Return output -------------------------------------------------
  if (!return_details) {
    # Only return the champion (team name)
    return(champion)
  } else {
    # Return a list containing full tournament details
    return(list(
      champion          = champion,
      final_pair        = final_pair,
      semi_finalists    = sf_w,
      quarter_finalists = qf_w,
      r16_winners       = r16_w,
      group_results     = group_results,
      knockout_seeding  = seeded,
      round_of_16_pairs = round_of_16_pairs
    ))
  }
}

# -------------------------------------------------------------------
# Explicit AFCON groups (same as default, but defined here for clarity)
# -------------------------------------------------------------------
afcon_groups <- list(
  A = c("Morocco", "Mali", "Zambia", "Comoros"),
  B = c("Egypt", "South Africa", "Angola", "Zimbabwe"),
  C = c("Nigeria", "Tunisia", "Uganda", "Tanzania"),
  D = c("Senegal", "DR Congo", "Benin", "Botswana"),
  E = c("Algeria", "Burkina Faso", "Equatorial Guinea", "Sudan"),
  F = c("Ivory Coast", "Cameroon", "Gabon", "Mozambique")
)

# Quick check: all groups should have 4 teams
sapply(afcon_groups, length)

# -------------------------------------------------------------------
# Run one simulation using the RF model from models/
# 'ranks' must already be loaded into the environment (from
# feature_engineer.R or another script).
# -------------------------------------------------------------------
res <- simulate_afcon_2025(
  # model defaults to rf_fit loaded from models/
  ranks_df     = ranks,
  afcon_groups = afcon_groups,
  seed         = 123
)

# Inspect the winner and group standings in the console
res$champion
res$group_results

# ---------- Save result to results/ folder ----------

saveRDS(res, file = "results/afcon_2025_sim_seed123.rds")
cat("Simulation result saved to results/afcon_2025_sim_seed123.rds\n")

print("Simulating AFCON 2025: Done")













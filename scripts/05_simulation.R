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
## - ranks         (FIFA rankings from feature_engineering.R; must be
##                  available in the environment as a data frame)
## - data/afcon_country_matches_2000-2025.csv
##   (for head-to-head summaries)
##
## This script:
##  1. Loads the trained Random Forest model used to predict match outcomes
##  2. Builds a head-to-head summary table from historical matches
##  3. Defines AFCON 2025 groups (or accepts custom groups)
##  4. Uses the latest FIFA rankings + H2H data to build features
##  5. Simulates:
##      - Group stage (round-robin)
##      - Qualification of top 2 + best 4 third-placed teams
##      - Knockout rounds (R16 -> QF -> SF -> Final)
##  6. Returns a detailed result object and saves it to results/

library(dplyr)
library(lubridate)
library(tibble)
library(stringr)
library(randomForest)  # for predict.randomForest
library(readr)

# ---------- Ensure results/ folder exists ----------

if (!dir.exists("results")) {
  dir.create("results", recursive = TRUE)
}

# ---------- Load Random Forest model from models/ ----------

rf_fit <- readRDS("models/rf_model.rds")

# ---------- Build head-to-head summary from historical matches ----------

# We use the same raw match file as in feature engineering
matches_hist <- read_csv("data/afcon_country_matches_2000-2025.csv",
                         show_col_types = FALSE) |>
  mutate(date = as.Date(date))

# For each unordered pair of teams (team_low, team_high), compute:
#  - total number of matches
#  - total goal difference from team_low perspective
#  - total result sum from team_low perspective (win=1, draw=0, loss=-1)
h2h_summary <- matches_hist |>
  mutate(
    team_low  = pmin(home_team, away_team),
    team_high = pmax(home_team, away_team),
    low_goal_diff = case_when(
      home_team == team_low ~ home_score - away_score,
      TRUE                  ~ away_score - home_score
    ),
    low_result_num = case_when(
      home_score == away_score ~ 0,
      (home_team == team_low & home_score > away_score) |
        (away_team == team_low & away_score > home_score) ~ 1,
      TRUE ~ -1
    )
  ) |>
  group_by(team_low, team_high) |>
  summarise(
    h2h_matches_total       = n(),
    h2h_goal_diff_low_total = sum(low_goal_diff, na.rm = TRUE),
    h2h_result_sum_low_total = sum(low_result_num, na.rm = TRUE),
    .groups = "drop"
  )

# Name map for FIFA rankings
# --------------------------
name_map <- c(
  "DR Congo"    = "Congo DR",
  "Ivory Coast" = "Côte d'Ivoire"
)

# -------------------------------------------------------------------
# Helper: get head-to-head features for a pair (home, away)
# -------------------------------------------------------------------
get_h2h_features <- function(home_team, away_team) {
  tl <- pmin(home_team, away_team)
  th <- pmax(home_team, away_team)
  
  rec <- h2h_summary |>
    filter(team_low == tl, team_high == th)
  
  if (nrow(rec) == 0) {
    # No previous meetings: neutral H2H
    matches_before_home <- 0
    goal_diff_before_home <- 0
    winrate_before <- 0.5
  } else {
    matches_before_home <- rec$h2h_matches_total[1]
    goal_diff_low_total <- rec$h2h_goal_diff_low_total[1]
    result_sum_low_total <- rec$h2h_result_sum_low_total[1]
    
    # Convert from team_low perspective to HOME team perspective
    if (home_team == tl) {
      goal_diff_before_home <- goal_diff_low_total
      result_sum_before_home <- result_sum_low_total
    } else {
      goal_diff_before_home <- -goal_diff_low_total
      result_sum_before_home <- -result_sum_low_total
    }
    
    winrate_before <- (result_sum_before_home + matches_before_home) /
      (2 * matches_before_home)
  }
  
  list(
    h2h_matches_before_home = matches_before_home,
    h2h_goal_diff_before_home = goal_diff_before_home,
    h2h_home_winrate_before = winrate_before
  )
}

# -------------------------------------------------------------------
# simulate_afcon_2025()
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
  
  group_sizes <- vapply(afcon_groups, length, integer(1))
  if (any(group_sizes < 2)) {
    bad <- names(afcon_groups)[group_sizes < 2]
    stop("These groups have fewer than 2 teams: ",
         paste(bad, collapse = ", "),
         "\nCheck 'afcon_groups'.")
  }
  
  # 2. Latest FIFA ranking snapshot ---------------------------------
  ranks_df$rank_date <- as.Date(ranks_df$rank_date)
  latest_rank_date <- max(ranks_df$rank_date, na.rm = TRUE)
  
  ranks_latest <- ranks_df |>
    filter(rank_date == latest_rank_date) |>
    dplyr::select(country_full, rank, total_points)
  
  # --- Helper functions inside simulate_afcon_2025 ------------------
  
  # Get ranking info for a given team
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
  
  # Build feature vector for a match (home = team1, away = team2)
  # Mirrors predictors used in 04_modeling.R:
  #   rank_diff, points_diff, is_afcon, is_qual, is_friendly,
  #   year, h2h_matches_before_home, h2h_goal_diff_before_home,
  #   h2h_home_winrate_before
  build_match_features <- function(team1, team2) {
    home <- get_team_rank(team1)
    away <- get_team_rank(team2)
    h2h  <- get_h2h_features(team1, team2)
    
    tibble(
      rank_diff   = away["rank"] - home["rank"],
      points_diff = home["total_points"] - away["total_points"],
      is_afcon    = TRUE,
      is_qual     = FALSE,
      is_friendly = FALSE,
      year        = year_for_model,
      h2h_matches_before_home  = h2h$h2h_matches_before_home,
      h2h_goal_diff_before_home = h2h$h2h_goal_diff_before_home,
      h2h_home_winrate_before  = h2h$h2h_home_winrate_before
    )
  }
  
  # Predict outcome probabilities for a match
  predict_match_prob <- function(team1, team2) {
    feats <- build_match_features(team1, team2)
    probs <- predict(model, newdata = feats, type = "prob")[1, ]
    probs
  }
  
  # Simulate a single group-stage match
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
  
  # Generate all pairwise fixtures within a group (round-robin)
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
  
  # Simulate an entire group
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
  
  # Simulate one knockout round (R16, QF, SF, Final)
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
  group_names <- names(afcon_groups)
  group_list <- lapply(seq_along(afcon_groups), function(i) {
    gname <- group_names[i]
    teams <- afcon_groups[[i]]
    simulate_group(gname, teams)
  })
  group_results <- bind_rows(group_list)
  
  # 5. Qualifiers for knockout stage --------------------------------
  auto_qualified <- group_results |> filter(position <= 2)
  third_placed   <- group_results |>
    filter(position == 3) |>
    arrange(desc(points), desc(gd))
  best_thirds    <- third_placed |> slice(1:4)
  
  knockout_teams <- bind_rows(auto_qualified, best_thirds)
  
  # 6. Seeding + Round of 16 pairings -------------------------------
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
  
  # Quarter-finals
  qf_pairs <- tibble(
    team1 = r16_w[c(1, 3, 5, 7)],
    team2 = r16_w[c(2, 4, 6, 8)]
  )
  qf        <- simulate_knockout_round(qf_pairs)
  qf_w      <- qf$winner
  
  # Semi-finals
  sf_pairs <- tibble(
    team1 = qf_w[c(1, 3)],
    team2 = qf_w[c(2, 4)]
  )
  sf        <- simulate_knockout_round(sf_pairs)
  sf_w      <- sf$winner
  
  # Final
  final_pair <- tibble(team1 = sf_w[1], team2 = sf_w[2])
  final_res  <- simulate_knockout_round(final_pair)
  champion   <- final_res$winner[1]
  
  # 8. Return output -------------------------------------------------
  if (!return_details) {
    return(champion)
  } else {
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
# Run one simulation
# 'ranks' must already be loaded into the environment
# (from 03_feature_engineering.R or separately).
# -------------------------------------------------------------------
res <- simulate_afcon_2025(
  ranks_df     = ranks,
  afcon_groups = afcon_groups,
  seed         = 123
)

# Inspect
res$champion
res$group_results

# ---------- Save result to results/ folder ----------

saveRDS(res, file = "results/afcon_2025_sim_seed123.rds")
cat("Simulation result saved to results/afcon_2025_sim_seed123.rds\n")

print("Simulating AFCON 2025: Done")














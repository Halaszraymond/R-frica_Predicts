#!/usr/bin/env Rscript
# AFCON 2025 Tournament Simulation
# This script simulates the AFCON 2025 tournament using trained models

library(tidyverse)
library(randomForest)

set.seed(123)  # For reproducibility of simulation

cat("=== AFCON 2025 Tournament Simulation ===\n\n")

# ============================================================================
# LOAD MODELS AND DATA
# ============================================================================
cat("Loading models and data...\n")

# Load trained models
logistic_model <- readRDS("models/logistic_regression_model.rds")
rf_model <- readRDS("models/random_forest_model.rds")

# Load team statistics for strength estimates
team_stats <- read_csv("data/processed/team_statistics.csv", 
                       show_col_types = FALSE)

cat("✓ Models loaded\n")

# ============================================================================
# AFCON 2025 SETUP
# ============================================================================
cat("\nSetting up AFCON 2025 tournament...\n")

# Qualified teams for AFCON 2025 (24 teams)
afcon_2025_teams <- c(
  "Egypt", "Nigeria", "Senegal", "Cameroon", "Algeria", "Morocco",
  "Ghana", "Ivory Coast", "Tunisia", "Mali", "Burkina Faso",
  "South Africa", "Guinea", "DR Congo", "Cape Verde", "Angola",
  "Zambia", "Gabon", "Kenya", "Uganda", "Tanzania", 
  "Equatorial Guinea", "Mauritania", "Namibia"
)

# Assign FIFA rankings (approximate 2024 values, lower is better)
team_fifa_ranks <- data.frame(
  team = afcon_2025_teams,
  fifa_rank = c(36, 42, 20, 51, 37, 13, 64, 39, 41, 51, 61,
                58, 77, 63, 73, 120, 87, 84, 109, 93, 117,
                138, 96, 115)
) %>%
  mutate(strength = 200 - fifa_rank)

cat(sprintf("✓ Tournament set up with %d teams\n", length(afcon_2025_teams)))

# ============================================================================
# SIMULATION FUNCTIONS
# ============================================================================

# Function to predict match outcome using Random Forest
predict_match <- function(home_team, away_team, stage, model) {
  
  # Get team information
  home_info <- team_fifa_ranks %>% filter(team == home_team)
  away_info <- team_fifa_ranks %>% filter(team == away_team)
  
  # Create feature data for prediction
  match_features <- data.frame(
    is_knockout = ifelse(stage %in% c("Group Stage"), 0, 1),
    is_final = ifelse(stage == "Final", 1, 0),
    is_semifinal = ifelse(stage == "Semi Final", 1, 0),
    home_team_fifa_rank = home_info$fifa_rank,
    away_team_fifa_rank = away_info$fifa_rank,
    fifa_rank_diff = home_info$fifa_rank - away_info$fifa_rank,
    home_strength = home_info$strength,
    away_strength = away_info$strength,
    strength_difference = home_info$strength - away_info$strength,
    match_importance = case_when(
      stage == "Final" ~ 5,
      stage == "Semi Final" ~ 4,
      stage == "Quarter Final" ~ 3,
      stage == "Round of 16" ~ 2,
      TRUE ~ 1
    ),
    tournament_year = 2025
  )
  
  # Get probability of home win
  home_win_prob <- predict(model, match_features, type = "prob")[, 2]
  
  # Add some randomness to make it more realistic
  randomness <- rnorm(1, mean = 0, sd = 0.1)
  home_win_prob <- pmax(0.1, pmin(0.9, home_win_prob + randomness))
  
  # Simulate match result
  result <- sample(c("home", "away"), 1, prob = c(home_win_prob, 1 - home_win_prob))
  
  if (result == "home") {
    winner <- home_team
    loser <- away_team
  } else {
    winner <- away_team
    loser <- home_team
  }
  
  return(list(winner = winner, loser = loser, home_win_prob = home_win_prob))
}

# ============================================================================
# GROUP STAGE SIMULATION
# ============================================================================
cat("\n=== Simulating Group Stage ===\n")

# Divide teams into 6 groups of 4 teams (AFCON 2025 format)
# Seed teams based on FIFA ranking
teams_by_rank <- team_fifa_ranks %>% arrange(fifa_rank)

# Create groups (pot system)
pot1 <- teams_by_rank$team[1:6]
pot2 <- teams_by_rank$team[7:12]
pot3 <- teams_by_rank$team[13:18]
pot4 <- teams_by_rank$team[19:24]

groups <- list(
  A = c(pot1[1], pot2[6], pot3[1], pot4[6]),
  B = c(pot1[2], pot2[5], pot3[2], pot4[5]),
  C = c(pot1[3], pot2[4], pot3[3], pot4[4]),
  D = c(pot1[4], pot2[3], pot3[4], pot4[3]),
  E = c(pot1[5], pot2[2], pot3[5], pot4[2]),
  F = c(pot1[6], pot2[1], pot3[6], pot4[1])
)

cat("\nGroup Stage Draw:\n")
for (group_name in names(groups)) {
  cat(sprintf("Group %s: %s\n", group_name, paste(groups[[group_name]], collapse = ", ")))
}

# Simulate group stage
group_results <- list()

for (group_name in names(groups)) {
  cat(sprintf("\nSimulating Group %s...\n", group_name))
  
  group_teams <- groups[[group_name]]
  standings <- data.frame(
    team = group_teams,
    points = 0,
    wins = 0,
    draws = 0,
    losses = 0,
    stringsAsFactors = FALSE
  )
  
  # Each team plays each other once (round robin)
  for (i in 1:(length(group_teams) - 1)) {
    for (j in (i + 1):length(group_teams)) {
      home_team <- group_teams[i]
      away_team <- group_teams[j]
      
      result <- predict_match(home_team, away_team, "Group Stage", rf_model)
      
      cat(sprintf("  %s vs %s: ", home_team, away_team))
      
      if (result$winner == home_team) {
        cat(sprintf("%s wins\n", home_team))
        standings$points[standings$team == home_team] <- 
          standings$points[standings$team == home_team] + 3
        standings$wins[standings$team == home_team] <- 
          standings$wins[standings$team == home_team] + 1
        standings$losses[standings$team == away_team] <- 
          standings$losses[standings$team == away_team] + 1
      } else {
        cat(sprintf("%s wins\n", away_team))
        standings$points[standings$team == away_team] <- 
          standings$points[standings$team == away_team] + 3
        standings$wins[standings$team == away_team] <- 
          standings$wins[standings$team == away_team] + 1
        standings$losses[standings$team == home_team] <- 
          standings$losses[standings$team == home_team] + 1
      }
    }
  }
  
  # Sort by points
  standings <- standings %>% arrange(desc(points), desc(wins))
  group_results[[group_name]] <- standings
  
  cat(sprintf("\nGroup %s Final Standings:\n", group_name))
  print(standings)
}

# Get qualified teams (top 2 from each group + 4 best third-placed teams)
qualified_teams <- c()
third_placed_teams <- data.frame()

for (group_name in names(group_results)) {
  standings <- group_results[[group_name]]
  # Top 2 qualify directly
  qualified_teams <- c(qualified_teams, standings$team[1:2])
  # Store third-placed team
  third_placed_teams <- rbind(third_placed_teams, 
                               standings[3, ] %>% mutate(group = group_name))
}

# Add 4 best third-placed teams
third_placed_teams <- third_placed_teams %>% 
  arrange(desc(points), desc(wins))
qualified_teams <- c(qualified_teams, third_placed_teams$team[1:4])

cat("\n=== 16 Qualified Teams for Knockout Stage ===\n")
cat(paste(qualified_teams, collapse = ", "), "\n")

# ============================================================================
# KNOCKOUT STAGE SIMULATION
# ============================================================================
cat("\n=== Simulating Knockout Stage ===\n")

# Round of 16
cat("\n--- Round of 16 ---\n")
round_of_16_winners <- c()
round_of_16_matches <- matrix(qualified_teams, ncol = 2, byrow = TRUE)

for (i in 1:8) {
  home_team <- round_of_16_matches[i, 1]
  away_team <- round_of_16_matches[i, 2]
  result <- predict_match(home_team, away_team, "Round of 16", rf_model)
  cat(sprintf("Match %d: %s vs %s -> Winner: %s\n", 
              i, home_team, away_team, result$winner))
  round_of_16_winners <- c(round_of_16_winners, result$winner)
}

# Quarter Finals
cat("\n--- Quarter Finals ---\n")
quarter_final_winners <- c()
quarter_final_matches <- matrix(round_of_16_winners, ncol = 2, byrow = TRUE)

for (i in 1:4) {
  home_team <- quarter_final_matches[i, 1]
  away_team <- quarter_final_matches[i, 2]
  result <- predict_match(home_team, away_team, "Quarter Final", rf_model)
  cat(sprintf("Match %d: %s vs %s -> Winner: %s\n", 
              i, home_team, away_team, result$winner))
  quarter_final_winners <- c(quarter_final_winners, result$winner)
}

# Semi Finals
cat("\n--- Semi Finals ---\n")
semi_final_winners <- c()
semi_final_losers <- c()
semi_final_matches <- matrix(quarter_final_winners, ncol = 2, byrow = TRUE)

for (i in 1:2) {
  home_team <- semi_final_matches[i, 1]
  away_team <- semi_final_matches[i, 2]
  result <- predict_match(home_team, away_team, "Semi Final", rf_model)
  cat(sprintf("Match %d: %s vs %s -> Winner: %s\n", 
              i, home_team, away_team, result$winner))
  semi_final_winners <- c(semi_final_winners, result$winner)
  semi_final_losers <- c(semi_final_losers, result$loser)
}

# Third Place Match
cat("\n--- Third Place Match ---\n")
third_place_result <- predict_match(semi_final_losers[1], semi_final_losers[2], 
                                     "Third Place", rf_model)
cat(sprintf("%s vs %s -> Winner: %s\n", 
            semi_final_losers[1], semi_final_losers[2], 
            third_place_result$winner))

# Final
cat("\n--- FINAL ---\n")
final_result <- predict_match(semi_final_winners[1], semi_final_winners[2], 
                               "Final", rf_model)
cat(sprintf("%s vs %s\n", semi_final_winners[1], semi_final_winners[2]))
cat(sprintf("\n🏆 AFCON 2025 WINNER: %s 🏆\n", final_result$winner))
cat(sprintf("Runner-up: %s\n", final_result$loser))
cat(sprintf("Third Place: %s\n", third_place_result$winner))

# ============================================================================
# SAVE SIMULATION RESULTS
# ============================================================================
cat("\nSaving simulation results...\n")

simulation_summary <- data.frame(
  Position = c("Champion", "Runner-up", "Third Place"),
  Team = c(final_result$winner, final_result$loser, third_place_result$winner)
)

write_csv(simulation_summary, "outputs/predictions/afcon_2025_prediction.csv")
cat("✓ Saved: outputs/predictions/afcon_2025_prediction.csv\n")

# Save all qualified teams
qualified_df <- data.frame(
  team = qualified_teams,
  qualified = TRUE
)
write_csv(qualified_df, "outputs/predictions/afcon_2025_qualified_teams.csv")
cat("✓ Saved: outputs/predictions/afcon_2025_qualified_teams.csv\n")

cat("\n=== Simulation Complete ===\n")
cat(sprintf("Predicted AFCON 2025 Winner: %s\n", final_result$winner))

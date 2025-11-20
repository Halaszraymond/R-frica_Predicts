#!/usr/bin/env Rscript
# Data Cleaning Script for AFCON Match Data
# This script loads raw match data, performs cleaning and validation,
# and creates a processed dataset ready for analysis

library(tidyverse)
library(lubridate)

cat("=== AFCON Data Cleaning Script ===\n\n")

# Load raw data
cat("Loading raw data...\n")
raw_data <- read_csv("data/raw/afcon_matches_2000_2023.csv", 
                     show_col_types = FALSE)

cat(sprintf("Loaded %d matches\n\n", nrow(raw_data)))

# Data validation and cleaning
cat("Performing data validation and cleaning...\n")

# 1. Check for missing values
missing_summary <- raw_data %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "missing_count") %>%
  filter(missing_count > 0)

if (nrow(missing_summary) > 0) {
  cat("Warning: Missing values found:\n")
  print(missing_summary)
} else {
  cat("✓ No missing values detected\n")
}

# 2. Remove any duplicate matches
initial_rows <- nrow(raw_data)
clean_data <- raw_data %>% distinct()
cat(sprintf("✓ Removed %d duplicate rows\n", initial_rows - nrow(clean_data)))

# 3. Validate data types
clean_data <- clean_data %>%
  mutate(
    match_date = as.Date(match_date),
    tournament_year = as.integer(tournament_year),
    home_goals = as.integer(home_goals),
    away_goals = as.integer(away_goals),
    attendance = as.integer(attendance),
    home_team_fifa_rank = as.integer(home_team_fifa_rank),
    away_team_fifa_rank = as.integer(away_team_fifa_rank)
  )

cat("✓ Validated and converted data types\n")

# 4. Check for logical inconsistencies
inconsistent <- clean_data %>%
  filter(home_goals < 0 | away_goals < 0 | 
         attendance < 0 |
         home_team == away_team)

if (nrow(inconsistent) > 0) {
  cat("Warning: Found", nrow(inconsistent), "inconsistent records\n")
  clean_data <- clean_data %>%
    filter(home_goals >= 0, away_goals >= 0, 
           attendance >= 0,
           home_team != away_team)
}

cat("✓ Validated logical consistency\n")

# 5. Create additional features for modeling
cat("\nCreating derived features...\n")

clean_data <- clean_data %>%
  mutate(
    # Result from home team perspective
    result_numeric = case_when(
      result == "Home Win" ~ 1,
      result == "Draw" ~ 0,
      result == "Away Win" ~ -1
    ),
    
    # Binary outcome (Home win or not)
    home_win = ifelse(result == "Home Win", 1, 0),
    away_win = ifelse(result == "Away Win", 1, 0),
    is_draw = ifelse(result == "Draw", 1, 0),
    
    # Match intensity indicators
    is_knockout = ifelse(stage != "Group Stage", 1, 0),
    is_final = ifelse(stage == "Final", 1, 0),
    is_semifinal = ifelse(stage == "Semi Final", 1, 0),
    
    # Time-based features
    month = month(match_date),
    year = year(match_date),
    
    # Team strength indicators (inverse of FIFA rank - lower rank is better)
    home_strength = 200 - home_team_fifa_rank,
    away_strength = 200 - away_team_fifa_rank,
    strength_difference = home_strength - away_strength,
    
    # Historical performance indicators (simplified)
    match_importance = case_when(
      stage == "Final" ~ 5,
      stage == "Semi Final" ~ 4,
      stage == "Quarter Final" ~ 3,
      stage == "Round of 16" ~ 2,
      TRUE ~ 1
    )
  )

cat("✓ Created", sum(names(clean_data) %in% 
    c("result_numeric", "home_win", "away_win", "is_draw", "is_knockout",
      "is_final", "is_semifinal", "month", "year", "home_strength",
      "away_strength", "strength_difference", "match_importance")), 
    "new features\n")

# 6. Add team-level aggregated statistics
cat("\nCalculating team-level statistics...\n")

# Home team statistics
home_stats <- clean_data %>%
  group_by(home_team) %>%
  summarise(
    home_matches = n(),
    home_wins = sum(home_win),
    home_goals_scored = sum(home_goals),
    home_goals_conceded = sum(away_goals),
    .groups = 'drop'
  ) %>%
  rename(team = home_team)

# Away team statistics
away_stats <- clean_data %>%
  group_by(away_team) %>%
  summarise(
    away_matches = n(),
    away_wins = sum(away_win),
    away_goals_scored = sum(away_goals),
    away_goals_conceded = sum(home_goals),
    .groups = 'drop'
  ) %>%
  rename(team = away_team)

# Combine team statistics
team_stats <- home_stats %>%
  full_join(away_stats, by = "team") %>%
  mutate(
    total_matches = home_matches + away_matches,
    total_wins = home_wins + away_wins,
    total_goals_scored = home_goals_scored + away_goals_scored,
    total_goals_conceded = home_goals_conceded + away_goals_conceded,
    win_rate = total_wins / total_matches,
    avg_goals_scored = total_goals_scored / total_matches,
    avg_goals_conceded = total_goals_conceded / total_matches,
    goal_difference = total_goals_scored - total_goals_conceded
  ) %>%
  arrange(desc(win_rate))

# Save team statistics
write_csv(team_stats, "data/processed/team_statistics.csv")
cat("✓ Saved team statistics\n")

# 7. Save cleaned data
cat("\nSaving cleaned data...\n")
output_path <- "data/processed/afcon_matches_clean.csv"
write_csv(clean_data, output_path)

cat(sprintf("✓ Cleaned data saved to: %s\n", output_path))

# Generate summary report
cat("\n=== Data Cleaning Summary ===\n")
cat(sprintf("Original rows: %d\n", initial_rows))
cat(sprintf("Final rows: %d\n", nrow(clean_data)))
cat(sprintf("Columns: %d\n", ncol(clean_data)))
cat(sprintf("Date range: %s to %s\n", 
            min(clean_data$match_date), 
            max(clean_data$match_date)))
cat(sprintf("Number of unique teams: %d\n", 
            length(unique(c(clean_data$home_team, clean_data$away_team)))))
cat(sprintf("Number of tournaments: %d\n", 
            length(unique(clean_data$tournament_year))))

# Display data quality metrics
cat("\n=== Data Quality Metrics ===\n")
cat(sprintf("Completeness: %.1f%%\n", 
            100 * (1 - sum(is.na(clean_data)) / (nrow(clean_data) * ncol(clean_data)))))
cat(sprintf("Uniqueness: %.1f%%\n", 
            100 * nrow(distinct(clean_data)) / nrow(clean_data)))

cat("\n=== Top 10 Teams by Win Rate ===\n")
print(team_stats %>% 
        select(team, total_matches, total_wins, win_rate, goal_difference) %>%
        head(10), n = 10)

cat("\nData cleaning complete!\n")

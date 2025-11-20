#!/usr/bin/env Rscript
# Generate synthetic AFCON historical match data
# This script creates a realistic dataset of AFCON matches from 2000-2023

library(tidyverse)
library(lubridate)

set.seed(42)  # For reproducibility

# African national teams that typically participate in AFCON
teams <- c(
  "Egypt", "Nigeria", "Cameroon", "Senegal", "Ghana", "Algeria",
  "Morocco", "Ivory Coast", "Tunisia", "South Africa", "Mali",
  "Burkina Faso", "Guinea", "DR Congo", "Zambia", "Kenya",
  "Ethiopia", "Angola", "Gabon", "Equatorial Guinea", "Tanzania",
  "Uganda", "Zimbabwe", "Cape Verde"
)

# Tournament years (every 2 years, some exceptions)
tournament_years <- c(2000, 2002, 2004, 2006, 2008, 2010, 2012, 
                      2013, 2015, 2017, 2019, 2021, 2023)

# Function to assign team strength (used for realistic score generation)
team_strength <- c(
  "Egypt" = 85, "Nigeria" = 83, "Cameroon" = 82, "Senegal" = 84,
  "Ghana" = 80, "Algeria" = 83, "Morocco" = 81, "Ivory Coast" = 82,
  "Tunisia" = 78, "South Africa" = 76, "Mali" = 75, "Burkina Faso" = 73,
  "Guinea" = 72, "DR Congo" = 74, "Zambia" = 71, "Kenya" = 68,
  "Ethiopia" = 67, "Angola" = 70, "Gabon" = 69, "Equatorial Guinea" = 66,
  "Tanzania" = 65, "Uganda" = 68, "Zimbabwe" = 67, "Cape Verde" = 70
)

# Generate matches
generate_matches <- function() {
  matches <- data.frame()
  match_id <- 1
  
  for (year in tournament_years) {
    # 16 teams per tournament typically
    tournament_teams <- sample(teams, 16)
    
    # Group stage: 4 groups of 4 teams, 6 matches per group = 24 matches
    num_group_matches <- 24
    
    # Knockout stage: Round of 16 (8 matches), Quarter-finals (4), Semi-finals (2), 
    # 3rd place (1), Final (1) = 16 matches
    num_knockout_matches <- 16
    
    total_matches <- num_group_matches + num_knockout_matches
    
    for (i in 1:total_matches) {
      # Determine match stage
      if (i <= num_group_matches) {
        stage <- "Group Stage"
        venue_city <- sample(c("Cairo", "Johannesburg", "Abidjan", "Accra", "Lagos"), 1)
      } else if (i <= num_group_matches + 8) {
        stage <- "Round of 16"
        venue_city <- sample(c("Cairo", "Johannesburg", "Abidjan"), 1)
      } else if (i <= num_group_matches + 12) {
        stage <- "Quarter Final"
        venue_city <- sample(c("Cairo", "Johannesburg"), 1)
      } else if (i <= num_group_matches + 14) {
        stage <- "Semi Final"
        venue_city <- "Cairo"
      } else if (i == num_group_matches + 15) {
        stage <- "Third Place"
        venue_city <- "Cairo"
      } else {
        stage <- "Final"
        venue_city <- "Cairo"
      }
      
      # Select teams
      if (stage == "Group Stage") {
        home_team <- sample(tournament_teams, 1)
        away_team <- sample(setdiff(tournament_teams, home_team), 1)
      } else {
        # Later stages have stronger teams (simplified)
        stronger_teams <- tournament_teams[1:min(8, length(tournament_teams))]
        home_team <- sample(stronger_teams, 1)
        away_team <- sample(setdiff(stronger_teams, home_team), 1)
      }
      
      # Generate scores based on team strength
      home_strength <- team_strength[home_team]
      away_strength <- team_strength[away_team]
      
      # Calculate expected goals (Poisson-like distribution)
      home_lambda <- 1.5 * (home_strength / 75) * (1 + 0.15)  # Home advantage
      away_lambda <- 1.3 * (away_strength / 75)
      
      home_goals <- rpois(1, home_lambda)
      away_goals <- rpois(1, away_lambda)
      
      # Knockout matches can't be draws (simplified - no extra time detail)
      if (stage != "Group Stage" && home_goals == away_goals) {
        if (runif(1) > 0.5) {
          home_goals <- home_goals + 1
        } else {
          away_goals <- away_goals + 1
        }
      }
      
      # Generate match date
      match_date <- as.Date(paste0(year, "-01-15")) + days(sample(0:30, 1))
      
      # Attendance
      attendance <- round(rnorm(1, 
                               mean = ifelse(stage == "Final", 60000, 
                                           ifelse(stage %in% c("Semi Final", "Third Place"), 45000, 30000)),
                               sd = 5000))
      attendance <- max(attendance, 10000)  # Minimum attendance
      
      # Create match record
      match <- data.frame(
        match_id = match_id,
        tournament_year = year,
        match_date = match_date,
        stage = stage,
        home_team = home_team,
        away_team = away_team,
        home_goals = home_goals,
        away_goals = away_goals,
        venue_city = venue_city,
        attendance = attendance,
        home_team_fifa_rank = round(rnorm(1, mean = 100 - home_strength/2, sd = 10)),
        away_team_fifa_rank = round(rnorm(1, mean = 100 - away_strength/2, sd = 10)),
        stringsAsFactors = FALSE
      )
      
      matches <- rbind(matches, match)
      match_id <- match_id + 1
    }
  }
  
  return(matches)
}

# Generate the dataset
cat("Generating synthetic AFCON match data...\n")
afcon_data <- generate_matches()

# Add derived features
afcon_data <- afcon_data %>%
  mutate(
    total_goals = home_goals + away_goals,
    goal_difference = home_goals - away_goals,
    result = case_when(
      home_goals > away_goals ~ "Home Win",
      home_goals < away_goals ~ "Away Win",
      TRUE ~ "Draw"
    ),
    high_scoring = ifelse(total_goals >= 3, "Yes", "No"),
    fifa_rank_diff = home_team_fifa_rank - away_team_fifa_rank
  )

# Save the dataset
output_path <- "data/raw/afcon_matches_2000_2023.csv"
write_csv(afcon_data, output_path)

cat(sprintf("Generated %d matches across %d tournaments\n", 
            nrow(afcon_data), length(tournament_years)))
cat(sprintf("Data saved to: %s\n", output_path))

# Display summary statistics
cat("\n=== Data Summary ===\n")
cat(sprintf("Total matches: %d\n", nrow(afcon_data)))
cat(sprintf("Home wins: %d (%.1f%%)\n", 
            sum(afcon_data$result == "Home Win"),
            100 * sum(afcon_data$result == "Home Win") / nrow(afcon_data)))
cat(sprintf("Away wins: %d (%.1f%%)\n", 
            sum(afcon_data$result == "Away Win"),
            100 * sum(afcon_data$result == "Away Win") / nrow(afcon_data)))
cat(sprintf("Draws: %d (%.1f%%)\n", 
            sum(afcon_data$result == "Draw"),
            100 * sum(afcon_data$result == "Draw") / nrow(afcon_data)))
cat(sprintf("Average goals per match: %.2f\n", mean(afcon_data$total_goals)))

# Show first few rows
cat("\n=== Sample Data ===\n")
print(head(afcon_data, 10))

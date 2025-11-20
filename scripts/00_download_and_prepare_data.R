# scripts/00_download_and_prepare_data.R
# Purpose: Download Kaggle datasets (optional) and prepare data with feature engineering
# Datasets:
#   1. African national football from 2010-2024
#   2. FIFA world ranking

# Set seed for reproducibility
set.seed(42)

# Load required libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(data.table)
  library(zoo)
})

cat("=== Step 0: Download and Prepare Data ===\n")

# Create directories if they don't exist
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

# ============================================================================
# Step 1: Download datasets from Kaggle (optional)
# ============================================================================

download_from_kaggle <- function() {
  # Check if Kaggle CLI is available
  kaggle_available <- system("which kaggle", ignore.stdout = TRUE, ignore.stderr = TRUE) == 0
  
  if (!kaggle_available) {
    cat("Kaggle CLI not found. Skipping download.\n")
    cat("To enable Kaggle downloads:\n")
    cat("  1. Install Kaggle CLI: pip install kaggle\n")
    cat("  2. Place kaggle.json in credentials/ or ~/.kaggle/\n")
    return(FALSE)
  }
  
  # Check for kaggle.json in credentials/ or environment
  kaggle_json_paths <- c(
    "credentials/kaggle.json",
    path.expand("~/.kaggle/kaggle.json")
  )
  
  kaggle_json_found <- any(file.exists(kaggle_json_paths))
  
  if (!kaggle_json_found) {
    cat("kaggle.json not found in credentials/ or ~/.kaggle/\n")
    cat("Skipping Kaggle download. Please place CSV files manually in data/raw/\n")
    return(FALSE)
  }
  
  # Set KAGGLE_CONFIG_DIR if credentials/kaggle.json exists
  if (file.exists("credentials/kaggle.json")) {
    Sys.setenv(KAGGLE_CONFIG_DIR = normalizePath("credentials"))
  }
  
  cat("Downloading datasets from Kaggle...\n")
  
  # Download African football dataset
  cat("  - Downloading African national football data...\n")
  result1 <- system(
    "kaggle datasets download -d oussamalariouch/african-national-football-from-2010-2024 -p data/raw --unzip",
    ignore.stdout = FALSE, ignore.stderr = FALSE
  )
  
  # Download FIFA ranking dataset
  cat("  - Downloading FIFA world ranking data...\n")
  result2 <- system(
    "kaggle datasets download -d cashncarry/fifaworldranking -p data/raw --unzip",
    ignore.stdout = FALSE, ignore.stderr = FALSE
  )
  
  if (result1 == 0 && result2 == 0) {
    cat("Successfully downloaded datasets from Kaggle.\n")
    return(TRUE)
  } else {
    cat("Failed to download one or more datasets.\n")
    return(FALSE)
  }
}

# Attempt to download from Kaggle
download_success <- download_from_kaggle()

# ============================================================================
# Step 2: Load datasets
# ============================================================================

cat("\n=== Loading datasets ===\n")

# Find match data file
match_files <- list.files("data/raw", pattern = "(?i)(match|result|game|african)", full.names = TRUE)
match_files <- match_files[grepl("\\.(csv|CSV)$", match_files)]

if (length(match_files) == 0) {
  stop("ERROR: No match data CSV file found in data/raw/\n",
       "Please download and place the datasets manually:\n",
       "  1. African national football: https://www.kaggle.com/datasets/oussamalariouch/african-national-football-from-2010-2024\n",
       "  2. FIFA ranking: https://www.kaggle.com/datasets/cashncarry/fifaworldranking/code\n",
       "Place CSV files in data/raw/ directory.")
}

cat("Found match data file:", match_files[1], "\n")
matches_raw <- fread(match_files[1], stringsAsFactors = FALSE)

# Find FIFA ranking file
ranking_files <- list.files("data/raw", pattern = "(?i)(fifa|rank)", full.names = TRUE)
ranking_files <- ranking_files[grepl("\\.(csv|CSV)$", ranking_files)]

if (length(ranking_files) == 0) {
  stop("ERROR: No FIFA ranking CSV file found in data/raw/\n",
       "Please download and place the FIFA ranking dataset manually.")
}

cat("Found ranking file:", ranking_files[1], "\n")
rankings_raw <- fread(ranking_files[1], stringsAsFactors = FALSE)

# ============================================================================
# Step 3: Data cleaning and standardization
# ============================================================================

cat("\n=== Cleaning data ===\n")

# Inspect column names to understand the data structure
cat("Match data columns:", paste(names(matches_raw), collapse = ", "), "\n")
cat("Ranking data columns:", paste(names(rankings_raw), collapse = ", "), "\n")

# Standardize column names (lowercase and replace spaces)
names(matches_raw) <- tolower(gsub(" ", "_", names(matches_raw)))
names(rankings_raw) <- tolower(gsub(" ", "_", names(rankings_raw)))

# Parse dates in matches (common column names: date, match_date, etc.)
date_cols <- grep("date", names(matches_raw), value = TRUE, ignore.case = TRUE)
if (length(date_cols) > 0) {
  matches_raw[[date_cols[1]]] <- ymd(matches_raw[[date_cols[1]]])
  matches_raw <- matches_raw %>% rename(match_date = !!date_cols[1])
} else {
  stop("ERROR: No date column found in match data.")
}

# Parse dates in rankings
date_cols_rank <- grep("date|rank_date", names(rankings_raw), value = TRUE, ignore.case = TRUE)
if (length(date_cols_rank) > 0) {
  rankings_raw[[date_cols_rank[1]]] <- ymd(rankings_raw[[date_cols_rank[1]]])
  rankings_raw <- rankings_raw %>% rename(rank_date = !!date_cols_rank[1])
}

# Identify team columns (home_team, away_team or similar)
home_cols <- grep("home.*team|home", names(matches_raw), value = TRUE, ignore.case = TRUE)
away_cols <- grep("away.*team|away", names(matches_raw), value = TRUE, ignore.case = TRUE)

if (length(home_cols) > 0 && length(away_cols) > 0) {
  if (!"home_team" %in% names(matches_raw)) {
    matches_raw <- matches_raw %>% rename(home_team = !!home_cols[1])
  }
  if (!"away_team" %in% names(matches_raw)) {
    matches_raw <- matches_raw %>% rename(away_team = !!away_cols[1])
  }
}

# Identify score columns
home_score_cols <- grep("home.*score|home.*goal", names(matches_raw), value = TRUE, ignore.case = TRUE)
away_score_cols <- grep("away.*score|away.*goal", names(matches_raw), value = TRUE, ignore.case = TRUE)

if (length(home_score_cols) > 0 && length(away_score_cols) > 0) {
  if (!"home_score" %in% names(matches_raw)) {
    matches_raw <- matches_raw %>% rename(home_score = !!home_score_cols[1])
  }
  if (!"away_score" %in% names(matches_raw)) {
    matches_raw <- matches_raw %>% rename(away_score = !!away_score_cols[1])
  }
}

# Convert scores to numeric
matches_raw <- matches_raw %>%
  mutate(
    home_score = as.numeric(home_score),
    away_score = as.numeric(away_score)
  )

# Remove rows with missing essential data
matches <- matches_raw %>%
  filter(!is.na(match_date), !is.na(home_team), !is.na(away_team),
         !is.na(home_score), !is.na(away_score))

cat("Cleaned matches:", nrow(matches), "rows\n")

# ============================================================================
# Step 4: Merge FIFA rankings with match data
# ============================================================================

cat("\n=== Merging FIFA rankings ===\n")

# Standardize country names in rankings
team_col_rank <- grep("country|team", names(rankings_raw), value = TRUE, ignore.case = TRUE)[1]
rank_col <- grep("^rank$|ranking|position", names(rankings_raw), value = TRUE, ignore.case = TRUE)[1]

if (!is.null(team_col_rank) && !is.null(rank_col)) {
  rankings <- rankings_raw %>%
    select(rank_date, country = !!team_col_rank, rank = !!rank_col) %>%
    mutate(
      country = trimws(country),
      rank = as.numeric(rank)
    ) %>%
    filter(!is.na(rank_date), !is.na(country), !is.na(rank))
  
  # Function to get most recent ranking before match date
  get_rank <- function(team, date, rankings_df) {
    team_ranks <- rankings_df %>%
      filter(country == team, rank_date <= date) %>%
      arrange(desc(rank_date))
    
    if (nrow(team_ranks) > 0) {
      return(team_ranks$rank[1])
    } else {
      return(NA_real_)
    }
  }
  
  # Add rankings for home and away teams
  cat("Adding FIFA rankings for home teams...\n")
  matches <- matches %>%
    rowwise() %>%
    mutate(team_rank_home = get_rank(home_team, match_date, rankings)) %>%
    ungroup()
  
  cat("Adding FIFA rankings for away teams...\n")
  matches <- matches %>%
    rowwise() %>%
    mutate(team_rank_away = get_rank(away_team, match_date, rankings)) %>%
    ungroup()
  
  cat("Rankings merged successfully.\n")
} else {
  cat("WARNING: Could not identify ranking columns. Proceeding without rankings.\n")
  matches <- matches %>%
    mutate(team_rank_home = NA_real_, team_rank_away = NA_real_)
}

# ============================================================================
# Step 5: Feature engineering
# ============================================================================

cat("\n=== Feature engineering ===\n")

# Sort by date
matches <- matches %>% arrange(match_date)

# Create result variable (target)
matches <- matches %>%
  mutate(
    result = case_when(
      home_score > away_score ~ "home",
      home_score < away_score ~ "away",
      TRUE ~ "draw"
    ),
    result = factor(result, levels = c("home", "draw", "away"))
  )

# Days since last match for each team
calc_days_since_last <- function(team, date, matches_df) {
  prev_matches <- matches_df %>%
    filter((home_team == team | away_team == team), match_date < date) %>%
    arrange(desc(match_date))
  
  if (nrow(prev_matches) > 0) {
    return(as.numeric(difftime(date, prev_matches$match_date[1], units = "days")))
  } else {
    return(NA_real_)
  }
}

cat("Calculating days since last match...\n")
matches <- matches %>%
  rowwise() %>%
  mutate(
    days_since_home = calc_days_since_last(home_team, match_date, matches),
    days_since_away = calc_days_since_last(away_team, match_date, matches)
  ) %>%
  ungroup()

# Rolling averages (last 5 matches)
calc_rolling_stats <- function(team, date, matches_df, n = 5) {
  prev_matches <- matches_df %>%
    filter((home_team == team | away_team == team), match_date < date) %>%
    arrange(desc(match_date)) %>%
    head(n)
  
  if (nrow(prev_matches) == 0) {
    return(list(goals_scored = NA_real_, goals_conceded = NA_real_, 
                wins = 0, draws = 0, losses = 0))
  }
  
  # Calculate goals scored and conceded
  team_data <- prev_matches %>%
    mutate(
      scored = ifelse(home_team == team, home_score, away_score),
      conceded = ifelse(home_team == team, away_score, home_score),
      outcome = case_when(
        (home_team == team & home_score > away_score) | (away_team == team & away_score > home_score) ~ "W",
        home_score == away_score ~ "D",
        TRUE ~ "L"
      )
    )
  
  list(
    goals_scored = mean(team_data$scored, na.rm = TRUE),
    goals_conceded = mean(team_data$conceded, na.rm = TRUE),
    wins = sum(team_data$outcome == "W"),
    draws = sum(team_data$outcome == "D"),
    losses = sum(team_data$outcome == "L")
  )
}

cat("Calculating rolling statistics (this may take a while)...\n")
matches <- matches %>%
  rowwise() %>%
  mutate(
    home_stats = list(calc_rolling_stats(home_team, match_date, matches, 5)),
    away_stats = list(calc_rolling_stats(away_team, match_date, matches, 5))
  ) %>%
  ungroup() %>%
  mutate(
    home_goals_avg = sapply(home_stats, function(x) x$goals_scored),
    home_conceded_avg = sapply(home_stats, function(x) x$goals_conceded),
    home_wins_last5 = sapply(home_stats, function(x) x$wins),
    home_draws_last5 = sapply(home_stats, function(x) x$draws),
    home_losses_last5 = sapply(home_stats, function(x) x$losses),
    away_goals_avg = sapply(away_stats, function(x) x$goals_scored),
    away_conceded_avg = sapply(away_stats, function(x) x$goals_conceded),
    away_wins_last5 = sapply(away_stats, function(x) x$wins),
    away_draws_last5 = sapply(away_stats, function(x) x$draws),
    away_losses_last5 = sapply(away_stats, function(x) x$losses)
  ) %>%
  select(-home_stats, -away_stats)

# FIFA rank difference (home - away, lower is better for home)
matches <- matches %>%
  mutate(rank_difference = team_rank_home - team_rank_away)

# Home advantage flag (always 1 for home team)
matches <- matches %>%
  mutate(home_advantage = 1)

# Match importance (check for tournament indicators in data)
tournament_cols <- grep("tournament|competition|tourn", names(matches), value = TRUE, ignore.case = TRUE)
if (length(tournament_cols) > 0) {
  cat("Found tournament column:", tournament_cols[1], "\n")
  matches <- matches %>%
    mutate(
      match_importance = case_when(
        grepl("World Cup|WC|AFCON|Africa Cup", .data[[tournament_cols[1]]], ignore.case = TRUE) ~ "high",
        grepl("Qualifier|Qualif", .data[[tournament_cols[1]]], ignore.case = TRUE) ~ "medium",
        TRUE ~ "low"
      ),
      match_importance = factor(match_importance, levels = c("low", "medium", "high"))
    )
} else {
  matches <- matches %>%
    mutate(match_importance = factor("medium", levels = c("low", "medium", "high")))
}

# Recent form score (wins*3 + draws*1)
matches <- matches %>%
  mutate(
    home_form = home_wins_last5 * 3 + home_draws_last5,
    away_form = away_wins_last5 * 3 + away_draws_last5,
    form_difference = home_form - away_form
  )

# ============================================================================
# Step 6: Save processed data
# ============================================================================

cat("\n=== Saving processed data ===\n")

# Select final feature set
features_to_keep <- c(
  "match_date", "home_team", "away_team", "home_score", "away_score",
  "team_rank_home", "team_rank_away", "rank_difference",
  "days_since_home", "days_since_away",
  "home_goals_avg", "home_conceded_avg", "away_goals_avg", "away_conceded_avg",
  "home_wins_last5", "home_draws_last5", "home_losses_last5",
  "away_wins_last5", "away_draws_last5", "away_losses_last5",
  "home_form", "away_form", "form_difference",
  "home_advantage", "match_importance",
  "result"
)

# Keep only columns that exist
features_to_keep <- features_to_keep[features_to_keep %in% names(matches)]

matches_features <- matches %>%
  select(all_of(features_to_keep)) %>%
  filter(!is.na(result))  # Remove any rows without target

# Save to CSV
output_file <- "data/processed/matches_features.csv"
fwrite(matches_features, output_file)

cat("\nProcessed data saved to:", output_file, "\n")
cat("Total matches:", nrow(matches_features), "\n")
cat("Features:", ncol(matches_features) - 1, "\n")
cat("Target distribution:\n")
print(table(matches_features$result))

cat("\n=== Data preparation complete ===\n")

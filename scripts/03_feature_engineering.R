## ==============================
## 03_feature_engineering.R
## - Load raw data
## - Join FIFA rankings
## - Engineer features
## - Save model_data for modeling.R
## ==============================

library(tidyverse)
library(lubridate)
library(sqldf)

# ---------- Load data ----------

# Match data:
#  - One row per international match
#  - Contains teams, scores, tournament name, and date
matches <- read_csv("data/afcon_country_matches_2000-2025.csv",
                    show_col_types = FALSE) |>
  mutate(date = as.Date(date))  # ensure 'date' is a proper Date object

# FIFA rankings:
#  - One row per country per ranking_date
#  - Contains 'country_full', 'rank', 'total_points', 'rank_date'
ranks <- read_csv("data/fifa_monthly_rankings.csv",
                  show_col_types = FALSE) |>
  mutate(rank_date = as.Date(rank_date))  # ensure 'rank_date' is Date

# Quick data checks --------------------------------------------------
# These help verify that the data loaded correctly and give you
# a feel for structure and distributions. They are mainly for
# exploration; they don't affect the final model_data.

# Overview of variables and types in matches
glimpse(matches)

# Basic summary statistics (for numeric columns mostly)
summary(matches)

# Top 20 tournaments by number of matches
matches |> 
  count(tournament, sort = TRUE) |> 
  head(20)

# Distribution of total goals in a match
matches |> 
  mutate(total_goals = home_score + away_score) |>
  ggplot(aes(total_goals)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Total goals per match", x = "Goals", y = "Count")

# Time trend of number of matches per year
matches |>
  mutate(year = year(date)) |>
  count(year) |>
  ggplot(aes(year, n)) +
  geom_line() +
  labs(title = "Number of matches by year")

# AFCON-related subsets (optional exploration) -----------------------

# Matches where the tournament name contains "African"
# (rough proxy for AFCON-related competitions).
matches_africa <- matches |>
  filter(str_detect(tournament, "African"))

# More specific subset: AFCON matches that look like final
# tournament games (e.g. Group, Round, Quarter, Semi, Final)
matches_afcon_finals <- matches |>
  filter(str_detect(tournament, "African") &
           str_detect(tournament, "Group|Round|Quarter|Semi|Final"))

# Count AFCON final-tournament-type matches by year
matches_afcon_finals |>
  mutate(year = year(date)) |>
  count(year, sort = TRUE)

# Goal difference distribution for AFCON finals matches
matches_afcon_finals |>
  mutate(goal_diff = home_score - away_score) |>
  ggplot(aes(goal_diff)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Goal difference (home - away) in AFCON finals matches")

# ---------- Join FIFA rankings via sqldf ----------

# sqldf works on plain data.frames, so convert from tibbles
matches_df <- as.data.frame(matches)
ranks_df   <- as.data.frame(ranks)

# For each match, we want the FIFA ranking and points of:
#  - home team on or before the match date
#  - away team on or before the match date
# We do this by:
#  - joining on country name
#  - choosing the latest rank_date <= match date (MAX(rank_date) filter)
#
# This gives us 'home_rank', 'home_points', 'away_rank', 'away_points'
matches_ranked <- sqldf("
  SELECT
    m.*,
    rh.rank         AS home_rank,
    rh.total_points AS home_points,
    ra.rank         AS away_rank,
    ra.total_points AS away_points
  FROM matches_df m
  LEFT JOIN ranks_df rh
    ON rh.country_full = m.home_team
   AND rh.rank_date = (
       SELECT MAX(rank_date)
       FROM ranks_df
       WHERE country_full = m.home_team
         AND rank_date <= m.date
   )
  LEFT JOIN ranks_df ra
    ON ra.country_full = m.away_team
   AND ra.rank_date = (
       SELECT MAX(rank_date)
       FROM ranks_df
       WHERE country_full = m.away_team
         AND rank_date <= m.date
   )
")

# Back to tibble for tidyverse convenience
matches_ranked <- as_tibble(matches_ranked)

# ---------- Feature engineering ----------

# Here we:
#  - Drop matches with missing ranking info
#  - Create the target 'result' (H/D/A)
#  - Build ranking-based features (rank_diff, points_diff)
#  - Create tournament-type flags (AFCON final, qualifier, friendly)
#  - Add temporal features (year, month)
matches_model <- matches_ranked |>
  # Keep only matches where both teams have ranking information
  filter(!is.na(home_rank), !is.na(away_rank)) |>
  mutate(
    # Match result from the home team's perspective:
    #  - "H" = home win
    #  - "D" = draw
    #  - "A" = away win
    result = case_when(
      home_score > away_score ~ "H",
      home_score == away_score ~ "D",
      TRUE ~ "A"
    ),
    # Explicitly set factor levels so the order is well-defined
    result = factor(result, levels = c("H", "D", "A")),
    
    # ----- Ranking features -----
    
    # Difference in rank: away_rank - home_rank
    # Positive value: away team is "worse" ranked (higher numeric rank)
    # Negative value: away team is "better" ranked (lower numeric rank)
    rank_diff   = away_rank - home_rank,
    
    # Difference in FIFA total_points:
    # Positive: home team has more ranking points.
    points_diff = home_points - away_points,
    
    # ----- Tournament flags -----
    #
    # These are simple string-based indicators based on the
    # 'tournament' column. You can later refine the patterns
    # if you have more precise tournament names.
    
    # AFCON final-tournament-type match:
    #  - Contains "African" (African Cup of Nations)
    #  - And a stage keyword like Group/Round/Quarter/Semi/Final
    is_afcon   = str_detect(tournament, "African") &
      str_detect(tournament, "Group|Round|Quarter|Semi|Final"),
    
    # AFCON qualifier (intended as separate from main AFCON finals)
    # This is a rough pattern based on name; adjust as needed.
    is_qual    = str_detect(tournament, "African") &
      str_detect(tournament, "Group") &
      !is_afcon,
    
    # Friendly match flag
    is_friendly = str_detect(tournament, "Friendly"),
    
    # ----- Time features -----
    year  = year(date),
    month = month(date)
  )

# This is the dataset we will use for modeling.
# Only keep the variables that are needed for the model:
#  - result: target
#  - rank_diff, points_diff: strength differences
#  - is_afcon, is_qual, is_friendly: match type
#  - year: time (used for train/test split)
model_data <- matches_model |>
  dplyr::select(
    result,
    rank_diff,
    points_diff,
    is_afcon,
    is_qual,
    is_friendly,
    year
  )

# ---------- Save engineered data for modeling ----------

# Save as RDS so:
#  - factors/classes are preserved
#  - it's easy to load in modeling.R via readRDS()
saveRDS(model_data, "data/model_data.rds")

# Optional: keep 'ranks' in memory for later scripts (like simulation.R),
# where we need the FIFA rankings by country and date.
# (If all scripts are run in the same R session, this is handy.)
ranks <- ranks  # no-op, just to emphasize 'ranks' is meant to stay available

# Quick look to confirm the engineered data looks reasonable
head(model_data)

print("Feature Engineering: Done")

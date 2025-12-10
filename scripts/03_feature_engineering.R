## ==============================
## 03_feature_engineering.R
## - Load raw data
## - Join FIFA rankings
## - Engineer features (incl. head-to-head)
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

# ---------- Quick data checks (exploratory, optional) ----------

glimpse(matches)
summary(matches)

matches |>
  count(tournament, sort = TRUE) |>
  head(20)

matches |>
  mutate(total_goals = home_score + away_score) |>
  ggplot(aes(total_goals)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Total goals per match", x = "Goals", y = "Count")

matches |>
  mutate(year = year(date)) |>
  count(year) |>
  ggplot(aes(year, n)) +
  geom_line() +
  labs(title = "Number of matches by year")

# AFCON-related subsets (just for exploration)
matches_africa <- matches |>
  filter(str_detect(tournament, "African"))

matches_afcon_finals <- matches |>
  filter(str_detect(tournament, "African") &
           str_detect(tournament, "Group|Round|Quarter|Semi|Final"))

matches_afcon_finals |>
  mutate(year = year(date)) |>
  count(year, sort = TRUE)

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

# ---------- Head-to-head features (per pair, before each match) ----------

# We compute cumulative head-to-head history for each unordered pair of teams.
# Then we lag those cumulative stats so that for each match, the features
# only reflect matches BEFORE that match date (no data leakage).

matches_h2h <- matches_ranked |>
  mutate(
    # Unordered pair id for the two teams (same for A vs B and B vs A)
    team_low  = pmin(home_team, away_team),
    team_high = pmax(home_team, away_team),
    pair_id   = paste(team_low, team_high, sep = " - "),
    
    # From the perspective of team_low (so we have a consistent side)
    low_goal_diff = case_when(
      home_team == team_low ~ home_score - away_score,       # low is home
      TRUE                  ~ away_score - home_score        # low is away
    ),
    low_result_num = case_when(
      home_score == away_score ~ 0,                          # draw
      # low team wins
      (home_team == team_low & home_score > away_score) |
        (away_team == team_low & away_score > home_score) ~ 1,
      TRUE ~ -1                                              # low team loses
    )
  ) |>
  arrange(pair_id, date) |>
  group_by(pair_id) |>
  mutate(
    # cumulative stats for team_low up to and including each match
    c_matches_low    = row_number(),
    c_goal_diff_low  = cumsum(low_goal_diff),
    c_result_sum_low = cumsum(low_result_num),
    
    # shift by one match so features are based only on history
    h2h_matches_before_low    = lag(c_matches_low,    default = 0L),
    h2h_goal_diff_before_low  = lag(c_goal_diff_low,  default = 0),
    h2h_result_sum_before_low = lag(c_result_sum_low, default = 0),
    
    # transform to HOME-team perspective
    h2h_matches_before_home = h2h_matches_before_low,
    h2h_goal_diff_before_home = if_else(
      home_team == team_low,
      h2h_goal_diff_before_low,     # low is home → same sign
      -h2h_goal_diff_before_low     # low is away → flip sign
    ),
    h2h_result_sum_before_home = if_else(
      home_team == team_low,
      h2h_result_sum_before_low,
      -h2h_result_sum_before_low
    ),
    
    # Approximate home head-to-head win rate:
    # result_sum = wins - losses (draws = 0).
    # (wins - losses + matches) / (2 * matches) is between 0 and 1.
    h2h_home_winrate_before = if_else(
      h2h_matches_before_home > 0,
      (h2h_result_sum_before_home + h2h_matches_before_home) /
        (2 * h2h_matches_before_home),
      NA_real_
    )
  ) |>
  ungroup()

# Optional: replace NA head-to-head winrate (no prior meetings) with neutral 0.5
matches_h2h <- matches_h2h |>
  mutate(
    h2h_home_winrate_before = if_else(
      is.na(h2h_home_winrate_before),
      0.5,  # neutral: no advantage from H2H history
      h2h_home_winrate_before
    )
  )

# ---------- Feature engineering (final match-level dataset) ----------

matches_model <- matches_h2h |>
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
    is_afcon = str_detect(tournament, "African") &
      str_detect(tournament, "Group|Round|Quarter|Semi|Final"),
    
    # AFCON qualifiers (rough pattern; can be refined)
    is_qual = str_detect(tournament, "African") &
      str_detect(tournament, "Group") &
      !is_afcon,
    
    # Friendly match flag
    is_friendly = str_detect(tournament, "Friendly"),
    
    # ----- Time features -----
    year  = year(date),
    month = month(date)
  )

# ---------- Final model_data selection ----------

# This is the dataset we will use for modeling.
# One row per match with:
#  - result: outcome (H/D/A)
#  - rank_diff, points_diff: ranking-based strength differences
#  - is_afcon, is_qual, is_friendly: match type
#  - year: time (used for train/test split)
#  - h2h_*: head-to-head stats from home team perspective
model_data <- matches_model |>
  dplyr::select(
    result,
    rank_diff,
    points_diff,
    is_afcon,
    is_qual,
    is_friendly,
    year,
    h2h_matches_before_home,
    h2h_goal_diff_before_home,
    h2h_home_winrate_before
  )

# ---------- Save engineered data for modeling ----------

saveRDS(model_data, "data/model_data.rds")

# Optional: keep 'ranks' in memory for later scripts (like simulation.R),
# where we need the FIFA rankings by country and date.
ranks <- ranks  # no-op, just to emphasize 'ranks' is meant to stay available

# Quick look to confirm the engineered data looks reasonable
print(head(model_data))

print("Feature Engineering: Done")


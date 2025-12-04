## ==============================
## feature_engineer.R
## - Load raw data
## - Join FIFA rankings
## - Engineer features
## - Save model_data for modeling.R
## ==============================

library(tidyverse)
library(lubridate)
library(sqldf)

# ---------- Load data ----------

matches <- read_csv("data/afcon_country_matches_2000-2025.csv",
                    show_col_types = FALSE) |>
  mutate(date = as.Date(date))

ranks <- read_csv("data/fifa_monthly_rankings.csv",
                  show_col_types = FALSE) |>
  mutate(rank_date = as.Date(rank_date))

# Quick data checks
glimpse(matches)
summary(matches)

matches |> 
  count(tournament, sort = TRUE) |> 
  head(20)

# Distribution of goals
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

# AFCON subsets (optional exploration)
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

matches_df <- as.data.frame(matches)
ranks_df   <- as.data.frame(ranks)

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

matches_ranked <- as_tibble(matches_ranked)

# ---------- Feature engineering ----------

matches_model <- matches_ranked |>
  filter(!is.na(home_rank), !is.na(away_rank)) |>
  mutate(
    result = case_when(
      home_score > away_score ~ "H",
      home_score == away_score ~ "D",
      TRUE ~ "A"
    ),
    result = factor(result, levels = c("H", "D", "A")),
    
    # ranking features
    rank_diff   = away_rank - home_rank,
    points_diff = home_points - away_points,
    
    # AFCON / qualifier flags
    is_afcon   = str_detect(tournament, "African") &
      str_detect(tournament, "Group|Round|Quarter|Semi|Final"),
    is_qual    = str_detect(tournament, "African") &
      str_detect(tournament, "Group") &
      !is_afcon,
    is_friendly = str_detect(tournament, "Friendly"),
    
    # time features
    year  = year(date),
    month = month(date)
  )

# This is the dataset we will use for modeling
model_data <- matches_model |>
  select(result, rank_diff, points_diff, is_afcon, is_qual, is_friendly, year)

# ---------- Save engineered data for modeling ----------

# RDS is nice because it preserves types/factors
saveRDS(model_data, "data/model_data.rds")

# Optional: keep ranks in memory for later scripts (simulation)
# (If you're running everything in one R session, this is handy)
ranks <- ranks  # just to remind ourselves it's here


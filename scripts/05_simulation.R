# Install if needed:

library(tidyverse)
library(lubridate)
library(sqldf)
library(nnet)         # multinomial logistic
library(klaR)         # Naive Bayes
library(randomForest) # Random Forest

matches <- read_csv("data/afcon_country_matches_2000-2025.csv",
                    show_col_types = FALSE) |>
  mutate(date = as.Date(date))

ranks <- read_csv("data/fifa_monthly_rankings.csv",
                  show_col_types = FALSE) |>
  mutate(rank_date = as.Date(rank_date))

glimpse(matches)
summary(matches)

matches |> 
  count(tournament, sort = TRUE) |> 
  head(20)

# distribution of goals
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

matches_africa <- matches |>
  filter(str_detect(tournament, "African")) # qualifiers + finals

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

# For sqldf we need plain data.frames (no tibbles)
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

# Back to tibble for convenience
matches_ranked <- as_tibble(matches_ranked)

matches_model <- matches_ranked |>
  filter(!is.na(home_rank), !is.na(away_rank))

# Check missing ranks
matches_model <- matches_model |>
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
    
    # some time features
    year  = year(date),
    month = month(date)
  )

matches_model |> count(result)

model_data <- matches_model |>
  dplyr::select(result, rank_diff, points_diff, is_afcon, is_qual, is_friendly, year)


train_data <- model_data |> filter(year <= 2005)
test_data  <- model_data |> filter(year >= 2022)

train_result <- train_data$result
test_result  <- test_data$result

train_data_x <- train_data |> dplyr::select(-result)
test_data_x  <- test_data  |> dplyr::select(-result)

set.seed(123)
multinom_fit <- multinom(result ~ ., data = train_data)

# Predict class and probabilities
pred_multinom_class <- predict(multinom_fit, newdata = test_data)
pred_multinom_prob  <- predict(multinom_fit, newdata = test_data, type = "prob")

# Accuracy
mean(pred_multinom_class == test_result)

# Confusion matrix
table(Predicted = pred_multinom_class, Actual = test_result)

set.seed(123)
rf_fit <- randomForest(
  result ~ .,
  data = train_data,
  ntree = 500,
  mtry  = 3,
  importance = TRUE
)

pred_rf_class <- predict(rf_fit, newdata = test_data, type = "response")
pred_rf_prob  <- predict(rf_fit, newdata = test_data, type = "prob")

mean(pred_rf_class == test_result)
table(Predicted = pred_rf_class, Actual = test_result)

importance(rf_fit)
varImpPlot(rf_fit)

simulate_afcon_2025 <- function(model,
                                ranks_df,
                                afcon_groups = NULL,
                                year_for_model = 2025,
                                seed = NULL,
                                return_details = TRUE) {
  if (!is.null(seed)) set.seed(seed)
  
  # 1. Default groups if none given
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
  
  # Sanity check: all groups have ≥ 2 teams
  group_sizes <- vapply(afcon_groups, length, integer(1))
  if (any(group_sizes < 2)) {
    bad <- names(afcon_groups)[group_sizes < 2]
    stop("These groups have fewer than 2 teams: ",
         paste(bad, collapse = ", "),
         "\nCheck 'afcon_groups'.")
  }
  
  # 2. Latest FIFA ranking snapshot
  ranks_df$rank_date <- as.Date(ranks_df$rank_date)
  latest_rank_date <- max(ranks_df$rank_date, na.rm = TRUE)
  
  ranks_latest <- ranks_df |>
    dplyr::filter(rank_date == latest_rank_date) |>
    dplyr::select(country_full, rank, total_points)
  
  # 3. Helper functions
  
  get_team_rank <- function(team_name) {
    # Map AFCON name to FIFA ranking name if needed
    canonical_name <- if (team_name %in% names(name_map)) {
      name_map[[team_name]]
    } else {
      team_name
    }
    
    rec <- ranks_latest |> dplyr::filter(country_full == canonical_name)
    if (nrow(rec) == 0) {
      stop(
        "Missing ranking for team: ", team_name,
        " (mapped as '", canonical_name, "')",
        "\nCheck spelling vs 'country_full' in ranks_df or extend name_map."
      )
    }
    c(rank = rec$rank[1], total_points = rec$total_points[1])
  }
  
  
  build_match_features <- function(team1, team2) {
    home <- get_team_rank(team1)
    away <- get_team_rank(team2)
    
    tibble::tibble(
      rank_diff   = away["rank"] - home["rank"],
      points_diff = home["total_points"] - away["total_points"],
      is_afcon    = TRUE,
      is_qual     = FALSE,
      is_friendly = FALSE,
      year        = year_for_model
    )
  }
  
  predict_match_prob <- function(team1, team2) {
    feats <- build_match_features(team1, team2)
    probs <- predict(model, newdata = feats, type = "prob")[1, ]
    probs
  }
  
  simulate_match <- function(team1, team2) {
    probs <- predict_match_prob(team1, team2)
    outcome <- sample(c("H", "D", "A"), size = 1, prob = as.numeric(probs))
    
    if (outcome == "H") {
      tibble::tibble(team = c(team1, team2),
                     points = c(3, 0),
                     gd     = c(1, -1))
    } else if (outcome == "A") {
      tibble::tibble(team = c(team1, team2),
                     points = c(0, 3),
                     gd     = c(-1, 1))
    } else {
      tibble::tibble(team = c(team1, team2),
                     points = c(1, 1),
                     gd     = c(0, 0))
    }
  }
  
  group_fixtures <- function(teams) {
    # extra check here to avoid combn() n<m
    if (length(teams) < 2) {
      stop("group_fixtures called with < 2 teams: ",
           paste(teams, collapse = ", "))
    }
    combn(teams, 2) |>
      t() |>
      tibble::as_tibble(.name_repair = "minimal") |>
      rlang::set_names(c("team1", "team2"))
  }
  
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
      tibble::as_tibble()
    
    standings <- results |>
      dplyr::group_by(team) |>
      dplyr::summarise(points = sum(points),
                       gd     = sum(gd),
                       .groups = "drop") |>
      dplyr::arrange(dplyr::desc(points), dplyr::desc(gd)) |>
      dplyr::mutate(group = group_name,
                    position = dplyr::row_number())
    
    standings
  }
  
  simulate_knockout_round <- function(pairs_df) {
    winners <- sapply(seq_len(nrow(pairs_df)), function(i) {
      t1 <- pairs_df$team1[i]
      t2 <- pairs_df$team2[i]
      probs <- predict_match_prob(t1, t2)
      
      res <- sample(c("H", "D", "A"), size = 1, prob = as.numeric(probs))
      if (res == "H") return(t1)
      if (res == "A") return(t2)
      
      pH <- probs["H"]; pA <- probs["A"]
      sample(c(t1, t2), size = 1, prob = c(pH, pA))
    })
    
    tibble::tibble(winner = winners)
  }
  
  # 4. Group stage – use base lapply instead of imap_dfr
  group_names <- names(afcon_groups)
  group_list <- lapply(seq_along(afcon_groups), function(i) {
    gname <- group_names[i]
    teams <- afcon_groups[[i]]
    simulate_group(gname, teams)
  })
  group_results <- dplyr::bind_rows(group_list)
  
  # 5. Qualifiers
  auto_qualified <- group_results |> dplyr::filter(position <= 2)
  third_placed   <- group_results |>
    dplyr::filter(position == 3) |>
    dplyr::arrange(dplyr::desc(points), dplyr::desc(gd))
  best_thirds    <- third_placed |> dplyr::slice(1:4)
  
  knockout_teams <- dplyr::bind_rows(auto_qualified, best_thirds)
  
  # 6. Seeding + R16
  seeded <- knockout_teams |>
    dplyr::arrange(dplyr::desc(points), dplyr::desc(gd)) |>
    dplyr::mutate(seed = dplyr::row_number())
  
  round_of_16_pairs <- tibble::tibble(
    team1_seed = c(1, 2, 3, 4, 5, 6, 7, 8),
    team2_seed = c(16, 15, 14, 13, 12, 11, 10, 9)
  ) |>
    dplyr::left_join(seeded |> dplyr::select(seed, team1 = team),
                     by = c("team1_seed" = "seed")) |>
    dplyr::left_join(seeded |> dplyr::select(seed, team2 = team),
                     by = c("team2_seed" = "seed"))
  
  # 7. Knockouts
  r16 <- simulate_knockout_round(round_of_16_pairs)
  r16_winners <- r16$winner
  
  qf_pairs <- tibble::tibble(
    team1 = r16_winners[c(1, 3, 5, 7)],
    team2 = r16_winners[c(2, 4, 6, 8)]
  )
  qf <- simulate_knockout_round(qf_pairs)
  qf_winners <- qf$winner
  
  sf_pairs <- tibble::tibble(
    team1 = qf_winners[c(1, 3)],
    team2 = qf_winners[c(2, 4)]
  )
  sf <- simulate_knockout_round(sf_pairs)
  sf_winners <- sf$winner
  
  final_pair <- tibble::tibble(team1 = sf_winners[1], team2 = sf_winners[2])
  final_res  <- simulate_knockout_round(final_pair)
  champion   <- final_res$winner[1]
  
  if (!return_details) {
    return(champion)
  } else {
    return(list(
      champion          = champion,
      final_pair        = final_pair,
      semi_finalists    = sf_winners,
      quarter_finalists = qf_winners,
      r16_winners       = r16_winners,
      group_results     = group_results,
      knockout_seeding  = seeded,
      round_of_16_pairs = round_of_16_pairs
    ))
  }
}


afcon_groups <- list(
  A = c("Morocco", "Mali", "Zambia", "Comoros"),
  B = c("Egypt", "South Africa", "Angola", "Zimbabwe"),
  C = c("Nigeria", "Tunisia", "Uganda", "Tanzania"),
  D = c("Senegal", "DR Congo", "Benin", "Botswana"),
  E = c("Algeria", "Burkina Faso", "Equatorial Guinea", "Sudan"),
  F = c("Ivory Coast", "Cameroon", "Gabon", "Mozambique")
)
sapply(afcon_groups, length)
# should give: A B C D E F 
#              4 4 4 4 4 4
# rf_fit is your trained Random Forest
# ranks is the full fifa_monthly_rankings data.frame

name_map <- c(
  "DR Congo"    = "Congo DR",
  "Ivory Coast" = "Côte d'Ivoire"
)


res <- simulate_afcon_2025(
  model        = rf_fit,
  ranks_df     = ranks,
  afcon_groups = afcon_groups,
  seed         = 123
)

res$champion
res$group_results


## ============================================================
## AFCON 2025 – BRACKET + GROUP TABLES
## Requires: res (output of simulate_afcon_2025)
## ============================================================

library(dplyr)
library(ggplot2)
library(cowplot)
library(grid)

## 1. Extract data from res -----------------------------------

r16_pairs <- res$round_of_16_pairs
r16_w     <- res$r16_winners
qf_w      <- res$quarter_finalists
sf_w      <- res$semi_finalists
final_p   <- res$final_pair
champion  <- res$champion
group_df  <- res$group_results %>%
  mutate(label = sprintf("%s (%d pts, GD %d)", team, points, gd))

## 2. Coordinates for the bracket -----------------------------

r16_y   <- c(17, 13,  9,  5,  5,  9, 13, 17)
qf_y    <- c(15,  7,  7, 15)
sf_y    <- c(13,  9)
final_y <- 11

r16_df <- r16_pairs %>%
  mutate(
    round  = "R16",
    match  = row_number(),
    winner = r16_w[match],
    x      = if_else(match <= 4, 1, 5),
    y      = r16_y
  )

qf_df <- tibble::tibble(
  team1 = r16_w[c(1, 3, 5, 7)],
  team2 = r16_w[c(2, 4, 6, 8)]
) %>%
  mutate(
    round  = "QF",
    match  = row_number(),
    winner = qf_w[match],
    x      = if_else(match <= 2, 2, 4),
    y      = qf_y
  )

sf_df <- tibble::tibble(
  team1 = qf_w[c(1, 3)],
  team2 = qf_w[c(2, 4)]
) %>%
  mutate(
    round  = "SF",
    match  = row_number(),
    winner = sf_w[match],
    x      = 3,
    y      = sf_y
  )

final_df <- final_p %>%
  mutate(
    round  = "Final",
    match  = 1L,
    winner = champion,
    x      = 3,
    y      = final_y
  )

bracket_df <- bind_rows(r16_df, qf_df, sf_df, final_df) %>%
  mutate(
    label = if_else(
      round != "Final",
      sprintf("%s\nvs\n%s\nWinner: %s", team1, team2, winner),
      sprintf("%s\nvs\n%s\nCHAMPION: %s", team1, team2, winner)
    )
  )

## 3. Bracket plot -------------------------------------------

p_bracket <- ggplot(bracket_df, aes(x = x, y = y)) +
  geom_rect(
    aes(
      xmin = x - 0.6, xmax = x + 0.6,
      ymin = y - 1.0, ymax = y + 1.0,
      fill = round
    ),
    colour    = "white",
    linewidth = 0.6
  ) +
  geom_text(
    aes(label = label),
    colour     = "white",
    size       = 3,
    lineheight = 1.1
  ) +
  scale_fill_manual(values = c(
    "R16"   = "#1E88E5",
    "QF"    = "#43A047",
    "SF"    = "#FB8C00",
    "Final" = "#8E24AA"
  )) +
  coord_cartesian(
    xlim   = c(0.5, 5.5),
    ylim   = c(3, 19),
    expand = FALSE
  ) +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "#0D47A1", colour = NA),
    panel.background = element_rect(fill = "#0D47A1", colour = NA),
    legend.position  = "none",
    plot.margin      = unit(c(5, 5, 5, 5), "pt")
  ) +
  annotate(
    "text",
    x      = 3,
    y      = 18.2,
    label  = "AFCON 2025 – Simulated Bracket",
    colour = "white",
    fontface = "bold",
    size   = 6
  )

## 4. Group table function (left & right) --------------------

make_group_table <- function(groups_vec) {
  group_df %>%
    filter(group %in% groups_vec) %>%
    ggplot(aes(x = 0.5, y = -position)) +
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
    geom_text(
      aes(label = label),
      hjust    = 0,
      nudge_x  = -0.35,
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

p_left  <- make_group_table(c("A", "B", "C"))
p_right <- make_group_table(c("D", "E", "F"))

## 5. Combine plots ------------------------------------------

final_plot <- plot_grid(
  p_left, p_bracket, p_right,
  nrow       = 1,
  rel_widths = c(0.65, 1.7, 0.65),
  align      = "h"
)

final_plot












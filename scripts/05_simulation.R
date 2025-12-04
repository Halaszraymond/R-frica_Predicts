## ==============================
## simulation.R
## - simulate_afcon_2025()
## - define groups + name_map
## - run one simulation -> res
## ==============================

# Uses:
# - rf_fit  (from modeling.R)
# - ranks   (from feature_engineer.R)

library(dplyr)
library(lubridate)
library(tibble)
library(stringr)

# Name map for FIFA rankings
name_map <- c(
  "DR Congo"    = "Congo DR",
  "Ivory Coast" = "Côte d'Ivoire"
)

simulate_afcon_2025 <- function(model,
                                ranks_df,
                                afcon_groups = NULL,
                                year_for_model = 2025,
                                seed = NULL,
                                return_details = TRUE) {
  if (!is.null(seed)) set.seed(seed)
  
  # 1. Default groups
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
  
  # Group size sanity check
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
    filter(rank_date == latest_rank_date) |>
    select(country_full, rank, total_points)
  
  # --- Helper functions ---
  
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
  
  predict_match_prob <- function(team1, team2) {
    feats <- build_match_features(team1, team2)
    probs <- predict(model, newdata = feats, type = "prob")[1, ]
    probs
  }
  
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
    
    tibble(winner = winners)
  }
  
  # 4. Group stage
  group_names <- names(afcon_groups)
  group_list <- lapply(seq_along(afcon_groups), function(i) {
    gname <- group_names[i]
    teams <- afcon_groups[[i]]
    simulate_group(gname, teams)
  })
  group_results <- bind_rows(group_list)
  
  # 5. Qualifiers
  auto_qualified <- group_results |> filter(position <= 2)
  third_placed   <- group_results |>
    filter(position == 3) |>
    arrange(desc(points), desc(gd))
  best_thirds    <- third_placed |> slice(1:4)
  
  knockout_teams <- bind_rows(auto_qualified, best_thirds)
  
  # 6. Seeding + R16
  seeded <- knockout_teams |>
    arrange(desc(points), desc(gd)) |>
    mutate(seed = row_number())
  
  round_of_16_pairs <- tibble(
    team1_seed = c(1, 2, 3, 4, 5, 6, 7, 8),
    team2_seed = c(16, 15, 14, 13, 12, 11, 10, 9)
  ) |>
    left_join(seeded |> select(seed, team1 = team),
              by = c("team1_seed" = "seed")) |>
    left_join(seeded |> select(seed, team2 = team),
              by = c("team2_seed" = "seed"))
  
  # 7. Knockout rounds
  r16        <- simulate_knockout_round(round_of_16_pairs)
  r16_w      <- r16$winner
  
  qf_pairs <- tibble(
    team1 = r16_w[c(1, 3, 5, 7)],
    team2 = r16_w[c(2, 4, 6, 8)]
  )
  qf        <- simulate_knockout_round(qf_pairs)
  qf_w      <- qf$winner
  
  sf_pairs <- tibble(
    team1 = qf_w[c(1, 3)],
    team2 = qf_w[c(2, 4)]
  )
  sf        <- simulate_knockout_round(sf_pairs)
  sf_w      <- sf$winner
  
  final_pair <- tibble(team1 = sf_w[1], team2 = sf_w[2])
  final_res  <- simulate_knockout_round(final_pair)
  champion   <- final_res$winner[1]
  
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

# Explicit groups (same as default, but clear)
afcon_groups <- list(
  A = c("Morocco", "Mali", "Zambia", "Comoros"),
  B = c("Egypt", "South Africa", "Angola", "Zimbabwe"),
  C = c("Nigeria", "Tunisia", "Uganda", "Tanzania"),
  D = c("Senegal", "DR Congo", "Benin", "Botswana"),
  E = c("Algeria", "Burkina Faso", "Equatorial Guinea", "Sudan"),
  F = c("Ivory Coast", "Cameroon", "Gabon", "Mozambique")
)

sapply(afcon_groups, length)

# Run one simulation
res <- simulate_afcon_2025(
  model        = rf_fit,
  ranks_df     = ranks,
  afcon_groups = afcon_groups,
  seed         = 123
)

res$champion
res$group_results












simulate_afcon_2025 <- function(model,
                                ranks_df,
                                groups = NULL,
                                year_for_model = 2025,
                                seed = NULL,
                                return_details = TRUE) {
  if (!is.null(seed)) set.seed(seed)
  
  # --------------------------
  # 1. Default groups (if not supplied)
  # --------------------------
  if (is.null(groups)) {
    groups <- list(
      A = c("Morocco", "Mali", "Zambia", "Comoros"),
      B = c("Egypt", "South Africa", "Angola", "Zimbabwe"),
      C = c("Nigeria", "Tunisia", "Uganda", "Tanzania"),
      D = c("Senegal", "DR Congo", "Benin", "Botswana"),
      E = c("Algeria", "Burkina Faso", "Equatorial Guinea", "Sudan"),
      F = c("Ivory Coast", "Cameroon", "Gabon", "Mozambique")
    )
  }
  
  # --------------------------
  # 2. Latest FIFA ranking snapshot
  # --------------------------
  ranks_df$rank_date <- as.Date(ranks_df$rank_date)
  latest_rank_date <- max(ranks_df$rank_date, na.rm = TRUE)
  
  ranks_latest <- ranks_df |>
    dplyr::filter(rank_date == latest_rank_date) |>
    dplyr::select(country_full, rank, total_points)
  
  # --------------------------
  # 3. Helper functions (nested)
  # --------------------------
  
  # 3.1 Get rank info for a team
  get_team_rank <- function(team_name) {
    rec <- ranks_latest |> dplyr::filter(country_full == team_name)
    if (nrow(rec) == 0) {
      stop(paste("Missing ranking for team:", team_name))
    }
    c(rank = rec$rank[1], total_points = rec$total_points[1])
  }
  
  # 3.2 Build feature row for model
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
  
  # 3.3 Predict probabilities with chosen model (Random Forest / multinom / NB)
  predict_match_prob <- function(team1, team2) {
    feats <- build_match_features(team1, team2)
    probs <- predict(model, newdata = feats, type = "prob")[1, ]
    # Ensure named vector c(H=..., D=..., A=...)
    return(probs)
  }
  
  # 3.4 Simulate a single match (group stage)
  simulate_match <- function(team1, team2) {
    probs <- predict_match_prob(team1, team2)
    outcome <- sample(c("H", "D", "A"), size = 1, prob = as.numeric(probs))
    
    if (outcome == "H") {
      tibble::tibble(
        team   = c(team1, team2),
        points = c(3, 0),
        gd     = c(1, -1)
      )
    } else if (outcome == "A") {
      tibble::tibble(
        team   = c(team1, team2),
        points = c(0, 3),
        gd     = c(-1, 1)
      )
    } else {
      tibble::tibble(
        team   = c(team1, team2),
        points = c(1, 1),
        gd     = c(0, 0)
      )
    }
  }
  
  # 3.5 Generate all pairings in a group (round robin)
  group_fixtures <- function(teams) {
    combn(teams, 2) |>
      t() |>
      tibble::as_tibble(.name_repair = "minimal") |>
      rlang::set_names(c("team1", "team2"))
  }
  
  # 3.6 Simulate a full group
  simulate_group <- function(group_name, teams) {
    fixtures <- group_fixtures(teams)
    
    results <- purrr::map_dfr(seq_len(nrow(fixtures)), function(i) {
      t1 <- fixtures$team1[i]
      t2 <- fixtures$team2[i]
      simulate_match(t1, t2)
    })
    
    standings <- results |>
      dplyr::group_by(team) |>
      dplyr::summarise(
        points = sum(points),
        gd     = sum(gd),
        .groups = "drop"
      ) |>
      dplyr::arrange(dplyr::desc(points), dplyr::desc(gd)) |>
      dplyr::mutate(
        group    = group_name,
        position = dplyr::row_number()
      )
    
    standings
  }
  
  # 3.7 Simulate a knockout round (must produce a winner)
  simulate_knockout_round <- function(pairs_df) {
    winners <- purrr::map_chr(seq_len(nrow(pairs_df)), function(i) {
      t1 <- pairs_df$team1[i]
      t2 <- pairs_df$team2[i]
      probs <- predict_match_prob(t1, t2)
      
      res <- sample(c("H", "D", "A"), size = 1, prob = as.numeric(probs))
      if (res == "H") return(t1)
      if (res == "A") return(t2)
      
      # If draw: penalties, weighted by H vs A probability
      pH <- probs["H"]; pA <- probs["A"]
      winner <- sample(c(t1, t2), size = 1, prob = c(pH, pA))
      winner
    })
    
    tibble::tibble(winner = winners)
  }
  
  # --------------------------
  # 4. GROUP STAGE
  # --------------------------
  group_results <- purrr::imap_dfr(groups, simulate_group)
  
  # Top 2 from each group
  auto_qualified <- group_results |>
    dplyr::filter(position <= 2)
  
  # Third-placed teams
  third_placed <- group_results |>
    dplyr::filter(position == 3) |>
    dplyr::arrange(dplyr::desc(points), dplyr::desc(gd))
  
  best_thirds <- third_placed |> dplyr::slice(1:4)
  
  knockout_teams <- dplyr::bind_rows(auto_qualified, best_thirds)
  
  # --------------------------
  # 5. SEEDING & ROUND OF 16
  # --------------------------
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
  
  # --------------------------
  # 6. KNOCKOUTS
  # --------------------------
  # R16
  r16 <- simulate_knockout_round(round_of_16_pairs)
  r16_winners <- r16$winner
  
  # QF: (1 vs 2), (3 vs 4), (5 vs 6), (7 vs 8)
  qf_pairs <- tibble::tibble(
    team1 = r16_winners[c(1, 3, 5, 7)],
    team2 = r16_winners[c(2, 4, 6, 8)]
  )
  qf <- simulate_knockout_round(qf_pairs)
  qf_winners <- qf$winner
  
  # SF: (1 vs 2), (3 vs 4)
  sf_pairs <- tibble::tibble(
    team1 = qf_winners[c(1, 3)],
    team2 = qf_winners[c(2, 4)]
  )
  sf <- simulate_knockout_round(sf_pairs)
  sf_winners <- sf$winner
  
  # Final
  final_pair <- tibble::tibble(team1 = sf_winners[1], team2 = sf_winners[2])
  final_res  <- simulate_knockout_round(final_pair)
  champion   <- final_res$winner[1]
  
  # --------------------------
  # 7. Return
  # --------------------------
  if (!return_details) {
    return(champion)
  } else {
    return(list(
      champion         = champion,
      final_pair       = final_pair,
      semi_finalists   = sf_winners,
      quarter_finalists = qf_winners,
      r16_winners      = r16_winners,
      group_results    = group_results,
      knockout_seeding = seeded,
      round_of_16_pairs = round_of_16_pairs
    ))
  }
}

#!/usr/bin/env Rscript
# Exploratory Data Analysis (EDA) for AFCON Match Data
# This script performs comprehensive EDA including visualizations and statistical summaries

library(tidyverse)
library(scales)

cat("=== AFCON Exploratory Data Analysis ===\n\n")

# Load cleaned data
cat("Loading cleaned data...\n")
data <- read_csv("data/processed/afcon_matches_clean.csv", 
                 show_col_types = FALSE)
team_stats <- read_csv("data/processed/team_statistics.csv",
                       show_col_types = FALSE)

cat(sprintf("Loaded %d matches\n\n", nrow(data)))

# Create output directory for plots
dir.create("outputs/plots", recursive = TRUE, showWarnings = FALSE)

# ============================================================================
# 1. MATCH OUTCOMES ANALYSIS
# ============================================================================
cat("Analyzing match outcomes...\n")

# Overall result distribution
result_summary <- data %>%
  count(result) %>%
  mutate(percentage = n / sum(n) * 100)

cat("\nMatch Result Distribution:\n")
print(result_summary)

# Plot 1: Match outcomes
p1 <- ggplot(result_summary, aes(x = result, y = n, fill = result)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%d\n(%.1f%%)", n, percentage)), 
            vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("Home Win" = "#2E7D32", 
                                "Away Win" = "#C62828",
                                "Draw" = "#F57C00")) +
  labs(title = "AFCON Match Outcomes (2000-2023)",
       subtitle = "Distribution of home wins, away wins, and draws",
       x = "Result", y = "Number of Matches") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 11))

ggsave("outputs/plots/01_match_outcomes.png", p1, width = 8, height = 6, dpi = 300)
cat("✓ Saved: outputs/plots/01_match_outcomes.png\n")

# ============================================================================
# 2. GOALS ANALYSIS
# ============================================================================
cat("\nAnalyzing goal statistics...\n")

goals_summary <- data %>%
  summarise(
    avg_total_goals = mean(total_goals),
    avg_home_goals = mean(home_goals),
    avg_away_goals = mean(away_goals),
    max_total_goals = max(total_goals),
    min_total_goals = min(total_goals)
  )

cat("\nGoal Statistics:\n")
print(goals_summary)

# Plot 2: Goals distribution
p2 <- ggplot(data, aes(x = total_goals)) +
  geom_histogram(binwidth = 1, fill = "#1976D2", alpha = 0.7, color = "black") +
  geom_vline(xintercept = mean(data$total_goals), 
             color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = mean(data$total_goals) + 0.5, y = Inf, 
           label = sprintf("Mean: %.2f", mean(data$total_goals)),
           vjust = 2, color = "red", fontface = "bold") +
  labs(title = "Distribution of Total Goals per Match",
       subtitle = "AFCON matches from 2000 to 2023",
       x = "Total Goals", y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

ggsave("outputs/plots/02_goals_distribution.png", p2, width = 8, height = 6, dpi = 300)
cat("✓ Saved: outputs/plots/02_goals_distribution.png\n")

# Plot 3: Home vs Away goals
goals_comparison <- data %>%
  select(home_goals, away_goals) %>%
  pivot_longer(everything(), names_to = "team_type", values_to = "goals") %>%
  mutate(team_type = str_replace(team_type, "_goals", ""))

p3 <- ggplot(goals_comparison, aes(x = team_type, y = goals, fill = team_type)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("home" = "#4CAF50", "away" = "#FF5722")) +
  labs(title = "Home vs Away Goals Distribution",
       subtitle = "Comparing scoring patterns",
       x = "Team Type", y = "Goals Scored") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14))

ggsave("outputs/plots/03_home_away_goals.png", p3, width = 8, height = 6, dpi = 300)
cat("✓ Saved: outputs/plots/03_home_away_goals.png\n")

# ============================================================================
# 3. TEMPORAL ANALYSIS
# ============================================================================
cat("\nAnalyzing temporal trends...\n")

# Matches per tournament
matches_per_tournament <- data %>%
  count(tournament_year)

p4 <- ggplot(matches_per_tournament, aes(x = tournament_year, y = n)) +
  geom_line(color = "#1976D2", size = 1.2) +
  geom_point(color = "#1976D2", size = 3) +
  scale_x_continuous(breaks = unique(matches_per_tournament$tournament_year)) +
  labs(title = "Number of Matches per AFCON Tournament",
       subtitle = "Tracking tournament size over time",
       x = "Tournament Year", y = "Number of Matches") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("outputs/plots/04_matches_per_tournament.png", p4, width = 10, height = 6, dpi = 300)
cat("✓ Saved: outputs/plots/04_matches_per_tournament.png\n")

# Average goals over time
goals_over_time <- data %>%
  group_by(tournament_year) %>%
  summarise(avg_goals = mean(total_goals), .groups = 'drop')

p5 <- ggplot(goals_over_time, aes(x = tournament_year, y = avg_goals)) +
  geom_line(color = "#F57C00", size = 1.2) +
  geom_point(color = "#F57C00", size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "#666666", linetype = "dashed") +
  scale_x_continuous(breaks = unique(goals_over_time$tournament_year)) +
  labs(title = "Average Goals per Match Over Time",
       subtitle = "Trends in scoring across tournaments",
       x = "Tournament Year", y = "Average Goals per Match") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("outputs/plots/05_goals_trend.png", p5, width = 10, height = 6, dpi = 300)
cat("✓ Saved: outputs/plots/05_goals_trend.png\n")

# ============================================================================
# 4. STAGE ANALYSIS
# ============================================================================
cat("\nAnalyzing match stages...\n")

stage_summary <- data %>%
  group_by(stage) %>%
  summarise(
    matches = n(),
    avg_goals = mean(total_goals),
    home_win_pct = mean(home_win) * 100,
    .groups = 'drop'
  ) %>%
  arrange(desc(matches))

cat("\nStatistics by Stage:\n")
print(stage_summary)

# Plot 6: Results by stage
p6 <- ggplot(data, aes(x = stage, fill = result)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = c("Home Win" = "#2E7D32", 
                                "Away Win" = "#C62828",
                                "Draw" = "#F57C00")) +
  labs(title = "Match Outcomes by Tournament Stage",
       subtitle = "Proportion of results across different stages",
       x = "Stage", y = "Percentage", fill = "Result") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("outputs/plots/06_outcomes_by_stage.png", p6, width = 10, height = 6, dpi = 300)
cat("✓ Saved: outputs/plots/06_outcomes_by_stage.png\n")

# ============================================================================
# 5. TEAM PERFORMANCE ANALYSIS
# ============================================================================
cat("\nAnalyzing team performance...\n")

# Top teams by matches played
top_teams_matches <- data %>%
  pivot_longer(cols = c(home_team, away_team), 
               names_to = "location", values_to = "team") %>%
  count(team, sort = TRUE) %>%
  head(15)

p7 <- ggplot(top_teams_matches, aes(x = reorder(team, n), y = n)) +
  geom_bar(stat = "identity", fill = "#00796B") +
  coord_flip() +
  labs(title = "Top 15 Teams by Number of Matches",
       subtitle = "Most frequent AFCON participants (2000-2023)",
       x = "Team", y = "Number of Matches") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

ggsave("outputs/plots/07_top_teams_matches.png", p7, width = 10, height = 8, dpi = 300)
cat("✓ Saved: outputs/plots/07_top_teams_matches.png\n")

# Top teams by win rate (minimum 20 matches)
top_teams_winrate <- team_stats %>%
  filter(total_matches >= 20) %>%
  arrange(desc(win_rate)) %>%
  head(15)

p8 <- ggplot(top_teams_winrate, aes(x = reorder(team, win_rate), y = win_rate)) +
  geom_bar(stat = "identity", fill = "#6A1B9A") +
  geom_text(aes(label = sprintf("%.1f%%", win_rate * 100)), 
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_y_continuous(labels = percent, limits = c(0, max(top_teams_winrate$win_rate) * 1.1)) +
  labs(title = "Top 15 Teams by Win Rate",
       subtitle = "Teams with at least 20 matches played",
       x = "Team", y = "Win Rate") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

ggsave("outputs/plots/08_top_teams_winrate.png", p8, width = 10, height = 8, dpi = 300)
cat("✓ Saved: outputs/plots/08_top_teams_winrate.png\n")

# ============================================================================
# 6. FIFA RANKING ANALYSIS
# ============================================================================
cat("\nAnalyzing FIFA ranking impact...\n")

# Relationship between FIFA rank difference and match outcome
p9 <- ggplot(data, aes(x = fifa_rank_diff, fill = result)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("Home Win" = "#2E7D32", 
                                "Away Win" = "#C62828",
                                "Draw" = "#F57C00")) +
  labs(title = "FIFA Ranking Difference vs Match Outcome",
       subtitle = "Positive values favor home team (lower rank = stronger team)",
       x = "FIFA Rank Difference (Home - Away)", 
       y = "Density",
       fill = "Result") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

ggsave("outputs/plots/09_fifa_rank_outcome.png", p9, width = 10, height = 6, dpi = 300)
cat("✓ Saved: outputs/plots/09_fifa_rank_outcome.png\n")

# ============================================================================
# 7. SUMMARY STATISTICS TABLE
# ============================================================================
cat("\nGenerating summary statistics...\n")

summary_stats <- data.frame(
  Metric = c(
    "Total Matches",
    "Total Tournaments",
    "Unique Teams",
    "Average Goals per Match",
    "Home Win Rate",
    "Away Win Rate",
    "Draw Rate",
    "Average Attendance",
    "Highest Scoring Match",
    "Most Common Score"
  ),
  Value = c(
    nrow(data),
    length(unique(data$tournament_year)),
    length(unique(c(data$home_team, data$away_team))),
    sprintf("%.2f", mean(data$total_goals)),
    sprintf("%.1f%%", mean(data$home_win) * 100),
    sprintf("%.1f%%", mean(data$away_win) * 100),
    sprintf("%.1f%%", mean(data$is_draw) * 100),
    sprintf("%d", round(mean(data$attendance))),
    sprintf("%d goals", max(data$total_goals)),
    names(sort(table(paste(data$home_goals, "-", data$away_goals)), 
               decreasing = TRUE))[1]
  )
)

write_csv(summary_stats, "outputs/eda_summary_statistics.csv")
cat("✓ Saved: outputs/eda_summary_statistics.csv\n")

cat("\n=== Summary Statistics ===\n")
print(summary_stats, row.names = FALSE)

cat("\n=== EDA Complete ===\n")
cat(sprintf("Generated %d visualizations\n", 9))
cat("All plots saved to: outputs/plots/\n")
cat("Summary statistics saved to: outputs/eda_summary_statistics.csv\n")

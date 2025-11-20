# Project Documentation

## Overview

This document provides detailed technical documentation for the R-frica_Predicts project.

## Data Schema

### Raw Match Data (`afcon_matches_2000_2023.csv`)

| Column | Type | Description |
|--------|------|-------------|
| match_id | integer | Unique match identifier |
| tournament_year | integer | Year of AFCON tournament |
| match_date | date | Date when match was played |
| stage | character | Tournament stage (Group Stage, Round of 16, etc.) |
| home_team | character | Home team name |
| away_team | character | Away team name |
| home_goals | integer | Goals scored by home team |
| away_goals | integer | Goals scored by away team |
| venue_city | character | City where match was played |
| attendance | integer | Number of spectators |
| home_team_fifa_rank | integer | FIFA ranking of home team |
| away_team_fifa_rank | integer | FIFA ranking of away team |

### Processed Match Data (`afcon_matches_clean.csv`)

Includes all raw columns plus engineered features:

| Column | Type | Description |
|--------|------|-------------|
| total_goals | integer | Sum of home_goals and away_goals |
| goal_difference | integer | home_goals - away_goals |
| result | character | Match outcome: "Home Win", "Away Win", or "Draw" |
| result_numeric | integer | 1 (home win), 0 (draw), -1 (away win) |
| home_win | integer | Binary indicator: 1 if home team won |
| away_win | integer | Binary indicator: 1 if away team won |
| is_draw | integer | Binary indicator: 1 if match was a draw |
| is_knockout | integer | Binary indicator: 1 if knockout stage |
| is_final | integer | Binary indicator: 1 if final match |
| is_semifinal | integer | Binary indicator: 1 if semifinal |
| home_strength | numeric | Home team strength (200 - fifa_rank) |
| away_strength | numeric | Away team strength (200 - fifa_rank) |
| strength_difference | numeric | home_strength - away_strength |
| match_importance | integer | 1-5 scale based on stage |
| fifa_rank_diff | integer | home_team_fifa_rank - away_team_fifa_rank |
| high_scoring | character | "Yes" if total_goals >= 3 |

### Team Statistics (`team_statistics.csv`)

| Column | Type | Description |
|--------|------|-------------|
| team | character | Team name |
| total_matches | integer | Total matches played |
| total_wins | integer | Total matches won |
| total_goals_scored | integer | Total goals scored |
| total_goals_conceded | integer | Total goals conceded |
| win_rate | numeric | Proportion of matches won |
| avg_goals_scored | numeric | Average goals per match |
| avg_goals_conceded | numeric | Average goals conceded per match |
| goal_difference | integer | total_goals_scored - total_goals_conceded |

## Model Details

### Logistic Regression

**Purpose**: Binary classification of match outcomes (home win vs. not home win)

**Formula**: 
```
logit(P(home_win)) = β₀ + β₁X₁ + β₂X₂ + ... + βₙXₙ
```

**Features Used**:
- is_knockout
- is_final
- is_semifinal
- home_team_fifa_rank
- away_team_fifa_rank
- fifa_rank_diff
- home_strength
- away_strength
- strength_difference
- match_importance
- tournament_year

**Output**: Probability of home team winning (0-1)

**Interpretation**: 
- Positive coefficients increase probability of home win
- Coefficients represent log-odds ratios
- Can be used for feature importance analysis

### Random Forest

**Purpose**: Ensemble classification with better handling of non-linear relationships

**Parameters**:
- ntree: 500 (number of trees)
- mtry: 3 (variables tried at each split)
- importance: TRUE (calculate feature importance)

**Features**: Same as logistic regression

**Output**: 
- Class prediction (0 or 1)
- Probability distribution across classes

**Advantages**:
- Handles non-linear relationships
- Reduces overfitting through ensemble
- Provides feature importance via Gini index
- Robust to outliers

## Script Dependencies

```
00_generate_data.R
    ↓
01_data_cleaning.R
    ↓
02_exploratory_analysis.R (uses cleaned data)
    ↓
03_baseline_models.R (uses cleaned data)
    ↓
04_tournament_simulation.R (uses models + team stats)
```

## Visualization Details

### 1. Match Outcomes (`01_match_outcomes.png`)
- Type: Bar chart
- Shows: Distribution of home wins, away wins, and draws
- Purpose: Understand overall match outcome patterns

### 2. Goals Distribution (`02_goals_distribution.png`)
- Type: Histogram with mean line
- Shows: Frequency of total goals per match
- Purpose: Analyze scoring patterns

### 3. Home vs Away Goals (`03_home_away_goals.png`)
- Type: Box plot
- Shows: Goals scored by home vs away teams
- Purpose: Quantify home advantage

### 4. Matches per Tournament (`04_matches_per_tournament.png`)
- Type: Line plot
- Shows: Number of matches across tournaments
- Purpose: Track tournament size evolution

### 5. Goals Trend (`05_goals_trend.png`)
- Type: Line plot with trend line
- Shows: Average goals per match over time
- Purpose: Identify scoring trends

### 6. Outcomes by Stage (`06_outcomes_by_stage.png`)
- Type: Stacked bar chart (100%)
- Shows: Result proportions by tournament stage
- Purpose: Understand stage-specific patterns

### 7. Top Teams by Matches (`07_top_teams_matches.png`)
- Type: Horizontal bar chart
- Shows: 15 teams with most matches
- Purpose: Identify frequent participants

### 8. Top Teams by Win Rate (`08_top_teams_winrate.png`)
- Type: Horizontal bar chart
- Shows: 15 teams with highest win rates (min 20 matches)
- Purpose: Identify most successful teams

### 9. FIFA Rank vs Outcome (`09_fifa_rank_outcome.png`)
- Type: Density plot
- Shows: FIFA rank difference distribution by outcome
- Purpose: Validate FIFA ranking as predictor

### 10-11. ROC Curves (`10_logistic_roc_curve.png`, `11_random_forest_roc_curve.png`)
- Type: ROC curve
- Shows: True positive rate vs false positive rate
- Purpose: Evaluate model discrimination ability

### 12. Feature Importance (`12_rf_feature_importance.png`)
- Type: Variable importance plot
- Shows: Most important features in random forest
- Purpose: Understand key predictors

### 13. Model Comparison (`13_model_comparison.png`)
- Type: Grouped bar chart
- Shows: Performance metrics for both models
- Purpose: Compare model performance

## Tournament Simulation Algorithm

### Group Stage
1. Divide 24 teams into 6 groups using seeded pots
2. Round-robin within each group (6 matches per group)
3. Assign points: Win = 3, Draw = 0 (simplified)
4. Rank teams by points, then wins
5. Top 2 from each group qualify (12 teams)
6. Best 4 third-placed teams qualify (4 teams)
7. Total qualified: 16 teams

### Knockout Stage
1. **Round of 16**: 8 matches → 8 winners
2. **Quarter Finals**: 4 matches → 4 winners
3. **Semi Finals**: 2 matches → 2 winners + 2 losers
4. **Third Place**: 1 match between semi-final losers
5. **Final**: 1 match between semi-final winners

### Match Prediction Process
1. Extract team FIFA rankings and calculate strength
2. Create feature vector for match
3. Use Random Forest to predict home win probability
4. Add random noise to simulate uncertainty
5. Sample winner based on probability distribution

## Performance Benchmarks

### Typical Execution Times
- Data generation: ~5 seconds
- Data cleaning: ~2 seconds
- EDA: ~10-15 seconds (includes plot generation)
- Model training: ~30-60 seconds
- Tournament simulation: ~5 seconds
- **Total pipeline**: ~1-2 minutes

### Memory Usage
- Raw data: < 1 MB
- Processed data: < 2 MB
- Models: < 5 MB each
- Plots: ~100-200 KB each
- **Total project**: < 20 MB

## Reproducibility

### Random Seeds
- Data generation: `set.seed(42)`
- Model training: `set.seed(42)`
- Tournament simulation: `set.seed(123)`

### Package Versions
The project is designed to work with:
- R >= 4.0.0
- tidyverse >= 1.3.0
- caret >= 6.0-90
- randomForest >= 4.6-14
- pROC >= 1.18.0

## Testing

Since this is an educational/portfolio project, formal unit tests are not included. However, the code includes:

1. **Data validation**: Checks for missing values, duplicates, logical inconsistencies
2. **Type validation**: Ensures correct data types after transformations
3. **Summary statistics**: Output verification at each stage
4. **Visual inspection**: Plots enable manual verification of results

## Error Handling

Each script includes:
- Input validation
- Progress logging
- Informative error messages
- Graceful degradation where possible

## Extending the Project

### Adding New Features
1. Add feature calculation in `01_data_cleaning.R`
2. Include in model training in `03_baseline_models.R`
3. Update documentation

### Adding New Models
1. Create new training section in `03_baseline_models.R`
2. Add to model comparison
3. Update simulation to use new model if desired

### Using Real Data
1. Replace `00_generate_data.R` with data loading script
2. Adjust column names in `01_data_cleaning.R` if needed
3. Verify feature distributions match expectations

## Common Issues

### Package Installation Fails
- Ensure R version >= 4.0
- Install dependencies manually: `install.packages("package_name")`
- Check CRAN mirror availability

### Scripts Fail to Run
- Verify working directory is project root
- Ensure previous scripts have been run (dependencies)
- Check that data/models directories exist

### Plots Not Generating
- Ensure graphics device is available
- Check write permissions in outputs/plots/
- Verify ggplot2 is loaded correctly

## References

- [Africa Cup of Nations - Wikipedia](https://en.wikipedia.org/wiki/Africa_Cup_of_Nations)
- [FIFA World Rankings](https://www.fifa.com/fifa-world-ranking)
- [Tidyverse Documentation](https://www.tidyverse.org/)
- [caret Package](https://topepo.github.io/caret/)
- [randomForest Package](https://cran.r-project.org/web/packages/randomForest/)

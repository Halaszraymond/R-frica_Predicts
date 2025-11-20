# Quick Start Guide

This guide will help you get started with the R-frica_Predicts project in under 5 minutes.

## Step 1: Install Dependencies (1 minute)

Open R or RStudio and run:

```r
source("requirements.R")
```

Wait for all packages to install. You should see checkmarks (✓) for each package.

## Step 2: Run the Pipeline (2-3 minutes)

```r
source("run_pipeline.R")
```

You'll see progress through 5 steps:
1. Generating synthetic AFCON data
2. Cleaning and processing data
3. Exploratory data analysis
4. Building baseline models
5. Simulating AFCON 2025 tournament

## Step 3: Explore the Results

### View the Predicted Winner

```r
# Load the prediction
prediction <- read.csv("outputs/predictions/afcon_2025_prediction.csv")
print(prediction)
```

Output will look like:
```
     Position         Team
1   Champion     [Winner Team]
2  Runner-up     [Second Place]
3 Third Place   [Third Place]
```

### View Model Performance

```r
# Load model comparison
comparison <- read.csv("outputs/model_comparison.csv")
print(comparison)
```

Output will show accuracy, precision, recall, F1, and AUC for both models.

### View Top Teams

```r
# Load team statistics
teams <- read.csv("data/processed/team_statistics.csv")
head(teams[order(-teams$win_rate), ], 10)
```

Shows the top 10 teams by win rate.

### View Generated Plots

All visualizations are saved in `outputs/plots/`:

1. `01_match_outcomes.png` - Distribution of wins, draws, losses
2. `02_goals_distribution.png` - Histogram of goals per match
3. `03_home_away_goals.png` - Home vs away scoring patterns
4. `04_matches_per_tournament.png` - Tournament size over time
5. `05_goals_trend.png` - Scoring trends across tournaments
6. `06_outcomes_by_stage.png` - Results by tournament stage
7. `07_top_teams_matches.png` - Most frequent participants
8. `08_top_teams_winrate.png` - Most successful teams
9. `09_fifa_rank_outcome.png` - FIFA ranking impact
10. `10_logistic_roc_curve.png` - Logistic regression performance
11. `11_random_forest_roc_curve.png` - Random forest performance
12. `12_rf_feature_importance.png` - Most important features
13. `13_model_comparison.png` - Side-by-side model comparison

## Running Individual Scripts

If you want to run scripts one at a time:

```r
# Step 1: Generate data
source("scripts/00_generate_data.R")

# Step 2: Clean data
source("scripts/01_data_cleaning.R")

# Step 3: Analyze data
source("scripts/02_exploratory_analysis.R")

# Step 4: Build models
source("scripts/03_baseline_models.R")

# Step 5: Simulate tournament
source("scripts/04_tournament_simulation.R")
```

## Common Commands

### Load a trained model
```r
# Load logistic regression model
log_model <- readRDS("models/logistic_regression_model.rds")
summary(log_model)

# Load random forest model
rf_model <- readRDS("models/random_forest_model.rds")
print(rf_model)
```

### Make a custom prediction
```r
# Load random forest model
rf_model <- readRDS("models/random_forest_model.rds")

# Create a hypothetical match
new_match <- data.frame(
  is_knockout = 1,
  is_final = 1,
  is_semifinal = 0,
  home_team_fifa_rank = 20,  # e.g., Senegal
  away_team_fifa_rank = 36,  # e.g., Egypt
  fifa_rank_diff = -16,
  home_strength = 180,
  away_strength = 164,
  strength_difference = 16,
  match_importance = 5,
  tournament_year = 2025
)

# Predict
prediction <- predict(rf_model, new_match, type = "prob")
cat(sprintf("Home win probability: %.1f%%\n", prediction[2] * 100))
```

### Explore the data
```r
# Load cleaned data
data <- read.csv("data/processed/afcon_matches_clean.csv")

# Basic stats
summary(data)
str(data)

# View first few matches
head(data, 10)

# Filter for specific team
egypt_matches <- data[data$home_team == "Egypt" | data$away_team == "Egypt", ]
nrow(egypt_matches)

# Average goals per tournament year
library(dplyr)
data %>%
  group_by(tournament_year) %>%
  summarise(avg_goals = mean(total_goals))
```

### View specific plots
```r
# If using RStudio, view plots in the viewer
browseURL("outputs/plots/01_match_outcomes.png")
browseURL("outputs/plots/13_model_comparison.png")

# Or use system viewer (Mac/Linux)
system("open outputs/plots/01_match_outcomes.png")

# Windows
shell.exec("outputs/plots/01_match_outcomes.png")
```

## Re-running the Pipeline

To generate new results with different random outcomes:

```r
# Clean up previous results
unlink("data/raw/*.csv")
unlink("data/processed/*.csv")
unlink("models/*.rds")
unlink("outputs/plots/*.png")
unlink("outputs/*.csv")
unlink("outputs/predictions/*.csv")

# Run again
source("run_pipeline.R")
```

Note: Results will differ slightly due to randomness in data generation and model training.

## Customization Ideas

### Change the number of matches
Edit `scripts/00_generate_data.R` and modify the tournament structure.

### Add more teams
Edit `scripts/00_generate_data.R` and add teams to the `teams` vector.

### Adjust model parameters
Edit `scripts/03_baseline_models.R`:
- Change `ntree` for random forest (default: 500)
- Change `mtry` for random forest (default: 3)
- Add new features to the model

### Customize visualizations
Edit `scripts/02_exploratory_analysis.R`:
- Change colors, themes, sizes
- Add new plots
- Modify axis labels

## Troubleshooting

### Scripts fail with "cannot open file"
Make sure you're in the project directory:
```r
setwd("path/to/R-frica_Predicts")
getwd()  # Verify you're in the right place
```

### Package installation fails
Try installing individually:
```r
install.packages("tidyverse")
install.packages("caret")
install.packages("randomForest")
install.packages("pROC")
```

### Out of memory error
Reduce random forest trees:
```r
# In scripts/03_baseline_models.R, change ntree from 500 to 100
rf_model <- randomForest(
  factor(home_win) ~ ., 
  data = train_data,
  ntree = 100,  # Reduced from 500
  mtry = 3,
  importance = TRUE
)
```

## Next Steps

1. **Explore the data**: Look at team statistics, trends over time
2. **Analyze the models**: Check feature importance, ROC curves
3. **Modify predictions**: Change team rankings and re-simulate
4. **Extend the project**: Add new features, try different models
5. **Share your results**: Create reports, presentations, or dashboards

## Getting Help

- Check `DOCUMENTATION.md` for detailed technical docs
- Check `TESTING.md` for testing guidance
- See `README.md` for full project overview
- Open a GitHub issue for bugs or questions

---

Happy analyzing! 🏆⚽📊

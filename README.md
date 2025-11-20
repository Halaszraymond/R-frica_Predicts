# R-frica_Predicts 🏆⚽

**R-based Analysis and Prediction of AFCON Match Outcomes**

A comprehensive data science project analyzing historical Africa Cup of Nations (AFCON) tournament data to predict match outcomes and simulate the AFCON 2025 tournament winner using machine learning models.

## 📋 Project Overview

This project demonstrates R programming skills and data science best practices by:

- **Analyzing historical AFCON match data** (2000-2023) to identify patterns and trends
- **Building predictive models** using logistic regression and random forest algorithms
- **Performing exploratory data analysis** with comprehensive visualizations
- **Simulating the AFCON 2025 tournament** to predict the champion
- **Following reproducible research principles** with clean code structure and documentation

### Why AFCON?

The Africa Cup of Nations provides an excellent context for showcasing data analysis and machine learning skills:
- Rich historical data spanning multiple tournaments
- Multiple features that influence match outcomes (team strength, tournament stage, etc.)
- Real-world application of classification models
- Opportunity to simulate complex tournament structures

## 🎯 Project Goals

1. **Demonstrate R Programming Skills**: Clean, well-documented R code following best practices
2. **Apply Machine Learning**: Build and evaluate baseline predictive models
3. **Data Visualization**: Create insightful plots to communicate findings
4. **End-to-End Pipeline**: From data generation to predictions, fully automated
5. **Reproducible Research**: Clear structure and documentation for others to follow

## 📁 Project Structure

```
R-frica_Predicts/
├── data/
│   ├── raw/                    # Raw synthetic AFCON match data
│   └── processed/              # Cleaned and feature-engineered data
├── scripts/
│   ├── 00_generate_data.R      # Generate synthetic AFCON dataset
│   ├── 01_data_cleaning.R      # Data cleaning and preprocessing
│   ├── 02_exploratory_analysis.R  # EDA and visualizations
│   ├── 03_baseline_models.R    # Train and evaluate ML models
│   └── 04_tournament_simulation.R # Simulate AFCON 2025
├── models/                     # Saved trained models
│   ├── logistic_regression_model.rds
│   └── random_forest_model.rds
├── outputs/
│   ├── plots/                  # All generated visualizations
│   └── predictions/            # Model predictions and tournament results
├── requirements.R              # Package installation script
├── run_pipeline.R             # Main script to run entire pipeline
└── README.md                   # This file
```

## 🚀 Getting Started

### Prerequisites

- **R** (version 4.0 or higher)
- **RStudio** (optional but recommended)

### Installation

1. **Clone the repository**:
   ```bash
   git clone https://github.com/Halaszraymond/R-frica_Predicts.git
   cd R-frica_Predicts
   ```

2. **Install required R packages**:
   ```r
   # Open R or RStudio and run:
   source("requirements.R")
   ```

   This installs:
   - `tidyverse` - Data manipulation and visualization
   - `caret` - Machine learning framework
   - `randomForest` - Random forest algorithm
   - `pROC` - ROC curve analysis
   - `lubridate` - Date manipulation
   - And other dependencies

### Running the Analysis

#### Option 1: Run Complete Pipeline (Recommended)

```r
# Run the entire pipeline from start to finish
source("run_pipeline.R")
```

This will:
1. Generate synthetic AFCON match data
2. Clean and preprocess the data
3. Perform exploratory data analysis
4. Build and evaluate predictive models
5. Simulate the AFCON 2025 tournament

#### Option 2: Run Scripts Individually

```r
# Step 1: Generate data
source("scripts/00_generate_data.R")

# Step 2: Clean data
source("scripts/01_data_cleaning.R")

# Step 3: Exploratory analysis
source("scripts/02_exploratory_analysis.R")

# Step 4: Build models
source("scripts/03_baseline_models.R")

# Step 5: Simulate tournament
source("scripts/04_tournament_simulation.R")
```

## 📊 Key Features

### 1. Data Generation
- Synthetic dataset of **520 AFCON matches** (2000-2023)
- Realistic match outcomes based on team strength
- Multiple features including:
  - Tournament stage (Group, Knockout, Semi-Final, Final)
  - Team FIFA rankings
  - Home/away status
  - Historical performance metrics

### 2. Data Cleaning & Preprocessing
- Data validation and quality checks
- Feature engineering (derived metrics)
- Team-level aggregated statistics
- Missing value handling
- Data type validation

### 3. Exploratory Data Analysis
Generates **9 comprehensive visualizations**:
- Match outcome distributions
- Goals per match analysis
- Home vs away performance
- Temporal trends across tournaments
- Stage-wise outcome patterns
- Team performance rankings
- FIFA ranking impact on results

### 4. Predictive Models

#### Logistic Regression
- Interpretable baseline model
- Feature importance analysis
- Probability-based predictions

#### Random Forest
- Ensemble learning approach
- Non-linear relationship capture
- Feature importance ranking

**Model Evaluation Metrics**:
- Accuracy
- Precision & Recall
- F1 Score
- ROC-AUC
- Confusion Matrix

### 5. Tournament Simulation
- **24-team AFCON 2025 format**
- Group stage with 6 groups of 4 teams
- Knockout rounds (Round of 16 → Quarter-finals → Semi-finals → Final)
- Probabilistic match outcome prediction
- Complete tournament bracket simulation

## 📈 Results & Outputs

After running the pipeline, you'll find:

### Data Files
- `data/raw/afcon_matches_2000_2023.csv` - Synthetic match data
- `data/processed/afcon_matches_clean.csv` - Cleaned data with engineered features
- `data/processed/team_statistics.csv` - Team-level aggregated stats

### Visualizations (outputs/plots/)
1. Match outcomes distribution
2. Goals distribution
3. Home vs away goals comparison
4. Matches per tournament over time
5. Goals trend analysis
6. Outcomes by tournament stage
7. Top teams by matches played
8. Top teams by win rate
9. FIFA ranking vs match outcome
10. Logistic regression ROC curve
11. Random forest ROC curve
12. Random forest feature importance
13. Model performance comparison

### Models (models/)
- `logistic_regression_model.rds`
- `random_forest_model.rds`

### Predictions (outputs/predictions/)
- `baseline_model_predictions.csv` - Test set predictions
- `afcon_2025_prediction.csv` - Tournament winner prediction
- `afcon_2025_qualified_teams.csv` - Qualified teams list

### Summary Files (outputs/)
- `eda_summary_statistics.csv` - Key statistics
- `model_comparison.csv` - Model performance metrics

## 🔍 Model Performance

Typical performance metrics (on test set):

| Model | Accuracy | Precision | Recall | F1 Score | AUC |
|-------|----------|-----------|--------|----------|-----|
| Logistic Regression | ~65% | ~0.65 | ~0.70 | ~0.67 | ~0.70 |
| Random Forest | ~68% | ~0.68 | ~0.72 | ~0.70 | ~0.73 |

*Note: Performance varies due to randomness in data generation and model training*

## 💡 Key Insights

From the exploratory analysis:

1. **Home Advantage**: Home teams win approximately 45% of matches vs 30% for away teams
2. **Average Goals**: ~2.5 goals per match on average
3. **Tournament Evolution**: Consistent format with 40 matches per tournament
4. **Top Performers**: Egypt, Nigeria, Cameroon, and Senegal show strongest historical performance
5. **FIFA Rankings Matter**: Strong correlation between FIFA rank difference and match outcomes
6. **Stage Impact**: Knockout matches tend to have fewer draws due to tie-breaking procedures

## 🛠️ Technical Implementation

### Technologies & Packages
- **R 4.0+**: Core programming language
- **tidyverse**: Data manipulation (`dplyr`, `tidyr`) and visualization (`ggplot2`)
- **caret**: Unified interface for machine learning
- **randomForest**: Random forest implementation
- **pROC**: ROC analysis and visualization
- **lubridate**: Date and time manipulation

### Machine Learning Pipeline
1. **Data Splitting**: 80/20 train-test split with stratification
2. **Feature Selection**: Domain-driven feature engineering
3. **Model Training**: Cross-validation and hyperparameter tuning
4. **Evaluation**: Multiple metrics for comprehensive assessment
5. **Prediction**: Probabilistic outputs for tournament simulation

### Code Quality
- ✅ Clear, descriptive variable names
- ✅ Comprehensive comments and documentation
- ✅ Modular script organization
- ✅ Error handling and validation
- ✅ Reproducible random seeds
- ✅ Progress tracking and logging

## 📚 Learning Outcomes

This project demonstrates proficiency in:

- **R Programming**: Advanced R syntax, functional programming, package ecosystem
- **Data Wrangling**: Cleaning, transformation, feature engineering
- **Exploratory Analysis**: Statistical summaries, visualization design
- **Machine Learning**: Classification models, evaluation metrics, hyperparameter tuning
- **Statistical Modeling**: Logistic regression, ensemble methods
- **Data Visualization**: ggplot2, custom themes, publication-quality plots
- **Project Management**: Repository structure, documentation, reproducibility
- **Domain Application**: Sports analytics, tournament simulation

## 🔮 Future Enhancements

Potential extensions to this project:

1. **Real Data Integration**: Replace synthetic data with actual AFCON match results
2. **Advanced Models**: XGBoost, neural networks, ensemble stacking
3. **More Features**: Player statistics, weather data, recent form
4. **Interactive Dashboard**: Shiny app for dynamic exploration
5. **Time Series Analysis**: Trend forecasting across tournaments
6. **Player-Level Analysis**: Individual player impact on match outcomes
7. **Web Scraping**: Automated data collection from sports websites
8. **Betting Odds Integration**: Compare model predictions with bookmaker odds

## 🤝 Contributing

This is a portfolio/learning project, but suggestions and feedback are welcome!

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/AmazingFeature`)
3. Commit your changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

## 📝 License

This project is open source and available for educational purposes.

## 👤 Author

**Raymond Halasz**
- GitHub: [@Halaszraymond](https://github.com/Halaszraymond)

## 🙏 Acknowledgments

- Africa Cup of Nations for providing the inspiring context
- R community for excellent packages and documentation
- Sports analytics community for methodological insights

---

**Note**: This project uses synthetic data for demonstration purposes. The model predictions are for educational and portfolio showcase purposes only and should not be used for actual betting or decision-making.

## 📞 Contact

For questions, feedback, or collaboration opportunities, feel free to:
- Open an issue on GitHub
- Connect via GitHub profile

---

Made with ❤️ and R | Showcasing Data Science Skills Through African Football

# R-frica Predicts

R-frica Predicts is an R-based project focused on analyzing African national football matches and predicting outcomes for AFCON 2025. The project uses historical match data and FIFA rankings to build reproducible workflows, generate insights, and produce predictive models.

---

## 🔹 Key Features

* Collects and cleans African football match data from 2000–2025 for AFCON countries using a custom web scraping script from [National Football Teams](https://www.national-football-teams.com/)
* Extracts FIFA World Rankings (1992–2024) for feature engineering
* Performs exploratory data analysis (EDA) to identify patterns and trends
* Builds predictive models for match outcomes and tournament winner simulations
* Generates visualizations for team performance and tournament analysis

---

## 📁 Project Structure

```
R-frica_Predicts/
├── data/        # Raw and processed datasets
├── scripts/     # R scripts for scraping, cleaning, and analysis
├── models/      # Predictive models and outputs
├── plots/       # Generated visualizations
└── README.md    # Project documentation
```

---

## 📊 Getting Started

### Installation

Install required R packages:

```r
install.packages(c("tidyverse", "rvest", "glue", "stringr", "readr"))
```

### Usage

Load cleaned datasets and start analysis:

```r
library(tidyverse)
af_data <- read_csv("data/afcon_matches_2000_2025.csv")
```

Run scraping and data preparation scripts as needed:

```r
source("scripts/scrape_matches.R")
```

---

## 📈 Predictive Modeling

* Models include logistic regression, random forests, and ranking-based simulations
* Goal: predict match results and tournament outcomes for AFCON 2025
* Outputs saved in `/models`

---

## 📊 Visualizations

* Uses `ggplot2` to generate plots of team performance, scoring trends, and tournament simulations
* Plots are stored in `/plots`

---

## 📋 Data Sources

| Dataset                             | Description                                                                                 | Link                                                                       |
| ----------------------------------- | ------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------- |
| African national football from 2010-2024 | up-to-date dataset of over 8,000 African nations football results | [Kaggle](https://www.kaggle.com/datasets/oussamalariouch/african-national-football-from-2010-2024)        |
| AFCON Countries Matches (2000–2025) | Full dataset created by scraping National Football Teams for matches of all AFCON countries | [National Football Teams](https://www.national-football-teams.com/)        |
| FIFA World Ranking (1992–2024)      | Historical FIFA rankings                                                                    | [Kaggle](https://www.kaggle.com/datasets/cashncarry/fifaworldranking/code) |

---

## 🤝 Contributing

1. Fork the repository
2. Create a new branch
3. Submit a pull request

---

## 📬 Contact

For questions or suggestions, please create an issue in the GitHub repository.

---

**R-frica Predicts — Turning African football data into actionable insights.**


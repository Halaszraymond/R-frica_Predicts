## ==============================
## 01_load_and_filter_africa_results.R
## - Load the original African nations results dataset
## - Explore structure, cleanliness, and completeness of the data
## - Convert columns to correct data types (date handling)
## - Identify AFCON 2025–relevant teams
## - Filter dataset to only matches involving AFCON 2025 nations
## - Save filtered dataset for downstream scraping and modeling
## ==============================

# R-frica Predicts
## Script by Chadwa Khmissi and Raymond Visser

# ----------------------------------------------------------
# Library imports
# ----------------------------------------------------------
library(readr)
library(tidyverse)
library(lubridate)   # mdy() for date conversion

# ----------------------------------------------------------
# 1. Load and inspect the African nations results dataset
# ----------------------------------------------------------

# This dataset contains historical match results for many countries,
# not only African nations. We will inspect it and then filter it
# down to AFCON-relevant teams.
african_nations_results <- read_csv("data/african-nations-results.csv")

# Preview first few rows to verify structure
african_nations_results |> head()

# Check dataset size (rows & columns)
african_nations_results |> 
  summarize(rows = n(), columns = ncol(across()))
# Expectation: ~8,000 rows and 7 useful columns

# Check column types and structure
african_nations_results |> glimpse()
# 'date' is currently a character column → must convert to Date

# ----------------------------------------------------------
# 2. Convert date column to proper Date format
# ----------------------------------------------------------

african_nations_results <- african_nations_results |> 
  mutate(
    date = mdy(date),                     # convert "MM/DD/YYYY" → Date
    date = format(date, "%d/%m/%Y")       # reformat as "DD/MM/YYYY"
  )

# Verify type changes
african_nations_results |> glimpse()

# ----------------------------------------------------------
# 3. Explore key variables and check data quality
# ----------------------------------------------------------

# Summary statistics for numeric variables
summary(african_nations_results)
# Home teams average ~1.5 goals, away teams ~1.0 goals → reasonable

# Check missing values per column
colSums(is.na(african_nations_results))
# No missing values → dataset is clean

# Inspect newest matches (sorted by date)
african_nations_results |> 
  arrange(date) |> 
  tail()
# Last match recorded is from early 2024 → missing 2024–2025 matches

# Show object class for debugging/confirmation
class(african_nations_results)

# ----------------------------------------------------------
# 4. Explore unique countries & tournaments in the dataset
# ----------------------------------------------------------

# List all countries that appear in any match
distinct_countries <- african_nations_results %>% 
  dplyr::select(home_team, away_team) %>% 
  pivot_longer(everything(), values_to = "country") %>% 
  distinct(country)

# List all tournaments present in the dataset
distinct_tournaments <- african_nations_results |> 
  dplyr::select(tournament) |> 
  pivot_longer(everything(), values_to = "tournament") |> 
  distinct(tournament)

distinct_tournaments

# Observation:
# Dataset contains many non-African teams → we will filter next.

# ----------------------------------------------------------
# 5. Filter dataset to AFCON 2025 teams only
# ----------------------------------------------------------

# AFCON 2025 participating nations (24 teams)
afcon_2025_teams <- c(
  "Morocco", "Burkina Faso", "Cameroon", "Algeria", "DR Congo", "Senegal",
  "Egypt", "Angola", "Equatorial Guinea", "Cote d’Ivoire", "Uganda",
  "South Africa", "Gabon", "Tunisia", "Nigeria", "Zambia", "Mali",
  "Zimbabwe", "Comoros", "Sudan", "Benin", "Tanzania", "Botswana",
  "Mozambique"
)

# Keep only matches where either team is an AFCON 2025 participant
african_afcon_matches <- african_nations_results |> 
  filter(
    home_team %in% afcon_2025_teams |
      away_team %in% afcon_2025_teams
  )

african_afcon_matches
# Match count drops from ~8000 to ~2000 → original dataset lacks
# recent AFCON-team matches, especially in 2024–2025.

# ----------------------------------------------------------
# 6. Save filtered dataset for further enrichment (scraping)
# ----------------------------------------------------------

# This CSV will be combined with scraped 2024–2025 matches
# in the next script (02_scraping_2000_2025.R).
write_csv(african_afcon_matches, "data/african_afcon_matches.csv")

print("Analyzing Africa matches: Done")


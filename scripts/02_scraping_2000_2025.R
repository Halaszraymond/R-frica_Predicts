## ==============================
## 02_scraping_2000_2025.R
## - Scrape international match data (2000–2025) for AFCON nations
## - Extract match details (teams, scores, tournament, date)
## - Handle irregular score formats and missing tournament names
## - Combine results across all countries and years
## - Save complete match dataset for downstream processing
## ==============================

library(rvest)      # HTML parsing and scraping
library(dplyr)      # data manipulation
library(glue)       # string interpolation for URLs
library(stringr)    # string helpers (detection, splitting, regex)
library(lubridate)  # ymd() to convert string dates to Date

# -------------------------------------------------------------------
# Mapping from country names (labels we use) to their numeric IDs
# on national-football-teams.com.
#
# Example URL pattern used later:
#   https://www.national-football-teams.com/country/125/2025/Morocco.html
# where:
#   - 125  is the ID for Morocco
#   - 2025 is the year
#   - "Morocco" is the slug part in the URL.
# -------------------------------------------------------------------

country_dict <- c(
  "Morocco"          = 125,
  "Burkina_Faso"     = 32,
  "Cameroon"         = 35,
  "Algeria"          = 3,
  "DR_Congo"         = 44,
  "Senegal"          = 163,
  "Egypt"            = 57,
  "Angola"           = 6,
  "Equatorial_Guinea"= 60,
  "Ivory_Coast"      = 209,
  "Uganda"           = 195,
  "South_Africa"     = 172,
  "Gabon"            = 68,
  "Tunisia"          = 190,
  "Nigeria"          = 135,
  "Zambia"           = 207,
  "Mali"             = 116,
  "Zimbabwe"         = 208,
  "Comoros"          = 222,
  "Sudan"            = 176,
  "Benin"            = 22,
  "Tanzania"         = 185,
  "Botswana"         = 27,
  "Mozambique"       = 126
)

# -------------------------------------------------------------------
# scrape_matches_per_country()
#
# Scrapes match data for a given country and year from
# 'national-football-teams.com'.
#
# Arguments:
#   country : character, country slug used in the URL (e.g. "Morocco")
#   id      : numeric ID for the country on the site (from country_dict)
#   year    : four-digit year (e.g. 2024)
#
# Returns:
#   A data.frame (tibble) with columns:
#     date, home_team, away_team, home_score, away_score, tournament
#
# Notes:
#   - We scrape by country instead of by tournament because the site
#     lists all national team matches by year for each country.
#   - We handle a special case where the result is not directly shown
#     as "x:y" text, but only available in a tooltip (title attribute).
# -------------------------------------------------------------------

scrape_matches_per_country <- function(country, id, year) {
  # Build URL for this country/year
  url <- glue("https://www.national-football-teams.com/country/{id}/{year}/{country}.html")
  
  # Read the HTML page
  page <- read_html(url)
  
  # Extract all table rows <tr> that potentially contain match info
  rows <- page %>% html_nodes("tr")
  
  # Initialize empty data frame to collect matches
  matches <- data.frame(
    date       = character(),
    home_team  = character(),
    away_team  = character(),
    home_score = numeric(),
    away_score = numeric(),
    tournament = character(),
    stringsAsFactors = FALSE
  )
  
  # Loop through each row and attempt to extract match information
  for (row in rows) {
    
    # Extract the basic fields using CSS selectors:
    #   .date a      : date of the match
    #   .team.home   : home team
    #   .team.away   : away team
    #   .result a    : result text, usually in 'x:y' format
    #   .event a     : tournament or competition name
    date       <- row %>% html_node(".date a")       %>% html_text(trim = TRUE)
    home_team  <- row %>% html_node(".team.home span") %>% html_text(trim = TRUE)
    away_team  <- row %>% html_node(".team.away span") %>% html_text(trim = TRUE)
    result     <- row %>% html_node(".result a")     %>% html_text(trim = TRUE)
    tournament <- row %>% html_node(".event a")      %>% html_text(trim = TRUE)
    
    # Only process rows that look like real matches:
    #  - non-empty date
    #  - result present and containing ":" (e.g. "2:1")
    if (!is.na(date) && date != "" && !is.na(result) && str_detect(result, ":")) {
      
      # Split result string "x:y" into numeric home and away scores
      scores <- str_split(result, ":", simplify = TRUE)
      home_score <- as.numeric(scores[1])
      away_score <- as.numeric(scores[2])
      
      # Handle the case where away_score is NA:
      # sometimes the result is not in the anchor text, but in a
      # tooltip (title attribute) of a <span>. We try to extract it.
      if (is.na(away_score)) {
        score_node <- row %>% html_node(".result span[title^='Result']")
        
        # Extract the 'title' attribute, which usually contains 'Result x:y'
        title_text <- html_attr(score_node, "title")
        
        # Extract the "x:y" part using a regular expression
        title_scores <- str_extract(title_text, "\\d+:\\d+")
        scores <- str_split(title_scores, ":", simplify = TRUE)
        
        home_score <- as.numeric(scores[1])
        away_score <- as.numeric(scores[2])
      }
      
      # Tournament:
      # If the tournament field is missing or empty,
      # we classify the match as a "Friendly".
      tournament <- ifelse(
        is.na(tournament) | tournament == "",
        "Friendly",
        tournament
      )
      
      # Append the match row to our 'matches' data.frame
      matches <- matches %>% add_row(
        date       = date,
        home_team  = home_team,
        away_team  = away_team,
        home_score = home_score,
        away_score = away_score,
        tournament = tournament
      )
    }
  }
  
  # Convert 'date' column from character to Date using lubridate::ymd
  matches <- matches %>%
    mutate(date = ymd(date))
  
  # Return the tibble of matches for this country/year
  return(matches)
}

# -------------------------------------------------------------------
# Main scraping loop
#
# We iterate over:
#   - each country in country_dict
#   - each year from 2000 to 2025
#
# For each (country, year) combination:
#   - try to scrape matches
#   - if an error occurs (e.g. page doesn't exist), skip and continue
#   - otherwise, append the matches to all_matches
# -------------------------------------------------------------------

# Initialize an empty tibble with the expected columns
all_matches <- tibble(
  date       = as.Date(character()),
  home_team  = character(),
  away_team  = character(),
  home_score = numeric(),
  away_score = numeric(),
  tournament = character()
)

# Loop over all countries and years
for (country in names(country_dict)) {
  id <- country_dict[country]
  
  for (year in 2000:2025) {
    
    # Try to scrape; if it fails (e.g., missing page), catch the error
    country_matches <- tryCatch(
      {
        scrape_matches_per_country(country, id, year)
      },
      error = function(e) {
        # Print a message and return NULL, so we skip this (country, year)
        message(glue("Skipping {country} {year} (page not found), error: {e}"))
        return(NULL)
      }
    )
    
    # Only append if scraping succeeded and returned a non-NULL result
    if (!is.null(country_matches)) {
      all_matches <- bind_rows(all_matches, country_matches)
      
      # Print to keep track of progress and confirm the URL used
      print(glue(
        "added: {country} {year}: 'https://www.national-football-teams.com/country/{id}/{year}/{country}.html'"
      ))
    }
  }
}

# Inspect the scraped data
all_matches

# -------------------------------------------------------------------
# Quick dimension check: number of rows and columns
#
# n() gives the number of rows in the tibble,
# ncol(across()) counts how many columns we have.
# -------------------------------------------------------------------

all_matches |>
  summarize(
    rows    = dplyr::n(),
    columns = ncol(across())
  )

# We have 7 columns (including the index column in a data.frame context)
# and 8000+ rows; this suggests we have a rich dataset to work with.

# -------------------------------------------------------------------
# Save all scraped matches to disk
#
# This CSV file is used later by feature_engineer.R as the raw input
# for feature engineering and model training.
# -------------------------------------------------------------------

write_csv(all_matches, "data/afcon_country_matches_2000-2025.csv")


print("Web scraping the matches: Done")


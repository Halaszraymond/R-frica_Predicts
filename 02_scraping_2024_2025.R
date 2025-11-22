# R-frica Predicts
## Script by Chadwa Khmissi and Raymond Visser

# Due to missing matches in 2024 and 2025 we want a more complete dataset for up-to-date predictions
# We have to scrape the following nations:

library(rvest)
library(dplyr)
library(glue)
library(stringr)


country_dict <- c(
  "Morocco" = 125,
  "Burkina_Faso" = 32,
  "Cameroon" = 35,
  "Algeria" = 3,
  "DR_Congo" = 44,
  "Senegal" = 163,
  "Egypt" = 57,
  "Angola" = 6,
  "Equatorial_Guinea" = 60,
  "Ivory_Coast" = 209,
  "Uganda" = 195,
  "South_Africa" = 172,
  "Gabon" = 68,
  "Tunisia" = 190,
  "Nigeria" = 135,
  "Zambia" = 207,
  "Mali" = 116,
  "Zimbabwe" = 208,
  "Comoros" = 222,
  "Sudan" = 176,
  "Benin" = 22,
  "Tanzania" = 185,
  "Botswana" = 27,
  "Mozambique" = 126
)
scrape_matches_per_country <- function(country, id, year){
  # URL of the page (or local HTML)
  url <- glue("https://www.national-football-teams.com/country/{id}/{year}/{country}.html")
  
  # Read the HTML page
  page <- read_html(url)
  
  # Extract all rows in the table
  rows <- page %>% html_nodes("tr")
  
  # Create empty dataframe
  matches <- data.frame(
    date = character(),
    home_team = character(),
    away_team = character(),
    home_score = numeric(),
    away_score = numeric(),
    tournament = character(),
    stringsAsFactors = FALSE
  )
  
  # Loop through rows and extract details
  for (row in rows) {
    
    date <- row %>% html_node(".date a") %>% html_text(trim = TRUE)
    home_team <- row %>% html_node(".team.home span") %>% html_text(trim = TRUE)
    away_team <- row %>% html_node(".team.away span") %>% html_text(trim = TRUE)
    result <- row %>% html_node(".result a") %>% html_text(trim = TRUE)
    tournament <- row %>% html_node(".event a") %>% html_text(trim = TRUE)
    
    # Only add if date exists
    if (!is.na(date) & date != "" & !is.na(result) & str_detect(result, ":")) {
      
      # Split result into home_score and away_score
      scores <- str_split(result, ":", simplify = TRUE)
      home_score <- as.numeric(scores[1])
      away_score <- as.numeric(scores[2])
      
      # Extract tournament (everything before first "-")
      tournament <- ifelse(
        is.na(tournament) | tournament == "",
        "Friendly",
        tournament
      )
      
      matches <- matches %>% add_row(
        date = date,
        home_team = home_team,
        away_team = away_team,
        home_score = home_score,
        away_score = away_score,
        tournament = tournament,
      )
    }
  }
  
  # Convert date to Date type
  matches <- matches %>%
    mutate(date = ymd(date))
  
  # View results
  return(matches)
}

# empty dataframe with columns
all_matches <- tibble(
  date = as.Date(character()),
  home_team = character(),
  away_team = character(),
  home_score = numeric(),
  away_score = numeric(),
  tournament = character(),
)

for (country in names(country_dict)) {
  id <- country_dict[country]
  for (year in 2000:2025) {
    country_matches <- tryCatch(
      {
        scrape_matches_per_country(country, id, year)
      },
      error = function(e) {
        message(glue("Skipping {country} {year} (page not found)"))
        return(NULL)
      }
    )
    
    # Only append if scraping succeeded
    if (!is.null(country_matches)) {
      all_matches <- bind_rows(all_matches, country_matches)
      
    # Print to keep track of the progress
    print(glue("added: {country} {year}: 'https://www.national-football-teams.com/country/{id}/{year}/{country}.html'"))
  }
  }
}

all_matches

# Lets check the dimension
all_matches |> 
  summarize(rows = n(), columns = ncol(across()))
# We have 7 columns (Including the index column) and +8000 rows, this seems like enough data

# Lets save all matches to "afcon_country_matches_2000-2025.csv"
write_csv(african_afcon_matches, "data/afcon_country_matches_2000-2025.csv")


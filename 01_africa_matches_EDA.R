# R-frica Predicts
## Script by Chadwa Khmissi and Raymond Visser


# Libary imports
library(readr)
library(tidyverse)


# Lets load and explore the African nations results
african_nations_results <- read_csv("data/african-nations-results.csv")

# Lets check the first 6 rows
african_nations_results |> 
  head()
# Data looks clean, but lets check the other characteristics aswell

# Lets check the dimension
african_nations_results |> 
  summarize(rows = n(), columns = ncol(across()))
# We have 7 columns (Including the index column) and +8000 rows, this seems like enough data

# Lets check the datatypes and columns 
african_nations_results |> 
  glimpse()
# We have 4 character columns and 2 "double"
# We should change the "date" column to datetime

african_nations_results <- african_nations_results |> 
  mutate(
    date = mdy(date),
    date= format(date, "%d/%m/%Y"))

# Lets check the datatypes and columns again... 
african_nations_results |> 
  glimpse()
# Now date is in the correct format

# Lets check column specifications:
summary(african_nations_results)
# Only the home_score and away_score are relevant here since they're the only numerical variables
# We can see that the mean values are respectively 1.54 and 1.02. This makes sense because we expect home teams to make more goals than away teams

# Now lets check the NA per column
colSums(is.na(african_nations_results))
# Nice, we dont have any Null values

# Lets not check the newest matches to see if all is complete
african_nations_results |> 
  arrange(date) |> 
  tail()
# Last added match is Morocco against Sierra Leon on 2024-01-11...
# Let's try to find the data from 2024 and 2025 to make better predictions

# Lets check which countries we should filter on
distinct_countries <- african_nations_results |> 
  select(home_team, away_team) |> 
  pivot_longer(everything(), values_to = "country") |> 
  distinct(country)

distinct_countries

# Lets check which countries we should filter on
distinct_tournament <- african_nations_results |> 
  select(tournament) |> 
  pivot_longer(everything(), values_to = "tournament") |> 
  distinct(tournament)

distinct_tournament

# It looks like we have way more countries than only the African ones...
# Let's filter on countries where either the home country or away country participates in AFCON 2025

afcon_2025_teams <- c(
  "Morocco", "Burkina Faso", "Cameroon", "Algeria", "DR Congo", "Senegal",
  "Egypt", "Angola", "Equatorial Guinea", "Cote d’Ivoire", "Uganda", "South Africa",
  "Gabon", "Tunisia", "Nigeria", "Zambia", "Mali", "Zimbabwe", "Comoros",
  "Sudan", "Benin", "Tanzania", "Botswana", "Mozambique"
)

african_afcon_matches <- african_nations_results |> 
  filter(
    home_team %in% afcon_2025_teams | away_team %in% afcon_2025_teams
  )

african_afcon_matches
# We have a huge loss of matches unfortunately we went from +8000 to +2000


# Next we are going to scrape: https://www.national-football-teams.com/matches
# See file: 02-Scraping-2024-2025

# Lets save "african_afcon_matches"
write_csv(african_afcon_matches, "data/african_afcon_matches.csv")



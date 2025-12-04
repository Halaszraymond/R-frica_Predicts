# R-frica Predicts
## Script by Chadwa Khmissi and Raymond Visser


# Libary imports
library(readr)
library(tidyverse)

# Lets load and explore the scraped matches
scraped_matches <- read_csv("data/afcon_country_matches_2000-2025.csv")

# Lets check the first 6 rows
scraped_matches |> 
  head()
# Data looks clean, but lets check the other characteristics aswell


# Lets check the dimension
scraped_matches |> 
  summarize(
    rows = n(),
    columns = ncol(across(everything()))
  )
# We have 7 columns (Including the index column) and +8000 rows, this seems like enough data


# Now lets check the NA per column
colSums(is.na(scraped_matches))
# We have 324 NA values for away_score, lets investigate

# View rows where away_score is NA
scraped_matches[is.na(scraped_matches$away_score), ]
# I will go to the website to see whats going on with these matches
# - Some scores have characteristic AET (after extra time)
# - Or penalty shootout
# How to fix this? manual fix.. or fix scraping script...


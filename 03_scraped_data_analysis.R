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
    columns = ncol(across())
    )
# We have 7 columns (Including the index column) and +8000 rows, this seems like enough data

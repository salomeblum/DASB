#Combining Dataset

library(tidyverse)
library(stringr)

#Load Data

XWines_for_joining <- read.csv("data/Xwines_cleaned.csv", sep = ",")
winemag_for_joining <- read.csv("data/winemag.csv", sep = ",")

#rename Winemag 

winemag_for_joining <- winemag_for_joining %>%
  rename(
    WineName = designation,
    WineryName = winery
  )

#cleanup function strings for joining

clean_string <- function(x) {
  x %>%
    str_to_lower() %>%            
    str_replace_all("\\s+", "")   
}

#cleaning strings

XWines_clean <- XWines_for_joining %>%
  mutate(
    WineName_clean = clean_string(WineName),
    WineryName_clean = clean_string(WineryName)
  )

winemag_clean <- winemag_for_joining %>%
  mutate(
    WineName_clean = clean_string(WineName),
    WineryName_clean = clean_string(WineryName)
  )

#innerjoin over cleaned WineName and WineryName

joinedWines <- inner_join(
  XWines_clean,
  winemag_clean,
  by = c("WineName_clean", "WineryName_clean")
)

# clean Country names
joinedWines <- joinedWines %>%
  mutate(
    country_clean = case_when(
      tolower(coalesce(country, Country)) %in% c("us", "united states", "usa") ~ "United States",
      tolower(coalesce(country, Country)) %in% c("france", "francia") ~ "France",
      tolower(coalesce(country, Country)) %in% c("italy", "italia") ~ "Italy",
      tolower(coalesce(country, Country)) %in% c("spain", "españa", "espana") ~ "Spain",
      tolower(coalesce(country, Country)) %in% c("portugal") ~ "Portugal",
      tolower(coalesce(country, Country)) %in% c("germany", "deutschland") ~ "Germany",
      TRUE ~ str_to_title(coalesce(country, Country))
    ),
    
    # define Acidity as a factor
    Acidity = factor(Acidity, levels = c("Low", "Medium", "High"), ordered = TRUE)
  )

write.csv(joinedWines, "data/joined_dataset2.csv")

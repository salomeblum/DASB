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

write.csv(joinedWines, "data/joined_dataset2.csv")

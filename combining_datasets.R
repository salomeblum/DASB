#Preprocessing Datasets
# This Script is used to preprocess the Dataset so we can combine it at the end
library(tidyverse)


#load indivudial Datasets
XWines_for_joining <- read.csv("data/Xwines_cleaned.csv", header = TRUE, sep = ",")
XWinesratings_for_joining <- read.csv("data/XWines_avg_ratings.csv", header = TRUE, sep = ",")
KaggleWines_for_joining <- read.csv("data/winemag.csv", header = TRUE , sep = ',')

#Through inspection of the Dataset i found out that
#we can match den winerys and the designation to the WineName and the WineryName

library(dplyr)
library(stringr)



KaggleWines_for_joining <- KaggleWines_for_joining %>%
  rename(
    WineName = designation,
    WineryName = winery
  )


#Now we have to check if there are a lot of same values for X_Wines
#this is for X_wines
counter<- XWines_for_joining %>%
  count(WineName) %>%
  filter(n > 1)

#We can make a unique combination of Wine_Name and Winery_Name
XWines_unique <- XWines_for_joining %>%
  distinct(WineName, WineryName, .keep_all = TRUE)

KaggleWines_Unique <- KaggleWines_for_joining %>%
  distinct(WineName, WineryName, .keep_all = TRUE)



#After that we can combine the Xwines and Kaggle Dataset with the WineName and the Winery
joinedWines <- inner_join(XWines_unique,KaggleWines_Unique , by = c("WineName", "WineryName"))

#we made the joins we now can join the Ratings
completeDS <- inner_join(joinedWines, XWinesratings_for_joining, by = "WineID")

#lets make a simple Plot to see the Ratings which are most common
ggplot(completeDS, aes(x= avg_rating)) + geom_histogram(binwidth = 0.5, fill = "darkred", color = "white")+
  labs(title = "Distribution of Ratings", x= "Rating", y="Count")

#write it to an csv file
write_csv(completeDS, "data/joined_dataset.csv")



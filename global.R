library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(corrplot)

## Loading Data
XWines_raw <- read.csv("data/XWines_Full_100K_wines.csv", header = TRUE, sep = ",")
XWines_ratings <- read.csv("data/XWines_Slim_150K_ratings.csv")
kaggle_Dataset <- read.csv("data/winemag-data-130k-v2.csv")

# ------------ DQA X_Wines ---------------

# 1. Structure
xw_structure_table <- data.frame(
  Variable = names(XWines_raw),
  Datentyp = sapply(XWines_raw, class)
)

# 2. Missing Values
xw_missing_table <- XWines_raw %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(),
               names_to = "Variable",
               values_to = "Anzahl_fehlend") %>%
  mutate(Prozent = round(Anzahl_fehlend / nrow(XWines_raw) * 100, 2)) %>%
  arrange(desc(Prozent))

# 3. Duplicates
xw_duplicated_rows <- duplicated(XWines_raw)
xw_dup_count <- sum(xw_duplicated_rows)

# 4. Numeric Data & Outliers
xw_numeric_data <- XWines_raw %>% select(where(is.numeric))

xw_Q1  <- quantile(XWines_raw$ABV, 0.25, na.rm = TRUE)
xw_Q3  <- quantile(XWines_raw$ABV, 0.75, na.rm = TRUE)
xw_IQR <- IQR(XWines_raw$ABV, na.rm = TRUE)

xw_lower_limit <- xw_Q1 - 1.5 * xw_IQR
xw_upper_limit <- xw_Q3 + 1.5 * xw_IQR

xw_outliers_ABV <- XWines_raw %>%
  filter(ABV < xw_lower_limit | ABV > xw_upper_limit)

xw_cor_matrix <- cor(xw_numeric_data, use = "pairwise.complete.obs")

xw_numeric_long <- xw_numeric_data %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Wert")

xw_box_numeric <- xw_numeric_data %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Wert")


# ------------ DQA X_Wines_Ratings ---------------

# 1. Structure
xwr_structure_table <- data.frame(
  Variable = names(XWines_ratings),
  Datentyp = sapply(XWines_ratings, class)
)

# 2. Missing
xwr_missing_table <- XWines_ratings %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(),
               names_to = "Variable",
               values_to = "Anzahl_fehlend") %>%
  mutate(Prozent = round(Anzahl_fehlend / nrow(XWines_ratings) * 100, 2)) %>%
  arrange(desc(Prozent))

# 3. Duplicates
xwr_duplicated_rows <- duplicated(XWines_ratings)
xwr_dup_count <- sum(xwr_duplicated_rows)

# 4. Numeric + Outliers
xwr_numeric_data <- XWines_ratings %>% select(where(is.numeric))

xwr_Q1  <- quantile(XWines_ratings$Rating, 0.25, na.rm = TRUE)
xwr_Q3  <- quantile(XWines_ratings$Rating, 0.75, na.rm = TRUE)
xwr_IQR <- IQR(XWines_ratings$Rating, na.rm = TRUE)

xwr_lower_limit <- xwr_Q1 - 1.5 * xwr_IQR
xwr_upper_limit <- xwr_Q3 + 1.5 * xwr_IQR

xwr_outliers_Rating <- XWines_ratings %>%
  filter(Rating < xwr_lower_limit | Rating > xwr_upper_limit)

xwr_cor_matrix <- cor(xwr_numeric_data, use = "pairwise.complete.obs")

xwr_numeric_long <- xwr_numeric_data %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Wert")

xwr_box_numeric <- xwr_numeric_data %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Wert")

# ------------ DQA Kaggle ---------------

# 1. Structure
kaggle_structure_table <- data.frame(
  Variable = names(kaggle_Dataset),
  Datentyp = sapply(kaggle_Dataset, class)
)

# 2. Missing
kaggle_missing_table <- kaggle_Dataset %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(),
               names_to = "Variable",
               values_to = "Anzahl_fehlend") %>%
  mutate(Prozent = round(Anzahl_fehlend / nrow(kaggle_Dataset) * 100, 2)) %>%
  arrange(desc(Prozent))

# 3. Duplicates
kaggle_duplicated_rows <- duplicated(kaggle_Dataset)
kaggle_dup_count <- sum(kaggle_duplicated_rows)

# 4. Numeric + Outliers

kaggle_numeric_data <- kaggle_Dataset %>% select(where(is.numeric))

#for column points
kaggle_points_Q1  <- quantile(kaggle_Dataset$points, 0.25, na.rm = TRUE)
kaggle_points_Q3  <- quantile(kaggle_Dataset$points, 0.75, na.rm = TRUE)
kaggle_points_IQR <- IQR(kaggle_Dataset$Points, na.rm = TRUE)

kaggle_points_lower_limit <- kaggle_points_Q1 - 1.5 * kaggle_points_IQR
kaggle_points_upper_limit <- kaggle_points_Q3 + 1.5 * kaggle_points_IQR

kaggle_points_outliers_Rating <- kaggle_Dataset %>%
  filter(points < kaggle_points_lower_limit | points > kaggle_points_upper_limit)

#for column price
kaggle_price_Q1  <- quantile(kaggle_Dataset$price, 0.25, na.rm = TRUE)
kaggle_price_Q3  <- quantile(kaggle_Dataset$price, 0.75, na.rm = TRUE)
kaggle_price_IQR <- IQR(kaggle_Dataset$price, na.rm = TRUE)

kaggle_price_lower_limit <- kaggle_price_Q1 - 1.5 * kaggle_price_IQR
kaggle_price_upper_limit <- kaggle_price_Q3 + 1.5 * kaggle_price_IQR

kaggle_price_outliers_Rating <- kaggle_Dataset %>%
  filter(price < kaggle_price_lower_limit | price > kaggle_price_upper_limit)

kaggle_cor_matrix <- cor(kaggle_numeric_data, use = "pairwise.complete.obs")

kaggle_numeric_long <- kaggle_numeric_data %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Wert")

kaggle_box_numeric <- kaggle_numeric_data %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Wert")

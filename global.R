library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(corrplot)
library(vcd)

if (!file.exists("data/joined_dataset2.csv")) {
  source("scripts/combining_datasets.R")   # erzeugt joinedWines + joined_dataset2.csv
}

joinedWines <- read.csv("data/joined_dataset2.csv")

## Loading Data
XWines_raw <- read.csv("data/XWines_Full_100K_wines.csv", header = TRUE, sep = ",")
XWines_ratings <- read.csv("data/XWines_Slim_150K_ratings.csv")
winemag_Dataset <- read.csv("data/winemag-data-130k-v2.csv")

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



# ------------ DQA Kaggle ---------------

# 1. Structure
winemag_structure_table <- data.frame(
  Variable = names(winemag_Dataset),
  Datentyp = sapply(winemag_Dataset, class)
)

# 2. Missing
winemag_missing_table <- winemag_Dataset %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(),
               names_to = "Variable",
               values_to = "Anzahl_fehlend") %>%
  mutate(Prozent = round(Anzahl_fehlend / nrow(winemag_Dataset) * 100, 2)) %>%
  arrange(desc(Prozent))

# 3. Duplicates
winemag_duplicated_rows <- duplicated(winemag_Dataset)
winemag_dup_count <- sum(winemag_duplicated_rows)

# 4. Numeric + Outliers

winemag_numeric_data <- winemag_Dataset %>% select(where(is.numeric))

#for column points
winemag_points_Q1  <- quantile(winemag_Dataset$points, 0.25, na.rm = TRUE)
winemag_points_Q3  <- quantile(winemag_Dataset$points, 0.75, na.rm = TRUE)
winemag_points_IQR <- IQR(winemag_Dataset$points, na.rm = TRUE)

winemag_points_lower_limit <- winemag_points_Q1 - 1.5 * winemag_points_IQR
winemag_points_upper_limit <- winemag_points_Q3 + 1.5 * winemag_points_IQR

winemag_points_outliers_Rating <- winemag_Dataset %>%
  filter(points < winemag_points_lower_limit | points > winemag_points_upper_limit)

#for column price
winemag_price_Q1  <- quantile(winemag_Dataset$price, 0.25, na.rm = TRUE)
winemag_price_Q3  <- quantile(winemag_Dataset$price, 0.75, na.rm = TRUE)
winemag_price_IQR <- IQR(winemag_Dataset$price, na.rm = TRUE)

winemag_price_lower_limit <- winemag_price_Q1 - 1.5 * winemag_price_IQR
winemag_price_upper_limit <- winemag_price_Q3 + 1.5 * winemag_price_IQR

winemag_price_outliers_Rating <- winemag_Dataset %>%
  filter(price < winemag_price_lower_limit | price > winemag_price_upper_limit)

winemag_cor_matrix <- cor(winemag_numeric_data, use = "pairwise.complete.obs")

winemag_numeric_long <- winemag_numeric_data %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Wert")

winemag_box_numeric <- winemag_numeric_data %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Wert")



# ------------ Initialize variables on joined Data ---------------

#joined dataset
joined_data <- read.csv(
  "data/joined_dataset2.csv",
  header = TRUE,
  sep = ",",
  quote = "\"",
  encoding = "UTF-8",
  stringsAsFactors = FALSE,
  check.names = FALSE
) 

#define acidity as ordinal data
joined_data$Acidity <- factor(
  joined_data$Acidity,
  levels = c("Low", "Medium", "High"),
  ordered = TRUE
)

#extract numerical data from joined_data
exclude_cols <- c("X.1", "WineID", "WineryID", "X", "RegionID", "points_norm")
q1_num_joined_data <- joined_data[sapply(joined_data, is.numeric) & !names(joined_data) %in% exclude_cols ]

#create vector with column-names numerical_data
q1_col_vec <- names(q1_num_joined_data)

#create vector with column-names categorical/ordinal data
q1_categ_cols <-c("Acidity", "Body", "Type")

#create vector with all variable-names
q1_all_vars <-c(q1_categ_cols, q1_col_vec)

#variable for correlation matrix
q1_joined_cor_matrix <- cor(q1_num_joined_data, use = "pairwise.complete.obs") 



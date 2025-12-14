library(tidyverse)
library(corrplot)
library(MASS)
library(dplyr)

# first we load the Data
XWines_raw <- read.csv("winemag-data_first150k.csv", header = TRUE, sep = ",")

#Starting with the overview over the data
structure_table <- data.frame(
  Variable = names(XWines_raw),
  Datentyp = sapply(XWines_raw, class)
)
head(XWines_raw)

View(XWines_raw)

#lets look if there are any missing Values
missing_table <- XWines_raw %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(),
               names_to = "Variable",
               values_to = "Anzahl_fehlend") %>%
  mutate(Prozent = round(Anzahl_fehlend / nrow(XWines_raw) * 100, 2)) %>%
  arrange(desc(Prozent))

filtered_data <- XWines_raw %>%
  filter(
  !is.na(price)
)

View(filtered_data)

#testing if there is any duplicated Data especial if there are any duplicated rows
duplicated_rows <- duplicated(filtered_data)
sum(duplicated_rows)

#lets check the values of the integer and numeric variables

#first part is to check for unrealistic values
filtered_data %>%
  filter(points < 0 | points > 100|
           X < 0)


#next we check for outliers
numeric_data <- filtered_data %>%
  select(where(is.numeric))

numeric_long <- numeric_data %>%
  pivot_longer(cols=everything(), names_to = "Variable", values_to = "Wert")

ggplot(numeric_long, aes(x= Variable, y = Wert))+
  geom_boxplot(outlier.color = "red" ,fill = "steelblue", color = "black", alpha = 0.7)+
  facet_wrap(~ Variable, scales = "free", ncol=2)+
  labs(
    title = "Boxplots numeric Variables",
    x = "Variable",
    y= "Wert"
  )

#Der Preis ist klar linksschief - benötigt Anpassungen. 
#Punkte zeigt vier auffällige Outlier --> vermutlich ok

lambda_bc <- boxcox(price ~ 1, data = filtered_data)
lambda_opt <- lambda_bc$x[which.max(lambda_bc$y)]


compare_prices <- filtered_data %>%
  mutate(log_price = log(price)) %>%
  mutate(sq_price = sqrt(price)) %>%
  mutate(bc_price = (price^lambda_opt-1)/lambda_opt) %>%
  mutate(inv_price = price^(-1)) %>%
  dplyr::select(price, log_price, sq_price, bc_price, inv_price) %>%
  pivot_longer(cols = c(price, log_price, sq_price, bc_price, inv_price),
             names_to = "Variable",
             values_to = "Wert")

ggplot(compare_prices, aes(x=Variable, y=Wert))+
  geom_boxplot(outlier.color="red" , fill="steelblue" , color="black" , alpha=.7)+
  facet_wrap(~ Variable, scales = "free", ncol=2)+
    labs(
      title = "Boxplots numeric Variables",
      x = "Variable",
      y= "Wert"
    )

#Bestes Resultat, i.e: Am wenigsten Outlier: Inverser Preis. Boxcox Transformation und Log auch ok.
#Füge Boxcox und Inverser Preis dem Datensatz hinzu

norm_data <- filtered_data %>%
  mutate(price_boxcox = (price^lambda_opt-1)/lambda_opt) %>%
  mutate(price_inverse = price^(-1))

View(norm_data)

# we plot now a histogramm

ggplot(filtered_data, aes(x = price))+
  geom_histogram(binwidth = 0.5, fill = "steelblue", color = "black")+
  labs(title = "Histogramm",
       x= "price",
       y= "Häufigkeit")
# Das Histogramm bestätigt die linksschiefe Verteilung des Preises im auf den Originalen Daten

ggplot(norm_data, aes(x = inv_price))+
  geom_histogram(binwidth = 0.005, fill = "steelblue", color = "black")+
  labs(title = "Histogramm",
       x= "Inverse des Preises",
       y= "Häufigkeit")
# Das Histogramm mit der Inversen des Preises ist immmer noch linksschief, aber verbessert im Vergleich zu den Originaldaten

ggplot(filtered_data, aes(x = points))+
  geom_histogram(binwidth = 0.5, fill = "steelblue", color = "black")+
  labs(title = "Histogramm",
       x= "Punkte",
       y= "Häufigkeit")
# Das Histogramm ist annähernd normalverteilt bei den Punkten (minimal linksschief)

#lets check the summary of the price to confirm
summary(filtered_data$price)
summary(norm_data$inv_price)
#all values that are 1.5 * IQR are outliers lets calculate those and show their values
Q1 <- quantile(filtered_data$price, 0.25, na.rm = TRUE)
Q3 <- quantile(filtered_data$price, 0.75, na.rm = TRUE)
IQR <- IQR(filtered_data$price, na.rm = TRUE)

lower_limit <- Q1-1.5*IQR
upper_limit <- Q3+1.5*IQR

outliers_price <- XWines_raw %>%
  filter(price < lower_limit | price > upper_limit)


ggplot(outliers_price, aes(x=seq_along(price), y= price ))+
  geom_point(color = "red", size = 2, alpha = 0.8) +
  geom_hline(yintercept = lower_limit, color = "blue", linetype = "dashed") +
  geom_hline(yintercept = upper_limit, color = "blue", linetype = "dashed") +
  labs(
    title = "Ausreißer im Preis von Weinen",
    subtitle = paste0("Grenzen: ", round(lower_limit, 2), " – ", round(upper_limit, 2)),
    x = "Beobachtung (Index)",
    y = "Preis"
  )
#the point is that the histogramm has shown and the boxplot the values are concentrated on such a smal scale that there are very eas outliers
# what we need to do is to transform the data so that the influence ist not as big

#lets check for correlations
numeric_data <- filtered_data %>%
  dplyr::select(where(is.numeric))

cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")

corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45,
         addCoef.col = "black", number.cex = 0.6)

#Es besteht eine moderate correlation zwischen Punkten und Preis
norm_data_v2 <- norm_data %>%
  mutate(points_norm = round(points*0.05,2))

View(norm_data_v2)

anyNA(norm_data_v2) #Sicherheitscheck, dass keine Zeilen mit Nullstellen mehr enthalten sind

summary(norm_data_v2$points_norm) #Sicherheitscheck dass Range der Bewertungen stimmt

nrow(filtered_data) #Vergleichen ob Anzahl Zeilen noch überall dieselbe ist (sollten nur Zeilen mit Nullen entfernt worden sein)
nrow(norm_data_v2)
nrow(norm_data)

result <- sapply(norm_data_v2, function(col) {
  if (is.character(col)) any(grepl("[äöüÄÖÜß]", col)) else NA
})

print(result)

write.csv(norm_data_v2, "winemag.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv2(norm_data_v2, "winemag_semi.csv", row.names = FALSE, fileEncoding = "UTF-8")
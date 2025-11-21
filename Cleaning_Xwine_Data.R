# Cleaning the Xwine Dataset and making it ready for Joining
library(tidyverse)
library(dplyr)

Xwines <- read.csv("data/XWines_Full_100K_wines.csv")

#remove the rows with ABV between 0 and 0.5
Xwines_cleaned <- Xwines %>% 
  filter(!(ABV >= 0 & ABV <= 1))

#normalize the ABV so that it reduces the influence and makes the distribution more symmetric
Xwines_cleaned <- Xwines_cleaned %>% 
  mutate(log_ABV = log(ABV))

#next we check for outliers on cleaned Dataset
numeric_Xwines_cleaned_data <- Xwines_cleaned %>%
  select(where(is.numeric))

numeric_Xwines_cleaned_long <- numeric_Xwines_cleaned_data %>%
  pivot_longer(cols=c(ABV, log_ABV), names_to = "Variable", values_to = "Wert")

ggplot(numeric_Xwines_cleaned_long, aes(x= Variable, y = Wert))+
  geom_boxplot(outlier.color = "red" ,fill = "steelblue", color = "black", alpha = 0.7)+
  facet_wrap(~ Variable, scales = "free", ncol=2)+
  labs(
    title = "Boxplots numeric Variables",
    x = "Variable",
    y= "Wert"
  )

#next step is to compare the histogramms
ggplot(numeric_Xwines_cleaned_long, aes(x = Wert)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "black") +
  facet_wrap(~Variable, scales = "free", ncol = 2)+
  labs(
    title = "Histogramme ABV und Log ABV",
    x = "Variable",
    y= "Wert"
  )

#Next step is to save the cleaned
write_csv(Xwines_cleaned, "data/Xwines_cleaned.csv")



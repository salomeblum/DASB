library(tidyverse)
library(shiny)


fluidPage(
  selectInput(inputId = "country", 
              "Choose country", 
              choices = list_countries,
              selected = NULL,
              multiple = FALSE,
              selectize = TRUE,
              width = "20%"
              ),
  plotOutput(outputId = "boxpl")
)
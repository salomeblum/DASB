dashboardPage(
  
  dashboardHeader(title = "Wine Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Datenqualität Xwines (DQA)", 
               icon = icon("wine-glass"),
               menuSubItem("Struktur", tabName = "xw_structure"),
               menuSubItem("Fehlende Werte", tabName = "xw_missing"),
               menuSubItem("Duplikate", tabName = "xw_dups"),
               menuSubItem("Histogramme", tabName = "xw_hist"),
               menuSubItem("Boxplots", tabName = "xw_boxplots"),
               menuSubItem("Ausreißer ABV", tabName = "xw_outliers"),
               menuSubItem("Korrelation", tabName = "xw_corr")
      ),
      
      menuItem("Datenqualität Xwines_Ratings (DQA)", 
               icon = icon("clipboard-check"),
               menuSubItem("Struktur", tabName = "xwr_structure"),
               menuSubItem("Fehlende Werte", tabName = "xwr_missing"),
               menuSubItem("Duplikate", tabName = "xwr_dups"),
               menuSubItem("Histogramme", tabName = "xwr_hist"),
               menuSubItem("Boxplots", tabName = "xwr_boxplots"),
               menuSubItem("Ausreißer Rating", tabName = "xwr_outliers"),
               menuSubItem("Korrelation", tabName = "xwr_corr")
      ),
      
      menuItem("Datenqualität Kaggle Datenset (DQA)", 
               icon = icon("clipboard-check"),
               menuSubItem("Struktur", tabName = "kaggle_structure"),
               menuSubItem("Fehlende Werte", tabName = "kaggle_missing"),
               menuSubItem("Duplikate", tabName = "kaggle_dups"),
               menuSubItem("Histogramme", tabName = "kaggle_hist"),
               menuSubItem("Boxplots", tabName = "kaggle_boxplots"),
               menuSubItem("Ausreißer Rating Spalte Points", tabName = "kaggle_points_outliers"),
               menuSubItem("Ausreißer Rating Spalte Price", tabName = "kaggle_price_outliers"),
               menuSubItem("Korrelation", tabName = "kaggle_corr")
      )
      
    )
  ),
  
  dashboardBody(
    tabItems(
      # Xwines Data
      tabItem("xw_structure", tableOutput("xw_structure_table")),
      tabItem("xw_missing", tableOutput("xw_missing_table")),
      tabItem("xw_dups", textOutput("xw_dup_count")),
      tabItem("xw_hist", plotOutput("xw_hist_numeric")),
      tabItem("xw_boxplots", plotOutput("xw_box_numeric")),
      tabItem("xw_outliers", plotOutput("xw_outliers_abv")),
      tabItem("xw_corr", plotOutput("xw_corr_matrix")),
      
      # Xwines Ratings
      tabItem("xwr_structure", tableOutput("xwr_structure_table")),
      tabItem("xwr_missing", tableOutput("xwr_missing_table")),
      tabItem("xwr_dups", textOutput("xwr_dup_count")),
      tabItem("xwr_hist", plotOutput("xwr_hist_numeric")),
      tabItem("xwr_boxplots", plotOutput("xwr_box_numeric")),
      tabItem("xwr_outliers", plotOutput("xwr_outliers_abv_r")), 
      tabItem("xwr_corr", plotOutput("xwr_corr_matrix")),
      
      # Kaggle Dataset
      tabItem("kaggle_structure", tableOutput("kaggle_structure_table")),
      tabItem("kaggle_missing", tableOutput("kaggle_missing_table")),
      tabItem("kaggle_dups", textOutput("kaggle_dup_count")),
      tabItem("kaggle_hist", plotOutput("kaggle_hist_numeric")),
      tabItem("kaggle_boxplots", plotOutput("kaggle_box_numeric")),
      tabItem("kaggle_points_outliers", plotOutput("kaggle_points_outliers")),
      tabItem("kaggle_price_outliers", plotOutput("kaggle_price_outliers")),
      tabItem("kaggle_corr", plotOutput("kaggle_corr_matrix"))
    )
  )
)

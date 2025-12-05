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
      ),
      
      menuItem("Q1: Effects on prices and ratings", 
               #icon = icon("clipboard-check"),
               menuSubItem("Description", tabName = "q1_description"),
               menuSubItem("Correlation", tabName = "q1_correlation"),
               menuSubItem("Scatterplots", tabName = "q1_scatter"),
               menuSubItem("Categorical Plots", tabName = "q1_categ")
      )
      
    )
  ),
  
  dashboardBody(
    tags$style(HTML("
    #q1_select_cor_out {
      font-size: 20px;
      color: black;
      font-weight: bold;
      margin-top: 10px;
      margin-bottom: 10px;
      }
    ")),
    
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
      tabItem("kaggle_corr", plotOutput("kaggle_corr_matrix")),
      
      # Q1 - Salomes Teil
      tabItem(
        tabName = "q1_description",
        h2("How do alcohol content and acidity affect the rating as well as the price of a wine, and is 	this the same for red, white, or sparkling wines? "),
        p("The following dashboards are dedicated to analyze data on winetypes, content, prices and ratings,
          in order to develop a better understanding of the relationships between these variables."),
        p("Possible starting points for hypotheses:"),
        p("> There is a correlation between ratings and the prices at which the wines are sold."),
        p("> There is a correlation between acidity and price, as well as alcohol and price."),
        p("> Sparkling wines have lower acidity content than red or white wines. ")),

      tabItem(
        tabName = "q1_correlation", 
        h2("Analyze correlation between different variables"),
        p("Choose two variables and compute their correlation value"),
        
        # ---------------- ROW 1: Inputs left, correlation result right ----------------
        fluidRow(
          
          # Left: Inputs
          column(width = 6,
                 selectInput("q1_select_cor_1", label = "Choose variable 1", choices = q1_col_vec, selected = "price"),
                 selectInput("q1_select_cor_2", label = "Choose variable 2", choices = q1_col_vec, selected = "points")
          ),
          
          # Right: Correlation output
          column(width = 6,
                 tags$div(
                   id = "cor_output_box",
                   style = "
               background-color: white;
               border: 1px solid #ccc;
               padding: 15px;
               border-radius: 6px;
               font-size: 20px;
               font-weight: bold;
               margin-top: 25px;
             ",
                   textOutput("q1_select_cor_out")
                 )
          )
        ),
        # --- Divider between upper and lower row ---
        tags$hr(style = "margin-top: 20px; margin-bottom: 20px; border-top: 2px solid #999;"),
        
        # ---------------- ROW 2: Plot left, variable description right ----------------
        p("View correlation plot between all the variables"),
        fluidRow(
          
          # Plot
          column(width = 7,
                 plotOutput("q1_correlation_graph")
          ),
          
          # Variable description box
          column(width = 5,
                 tags$div(
                   style = "
               background-color: white;
               border: 1px solid #ccc;
               padding: 15px;
               border-radius: 6px;
               margin-top: 0px;
             ",
                   h4("Variable descriptions"),
                   p("ABV – Alcohol by Volume [% vol], percentage of pure alcohol relative to total volume."),
                   p("log_ABV – logarithmic transformation of ABV (dimensionless)."),
                   p("points – Rating score from 0 to 100."),
                   p("price – Price in [USD]."),
                   p("price_boxcox – Boxcox-transformed price (dimensionless)."),
                   p("price_inverse – 1 / price [1/USD].")
                 )
          )
        )
      ),
      
      tabItem(
        tabName = "q1_scatter",
        h2("Analyze relationships between different variables via scatterplots"),
        
        # ---------- ROW 1: Inputs ----------
        
        fluidRow(
          column(width = 4,
                 selectInput("q1_select_scatter_x", label = "Choose X variable", choices = q1_col_vec, selected = "price")
          ),
          column(width = 4,
                 selectInput("q1_select_scatter_y", label = "Choose Y variable", choices = q1_col_vec, selected = "points")
          ),
          column(width = 4,
                 selectInput("q1_select_scatter_color", label = "Color by", 
                             choices = q1_categ_cols, selected = "Acidity")
          )
        ),
        
        # Divider
        tags$hr(style = "margin-top: 20px; margin-bottom: 20px; border-top: 2px solid #999;"),
        
        # ---------- ROW 2: Plot + Description ----------
        p("View scatterplots for the selected variables. A regression line is fitted for numerical variables"),
        p("to assess whether a linear model may be suitable for describing their relationship."),
        fluidRow(
          column(width = 7,
                 plotOutput("q1_scatter_plot")
          ),
          
          column(width = 5,
                 tags$div(
                   style = "
               background-color: white;
               border: 1px solid #ccc;
               padding: 15px;
               border-radius: 6px;
               margin-top: 0px;
             ",
                   h4("Variable descriptions"),
                   p("ABV – Alcohol by Volume [% vol], percentage of pure alcohol relative to total volume."),
                   p("log_ABV – logarithmic transformation of ABV (dimensionless)."),
                   p("points – Rating score from 0 to 100."),
                   p("price – Price in [USD]."),
                   p("price_boxcox – Boxcox-transformed price (dimensionless)."),
                   p("price_inverse – 1 / price [1/USD].")
                 )
          )
        )
      ),
      tabItem(
        tabName = "q1_categ",
        h2("Analyze relationships between categorical and numerical variables"),
        
        # ---------- ROW 1: Inputs ----------
        fluidRow(
          column(width = 6,
                 selectInput("q1_select_categ_x",
                             label = "Choose X variable",
                             choices = q1_all_vars,
                             selected = "ABV")
          ),
          column(width = 6,
                 selectInput("q1_select_categ_y",
                             label = "Choose Y variable",
                             choices = q1_all_vars,
                             selected = "Type")
          )
        ),
        
        
        
        # Divider
        tags$hr(style = "margin-top: 20px; margin-bottom: 20px; border-top: 2px solid #999;"),
        
        p("Compare categorical und numerical variables with violin- and proportional barplot"),
        
        # ---------- ROW 2: Plots ----------
        fluidRow(
          column(width = 8,
                 plotOutput("q1_violin_plot")
          ),
          column(width = 4,
                 tags$div(
                   style = "
               background-color: white;
               border: 1px solid #ccc;
               padding: 15px;
               border-radius: 6px;
               margin-top: 0px;
             ",
                   h4("Variable descriptions"),
                   p("ABV – Alcohol by Volume [% vol], percentage of pure alcohol relative to total volume."),
                   p("log_ABV – logarithmic transformation of ABV (dimensionless)."),
                   p("points – Rating score from 0 to 100."),
                   p("price – Price in [USD]."),
                   p("price_boxcox – Boxcox-transformed price (dimensionless)."),
                   p("price_inverse – 1 / price [1/USD].")
                 )
          )
        ),
        
        tags$div(style = "height: 40px;"),
        
        # ---------- ROW 3: Variable Description ----------
        fluidRow(
          column(width = 12,
                 plotOutput("q1_prop_bar_plot")
                 
          )
        )
        
      )
      
    ) 
    )
  )

      
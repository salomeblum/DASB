dashboardPage(
  
  dashboardHeader(title = "Wine Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "DQA der Datensätze",
        icon = icon("clipboard-check"),
        menuSubItem("DQA Xwines Dataset", tabName = "dqa_xwines"),
        menuSubItem("DQA winemag Dataset", tabName = "dqa_winemag")
      ),
      
      menuItem(
        "Variable comparisons", 
        icon = icon("glass-cheers"),
        menuSubItem("Description", tabName = "q1_description"),
        menuSubItem("Correlation", tabName = "q1_correlation"),
        menuSubItem("Scatterplots", tabName = "q1_scatter"),
        menuSubItem("Categorical Plots", tabName = "q1_categ")
      ),
      
      menuItem("Europe vs America", tabName = "thesis", icon = icon("globe"))
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
      
      ## ---- TAB 1: DQA XWINES ----
      tabItem(
        tabName = "dqa_xwines",
        h2(
          "DQA of the Xwine Dataset",
          style = "text-align:center; color:#5a2d82; font-weight:bold; margin-bottom:30px;"
        ),
        
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Structure of the Data",
            p("The Structure of the Data is shown"),
            tableOutput("xw_structure_table")
          ),
          
          tabPanel(
            "Missing Values",
            p("This Tab shows a Table with the number of missing Values"),
            tableOutput("xw_missing_table")
          ),
          
          tabPanel(
            "Duplicates",
            p("This Tab shows if there are duplicates in the Dataset"),
            tableOutput("xw_dup_count")
          ),
          
          tabPanel(
            "Histograms",
            p("This Tab shows the histograms of the numeric Data"),
            plotOutput("xw_hist_numeric")
          ),
          
          tabPanel(
            "Boxplots",
            p("This Tab shows the boxplots of the numeric Data"),
            plotOutput("xw_box_numeric")
          ),
          
          tabPanel(
            "Outliers",
            p("This Tab shows the outliers of the numeric column ABV"),
            plotOutput("xw_outliers_abv")
          ),
          
          tabPanel(
            "Correlation Matrix",
            p("This Tab shows the correlations between the numeric Data"),
            plotOutput("xw_corr_matrix")
          )
        )
      ),
      
      ## ---- TAB 2: DQA WINEMAG ----
      tabItem(
        tabName = "dqa_winemag",
        h2(
          "DQA of the winemag Dataset",
          style = "text-align:center; color:#5a2d82; font-weight:bold; margin-bottom:30px;"
        ),
        
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Structure of the Data",
            p("The Structure of the Data is shown"),
            tableOutput("winemag_structure_table")
          ),
          
          tabPanel(
            "Missing Values",
            p("This Tab shows a Table with the number of missing Values"),
            tableOutput("winemag_missing_table")
          ),
          
          tabPanel(
            "Duplicates",
            p("This Tab shows if there are duplicates in the Dataset"),
            tableOutput("winemag_dup_count")
          ),
          
          tabPanel(
            "Histograms",
            p("This Tab shows the histograms of the numeric Data"),
            plotOutput("winemag_hist_numeric")
          ),
          
          tabPanel(
            "Boxplots",
            p("This Tab shows the boxplots of the numeric Data"),
            plotOutput("winemag_box_numeric")
          ),
          
          tabPanel(
            "Outliers Points",
            p("This Tab shows the outliers of the numeric column Points"),
            plotOutput("winemag_points_outliers")
          ),
          
          tabPanel(
            "Outliers Price",
            p("This Tab shows the outliers of the numeric column Price"),
            plotOutput("winemag_price_outliers")
          ),
          
          tabPanel(
            "Correlation Matrix",
            p("This Tab shows the correlations between the numeric Data"),
            plotOutput("winemag_corr_matrix")
          )
        )
      ),
      
      ## ---- Q1 - Salomes Teil: DESCRIPTION ----
      tabItem(
        tabName = "q1_description",
        h2("How do alcohol content and acidity affect the rating as well as the price of a wine, and is 	this the same for red, white, or sparkling wines? "),
        p("The following dashboards are dedicated to analyze data on winetypes, content, prices and ratings,
          in order to develop a better understanding of the relationships between these variables."),
        p("Possible starting points for hypotheses:"),
        p("> There is a correlation between ratings and the prices at which the wines are sold."),
        p("> There is a correlation between acidity and price, as well as alcohol and price."),
        p("> Sparkling wines have lower acidity content than red or white wines. ")
      ),
      
      ## ---- Q1: CORRELATION ----
      tabItem(
        tabName = "q1_correlation", 
        h2("Analyze correlation between different variables"),
        p("Choose two variables and compute their correlation value"),
        
        # ---------------- ROW 1: Inputs left, correlation result right ----------------
        fluidRow(
          
          # Left: Inputs
          column(
            width = 6,
            selectInput("q1_select_cor_1", label = "Choose variable 1", choices = q1_col_vec, selected = "price"),
            selectInput("q1_select_cor_2", label = "Choose variable 2", choices = q1_col_vec, selected = "points")
          ),
          
          # Right: Correlation output
          column(
            width = 6,
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
          column(
            width = 7,
            plotOutput("q1_correlation_graph")
          ),
          
          # Variable description box
          column(
            width = 5,
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
      
      ## ---- Q1: SCATTER ----
      tabItem(
        tabName = "q1_scatter",
        h2("Analyze relationships between different variables via scatterplots"),
        
        # ---------- ROW 1: Inputs ----------
        fluidRow(
          column(
            width = 4,
            selectInput("q1_select_scatter_x", label = "Choose X variable", choices = q1_col_vec, selected = "price")
          ),
          column(
            width = 4,
            selectInput("q1_select_scatter_y", label = "Choose Y variable", choices = q1_col_vec, selected = "points")
          ),
          column(
            width = 4,
            selectInput(
              "q1_select_scatter_color", label = "Color by", 
              choices = q1_categ_cols, selected = "Acidity"
            )
          )
        ),
        
        # Divider
        tags$hr(style = "margin-top: 20px; margin-bottom: 20px; border-top: 2px solid #999;"),
        
        # ---------- ROW 2: Plot + Description ----------
        p("View scatterplots for the selected variables. A regression line is fitted for numerical variables"),
        p("to assess whether a linear model may be suitable for describing their relationship."),
        fluidRow(
          column(
            width = 7,
            plotOutput("q1_scatter_plot")
          ),
          
          column(
            width = 5,
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
      
      ## ---- Q1: CATEGORICAL ----
      tabItem(
        tabName = "q1_categ",
        h2("Analyze relationships between categorical and numerical variables"),
        
        # ---------- ROW 1: Inputs ----------
        fluidRow(
          column(
            width = 6,
            selectInput(
              "q1_select_categ_x",
              label = "Choose X variable",
              choices = q1_all_vars,
              selected = "ABV"
            )
          ),
          column(
            width = 6,
            selectInput(
              "q1_select_categ_y",
              label = "Choose Y variable",
              choices = q1_all_vars,
              selected = "Type"
            )
          )
        ),
        
        # Divider
        tags$hr(style = "margin-top: 20px; margin-bottom: 20px; border-top: 2px solid #999;"),
        
        p("Compare categorical und numerical variables with violin- and proportional barplot"),
        
        # ---------- ROW 2: Plots ----------
        fluidRow(
          column(
            width = 8,
            plotOutput("q1_violin_plot")
          ),
          column(
            width = 4,
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
          column(
            width = 12,
            plotOutput("q1_prop_bar_plot")
          )
        )
      ),
      
      ## ---- THESIS TAB ----
      tabItem(
        tabName = "thesis",
        
        h2(
          "Wine Analysis - Interactive Comparison",
          style = "text-align:center; color:#5a2d82; font-weight:bold; margin-bottom:30px;"
        ),
        
        tabsetPanel(
          type = "tabs",
          
          # ----- Tab 1 -----
          tabPanel(
            "1. Variable Comparison",
            br(),
            p("In this tab, you can compare the values of difrent variables for European and American countries."),
            p("To do this, select a variable in the first selection field at the top and use the Y minimum and Y maximum fields to set the axes so that they show the desired value range."),
            p("If you select the “Acidity” variable, the scale is set from low to high. It is important to note that certain values are not displayed depending on the scaling of the Y-axis."),
            br(),
            fluidRow(
              column(
                4,
                selectInput(
                  "yVar", "Y-Axis Variable:",
                  choices = c(
                    "Price"   = "price",
                    "Rating"  = "points",
                    "ABV"     = "ABV",
                    "Acidity" = "Acidity"
                  ),
                  selected = "points"
                )
              ),
              column(4, numericInput("yMin", "Y Minimum:", value = 0, min = 0)),
              column(4, numericInput("yMax", "Y Maximum:", value = 100, min = 1))
            ),
            br(),
            fluidRow(
              column(
                6,
                box(
                  title = "Europe", status = "primary", solidHeader = TRUE,
                  plotOutput("bubble1", height = "550px"), width = 12
                )
              ),
              column(
                6,
                box(
                  title = "Americas", status = "success", solidHeader = TRUE,
                  plotOutput("bubble2", height = "550px"), width = 12
                )
              )
            )
          ),
          
          # ----- Tab 2 -----
          tabPanel(
            "2. Rating vs. Variable",
            br(), 
            p("In the second tab, you can compare different variables with the rating. The values are divided again into European and American countries."), 
            p("In the first selection field, you can select the desired variable and use the “Y minimum” and “Y maximum” buttons to set the scale accordingly."), 
            p("If you select the “Acidity” variable, the scale is set from low to high. Please note that the display of certain values depends on the scaling of the Y-axis."), 
            br(),
            fluidRow(
              column(
                4,
                selectInput(
                  "hexVar", "Y-Variable vs Rating:",
                  choices = c(
                    "Price"   = "price",
                    "ABV"     = "ABV",
                    "Acidity" = "Acidity"
                  ),
                  selected = "price"
                )
              ),
              column(4, numericInput("hexMin", "Y Minimum:", value = 0)),
              column(4, numericInput("hexMax", "Y Maximum:", value = 200))
            ),
            br(),
            fluidRow(
              column(
                6,
                box(
                  title = "Europe", status = "primary", solidHeader = TRUE,
                  plotOutput("hex1", height = "550px"), width = 12
                )
              ),
              column(
                6,
                box(
                  title = "Americas", status = "success", solidHeader = TRUE,
                  plotOutput("hex2", height = "550px"), width = 12
                )
              )
            )
          ),
          
          # ----- Tab 3 -----
          tabPanel(
            "3. Wine Type",
            br(),
            p("In the third tab, the variables can be compared by wine type. Once again, a distinction is made between European and American countries."),
            p("The variable is specified in the first field and the wine type to be compared in the second. Use the “Y minimum” and “Y maximum” buttons to set the scale accordingly."),
            p("If you select the “Acidity” variable, the scale is set from low to high.Please note that depending on how the scale is set, certain values may no longer be visible."),
            br(),
            fluidRow(
              column(
                3,
                selectInput(
                  "wineType", "Wine Type:",
                  choices = c("All", "Red", "White", "Sparkling", "Dessert/Port", "Port"),
                  selected = "All"
                )
              ),
              column(
                3,
                selectInput(
                  "typeYVar", "Y-Variable:",
                  choices = c(
                    "Rating"  = "points",
                    "Price"   = "price",
                    "ABV"     = "ABV",
                    "Acidity" = "Acidity"
                  ),
                  selected = "points"
                )
              ),
              column(3, numericInput("typeYMin", "Y Minimum:", value = 0)),
              column(3, numericInput("typeYMax", "Y Maximum:", value = 100))
            ),
            br(),
            fluidRow(
              column(
                6,
                box(
                  title = "Europe", status = "primary", solidHeader = TRUE,
                  plotOutput("bubble_type_europe", height = "550px"), width = 12
                )
              ),
              column(
                6,
                box(
                  title = "Americas", status = "success", solidHeader = TRUE,
                  plotOutput("bubble_type_americas", height = "550px"), width = 12
                )
              )
            )
          )
        )
      )
    )
  )
)

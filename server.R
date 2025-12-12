server <- function(input, output){
  
  # ---- DQA Outputs XWines ----
  output$xw_structure_table <- renderTable({ xw_structure_table })
  output$xw_missing_table   <- renderTable({ xw_missing_table })
  output$xw_dup_count       <- renderText({ paste("Duplikate:", xw_dup_count) })
  
  output$xw_hist_numeric <- renderPlot({
    ggplot(xw_numeric_long, aes(x = Wert)) +
      geom_histogram(bins = 50, fill = "steelblue", color = "black") +
      facet_wrap(~Variable, scales = "free", ncol = 2)
  })
  
  output$xw_box_numeric <- renderPlot({
    ggplot(xw_box_numeric, aes(x = Variable, y = Wert)) +
      geom_boxplot(outlier.color = "red", fill = "steelblue", color = "black", alpha = 0.7) +
      facet_wrap(~Variable, scales = "free", ncol = 2)
  })
  
  output$xw_outliers_abv <- renderPlot({
    ggplot(xw_outliers_ABV, aes(x = seq_along(ABV), y = ABV)) +
      geom_point(color = "red") +
      geom_hline(yintercept = xw_lower_limit, color = "blue", linetype = "dashed") +
      geom_hline(yintercept = xw_upper_limit, color = "blue", linetype = "dashed")
  })
  
  output$xw_corr_matrix <- renderPlot({
    corrplot(xw_cor_matrix, method = "color", type = "upper",
             tl.col = "black", number.cex = 0.6)
  })
  
  
  # ---- DQA Outputs winemag ----
  output$winemag_structure_table <- renderTable({ winemag_structure_table })
  output$winemag_missing_table   <- renderTable({ winemag_missing_table })
  output$winemag_dup_count       <- renderText({ paste("Duplikate:", winemag_dup_count) })
  
  output$winemag_hist_numeric <- renderPlot({
    ggplot(winemag_numeric_long, aes(x = Wert)) +
      geom_histogram(bins = 50, fill = "steelblue", color = "black") +
      facet_wrap(~Variable, scales = "free", ncol = 2)
  })
  
  output$winemag_box_numeric <- renderPlot({
    ggplot(winemag_box_numeric, aes(x = Variable, y = Wert)) +
      geom_boxplot(outlier.color = "red", fill = "steelblue", color = "black", alpha = 0.7) +
      facet_wrap(~Variable, scales = "free", ncol = 2)
  })
  
  output$winemag_points_outliers <- renderPlot({
    ggplot(winemag_points_outliers_Rating, aes(x = seq_along(points), y = points)) +
      geom_point(color = "red") +
      geom_hline(yintercept = winemag_points_lower_limit, color = "blue", linetype = "dashed") +
      geom_hline(yintercept = winemag_points_upper_limit, color = "blue", linetype = "dashed")
  })
  
  output$winemag_price_outliers <- renderPlot({
    ggplot(winemag_price_outliers_Rating, aes(x = seq_along(price), y = price)) +
      geom_point(color = "red") +
      geom_hline(yintercept = winemag_price_lower_limit, color = "blue", linetype = "dashed") +
      geom_hline(yintercept = winemag_price_upper_limit, color = "blue", linetype = "dashed")
  })
  
  output$winemag_corr_matrix <- renderPlot({
    corrplot(winemag_cor_matrix, method = "color", type = "upper",
             tl.col = "black", number.cex = 0.6)
  })
  

  # ---- Outputs Q1 - Salomes Teil ----
  
  # ---- Correlations
  #Show correlation value of two selected variables
  output$q1_select_cor_out <- renderText({
    
    var1 <- input$q1_select_cor_1
    var2 <- input$q1_select_cor_2
    
    # if no variables were selected
    if (is.null(var1) || is.null(var2)) {
      return("Please choose two variables.")
    }
    
    # compute correlation value
    cor_value <- cor(
      q1_num_joined_data[[var1]],
      q1_num_joined_data[[var2]],
      use = "complete.obs"
    )
    
    paste("Correlation value:", round(cor_value, 3))
  })
  
  output$q1_correlation_graph <- renderPlot({
    corrplot(q1_joined_cor_matrix, method = "color", type = "upper",
             tl.col = "black", number.cex = 0.6)
  })
  
  # ---- Scatterplots
  #Show Scatterplots of two selected variables
  
  #Linear model - Reactive
  model_fit <- reactive({
    
    var_x <- input$q1_select_scatter_x
    var_y <- input$q1_select_scatter_y
    
    req(
      nzchar(var_x), nzchar(var_y),
      var_x %in% names(joined_data),
      var_y %in% names(joined_data),
      is.numeric(joined_data[[var_x]]),
      is.numeric(joined_data[[var_y]])
    )
    
    lm(
      joined_data[[var_y]] ~ joined_data[[var_x]]
    )
  })
  
  
  
  output$q1_scatter_plot <- renderPlot({
    
    fit <- model_fit()
    
    var_x <- input$q1_select_scatter_x
    var_y <- input$q1_select_scatter_y
    var_color <- input$q1_select_scatter_color
    
    req(
      nzchar(var_x), nzchar(var_y),
      var_x %in% names(joined_data),
      var_y %in% names(joined_data)
    )
    
    p <- ggplot(
      joined_data,
      aes(x = .data[[var_x]], y = .data[[var_y]])
    )
    
    if (nzchar(var_color) && var_color %in% names(joined_data)) {
      p <- p + geom_point(aes(color = .data[[var_color]]), alpha = 0.6)
    } else {
      p <- p + geom_point(color = "#2C3E50", alpha = 0.6)
    }
    
    p +
      geom_abline(
        intercept = coef(fit)[1],
        slope = coef(fit)[2],
        color = "red",
        linewidth = 1
      ) +
      theme_minimal(base_size = 14) +
      labs(
        x = var_x,
        y = var_y,
        title = paste("Scatterplot:", var_x, "vs", var_y)
      )
  })
  
  
  output$q1_rss <- renderText({
    
    fit <- model_fit()
    paste("RSS:", round(deviance(fit), 2))
  })
  
  
  # ---- Categorical Plots
  #Violinplot
  output$q1_violin_plot <- renderPlot({
    var_x <- input$q1_select_categ_x
    var_y <- input$q1_select_categ_y
    
    ggplot(joined_data, aes_string(x = var_x, y = var_y)) +
      geom_violin(fill = "skyblue", alpha = 0.6) +
      geom_boxplot(width = 0.1, outlier.color = "red") +
      theme_minimal() +
      labs(
        title = paste("Violinplot:", var_x, "by", "Acidity"),
        x = var_x,
        y = var_y
      )
  })
  
  output$q1_prop_bar_plot <- renderPlot({
    var_x <- input$q1_select_categ_x
    var_y <- input$q1_select_categ_y
    
    df <- na.omit(joined_data[, c(var_x, var_y)])
    
    ggplot(df, aes_string(x = var_x, fill = var_y)) +
      geom_bar(position = "fill") +
      scale_y_continuous(labels = scales::percent) +
      theme_minimal() +
      labs(
        title = paste("Proportional Bar Plot:", var_x, "vs", var_y),
        x = var_x,
        fill = var_y
      )
  })

  # --- Teil Marlon ---
  europe <- c("Austria","France","Germany","Italy","Spain","Switzerland","Portugal",
              "Greece","Romania","Hungary","Slovenia","Croatia","Bulgaria",
              "England","Wales","Slovakia","Serbia","Georgia","Moldova","Luxembourg")
  
  americas <- c("United States","Canada","Mexico","Argentina","Chile","Uruguay","Brazil","Peru")
  
  europeDS <- reactive({ joinedWines %>% filter(country_clean %in% europe) })
  americasDS <- reactive({ joinedWines %>% filter(country_clean %in% americas) })
  
  europeFiltered <- reactive({
    df <- europeDS()
    if(input$wineType != "All") df <- df %>% filter(Type == input$wineType)
    df
  })
  americasFiltered <- reactive({
    df <- americasDS()
    if(input$wineType != "All") df <- df %>% filter(Type == input$wineType)
    df
  })
  
  prep <- function(df, yvar) {
    df %>%
      group_by(country_clean, .data[[yvar]]) %>%
      summarise(count = n(), .groups = "drop") %>%
      rename(y = .data[[yvar]])
  }
  
  
  # TAB 1 – EUROPE BUBBLE
  output$bubble1 <- renderPlot({
    req(input$yMax > input$yMin)
    df <- prep(europeDS(), input$yVar)
    
    if (input$yVar == "Acidity") {
      ggplot(df, aes(x = country_clean, y = y, size = count, color = country_clean)) +
        geom_point(alpha = 0.8) +
        scale_size_continuous(range = c(5, 22)) +
        scale_y_discrete(limits = c("Low", "Medium", "High")) +
        theme_minimal(base_size = 14) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "right"
        ) +
        guides(color = "none")
    }
    
    else {
      ggplot(df, aes(x = country_clean, y = y, size = count, color = country_clean)) +
        geom_point(alpha = 0.8) +
        scale_size_continuous(range = c(5, 22)) +
        scale_y_continuous(limits = c(input$yMin, input$yMax)) +
        theme_minimal(base_size = 14) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "right"
        ) +
        guides(color = "none")
    }
  })
  
  
  # TAB 1 – AMERICAS BUBBLE
  output$bubble2 <- renderPlot({
    req(input$yMax > input$yMin)
    df <- prep(americasDS(), input$yVar)
    
    if (input$yVar == "Acidity") {
      ggplot(df, aes(x = country_clean, y = y, size = count, color = country_clean)) +
        geom_point(alpha = 0.8) +
        scale_size_continuous(range = c(5, 22)) +
        scale_y_discrete(limits = c("Low", "Medium", "High")) +
        theme_minimal(base_size = 14) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "right"
        ) +
        guides(color = "none")
    } 
    
    else {
      ggplot(df, aes(x = country_clean, y = y, size = count, color = country_clean)) +
        geom_point(alpha = 0.8) +
        scale_size_continuous(range = c(5, 22)) +
        scale_y_continuous(limits = c(input$yMin, input$yMax)) +
        theme_minimal(base_size = 14) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "right"
        ) +
        guides(color = "none")
    }
  })
  
  
  # TAB 2 – HEX EUROPE
  output$hex1 <- renderPlot({
    req(input$hexMax > input$hexMin)
    df <- europeDS()
    
    if (input$hexVar == "Acidity") {
      ggplot(df, aes(x = points, y = Acidity)) +
        geom_hex(bins = 40) +
        scale_fill_viridis_c() +
        theme_minimal(base_size = 14)
    } 
    
    else {
      ggplot(df, aes(x = points, y = .data[[input$hexVar]])) +
        geom_hex(bins = 40) +
        scale_fill_viridis_c() +
        scale_y_continuous(limits = c(input$hexMin, input$hexMax)) +
        theme_minimal(base_size = 14)
    }
  })
  
  # TAB 2 – HEX AMERICAS
  output$hex2 <- renderPlot({
    req(input$hexMax > input$hexMin)
    df <- americasDS()
    
    if (input$hexVar == "Acidity") {
      ggplot(df, aes(x = points, y = Acidity)) +
        geom_hex(bins = 40) +
        scale_fill_viridis_c() +
        theme_minimal(base_size = 14)
    } 
    
    else {
      ggplot(df, aes(x = points, y = .data[[input$hexVar]])) +
        geom_hex(bins = 40) +
        scale_fill_viridis_c() +
        scale_y_continuous(limits = c(input$hexMin, input$hexMax)) +
        theme_minimal(base_size = 14)
    }
  })
  
  
  # TAB 3 – BUBBLE WINE TYPE EUROPE
  output$bubble_type_europe <- renderPlot({
    req(input$typeYMax > input$typeYMin)
    df <- prep(europeFiltered(), input$typeYVar)
    
    if (input$typeYVar == "Acidity") {
      ggplot(df, aes(x = country_clean, y = y, size = count, color = country_clean)) +
        geom_point(alpha = 0.8) +
        scale_size_continuous(range = c(5, 25)) +
        scale_y_discrete(limits = c("Low", "Medium", "High")) +
        theme_minimal(base_size = 14) + 
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "right"
        ) +
        guides(color = "none") + 
        labs(x = NULL, y = input$typeYVar)
    } 
    
    else {
      ggplot(df, aes(x = country_clean, y = y, size = count, color = country_clean)) +
        geom_point(alpha = 0.8) +
        scale_size_continuous(range = c(5, 25)) +
        scale_y_continuous(limits = c(input$typeYMin, input$typeYMax)) +
        theme_minimal(base_size = 14) + 
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "right"
        ) +
        guides(color = "none") + 
        labs(x = NULL, y = input$typeYVar)
    }
  })
  
  
  # TAB 3 – BUBBLE WINE TYPE AMERICAS
  output$bubble_type_americas <- renderPlot({
    req(input$typeYMax > input$typeYMin)
    df <- prep(americasFiltered(), input$typeYVar)
    
    if (input$typeYVar == "Acidity") {
      ggplot(df, aes(x = country_clean, y = y, size = count, color = country_clean)) +
        geom_point(alpha = 0.8) +
        scale_size_continuous(range = c(5, 25)) +
        scale_y_discrete(limits = c("Low", "Medium", "High")) +
        theme_minimal(base_size = 14) + 
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "right"
        ) +
        guides(color = "none") + 
        labs(x = NULL, y = input$typeYVar)
    } 
    
    else {
      ggplot(df, aes(x = country_clean, y = y, size = count, color = country_clean)) +
        geom_point(alpha = 0.8) +
        scale_size_continuous(range = c(5, 25)) +
        scale_y_continuous(limits = c(input$typeYMin, input$typeYMax)) +
        theme_minimal(base_size = 14) + 
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "right"
        ) +
        guides(color = "none") + 
        labs(x = NULL, y = input$typeYVar)
    }
  })
  
}

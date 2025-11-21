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
  
  
  # ---- DQA Outputs XWines Ratings ----
  output$xwr_structure_table <- renderTable({ xwr_structure_table })
  output$xwr_missing_table   <- renderTable({ xwr_missing_table })
  output$xwr_dup_count       <- renderText({ paste("Duplikate:", xwr_dup_count) })
  
  output$xwr_hist_numeric <- renderPlot({
    ggplot(xwr_numeric_long, aes(x = Wert)) +
      geom_histogram(bins = 50, fill = "steelblue", color = "black") +
      facet_wrap(~Variable, scales = "free", ncol = 2)
  })
  
  output$xwr_box_numeric <- renderPlot({
    ggplot(xwr_box_numeric, aes(x = Variable, y = Wert)) +
      geom_boxplot(outlier.color = "red", fill = "steelblue", color = "black", alpha = 0.7) +
      facet_wrap(~Variable, scales = "free", ncol = 2)
  })
  
  output$xwr_outliers_abv_r <- renderPlot({
    ggplot(xwr_outliers_Rating, aes(x = seq_along(Rating), y = Rating)) +
      geom_point(color = "red") +
      geom_hline(yintercept = xwr_lower_limit, color = "blue", linetype = "dashed") +
      geom_hline(yintercept = xwr_upper_limit, color = "blue", linetype = "dashed")
  })
  
  output$xwr_corr_matrix <- renderPlot({
    corrplot(xwr_cor_matrix, method = "color", type = "upper",
             tl.col = "black", number.cex = 0.6)
  })
  
  # ---- DQA Outputs Kaggle ----
  output$kaggle_structure_table <- renderTable({ kaggle_structure_table })
  output$kaggle_missing_table   <- renderTable({ kaggle_missing_table })
  output$kaggle_dup_count       <- renderText({ paste("Duplikate:", kaggle_dup_count) })
  
  output$kaggle_hist_numeric <- renderPlot({
    ggplot(xwr_numeric_long, aes(x = Wert)) +
      geom_histogram(bins = 50, fill = "steelblue", color = "black") +
      facet_wrap(~Variable, scales = "free", ncol = 2)
  })
  
  output$kaggle_box_numeric <- renderPlot({
    ggplot(xwr_box_numeric, aes(x = Variable, y = Wert)) +
      geom_boxplot(outlier.color = "red", fill = "steelblue", color = "black", alpha = 0.7) +
      facet_wrap(~Variable, scales = "free", ncol = 2)
  })
  
  output$kaggle_points_outliers <- renderPlot({
    ggplot(kaggle_points_outliers_Rating, aes(x = seq_along(Rating), y = Rating)) +
      geom_point(color = "red") +
      geom_hline(yintercept = xwr_lower_limit, color = "blue", linetype = "dashed") +
      geom_hline(yintercept = xwr_upper_limit, color = "blue", linetype = "dashed")
  })
  
  output$kaggle_price_outliers <- renderPlot({
    ggplot(kaggle_price_outliers_Rating, aes(x = seq_along(Rating), y = Rating)) +
      geom_point(color = "red") +
      geom_hline(yintercept = xwr_lower_limit, color = "blue", linetype = "dashed") +
      geom_hline(yintercept = xwr_upper_limit, color = "blue", linetype = "dashed")
  })
  
  output$kaggle_corr_matrix <- renderPlot({
    corrplot(kaggle_cor_matrix, method = "color", type = "upper",
             tl.col = "black", number.cex = 0.6)
  })
  
}

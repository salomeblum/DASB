function(input, output) {
  
  filtered_rows <- reactive({
    req(input$country)
    wine |>
      filter(country == input$country, !is.na(points), !is.na(price))
    })
  output$boxpl <- renderPlot({
    ggplot(
      filtered_rows(), 
      aes(x=points, y = price, color=price)
      )+
    geom_point(alpha = 0.4) +
    labs(title = paste("Rating vs. Price —", input$country),
         x = "Rating (points)", 
         y = "Price (USD)"
         ) +
    theme_minimal()
  })
}
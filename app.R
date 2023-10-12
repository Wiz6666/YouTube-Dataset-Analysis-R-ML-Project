ui_double_roc_plot <- fluidPage(
  # Panel for user to select variables
  sidebarLayout(
    sidebarPanel(
      selectInput("var1", "Choose variable for ROC 1:", independent_variables),
      selectInput("var2", "Choose variable for ROC 2:", independent_variables)
    ),
    # Display the ROC plots
    mainPanel(
      plotOutput("rocPlot1"),
      plotOutput("rocPlot2")
    )
  )
)

server_double_roc_plot <- function(input, output) {
  
  output$rocPlot1 <- renderPlot({
    var_to_plot <- input$var1
    prediction_probs <- Single_variable_model(target_column = youtube_train$earning_class, 
                                              feature_column = youtube_train[, var_to_plot],
                                              prediction_column = youtube_train[, var_to_plot])
    plot_roc(prediction_probs, youtube_train$earning_class)
  })
  
  output$rocPlot2 <- renderPlot({
    var_to_plot <- input$var2
    prediction_probs <- Single_variable_model(target_column = youtube_train$earning_class, 
                                              feature_column = youtube_train[, var_to_plot],
                                              prediction_column = youtube_train[, var_to_plot])
    plot_roc(prediction_probs, youtube_train$earning_class)
  })
  
}

#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# 假设你已经有了模型的预测结果和数据集
# 示例数据 - 在实际情况中，你应该使用你的实际数据和预测结果
weather_rf_train <- data.frame(
  Date = seq(as.Date('2021-01-01'), by = 'month', length.out = 12),
  Daily_Rainfall_Total_mm = runif(12, 0, 200)
)
predictions_test <- runif(12, 0, 200)
weather_rf_test <- data.frame(
  Date = seq(as.Date('2021-01-01'), by = 'month', length.out = 12),
  Daily_Rainfall_Total_mm = runif(12, 0, 200)
)

ui <- fluidPage(
  titlePanel("Time Series Forecast of Rainfall"),
  mainPanel(
    plotOutput("timeSeriesPlot") # 这将显示图表
  )
)

server <- function(input, output) {
  output$timeSeriesPlot <- renderPlot({
    # 在此处包含你的表3的代码来准备数据和绘制图表
    # 确保predictions_test是一个向量，且长度与weather_rf_test中的行数相同
    results <- data.frame(
      Date = weather_rf_test$Date, 
      Predicted = predictions_test, 
      Actual = weather_rf_test$Daily_Rainfall_Total_mm
    )
    
    # 绘制时间序列图
    p_time_series <- ggplot() +
      geom_line(data = weather_rf_train, aes(x = Date, y = Daily_Rainfall_Total_mm), color = "green", size = 0.7) +
      geom_line(data = results, aes(x = Date, y = Predicted), color = "blue", linetype = "dotted", size = 0.7) +
      labs(y = "Rainfall (mm)", x = "Date", title = "Time Series of Rainfall (Actual in green, Predicted in blue)") +
      theme_bw()
    
    p_time_series # 这将在UI中渲染图表
  })
}

# 运行应用程序
shinyApp(ui = ui, server = server)

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
)

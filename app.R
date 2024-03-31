#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
pacman::p_load(shiny,shinydashboard,plotly,ggplot2,ggstatsplot,dplyr,bslib)

#READ DATA

weather_data_imputed <- read.csv('data/weather_data_imputed.csv')
rainfall_year <- read.csv('data/rainfall_year.csv')
rf_data_month <- read.csv('data/rf_data_month.csv')
temp_month <- read.csv('data/temp_month.csv')
temp_year1 <- read.csv('data/temp_year1.csv')

### Dashboard Header 
header <- dashboardHeader(title = h3('ISSS608 VAA Grp Project - Weather.SG'), titleWidth = 300)

### Body

ui <- dashboardPage(
  dashboardHeader(title = h3("ISSS608 VAA Grp Project - Weather.SG"), titleWidth = 300),
  dashboardSidebar(
    sidebarMenu(
      menuItem("EDA", tabName = "eda", icon = icon("chart-line"),
               menuSubItem("Time Series Line Graph", tabName = "ts_line_graph"),
               menuSubItem("Horizon Plot", tabName = "horizon_plot"),
               menuSubItem("Correlation", tabName = "correlation"),
               menuSubItem("Box Plot", tabName = "box_plot")
      ),
      menuItem("CDA", tabName = "cda", icon = icon("project-diagram"),
               menuSubItem("Hypothesis 1", tabName = "hypo1"),
               menuSubItem("Hypothesis 2", tabName = "hypo2")
      ),
      menuItem("Cluster", tabName = "cluster", icon = icon("braille")),
      menuItem("Weather Forecast", tabName = "forecast", icon = icon("cloud-sun-rain"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "ts_line_graph",
              fluidPage(
                titlePanel("Visualising Weather Data"),
                sidebarLayout(
                  sidebarPanel(
                    sliderInput("yearRange", "Select Year Range:", min = 2014, max = 2023, value = c(2014, 2023), step = 1),
                    selectInput("yAxis", "Select Y-Axis Data:",
                                choices = list("Rainfall (mm)" = "Daily_Rainfall_Total_mm",
                                               "Mean Temperature" = "Mean_Temperature",
                                               "Max Temperature" = "Max_Temperature",
                                               "Min Temperature" = "Min_Temperature"),
                                selected = "Daily_Rainfall_Total_mm")
                  ),
                  mainPanel(
                    title = "Weather Data Visualization",
                    plotlyOutput("weatherPlot", height = "800px", width = "800px")
                  )
                )
              )
      ),
      # Define other tabItems here
      tabItem(tabName = "horizon_plot",
              fluidPage(
                titlePanel("Visualising Weather Data"),
                sidebarLayout(
                  sidebarPanel(
                    sliderInput("yearRange", "Select Year Range:", min = 2014, max = 2023, value = c(2014, 2023), step = 1),
                    selectInput("yAxis", "Select Y-Axis Data:",
                                choices = list("Rainfall (mm)" = "Daily_Rainfall_Total_mm",
                                               "Mean Temperature" = "Mean_Temperature",
                                               "Max Temperature" = "Max_Temperature",
                                               "Min Temperature" = "Min_Temperature"),
                                selected = "Daily_Rainfall_Total_mm")
                  ),
                  mainPanel(
                    title = "Weather Data Visualization",
                    plotlyOutput("weatherPlot", height = "800px", width = "800px")
                  )
                )
              )
      ),
      tabItem(tabName = "correlation",
              fluidPage(
                titlePanel("Visualising Weather Data"),
                sidebarLayout(
                  sidebarPanel(
                    sliderInput("yearRange", "Select Year Range:", min = 2014, max = 2023, value = c(2014, 2023), step = 1),
                    selectInput("yAxis", "Select Y-Axis Data:",
                                choices = list("Rainfall (mm)" = "Daily_Rainfall_Total_mm",
                                               "Mean Temperature" = "Mean_Temperature",
                                               "Max Temperature" = "Max_Temperature",
                                               "Min Temperature" = "Min_Temperature"),
                                selected = "Daily_Rainfall_Total_mm")
                  ),
                  mainPanel(
                    title = "Weather Data Visualization",
                    plotOutput("weatherPlot", height = "800px", width = "800px")
                  )
                )
              )
      ),
      tabItem(tabName = "box_plot",
              fluidPage(
                titlePanel("Visualising Weather Data"),
                sidebarLayout(
                  sidebarPanel(
                    sliderInput("yearRange", "Select Year Range:", min = 2014, max = 2023, value = c(2014, 2023), step = 1),
                    selectInput("yAxis", "Select Y-Axis Data:",
                                choices = list("Rainfall (mm)" = "Daily_Rainfall_Total_mm",
                                               "Mean Temperature" = "Mean_Temperature",
                                               "Max Temperature" = "Max_Temperature",
                                               "Min Temperature" = "Min_Temperature"),
                                selected = "Daily_Rainfall_Total_mm")
                  ),
                  mainPanel(
                    title = "Weather Data Visualization",
                    plotOutput("weatherPlot", height = "800px", width = "800px")
                  )
                )
              )
      )
    )
  )
)

# Define server logic required
server <- function(input, output) {
  #这是一个图 你删了弄你的就行
  output$weatherPlot <- renderPlotly({
    # Assuming 'weather_data_imputed' is correctly loaded and available in this environment
    filtered_data <- weather_data_imputed %>%
      filter(Year >= input$yearRange[1], Year <= input$yearRange[2])
    
    y_data_column <- input$yAxis
    
    p <- ggplot(data = filtered_data, aes_string(x = "Year", y = y_data_column)) +
      geom_line() + 
      xlab("Year") + 
      ylab(switch(y_data_column,
                  "Daily_Rainfall_Total_mm" = "Daily Rainfall Total (mm)",
                  "Mean_Temperature" = "Mean Temperature",
                  "Max_Temperature" = "Max Temperature",
                  "Min_Temperature" = "Min Temperature")) +
      theme_minimal()
    
    ggplotly(p)
  })
  #又一个图
  output$correlationPlot <- renderPlot({
    ggstatsplot::ggcorrmat(data = weather_data_imputed, type = "continuous")
  })
  #又一个
  output$boxPlot <- renderPlotly({
    data_2023 <- filter(weather_data_imputed, Year == 2023)
    p1 <- ggplot(data_2023 %>% filter(Station == "Changi"),
                 aes(y = Mean_Temperature, x = Month_Name)) +
      geom_boxplot() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
      labs(title = "Mean Temperature by Month for Changi Station, 2023",
           y = "Mean Temperature (°C)", x = "Month")
    ggplotly(p1)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

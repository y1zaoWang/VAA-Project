pacman::p_load(shiny,shinydashboard,plotly,ggplot2,ggstatsplot,dplyr,bslib,RColorBrewer,feasts,randomForest)

#READ DATA

weather_data_imputed <- read.csv('data/weather_data_imputed.csv')
rainfall_year <- read.csv('data/rainfall_year.csv')
rf_data_month <- read.csv('data/rf_data_month.csv')
temp_month <- read.csv('data/temp_month.csv')
temp_year1 <- read.csv('data/temp_year1.csv')
weather <- read.csv("data/weather_imputed_11stations.csv")

# Combining datasets by 'Year' and 'Month'
combined_data_month <- merge(temp_month, rf_data_month, by = c("Year", "Month"), all = TRUE)

# Renaming columns for consistency with UI choices
combined_data_month <- combined_data_month %>%
  rename(
    Daily_Rainfall_Total_mm = monthly_rainfall,
    Mean_Temperature = median_mean_temp,
    Max_Temperature = median_max_temp,
    Min_Temperature = median_min_temp
  )

combined_data_month$Year <- factor(combined_data_month$Year)

color_palette <- brewer.pal("Set3", n = length(unique(combined_data_month$Year)))

named_color_palette <- setNames(color_palette, levels(combined_data_month$Year))
### Dashboard Header 
header <- dashboardHeader(title = h3('ISSS608 VAA Grp Project - Weather.SG'), titleWidth = 300)

### Body

ui <- dashboardPage(
  dashboardHeader(title = h3("ISSS608 VAA Grp Project - Weather.SG"), titleWidth = 300),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Explore Stations", tabName = "EDA1", icon = icon("thermometer-half")),
      menuItem("Monthly Weather", tabName = "EDA2", icon = icon("chart-line")),
      menuItem("CDA", tabName = "cda", icon = icon("project-diagram")),
      menuItem("Correlation", tabName = "correlation", icon = icon("chart-area")),
      menuItem("Cluster", tabName = "cluster", icon = icon("braille")),
      menuItem("Cluster Analysis", tabName = "cluster_analysis", icon = icon("calendar-alt")),
      menuItem("Weather Forecast", tabName = "forecast", icon = icon("cloud-sun-rain"))
    )
  ),
  dashboardBody(
    tabItems(
      # EDA2 Tab
      tabItem(tabName = "EDA1",
              fluidPage(
                titlePanel("Temperature and Rainfall Visualization"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("eda_selectedStation", "Select Station:",
                                choices = unique(weather_data_imputed$Station), multiple = T,
                                selected="Changi"),
                    selectInput("eda_xAxisType", "Select X-axis Type:",
                                choices = list("Year" = "Year", "Month" = "Month")),
                    selectInput("eda_yAxisData", "Select Y-Axis Data:",
                                choices = list("Rainfall (mm)" = "Daily_Rainfall_Total_mm",
                                               "Mean Temperature" = "Mean_Temperature",
                                               "Max Temperature" = "Max_Temperature",
                                               "Min Temperature" = "Min_Temperature"),
                                selected = ("Mean_Temperature"))
                  ),
                  mainPanel(
                    plotlyOutput("tempPlot", height = "800px", width = "100%")
                  )
                )
              )),
      tabItem(tabName = "EDA2",
              fluidPage(
                titlePanel("Distribution of Monthly Mean Temperature"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("eda3_yAxisData", "Select Y-Axis Data:",
                                choices = list("Rainfall (mm)" = "Daily_Rainfall_Total_mm",
                                               "Mean Temperature" = "Mean_Temperature",
                                               "Max Temperature" = "Max_Temperature",
                                               "Min Temperature" = "Min_Temperature"),
                                selected = "Mean_Temperature"),
                    selectInput("eda3_plotType", "Select Plot Type:",
                                choices = list("Line" = "line",
                                               "Bar" = "bar"),
                                selected = "line")
                  ),
                  mainPanel(
                    plotlyOutput("monthlyTempPlot", height = "800px", width = "100%")
                  )
                )
              )
      ),
      # CDA Tab
      tabItem(tabName = "cda",
              fluidPage(
                titlePanel("Rainfall Statistics Visualization"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("cda_selectedStation", "Select Station:",
                                choices = c("All" = "All", unique(weather_data_imputed$Station)),
                                selected = "All"),
                    selectInput("cda_xAxisType", "Select X-axis Type:",
                                choices = list("Year" = "Year", "Month" = "Month")),
                    selectInput("cda_yAxisData", "Select Y-Axis Data:",
                                choices = list("Rainfall (mm)" = "Daily_Rainfall_Total_mm",
                                               "Mean Temperature" = "Mean_Temperature",
                                               "Max Temperature" = "Max_Temperature",
                                               "Min Temperature" = "Min_Temperature"),
                                selected = "Daily_Rainfall_Total_mm"),
                    selectInput("cda_statTest", "Statistical Test",
                                choices = c("Parametric ANOVA" = "anova",
                                            "Non-parametric Kruskal-Wallis" = "kruskal.test"),
                                selected = "kruskal.test"),
                    selectInput("cda_plotType", "Plot Type",
                                choices = list("Boxplot" = "boxplot",
                                               "Violin" = "violin",
                                               "Dotplot" = "dotplot")),
                    selectInput("cda_confLevel", "Confidence Level",
                                choices = list("95%" = 0.95, "99%" = 0.99),
                                selected = 0.95)
                  ),
                  mainPanel(
                    plotOutput("statsPlot", height = "900px", width = "100%")
                  )
                )
              )),
      # Correlation Tab
      tabItem(tabName = "correlation",
              fluidPage(
                titlePanel("Correlation Matrix"),
                sidebarLayout(
                  sidebarPanel(
                    checkboxGroupInput("cor_vars", "Select Variables:",
                                       choices = names(weather_data_imputed)[sapply(weather_data_imputed, is.numeric)],
                                       selected = c("Daily_Rainfall_Total_mm", "Mean_Temperature", "Max_Temperature", "Min_Temperature"))
                  ),
                  mainPanel(
                    plotOutput("corrPlot")
                  )
                )
              )
      ),
      # Cluster Tab
      tabItem(tabName = "cluster",
              fluidPage(
                titlePanel("Monthly Rainfall by Station"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("selectedStations", "Choose Stations:", 
                                choices = c("Admiralty", "Ang Mo Kio", "Changi"), multiple = TRUE)
                  ),
                  mainPanel(
                    plotOutput("rainfallPlot")
                  )
                )
              )
      ),
      tabItem(tabName = "cluster_analysis",
              fluidPage(
                titlePanel("Seasonal Plot by Week"),
                sidebarLayout(
                  sidebarPanel(
                    # Add input controls if needed, such as a dropdown to select stations
                    selectInput("selectedStationsSeasonal", "Choose Stations:", 
                                choices = unique(weather_data_imputed$Station), multiple = TRUE,
                                selected = unique(weather_data_imputed$Station)[1]),
                    # More input controls can be added here if necessary
                  ),
                  mainPanel(
                    # Output container for the seasonal plot
                    plotOutput("seasonalPlotOutput")
                  )
                )
              )
      ),
      tabItem(tabName = "forecast",
              fluidPage(
                titlePanel("Weather Forecast Analysis"),
                sidebarLayout(
                  sidebarPanel(
                    numericInput("ntree", "Number of Trees:", 500),
                    numericInput("mtry", "Number of Variables Tried at Each Split:", 3),
                    numericInput("nodesize", "Minimum Size of Terminal Nodes:", 5),
                    actionButton("runModel", "Run Model")
                  ),
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Actual vs Predicted", plotOutput("plotActualVsPredicted")),
                      tabPanel("Residuals", plotOutput("plotResiduals")),
                      tabPanel("Variable Importance", plotOutput("plotImportance")),
                      tabPanel("Time Series", plotOutput("plotTimeSeries"))
                    )
                  )
                )
              )
      )
    )
  )
)


# Define server logic required
library(tsibble)
library(feasts)
library(fable)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(randomForest)

server <- function(input, output) {
  # Plot for EDA1 Tab
  output$tempPlot <- renderPlotly({
    req(input$eda_selectedStation)
    station_data <- weather_data_imputed %>%
      filter(Station %in% input$eda_selectedStation)
    if(input$eda_xAxisType == "Year"){
      df <- station_data %>%
        group_by(Year, Station) %>%
        summarise(
          Daily_Rainfall_Total_mm=mean(Daily_Rainfall_Total_mm),
          Mean_Temperature =mean(Mean_Temperature ),
          Max_Temperature=mean(Max_Temperature),
          Min_Temperature =mean(Min_Temperature )
        )
      p <- ggplot(df, aes_string(x="Year", y=input$eda_yAxisData, color="Station")) + 
        geom_line()
    }else{
      df <- station_data %>%
        mutate(Month=factor(Month, levels=1:12, labels=month.abb)) %>%
        group_by(Month, Station) %>%
        summarise(
          Daily_Rainfall_Total_mm=mean(Daily_Rainfall_Total_mm),
          Mean_Temperature =mean(Mean_Temperature ),
          Max_Temperature=mean(Max_Temperature),
          Min_Temperature =mean(Min_Temperature )
        )
      p <- ggplot(df, aes_string(x="Month", y=input$eda_yAxisData, color="Station", group=1)) + 
        geom_line()
    }
    p %>%
      ggplotly()
  })
  
  output$monthlyTempPlot <- renderPlotly({
    # Ensure combined_data_month is available
    req(combined_data_month)
    
    # Dynamically choose the column for Y-axis and plot type
    y_column <- input$eda3_yAxisData
    plot_type <- input$eda3_plotType
    
    # Create the ggplot object
    p <- ggplot(combined_data_month, aes_string(x = "Month", y = y_column, group = "Year", color = "Year")) +
      {
        if (plot_type == "line") {
          geom_line(size = 1.5)
        } else if (plot_type == "bar") {
          geom_bar(stat = "identity", position = "dodge")
        }
      } +
      facet_wrap(~Year, scales = "free_x") +
      labs(title = "Distribution of Monthly Climate Data From 2014 to 2023",
           x = "Month", y = "Value") +
      scale_x_discrete(limits = 1:12) +
      scale_colour_manual(values = named_color_palette) + # Use the named color palette
      theme_minimal() +
      theme(panel.spacing.y = unit(0.5, "lines"))
    
    # Convert to plotly object
    ggplotly(p)
  })
  
  # Plot for CDA Tab
  output$statsPlot <- renderPlot({
    # Check if 'All' is selected; if not, filter by the selected station
    if (input$cda_selectedStation == "All") {
      filtered_data <- weather_data_imputed
    } else {
      filtered_data <- weather_data_imputed %>%
        filter(Station == input$cda_selectedStation)
    }
    
    test_to_use <- switch(input$cda_statTest,
                          "anova" = "parametric",
                          "kruskal.test" = "nonparametric")
    
    p <- ggbetweenstats(
      data = filtered_data,
      x = !!sym(input$cda_xAxisType),
      y = !!sym(input$cda_yAxisData),
      type = test_to_use,
      plot.type = input$cda_plotType,
      pairwise.comparisons = TRUE,
      pairwise.display = "non-significant",
      conf.level = input$cda_confLevel,
      messages = FALSE,
      title = paste("Distribution of", input$cda_yAxisData,
                    if (input$cda_selectedStation == "All") "across All Stations" else paste("at Station:", input$cda_selectedStation),
                    "from 2014 to 2023"),
      ylab = paste(input$cda_yAxisData, "(mm)"),
      xlab = input$cda_xAxisType,
      ggsignif.args = list(textsize = 4)
    ) +
      theme(text = element_text(size = 12), plot.title = element_text(size = 22))
    
    p
  })
  # Plot for correlation
  output$corrPlot <- renderPlot({
    req(input$cor_vars) # Ensure that the input is not NULL
    
    # Filter data to only include selected variables
    data_selected <- weather_data_imputed %>%
      select(all_of(input$cor_vars))
    
    # Generate the correlation matrix plot
    ggcorrmat(
      data = data_selected,
      type = "continuous",
      output = "plot"
    )
  })
  # Plot for cluster
  output$rainfallPlot <- renderPlot({
    
    selected_stations <- input$selectedStations
    
    weather_filtered <- weather %>%
      filter(Station %in% selected_stations)
    
    # Use lubridate's make_date function
    weather_filtered$Date <- lubridate::make_date(year = weather_filtered$Year, month = weather_filtered$Month, day = 1)
    
    monthly_rainfall <- weather_filtered %>%
      group_by(Station, Date) %>%
      summarise(Total_Rainfall = sum(Daily.Rainfall.Total..mm., na.rm = TRUE), .groups = "drop")
    
    ggplot(monthly_rainfall, aes(x = Date, y = Total_Rainfall, color = Station)) +
      geom_line() +
      labs(title = "Monthly Rainfall by Station", x = "Date", y = "Total Rainfall (mm)") +
      theme_minimal()
  })
  
  output$seasonalPlot <- renderPlot({
    
    # Ensure the 'weather' dataset is filtered according to the input$selectedStations
    weather_filtered <- weather %>%
      filter(Station %in% input$selectedStations)
    
    # Time series analysis and plot generation code
    weather_tsibble <- weather_filtered %>%
      mutate(Year = year(Date), Month = month(Date)) %>%
      group_by(Station, Year, Month) %>%
      summarise(Total_Rainfall = sum(Daily.Rainfall.Total..mm., na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(Date = make_date(Year, Month)) %>%
      select(-Year, -Month) %>%
      as_tsibble(index = Date, key = Station)
    
    # Seasonal decomposition using the feasts package
    weather_decomposed <- weather_tsibble %>%
      model(STL(Total_Rainfall ~ season(window = "periodic")))
    
    # Create the seasonal plots
    seasonal_plots <- weather_tsibble %>%
      mutate(Week = factor(isoweek(Date))) %>%
      as_tsibble(index = Date, key = Station) %>%
      fill_gaps() %>%
      mutate(Total_Rainfall = replace_na(Total_Rainfall, 0)) %>%
      gg_season(Total_Rainfall, period = "week") +
      labs(title = "Seasonal Plot by Week",
           subtitle = "Individual time plot for each Station",
           y = "Total Rainfall") +
      facet_wrap(~ Station, scales = "free_y", ncol = 1) +
      aes(color = Week) +
      scale_color_manual(values = rainbow(length(unique(weather_tsibble$Week))))
    
    # Print the seasonal plots
    print(seasonal_plots)
  })
  # Plot for forecast
  modelOutput <- eventReactive(input$runModel, {
    # Split data into training and testing sets
    # Run the Random Forest model
    # Perform predictions and calculate performance metrics
    # Create all plots
    # Return a list containing the model, predictions, and plots
    # Note: You should use your actual data and column names
    
    # Train the Random Forest model
    rf_model <- randomForest(Daily_Rainfall_Total_mm ~ ., data = weather_rf_train, ntree = input$ntree, mtry = input$mtry, nodesize = input$nodesize, importance = TRUE, na.action = na.omit)
    
    # Predictions
    predictions_test <- predict(rf_model, newdata = weather_rf_test)
    
    # Create the residual plot
    residuals <- weather_rf_test$Daily_Rainfall_Total_mm - predictions_test
    p_residuals <- ggplot() +
      geom_point(aes(x = predictions_test, y = residuals)) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(x = "Predicted", y = "Residuals", title = "Residual Plot")
    
    # Create the variable importance plot
    var_imp <- importance(rf_model)
    p_importance <- ggplot() +
      geom_bar(aes(x = row.names(var_imp), y = var_imp[,1]), stat = "identity") +
      coord_flip() +
      labs(x = "Variables", y = "Increase in Node Purity", title = "Variable Importance")
    
    # Create the actual vs predicted plot
    p_actual_vs_predicted <- ggplot() +
      geom_point(aes(x = weather_rf_test$Daily_Rainfall_Total_mm, y = predictions_test)) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
      labs(x = "Actual", y = "Predicted", title = "Actual vs Predicted")
    
    # Assuming you have actual time series data and forecasted data
    # Create the time series plot
    p_time_series <- ggplot() +
      geom_line(aes(x = weather_rf_train$Date, y = weather_rf_train$Daily_Rainfall_Total_mm), color = "green") +
      geom_line(aes(x = weather_rf_test$Date, y = predictions_test), color = "blue", linetype = "dotted") +
      labs(x = "Date", y = "Rainfall (mm)", title = "Time Series of Rainfall")
    
    # Return a list containing the model and plots
    list(
      model = rf_model,
      plotResiduals = p_residuals,
      plotImportance = p_importance,
      plotActualVsPredicted = p_actual_vs_predicted,
      plotTimeSeries = p_time_series
    )
  })
  
  # Outputs for the plots
  output$plotResiduals <- renderPlot({ modelOutput()$plotResiduals })
  output$plotImportance <- renderPlot({ modelOutput()$plotImportance })
  output$plotActualVsPredicted <- renderPlot({ modelOutput()$plotActualVsPredicted })
  output$plotTimeSeries <- renderPlot({ modelOutput()$plotTimeSeries })
}
# Run the application
shinyApp(ui = ui, server = server)

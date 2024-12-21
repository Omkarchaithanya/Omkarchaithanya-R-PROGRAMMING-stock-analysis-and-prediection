# Install necessary libraries
install.packages(c("quantmod", "tidyverse", "caret", "forecast", "shiny", "shinydashboard", "TTR"))

# Load libraries
library(quantmod)
library(tidyverse)
library(caret)
library(forecast)
library(shiny)
library(shinydashboard)
library(TTR)

# Shiny App for Stock Analysis
ui <- fluidPage(
  titlePanel("Stock Analysis Dashboard"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV File", accept = ".csv"), # Upload CSV file
      dateRangeInput("date_range", "Select Date Range:",
                     start = "2015-01-01", end = Sys.Date()), # Date range input
      actionButton("analyze", "Analyze") # Button to trigger analysis
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Stock Prices", plotOutput("stock_plot")),
        tabPanel("Summary", tableOutput("summary_table")),
        tabPanel("Predictions", plotOutput("prediction_plot")),
        tabPanel("Missing Columns", tableOutput("missing_columns"))
      )
    )
  )
)

server <- function(input, output, session) {
  # Reactive data for stock analysis
  stock_data <- reactive({
    req(input$file) # Ensure file is uploaded
    data <- read.csv(input$file$datapath)
    
    # Define expected columns
    expected_columns <- c("Date", "Symbol", "Series", "Prev Close", "Open", 
                          "High", "Low", "Close", "VWAP", "Volume", "Turnover")
    
    # Check for missing or extra columns
    missing_columns <- setdiff(expected_columns, colnames(data))
    extra_columns <- setdiff(colnames(data), expected_columns)
    
    # Store missing and extra columns in a reactive list
    output$missing_columns <- renderTable({
      tibble(
        Missing_Columns = if (length(missing_columns) > 0) missing_columns else "None",
        Extra_Columns = if (length(extra_columns) > 0) extra_columns else "None"
      )
    })
    
    # Ensure essential columns exist
    essential_columns <- c("Date", "Close")
    if (!all(essential_columns %in% colnames(data))) {
      stop("The uploaded file is missing essential columns: Date or Close.")
    }
    
    # Format the Date column
    data$Date <- as.Date(data$Date, format = "%Y-%m-%d")
    data <- data %>% filter(Date >= input$date_range[1] & Date <= input$date_range[2])
    data <- data %>% drop_na() # Remove missing rows
    data
  })
  
  # Plot Stock Prices
  output$stock_plot <- renderPlot({
    req(stock_data()) # Ensure data is available
    ggplot(stock_data(), aes(x = Date, y = Close)) +
      geom_line(color = "blue") +
      labs(title = "Stock Prices Over Time", x = "Date", y = "Closing Price") +
      theme_minimal()
  })
  
  # Show Summary Table
  output$summary_table <- renderTable({
    req(stock_data()) # Ensure data is available
    stock_data() %>% summary() # Summary of the dataset
  })
  
  # Reactive data for modeling
  model_data <- reactive({
    req(stock_data()) # Ensure data is available
    
    # Add indicators: SMA, EMA, and Percent Change
    data <- stock_data() %>%
      mutate(
        SMA_50 = SMA(Close, n = 50),
        EMA_20 = EMA(Close, n = 20),
        Percent_Change = (Close - lag(Close)) / lag(Close) * 100
      ) %>%
      drop_na() # Remove rows with missing values
    data
  })
  
  # Predictions Plot
  output$prediction_plot <- renderPlot({
    req(model_data()) # Ensure model data is available
    
    # Check if the dataset is large enough for partitioning
    if (nrow(model_data()) < 30) {
      ggplot() +
        annotate("text", x = 1, y = 1, label = "Insufficient data for prediction", size = 5) +
        theme_void()
      return()
    }
    
    data <- model_data()
    
    # Split data into training and testing sets
    set.seed(123)
    train_index <- createDataPartition(data$Close, p = 0.8, list = FALSE)
    train_data <- data[train_index, ]
    test_data <- data[-train_index, ]
    
    # Train a linear regression model
    lm_model <- lm(Close ~ SMA_50 + EMA_20 + Percent_Change, data = train_data)
    predictions <- predict(lm_model, newdata = test_data)
    
    # Add predictions to the test data
    test_data <- test_data %>% mutate(Predicted_Close = predictions)
    
    # Plot actual vs predicted closing prices
    ggplot(test_data, aes(x = Date)) +
      geom_line(aes(y = Close, color = "Actual")) +
      geom_line(aes(y = Predicted_Close, color = "Predicted")) +
      labs(title = "Actual vs Predicted Closing Prices", x = "Date", y = "Close Price") +
      scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
      theme_minimal()
  })
}

# Run the Shiny App
shinyApp(ui, server)

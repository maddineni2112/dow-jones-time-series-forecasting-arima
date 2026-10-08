# Install/load packages
if (!require("forecast")) {
  install.packages("forecast")
  library(forecast)
}

if (!require("ggplot2")) {
  install.packages("ggplot2")
  library(ggplot2)
}

# Load training data
dow_train <- read.csv("DowJones Training.csv", stringsAsFactors = FALSE)

# Convert Date column
dow_train$Date <- as.Date(dow_train$Date, format = "%m/%d/%Y")

# Sort by date
dow_train <- dow_train[order(dow_train$Date), ]

# Create weekly time series
dow_ts <- ts(dow_train$Value, frequency = 52)

# Plot original series
autoplot(dow_ts) +
  labs(
    title = "Dow Jones Training Series",
    x = "Time",
    y = "Index Value"
  )

# Log transform
log_dow <- log(dow_ts)

best_model <- auto.arima(log_dow, seasonal = TRUE, stepwise = TRUE, approximation = FALSE)
summary(best_model)

# Forecast next 10 periods
forecast_log <- forecast(best_model_log, h = 10)

# Plot forecast
autoplot(forecast_log) +
  labs(
    title = "Auto ARIMA Forecast on Log Dow Jones Series",
    x = "Time",
    y = "Log(Index Value)"
  )

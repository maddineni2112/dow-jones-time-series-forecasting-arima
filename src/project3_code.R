# ----------------
# Install / load packages
# ----------------
packages <- c("forecast", "ggplot2", "tseries", "stats", "lubridate")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# ==========================================================
#TASK 01: Making the time series stationary and checking using ADF test.
# Load training data
#=============================================================
dow_train <- read.csv("DowJones Training.csv", stringsAsFactors = FALSE)
dow_train$Date <- as.Date(dow_train$Date, format = "%m/%d/%Y")
dow_train <- dow_train[order(dow_train$Date), ]

start_year <- year(min(dow_train$Date))
start_week <- isoweek(min(dow_train$Date))

dow_ts <- ts(dow_train$Value, start = c(start_year, start_week), frequency = 52)

# ----------------
# Plot original series
# ----------------
ggplot(dow_train, aes(x = Date, y = Value)) +
  geom_line(color = "black") +
  labs(
    title = "Dow Jones Training Series",
    x = "Year",
    y = "Index Value"
  ) +
  theme_minimal()

# ----------------
# ADF test on original series
# ----------------
cat("\nADF test on original series:\n")
adf_original <- adf.test(dow_ts, k = 12)
print(adf_original)

Acf(dow_ts)
Pacf(dow_ts)

# ----------------
#1.  First differencing
# ----------------
diff1 <- diff(dow_ts, differences = 1)

autoplot(diff1) +
  labs(
    title = "First Differenced Dow Jones Series",
    x = "Time",
    y = "Differenced Value"
  ) +
  theme_minimal()

Acf(diff1, main = "ACF of First Differenced Series")
Pacf(diff1, main = "PACF of First Differenced Series")

# ----------------
# 2. Log transform + first differencing
# ----------------
log_dow <- log(dow_ts)
log_diff1 <- diff(log_dow, differences = 1)

autoplot(log_dow) +
  labs(
    title = "Log Transformed Dow Jones Series",
    x = "Time",
    y = "Log(Index Value)"
  ) +
  theme_minimal()

autoplot(log_diff1) +
  labs(
    title = "Log + First Differenced Dow Jones Series",
    x = "Time",
    y = "Differenced Log(Value)"
  ) +
  theme_minimal()
#-----------
# 3. Stationary testing using ADF
#-----------------
cat("\nADF test after log transform + first differencing:\n")
adf_log_diff1 <- adf.test(log_diff1, k = 12)
print(adf_log_diff1)

#======================================================
#TASK02: Plotting ACF and PACF on stationary series
#finding the correct p,q, values and justifying using AIC,BIC and AICc
#===============================================================

Acf(log_diff1, main = "ACF of Log + First Differenced Series")
Pacf(log_diff1, main = "PACF of Log + First Differenced Series")

# ----------------
# Manual ARIMA fitting on log_dow only
# d = 1 because we want ARIMA(p,1,q) on the log-transformed series
# Final model selection will be based on BIC
# ----------------

results <- data.frame(
  p = integer(),
  d = integer(),
  q = integer(),
  AIC = numeric(),
  BIC = numeric(),
  stringsAsFactors = FALSE
)

best_aic <- Inf
best_bic <- Inf

best_model_aic <- NULL
best_model_bic <- NULL

best_order_aic <- c(NA, 1, NA)
best_order_bic <- c(NA, 1, NA)

for (p in 1:3) {
  for (q in 1:3) {
    
    fit <- tryCatch(
      forecast::Arima(log_dow, order = c(p, 1, q), include.drift = TRUE),
      error = function(e) NULL
    )
    
    if (!is.null(fit)) {
      model_aic <- AIC(fit)
      model_bic <- BIC(fit)
      
      results <- rbind(
        results,
        data.frame(
          p = p,
          d = 1,
          q = q,
          AIC = model_aic,
          BIC = model_bic
        )
      )
      
      # Track best model by AIC
      if (model_aic < best_aic) {
        best_aic <- model_aic
        best_model_aic <- fit
        best_order_aic <- c(p, 1, q)
      }
      
      # Track best model by BIC
      if (model_bic < best_bic) {
        best_bic <- model_bic
        best_model_bic <- fit
        best_order_bic <- c(p, 1, q)
      }
    }
  }
}

# Rankings
results_aic <- results[order(results$AIC), ]
results_bic <- results[order(results$BIC), ]

cat("\nCandidate ARIMA models ranked by AIC:\n")
print(results_aic)

cat("\nCandidate ARIMA models ranked by BIC:\n")
print(results_bic)

cat("\nBest model based on AIC:\n")
cat("ARIMA(", best_order_aic[1], ",", best_order_aic[2], ",", best_order_aic[3], ")\n", sep = "")
print(best_model_aic)

cat("\nBest model based on BIC:\n")
cat("ARIMA(", best_order_bic[1], ",", best_order_bic[2], ",", best_order_bic[3], ")\n", sep = "")
print(best_model_bic)

# Final selected model based on BIC
final_model <- best_model_bic
final_order <- best_order_bic

cat("\nFinal selected model (based on BIC):\n")
cat("ARIMA(", final_order[1], ",", final_order[2], ",", final_order[3], ")\n", sep = "")
print(final_model)

# Confirming with auto.arima
auto_model <- auto.arima(log_dow)
print(auto_model)


# ==================================================================================================
# TASK 03: Testing ARIMA (1,1,1) on test data
# ====================================================================================
# Load test data
dow_test <- read.csv("DowJones Testing.csv", stringsAsFactors = FALSE)
dow_test$Date <- as.Date(dow_test$Date, format = "%m/%d/%Y")
dow_test <- dow_test[order(dow_test$Date), ]

test_ts <- ts(dow_test$Value, frequency = 52)

# ----------------
# 1. getting the RMSE values 
# Forecast on log scale, then back-transform
# ----------------
h <- length(test_ts)
fc <- forecast(final_model, h = h)

fc_log_values <- as.numeric(fc$mean)
fc_values <- exp(fc_log_values)

forecast_df <- data.frame(
  Date = dow_test$Date,
  Actual = dow_test$Value,
  Forecast = fc_values
)

# ----------------
# Accuracy metrics on original scale
# ----------------
rmse <- sqrt(mean((forecast_df$Actual - forecast_df$Forecast)^2, na.rm = TRUE))
mae  <- mean(abs(forecast_df$Actual - forecast_df$Forecast), na.rm = TRUE)
mape <- mean(abs((forecast_df$Actual - forecast_df$Forecast) / forecast_df$Actual), na.rm = TRUE) * 100

# Print final selected model
cat("\nFinal Selected Model:\n")
cat("ARIMA(",
    final_order[1], ",",
    final_order[2], ",",
    final_order[3], ")\n", sep="")
cat("\nTest set accuracy:\n")
cat("RMSE :", rmse, "\n")
cat("MAE  :", mae, "\n")
cat("MAPE :", mape, "%\n")

# ----------------
# 2. Plots of the training+testing(actual v/s Predicted)
# Training fitted values, fitted(best_model) is on log scale, so convert back with exp()
# IMPORTANT: fitted values may be shorter than original training rows
# ----------------
train_fitted_log <- as.numeric(fitted(final_model))
train_fitted <- exp(train_fitted_log)

n_train_fit <- length(train_fitted)

train_df <- data.frame(
  Date = tail(dow_train$Date, n_train_fit),
  Actual = tail(dow_train$Value, n_train_fit),
  Predicted = train_fitted,
  Segment = "Train"
)

# ----------------
# Test predicted values
# ----------------
test_df <- data.frame(
  Date = dow_test$Date,
  Actual = dow_test$Value,
  Predicted = fc_values,
  Segment = "Test"
)

# ----------------
# Combine train and test
# ----------------
all_df <- rbind(train_df, test_df)


# ----------------
# Plot actual vs predicted for both train and test
# ----------------
ggplot(all_df, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual"), linewidth = 1) +
  geom_line(aes(y = Predicted, color = "Predicted"),
            linewidth = 1,
            linetype = "dashed") +
  geom_vline(
    xintercept = as.numeric(max(dow_train$Date)),
    linetype = "dotted",
    linewidth = 1
  ) +
  labs(
    title = "Actual vs Predicted Values for Train and Test Segments",
    subtitle = "ARIMA fitted on log(Dow Jones), predictions back-transformed with exp()",
    x = "Date",
    y = "Dow Jones Value",
    color = "Series"
  ) +
  theme_minimal()
# ----------------
#Getting zoomed look at the testdata plot
# Add last few training points + full test points (zoomed transition view)
# ----------------

n_last_train <- 3   # change to 2 or 3 as desired

zoom_actual_df <- rbind(
  data.frame(
    Date = tail(dow_train$Date, n_last_train),
    Actual = tail(dow_train$Value, n_last_train),
    Segment = "Train"
  ),
  data.frame(
    Date = dow_test$Date,
    Actual = dow_test$Value,
    Segment = "Test"
  )
)

zoom_forecast_df <- data.frame(
  Date = dow_test$Date,
  Forecast = fc_values
)

# ----------------
# Plot zoomed train/test boundary
# ----------------
ggplot() +
  geom_line(
    data = zoom_actual_df,
    aes(x = Date, y = Actual, color = "Actual"),
    linewidth = 1
  ) +
  geom_line(
    data = zoom_forecast_df,
    aes(x = Date, y = Forecast, color = "Forecast"),
    linewidth = 1,
  ) +
  geom_vline(
    xintercept = as.numeric(max(dow_train$Date)),
    linetype = "dotted",
    linewidth = 1
  ) +
  labs(
    title = paste0(
      "Zoomed Forecast vs Actual: ARIMA(",
      final_order[1], ",", final_order[2], ",", final_order[3], ")"
    ),
    subtitle = "Last training observations + full test forecast",
    x = "Date",
    y = "Dow Jones Value",
    color = "Series"
  ) +
  theme_minimal()





# =========================================================
# TASK 4 :Seasonal analysis + seasonal ARIMA (SARIMA)
# =========================================================

# ----------------
# 1) Extract seasonal signal from original time series
# ----------------
# Since your data is weekly with frequency = 52, we treat 52 as the seasonal period

decomp <- stl(log_dow, s.window = "periodic")
plot(decomp, main = "STL Decomposition of Log Dow Jones Series")

seasonal_signal <- decomp$time.series[, "seasonal"]
trend_signal    <- decomp$time.series[, "trend"]
remainder_signal <- decomp$time.series[, "remainder"]

autoplot(seasonal_signal) +
  labs(
    title = "Extracted Seasonal Signal from Log Dow Jones Series",
    x = "Time",
    y = "Seasonal Component"
  ) +
  theme_minimal()
auto.arima(
  log_dow,
  seasonal = TRUE
)
# ----------------
# 2) Seasonal ACF and PACF on seasonal signal
# ----------------
Acf(seasonal_signal, lag.max = 104, main = "Seasonal ACF of Extracted Seasonal Signal")
Pacf(seasonal_signal, lag.max = 104, main = "Seasonal PACF of Extracted Seasonal Signal")


cat("\nInspect the seasonal ACF/PACF at lags 52, 104, ... to infer Q and P.\n")
cat("Typical interpretation:\n")
cat("- Q is based on significant cutoff in seasonal ACF at seasonal lags.\n")
cat("- P is based on significant cutoff in seasonal PACF at seasonal lags.\n")

# ----------------
# seasonal differencing to inspect stationarity
# ----------------
seasonal_diff_log <- diff(diff(log_dow, lag = 52))

autoplot(seasonal_diff_log) +
  labs(
    title = "Seasonally Differenced Log Dow Jones Series (lag = 52)",
    x = "Time",
    y = "Seasonally Differenced Log Value"
  ) +
  theme_minimal()

cat("\nADF test on seasonally differenced log series:\n")
print(adf.test(na.omit(seasonal_diff_log)))

Acf(na.omit(seasonal_diff_log), lag.max = 208,
    main = "ACF of Seasonally Differenced Log Series")
Pacf(na.omit(seasonal_diff_log), lag.max = 208,
     main = "PACF of Seasonally Differenced Log Series")

# ----------------
# 3) Fit seasonal ARIMA model
# ----------------
# IMPORTANT:
# Replace P_season and Q_season as per requiremnets
P_season <- 0
D_season <- 1
Q_season <- 1

sarima_model <- Arima(
  log_dow,
  order = c(final_order[1], final_order[2], final_order[3]),
  seasonal = list(order = c(P_season, D_season, Q_season), period = 52),
  include.drift = TRUE
)

cat("\nSeasonal ARIMA model summary:\n")
print(summary(sarima_model))

# ----------------
# Forecast using seasonal model on test horizon
# ----------------
sarima_fc <- forecast(sarima_model, h = length(test_ts))

sarima_fc_log_values <- as.numeric(sarima_fc$mean)
sarima_fc_values <- exp(sarima_fc_log_values)

sarima_forecast_df <- data.frame(
  Date = dow_test$Date,
  Actual = dow_test$Value,
  Forecast = sarima_fc_values
)

# ----------------
# Test accuracy for seasonal model
# ----------------
sarima_rmse <- sqrt(mean((sarima_forecast_df$Actual - sarima_forecast_df$Forecast)^2, na.rm = TRUE))
sarima_mae  <- mean(abs(sarima_forecast_df$Actual - sarima_forecast_df$Forecast), na.rm = TRUE)
sarima_mape <- mean(abs((sarima_forecast_df$Actual - sarima_forecast_df$Forecast) /
                          sarima_forecast_df$Actual), na.rm = TRUE) * 100

cat("\nSeasonal model test accuracy:\n")
cat("RMSE :", sarima_rmse, "\n")
cat("MAE  :", sarima_mae, "\n")
cat("MAPE :", sarima_mape, "%\n")

# ----------------
# Training fitted values from seasonal model
# ----------------
sarima_train_fitted_log <- as.numeric(fitted(sarima_model))
sarima_train_fitted <- exp(sarima_train_fitted_log)

n_train_fit_seasonal <- length(sarima_train_fitted)

sarima_train_df <- data.frame(
  Date = tail(dow_train$Date, n_train_fit_seasonal),
  Actual = tail(dow_train$Value, n_train_fit_seasonal),
  Predicted = sarima_train_fitted,
  Segment = "Train"
)

# ----------------
# Test predictions from seasonal model
# ----------------
sarima_test_df <- data.frame(
  Date = dow_test$Date,
  Actual = dow_test$Value,
  Predicted = sarima_fc_values,
  Segment = "Test"
)

# ----------------
# Combine train + test for seasonal model plot
# ----------------
sarima_all_df <- rbind(sarima_train_df, sarima_test_df)

ggplot(sarima_all_df, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual"), linewidth = 1) +
  geom_line(aes(y = Predicted, color = "Predicted"),
            linewidth = 1,
            linetype = "dashed") +
  geom_vline(
    xintercept = as.numeric(max(dow_train$Date)),
    linetype = "dotted",
    linewidth = 1
  ) +
  labs(
    title = "Seasonal ARIMA: Actual vs Predicted for Train and Test",
    subtitle = paste0(
      "SARIMA(",
      final_order[1], ",", final_order[2], ",", final_order[3], ")(",
      P_season, ",", D_season, ",", Q_season, ")[52]"
    ),
    x = "Date",
    y = "Dow Jones Value",
    color = "Series"
  ) +
  theme_minimal()

# ----------------
# 4) Compare seasonal vs non-seasonal model
# ----------------
cat("\nModel comparison:\n")
cat("Non-seasonal ARIMA AIC :", AIC(final_model), "\n")
cat("Seasonal ARIMA AIC     :", AIC(sarima_model), "\n")
cat("Non-seasonal RMSE      :", rmse, "\n")
cat("Seasonal RMSE          :", sarima_rmse, "\n")

if (sarima_rmse < rmse) {
  cat("\nConclusion: The seasonal ARIMA model is better based on lower RMSE.\n")
} else if (sarima_rmse > rmse) {
  cat("\nConclusion: The non-seasonal ARIMA model is better based on lower RMSE.\n")
} else {
  cat("\nConclusion: Both models have the same RMSE.\n")
}
# ----------------
# Getting zoomed look at the test data plot
# Add last few training points + full test points (zoomed transition view)
# ----------------

n_last_train <- 3   # change to 2 or 3 as desired

zoom_actual_df <- rbind(
  data.frame(
    Date = tail(dow_train$Date, n_last_train),
    Actual = tail(dow_train$Value, n_last_train),
    Segment = "Train"
  ),
  data.frame(
    Date = dow_test$Date,
    Actual = dow_test$Value,
    Segment = "Test"
  )
)

zoom_forecast_df <- data.frame(
  Date = dow_test$Date,
  Forecast = sarima_fc_values
)

# ----------------
# Plot zoomed train/test boundary for Seasonal ARIMA
# ----------------
ggplot() +
  geom_line(
    data = zoom_actual_df,
    aes(x = Date, y = Actual, color = "Actual"),
    linewidth = 1
  ) +
  geom_line(
    data = zoom_forecast_df,
    aes(x = Date, y = Forecast, color = "Forecast"),
    linewidth = 1,
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = as.numeric(max(dow_train$Date)),
    linetype = "dotted",
    linewidth = 1
  ) +
  labs(
    title = paste0(
      "Zoomed Forecast vs Actual: SARIMA(",
      final_order[1], ",", final_order[2], ",", final_order[3], ")(",
      P_season, ",", D_season, ",", Q_season, ")[52]"
    ),
    subtitle = "Last training observations + full test forecast",
    x = "Date",
    y = "Dow Jones Value",
    color = "Series"
  ) +
  theme_minimal()

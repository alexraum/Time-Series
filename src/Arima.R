### ARIMA models

# import libraries
library(zoo)
library(tidyverse)
library(forecast)

# set the working directory
setwd("C:/Users/alexr/OneDrive/Documents/NCSU/MSA Program/Fall 2021/AA502/Time Series 2/Homework 1/data")
# read in the data set
data <- read.csv('UK.csv')


## Part 0: check for missing months

# convert the Date column to date object and store as a variable
months <- as.Date(as.yearmon(data$Date, "%b-%y"))
# use seq.Date to create a range of months based on min and max Dates, spaced by month
rangeMonths <- seq.Date(min(months), max(months), by = "month")
# determine which months in the complete interval are missing in our data
missing <- !(rangeMonths %in% months)
# use indexing to obtain these months from the complete interval
missingMonths <- rangeMonths[missing]
# determine the total number of missing months
length(missingMonths)


## Part 1: Visualize and split the data

# create a time series object
energy <- ts(data$Hydro_energy, start = c(2006, 1), frequency = 12)

# decompose the training set and view the components
decomp_stl <- stl(energy, s.window = 7)
autoplot(decomp_stl)

# all but last 17 months used for training data
training = subset(energy, end = length(energy)-17)
# first 12 of last 17 months form validation data
validation <- subset(energy, start = length(energy)-16, end = length(energy)-5)
# last 5 observations form test data
test <- subset(energy, start = length(energy)-4)


## Part 2: Evaluate the best approach to use to account for seasonality

# use unit root testing to check to see if differencing is needed
training %>% nsdiffs()
# since seasonal differencing is required, we determine that we have stochastic seasonality

# determine how many regular differences to take after taking seasonal difference
training %>% diff(lag = 12) %>% ndiffs()
# no regular differencing required


## Part 3: Build an exponential smoothing model and forecast on validation set only

# fit an additive HW ESM to the training data
energy_hwesa_train <- hw(training, seasonal = "additive", h = 12) # forecast 12 steps since validation data is 12 months
# calculate Holt/Winters (additive) prediction errors from forecast
error_hwesa <- validation - energy_hwesa_train$mean
# calculate MAE
MAE_hwesa <- mean(abs(error_hwesa))
print(MAE_hwesa)
# calculate MAPE
MAPE_hwesa <- mean(abs(error_hwesa) / abs(validation)) * 100
print(MAPE_hwesa)

# fit a multiplicative HW ESM to the training data
energy_hwesm_train <- hw(training, seasonal = "multiplicative", h = 12) # forecast 12 steps since validation data is 12 months
# calculate Holt/Winters (multiplicative) prediction errors from forecast
error_hwesm <- validation - energy_hwesm_train$mean
# calculate MAE
MAE_hwesm <- mean(abs(error_hwesm))
print(MAE_hwesm)
# calculate MAPE
MAPE_hwesm <- mean(abs(error_hwesm) / abs(validation)) * 100
print(MAPE_hwesm)


## Part 4: Build a seasonal ARIMA model

# use the auto.arima function to select the best starting model
# specify seasonal = TRUE to allow function to take seasonal difference for us
energy_seas_arima <- auto.arima(training, method = "ML", seasonal = TRUE)
# view the model summary
summary(energy_seas_arima)

# check the residuals and run Ljung-Box test to see if we have captured the signal
checkresiduals(energy_seas_arima)

# p-val = 0.304 ==> fail to reject H_0 and conclude that we have white noise

# next, use this model to forecast the next 12 months
f_cast <- forecast::forecast(energy_seas_arima, h = 12)
# plot the forecast
autoplot(f_cast, size = 1.0) + 
  autolayer(fitted(energy_seas_arima), series = "Fitted", size = 1.0) +
  autolayer(validation, color = "green", size = 1.0) +
  ggtitle("Forecast vs. Observed Values for Validation Data") +
  xlab("Time (Months)") +
  ylab("Energy Consumption (GWh)") +
  geom_vline(xintercept = 2019.85, size = 1.0, color = "black", linetype = "dashed") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# calculate prediction errors from forecast
s_arima_error <- validation - f_cast$mean

# calculate MAE
MAE_s_arima <- mean(abs(s_arima_error))
print(MAE_s_arima)
# calculate MAPE
MAPE_s_arima <- mean(abs(s_arima_error) / abs(validation)) * 100
print(MAPE_s_arima)

# the MAE and MAPE did not beat these metrics from the HW ESM, this may be
# due to the outlier we see in the ACF plot


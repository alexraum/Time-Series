### Homework 2: Time Series 2 (Alex Raum)

# import libraries
library(zoo)
library(tidyverse)
library(forecast)
library(Rcpp)
library(prophet)

# set the working directory
setwd("C:/Users/alexr/OneDrive/Documents/NCSU/MSA Program/Fall 2021/AA502/Time Series 2/Homework 2")

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


## Part 2: Build a prophet model

# create a dummy variable for the impulse in the data
impulse <- rep(0, 207)
impulse[which.max(training)] <- 1

# create a data frame for user-defined holidays
holidays <- data.frame(
  holiday = 'impulse',
  ds = as.Date(c('2014-01-01')),
  lower_window = 0,
  upper_window = 1
)

# specify a data frame to pass dates to prophet function
prophet_data <- data.frame(ds = seq(as.Date('2006-01-01'), as.Date('2019-11-01'), by = 'm'), y = training)

# instantiate a prophet model
fit_prof <- prophet()
# add UK holidays to the model
fit_prof <- add_country_holidays(fit_prof, "UK")
# add yearly seasonality to the model (fourier.order = 9 and 10 also work well)
fit_prof <- add_seasonality(fit_prof, name = 'yearly', period = 365.25, fourier.order = 8)
# fit the model
fit_prof <- fit.prophet(fit_prof, prophet_data)

# generate predictions for each observation and forecast 12 months into the future
forecast_data <- make_future_dataframe(fit_prof, periods = 12, freq = 'month')
# plot the predictions alongside the training data
plot(fit_prof, predict(fit_prof, forecast_data))

# calculate the error
prophet_error <- validation - tail(predict(fit_prof, forecast_data)$yhat, 12)

# calculate prediction error statistics (MAE and MAPE)
prophet_mae <- mean(abs(prophet_error))
prophet_mape  <- mean(abs(prophet_error) / abs(validation)) * 100

# print these values
print(prophet_mae)
print(prophet_mape)


## Part 2: Build a neural network model

# use an autoarima procedure to determine the number of lags, take seasonal differences on data
set.seed(12345)
nn_model <- nnetar(diff(training, 12), p = 6, P = 8) # MAE and MAPE are minimized when p = 6, and P = 6

# forecast differences 12 months into the future
nn_forecast <- forecast::forecast(nn_model, h = 12)
# plot the forecast of the differences
plot(nn_forecast) + grid()

# calculate the forecasts of the actual data based on the differences (needs to be repeated for each month we want to forecast)
energy_forecast <- rep(NA, 12)

for(i in 1:12){
  energy_forecast[i] <- training[length(training) - 12 + i] + forecast::forecast(nn_model, h = 24)$mean[i] # should h = 12?
}

# calculate prediction errors from forecast
nn_error <- validation - energy_forecast

# calculate prediction error statistics (MAE and MAPE)
nn_mae <- mean(abs(nn_error))
nn_mape <- mean(abs(nn_error) / abs(validation)) * 100

# print these values
print(nn_mae)
print(nn_mape)

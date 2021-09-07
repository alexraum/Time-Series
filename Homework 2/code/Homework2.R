### Homework 2: Time Series

# set the working directory
setwd("C:/Users/alexr/Documents/Documents/NCSU/MSA Program/Fall 2021/AA502/Time Series/Time Series/Homework 2/data")

# import libraries
library(zoo)
library(tidyverse)
library(forecast)

# read in the data set
energyData <- read.csv("UK.csv")



## Part 1

# Determine the total number of months missing

# convert the Date column to date object and store as a variable
months <- as.Date(as.yearmon(energyData$Date, "%b-%y"))
# use seq.Date to create a range of months based on min and max Dates, spaced by month
rangeMonths <- seq.Date(min(months), max(months), by = "month")

# determine which months in the complete interval are missing in our data
missing <- !(rangeMonths %in% months)
# use indexing to obtain these months from the complete interval
missingMonths <- rangeMonths[missing]
# determine the total number of missing months
length(missingMonths)


# Visualize and split the data

# create a time series object
energy <- ts(energyData$Hydro_energy, start = c(2006, 1), frequency = 12)


# visualize the entire data set
autoplot(energy, size = 1.0) +
  ggtitle("Observed Energy vs. Time (Training Data)") +
  xlab("Time (Months)") +
  ylab("Energy (GWh)") +
  #scale_y_continuous(labels = comma) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# all but last 17 months used for training data
training = subset(energy, end = length(energy)-17)
# first 12 of last 17 months form validation data
validation <- subset(energy, start = length(energy)-16, end = length(energy)-5)
# last 5 observations form test data
test <- subset(energy, start = length(energy)-4)


# variation around trend appears to remain roughly constant, implying an additive model

# since we have an additive model, we will use STL decomposition

# seasonality also appears to be changing throughout the years, which we are able to 
# detect by using STL decomposition



## Part 2

# Create a monthly exponential smoothing model withholding the last 17 months

# SIMPLE ESM (can only forecast one time step ahead)

# simple exponential smoothing model
energy_ses <- ses(training, h = 24)
summary(energy_ses)

# plot the SES model on the energy data
autoplot(energy_ses) +
  autolayer(fitted(energy_ses), series="Fitted") # what do we pass as arguments to autolayer and autoplot???


# HOLT ESM (incorporates trend and can forecast several time steps ahead, but still best to only forecast one step)

# linear/Holt exponential smoothing model
energy_les <- holt(training, initial = "optimal", h = 24)
summary(energy_les)

# plot the LES model on the energy data
autoplot(energy_les) +
  autolayer(fitted(energy_les), series = "Fitted")


# DAMPED TREND (forecasts further out are damped)

# damped trend exponential smoothing model
energy_ldes <- holt(training, initial = "optimal", h = 24, damped = T)
summary(energy_ldes)

# plot the LDES model on the energy data
autoplot(energy_ldes) +
  autolayer(fitted(energy_ldes), series = "Fitted")


# HOLT/WINTERS (can incorporate seasonality, use to forecast one step ahead, then rerun with new prediction value)

# Holt/Winters exponential smoothing model (additive seasonality)
energy_hwes <- hw(training, seasonal = "additive")
summary(energy_hwes)

# plot the HW model on the energy data
autoplot(energy_hwes) +
  autolayer(fitted(energy_hwes), series = "Fitted")

# using MAPE, we determine that a HW model that incorporates additive seasonality is the best candidate ESM



## Part 3

# Create visualization of actual electricity values overlaid with the
# trend/cycle component for the training set

# decompose the training set and view the components
decomp_stl <- stl(training, s.window = 7)
autoplot(decomp_stl)

# overlay actual values and trend
autoplot(training, size = 1.0) + 
  geom_line(aes(y = decomp_stl$time.series[,2]), color = "blue", size = 1.0) +
  ggtitle("Observed Energy Values and Trend Component vs. Time") +
  xlab("Time (Months)") +
  ylab("Energy (GWh)") +
  #scale_y_continuous(labels = comma) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


# create visualization of actual electricity values overlaid with the
# seasonally adjusted electricity values for the training data set

# create seasonally adjusted data by subtracting seasonality component from data
seas_adj <- training - decomp_stl$time.series[,1]


# overlay actual values and seasonally adjusted values
autoplot(training, size = 1.0) +
  geom_line(aes(y = seas_adj), color = "red", size = 1.0) +
  ggtitle("Observed Energy Values and Seasonally Adjusted Values vs. Time") +
  xlab("Time (Months)") +
  ylab("Energy (GWh)") +
  #scale_y_continuous(labels = comma) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))



## Part 4

# Use validation data set to determine which model performs best (using MAPE)
# (we get an initial idea of what model this may be by examining training data as above)

# check each model to determine which performs best on validation data

# fit SES model on training data to use on validation data set
energy_ses_train <- ses(training, h = 12) # forecast 12 steps since validation data is 12 months

# calculate SES prediction errors from forecast
error_ses <- validation - energy_ses_train$mean

# calculate MAPE
MAPE_ses <- mean(abs(error_ses) / abs(validation))


# fit linear/Holt model on training data to use on validation data set
energy_les_train <- holt(training, h = 12) # forecast 12 steps since validation data is 12 months

# calculate LES prediction errors from forecast
error_les <- validation - energy_les_train$mean

# calculate MAPE
MAPE_les <- mean(abs(error_les) / abs(validation))


# fit damped model on training data to use on validation data set
energy_ldes_train <- holt(training, h = 12, damped = T) # forecast 12 steps since validation data is 12 months

# calculate LDES prediction errors from forecast
error_ldes <- validation - energy_ldes_train$mean

# calculate MAPE
MAPE_ldes <- mean(abs(error_ldes) / abs(validation))


# fit Holt/Winters model with additive seasonality on training data to use on validation data set
energy_hwesa_train <- hw(training, seasonal = "additive", h = 12) # forecast 12 steps since validation data is 12 months

# calculate Holt/Winters (additive) prediction errors from forecast
error_hwesa <- validation - energy_hwesa_train$mean

# calculate MAPE
MAPE_hwesa <- mean(abs(error_hwesa) / abs(validation))


# fit Holt/Winters model with multiplicative seasonality on training data to use on validation data set
energy_hwesm_train <- hw(training, seasonal = "multiplicative", h = 12) # forecast 12 steps since validation data is 12 months

# calculate Holt/Winters (multiplicative) prediction errors from forecast
error_hwesm <- validation - energy_hwesm_train$mean

# calculate MAPE
MAPE_hwesm <- mean(abs(error_hwesm) / abs(validation))

# thus, we determine that the Holt/Winters additive model performs the best on the validation data 


# combine training and validation set, rerun the best ESM to update parameters, then calculate
# the model accuracy on test data set

# combine training and validation data set into a new training data set
training_validation = subset(energy, end = length(energy)-5)

# refit the data to update the parameters
energy_hwes_tr_val <- hw(training_validation, seasonal = "additive", h = 5) # forecast 5 steps since test data is 5 months

# calculate accuracy
error <- test - energy_hwes_tr_val$mean
MAPE <- mean(abs(error) / abs(test))

# conclude that the MAPE is 0.125351



## Part 5

# Create time plots of predicted values vs. actual values for validation data and test data

# actual values vs. predicted values for the validation data set
autoplot(validation, size = 1.0) + 
  geom_line(aes(y = energy_hwesa_train$mean), color = "blue", size = 1.4) +
  ggtitle("Forecast vs. Observed Values for Validation Data") +
  xlab("Time (Months)") +
  ylab("Energy") +
  theme(plot.title = element_text(hjust = 0.5))

# actual values vs. predicted values for the validation data set (training data included)
autoplot(energy_hwesa_train, PI = F, size = 1.0) +
  autolayer(fitted(energy_hwesa_train), series = "Fitted", size = 1.0) +
  autolayer(validation, color = "green", size = 1.0) +
  ggtitle("Forecast vs. Observed Values for Validation Data") +
  xlab("Time (Months)") +
  ylab("Energy (GHz)") +
  xlim(2015, NA) +
  geom_vline(xintercept = 2019.877, size = 1.0, color = "black", linetype = "dashed") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# actual values vs. predicted values for the test data set (training and validation data included)
autoplot(test, size = 1.0) + 
    geom_line(aes(y = energy_hwes_tr_val$mean), color = "red", size = 1.4) +
    ggtitle("Forecast vs. Observed Values for Test Data") +
    xlab("Time (Months)") +
    ylab("Energy") +
    theme(plot.title = element_text(hjust = 0.5))

autoplot(energy_hwes_tr_val, PI = F, size = 1.0) +
  autolayer(fitted(energy_hwes_tr_val), series = "Fitted", size = 1.0) +
  autolayer(test, color = "green", size = 1.0) +
  ggtitle("Forecast vs. Observed Values for Test Data") +
  xlab("Time (Months)") +
  ylab("Energy (GHz)") +
  xlim(2015, NA) +
  geom_vline(xintercept = 2020.877, size = 1.0, color = "black", linetype = "dashed") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


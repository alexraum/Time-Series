### Cheking for Stationarity

# set the working directory
setwd("C:/Users/alexr/OneDrive/Documents/NCSU/MSA Program/Fall 2021/AA502/Time Series/DataR")

# import data set
library(tidyverse)
library(dplyr)
library(forecast)

# read in the data set
energyData = read.csv("UK.csv")


## Part 1: Check the stationarity of the monthly generation of hydro-electricity

# first, best to visualize the data
energy = ts(energyData$Hydro_energy, start = c(2006, 1), frequency = 12)

autoplot(energy, size = 1.0) +
  ggtitle("Observed Energy vs. Time") +
  xlab("Time (Months)") +
  ylab("Energy (GWh)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# split the data into training, validation, and test data (same as in Homework 2)

# all but the last 17 months used for training data
training = subset(energy, end = length(energy)-17)
# first 12 of the last 17 months used for validation data
validation = subset(energy, start = length(energy)-16, end = length(energy)-5)
# last 5 observations used for test data
test = subset(energy, start = length(energy)-4)

# create seasonally adjusted training data by subtracting out seasonality component
decomp_stl <- stl(training, s.window = 7)
seas_adj <- training - decomp_stl$time.series[,1]

# store just the seasonal component of the training data
seas <- decomp_stl$time.series[,1]
# plot the seasonal component
autoplot(seas, size = 1.0)

# run augmented Dickey-Fuller Test on training data
aTSA::adf.test(training)
# run augmented Dickey-Fuller Test on seasonally adjusted training data
aTSA::adf.test(seas_adj)

# observing the p-values for the Type 2 output, we can reject the null hypothesis 
# and conclude that the data is stationary (we observe type 2 output since we 
# have a nonzero mean and no visible trend when examining the data). Since data 
# is stationary, differencing is not required.


## Part 2: Provide information on what information you are seeing in the 
##            autocorrelation and partial autocorrelation plots

# plot autocorrelation between observations for up to ten lags 
acf = Acf(training, lag=50)$acf
# plot autocorrelation between observations of seasonally adjusted data for up to ten lags 
seas_acf = Acf(seas_adj, lag=10)$acf

# plot partial autocorrelation between observations for up to ten lags
pacf = Pacf(training, lag=50)$acf
# plot partial autocorrelation between observations of seasonally adjusted data for up to ten lags
seas_pacf = Pacf(seas_adj, lag=10)$acf

# create a ggplot of autocorrelations
idx = seq(1, length(pacf))
corr_data = data.frame(cbind(acf[2:11], pacf, idx))
colnames(corr_data) = c("acf", "pacf", "index")

ggplot(corr_data, aes(x=factor(index), y=acf)) +
  geom_col() +
  labs(x="Lags")

# observing the autocorrelation plots, it appears that there is dependency in the
# data. It appears from the autocorrelation plot that there is a positive autocorrelation
# for the first two lags but that this relationship becomes negative and increases
# in strength for lags four and five. The partial autocorrelation is again positive
# for lag one (0.68253) but is negative for lags two through five. We notice here that
# both autocorrelation and partial autocorrelation is non-zero, thus implying that
# the data is not a random walk. This is in agreement with our result for Problem 1.


## Part 3: Using the stationary time series, does the series exhibit white noise?

# we use the Ljung-Box test to determine if the data is white noise, since the data
# is not seasonal we will go back 10 lags (based on the first 2 problems, we expect
# the data still has a dependency structure and is not merely white noise)

# initialize a vector of NAs the same length as the number of lags
white_lb = rep(NA, 10)

for(i in 1:10) {
  # loop over the length of the vector and populate the ith entry
  # with the Ljung-Box test for the ith lag
  white_lb[i] = Box.test(training, lag=i, type="Ljung-Box", fitdf=0)$p.value
}

# create a data frame of the p-values
white_df = data.frame(cbind(white_lb, idx))
colnames(white_df) = c("pvalues", "Lag")

# plot the p-values
ggplot(white_df, aes(x=factor(Lag), y=pvalues)) +
  geom_col() +
  labs(title="Ljung-Box test p-values",
       x="Lags", y="p-values") +
  coord_cartesian(ylim=c(0, 0.025))

# observing the plot, we see that each of the p-values is approximately zero.
# This is in agreement with our prior results and indicates that a dependency
# structure still exists and that further modeling is needed.
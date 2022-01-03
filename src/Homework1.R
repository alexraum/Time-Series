### Homework 1: Time Series

# set the working directory
setwd("C:/Users/alexr/OneDrive/Documents/NCSU/MSA Program/Fall 2021/AA502/Time Series/Homework1")
# import libraries
library(lubridate)
library(dplyr)
library(forecast)

# read in the data set
ozone <- read.csv("Ozone_Raleigh2.csv")


## Problem 1

# determine the number of days missing

# store days in a vector
days <- ozone$Date
# convert the days to Date objects
days <- as.Date(days, "%m/%d/%Y")
# create a range of days based on min and max Dates
rangeDays <- seq(min(days), max(days), by = 1)

# determine which days are missing in the complete interval
missing <- !(rangeDays %in% days)
# use indexing to obtain these days from the complete interval
missingDays <- rangeDays[missing]
# determine the number of missing days
length(missingDays)


## Problem 2

# determine which year has the most missing days

# use the Year function to extract the year from the missing
# days and store in a data frame
Years <- data.frame("Year" = Year(missingDays))

# use count function the number of observations by year and 
# sort them in descending order
counts <- Years %>%
  count(Year, sort = T)


## Problem 3

# rolling up the monthly data, what is the mean max 8 hour Ozone
# Concentration for January 2017? Keeping precision to 3 decimals.

# add a column to ozone of Dates as Date objects
ozone <- ozone %>%
  mutate(Dates = as.Date(Date, "%m/%d/%Y"))

# use dplyr verbs to determine max 8 hour Ozone Concentration
ozoneConc <- ozone %>%
  filter(Month(Dates) == 1 & Year(Dates) == 2017) %>%
  select(Daily.Max.8.hour.Ozone.Concentration)

# calculate the mean of this column
mean(ozoneConc$Daily.Max.8.hour.Ozone.Concentration)


## Problem 4

# create a time plot of the mean monthly max 8 hour ozone concentration

# create a new column that contains the monthly average of ozone concentrations
ozoneConcMonthly <- ozone %>%
  group_by(Year = Year(Dates), Month = Month(Dates)) %>%
  summarize(Avg = mean(Daily.Max.8.hour.Ozone.Concentration))

# plot the stl time series data
maxOzone <- ts(ozoneConcMonthly$Avg, start = c(2014, 1), frequency = 12)
decomp_stl <- stl(maxOzone, s.window = 7)
plot(decomp_stl)

# plot the time series data
forecast::autoplot(maxOzone)


###################
###################
######## chapter 1
###################
###################
# https://learn.datacamp.com/courses/time-series-analysis-in-r

# Time series analysis in R

ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}
packages <- c("dplyr", "xtable",
              "boot", "tidyr","ggplot2","dbplot", "astsa","xts","lubridate")
ipak(packages)


# Introduction
# Time series: A sequence of data in chronological order
# data is commonly recorded sequentially, over time
# time series data is everywhere

# daily log stock return
# monthly values of the consumer price index (CPI)

# time series data is dated or time stamped in R

# models for time series data
# white noise
# random walk
# autoregression
# simple moving average

# 

# Print the Nile dataset
print(Nile) # Nile is of the class "ts"

# List the number of observations in the Nile dataset
length(Nile)

# Display the first 10 elements of the Nile dataset
head(Nile, 10)

# Display the last 12 elements of the Nile dataset
tail(Nile, 12)

# Plot the Nile data
plot(Nile)

# Plot the Nile data with xlab and ylab arguments
plot(Nile, xlab = "Year", ylab = "River Volume (1e9 m^{3})")

# Plot the Nile data with xlab, ylab, main, and type arguments
plot(Nile, xlab = "Year", ylab = "River Volume (1e9 m^{3})",
     main = "Annual River Nile Volume at Aswan, 1871-1970",
     type = "b")


# What does the time index tell us?
# some data are naturally evenly spaced by time
# the time series, discrete_data shown in the top figure has 20 observations
# with one observation appearing at each of the discrete time indices 1 through 20
# discrete time indexing is appropriate for discrete_data
# 

# Plot the continuous_series using continuous time indexing
par(mfrow=c(2,1))
plot(continuous_time_index, continuous_series, type = "b")

# Make a discrete time index using 1:20 
discrete_time_index <- 1:20

# Now plot the continuous_series using discrete time indexing
plot(discrete_time_index, continuous_series, type = "b")



# Sampling frequency: exact
# Some time series data is exactly evenly spaced (e.g., hourly temperature)
# Some time series is only approximately evenly spaced
# Some time series data is evenly spaced, but with missing values

# Basic assumptions
# Consecutive observations are equally spaced
# Apply a discrete time observation index
# this may only hold approximately

# daily log returns on stock may only be available for weekdays
# montly CPI values are equally spaced by month, not by days
# 

start(Hourly_series) # 1, 1 -- day 1, hour 1
end(Hourly_series) # 1, 1 -- day 1, hour 24

# 
frequency(Hourly_series)

# 
deltat(Hourly_series) # time interval length

# 
time(AirPassengers)
# 
frequency(AirPassengers) # 12 means monthly data?

# 
cycle(AirPassengers) # year, month matrix
# 
# That's right! There are always the exact same number of hours in each day without exception.

# Plot the AirPassengers data
plot(AirPassengers)

# Compute the mean of AirPassengers
mean(AirPassengers, na.rm = TRUE)

# Impute mean values to NA in AirPassengers
AirPassengers[85:96] <- mean(AirPassengers, na.rm = TRUE)

# Generate another plot of AirPassengers
plot(AirPassengers)

# Add the complete AirPassengers data to your plot
rm(AirPassengers)
points(AirPassengers, type = "l", col = 2, lty = 3)


############################################################
############################################################
###### Basic time series
############################################################
############################################################

# Building ts() objects
# start with a vector of data
# apply the ts() function

# by default, R uses a simple observation index

# 
data_vector <- rnorm(100, 0, 1)

#
time_series <- ts(data_vector)

# 
plot(time_series)

##############################################################
# Building ts() objects
# with a starting point
time_series <- ts(data_vector, start = 2001, frequency = 1)

plot(time_series)

# is.ts()
# checks whether an object is of the ts() class
# 
is.ts(data_vector)
is.ts(time_series)

# why ts() objects
# there are many functions available 
# improved plotting
# access to time index information
# model estimation and forecasting (later chapters)

# 

# Creating a time series object with ts()
# 
# The function ts() can be applied to create time series objects. 
# A time series object is a vector (univariate) or matrix (multivariate) with additional attributes, including time indices for each observation, 
# the sampling frequency and time increment between observations, and the cycle length for periodic data. 
# Such objects are of the ts class, and represent data that has been observed at (approximately) equally spaced time points. 
# Now you will create time series objects yourself.
# 
# The advantage of creating and working with time series objects of the ts class 
# is that many methods are available for utilizing time series attributes, such as time index information. For example, 
# as you've seen in earlier exercises, calling plot() on a ts object will automatically generate a plot over time.
# 
# In this exercise, you'll familiarize yourself with the ts class by encoding some time series data 
# (saved as data_vector) into ts and exploring the result. Your time series data_vector starts in the year 2004 and 
# has 4 observations per year (i.e. it is quarterly data).

# Check whether data_vector and time_series are ts objects
is.ts(data_vector)
is.ts(time_series)


# Check whether Nile is a ts object
is.ts(Nile)

# Check whether AirPassengers is a ts object
is.ts(AirPassengers)


#############################################################################################

# Check whether eu_stocks is a ts object
is.ts(eu_stocks)

# View the start, end, and frequency of eu_stocks
start(eu_stocks)
end(eu_stocks)
frequency(eu_stocks)



# Generate a simple plot of eu_stocks
plot(eu_stocks)

# Use ts.plot with eu_stocks
ts.plot(eu_stocks, col = 1:4, xlab = "Year", ylab = "Index Value", main = "Major European Stock Indices, 1991-1998")

# Add a legend to your ts.plot
legend("topleft", colnames(eu_stocks), lty = 1, col = 1:4, bty = "n")

#
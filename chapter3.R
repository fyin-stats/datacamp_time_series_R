################# chapter 3
################# correlation analysis and autocorrelation
#################
ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}
packages <- c("dplyr", "xtable",
              "boot", "tidyr","ggplot2","dbplot", "astsa","xts","lubridate","Ecdat")
ipak(packages)


## stock prices
## 
ts.plot(cbind(stock_A, stock_B))

## scatterplot of prices stock B vs A
## stock prices are correlated
plot(stock_A, stock_B)

## Log returns for stock A and B
## 
stock_A_logreturn = diff(log(stock_A)) # first log, then figure out the diff
stock_B_logreturn = diff(log(stock_B))

## 
plot(stock_A_logreturn, stock_B_logreturn) # positive relationship between log returns of these two stocks

## 
# Asset prices vs. asset returns
# 
# The goal of investing is to make a profit. The revenue or loss from investing depends on the amount invested and changes in prices, and high revenue relative to the size of an investment is of central interest. This is what financial asset returns measure, changes in price as a fraction of the initial price over a given time horizon, for example, one business day.
# 
# Let's again consider the eu_stocks dataset. This dataset reports index values, which we can regard as prices. The indices are not investable assets themselves, but there are many investable financial assets that closely track major market indices, including mutual funds and exchange traded funds.
# 
# Log returns, also called continuously compounded returns, are also commonly used in financial time series analysis. They are the log of gross returns, or equivalently, the changes (or first differences) in the logarithm of prices.
# 
# The change in appearance between daily prices and daily returns is typically substantial, while the difference between daily returns and log returns is usually small. As you'll see later, one advantage of using log returns is that calculating multi-period returns from individual periods is greatly simplified - you just add them together!
#     
#     In this exercise, you'll further explore the eu_stocks dataset, including plotting prices, converting prices to (net) returns, and converting prices to log returns.
# Instructions


# Plot eu_stocks
plot(EuStockMarkets)

# Use this code to convert prices to returns
returns <- eu_stocks[-1,] / eu_stocks[-1860,] - 1

# Convert returns to ts
returns <- ts(returns, start = c(1991, 130), frequency = 260)

# Plot returns
plot(returns)

# Use this code to convert prices to log returns
logreturns <- diff(log(eu_stocks))

# Plot logreturns
plot(logreturns)


# 

# Characteristics of financial time series
# 
# Daily financial asset returns typically share many characteristics. Returns over one day are typically small, and their average is close to zero. At the same time, their variances and standard deviations can be relatively large. Over the course of a few years, several very large returns (in magnitude) are typically observed. These relative outliers happen on only a handful of days, but they account for the most substantial movements in asset prices. Because of these extreme returns, the distribution of daily asset returns is not normal, but heavy-tailed, and sometimes skewed. In general, individual stock returns typically have even greater variability and more extreme observations than index returns.
# 
# In this exercise, you'll work with the eu_percentreturns dataset, which is the percentage returns calculated from your eu_stocks data. For each of the four indices contained in your data, you'll calculate the sample mean, variance, and standard deviation.
# 
# Notice that the average daily return is about 0, while the standard deviation is about 1 percentage point. Also apply the hist() and qqnorm() functions to make histograms and normal quantile plots, respectively, for each of the indices.

# 

# Generate means from eu_percentreturns
colMeans(eu_percentreturns)

# Use apply to calculate sample variance from eu_percentreturns
apply(eu_percentreturns, MARGIN = 2, FUN = var)

# Use apply to calculate standard deviation from eu_percentreturns
apply(eu_percentreturns, 2, sd)

# Display a histogram of percent returns for each index
par(mfrow = c(2,2))
apply(eu_percentreturns, MARGIN = 2, FUN = hist, main = "", xlab = "Percentage Return")

# Display normal quantile plots of percent returns for each index
par(mfrow = c(2,2))
apply(eu_percentreturns, MARGIN = 2, FUN = qqnorm, main = "")
qqline(eu_percentreturns)

## 

# Make a scatterplot of DAX and FTSE
plot(DAX, FTSE)

# Make a scatterplot matrix of eu_stocks
pairs(eu_stocks)

# Convert eu_stocks to log returns
logreturns <- diff(log(eu_stocks))

# Plot logreturns
plot(logreturns)

# Make a scatterplot matrix of logreturns
pairs(logreturns) # pairs command is a useful way to quickly check for relationships between
# your indices

################################
### covariance and correlation
#################################
mean(stock_A)
sd(stock_A)


cov(stock_A, sotck_B)

#################################
#################################
## correlation: standardized version of covariance that does not depend on the scales of variables
#################################
#################################

# +1 : perfectly positive linear relationship
# -1 : perfectly negative linear relationship
# 0 : no linear association

# 
cor(stock_A, stock_B)

# cov(stock_A, stock_B) / (sd(stock_A) * sd(Stock_B))

# covariance and correlation: log returns

# 
# Use cov() with DAX_logreturns and FTSE_logreturns
cov(DAX_logreturns, FTSE_logreturns)

# Use cov() with logreturns
cov(logreturns)

# Use cor() with DAX_logreturns and FTSE_logreturns
cor(DAX_logreturns, FTSE_logreturns)

# Use cor() with logreturns
cor(logreturns)

##############################
##############################
### Autocorrelation
##############################
##############################
# Processes with higher autocorrelation are more predictable than those without
# 

# Lag 1 autocorrelation
# correlation of stock A today and stock A yesterday
cor(stock_A[-100],
    stock_A[-1])

# Lag 2 autocorrelation
# stock A today and stock A two days earlier
# correlation between their pairs

# autocorrelation function 
acf(stock_A, plot=FALSE)


# autocorrelation function 2
acf(stock_A, plot = TRUE)

# 

# Define x_t0 as x[-1]
x_t0 <- x[-1]

# Define x_t1 as x[-n]
x_t1 <- x[-n]

# Confirm that x_t0 and x_t1 are (x[t], x[t-1]) pairs  
head(cbind(x_t0, x_t1))

# Plot x_t0 and x_t1
plot(x_t0, x_t1)

# View the correlation between x_t0 and x_t1
cor(x_t0, x_t1)

# Use acf with x
acf(x, lag.max = 1, plot = FALSE)

# Confirm that difference factor is (n-1)/n
cor(x_t1, x_t0) * (n-1)/n


# 
# Visualizing the autocorrelation function
# 
# Estimating the autocorrelation function (ACF) at many lags allows us to assess how a time series x relates to its past. The numeric estimates are important for detailed calculations, but it is also useful to visualize the ACF as a function of the lag.
# 
# In fact, the acf() command produces a figure by default. It also makes a default choice for lag.max, the maximum number of lags to be displayed.
# 
# Three time series x, y, and z have been loaded into your R environment and are plotted on the right. The time series x shows strong persistence, meaning the current value is closely relatively to those that proceed it. The time series y shows a periodic pattern with a cycle length of approximately four observations, meaning the current value is relatively close to the observation four before it. The time series z does not exhibit any clear pattern.
# 
# In this exercise, you'll plot an estimated autocorrelation function for each time series. In the plots produced by acf(), the lag for each autocorrelation estimate is denoted on the horizontal axis and each autocorrelation estimate is indicated by the height of the vertical bars. Recall that the ACF at lag-0 is always 1.
# 
# Finally, each ACF figure includes a pair of blue, horizontal, dashed lines representing lag-wise 95% confidence intervals centered at zero. These are used for determining the statistical significance of an individual autocorrelation estimate at a given lag versus a null value of zero, i.e., no autocorrelation at that lag.


# 
# Great job! Plotting the estimated ACF of x shows large positive correlations for several lags which quickly decay towards zero. Plotting the estimated ACF of y shows large positive correlations at lags which are multiples of four, although these also decay towards zero as the lag multiple increases. Finally, the estimated ACF of z is near zero at all lags. 
# It appears the series z is not linearly related to its past, at least through lag 20. 


# autoregressive model - I
# The autoregressive recursion
# Today = constant + slope * Yesterday + Noise
# mean centered version 
# today - constant = slope * Yesterday + Noise
# If slope = 0, Yt is white noise
# If slope != 0, Yt dpends on both epsilon t and Y_t-1

# and the process Yt is autocorrelated
# large values of phi lead to greater autocorrelation
# negative vaues of phi result in oscillatory time series

# autocorrelations
# phi = 1, Yt is not stationary in this case
# 

# Simulate an AR model with 0.5 slope
x <- arima.sim(model = list(order = c(1,0,0), ar = 0.5), n = 100)

# Simulate an AR model with 0.9 slope
y <- arima.sim(model = list(order = c(1,0,0), ar = 0.9), n = 100)

# Simulate an AR model with -0.75 slope
z <- arima.sim(model = list(order =c (1,0,0), ar = -0.75), n=100)

# Plot your simulated data
plot.ts(cbind(x, y, z))


#
# Calculate the ACF for x
acf(x)

# Calculate the ACF for y
acf(y)

# Calculate the ACF for z
acf(z)

##################################################
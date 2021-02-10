#####################
#####################
############# chapter2
#####################
#####################

#####################
#####################
# Trends: linear
#####################
#####################


# trends: linear, rapid growth (increase more quickly than linear)
# trends: periodic or sinusodial trends
# trends: variance (increasing variance over time)

# sample transformations: log()
# can linearize a rapid growth trend
# can also stablize a series that exhibits increasing variance
# main restriction: defined only for positive


# sample transformations: diff()
# can remove a linear trend

# sample transformations: diff(,s)
# can remove periodic trends
# 

# random or not random
# Not quite! Time series A and C both show some very 
# short-term persistence that may be predictable. When assessing predictability, try to determine if there is a 
# consistent relationship between one data point and the next. 
# Time series D and F do not show any obvious predictability. 
# While these series may be predictable, 
# the underlying relationships are not as immediately obvious 
# as the other series. 



# white noise model
# WN model is the simplest example of a stationary process
# a weak white noise process has
# a fixed, constant mean
# a fixed, constant variance

# no correlation over time

# keys: trend, periodic, variability 

# white noise time series
WN_1 <- arima.sim(model = list(order = c(0,0,0)), n=50)
WN_2 <- arima.sim(model = list(order = c(0,0,0)), n=50, mean=4, sd=2)
ts.plot(WN_2)

# Estimating white noise
# fit the WN model with arima()
arima(WN_2,
      order = c(0,0,0))

# fit the WN model with sarima()
sarima(WN_2, p=0, d=0, q=0)


# 
mean(WN_2)

# 
var(WN_2)


# 



# random walk
# random model: defined recursively
# Y_t = Y_t-1 + epsilon_t

# simulation requires an initial point Y0
# only one parameter, the WN variance sigma^2

# random walk 1:
# random walk process: Y_t = Y_t-1 + epsilon_t
# Y_t - Y_t-1, diff(Y) is WN
# 
# random walk 2:
# diff(Y) is WN if Y is random walk

# random walk with a drift
# Today = Constant + Yesterday + Noise
# Y_t = c + Y_t-1 + epsilon_t
# where epsilon_t is mean zero white noise (WN)
# two parameters, the constant c, and the WN variance sigma^2
# Y_t - Y_t-1 = WN with mean c

# 
# Generate a RW model using arima.sim
random_walk <- arima.sim(model = list(order = c(0,1,0)), n = 100)

# Plot random_walk
ts.plot(random_walk)

# Calculate the first difference series
random_walk_diff <- diff(random_walk)

# Plot random_walk_diff
ts.plot(random_walk_diff)


# Generate a RW model with a drift uing arima.sim
rw_drift <- arima.sim(model = list(order = c(0, 1, 0)), n = 100, mean = 1)

# Plot rw_drift
ts.plot(rw_drift)

# Calculate the first difference series
rw_drift_diff <- diff(rw_drift) 

# Plot rw_drift_diff
ts.plot(rw_drift_diff)

# 
# Difference your random_walk data
rw_diff <- diff(random_walk)

# Plot rw_diff
ts.plot(rw_diff)

# Now fit the WN model to the differenced data
model_wn <- arima(rw_diff,
                  order = c(0,0,0))

# Store the value of the estimated time trend (intercept)
int_wn <- model_wn$coef

# Plot the original random_walk data
ts.plot(random_walk)

# Use abline(0, ...) to add time trend to the figure
abline(0, int_wn)


##############################################
##############################################
## Stationarity
##############################################
##############################################
# for parsimonious purpose, make assumptions
# stationary models are parsimonious
# stationary processes have distributional stability over time

# observed time series:
# fluctuate randomly
# but behave similarly from one time period to the next

# weak stationary: mean, variance, covariance constant over time.
# Y1, Y2, ..., is a weakly stationary process if
# mean of Yt is same (constant) for all t
# variance of Yt is same (constant) for all t

# covariance of Yt and Ys is same for all |t-s| = h, for all h
# cov(Y2, Y6) = cov(Y7, Y10)
# since each pair is separated by three units of time

# why stationary models?
# A stationary process can be modeled with fewer parameters
# for example, we do not need a different expectation for each Yt; rather they all have a common expectation mu
# estimate mu accurately by ybar
# stationary: when?
# many financial time series do not exhibit stationarity, however
# the changes in the series are often approximately stationary
# a staionary series should show random oscillation around some fixed level, a phenomenon called mean-reversion
# for example, inflation rates vs changes in inflation rates
# for changes in inflation rates, you see a very quick reversion to the common mean
# and there is no clear pattern over time

# Exactly! Time series B is not periodic but does appear to be mean reverting. 
# Of the options, this is the most likely to be stationary. 

# Use arima.sim() to generate WN data
white_noise <- arima.sim(model = list(order = c(0,0,0)), n=100)

# Use cumsum() to convert your WN data to RW
random_walk <- cumsum(white_noise)

# Use arima.sim() to generate WN drift data
wn_drift <- arima.sim(model = list(order = c(0,0,0)), n=100, mean = 0.4)

# Use cumsum() to convert your WN drift data to RW
rw_drift <- cumsum(wn_drift)

# Plot all four data objects
plot.ts(cbind(white_noise, random_walk, wn_drift, rw_drift))

########################
########################
######## Chapter 5
########################
########################


### the simple moving average model
### today = mean + noise + slope * (yesterday's noise)
### More formally: Y_t = mu + \epsilon_t + \theta * \epsilon_{t-1}
### If theta is not zero, then Yt depends on both epsilon_t and epsilon_t-1
### And the process Yt is autocorrelated 
### large values of theta lead to greater autocorrelation
### negative values of theta result in oscillatory time series

### theta = 0, white noise
### autocorrelations for MA(1), only lag 1 autocorrelation non-zero for the MA model
### 

# Generate MA model with slope 0.5
x <- arima.sim(model = list(order = c(0,0,1), ma = 0.5), n = 100)

# Generate MA model with slope 0.9
y <- arima.sim(model = list(order = c(0,0,1), ma = 0.9), n=100)

# Generate MA model with slope -0.5
z <- arima.sim(model = list(order = c(0,0,1), ma = -0.5), n=100)

# Plot all three models together
plot.ts(cbind(x, y, z))

# Calculate ACF for x
acf(x)

# Calculate ACF for y
acf(y)

# Calculate ACF for z
acf(z)


# MA model estimation and forecasting
# One-month US inflation rate (in percent, annual rate)
# monthly observations from 1950 through 1990
# 
data(Mishkin, package = "Ecdat")
inflation <- as.ts(Mishkin[,1])
inflation_changes <- diff(inflation)
ts.plot(inflation)
ts.plot(inflation_changes)


# MA processes: changes in inflation rate - II
# 
ts.plot(inflation_changes)
acf(inflation_changes, lag.max = 24)

# 
# Today = Mean + Noise + Slope * (Yesterday's noise)
# Yt = \mu + epsilon_t + epsilon_{t-1}
# epsilon_t ~ white noise
# MA_inflation_changes <- arima(inflation_changes, order = c(0,0,1))
# print(MA_inflation_changes)
# ma1 = theta_hat
# intercept = mu_hat
# sigma_2 = 

# MA fitted values: Today_hat = Mean_hat + slope_hat * yesterday's noise_hat

# residuals 
# Today - Today_hat

# 
ts.plot(inflation_changes)
MA_inflation_changes_fitted <- inflation_changes - residuals(MA_inflation_changes)
# 
points(MA_inflation_changes_fitted, type = "l",
       col = "red", lty = 2)
# forecasting 1-step ahead forecasts:
# 
predict(MA_inflation_changes)$pred

# 
predict(MA_inflation_changes)$se

# h-step ahead forecasts

# n.ahead = 6

# MA model only has memory or autocorrelation for one time lag
# 

# Fit the MA model to x
arima(x, order = c(0, 0, 1))

# Paste the slope (ma1) estimate below
0.7928

# Paste the slope mean (intercept) estimate below
0.1589

# Paste the innovation variance (sigma^2) estimate below
0.9576

# Fit the MA model to Nile
MA <- arima(Nile, order = c(0,0,1))
print(MA)

# Plot Nile and MA_fit 
ts.plot(Nile)
MA_fit <- Nile - resid(MA)
points(MA_fit, type = "l", col = 2, lty = 2)


###################################
# Make a 1-step forecast based on MA
predict_MA <- predict(MA)

# Obtain the 1-step forecast using $pred[1]
predict_MA$pred[1] 

# Make a 1-step through 10-step forecast based on MA
predict(MA, n.ahead = 10)

# Plot the Nile series plus the forecast and 95% prediction intervals
ts.plot(Nile, xlim = c(1871, 1980))
MA_forecasts <- predict(MA, n.ahead = 10)$pred
MA_forecast_se <- predict(MA, n.ahead = 10)$se
points(MA_forecasts, type = "l", col = 2)
points(MA_forecasts - 2*MA_forecast_se, type = "l", col = 2, lty = 2)
points(MA_forecasts + 2*MA_forecast_se, type = "l", col = 2, lty = 2)


# compare AR and MA processes
# both have mean zero white noise terms epsilon with a variance parameter sigma_squared
# both include a mean parameter mu
# MA
# Today = Mean + Noise + Slope*(Yesterday's noise)
# AR
# Today - mean = slope * (yesterday - mean) + noise

# where epsilon_t ~ white noise(0, sigma_t^2)

# MA and AR processes: autocorrelations
# AR model
# MA model
# MA and AR processes: simulations
# changes in one-month US inflation rate
# back testing or forecast updating are more advanced concepts
# that can be used to determine which of the two models produces better forecasts for a given data set
# another approach to selecting a model is using a goodness of fit measure such as an information
# criterion
# AIC and BIC are commonly used for time series
# 

# Excellent work! Series A shows short-run dependence but reverts quickly to the mean, 
# so it must be the MA model. Series B and C are consistent with AR and RW, respectively. 
# Series D does not show any clear patterns, so it must be the WN model. 


#
# Exactly right! Plot A shows autocorrelation for the first lag only, 
# which is consistent with the expectations of the MA model. 
# Plot B shows dissipating autocorrelation across several lags, 
# consistent with the AR model. Plot C is consistent with a RW model with 
# considerable autocorrelation for many lags. Finally. Plot D shows virtually no autocorrelation with any lags, 
# consistent with a WN model. 
# Understanding the logic behind these ACF plots is crucial for understanding how each model operates. 

# 
###########################
###########################
########### chapter 4
###########################
###########################

# Persistence and anti-persistence
# 
# Autoregressive processes can exhibit varying levels of persistence as well as anti-persistence or oscillatory behavior. Persistence is defined by a high correlation between an observation and its lag, while anti-persistence is defined by a large amount of variation between an observation and its lag.
# 
# The four plots on the right show varying degrees of persistence and anti-persistence. Which series exhibits the greatest persistence?


# Exactly right! Plot D shows a very high degree of persistence, with a clear downward trend over time.


# Simulate and plot AR model with slope 0.9 
x <- arima.sim(model = list(order = c(1,0,0), ar = 0.9), n = 200)
ts.plot(x)
acf(x)

# Simulate and plot AR model with slope 0.98
y <- arima.sim(model = list(order = c(1,0,0), ar = 0.98), n = 200)
ts.plot(y)
acf(y)

# Simulate and plot RW model
z <- arima.sim(model = list(order = c(0,1,0)), n=200)
ts.plot(z)
acf(z)

###########################################
###########################################
###########################################
######## AR model estimation and forecasting
###########################################
###########################################
###########################################

# AR processes: inflation rate
# one-month US inflation rate (in percent, annual rate)
# montly observations from 1950 through 1990
# 

data(Mishkin, package = "Ecdat")
inflation <- as.ts(Mishkin[,1])
ts.plot(inflation)
acf(inflation)


########################################
## 
# Today - Mean = slope * (yesterday - mean) + noise
# 
AR_inflation <- arima(inflation, order = c(1,0,0))
# 
print(AR_inflation)

# AR fitted values
# Today_hat = Mean_hat + slope_hat * (Yesterday - Mean_hat)

# 
ts.plot(inflation)

# 
AR_inflation_fitted <- inflation - residuals(AR_inflation) # extract residuals

# add fitted values to the figure using points function
points(AR_inflation_fitted, type = "l", col = "red", lty = 2)

# can use predict function to do model-based forecasting
predict(AR_inflation)$pred

# 
predict(AR_inflation)$se

# h-step ahead forecasts
predict(AR_inflation, n.ahead = 6)$pred

# 
predict(AR_inflation, n.ahead = 6)$se

# 
# Fit the AR model to x
arima(x, order = c(1, 0, 0))

# Copy and paste the slope (ar1) estimate
0.8575

# Copy and paste the slope mean (intercept) estimate
-0.0948

# Copy and paste the innovation variance (sigma^2) estimate
1.022

# Fit the AR model to AirPassengers
AR <- arima(AirPassengers, order = c(1, 0, 0))
print(AR)

# Run the following commands to plot the series and fitted values
ts.plot(AirPassengers)
AR_fitted <- AirPassengers - residuals(AR)
points(AR_fitted, type = "l", col = 2, lty = 2)



# Fit an AR model to Nile
AR_fit <- arima(Nile, order  = c(1,0,0))
print(AR_fit)

# Use predict() to make a 1-step forecast
predict_AR <- predict(AR_fit)

# Obtain the 1-step forecast using $pred[1]
predict_AR$pred[1]


# Use predict to make 1-step through 10-step forecasts
predict(AR_fit, n.ahead = 10)

# Run to plot the Nile series plus the forecast and 95% prediction intervals
ts.plot(Nile, xlim = c(1871, 1980))
AR_forecast <- predict(AR_fit, n.ahead = 10)$pred
AR_forecast_se <- predict(AR_fit, n.ahead = 10)$se
points(AR_forecast, type = "l", col = 2)
points(AR_forecast - 2*AR_forecast_se, type = "l", col = 2, lty = 2)
points(AR_forecast + 2*AR_forecast_se, type = "l", col = 2, lty = 2)
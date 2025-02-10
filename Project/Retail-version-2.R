## Forecasting Monthly Retail Trade and Food Services Data in the United States

## use forecast library
library(forecast)
library(zoo)

## Set working directory for locating files.
setwd("/Users/shalvideshmukh/Library/Mobile Documents/com~apple~CloudDocs/My Journal/CSU EB/SPRING 24/BAN 673 Time series/project")

## create data frame 
Retail.data <- read.csv("Retail.csv")

# See the first 6 records of the file.
head(Retail.data)
tail(Retail.data)

## Time series data set
retail.ts <- ts(Retail.data$Value, start = c(1992, 1), end = c(2023, 12), freq = 12)
retail.ts

# Use stl() function to plot times series components of the original data. 
# The plot includes original data, trend, seasonal, and reminder (level and noise component).
retail.stl <- stl(retail.ts, s.window = "periodic")
autoplot(retail.stl, main = "Retail Trade and Food Services - Time Series Components")

## Use plot() to plot time series data  
plot(retail.ts, 
     xlab = "Time", ylab = "Sales (in 000s)", 
     ylim = c(162000, 703000), xaxt = 'n',
     main = "Retail Trade and Food Services")
# Establish x-axis scale interval for time in months.
axis(1, at = seq(1992, 2025, 1), labels = format(seq(1992, 2025, 1)))

## Acf() function to identify autocorrelation and plot autocorrelation for different lags.
autocor <- Acf(retail.ts, lag.max = 12, main = "Autocorrelation for Retail Data")

# Display autocorrelation coefficients for various lags.
Lag <- round(autocor$lag, 0)
ACF <- round(autocor$acf, 3)
data.frame(Lag, ACF)

## Data Partition in the training and validation sets: nTrain and nValid
# Total number of period length(ridership.ts) = 384.
# nvalid = 84 months (7 years), from January 2017 to December 2023
# nTrain = 300 months (25 years), from January 1992 to December 2016.
length(retail.ts) 
nValid <- 84
nTrain <- length(retail.ts) - nValid
nTrain
train.ts <- window(retail.ts, start = c(1992, 1), end = c(1992, nTrain))
valid.ts <- window(retail.ts, start = c(1992, nTrain + 1), 
                   end = c(1992, nTrain + nValid))
length(train.ts) 
length(valid.ts) 

# Plot the time series data and visualize partitions. 
plot(train.ts, 
     xlab = "Time", ylab = "Sales (in 000s)", ylim = c(162000, 730000), 
     bty = "l", xlim = c(1992, 2025.25), xaxt = 'n', main = "", lwd = 2) 
axis(1, at = seq(1992, 2025, 1), labels = format(seq(1992, 2025, 1)))
lines(valid.ts, col = "black", lty = 1, lwd = 2)

# Plot on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2017, 2017), c(0, 720000))
lines(c(2024, 2024), c(0, 720000))
text(2008, 700000, "Training", pos = 3)  # Adjusted y-coordinate and pos
text(2020, 700000, "Validation", pos = 3)  # Adjusted y-coordinate and pos
text(2025.5, 700000, "Future", pos = 3)  # Adjusted y-coordinate and pos
arrows(1992, 700000, 2016.9, 700000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.1, 700000, 2023.9, 700000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2024.1, 700000, 2027.3, 700000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

###############################################################################################

##Assignment -1 -1st segment
## Create trailing moving average with window widths of k = 4, 6, and 12.
# In rollmean(), use argument align = "right" to calculate a trailing MA.
ma.trailing_4 <- rollmean(train.ts, k = 4, align = "right")
ma.trailing_6 <- rollmean(train.ts, k = 6, align = "right")
ma.trailing_12 <- rollmean(train.ts, k = 12, align = "right")

## Create forecast for the validation data for the window widths of k = 4, 6, and 12. 
ma.trail_4.pred <- forecast(ma.trailing_4, h = nValid, level = 0)
ma.trail_4.pred

ma.trail_6.pred <- forecast(ma.trailing_6, h = nValid, level = 0)
ma.trail_6.pred

ma.trail_12.pred <- forecast(ma.trailing_12, h = nValid, level = 0)
ma.trail_12.pred

## Use accuracy() function to identify common accuracy measures.
round(accuracy(ma.trail_4.pred$mean, valid.ts), 3)
round(accuracy(ma.trail_6.pred$mean, valid.ts), 3)
round(accuracy(ma.trail_12.pred$mean, valid.ts), 3) #better performance-use k=12 for two level

## GENERATE PLOT FOR PARTITION DATA AND TRAILING MA.

# Plot original data and forecast for training and validation partitions
# using trailing MA with window widths of k = 4 and k = 12.
plot(retail.ts, 
     xlab = "Time", ylab = "Sales (in 000s)", 
     ylim = c(162000, 730000), bty = "l", xaxt = "n",
     xlim = c(1992, 2025.25), main = "Trailing Moving Average") 
axis(1, at = seq(1992, 2025, 1), labels = format(seq(1992, 2025, 1)) )
lines(ma.trailing_12, col = "blue", lwd = 2, lty = 1)
lines(ma.trail_12.pred$mean, col = "blue", lwd = 2, lty = 2)
legend(1992,700000, legend = c("Sales Data", 
                             "Trailing MA, k=12, Training Partition", 
                             "Trailing MA, k=12, Validation Partition"), 
       col = c("black", "blue", "blue"), 
       lty = c(1, 1, 2, 1, 2), lwd =c(1, 2, 2, 2, 2), bty = "n")

# Plot on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2017, 2017), c(0, 720000))
lines(c(2024, 2024), c(0, 720000))
text(2008, 700000, "Training", pos = 3)  # Adjusted y-coordinate and pos
text(2020, 700000, "Validation", pos = 3)  # Adjusted y-coordinate and pos
text(2025.5, 700000, "Future", pos = 3)  # Adjusted y-coordinate and pos
arrows(1992, 700000, 2015.9, 700000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.1, 700000, 2023.9, 700000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2024.1, 700000, 2027.3, 700000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#############################################################################################

## Fit a regression model with linear trend and seasonality for training partition. 
lin.trend.seas <- tslm(train.ts ~ trend  + season)
summary(lin.trend.seas)

# Create regression forecast with linear trend and seasonality for validation period.
lin.trend.seas.pred <- forecast(lin.trend.seas, h = nValid, level = 0)
lin.trend.seas.pred

## Identify and display residuals based on the regression model in training period.
lin.trend.seas.res <- lin.trend.seas$residuals
lin.trend.seas.res

# Apply trailing MA for residuals with window width k = 12. 
ma.trail.res <- rollmean(lin.trend.seas.res, k = 12, align = "right")
ma.trail.res

# Regression residuals in validation period.
lin.trend.seas.res.valid <- valid.ts - lin.trend.seas.pred$mean
lin.trend.seas.res.valid

# Create residuals forecast for validation period.
ma.trail.res.pred <- forecast(ma.trail.res, h = nValid, level = 0)
ma.trail.res.pred

## Develop two-level forecast for validation period by combining  
# regression forecast and trailing MA forecast for residuals.
fst.2level <- lin.trend.seas.pred$mean + ma.trail.res.pred$mean
fst.2level

# Create a table for validation period: validation data, regression 
# forecast, trailing MA for residuals and total forecast.
valid.df <- round(data.frame(valid.ts, lin.trend.seas.pred$mean, 
                             ma.trail.res.pred$mean, 
                             fst.2level), 3)
names(valid.df) <- c("Retail Sales", "Regression.Fst", 
                     "MA.Residuals.Fst", "Combined.Fst")
valid.df

# Use accuracy() function to identify common accuracy measures.
round(accuracy(lin.trend.seas.pred$mean, valid.ts), 3)
round(accuracy(fst.2level, valid.ts), 3)

## 3d.Fit a regression model with linear trend and seasonality for entire data set.
tot.trend.seas <- tslm(retail.ts ~ trend + season)
summary(tot.trend.seas)

# Create regression forecast for future 12 periods.
tot.trend.seas.pred <- forecast(tot.trend.seas, h = 12, level = 0)
tot.trend.seas.pred

# Identify and display regression residuals for entire data set.
tot.trend.seas.res <- tot.trend.seas$residuals
tot.trend.seas.res

# Use trailing MA to forecast residuals for entire data set.
tot.ma.trail.res <- rollmean(tot.trend.seas.res, k = 12, align = "right")
tot.ma.trail.res

# Create forecast for trailing MA residuals for future 12 periods.
tot.ma.trail.res.pred <- forecast(tot.ma.trail.res, h = 12, level = 0)
tot.ma.trail.res.pred

# Develop 2-level forecast for future 12 periods by combining 
# regression forecast and trailing MA for residuals for future 12 periods.
tot.fst.2level <- tot.trend.seas.pred$mean + tot.ma.trail.res.pred$mean
tot.fst.2level

# Create a table with regression forecast, trailing MA for residuals,
# and total forecast for future 12 periods.
future12.df <- data.frame(tot.trend.seas.pred$mean, tot.ma.trail.res.pred$mean, 
                          tot.fst.2level)
names(future12.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
future12.df

## Use accuracy() function to identify common accuracy measures.
round(accuracy(tot.trend.seas.pred$fitted, retail.ts), 3)
round(accuracy(tot.trend.seas.pred$fitted+tot.ma.trail.res, retail.ts), 3)
round(accuracy((snaive(retail.ts))$fitted, retail.ts), 3)

## GENERATE PLOT OF ORIGINAL DATA AND REGRESSION FORECAST, AND PREDICTIONS
## IN FUTURE 12 PERIODS.
## GENERATE PLOT OF REGRESSION RESIDUALS, TRAILING MA FOR RESIDUALS, AND 
## TRAILING MA FORECAST IN FUTURE 12 PERIODS.

# Plot original Ridership time series data and regression model.
plot(retail.ts, 
     xlab = "Time", ylab = "Sales (in 000s)", ylim = c(162000, 730000), 
     bty = "l", xlim = c(1992, 2025.25), lwd =1, xaxt = "n",
     main = "Retail Sales Data and Regression with Trend and Seasonality") 
axis(1, at = seq(1992, 2025.25, 1), labels = format(seq(1992, 2025.25, 1)))
lines(tot.trend.seas$fitted, col = "blue", lwd = 2)
lines(tot.trend.seas.pred$mean, col = "blue", lty =5, lwd = 2)
legend(1992,700000, legend = c("Retail Sales", "Regression",
                             "Regression Forecast for Future 12 Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on chart vertical lines and horizontal arrows describing
# entire data set and future prediction intervals.
lines(c(2024, 2024), c(0, 730000))
text(2005, 700000, "Data Set", pos = 3)
text(2025.2, 700000, "Future", pos = 3)
arrows(1992, 700000, 2023.9, 700000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2024.1, 700000, 2026.3, 700000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


# Plot regression residuals data and trailing MA based on residuals.
plot(tot.trend.seas.res, 
     xlab = "Time", ylab = "Sales (in 000s)", ylim = c(-81000, 81000), 
     bty = "l", xaxt = "n", xlim = c(1992, 2025.25), lwd =1, col = "brown", 
     main = "Regression Residuals and Trailing MA for Residuals") 
axis(1, at = seq(1992, 2025.25, 1), labels = format(seq(1992, 2025.25, 1)))
lines(tot.ma.trail.res, col = "blue", lwd = 2, lty = 1)
lines(tot.ma.trail.res.pred$mean, col = "blue", lwd = 2, lty = 2)
legend(1992,700000, legend = c("Regresssion Residuals", 
                             "Trailing MA (k=12) for Residuals", 
                             "Trailing MA Forecast (k=12) for Future 12 Periods"), 
       col = c("brown", "blue", "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on chart vertical lines and horizontal arrows describing
# entire data set and future prediction intervals.
lines(c(2024, 2024), c(-81000, 81000))
text(2005, 70000, "Data Set", pos = 3)
text(2025.2, 70000, "Future", pos = 3)
arrows(1992, 70000, 2023.9, 70000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2024.1, 70000, 2026.3, 70000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#########################################################################################

## Use ets() function with model = "ZZZ"- automated selection of
# error, trend, and seasonality options & use optimal alpha, beta, & gamma
hw.ZZZ <- ets(train.ts, model = "ZZZ")
hw.ZZZ 

# Use forecast() function to make predictions using this HW model with validation period (nValid).
hw.ZZZ.pred <- forecast(hw.ZZZ, h = nValid, level = 0)
hw.ZZZ.pred

## forecast in the 12 months of 2024 using the hw model for entire data
# Use ets() function with model = "ZZZ"
HW.ZZZ <- ets(retail.ts, model = "ZZZ")
HW.ZZZ 

# Use forecast() function to make predictions using this hw model for 12 month into the future.
HW.ZZZ.pred <- forecast(HW.ZZZ, h = 12 , level = 0)
HW.ZZZ.pred

## Performance measures for seasonal naive and hw forecast
round(accuracy(snaive(retail.ts)$fitted, retail.ts), 3)
round(accuracy(HW.ZZZ.pred$fitted, retail.ts), 3)

## Performance measures for two-level combined model and  hw forecast
round(accuracy(tot.trend.seas.pred$fitted+tot.ma.trail.res, retail.ts), 3)
round(accuracy(HW.ZZZ.pred$fitted, retail.ts), 3)

# plot HW predictions for original data, optimal smoothing parameters.
plot(HW.ZZZ.pred$mean, 
     xlab = "Time", ylab = "Sales (in 000s)", ylim = c(162000, 730000), 
     bty = "l", xlim = c(1992, 2025.25), xaxt = "n",
     main = "Holt-Winter's Automated Model for Entire Data Set and Forecast for Future 12 Periods", 
     lty = 2, col = "blue", lwd = 2) 
axis(1, at = seq(1992, 2025.25, 1), labels = format(seq(1992, 2025.25, 1)))
lines(HW.ZZZ.pred$fitted, col = "blue", lwd = 2)
lines(retail.ts)
legend(1992,700000, 
       legend = c("Retail Sales", 
                  "Holt-Winter'sModel for Entire Data Set",
                  "Holt-Winter's Model Forecast, Future 12 Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on chart vertical lines and horizontal arrows describing
# entire data set and future prediction intervals.
lines(c(2024, 2024), c(0, 730000))
text(2005, 700000, "Data Set", pos = 3)
text(2025.2, 700000, "Future", pos = 3)
arrows(1992, 700000, 2023.9, 700000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2024.1, 700000, 2026.3, 700000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

###################################################################################################

## Assignment 2 - 2nd segment
## 5 REGRESSION MODELS

## FIT REGRESSION MODEL WITH LINEAR TREND: MODEL 1. 
# Use tslm() function to create linear trend model.
train.lin <- tslm(train.ts ~ trend)

# See summary of linear trend model and associated parameters.
summary(train.lin)

# Apply forecast() function to make predictions for ts data in validation set.  
train.lin.pred <- forecast(train.lin, h = nValid, level = 0)
train.lin.pred

## FIT REGRESSION MODEL WITH QUADRATIC (POLYNOMIAL) TREND: MODEL 2. 
# Use tslm() function to create quadratic (polynomial) trend model.
train.quad <- tslm(train.ts ~ trend + I(trend^2))

# See summary of quadratic trend model and associated parameters.
summary(train.quad)

# Apply forecast() function to make predictions for ts data in validation set.  
train.quad.pred <- forecast(train.quad, h = nValid, level = 0)
train.quad.pred

## FIT REGRESSION MODEL WITH SEASONALITY: MODEL 3.
# Use tslm() function to create seasonal model.
train.season <- tslm(train.ts ~ season)

# See summary of seasonal model and associated parameters.
summary(train.season)

# If necessary, run the following code to identify seasons.
train.season$data 

# Apply forecast() function to make predictions for ts with seasonality data in validation set.  
train.season.pred <- forecast(train.season, h = nValid, level = 0)
train.season.pred

## FIT REGRESSION MODEL WITH LINEAR TREND AND SEASONALITY: MODEL 4.
# Use tslm() function to create linear trend and seasonal model.
train.lin.trend.season <- tslm(train.ts ~ trend  + season)

# See summary of linear trend and seasonality model and associated parameters.
summary(train.lin.trend.season)

# Apply forecast() function to make predictions for ts with linear trend and seasonality data in validation set.  
train.lin.trend.season.pred <- forecast(train.lin.trend.season, h = nValid, level = 0)
train.lin.trend.season.pred

## FIT REGRESSION MODEL WITH QUADRATIC TREND AND SEASONALITY: MODEL5
# Use tslm() function to create quadratic trend and seasonal model.
train.quad.trend.season <- tslm(train.ts ~ trend + I(trend^2) + season)

# See summary of quadratic trend and seasonality model and associated parameters.
summary(train.quad.trend.season)

# Apply forecast() function to make predictions for ts with quadratic trend and seasonality data in validation set.  
train.quad.trend.season.pred <- forecast(train.quad.trend.season, h = nValid, level = 0)
train.quad.trend.season.pred

## Performance measures of the 5 forecasts
# Use accuracy() function to identify common accuracy measures
round(accuracy(train.lin.pred$mean, valid.ts),3)
round(accuracy(train.quad.pred$mean, valid.ts),3)
round(accuracy(train.season.pred$mean, valid.ts),3)
round(accuracy(train.lin.trend.season.pred$mean, valid.ts),3)
round(accuracy(train.quad.trend.season.pred$mean, valid.ts),3)

## Time series forecast with the three most accurate regression models.

## 1. LINEAR TREND AND SEASONALITY MODEL.
# Use tslm() function to create linear trend and seasonal model.
lin.trend.season <- tslm(retail.ts ~ trend + season)

# See summary of linear trend and seasonality equation and associated parameters.
summary(lin.trend.season)

# Apply forecast() function to make predictions for ts with trend and seasonality data in the future 12 months.  
lin.trend.season.pred <- forecast(lin.trend.season, h = 12, level = 0)
lin.trend.season.pred

## 2. LINEAR TREND MODEL.
# Use tslm() function to create linear trend model.
lin.trend <- tslm(retail.ts ~ trend)

# See summary of linear trend equation and associated parameters.
summary(lin.trend)

# Apply forecast() function to make predictions in the future 12 months.  
lin.trend.pred <- forecast(lin.trend, h = 12, level = 0)
lin.trend.pred

## 3. QUADRATIC TREND AND SEASONALITY MODEL
# Use tslm() function to create quadratic trend and seasonality model.
quad.trend.season <- tslm(retail.ts ~ trend + I(trend^2)+ season)

# See summary of quadratic trend and seasonality equation and associated parameters.
summary(quad.trend.season)

# Apply forecast() function to make predictions for ts with trend and seasonality data in the future 12 months.  
quad.trend.season.pred <- forecast(quad.trend.season, h = 12, level = 0)
quad.trend.season.pred

## Performance measure of the forecasts 
round(accuracy(lin.trend.season.pred$fitted, retail.ts),3)
round(accuracy(lin.trend.pred$fitted, retail.ts),3)
round(accuracy(quad.trend.season.pred$fitted, retail.ts),3)
round(accuracy((naive(retail.ts))$fitted, retail.ts), 3)
round(accuracy((snaive(retail.ts))$fitted, retail.ts), 3)

####################################################################################################################

## Assignment 3 - 3rd segment

## Time series predictability

## Use Arima() function to fit AR(1) model for walmart revenues.
# The ARIMA model of order = c(1,0,0) gives an AR(1) model.
retail.ar1<- Arima(retail.ts, order = c(1,1,0))
summary(retail.ar1)

# Apply z-test to test the null hypothesis that beta coefficient of AR(1) is equal to 1.
ar1 <- 0.032
s.e. <- 0.051
null_mean <- 1
alpha <- 0.05
z.stat <- (ar1-null_mean)/s.e.
z.stat
p.value <- pnorm(z.stat)
p.value
if (p.value<alpha) {
  "Reject null hypothesis"
} else {
  "Accept null hypothesis"
}

## 1.b.Create first difference of walmart revenue data using diff() function.
diff.retail <- diff(retail.ts, lag = 1)
diff.retail

# Use Acf() function to identify autocorrealtion for first differenced walmart revenue 
# and plot autocorrelation for different lags (up to maximum of 8).
Acf(diff.retail, lag.max = 8, 
    main = "Autocorrelation for First Differenced Retail Sales Data")


## Two-level forecast with regression model and AR model for residuals

## FIT REGRESSION MODEL WITH LINEAR TREND AND SEASONALITY
# Use tslm() function to create linear trend and seasonal model.
train.lin.trend.season <- tslm(train.ts ~ trend  + season)

# See summary of linear trend and seasonality model and associated parameters.
summary(train.lin.trend.season)

# Apply forecast() function to make predictions for ts with linear trend and seasonality data in validation set.  
train.lin.trend.season.pred <- forecast(train.lin.trend.season, h = nValid, level = 0)
train.lin.trend.season.pred

## Regression model’s residuals for the training period
# Use Acf() function to identify autocorrelation for the model residuals for the training period 
# and plot autocorrelation for different lags (up to maximum of 8).
Acf(train.lin.trend.season.pred$residuals, lag.max = 8, 
    main = "Autocorrelation for Walmart revenue Training Residuals")

## Use Arima() function to fit AR(1) model for training residuals. 
# The Arima model of order = c(1,0,0) gives an AR(1) model.
# Use summary() to identify parameters of AR(1) model. 
res.ar1 <- Arima(train.lin.trend.season$residuals, order = c(1,0,0))
summary(res.ar1)
res.ar1$fitted

# Use forecast() function to make prediction of residuals in validation set.
res.ar1.pred <- forecast(res.ar1, h = nValid, level = 0)
res.ar1.pred

# Use Acf() function to identify autocorrelation for the training 
# residual of residuals and plot autocorrelation for different lags (up to maximum of 8).
Acf(res.ar1$residuals, lag.max = 8, 
    main = "Autocorrelation for Retail Sales Training Residuals of Residuals")

## Create two-level model's forecast with linear trend and seasonality 
# regression + AR(1) for residuals for validation period.

# Create data table with validation data, regression forecast
# for validation period, AR(1) residuals for validation, and 
# two level model results. 
two.level.pred <- train.lin.trend.season.pred$mean + res.ar1.pred$mean

valid.df <- round(data.frame(valid.ts, train.lin.trend.season.pred$mean, 
                             res.ar1.pred$mean, two.level.pred),3)
names(valid.df) <- c("Retail Sales", "Reg.Forecast", 
                     "AR(1)Forecast", "Combined.Forecast")
valid.df

## Fit Regression Model with Linear Trend and Seasonality for entire dataset
# Use tslm() function to create linear trend and seasonality model.
lin.trend.season <- tslm(retail.ts ~ trend + season)

# See summary of linear trend equation and associated parameters.
summary(lin.trend.season)

# Apply forecast() function to make predictions with linear trend and seasonal 
# model into the future 4 quarters.  
lin.trend.season.pred <- forecast(lin.trend.season, h = 12, level = 0)
lin.trend.season.pred

# Use Arima() function to fit AR(1) model for regression residuals.
residual.ar1 <- Arima(lin.trend.season$residuals, order = c(1,0,0))
residual.ar1.pred <- forecast(residual.ar1, h = 12, level = 0)

# Use summary() to identify parameters of AR(1) model.
summary(residual.ar1)

# Use Acf() function to identify autocorrelation for the residuals of residuals 
# and plot autocorrelation for different lags (up to maximum of 12).
Acf(residual.ar1$residuals, lag.max = 8, 
    main = "Autocorrelation for Residuals of Residuals for Entire Data Set")

# Identify forecast for the future 4 periods as sum of linear trend and 
# seasonal model and AR(1) model for residuals.
lin.trend.season.ar1.pred <- lin.trend.season.pred$mean + residual.ar1.pred$mean
lin.trend.season.ar1.pred

# Create a data table with linear trend and seasonal forecast for 4 future periods,
# AR(1) model for residuals for 4 future periods, and combined 
# two-level forecast for 4 future periods. 
table.df <- round(data.frame(lin.trend.season.pred$mean, 
                             residual.ar1.pred$mean, lin.trend.season.ar1.pred),3)
names(table.df) <- c("Reg.Forecast", "AR(1)Forecast","Combined.Forecast")
table.df

## ARIMA MODELS
## Use Arima() function to fit ARIMA(1,1,1)(1,1,1) for training data.
# Use summary() to show the ARIMA model and its parameters.
train.arima <- Arima(train.ts, order = c(1,1,1), seasonal = c(1,1,1))
summary(train.arima)
train.arima.pred <- forecast(train.arima, h = nValid, level = 0)
train.arima.pred

## Utilize auto.arima() function to automatically identify 
# the ARIMA model structure and parameters. 
train.auto.arima <- auto.arima(train.ts)
summary(train.auto.arima)
train.auto.arima.pred <- forecast(train.auto.arima, 
                                  h = nValid, level = 0)
train.auto.arima.pred

## Accuracy measures for the two ARIMA models in questions 3a and 3b.
round(accuracy(train.arima.pred$mean, valid.ts), 3)
round(accuracy(train.auto.arima.pred$mean, valid.ts), 3)

## ARIMA models to fit the entire dataset
# Use Arima() function to fit ARIMA(1,1,1)(1,1,1) model for the entire dataset
# Use summary() to show ARIMA model and its parameters.
# Apply forecast for the 4 periods in the future. 
retail.arima <- Arima(retail.ts, order = c(1,1,1), 
                       seasonal = c(1,1,1))
summary(retail.arima)
retail.arima.pred <- forecast(retail.arima, h = 12, level = 0)
retail.arima.pred

# Use auto.arima() function for the entire data set..
# Use summary() to show ARIMA model and its parameters.
# Apply forecast for the 4 periods in the future. 
retail.auto.arima <- auto.arima(retail.ts)
summary(retail.auto.arima)
retail.auto.arima.pred <- forecast(retail.auto.arima, h = 12, level = 0)
retail.auto.arima.pred

## Use accuracy() function to identify common accuracy measures for:
# (1) Regression model with linear trend and seasonality, 
# (2) Two-level model (regression model + AR(1) for regression residuals),
# (3) ARIMA(1,1,1)(1,1,1),
# (4) Auto ARIMA model,
# (5) Seasonal naive forecast.
round(accuracy(lin.trend.season$fitted, retail.ts), 3)
round(accuracy(lin.trend.season$fitted + residual.ar1$fitted, retail.ts), 3)
round(accuracy(retail.arima.pred$fitted, retail.ts), 3)
round(accuracy(retail.auto.arima.pred$fitted, retail.ts), 3)
round(accuracy((snaive(retail.ts))$fitted, retail.ts), 3)

#####################################################################################################

#Final Performance comparison

## Performance measures for two-level combined model and  hw forecast -1st segment
round(accuracy(tot.trend.seas.pred$fitted+tot.ma.trail.res, retail.ts), 3)
round(accuracy(HW.ZZZ.pred$fitted, retail.ts), 3)

## Performance measure of the forecasts -2nd segment-regression models
round(accuracy(lin.trend.season.pred$fitted, retail.ts),3)
round(accuracy(lin.trend.pred$fitted, retail.ts),3)
round(accuracy(quad.trend.season.pred$fitted, retail.ts),3)

## Performance measure of the forecasts -3rd segment-arima
round(accuracy(lin.trend.season$fitted + residual.ar1$fitted, retail.ts), 3)
round(accuracy(retail.arima.pred$fitted, retail.ts), 3)
round(accuracy(retail.auto.arima.pred$fitted, retail.ts), 3)

##naive and snaive
round(accuracy((naive(retail.ts))$fitted, retail.ts), 3)
round(accuracy((snaive(retail.ts))$fitted, retail.ts), 3)


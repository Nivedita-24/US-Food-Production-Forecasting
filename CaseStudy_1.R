
## CASE STUDY 1: FOOD PRODUCTION INDUSTRY IN US
## The goal is to identify the best forecasting method to predict monthly food production in the U.S. in 12 months of 2017

##------------------------------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------------------------------
## R LIBRARIES

library(forecast)
library(zoo)

## CREATE DATA FRAME. 

# Create data frame.
Production.data <- read.csv("673_case1.csv")

# See the first and last 6 records of the file.
head(Production.data)
tail(Production.data)

##------------------------------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------------------------------
## SOLUTION 1a

## USE ts() FUNCTION TO CREATE TIME SERIES DATA SET.
## USE stl() FUNCTION TO PLOT TIME SERIES COMPONENTS 
## USE Acf() FUNCTION TO IDENTIFY AUTOCORRELATION

# Function ts() takes three arguments: start, end, and freq.
# With monthly data, frequency (freq) of periods in a season (year) is 12. 
# With quarterly data, frequency in a season (year) is equal to 4.
# Arguments start and end are pairs: (season number, period number).

prod.ts <- ts(Production.data$production, 
                   start = c(1997, 1), end = c(2016, 12), freq = 12)
prod.ts

## Use plot() to plot time series data  

##------------------------------------------------------------------------------------------------------------------------
## SOLUTION 1b.
plot(prod.ts, 
     xlab = "Time", ylab = "Food Production (in million tons)",ylim = c(500, 1000), main = "US Food Production",xaxt = "n", xlim = c(1997, 2018), col = "blue")
axis(1, at = seq(1997, 2018, 1), labels = format(seq(1997, 2018, 1)))

##------------------------------------------------------------------------------------------------------------------------
## SOLUTION 1c.
# Use stl() function to plot times series components of the original data. 
# The plot includes original data, trend, seasonal, and reminder 
# (level and noise component).
Production.stl <- stl(prod.ts, s.window = "periodic")
autoplot(Production.stl, main = "US Food Production Time Series Components")

# Use Acf() function to identify autocorrelation and plot autocorrelation
# for different lags (up to maximum of 12).
autocor <- Acf(prod.ts, lag.max = 12, main = "Autocorrelation for US Food Production")

# Display autocorrelation coefficients for various lags.
Lag <- round(autocor$lag, 0)
ACF <- round(autocor$acf, 3)
data.frame(Lag, ACF)

##------------------------------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------------------------------
## SOLUTION 2a

## CREATE DATA PARTITION.
## PLOT DATA PARTITION.

# Define the numbers of months in the training and validation sets,
# nTrain and nValid, respectively.
nValid <- 48
nTrain <- length(prod.ts) - nValid
train.ts <- window(prod.ts, start = c(1997, 1), end = c(1997, nTrain))
valid.ts <- window(prod.ts, start = c(1997, nTrain + 1), 
                   end = c(1997, nTrain + nValid))

# Plot the time series data and visualize partitions. 
plot(train.ts, 
     xlab = "Time", ylab = "Food Production (in million tons)", ylim = c(500, 1000), bty = "l",
     xaxt = "n", xlim = c(1997, 2020), main = "", lwd = 2) 
axis(1, at = seq(1997, 2020, 1), labels = format(seq(1997, 2020, 1)))
lines(valid.ts, col = "black", lty = 1, lwd = 2)

# Plot on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2017 - 4, 2017 - 4), c(0, 1500))
lines(c(2017, 2017), c(0, 1500))
text(2005, 1000, "Training")
text(2015, 1000, "Validation")
text(2018.5, 1000, "Future")
arrows(2017 - 4.5, 970, 1997, 970, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017 - 3.7, 970, 2016.7, 970, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.5, 970, 2019.5, 970, code = 3, length = 0.1,
       lwd = 1, angle = 30)


##------------------------------------------------------------------------------------------------------------------------
## SOLUTION 2b

# # Create trailing MA with window widths (number of periods)  k = 2, 4, 6 and 12.

# In rollmean(), use argument align = "right" to calculate a trailing MA.
ma.trailing_2 <- rollmean(train.ts, k = 2, align = "right")
ma.trailing_4 <- rollmean(train.ts, k = 4, align = "right")
ma.trailing_6 <- rollmean(train.ts, k = 6, align = "right")
ma.trailing_12 <- rollmean(train.ts, k = 12, align = "right")

# Use head() function to show training MA (windows width k=12 
# for the first 6 MA results and tail() function to show the 
# last 6 MA results for MA. 
head(ma.trailing_12)
tail(ma.trailing_12)

##------------------------------------------------------------------------------------------------------------------------
## SOLUTION 2c

# Create forecast for the validation data for the window widths 
# of k = 2, 4, 6 and 12. 
ma.trail_2.pred <- forecast(ma.trailing_2, h = nValid, level = 0)
ma.trail_4.pred <- forecast(ma.trailing_4, h = nValid, level = 0)
ma.trail_6.pred <- forecast(ma.trailing_6, h = nValid, level = 0)
ma.trail_12.pred <- forecast(ma.trailing_12, h = nValid, level = 0)
ma.trail_6.pred

## GENERATE PLOT FOR PARTITION DATA AND TRAILING MA.

# Plot original data and forecast for training and validation partitions
# using trailing MA with window widths k = 6.
plot(prod.ts, 
     xlab = "Time", ylab = "Food Production (in million tons)", ylim = c(500, 1000), bty = "l",
     xlim = c(1997, 2020), main = "Trailing Moving Average") 
axis(1, at = seq(1997, 2020, 1), labels = format(seq(1997, 2020, 1)) )
lines(ma.trailing_6, col = "blue", lwd = 2, lty = 1)
lines(ma.trail_6.pred$mean, col = "blue", lwd = 2, lty = 2)
legend(1997,600, legend = c("US Food Production Data", 
                             "Trailing MA, k=6, Training Partition", 
                             "Trailing MA, k=6, Validation Partition"), 
       col = c("black","blue", "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")


# Plot on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2017 - 4, 2017 - 4), c(0, 1500))
lines(c(2017, 2017), c(0, 1500))
text(2005, 1000, "Training")
text(2015, 1000, "Validation")
text(2018.5, 1000, "Future")
arrows(2017 - 4.5, 970, 1997, 970, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017 - 3.7, 970, 2016.7, 970, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.5, 970, 2019.5, 970, code = 3, length = 0.1,
       lwd = 1, angle = 30)


##------------------------------------------------------------------------------------------------------------------------
## SOLUTION 2d

# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.
round(accuracy(ma.trail_2.pred$mean, valid.ts), 3)
round(accuracy(ma.trail_4.pred$mean, valid.ts), 3)
round(accuracy(ma.trail_6.pred$mean, valid.ts), 3)
round(accuracy(ma.trail_12.pred$mean, valid.ts), 3)

##------------------------------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------------------------------
## SOLUTION 3a

## USE REGRESSION MODEL WITH TREND AND SEASONALITY FOR TRAINING PARTITION.
## IDENTIFY REGRESSION RESIDUALS FOR TRAINING PARTITION. 
## CREATE TRAILING MA USING REGRESSION RESIDUALS FOR TRAINING PARTITION. 
## PLOT REGRESSION RESIDUALS AND TRAILING MA FOR RESIDUALS IN TRAINING PARTITION.

# Fit a regression model with linear trend and seasonality for
# training partition. 
trend.seas <- tslm(train.ts ~ trend + season)
summary(trend.seas)

# Apply forecast() function to make predictions for ts data in
# training and validation sets.  
prod.lin.pred <- forecast(trend.seas, h = nValid, level = 0)
prod.lin.pred

# Plot predictions for linear trend forecast.
plot(prod.ts, 
     xlab = "Time", ylab = "Food Production (in million tons)", ylim = c(500, 1000), bty = "l",
     xlim = c(1997, 2020), 
     main = "Regression Forecast in Training and Validation Partitions ") 
axis(1, at = seq(1997, 2020, 1), labels = format(seq(1997, 2020, 1)))
lines(trend.seas$fitted, col = "blue", lwd = 2, lty = 1)
lines(prod.lin.pred$mean, col = "blue", lwd = 2, lty = 2)
legend(1997,600, legend = c("US Food Production Data", 
                             "Regression Forecast, Training Partition", 
                             "Regression Forecast, Validation Partition"), 
       col = c("black", "blue", "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2017 - 4, 2017 - 4), c(0, 1500))
lines(c(2017, 2017), c(0, 1500))
text(2005, 1000, "Training")
text(2015, 1000, "Validation")
text(2018.5, 1000, "Future")
arrows(2017 - 4.5, 970, 1997, 970, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017 - 3.7, 970, 2016.7, 970, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.5, 970, 2019.5, 970, code = 3, length = 0.1,
       lwd = 1, angle = 30)

## IDENTIFY FORECAST ACCURACY

# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.
round(accuracy(prod.lin.pred$mean, valid.ts), 3)

##------------------------------------------------------------------------------------------------------------------------
## SOLUTION 3b

# Identify and display residuals for time series based on the regression
# (differences between actual and regression values in the same periods).
trend.seas.res <- trend.seas$residuals
trend.seas.res

# Apply trailing MA for residuals with window width k = 6. 
ma.trail.res <- rollmean(trend.seas.res, k = 6, align = "right")
ma.trail.res

# Regression residuals in validation period.
trend.seas.res.valid <- valid.ts - prod.lin.pred$mean
trend.seas.res.valid

# Create residuals forecast for validation period.
ma.trail.res.pred <- forecast(ma.trail.res, h = nValid, level = 0)
ma.trail.res.pred

# Plot residuals and MA residuals forecast in training and validation partitions. 
plot(trend.seas.res, 
     xlab = "Time", ylab = "Food Production (in million tons)", ylim = c(-170, 180), bty = "l",
     xlim = c(1997, 2020), 
     main = "Regression Residuals and Trailing MA for Residuals", 
     col = "brown", lwd =2) 
axis(1, at = seq(1997, 2020, 1), labels = format(seq(1997, 2020, 1)))
lines(trend.seas.res.valid, col = "brown", lwd = 2, lty = 2)
lines(ma.trail.res, col = "blue", lwd = 2, lty = 1)
lines(ma.trail.res.pred$mean, col = "blue", lwd = 2, lty = 2)
legend(1997, -90, legend = c("Regression Residuals, Training Partition", 
                            "Regression Residuals, Validation Partition",
                            "MA Forecast (k=6), Training Partition", 
                            "MA forecast (k=6), Validation Partition"), 
       col = c("brown", "brown", "blue", "blue"), 
       lty = c(1, 2, 1, 2), lwd =c(2, 2, 2, 2), bty = "n")

# Plot on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2017 - 4, 2017 - 4), c(-150, 150))
lines(c(2017, 2017), c(-150, 150))
text(2005, 175, "Training")
text(2015, 175, "Validation")
text(2018.5, 175, "Future")
arrows(2017 - 4.5, 165, 1997, 165, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017 - 3.7, 165, 2016.7, 165, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.5, 165, 2019.5, 165, code = 3, length = 0.1,
       lwd = 1, angle = 30)

##------------------------------------------------------------------------------------------------------------------------
## SOLUTION 3c


# Develop two-level forecast for validation period by combining  
# regression forecast and trailing MA forecast for residuals.
fst.2level <- prod.lin.pred$mean + ma.trail.res.pred$mean
fst.2level

# Create a table for validation period: validation data, regression 
# forecast, trailing MA for residuals and total forecast.
valid.df <- data.frame(valid.ts, prod.lin.pred$mean, 
                       ma.trail.res.pred$mean, 
                       fst.2level)
names(valid.df) <- c("US Food Production", "Regression.Fst", 
                     "MA.Residuals.Fst", "Combined.Fst")
valid.df

# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.
round(accuracy(prod.lin.pred$mean, valid.ts), 3)
round(accuracy(fst.2level, valid.ts), 3)

##------------------------------------------------------------------------------------------------------------------------
## SOLUTION 3d

## USE REGRESSION AND TRAILING MA FORECASTS FOR ENTIRE DATA SET. 
## USE 2-LEVEL (COMBINED) FORECAST TO FORECAST 12 FUTURE PERIODS.
## MEASURE ACCURACY OF REGRESSION AND TWO-LEVEL FORECASTS FOR
## ENTIRE DATA SET.

# Fit a regression model with linear trend and seasonality for
# entire data set.
tot.trend.seas <- tslm(prod.ts ~ trend + season)
summary(tot.trend.seas)

# Create regression forecast for future 12 periods.
tot.trend.seas.pred <- forecast(tot.trend.seas, h = 12, level = 0)
tot.trend.seas.pred

# Identify and display regression residuals for entire data set.
tot.trend.seas.res <- tot.trend.seas$residuals
tot.trend.seas.res

# Use trailing MA to forecast residuals for entire data set.
tot.ma.trail.res <- rollmean(tot.trend.seas.res, k = 6, align = "right")
tot.ma.trail.res

# Create forecast for trailing MA residuals for future 12 periods.
tot.ma.trail.res.pred <- forecast(tot.ma.trail.res, h = 12, level = 0)
tot.ma.trail.res.pred

# Develop 2-level forecast for future 12 periods by combining 
# regression forecast and trailing MA for residuals for future
# 12 periods.
tot.fst.2level <- tot.trend.seas.pred$mean + tot.ma.trail.res.pred$mean
tot.fst.2level

# Create a table with regression forecast, trailing MA for residuals,
# and total forecast for future 12 periods.
future12.df <- data.frame(tot.trend.seas.pred$mean, tot.ma.trail.res.pred$mean, 
                          tot.fst.2level)
names(future12.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
future12.df

## GENERATE PLOT OF ORIGINAL DATA AND REGRESSION FORECAST, AND PREDICTIONS
## IN FUTURE 12 PERIODS.
## GENERATE PLOT OF REGRESSION RESIDUALS, TRAILING MA FOR RESIDUALs, AND 
## TRAILING MA FORECAST IN FUTURE 12 PERIODS.

# Plot original Ridership time series data and regression model.
plot(prod.ts, 
     xlab = "Time", ylab = "Food Production (in million tons)", ylim = c(500, 1000), bty = "l",
     xaxt = "n", xlim = c(1997, 2020), lwd =1,
     main = "US Food Production Data and Regression with Trend and Seasonality") 
axis(1, at = seq(1997, 2020, 1), labels = format(seq(1997, 2020, 1)))
lines(tot.trend.seas$fitted, col = "blue", lwd = 2)
lines(tot.trend.seas.pred$mean, col = "blue", lty =5, lwd = 2)
legend(1997,600, legend = c("US Food Production", "Regression",
                             "Regression Forecast for Future 12 Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")


# Plot on chart vertical lines and horizontal arrows describing
# entire data set and future prediction intervals.

lines(c(2017, 2017), c(0, 1500))
text(2008, 1000, "Data Set")
text(2018.5, 1000, "Future")
arrows(2016.5, 970, 1997.5, 970, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.5, 970, 2019.5, 970, code = 3, length = 0.1,
       lwd = 1, angle = 30)


# Plot regression residuals data and trailing MA based on residuals.
plot(tot.trend.seas.res, 
     xlab = "Time", ylab = "Food Production (in million tons)", ylim = c(-170, 200), bty = "l",
     xaxt = "n", xlim = c(1997, 2020), lwd =1, col = "brown", 
     main = "Regression Residuals and Trailing MA for Residuals") 
axis(1, at = seq(1997, 2020, 1), labels = format(seq(1997, 2020, 1)))
lines(tot.ma.trail.res, col = "blue", lwd = 2, lty = 1)
lines(tot.ma.trail.res.pred$mean, col = "blue", lwd = 2, lty = 2)
legend(1997,-95, legend = c("Regresssion Residuals", 
                             "Trailing MA (k=6) for Residuals", 
                             "Trailing MA Forecast (k=6) for Future 12 Periods"), 
       col = c("brown", "blue", "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on chart vertical lines and horizontal arrows describing
# entire data set and future prediction intervals.
lines(c(2017, 2017), c(-180, 200))
text(2008, 190, "Data Set")
text(2018.5, 190, "Future")
arrows(2016.5, 170, 1997.5, 170, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.5, 170, 2019.5, 170, code = 3, length = 0.1,
       lwd = 1, angle = 30)


##------------------------------------------------------------------------------------------------------------------------
## SOLUTION 3e


##SEASONAL NAIVE FORECASTS.

prod.snaive.pred <- snaive(prod.ts, h = 12)

# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.
round(accuracy(prod.snaive.pred$fitted, prod.ts), 3)
round(accuracy(tot.trend.seas.pred$fitted, prod.ts), 3)
round(accuracy(tot.trend.seas.pred$fitted+tot.ma.trail.res, prod.ts), 3)

##------------------------------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------------------------------
## SOLUTION 4a

## HOLT-WINTER'S (HW) EXPONENTIAL SMOOTHING WITH PARTITIONED DATA, AUTOMATED
## ERROR, TREND and SEASONALITY (ZZZ) OPTIONS, AND OPTIMAL PARAMETERS
## ALPHA, BETA, AND GAMMA.

# Create Holt-Winter's (HW) exponential smoothing for partitioned data.
# Use ets() function with model = "ZZZ", i.e., automated selection 
# error, trend, and seasonality options.
# Use optimal alpha, beta, & gamma to fit HW over the training period.
hw.ZZZ <- ets(train.ts, model = "ZZZ")
hw.ZZZ # Model appears to be (A, N, A), with alpha = 0.6567  and gamma = 2e-04.

# Use forecast() function to make predictions using this HW model with 
# validation period (nValid). 
# Show predictions in tabular format.
hw.ZZZ.pred <- forecast(hw.ZZZ, h = nValid, level = 0)
hw.ZZZ.pred

# Plot HW predictions for original data, automated selection of the 
# model and optimal smoothing parameters.
plot(hw.ZZZ.pred$mean, 
     xlab = "Time", ylab = "Food Production (in million tons)", ylim = c(500, 1000), bty = "l",
     xaxt = "n", xlim = c(1997, 2020), 
     main = "Holt-Winter's Model with Automated Selection of Model Options", 
     lty = 5, col = "blue", lwd = 2) 
axis(1, at = seq(1997, 2020, 1), labels = format(seq(1997, 2020, 1)))
lines(hw.ZZZ.pred$fitted, col = "blue", lwd = 2)
lines(prod.ts)
legend(1997,600, 
       legend = c("US Food Production", 
                  "Holt-Winter's Automated Model for Training Partition",
                  "Holt-Winter's Automated Model for Validation Partition"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2017 - 4, 2017 - 4), c(0, 1500))
lines(c(2017, 2017), c(0, 1500))
text(2005, 1000, "Training")
text(2015, 1000, "Validation")
text(2018.5, 1000, "Future")
arrows(2017 - 4.5, 970, 1997, 970, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017 - 3.7, 970, 2016.7, 970, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.5, 970, 2019.5, 970, code = 3, length = 0.1,
       lwd = 1, angle = 30)

##------------------------------------------------------------------------------------------------------------------------
## SOLUTION 4b

## FORECAST WITH HOLT-WINTER'S MODEL USING ENTIRE DATA SET INTO
## THE FUTURE FOR 12 PERIODS.

# Create Holt-Winter's (HW) exponential smoothing for full Amtrak data set. 
# Use ets() function with model = "ZZZ", to identify the best HW option
# and optimal alpha, beta, & gamma to fit HW for the entire data period.
HW.ZZZ <- ets(prod.ts, model = "ZZZ")
HW.ZZZ # Model appears to be (M, N, A), with alpha = 0.723 and gamma = 2e-04.

# Use forecast() function to make predictions using this HW model for
# 12 month into the future.
HW.ZZZ.pred <- forecast(HW.ZZZ, h = 12 , level = 0)
HW.ZZZ.pred


# plot HW predictions for original data, optimal smoothing parameters.
plot(HW.ZZZ.pred$mean, 
     xlab = "Time", ylab = "Food Production (in million tons)", ylim = c(500, 1000), bty = "l",
     xaxt = "n", xlim = c(1997, 2020),
     main = "Holt-Winter's Automated Model for Entire Data Set and Forecast for Future 12 Periods", 
     lty = 2, col = "blue", lwd = 2) 
axis(1, at = seq(1997, 2020, 1), labels = format(seq(1997, 2020, 1)))
lines(HW.ZZZ.pred$fitted, col = "blue", lwd = 2)
lines(prod.ts)
legend(1997,600, 
       legend = c("US Food production", 
                  "Holt-Winter's Automated Model for Entire Data Set",
                  "Holt-Winter's Automated Model's Forecast, Future 12 Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on chart vertical lines and horizontal arrows describing
# entire data set and future prediction intervals.
lines(c(2017, 2017), c(0, 1500))
text(2008, 1000, "Data Set")
text(2018.5, 1000, "Future")
arrows(2016.5, 970, 1997.5, 970, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.5, 970, 2019.5, 970, code = 3, length = 0.1,
       lwd = 1, angle = 30)


# Use ets() function with alternative model = "ANA". 
# Use forecast() function to make predictions using this HW model for
# 12 month into the future.
HW.ANA <- ets(prod.ts, model = "ANA")
HW.ANA # Model appears to be (A, N, A), with alpha = 0.7113 and gamma = 1e-04.

# Use forecast() function to make predictions using this HW model for
# 12 month into the future.
HW.ANA.pred <- forecast(HW.ANA, h = 12 , level = 0)
HW.ANA.pred

# plot HW predictions for original data, optimal smoothing parameters.
plot(HW.ANA.pred$mean, 
     xlab = "Time", ylab = "Food Production (in million tons)", ylim = c(500, 1000), bty = "l",
     xaxt = "n", xlim = c(1997, 2020),
     main = "Holt-Winter's Automated Model for Entire Data Set and Forecast for Future 12 Periods", 
     lty = 2, col = "blue", lwd = 2) 
axis(1, at = seq(1997, 2020, 1), labels = format(seq(1997, 2020, 1)))
lines(HW.ANA.pred$fitted, col = "blue", lwd = 2)
lines(prod.ts)
legend(1997,600, 
       legend = c("US Food production", 
                  "Holt-Winter's Automated Model for Entire Data Set",
                  "Holt-Winter's Automated Model's Forecast, Future 12 Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on chart vertical lines and horizontal arrows describing
# entire data set and future prediction intervals.
lines(c(2017, 2017), c(0, 1500))
text(2008, 1000, "Data Set")
text(2018.5, 1000, "Future")
arrows(2016.5, 970, 1997.5, 970, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.5, 970, 2019.5, 970, code = 3, length = 0.1,
       lwd = 1, angle = 30)

##------------------------------------------------------------------------------------------------------------------------
## SOLUTION 4c
# Identify performance measures for HW forecast.
round(accuracy(HW.ZZZ.pred$fitted, prod.ts), 3)
round(accuracy(HW.ANA.pred$fitted, prod.ts), 3)
round(accuracy((snaive(prod.ts))$fitted, prod.ts), 3)

##------------------------------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------------------------------



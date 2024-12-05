#Loading necessary libraries
library(forecast)
library(zoo)
#Setting working directory
setwd("~/Documents/Mac Projects")

######## Data Preparation ##########

#Reading the Data
sales.data<- read.csv("SalesData.csv")
#Modifying the data type
sales.data$Month <- as.Date(sales.data$Month)
#Creating time series Object
sales.data.ts <- ts(sales.data$Sales,start=c(2014,1), end=c(2023,12), frequency=12)
#Stats to understand the operating limits and brief overview of our response variable
summary(sales.data.ts)

########Time series Visualization and prep for modeling##########

plot(sales.data.ts, ylim = c(10, 600),  ylab = "Sales", xlab = "Time", bty = "l", xaxt = "n", 
     xlim = c(2014,2023), main = "Sales over Time")
axis(1, at = seq(2014,2023 , 1), labels = format(seq(2014, 2023, 1)))

#Splitting the dataset
nValid <- 24
nTrain <-length(sales.data.ts) - nValid
train.ts <- window(sales.data.ts, start=c(2014,1),end=c(2014,nTrain))
valid.ts <- window(sales.data.ts, start=c(2014,nTrain+1),end=c(2014,length(sales.data.ts)))

########## AutoRegressive Model on Original Data#############

#Fitting AR1 model to sales data
sales_arima <-Arima(sales.data.ts, order = c(1,0,0))
summary(sales_arima)

#Forecasting next 12 months of sales
forecast_arima <- forecast(sales_arima, h = 12, level=95)

#Final Plot including Sales data and forecasting values
plot(sales.data.ts, ylim = c(10, 700),  ylab = "Sales", xlab = "Time", bty = "l", xaxt = "n", 
     xlim = c(2014,2025), main = "Sales over Time")
axis(1, at = seq(2014,2025 , 1), labels = format(seq(2014, 2025, 1)))
lines(forecast_arima$mean, col = "blue", lwd = 2)
lines(forecast_arima$lower, col = "red", lty = 2)
lines(forecast_arima$upper, col = "red", lty = 2)
legend("topleft", legend = c("Actual Sales", "Forecast", "95% CI"), 
       col = c("black", "blue", "red"), lwd = c(1, 2, 1), lty = c(1, 1, 2))

#To evaluate the performance of this sales forecast based on accuracy results
accuracy(sales_arima, sales.data.ts)

#To see the residuals and Auto correlation values
plot(sales_arima$residuals, ylab="Residuals", main="AR1 residual plot")
Acf(sales_arima$residuals, lag.max = 12, main = "")



#Linear Regression with seasonality
train.lm.trend.season <- tslm(train.ts ~ trend+ I(trend^2) + season)
train.lm.trend.season.pred <- forecast(train.lm.trend.season, h = nValid, level = 0)

#Plotting
dev.off()
plot(train.lm.trend.season.pred, ylim = c(0, 700),  ylab = "Sales", xlab = "Time", 
     bty = "l", xaxt = "n", xlim = c(2014,2024), main = "Sales over Time", flty = 2)
axis(1, at = seq(2014, 2025, 1), labels = format(seq(2014, 2025, 1)))
lines(train.lm.trend.season.pred$fitted, lwd = 2, col = "red")
lines(valid.ts)

#Looking at Residuals
plot(train.lm.trend.season$residuals, ylab="Residuals", main="Residuals over time")
Acf(train.lm.trend.season$residuals, lag.max = 12, main = "")

#AR(1) model
train.res.arima <- Arima(train.lm.trend.season$residuals, order = c(1,0,0))
train.res.arima.pred <- forecast(train.res.arima, h = nValid)
#Summary of AR(1)
summary(train.res.arima)
#To check if there's any effect on residuals by applying AR(1)
Acf(train.res.arima$residuals, lag.max = 12, main = "ACT @ lag.max=12")



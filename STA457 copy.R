# install andrequire package forecast
# install.packages("forecast")
library(forecast)
# install and require package ITSMR
# install.packages(itsmr)
library("itsmr")
# install tseries package
#install.packages('tseries')
library('tseries')

#Data clean up
data <- read.csv(file.choose(), head=T, strip.white=T, stringsAsFactors=F)
OpenPrice <- as.numeric(data$Open)
OpenPrice1 <- rev(OpenPrice)
AbReturn <- diff(OpenPrice1)
# Get the final time series to work on
ExcessReturn <- ts(AbReturn/(head(OpenPrice1,-1)))
# Prelimenary Checking
# plot(ExcessReturn,xlab="Time Index", type="l")
plot.ts(ExcessReturn)
par(mfrow=c(1, 2))
acf(ExcessReturn)
pacf(ExcessReturn)
par(mfrow=c(1, 1))
# Difference the Excess Return
ExcessReturnDiff <- diff(ExcessReturn, lag=1, differences=1)
plot.ts(ExcessReturnDiff)
ExcessReturnDiff2 <- diff(ExcessReturn, lag=1, differences=2)
plot.ts(ExcessReturnDiff2)
ExcessReturnDiffS1 <- diff(ExcessReturn, lag=4, differences=1)
plot.ts(ExcessReturnDiff1)
ExcessReturnDiffS2 <- diff(ExcessReturn, lag=12, differences=1)
plot.ts(ExcessReturnDiff2)
# Seems that differencing once is enough
# Check the property of the ExcessReturndiff1
acf(ExcessReturnDiff2)
pacf(ExcessReturnDiff2)
acf(ExcessReturnDiff1)
pacf(ExcessReturnDiff1)
acf(ExcessReturnDiff2)
pacf(ExcessReturnDiff2)

# Find out there is no seasonal component
ExcessRseries <- decompose(ExcessReturn)
# Find out there is no strend in data
adf.test(ExcessReturn, alternative = "stationary")
adf.test(ExcessReturnDiff, alternative = "stationary")
adf.test(ExcessReturnDiff2, alternative = "stationary")
# conclude that no differencing needed
qqnorm(ExcessReturn)
qqline(ExcessReturn)

# Take log of the data
LogExcessReturn <- log(ExcessReturn)


# Use to auto fit check which p, q, d of Arima model
auto.arima(ExcessReturn,max.Q=12,max.P=12, seasonal=T, max.p=10, max.q = 10)
auto.arima(ExcessReturn,max.p=10, max.q = 10, ic='aicc')
autofit(ExcessReturnDiff1, p = 0:5, q = 0:5)
autofit(ExcessReturn, p=0:5, q=0:5)
# Autoarima suggesting ARIMA(2, 0, 2)
# Auofit suggesting ARIMA(3, 0, 1)
# Seems from the graph that ARIMA(3, 0, 1) more likely
# Have lower AICC

# Fit two possible model 
fit <- arima(ExcessReturn, order=c(3, 1, 1))
fit1 <- arima(ExcessReturn, order=c(2,0,2))
# Check the residual
tsdisplay(residuals(fit))
tsdisplay(residuals(fit1),, main="ARMA(2,2) Residuals")

# Formally test whether the risedual is WN
Box.test(fit$residuals, lag=20, type="Ljung-Box")
Box.test(fit1$residuals, lag=20, type="Ljung-Box")

# Cehck qq plot
qqnorm(fit$residuals)
qqnorm(fit1$residuals)
qqline(fit1$residuals)
# Forecast the value at lag h=2
fcast <- forecast(fit1, h=2)
plot(fcast, main="Forecasts from ARIMA(2,0,2) for March and April Monthly Excess Return")
lines(fitted(fit1), col='red')

AiccInfo <- matrix( rep( 0, len=36), nrow=6)
for (i in 0:5 ) {
  for(j in 0:7){
    print(c(i,j))
    s <- arma(ExcessReturn,p=i,q=j)
    print(s$aicc)
  }
}

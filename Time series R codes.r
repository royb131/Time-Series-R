#Data Description

##Variable description and its statistical summary
Data are in mean deviation of land ocean temperature

# Libraries
library(astsa)
library(astsa)
library(timeSeries)
library(tseries)
library(forecast)
library(xts)
library(TTR)
# Get dataset from astsa
data("globtemp")
class(globtemp)
# Statistics of globaltemp dataset
mean(globtemp)
mean(globtemp)
median(globtemp)
sd(globtemp)
IQR(globtemp)
mad(globtemp)
summary(globtemp)


##Time series (ts) description 
gtemp=ts(globtemp,frequency=1,start=1880,end=2015)
head(gtemp,n=10)

##Data Exploration
#Plotting data 

plot(globtemp,main="Yearly average global temperature deviations (1880 to 2015) in degrees centigrade",type="o", ylab="Global Temperature Deviations")
grid(lty=1, col=gray(.9)) 

#boxplot

boxplot(gtemp)

#ts plot with abline

ts.plot(gtemp)
tt <- 1:length(globtemp)
fit <- ts(loess(globtemp ~ tt, span = 0.2)$fitted, start = 1880, frequency = 1)
plot(globtemp, type='l')
lines(fit, col = 'red')
grid()

plot(gtemp) 
abline(reg=lm(gtemp~time(gtemp)))

##Density plot
plot(density(gtemp), main = "globtemp density plot")

#Data description

# Differencing Time series data
par(mfrow=c(2,1))
plot(diff(globtemp), type="o")
mean(diff(globtemp)) # drift estimate = .008
acf(diff(gtemp), 48)

## Differenced process shows minimal autocorelation which proved again that the trend is random walk with drift. In traditional time series analysis in a random walk with drift the estimated drift is always about .008 with 1 degree increase in about hundred years. As we can see in above plots, random walk trend is not stationary, but differenced data are stationary. The mean is non-constant and there is clearly an upward trend. The variance appears to be pretty consistent however.


acf2(gtemp)

#Plot differenceting with acf2.

acf2(diff(gtemp))


## Transformation

#KPSS Test for Level Stationarity:-

require(tseries)
globtem_diff<-diff(gtemp,d=1)
kpss.test(gtemp)

##Data Decomposition

Decompose ts into trends, seasons, residuals:-
  
  globtemp dataset has no seasonal cycles or less than 2 seasonal cycles. Decomposing of non seasonal time series involves tying to separate the time series into these individual components. Here we can use state space model for smoothing the data. 


smoothinggtemp<- SMA(gtemp, n=3)
plot.ts(smoothinggtemp)
#It seems like there is still some random fluctuations in the data, we might want to try a big larger of a smoother.
smoothinggtemp<- SMA(gtemp, n=10)
plot.ts(smoothinggtemp)

# Now we can see a positive trend of temperature by using smoothing techniques which iss in upword slopping, 
### Regression


auto.arima(gtemp, d = 1, max.p = 5, max.q = 5)

arima.fit2<-Arima(gtemp, order=c(1,1,3), include.drift =TRUE, method="ML")
arima.fit2

# Check for confidence interbvals of auto arima.
confint(arima.fit2)
tsdisplay(residuals(arima.fit2), lag.max=25, main=' Model Residuals')
# creating a linear regression model.
globtemp.reg<-lm(gtemp~time(gtemp))
summary(globtemp.reg)

## ARMA/ARIMA model 

#Plot And Print ACF And PACF Of A Time Series: Max lag: sqrt n + 10
plot(acf(gtemp))

# plot ACF with diff()
plot(acf(diff(gtemp)))
# plot partial autocorrelation pacf
plot(pacf(gtemp))
# plot pacf with diff.
plot(pacf(diff(gtemp)))

## Model Diagnostics

arima.fit2<-arima(gtemp, order=c(1,1,3))
arima.fit2
arima.fit3<- arima(gtemp,order=c(1,1,1))
arima.fit3
tsdisplay(arima(gtemp,order=c(1,1,3))$residuals)
tsdisplay(arima(gtemp,order=c(1,1,1))$residuals)

tsdisplay(arima(gtemp,order=c(1,1,3))$residuals)

# check residuals
residuals<- auto.arima(gtemp)
checkresiduals(residuals)





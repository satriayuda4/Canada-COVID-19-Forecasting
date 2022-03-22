setwd('C:/Users/LENOVO-PC/Documents/New/COVID-19')

data <- read.csv('new-cases-2021.csv')
head(data)

data_ts = ts(data$new_cases_per_million, frequency = 30, start = c(1,1))
data_ts = ts(data$new_cases_per_million, frequency = 7, start = c(1,1,2021))
plot(data_ts)

data_ts2 = data$new_cases_per_million[c(1,1:180)]
data_ts2 = ts(data_ts2, frequency = 7)
plot(data_ts2)

packageVersion("forecast")

#===========================================================================
#Part - 0 Plot time series data
#===========================================================================

#install.packages("TSstudio")
library(TSstudio)

ts_info(data_ts)

plot.xts(Michigan_CS)

plot.xts(data_ts)

new_cases <- window(data_ts, start(c(2021,1,1)))
ts_plot(new_cases,
        title = "New covid-19 cases",
        Xgrid = TRUE,
        Ygrid = TRUE)

plot(decompose(data_ts))
plot(decompose(diff(data_ts, 1)))
plot(decompose(log(data_ts)))
plot(decompose(diff(log(data_ts), 1)))

library(forecast)
BoxCox.lambda(data_ts)
#the result is very close to 0, so the transformation that 
#needed is the log transformation

#===========================================================================
#Part - A Time Series Component 
#===========================================================================

#Trend Component Test
library(pastecs)
trend.test(data_ts)
#The Spearman's is signficant, so, the Trend component is exist.  

#Seasonal Component Test
library(seastests)
isSeasonal(data_ts)

seasonplot(data_ts)

#Check whether its need to do seasonal differencing
nsdiffs(data_ts)

#to make sure, do boxcox lambda
lambda <- BoxCox.lambda(data_ts)
lambda

#Check whether its need to do regular differencing
ndiffs(diff(log(data_ts), 7))

#Check whether its need to do regular differencing
ndiffs(diff(data_ts, 1))

#Looking seasonal using ACF Plot
#ACF Plot
acf(diff(data_ts,1), ylim=c(-1,1), lag.max = length(data_ts)/5)
acf(diff(log(data_ts),1), ylim=c(-1,1))
acf(diff(log(data_ts),1), ylim=c(-1,1), lag.max = length(data_ts)*2/5)

#acf(diff(data_ts, 7), ylim=c(-0.5,1), lag.max = length(data_ts)/5)
#acf(diff(data_ts, 7), ylim=c(-0.5,1), lag.max = length(data_ts))

#PACF plot
pacf(data_ts, ylim=c(-1,1))
pacf(diff(log(data_ts), 1), ylim=c(-1,1))
pacf(data_ts, ylim=c(-1,1), lag.max = length(data_ts)/5)
pacf(data_ts, ylim=c(-1,1), lag.max = length(data_ts)/5)
par(mfrow = c(2,1))
pacf(diff(data_ts, 1), ylim=c(-1,1), lag.max = length(data_ts)/5)

#Apply differencing 
acf(diff(data_ts, 7), ylim = c(-1,1), lag.max = 50)
pacf(diff(data_ts, 7), ylim = c(-1,1), lag.max = 50)

#comparing ACF and PACF
par(mfrow=c(1,2))
acf(diff(data_ts, 7), ylim = c(-1,1), lag.max = length(data_ts)/5)
pacf(diff(data_ts, 7), ylim = c(-1,1), lag.max = length(data_ts)/5)
par(mfrow=c(1,1))

tsdisplay(data_ts)
#tsdisplay(log(data_ts))
tsdisplay(diff(data_ts, 7))

library(tseries) 
adf.test(data_ts)
#p-value is not significant, need differencing
adf.test(diff(data_ts, 1))
#p-value is significant, no need differencing

library(uroot)
ch.test(data_ts)
ch.test(diff(log(data_ts), 1))



nsdiffs(diff(data_ts, 7))
ndiffs(diff(data_ts, 7))

plot(data_ts) 

#===========================================================================
#Part - B
#===========================================================================

library(TTR)

plot(data_ts)

#Naive forecast
naive_fit = naive(diff(data_ts))
summary(naive_fit)
fitted(naive_fit)
accuracy(naive_fit)

#Moving Average forecast
MA_fit = ma(data_ts, 7)
summary(MA_fit)
accuracy(MA_fit, data_ts)

#SMA - Simple Moving Average
SMA_fit = SMA(data_ts, 7)
summary(SMA_fit)
accuracy(SMA_fit, data_ts)

#Weighted Moving Average
WMA_fit = WMA(data_ts, 7)
summary(WMA_fit)
accuracy(WMA_fit, data_ts)

plot(data_ts)
#lines(fitted(naive_fit), col = "red", lwd = 2)
lines(MA_fit, col = "red", lwd = 2)
lines(SMA_fit, col = "blue", lwd = 2)
lines(WMA_fit, col = "green", lwd = 2)

#Exponential Moving Average
EMA_fit = EMA(data_ts, 7)
summary(EMA_fit)
accuracy(EMA_fit, data_ts)

#Holt Method
holt_prod = holt(data_ts, initial = "simple")
summary(holt_prod)
accuracy(holt_prod)

plot(data_ts)
lines(fitted(holt_prod), col = "blue", lwd = 2)
lines(EMA_fit, col = "red", lwd = 2)

#Linear Trend Model

#Exponential Trend Model

#Quadratic Trend Model


#Holts Winter
HW_fit = HoltWinters(data_ts)
summary(HW_fit)
accuracy(HW_fit, data_ts)
lines(fitted(HW_fit)[,1], col = "red", lwd = 2)


##HW_fit = hw(data_ts, initial = "optimal")
##summary(HW_fit)
##accuracy(HW_fit)


#Decomposition 
#dc_prod = stlf(data_ts, s.window = "periodic")
#lines(fitted(dc_prod), col = "blue", lwd = 2)
#summary(dc_prod)

#Regression Model With Dummy Variable

#SES
ses_prod = ses(data_ts, alpha = 0.8, initial = "simple")
lines(fitted(ses_prod), col = "red", lwd = 2)
summary(ses_prod)

##ses_prod_opt = ses(data_ts, initial = "optimal")
ses_prod_opt = ses(data_ts, alpha =  0.2242, initial = "simple")
summary(ses_prod_opt)

#Seasonal Trend Decomposition using Loess
STL_fit = stl(data_ts, s.window = "periodic")
summary(STL_fit)

seasonal(STL_fit)
plot(data_ts)
lines(seasadj(STL_fit), col = "red", lwd = 2)

##STLF_fit = stlf(data_ts, s.window = "periodic")
##plot(stlf(data_ts, s.window = "periodic, h = 5))
##fitted1 = trendcycle(STL_fit) + seasonal(STL_fit)
##lines(fitted, col = "red", lwd = 2)
##summary(STLF_fit)

#===========================================================================
#Only Additive model
#===========================================================================

season = seasonaldummy(data_ts)
time = 1:length(data_ts)

reg_fit = tslm(data_ts ~ season + time)
summary(reg_fit)

reg_fit1 = tslm(data_ts ~ relevel(season, ref = "3") + time)
summary(reg_fit1)


#===========================================================================
#Part - C Training and Testing Data
#===========================================================================

length(data_ts)

k = round(length(data_ts)*0.8,0)
train = subset(data_ts, end = k)
test = subset(data_ts, start = k+1)

length(train)

length(test)



#===========================================================================
#Part - C/1 train with the selected model
#===========================================================================

# Weighted Moving Average
WMA_fit = WMA(diff(train, 7), 7)
summary(WMA_fit)
accuracy(WMA_fit, train)
accuracy(WMA_fit, test)

# Exponential Moving Average
EMA_fit = EMA(train, 7)
summary(EMA_fit)
accuracy(EMA_fit, train)
accuracy(EMA_fit, test)

# Exponential Smoothing 
SES_fit_0.5 = ses(train, alpha = 0.5 , initial = "simple")
summary(SES_fit_0.5)
accuracy(SES_fit_0.5, test)

##SES_fit_opt = ses(train, initial = "optimal")
##summary(SES_fit_opt)
##accuracy(SES_fit_opt, test)

SES_fit_0.3 = ses(train, alpha = 0.3014 , initial = "simple")
summary(SES_fit_0.3)
accuracy(SES_fit_0.3, test)

#Linear Trend Model

#Exponential Trend Model

#Quadratic Trend Model

#Holts Winter Method 
HW_fit = HoltWinters(train, beta = FALSE, gamma = FALSE)
summary(HW_fit)
accuracy(fitted(HW_fit)[,1], train)
HW_test = forecast(HW_fit, h = length(test))
accuracy(HW_test$mean, test)

#Decomposition Model

#===========================================================================
#Part - Model 
#===========================================================================

#Holts Winter Method 
HW_fit = HoltWinters(train)
summary(HW_fit)
accuracy(fitted(HW_fit)[,1], train)
HW_test = forecast(HW_fit, h = length(test))
accuracy(HW_test$mean, test)

accuracy_ <- rbind(accuracy(fitted(HW_fit)[,1], train), accuracy(HW_test$mean, test))
rownames(accuracy_) <- c("HW Train", "HW Test")

accuracy_

#SES Method
##SES_fit_opt = ses(train, initial = "optimal")
##summary(SES_fit_opt)
##accuracy(SES_fit_opt, test)
#SES_fit_opt_test = forecast(SES_fit_opt, h = length(test))
#accuracy(HW_test$mean, test)

#Seasonal Trend Decomposition using Loess
STL_fit = stl(data_ts, s.window = "periodic")
summary(STL_fit)
seasonal(STL_fit)
plot(data_ts)
lines(seasadj(STL_fit), col = "red", lwd = 2)

#===========================================================================
#Part - C.2 Model Comparison
#===========================================================================

#===========================================================================
#Part - E Auto Correlation Analysis
#===========================================================================

par(mfrow = c(2,1))
acf(data_ts, ylim = c(-1,1), lag.max = length(data_ts)/15)
pacf(data_ts, ylim = c(-1,1), lag.max = length(data_ts)/15)
#the ACF show thee high autocorrelation, so it need trend differencing

ndiffs(data_ts)
nsdiffs(data_ts)

BoxCox.lambda(data_ts)
#Because the BoxCox.lambda log transformation is need

BoxCox.lamda

par(mfrow = c(1,2))
acf(log(data_ts), ylim = c(-1,1), lag.max = 30)
pacf(log(data_ts), ylim = c(-1,1), lag.max = 30)

ndiffs(log(data_ts))
nsdiffs(log(data_ts))

ch.test(log(data_ts), sid = 7)
adf.test(log(data_ts))

acf(diff(log(data_ts),7), ylim = c(-1,1), lag.max = 40)
pacf(diff(log(data_ts),7), ylim = c(-1,1), lag.max = 40)

ch.test(diff(log(data_ts), 7))
ndiffs(diff(log(data_ts),7))
nsdiffs(diff(log(data_ts),7))
#The ACf there is no significant spike, However the PACF is diesdown in between lag

acf(diff(diff(log(data_ts),7), 1), ylim = c(-1,1), lag.max = 30)
pacf(diff(diff(log(data_ts),7), 1), ylim = c(-1,1), lag.max = 30)

ch.test(diff(diff(log(data_ts), 7), 1))
ndiffs(diff(diff(log(data_ts),7), 1))
nsdiffs(diff(log(data_ts),7))

#===========================================================================
#Part - E Providing ARIMA and SARIMA
#===========================================================================

library(lmtest)

#ARIMA 
model1 = auto.arima(log(data_ts), trace=TRUE, D = 1)
summary(model1)
accuracy(model1)

coeftest(model1)
checkresiduals(model1)

#SARIMA1 
model2 = Arima(data_ts, order = c(2,0,2), seasonal = c(0,1,1), lambda = "auto")
summary(model2)
accuracy(model2)

coeftest(model2)
checkresiduals(model2)

#SARIMA
model3 = Arima(data_ts, order = c(0,0,1), seasonal = c(1,1,1), lambda = "auto")
summary(model3)
accuracy(model3)

coeftest(model3)
checkresiduals(model3)

#===========================================================================
#Part - E Forecasting 5 periods 
#===========================================================================

fitModel <- predict(model2, n.ahead = 5*7)
plot(data_ts)
plot(forecast(model2, h = 5))

len <- length(forecast(model2, h =  5)$mean)
forecast(model2, h =  5)$mean[1:len]

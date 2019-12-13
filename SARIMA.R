library(MASS)
library(tseries)
library(forecast)
library(DMwR)
library(nortest)
library(ggplot2)
library(reshape2)
#############################
##Section 1. Data Prep
#Load the data
dat = read.csv("Data.csv", header = TRUE)
dim(dat)
str(dat)
y = dat$pm2.5

#daily avg pm2.5
n = floor(length(y)/24) #1739 days
Y = NULL
for (i in 1:n) {
  avg = mean(y[(24*(i-1)+1):(24*i)])
  Y = c(Y,avg)
}

#Divide into train vs test sets
train_y = Y[1:1471]
test_y = Y[1472:1739]

#Convert to time series data
train_TS = ts(train_y, frequency = 347) 
test_TS = ts(test_y, frequency = 347)  #347 days in a year on avg

################################
##Section 2. Fitting SARIMA model
#1)Exploratory analysis
#first check stationarity - d = D = 0
plot(train_TS, main= 'Daily average PM2.5',ylab='PM2.5',
     xlab = 'Year')
decomp = decompose(train_TS,type='additive')
plot(decomp)

adf.test(train_TS) #p-value = 0.01 (Null: non-stationary)

#difference it once - more stationary
#train_TS = diff(train_TS, lag = 1)
#plot(train_TS, main= 'Daily average PM2.5 - Lag 1',ylab='PM2.5',
#     xlab = 'Year')
#decomp2 = decompose(train_TS,type='additive')
#plot(decomp2)


#choose AR parameters - p = 2, P  = 0
pacf(train_TS) #cuts off ater 2 lags 
pacf(train_TS, lag.max = 365) 
pacf(diff(train_TS), lag.max = 365, main = 'Diff = 1') 

#choose MA(q) - q = 3, D = 1, Q = 0
acf(train_TS) #cuts off ater 3 lags 
acf(train_TS, lag.max = 365) 
acf(diff(train_TS), lag.max = 365, main = 'Diff = 1') 


#2. Fit the model
#model1 = arima(train_TS, order = c(2, 0, 3),
#               xreg=fourier(train_TS, K=4))

model0 =  auto.arima(train_TS, D = 1) #auto selection - fix d and choose p,d by KPSS test (minimize AIC)
#SARIMA (1,0,1,0,1,0)[365]

#3. Evaluate
pred0 = forecast(model0, h=268)
predicted = pred0$mean
#pred1 = forecast(model1, h=268)
#predicted1 = pred1$mean

#normality of the residuals
residual = pred0$residuals
hist(residual)
ad.test(residual)

#4. Plot true vs predicted
day = seq(1,length(test_TS),1)
true = as.numeric(test_TS)
prediction = as.numeric(predicted)
MAE = mean(abs(true - prediction))

dat = as.data.frame(cbind(day,true,prediction))
dat = melt(dat, id.var = "day")
names(dat) = c('Day','Legend','Value')

ggplot(dat) + 
  geom_line(aes(x = Day, y = Value, color = Legend)) + 
  xlab('Day') +
  ylab('PM2.5') +
  ggtitle('True vs Predicted (MAE = 62) -\n SARIMA Model')
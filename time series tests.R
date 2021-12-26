library(tidyverse)
library(readxl)
averages <- read_excel('C:\\Users\\asche\\OneDrive\\Documents\\research project\\world athletics data\\averages_no_comments.xlsx')
averages
w800avgs <- averages[, c('Year', 'W800')]
sd(w800avgs$W800)/sqrt(30)
View(w800avgs)
plot(w800avgs$Year, w800avgs$W800)
#linear regression:
#w800 = alpha+beta(year)+ep
w800_linreg <- lm(W800 ~ Year, data = w800avgs)
summary(w800_linreg)
abline(lm(w800_linreg))
#want to see if significant beta (p in this case)--> here was .367
#want to see if we can predict for 2020.5 (for x = 2020)
98.825091 + 0.009297*2020   
#get 117.605 -> 1:57.605
#need to measure goodness of fit - aikeke criterion
AIC(w800_linreg) #gives 45.18385 (doesn't mean anything on its own, use to compare to other models)
#are there any problems with comparison?
plot(fitted(w800_linreg), resid(w800_linreg))
abline(0,0)
#residual plot looks ok
#let's do a qq plot --> looking for extreme violations of normality
qqnorm(resid(w800_linreg))
qqline(resid(w800_linreg))
#maybe slight problems, but not dramatic (but it's not like this plot works anyway bc there's no independence)

#ARMA model time --> ARMA(1,1)
#p = 1, q = 1
w800avgs_ts <- averages[, c('W800')]
w800avgs_ts <- ts(w800avgs_ts)
View(w800avgs_ts)
arima(w800avgs_ts, order = c(1, 0, 1)) #this says arima but is an arma model because our d in the order is 0. having any other value would make it arima
#but what should the order actually be? we set it to 1,1 but it probably should be something else
arima(w800avgs_ts, order = c(1, 0, 1))


install.packages("tseries")
library(tseries)

#now: going to try adf.test() and kpss.test()
adf.test(w800avgs_ts)
#p value = .4711
kpss.test(w800avgs_ts)
#p value is greater than .1 so it's not going to print the specific p value
#both have p values >.05, so we are not rejecting the stationarity i.e, stationarity works

#detrended time series
install.packages("pracma")
library(pracma)
dt_w800avgs_ts <- detrend(w800avgs_ts)
dt_w800avgs_ts
plot(dt_w800avgs_ts)

#another way of detrending
#find trend that's in there and then remove it
trend <- lm(w800avgs_ts ~ c(1:30))
View(trend)
detrend <- residuals(trend)
detrend

#adf test on detrended data
adf.test(detrend)
kpss.test(detrend)

?arima.sim
arma_test_w800 <- arima(w800avgs_ts, order = c(1, 0, 0))
arma_test_w800


#install forecast package
install.packages("forecast")
library(forecast)
arma_forecast_testw800 <- auto.arima(w800avgs_ts)
arma_forecast_testw800
#0,0,0 basically means all independent --> no evidence that errors are correlated

#because 0,0,0, forecast is going to be the mean of linear regression

#TO DO:
#forecast with 0,0,0 - get forecast with some sort of interval (confidence or prediction)
#forecast with lin reg w/ time as predictor
w800linreg_forecast <- forecast(w800_linreg, h = 1) #error - argument newdata is missing, w no default
#plot(w800linreg_forecast)
#forecast with lin reg w/ intercept only (using x bar)

#record forecasts in sheet, upload later

#forecast with 0,0,0
forecast_autoarima_w800 <- forecast(arma_forecast_testw800, h = 1 )
forecast_autoarima_w800
plot(forecast_autoarima_w800)
#forecast is horizontal bc no drift/trend

#to do:
#do the arma models for without semenya
#look through other races for any with a trend/drift -> maybe w1500/mens 400
#look at w800 times from 2021 - see if outside prediction interval

averages
w800avgs_ns <- averages[, c('Year', 'W800nS')]
w800avgs_ns
#linear regression
w800ns_linreg <- lm(W800nS ~ Year, data = w800avgs_ns)
summary(w800ns_linreg) # p valye is 0.06198
AIC(w800ns_linreg) #44.09804
plot(fitted(w800ns_linreg), resid(w800ns_linreg))
abline(0,0)
qqnorm(resid(w800ns_linreg))
qqline(resid(w800ns_linreg)) #nothing crazy I think
#ARMA model time --> arma(1,1)
w800avgs_ns_ts <- averages[, c('W800nS')]
w800avgs_ns_ts <- ts(w800avgs_ns_ts)
mean(w800avgs_ns_ts)
arima(w800avgs_ns_ts, order = c(1, 0, 1))

#now: going to try adf.test() and kpss.test()
adf.test(w800avgs_ns_ts)
#p value = 0.4214
kpss.test(w800avgs_ns_ts)
#p value = 0.069
arma_test_w800ns <- arima(w800avgs_ns_ts, order = c(1, 0, 0))
arma_test_w800ns #aic = 47.9

#forecasting
arma_forecast_testw800ns <- auto.arima(w800avgs_ns_ts) 
arma_forecast_testw800ns #prediction of 1:57.46, aic = 46.02
forecast_autoarima_w800ns <- forecast(arma_forecast_testw800ns, h = 1)
forecast_autoarima_w800ns

#mens 400 - seems to be some drift/trend
#same w w1500

#mens 400 time
m400avgs <- averages[, c('Year', 'M400')]
View(m400avgs)
m400_linreg <- lm(M400 ~ Year, data = m400avgs)
summary(m400_linreg) # p value is 0.03612, correlation is higher than the women's...women's looks like random noise but both are low 
cooks.distance(m400_linreg)


AIC(m400_linreg) # AIC is -8.974603
qqnorm(resid(m400_linreg))
qqline(resid(m400_linreg))
res = m400_linreg$residuals
fit = m400_linreg$fitted.values
plot(fit, res)
abline(0,0)
install.packages("splines")
library(splines)
fit_spline = smooth.spline(m400avgs$Year, m400avgs$M400)
plot(m400avgs$Year, m400avgs$M400)
#lines(fit_spline, )...
#guess what? it's arma time
m400avgs_ts <- averages[, c('M400')]
m400avgs_ts <- ts(m400avgs_ts)
m400avgs_ts
arima(m400avgs_ts, order = c(1, 0, 1))
adf.test(m400avgs_ts) #p value is 0.5762
kpss.test(m400avgs_ts) #p value greater than 0.1
#different arma test
arma_test_m400 <- arima(m400avgs_ts, order = c(1, 0, 0))
arma_test_m400
#forecasting
arma_forecast_m400 <- auto.arima(m400avgs_ts)
arma_forecast_m400 # --> arima(1,0,0), nonzero mean
forecast_autoarima_m400 <- forecast(arma_forecast_m400, h = 1)
forecast_autoarima_m400
#why is the auto.arima forecast different from the forecast of the time series variable?
#i.e. why is forecast(m400avgs_ts, h = 1) different from the forecast after we do the auto.arima? Because one is actually measuring for stationarity/a relationship and the other is just the data?
#which prediction/forecast do we want to pay attention to?
plot(forecast_autoarima_m400)


#TO DO: Make sure you understand all the output for m400, understand how the predictions got what they got
#include the linear model in there too - what the prediction was, how it works, summary of how we noticed the problems in the linear/normal dist plots, possible influential pts

#w1500
w1500avgs <- averages[,c("Year", "W1500")]
View(w1500avgs)
#linear regression
w1500_linreg <- lm(W1500 ~ Year, data = w1500avgs)
summary(w1500_linreg) #p value: 0.02933
qqnorm(resid(w1500_linreg))
qqline(resid(w1500_linreg))

res = w1500_linreg$residuals
fit = w1500_linreg$fitted.values
plot(fit, res)
abline(0,0)
#LOTS of values off the line
w1500avgs_ts <- averages[, c("W1500")]
w1500avgs_ts <- ts(w1500avgs_ts)
w1500avgs_ts
arima(w1500avgs_ts, order = c(1, 0, 1))
adf.test(w1500avgs_ts) #p value 0.6489
kpss.test(w1500avgs_ts)
arima(w1500avgs_ts, order = c(1,0,0))
#forecasting
arma_forecast_w1500 <- auto.arima(w1500avgs_ts)
arma_forecast_w1500 #arma (0,0,1)...interesting
forecast_autoarima_w1500 <- forecast(arma_forecast_w1500, h = 1)
forecast_autoarima_w1500

#m1500
m1500avgs <- averages[, c('Year', 'M1500')]
View(m1500avgs)
#linear regression
m1500_linreg <- lm(M1500 ~ Year, data = m1500avgs)
summary(m1500_linreg) #p value 0.003983
qqnorm(resid(m1500_linreg))
qqline(resid(m1500_linreg)) #some values off the line

res = m1500_linreg$residuals
fit = m1500_linreg$fitted.values
plot(fit, res)
abline(0,0)
m1500avgs_ts <- averages[, c('M1500')]
m1500avgs_ts <- ts(m1500avgs_ts)
arima(m1500avgs_ts, order = c(1, 0, 1))
adf.test(m1500avgs_ts) #p value 0.3425
kpss.test(m1500avgs_ts)
arima(m1500avgs_ts, order = c(1,0,0))
#forecasting
arma_forecast_m1500 <- auto.arima(m1500avgs_ts)
arma_forecast_m1500 #arma (0,1,0)...interesting
forecast_autoarima_m1500 <- forecast(arma_forecast_m1500, h = 1)
forecast_autoarima_m1500 #CRAZY confidence interval!

#w400
w400avgs <- averages[, c('Year', 'W400')]
w400avgs
#linear regression
w400_linreg <- lm(W400 ~ Year, data = w400avgs)
summary(w400_linreg) #p value = 0.2709
qqnorm(resid(w400_linreg))
qqline(resid(w400_linreg)) #bit off the line, not as bad as some others though
w400avgs_ts <- averages[, c('W400')]
w400avgs_ts <- ts(w400avgs_ts)
arima(w400avgs_ts, order = c(1, 0, 1))
adf.test(w400avgs_ts) #p value 0.3473
kpss.test(w400avgs_ts)
arima(w400avgs_ts, order = c(1,0,0)) 
arma_forecast_w400 <- auto.arima(w400avgs_ts) #(0,0,1) again (same as w1500)
arma_forecast_w400 
forecast_autoarima_w400 <- forecast(arma_forecast_w400, h = 1)
forecast_autoarima_w400

#m800
m800avgs <- averages[, c("Year", "M800")]
m800avgs
#linear regression
m800_linreg <- lm(M800 ~ Year, data = m800avgs)
summary(m800_linreg) #p value = 0.01778
qqnorm(resid(m800_linreg))
qqline(resid(m800_linreg)) #some values off
m800avgs_ts <- averages[, c("M800")]
m800avgs_ts <- ts(m800avgs_ts)
arima(m800avgs_ts, order = c(1, 0, 1))
adf.test(m800avgs_ts) 
kpss.test(m800avgs_ts)
arima(m800avgs_ts, order = c(1,0,0)) 
arma_forecast_m800 <- auto.arima(m800avgs_ts) 
arma_forecast_m800 #(0,0,0)
forecast_autoarima_m800 <- forecast(arma_forecast_m800, h = 1)
forecast_autoarima_m800


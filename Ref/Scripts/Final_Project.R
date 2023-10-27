library(forecast)
install.packages("TSA")
library(TSA)
library(readxl)

getwd()
setwd('/Users/harsh/Documents/Uchicago/Timeseries/Final Project/Data')
file_list <- list.files(path="/Users/harsh/Documents/Uchicago/Timeseries/Final Project/Data")
dataset <- data.frame()
file_list
for (i in 1:length(file_list)){
  temp_data <- read.csv(file_list[i])
  #temp_data <- head(temp_data, -2)
  #temp_data <- tail(temp_data, -2)
  dataset <- rbind(dataset, temp_data)
}
dataset$avgTemp <- (dataset$tempmax+dataset$tempmin)/2
write.csv(dataset, file = 'finalData.csv', row.names = FALSE)

dataset <- read.csv('finalData.csv')

df_ts <- ts(dataset, start = c(2012,5), frequency = 365.25)
df_ts_1 <- ts(dataset, start = c(2012,121.25), frequency = 365.25)

help('ts')

df_ts_1[,'avgTemp']

plot(df_ts[,'tempmax'], main = 'Temp Max Timeseries')
plot(df_ts[,'avgTemp'], main = 'Avg Temp Timeseries')

BoxCox.lambda(df_ts[,'avgTemp']) #There is some variance in the data

fit_autO_arima <- auto.arima(df_ts[,'avgTemp'], seasonal = TRUE, lambda = 1.4)


df_ts_boxcox <- BoxCox(df_ts[,'avgTemp'], lambda = 1.5)

Acf(df_ts_boxcox)
Pacf(df_ts_boxcox)

df_ts[,'avgTemp']

fit_add <- decompose(df_ts_boxcox, type = 'additive')
plot(fit_add)

fit_mul <- decompose(df_ts_boxcox, type = 'multiplicative')
plot(fit_mul)

help("decompose")

df_box_cox <- BoxCox(df_ts[,'avgTemp'], lambda = 'auto')
df_box_cox
help("BoxCox")

#From the spectral analysis there is frequency of only one year i.e for 368
temp <- periodogram(df_ts[,'avgTemp'])
periodogram(df_box_cox)
periodogram(df_ts[,'avgTemp'], log = 'yes')
help("periodogram")
temp$freq
max_freq <- temp$freq[which.max(temp$spec)]
Seasonlaity <- 1/max_freq
Seasonlaity
round(temp$spec/1e5,3)

help('decompose')

help("diff")

help('auto.arima')

setwd('/Users/harsh/Documents/Uchicago/Timeseries/Assignments/Assignment4')
source('eacf.R')

cor(df_ts[,'humidity'], df_ts[,'temp'])
cor(df_ts[,'windspeed'], df_ts[,'temp'])
cor(df_ts[,'solarradiation'], df_ts[,'temp'])
cor(df_ts[,'cloudcover'], df_ts[,'temp'])
cor(df_ts[,'windgust'], df_ts[,'temp'])
cor(df_ts[,'dew'], df_ts[,'temp'])

cor_mat <- cor(df_ts[, c('temp','humidity', 'windspeed', 'solarradiation', 'cloudcover', 'dew')])
cor_mat
corrplot(cor_mat)

BoxCox.lambda(df_ts[,'dew']+50)
BoxCox.lambda(df_ts[,'solarradiation'])


df_ts_boxcox_temp <- BoxCox(train_ts_1[,'temp']+50, lambda = 2)
df_ts_boxcox_dew <- BoxCox(train_ts_1[,'dew']+50, lambda = 2)

VARselect(cbind(df_ts_boxcox_temp, df_ts_boxcox_dew),
          lag.max = 10, type="trend")$selection

var_model <- VAR(cbind(df_ts_boxcox_temp, df_ts_boxcox_dew), 
    p = 6, type = "both", season = 365.25)

pred <- predict(var_model, n.ahead = 730)

plot(ts(actual_scale_pred_temp))

temp.pred <- ts(pred$fcst$df_ts_boxcox_temp[,1], st = c(2021, 121), fr = 365.25)

temp.pred
test_ts_1

window(df_ts_boxcox_temp, start = c(2020, 121))

plot(cbind(InvBoxCox(df_ts_boxcox_temp, lambda = 2), actual_scale_pred_temp))


ts.plot(cbind(window(df_ts_boxcox_temp, start = c(2020, 121)), temp.pred), lty = 1:2, main = 'Forecast uisng VAR(6)')

plot(ts(pred$fcst$df_ts_boxcox_temp))

actual_scale_pred_temp[,'fcst']

actual_scale_pred_temp <- InvBoxCox(pred$fcst$df_ts_boxcox_temp, lambda = 2)

error <- test_ts_1[,'temp']- ts(actual_scale_pred_temp[,'fcst']-50, 
                                start = c(2021, 121), frequency = 365.25)

sqrt(mean(error)^2)

pred

install.packages('corrplot')

library(corrplot)



acf(resid(var_model)[, 1], main='Residulas of Avg Temp')
acf(resid(var_model)[, 2], main='Residulas of Dew')

summary(var_model)

library(vars)



train_ts_2 <- window(df_ts[,'temp'], start = c(2012, 121.25), end = c(2021, 120))
test_ts_2 <- window(df_ts[,'temp'], start = c(2021, 121))

train_ts_1 <- window(df_ts_1, start = c(2012, 121.25), end = c(2021, 120))
test_ts_1 <- window(df_ts_1, start = c(2021, 121))


eacf(train_ts_2)



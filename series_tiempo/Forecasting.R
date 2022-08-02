#setwd("G:/My Drive/Files/MCD/Modelo de Negocios/Caso de Estudio Forecasting")
setwd("C:/Users/chccr/Dropbox/Maestría MCD/MCD Modelo de Negocios/series_tiempo")


getwd()
library(timeSeries)
library(forecast)
library(tseries)

Competidor <- read.csv('Competidor1.csv', sep=',', header=T)
View(Competidor)
plot(Competidor$Product1)
ts.CompProd <- ts(Competidor$Product1, frequency=52,
                    start=c(2019,1))
plot(ts.CompProd)
ARIMA.CompProd <- auto.arima(ts.CompProd, D=1) #D=1 forza seasonalidad al modelo
summary(ARIMA.CompProd)
plot(ARIMA.CompProd$x, col='blue')
par(new=T)
plot(ARIMA.CompProd$fitted, col='red')
title(main="Real vs Forecast")
Forecast.CompProd <- forecast(ARIMA.CompProd, 25,
                                level=99)
Forecast.CompProd
plot(Forecast.CompProd, main="Strong AWAP Forecasting",
     ylab="Strong AWAP Volume")
Competidor$Prod1Fitted<-ARIMA.CompProd$fitted
Competidor$ForecastProd1<-Forecast.CompProd$mean
View(Competidor)
write.csv(Competidor, 'Competidor1_Forecast.csv', row.names = F)

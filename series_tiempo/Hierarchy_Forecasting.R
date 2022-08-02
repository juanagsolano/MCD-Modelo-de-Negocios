setwd("G:/My Drive/Files/MCD/Modelo de Negocios/Caso de Estudio Forecasting")

S <-matrix(c(1,1,1,1,1,
           1,0,0,0,0,
           0,1,0,0,0,
           0,0,1,0,0,
           0,0,0,1,0,
           0,0,0,0,1), nrow=6, ncol=5, byrow = T)
S%*%solve(t(S)%*%S)%*%t(S)

library(timeSeries)
library(forecast)
library(tseries)

Participacion <- read.csv('Participacion.csv', sep=',', header=T)
View(Participacion)
plot(Participacion$Competidor.05)
ts.PartComp <- ts(Participacion$Competidor.05, frequency=52,
                  start=c(2019,1))
plot(ts.PartComp)
ARIMA.PartComp <- auto.arima(ts.PartComp, D=1) #D=1 forza seasonalidad al modelo
summary(ARIMA.PartComp)
plot(ARIMA.PartComp$x, col='blue')
par(new=T)
plot(ARIMA.PartComp$fitted, col='red')
title(main="Real vs Forecast")
Forecast.PartComp <- forecast(ARIMA.PartComp, 12,
                              level=99)
Forecast.PartComp
plot(Forecast.PartComp, main="Strong AWAP Forecasting",
     ylab="Strong AWAP Volume")
Participacion$Comp4Fitted<-ARIMA.PartComp$fitted
Participacion$ForecastComp5 <- 0
Participacion$ForecastComp5[1:12]<- Forecast.PartComp$mean
View(Participacion)
Participacion$PartTotal <- 1
Participacion$ForecastPartTotal = rowSums(Participacion[,c("ForecastComp1", "ForecastComp2", "ForecastComp3"
                                                   ,"ForecastComp4", "ForecastComp5")])
View(Participacion)

Participacion$HTSForecastTotal<-0
Participacion$HTSForecastComp1<-0
Participacion$HTSForecastComp2<-0
Participacion$HTSForecastComp3<-0
Participacion$HTSForecastComp4<-0
Participacion$HTSForecastComp5<-0
for (i in 1:12){
  V<- as.numeric(Participacion[i, c("PartTotal", "ForecastComp1", "ForecastComp2", "ForecastComp3"
                                     ,"ForecastComp4", "ForecastComp5")])
  r <-(S%*%solve(t(S)%*%S)%*%t(S)) %*% V
  Participacion$HTSForecastTotal[i] <- r[1]
  Participacion$HTSForecastComp1[i] <- r[2]
  Participacion$HTSForecastComp2[i] <- r[3]
  Participacion$HTSForecastComp3[i] <- r[4]
  Participacion$HTSForecastComp4[i] <- r[5]
  Participacion$HTSForecastComp5[i] <- r[6]
  }
View(Participacion)

write.csv(Participacion, 'Participacion_Forecast.csv', row.names = F)

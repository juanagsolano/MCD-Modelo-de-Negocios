###################################Serie Jerarquica#######################

Participacion <- read.csv("C:/Users/chccr/Dropbox/Maestría MCD/MCD Modelo de Negocios/series_tiempo/competidores/Competidor1.csv",header =T,sep=",")

plot(Participacion$Competidor1)
plot(Participacion$Competidor2)
plot(Participacion$Competidor3)
plot(Participacion$Competidor4)
plot(Participacion$Competidor5)

ts.C1 <- ts(Participacion[,2], frequency = 52, start = c(2019,1))
ts.C2 <- ts(Participacion[,3], frequency = 52, start = c(2019,1))
ts.C3 <- ts(Participacion[,4], frequency = 52, start = c(2019,1))
ts.C4 <- ts(Participacion[,5], frequency = 52, start = c(2019,1))
ts.C5 <- ts(Participacion[,6], frequency = 52, start = c(2019,1))

plot(ts.C1)
plot(ts.C2)
plot(ts.C3)
plot(ts.C4)
plot(ts.C5)

ARIMA.C1 <- auto.arima(ts.C1,D=1)

summary(ARIMA.C1)
plot(ARIMA.C1$x,col="blue")
par(new=TRUE)
plot(ARIMA.C1$fitted,col="red") 
title(main="Real vs Forecast")

ARIMA.C2 <- auto.arima(ts.C2, D=1)

summary(ARIMA.C2)
plot(ARIMA.C2$x,col="blue")
par(new=TRUE)
plot(ARIMA.C2$fitted,col="red") 
title(main="Real vs Forecast")


ARIMA.C3 <- auto.arima(ts.C3, D=1)

summary(ARIMA.C3)
plot(ARIMA.C3$x,col="blue")
par(new=TRUE)
plot(ARIMA.C3$fitted,col="red") 
title(main="Real vs Forecast")


ARIMA.C4 <- auto.arima(ts.C4,D=1)

summary(ARIMA.C4)
plot(ARIMA.C4$x,col="blue")
par(new=TRUE)
plot(ARIMA.C4$fitted,col="red") 
title(main="Real vs Forecast")


ARIMA.C5 <- auto.arima(ts.C5, D=1)

summary(ARIMA.C5)
plot(ARIMA.C5$x,col="blue")
par(new=TRUE)
plot(ARIMA.C5$fitted,col="red") 
title(main="Real vs Forecast")

Forecast.C1<- forecast(ARIMA.C1,12,level = 99)

plot(Forecast.C1,main="P1C1 Forecasting",
     ylab="P1C1 Volume")

Forecast.C2<- forecast(ARIMA.C2,12,level = 99)

plot(Forecast.C2,main="P1C2 Forecasting",
     ylab="P1C2 Volume")

Forecast.C3<- forecast(ARIMA.C3,12,level = 99)

plot(Forecast.C3,main="P1C3 Forecasting",
     ylab="P1C3 Volume")

Forecast.C4<- forecast(ARIMA.C4,12,level = 99)

plot(Forecast.C4,main="P1C4 Forecasting",
     ylab="P1C4 Volume")

Forecast.C5<- forecast(ARIMA.C5,12,level = 99)

plot(Forecast.C5,main="P1C5 Forecasting",
     ylab="P1C5 Volume")

Forecast_Total <- c(1,1,1,1,1,1,1,1,1,1,1,1)

Forecast_P1 <- data.frame("Fecha"=FechasForecast,
                          "Total"=Forecast_Total,
                          "Competidor1"=Forecast.C1$mean,
                          "Competidor2"=Forecast.C2$mean,
                          "Competidor3"=Forecast.C3$mean,
                          "Competidor4"=Forecast.C4$mean,
                          "Competidor5"=Forecast.C5$mean)

S <- matrix(c(1,1,0,0,0,0,
              1,0,1,0,0,0,
              1,0,0,1,0,0,
              1,0,0,0,1,0,
              1,0,0,0,0,1), nrow = 6 , ncol = 5)

V <- as.matrix(Forecast_P1[2:7])
t <- t(V)

r = (S %*% solve(t(S)%*%S) %*%t(S)) %*% Vt
rt <- t(r)


Forecast_Jer <- data.frame("Fecha"=FechasForecast,
                           "Total"=round(rt[,1],0),
                           "Competidor1"=rt[,2],
                           "Competidor2"=rt[,3],
                           "Competidor3"=rt[,4],
                           "Competidor4"=rt[,5],
                           "Competidor5"=rt[,6])

write.csv(Forecast_Jer, "C:/Users/jalopalv/Desktop/Alberto/MCD/4toTetra/BI/Practica_4/Forecast_Jer.csv")

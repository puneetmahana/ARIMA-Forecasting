library(forecast)
library(fpp)

plot(sunspotarea, main= " Original Dataset")
sunspotarea1=diff(sunspotarea)
plot(sunspotarea1)
acf(sunspotarea1)
acf(sunspotarea1, plot=FALSE)
pacf(sunspotarea1)
pacf(sunspotarea1, plot=FALSE)
fit<-Arima(sunspotarea, order=c(2,1,5))
tsdisplay(residuals(fit))
accuracy(fit)
plot(forecast(fit, h=25), main="ARIMA FORECASTING", xlab="time", ylab="sunspotarea")

#accuracy
sun <- window(sunspotarea, end=2000)
plot(sun, xlim=c(1874,2015))
lines(meanf(sun,h=25)$mean, col=4)
lines(rwf(sun,h=25)$mean, col=2)
lines(rwf(sun,drift=TRUE,h=25)$mean, col=3)
legend("topleft", lty=1, col=c(4,2,3),
       legend=c("Mean method","Naive method","Drift method"))
lines(sunspotarea)

sun1 <- meanf(sun,h=25)
sun2 <- rwf(sun,h=25)
sun3 <- rwf(sun,h=25, drift=TRUE)
sun4<- Arima(sunspotarea, order=c(2,1,5))

sunnew <- window(sunspotarea, start=2001)
accuracy(sun1, sunnew)
accuracy(sun2, sunnew)
accuracy(sun3, sunnew)
accuracy(fit)


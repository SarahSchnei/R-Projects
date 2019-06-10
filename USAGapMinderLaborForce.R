United States	
"59.4	59.3	58.8	58.7	58.7	58.9	59.2	59.6	59.6	60.1	
60.4	60.2	60.4	60.8	61.3	61.2	61.6	62.3	63.2	63.7	
63.8	63.9	64	64	64.4	64.8	65.3	65.6	65.9	66.5	66.5
66.2	66.4	66.3	66.7	66.7	66.9	67.2	67.1	67.1	67.1
66.8	66.6	66.2	66	66	66.2	66	66	65.4	64.7	64.1	
63.7	63.2	62.9	62.7	62.8	62.9"

usdata = scan()

usdatats = ts(usdata, start = 1960)

plot(usdata, ylab = "Labor Force Participation Rate for all since 1960")

library(forecast)
arimaus = auto.arima(usdatats)

plot(forecast(arimaus, h=55))

holttrend = holt(usdatats, h = 10)
holtdamped = holt(usdatats, h=10, damped = T)
arimafore = forecast(auto.arima(usdatats), h=10)


library(ggplot2)
# 3 Forecast Lines as Comparison
autoplot(usdatats) +
  forecast::autolayer(holttrend$mean, series = "Holt Linear Trend") +
  forecast::autolayer(holtdamped$mean, series = "Holt Damped Trend") +
  forecast::autolayer(arimafore$mean, series = "ARIMA") +
  xlab("year") + ylab("Labour Force Participation Rate") + 
  guides(colour=guide_legend(title="Forecast Method")) + theme(legend.position = c(0.8, 0.2)) +
  ggtitle("USA") + theme(plot.title=element_text(family="Times", hjust = 0.5, color = "blue",
                                                      face="bold", size=15))

autoplot(usdatats) + geom_line(size=2) +
  forecast::autolayer(holttrend$fitted, series = "Holt Linear Trend", size = 1.1) +
  forecast::autolayer(holtdamped$fitted, series = "Holt Damped Trend", size = 1.1) +
  forecast::autolayer(arimafore$fitted, series = "ARIMA", size = 1.1) +
  xlab("year") + ylab("Labour Force Participation Rate") + 
  guides(colour=guide_legend(title="Forecast Method")) + theme(legend.position = c(0.8, 0.2)) +
  ggtitle("USA Model vs actual performance") + theme(plot.title=element_text(family="Times", hjust = 0.5, 
                                                      color = "blue", face="bold", size=15))
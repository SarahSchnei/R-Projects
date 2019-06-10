library(tidyr)
revenue <- separate(revenue, col = V2,
                    sep = c(2, -3),
                    into = c("rest", "data", "rest2"))

myts <- ts(as.numeric(revenue$data), start = 1997, frequency = 12)

summary(myts)


library(forecast)
myts <- tsclean(myts)

summary(myts)

plot(myts)

mynnetar <- nnetar(myts)

nnetforecast <- forecast(mynnetar, h = 36, PI = T)

library(ggplot2)
autoplot(nnetforecast)

# data we need for the graph
data <- nnetforecast$x
lower <- nnetforecast$lower[,2]
upper <- nnetforecast$upper[,2]
pforecast <- nnetforecast$mean
mydata <- cbind(data, lower, upper,
                pforecast)
library(dygraphs)
dygraph(mydata, main = "Oregon Campsite Restaurant") %>% 
  dyRangeSelector() %>% 
  dySeries(name = "data", label = "Revenue Data") %>%
  dySeries(c("lower","pforecast","upper"), label = "Revenue Forecast") %>%
  dyLegend(show = "always", hideOnMouseOut = FALSE) %>%
  dyAxis("y", label = "Monthly Revenue USD") %>%
  dyHighlight(highlightCircleSize = 5,
              highlightSeriesOpts = list(strokeWidth = 2)) %>%
  dyOptions(axisLineColor = "navy", gridLineColor = "grey") %>%
  dyAnnotation("2010-8-1", text = "CF", tooltip = "Camp Festival", attachAtBottom = T)

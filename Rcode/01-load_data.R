library(data.table)
library(tidyr)

covid19Timeseries = fread("~/GitHub/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")


italy = covid19Timeseries[`Country/Region` == "Italy", -c(1:4)] %>% colSums
usa = covid19Timeseries[`Country/Region` == "US", -c(1:4)] %>% colSums

days = 1:58

italyPop = 60.48
usaPop = 327

plot(days, italy/italyPop, type = 'b', col  = "red")
lines(days, usa/usaPop, type = 'b', col= "blue")

italyTrunc = log(italy[-c(1:30)])
usaTrunc = log(usa[-c(1:30)])
daysTrunc = days[-c(1:30)] - 30


plot(daysTrunc, italyTrunc, type = 'b', col  = "red", ylim = c(0, 15))
lines(daysTrunc,usaTrunc, type = 'b', col= "blue")

italyLm = lm(italyTrunc ~ daysTrunc)$coef %>% print
usaLm = lm(usaTrunc ~ daysTrunc)$coef %>% print

abline(italyLm, col = 'red')
abline(usaLm, col = 'blue')
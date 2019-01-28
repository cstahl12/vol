library(timeSeries)
library(rugarch)
library(tidyquant)
library(ggplot2)
library(purrr)
library(dplyr)
library(tidyr)
library(lubridate)
library(FatTailsR)
library(stringr)
library(ggthemes)
library(forecast)

av_api_key("VOJYOHWHDXCGOTWQ")
date_from <- "2002-07-01"
date_price <- "2019-01-27"
date_strike <- "2019-04-18"

date_arr <- weekdays(seq(as.Date(date_price),
                         as.Date(date_strike),
                         by="days"),
                     abbreviate = TRUE)

days <- data.frame("DOW" = (date_arr)) %>%
  filter(!`DOW` %in% c("Sat", "Sun"))

days_sim <- length(days$DOW)

ticker_symbol <- "QQQ"

prices <- tq_get(ticker_symbol,
                 get = "alphavantager",
                 av_fun = "TIME_SERIES_DAILY_ADJUSTED",
                 outputsize = "full")

last_price <- prices$adjusted_close[length(prices$adjusted_close)]
start_price <- last_price

rets <- prices %>%
  mutate(`return` = log(`adjusted_close` / lag(`adjusted_close`, 1))) %>%
  drop_na()

rxts <- xts(rets$return, order.by = as.Date(rets$timestamp))

ggplot() +
  geom_line(aes(y = rets$return, x = rets$timestamp))

q <- 1
p <- 1

arima_fit <- arima(rxts, order=c(p, 0, q))

arfor <- forecast(arima_fit, days_sim)


# Specify and fit the GARCH model
spec <- ugarchspec(
  variance.model =list(garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(p, q), include.mean = T),
  distribution.model = "sged"
)

fit <- ugarchfit(spec, rxts, solver = 'hybrid')

gfor <- ugarchforecast(fit, n.ahead = 10)




mprice <- cumprod(1 + arfor$mean) * start_price
uprice <- cumprod(1 + arfor$mean + as.numeric(sigma(gfor))) * start_price
lprice <- cumprod(1 + arfor$mean - as.numeric(sigma(gfor))) * start_price
ggplot() +
  geom_line(aes(y = mean_price, x = c(1:days_sim))) +
  geom_line(aes(y = uprice, x = c(1:days_sim))) +
  geom_line(aes(y = lprice, x = c(1:days_sim)))

sims <- data.frame(replicate(10, cumprod(1 + rnorm(30, mean = fitted(gfor), sd = sigma(gfor))) * start_price)) %>%
  mutate(`trial` = c(1:30))
  gather()

ggplot(data = sims) +
  geom_line(aes(y = `value`, x = c(1:30), text = `key`))


vol <- sigma(gfor) * sqrt(365/days_sim)


vol


library(tidyquant)
library(ggplot2)
library(purrr)
library(dplyr)
library(tidyr)
library(lubridate)
library(FatTailsR)
library(stringr)
library(plotly)
library(ggthemes)

av_api_key("VOJYOHWHDXCGOTWQ")
date_from <- "2002-07-01"
date_price <- "2019-01-22"
date_strike <- "2019-04-18"

date_arr <- weekdays(seq(as.Date(date_price),
                         as.Date(date_strike),
                         by="days"),
                     abbreviate = TRUE)

days <- data.frame("Day" = date_arr) %>%
  filter(!`Day` %in% c("Sat", "Sun"))

days_sim <- length(days$Day)

ticker_symbol <- "SMH"

num_sim <- 100000
div <- 0

prices <- tq_get(ticker_symbol,
                 get = "alphavantager",
                 av_fun = "TIME_SERIES_DAILY_ADJUSTED",
                 outputsize = "full")

last_price <- prices$adjusted_close[length(prices$adjusted_close)]
start_price <- 90.19

rets <- prices %>%
  mutate(`return` = log(`adjusted_close` / lag(`adjusted_close`, days_sim))) %>%
  drop_na()

kfit <- regkienerLX(rets$return)
dist <- data.frame("return" = rkiener4(num_sim, m = 0, g = kfit$coefk4[2], k = kfit$coefk4[3], e = kfit$coefk4[4])) %>%
  mutate(`price` = (start_price * (1 + `return`)) - div)

quantiles <- c(.3, .7)
q <- quantile(dist$price, quantiles)

# ggplot(data = dist) +
#   geom_histogram(aes(x = `price`), bins = 100)

d = 1.0
cs <- as.numeric(round(q[2]))
ps <- as.numeric(round(q[1]))

c <- (sum(as.numeric(dist$price > cs)) / num_sim)
cs
c * d

p <- (sum(as.numeric(dist$price < ps)) / num_sim)
ps
p * d





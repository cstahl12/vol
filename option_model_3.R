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
date_price <- "2019-01-27"
date_strike <- "2019-04-18"

date_arr <- weekdays(seq(as.Date(date_price),
                         as.Date(date_strike),
                         by="days"),
                     abbreviate = TRUE)

days <- data.frame("Days" = date_arr) %>%
  filter(!`Days` %in% c("Sat", "Sun"))

days_sim <- length(days$Days)

ticker_symbol <- "SPY"

prices <- tq_get(ticker_symbol,
                 get = "alphavantager",
                 av_fun = "TIME_SERIES_DAILY_ADJUSTED",
                 outputsize = "full")

num_sim <- 10000
div <- 0.0

last_price <- prices$adjusted_close[length(prices$adjusted_close)]
start_price <- last_price

rets <- prices %>%
  mutate(`return` = log(`adjusted_close` / lag(`adjusted_close`, 1))) %>%
  drop_na()

kfit <- regkienerLX(rets$return)

dist1 <- data.frame(replicate(num_sim, rkiener4(days_sim,
                                                m = kfit$coefk4[1],
                                                g = kfit$coefk4[2],
                                                k = kfit$coefk4[3],
                                                e = kfit$coefk4[4]))) %>%
  gather(key = "iteration", value = "value") %>%
  nest(-`iteration`) %>%
  mutate(`price` = map_dbl(data, function(x) { (cumprod(1 + x$value) * start_price)[days_sim] - div }))

dist2 <- data.frame(replicate(num_sim, rnorm(days_sim, mean(rets$return), sd(rets$return)))) %>%
  gather(key = "iteration", value = "value") %>%
  nest(-`iteration`) %>%
  mutate(`price` = map_dbl(data, function(x) { (cumprod(1 + x$value) * start_price)[days_sim] - div }))

ggplot(data = dist1, aes(x = price)) +
  geom_histogram(bins = 100)


quantiles <- c(.01, .05, .1, .15, .2, .25, .3, .35, .4, .45, .5, .55, .6, .65, .7, .75, .8, .85, .9, .95, .99)
q1 <- quantile(dist1$price, quantiles)
q2 <- quantile(dist2$price, quantiles)

ggplot() +
  geom_line(aes(y = q1, x = quantiles), colour = "black") +
  geom_line(aes(y = q2, x = quantiles), colour = "blue") +
  geom_hline(yintercept = start_price) 


d = 1.0
cs <- as.numeric(round(q1[14]))
ps <- as.numeric(round(q1[4]))

c <- (sum(as.numeric(dist1$price > cs)) / num_sim)
cs
c * d

p <- (sum(as.numeric(dist1$price < ps)) / num_sim)
ps
p * d

(mean(dist1$price) - start_price) / start_price



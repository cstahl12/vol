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

cum_return <- function(x, p){
  return(cumprod(x+1) * p)
}

simulate_prices <- function(rets, days_sim, num_sim, start_price, n = FALSE){
  
  vol = sd(rets$ret, na.rm = TRUE)
  print(paste0("Vol: ", sqrt(252)*vol))
  
  for(i in c(1 : days_sim)) {
    
    if(n){
      dist <- rnorm(num_sim, mean = 0, sd = vol)
    }else{
      kfit <- regkienerLX(rets$ret)
      dist <- rkiener4(num_sim, m = 0, g = kfit$coefk4[2], k = kfit$coefk4[3], e = kfit$coefk4[4])
    }
    
    if(i == 1){
      df_sim <- data.frame(dist)
    }else{
      df_sim <- cbind(df_sim, data.frame(dist))
    }
  }
  
  df_price <- df_sim %>%
    t() %>%
    as.data.frame() %>%
    apply(2, cum_return, p = start_price) %>%
    t() %>%
    as.data.frame()
  
  df_flat <- data.frame("Price" = unlist(df_price[, days_sim]))
  
  return(df_flat)
}

run_sim <- function(prices, start_price, days_sim, num_sim, n = FALSE){
  
  rets <- prices %>%
    tq_transmute(select = adjusted_close, 
                 mutate_fun = periodReturn, 
                 period = "daily", 
                 type = "log",
                 col_rename = "ret")
  
  df_price <- simulate_prices(rets, days_sim, num_sim, start_price, n)
  
  return(df_price)
}

av_api_key("VOJYOHWHDXCGOTWQ")
date_from <- "2000-07-01"
date_price <- "2019-01-17"
date_strike <- "2019-02-15"

date_arr <- weekdays(seq(as.Date(date_price),
                         as.Date(date_strike),
                         by="days"),
                     abbreviate = TRUE)

days <- data.frame("Day" = date_arr) %>%
  filter(!`Day` %in% c("Sat", "Sun"))

days_sim <- length(days$Day)

ticker_symbol <- "SMH"
start_price <- 82
num_sim <- 100000

prices <- tq_get(ticker_symbol,
                 get = "alphavantager",
                 av_fun = "TIME_SERIES_DAILY_ADJUSTED",
                 outputsize = "full")

last_price <- prices$adjusted_close[length(prices$adjusted_close)]

df_k <- run_sim(prices %>% filter(`timestamp` >= "2005-01-01"), start_price, days_sim, num_sim, n = FALSE)

quantiles <- c(.25, .75)
q <- quantile(df_k$Price, quantiles)

d = 1.0
cs <- as.numeric(round(q[2]))
ps <- as.numeric(round(q[1]))

c <- (sum(as.numeric(df_k$Price > cs)) / num_sim)
cs
c * d

p <- (sum(as.numeric(df_k$Price < ps)) / num_sim)
ps
p * d


ggplot() +
  geom_histogram(data = df_k, aes(x = `Price`), bins = 500, alpha = .55, fill = "blue") +
  geom_vline(xintercept = ps) +
  geom_vline(xintercept = cs)





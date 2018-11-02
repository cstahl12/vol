library(tidyquant)
library(ggplot2)
library(purrr)
library(dplyr)
library(tidyr)
library(lubridate)
library(FatTailsR)
library(PerformanceAnalytics)
library(stringr)

cum_return <- function(x, p){
  return(cumprod(x+1) * p)
}

simulate_prices <- function(kfit, days_sim, num_sim, start_price){
  
  for(i in c(1 : days_sim)) {
    
    kfat <- rkiener4(num_sim, m = 0, g = kfit$coefk4[2], k = kfit$coefk4[3], e = kfit$coefk4[4])
    
    if(i == 1){
      df_sim <- data.frame(kfat)
    }else{
      df_sim <- cbind(df_sim, data.frame(kfat))
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

run_sim <- function(prices, start_price, days_sim, num_sim){

  rets <- prices %>%
    tq_transmute(select = adjusted, 
                 mutate_fun = periodReturn, 
                 period = "daily", 
                 col_rename = "ret")
  
  kfit <- regkienerLX(rets$ret, model = "K4")
  
  df_price <- simulate_prices(kfit, days_sim, num_sim, start_price)
  
  return(df_price)
}

date_from <- "2003-07-01"
date_to <- "2018-11-01"


sym <- "GLD"
start_price <- 116.45
days_sim <- 77

prices <- tq_get(sym,
                 get = "stock.prices",
                 from = date_from,
                 to = date_to)

num_sim <- 100000
df_results <- run_sim(prices, start_price, days_sim, num_sim)

d = 1
cs <- 120
ps <- 113

c <- (sum(as.numeric(df_results$Price > cs)) / num_sim)
c * d
p <- sum(as.numeric(df_results$Price < ps)) / num_sim
p * d

d * (p+c)


ggplot(data = df_results) +
  geom_histogram(aes(x = `Price`), bins = 100)



library(tidyquant)
library(ggplot2)
library(purrr)
library(dplyr)
library(tidyr)
library(lubridate)
library(FatTailsR)
library(PerformanceAnalytics)

simulate_prices <- function(kfit, days_sim, num_sim, start_price){
  
  for(i in c(1 : days_sim)) {
    
    kfat <- rkiener4(num_sim, m = 0, g = kfit$coefk4[2], k = kfit$coefk4[3], e = kfit$coefk4[4])
    
    if(i == 1){
      df_sim <- data.frame(kfat)
    }else{
      df_sim <- cbind(df_sim, data.frame(kfat))
    }
  }
  
  df_sim <- data.frame(t(df_sim))
  
  df_price <- df_sim %>%
    apply(2, Return.cumulative) %>%
    data.frame() %>%
    mutate(`Price` = start_price * (1 + `.`))
  
  colnames(df_price) <- c("CReturn", "Price")
  
  return(df_price)
}

run_sim <- function(symbol, date_from, date_to, days_sim, num_sim){
  prices <- tq_get(symbol,
                   get = "stock.prices",
                   from = date_from,
                   to = date_to)
  
  rets <- prices %>%
    tq_transmute(select = adjusted, 
                 mutate_fun = periodReturn, 
                 period = "daily", 
                 col_rename = "ret")
  
  start_price <- as.numeric(prices$adjusted[nrow(prices)])
  
  kfit <- regkienerLX(rets$ret, model = "K4")
  
  df_price <- simulate_prices(kfit, days_sim, num_sim, start_price)
  
  df_price$Symbol <- symbol
  
  return(df_price)
}

date_from <- "2005-07-01"
date_to <- "2018-10-26"
date_strike <- "2018-12-18"
days_sim <- 32
num_sim <- 100000

df_price <- run_sim("XOM", date_from, date_to, days_sim, num_sim)

c <- (sum(as.numeric(df_price$Price > 81.5)) / num_sim)
c*.5

p <- sum(as.numeric(df_price$Price < 160)) / num_sim
p*.5

# ggplot(data = df_price) +
# geom_histogram(aes(x = `Price`), bins = 100)



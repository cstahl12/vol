library(tidyquant)
library(ggplot2)
library(purrr)
library(dplyr)
library(tidyr)
library(lubridate)
library(rugarch)
library(FatTailsR)

# Sys.setenv(http_proxy="http://stahlc:@172.21.3.201:8080/") 
# Sys.setenv(https_proxy="http://stahlc:@172.21.3.201:8080/")  

date_from <- "2005-07-01"
date_to <- "2018-10-22"
date_strike <- "2018-11-24"

prices <- tq_get("SBUX",
                 get = "stock.prices",
                 from = date_from,
                 to = date_to)

rets <- prices %>%
  tq_transmute(select = adjusted, 
               mutate_fun = periodReturn, 
               period = "daily", 
               col_rename = "ret")

start_price <- as.numeric(prices$adjusted[nrow(prices)])


days_sim <- round(as.numeric(difftime(date_strike, date_to, units = "days")))
num_sim <- 10000

spec <- ugarchspec(mean.model = list(armaOrder=c(0, 0)), distribution = "std")
fit <- ugarchfit(spec, rets$ret)

coef(fit)

plot(sqrt(252) * fit@fit$sigma, type='l')

sim <- ugarchsim(fit, n.sim = days_sim, n.start = 1, m.sim = num_sim, startMethod = "sample")

df_sim <- data.frame(t(sigma(sim)))

get_returns <- function(d){
  rnorm(length(d), mean = 0, sd = d)
}
  
df_rets <- df_sim %>%
  apply(2, get_returns) %>%
  data.frame

df_csum <- df_rets %>%
  apply(1, cumsum) %>%
  data.frame

all_ret <- unlist(df_rets)

gauss <- rnorm(10000, mean = 0, sd = sd(rets$ret))

kfit <- regkienerLX(rets$ret, model = "K4")
kfit$coefk4[1]

kfat <- rkiener4(10000, m = 0, g = kfit$coefk4[2], k = kfit$coefk4[3], e = kfit$coefk4[4])

ggplot() +
  geom_density(aes(x = all_ret), colour = "red") +
  geom_density(aes(x = rets$ret)) +
  geom_density(aes(x = gauss), colour = "green") +
  geom_density(aes(x = kfat), colour = "blue") +
  xlim (-.15, .15)







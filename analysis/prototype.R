library(ggplot2)
#library(ggfortify)
#library(lubridate)
#library(gridExtra)
#library(extRemes)
library(forecast)
library(quantreg)
library(zoo)

theme_set(theme_bw())

priceDAH <- read.csv("~/Documents/Education/current/thesis/dev/data/priceDAH_BE.csv")
priceDAH$datetime <- as.POSIXct(priceDAH$datetime, format = "%Y-%m-%d %H:%M:%S")
#priceDAH <- subset(priceDAH, format(priceDAH$datetime, "%Y") < 2021 )
priceDAH <- subset(priceDAH, format(priceDAH$datetime, "%H") == "17" )
priceDAH$datetime <- as.Date(priceDAH$datetime)

meteoDaily <- read.csv("~/Documents/Education/current/thesis/dev/data/meteo_daily.csv")
meteoDaily$datetime <- as.Date(meteoDaily$time)
meteoDaily$sunhours <- as.numeric(as.difftime(meteoDaily$sunhours, format = "0 days %H:%M:%S"))


data <- merge(priceDAH, meteoDaily, by="datetime")


### DECOMPOSITION
#################

trend <- ksmooth(data$datetime, data$price, 'normal', bandwidth=365)
data$trend <- trend$y
season <- ksmooth(data$datetime, data$price-data$trend, 'normal', bandwidth = 30)
data$seasonal <- season$y
#priceDAH$price_stat <- priceDAH$price - priceDAH$trend - priceDAH$seasonal

ggplot(data) +
  geom_line(aes(datetime, price)) +
  geom_line(aes(datetime, trend), color="red") +
  scale_x_date(date_labels = "%d-%m-%Y") +
  labs(x="Date", y="Price")

ggplot(data) +
  geom_line(aes(datetime, price-trend)) +
  geom_line(aes(datetime, seasonal), color="red") +
  scale_x_date(date_labels = "%d-%m-%Y") +
  labs(x="Date", y="Price (detrended)")

ggplot(data) +
  geom_line(aes(datetime, price-trend-seasonal)) +
  scale_x_date(date_labels = "%d-%m-%Y") +
  labs(x="Date", y="Price (decomposed)")


### QUANTILE REGRESSION
#######################

formula1 <- price ~ trend + seasonal
#formula2 <- price ~ trend + seasonal + generation + load + solar + wind_onshore + wind_offshore
formula3 <- price ~ trend + seasonal + sunhours
#dataBE_zoo <- to_zoo(dataBE, timestamp = "date", value = "price")
#formula4 <- dataBE_zoo ~ L(dataBE_zoo, 1) + L(dataBE_zoo, 1) + dataBE$trend + dataBE$seasonal
# generation and load of neighbouring countries
# all combined

qreg <- rq(formula3,
            data = data,
            tau=1:99/100)

plot_predictions(qreg,data)

plot_predictions <- function(model,data) {
  
  predictions = data.frame(
    date = data$date,
    price <- data$price,
    q01 <- model$fitted.values[,1],
    q05 <- model$fitted.values[,5],
    q50 <- model$fitted.values[,50],
    q95 <- model$fitted.values[,95],
    q99 <- model$fitted.values[,99]
  )
  
  ggplot(predictions) +
    geom_line(aes(date, price)) +
    geom_line(aes(date, q50), color="red") +
    geom_line(aes(date, q05), color="blue") +
    geom_line(aes(date, q95), color="blue") +
    geom_line(aes(date, q01), color="lightblue") +
    geom_line(aes(date, q99), color="lightblue") +
    scale_x_date(date_labels = "%d-%m-%Y") +
    labs(x="Date", y="Price")
  
}


























#q05 <- rq(price~datetime, tau=0.05, data = priceDAH)
#q50 <- rq(price~datetime, tau=0.5, data = priceDAH)
#q95 <- rq(price~datetime, tau=0.95, data = priceDAH)

q50 <- rq(price ~ trend + seasonal, data=priceDAH, tau=0.5)
q95 <- rq(price ~ trend + seasonal, data=priceDAH, tau=0.95)
q05 <- rq(price ~ trend + seasonal, data=priceDAH, tau=0.05)

qreg <- rq(price ~ trend + seasonal, data=priceDAH, tau=1:9/10)
plot(qreg)

priceDAH$q95est <- q95$fitted.values
priceDAH$q50est <- q50$fitted.values
priceDAH$q05est <- q05$fitted.values

ggplot(priceDAH) +
  geom_line(aes(datetime, price)) +
  geom_line(aes(datetime, q95est), color="red") +
  geom_line(aes(datetime, q05est), color="red") +
  geom_line(aes(datetime, q50est), color="blue")
  

## dynamic quantiles

priceTS <- zoo(data.matrix(priceDAH[2:4]), priceDAH$datetime)
# same model as above
test <- dynrq(priceTS$price ~ priceTS$trend + priceTS$seasonal, tau=1:9/10)
test2 <- dynrq(priceTS$price ~ priceTS$trend + priceTS$seasonal + L(priceTS$price, 1) + L(priceTS$price, 2), tau=1:9/10)

plot(test2)


test2M <- dynrq(priceTS$price ~ priceTS$trend + priceTS$seasonal + L(priceTS$price, 1), tau=0.5)
test2Q05 <- dynrq(priceTS$price ~ priceTS$trend + priceTS$seasonal + L(priceTS$price, 1), tau=0.05)
test2Q95 <- dynrq(priceTS$price ~ priceTS$trend + priceTS$seasonal + L(priceTS$price, 1), tau=0.95)
priceDAH$q95est[2:2188] <- test2Q95$fitted.values
priceDAH$q50est[2:2188] <- test2M$fitted.values
priceDAH$q05est[2:2188] <- test2Q05$fitted.values

ggplot(priceDAH) +
  geom_line(aes(datetime, price)) +
  geom_line(aes(datetime, q95est), color="red") +
  geom_line(aes(datetime, q05est), color="red") +
  geom_line(aes(datetime, q50est), color="blue")


priceStat <- zoo(priceDAH$price_stat, priceDAH$datetime)
test4q05 <- dynrq(priceStat ~ L(priceStat, 1), tau=0.05)
test4q50 <- dynrq(priceStat ~ L(priceStat, 1), tau=0.50)
test4q95 <- dynrq(priceStat ~ L(priceStat, 1), tau=0.95)
priceDAH$q95est[2:2188] <- test4q95$fitted.values
priceDAH$q50est[2:2188] <- test4q50$fitted.values
priceDAH$q05est[2:2188] <- test4q05$fitted.values

ggplot(priceDAH) +
  geom_line(aes(datetime, price_stat)) +
  geom_line(aes(datetime, q95est), color="red") +
  geom_line(aes(datetime, q05est), color="red") +
  geom_line(aes(datetime, q50est), color="blue")

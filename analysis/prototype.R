library(ggplot2)
library(forecast)
library(quantreg)
library(zoo)
library(atsd)
library(tidyverse)

theme_set(theme_bw())

import_entsoe <- function(data) {
  
  url <- sprintf("~/Documents/Github/thesis-code/data/%s", data)
  df <- read.csv(url)
  
  df$datetime <- as.POSIXct(df$datetime, format = "%Y-%m-%d %H:%M:%S")
  df <- subset(df, format(df$datetime, "%Y") < 2021 )
  df <- subset(df, format(df$datetime, "%H") == "17" )
  df$datetime <- as.Date(df$datetime)
  
  return(df)
  
}

## DATA BE

priceBE <- import_entsoe("priceDAH_BE.csv")
loadBE <- import_entsoe("loadDAH_BE.csv")
generationBE <- import_entsoe("generationDAH_BE.csv")
renewablesBE <- import_entsoe("renewablesDAH_BE.csv")

list_BE <- list(priceBE, generationBE, loadBE, renewablesBE)
dataBE <- list_BE %>% reduce(full_join, by='datetime')
rm(list_BE, priceBE, loadBE, generationBE, renewablesBE)

## DATA SUNLIGHT

sunlight <- read.csv("~/Documents/Github/thesis-code/data/sunlight.csv")
sunlight$X <- NULL
sunlight$datetime <- as.Date(sunlight$day)
sunlight$day <- NULL
sunlight$sunhours <- as.numeric(as.difftime(sunlight$sunhours, format = "0 days %H:%M:%S"))

data <- merge(dataBE, sunlight, by="datetime")
rm(dataBE, sunlight)

## DATA CWE

# need to fix data
# there are two observations per date
# origin is to be found in DE generation
# french load is empty
#CWE <- import_entsoe("data_CWE.csv")
#CWE <- CWE[c("loadNL", "loadDE", "generationNL", "generationDE", "datetime")]
#data <- merge(data, CWE, by="datetime")
#rm(CWE)

data <- data %>% drop_na(price)


### DECOMPOSITION
#################

data$trend <- ksmooth(data$datetime, data$price, 'normal', bandwidth=365)$y
data$seasonal <- ksmooth(data$datetime, data$price-data$trend, 'normal', bandwidth = 30)$y

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
  labs(x="Date", y="Price (detrended and deseasonalized)")


### QUANTILE REGRESSION
#######################

# basic - trend and seasonal
formula1 <- price ~ trend + seasonal
# entsoe predictions 
formula2a <- price ~ trend + seasonal + generation + load 
formula2b <- price ~ trend + seasonal + solar + wind_onshore + wind_offshore
formula2c <- price ~ trend + seasonal + generation + load + solar + wind_onshore + wind_offshore
# meteorological variables
formula3 <- price ~ trend + seasonal + sunhours
# generation and load of CWE
formula4a <- price ~ trend + seasonal + generationFR + generationDE + generationNL + loadFR + loadDE + loadNL
formula4b <- price ~ trend + seasonal + generationFR + generationDE + generationNL + loadFR + loadDE + loadNL + generation + load
# AR terms
price_zoo <- to_zoo(data, timestamp = "datetime", value = "price")
formula5 <- price_zoo ~ data$trend + data$seasonal + L(price_zoo, 1) + L(price_zoo, 2)
# all 
formula6 <- price_zoo ~ data$trend + data$seasonal + L(price_zoo, 1) + L(price_zoo, 2) +
                        generation + load + solar + wind_onshore + wind_offshore +
                        generationFR + loadFR +
                        generationDE + loadDE +
                        generationNL + loadNL +
                        sunhours

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

qreg <- rq(formula3,
           data = data,
           tau=1:9/10)
summary(qreg)

plot_predictions(qreg,data)
























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

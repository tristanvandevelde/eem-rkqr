library(ggplot2)
library(forecast)
library(quantreg)
library(zoo)
library(atsd)
library(tidyverse)

theme_set(theme_bw())

import_entsoe <- function(data) {
  
  url <- sprintf("~/Documents/Github/eem-rkqr/data/%s", data)
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

sunlight <- read.csv("~/Documents/Github/eem-rkqr/data/sunlight.csv")
sunlight$X <- NULL
sunlight$datetime <- as.Date(sunlight$day)
sunlight$day <- NULL
sunlight$sunhours <- as.numeric(as.difftime(sunlight$sunhours, format = "0 days %H:%M:%S"))

data <- merge(dataBE, sunlight, by="datetime")
rm(dataBE, sunlight)

## DATA CWE

# french load is empty
# sum(is.na(CWE$loadFR))
loadCWE <- import_entsoe("load_CWE.csv")
generationCWE <- import_entsoe("generation_CWE.csv")
CWE <- merge(loadCWE, generationCWE, by="datetime")
data <- merge(data, CWE, by="datetime")
rm(CWE, loadCWE, generationCWE)

data <- data %>% drop_na(price)


### DECOMPOSITION
#################

data$trend <- ksmooth(data$datetime, data$price, 'normal', bandwidth=365)$y
data$seasonal <- ksmooth(data$datetime, data$price-data$trend, 'normal', bandwidth = 30)$y
data$residual <- data$price - data$trend - data$seasonal



p_decomposition_1 <- 
  ggplot(data) +
  geom_line(aes(datetime, price)) +
  geom_line(aes(datetime, trend), color="red") +
  scale_x_date(date_labels = "%d-%m-%Y") +
  labs(x="Date", y="Price (€/MWh)")

p_decomposition_2 <- 
  ggplot(data) +
  geom_line(aes(datetime, price-trend)) +
  geom_line(aes(datetime, seasonal), color="red") +
  scale_x_date(date_labels = "%d-%m-%Y") +
  labs(x="Date", y="Price (€/MWh)")

p_decomposition_3 <- 
  ggplot(data) +
  geom_line(aes(datetime, price-trend-seasonal)) +
  scale_x_date(date_labels = "%d-%m-%Y") +
  labs(x="Date", y="Price (€/MWh)")



p_acf <- ggAcf(data$residual) + ggtitle("")
p_pacf <- ggPacf(data$residual) + ggtitle("")

ggsave(p_decomposition_1, width=16, height=5, units="in", filename="p_decomposition_1.png")
ggsave(p_decomposition_2, width=16, height=5, units="in", filename="p_decomposition_2.png")
ggsave(p_decomposition_3, width=16, height=5, units="in", filename="p_decomposition_3.png")
ggsave(p_acf, width=8, height=5, units="in", filename="p_acf.png")
ggsave(p_pacf, width=8, height=5, units="in", filename="p_pacf.png")


### QUANTILE REGRESSION
#######################


formula <- price_zoo ~ L(price_zoo, 1) + L(price_zoo, 2)
                        + data$trend + data$seasonal
                        + data$generation + data$load
                        + data$solar + data$wind_onshore + data$wind_offshore
                        + data$sunhours
                        + data$generationFR + data$generationNL + data$generationDE
                        + data$loadNL + data$loadDE

plot_predictions <- function(model,data) {
  
  predictions = data.frame(
    date = data$date,
    price <- data$price,
    q01 <- NA,
    q05 <- NA,
    q50 <- NA,
    q95 <- NA,
    q99 <- NA
  )
  
  end <- length(predictions$price)
  start <- end - length(model$fitted.values[,1]) + 1
  #start <- 1
  #end <- length(model$fitted.values[,1])
    
  predictions$q01[start:end] <- model$fitted.values[,1]
  predictions$q05[start:end] <- model$fitted.values[,5]
  predictions$q50[start:end] <- model$fitted.values[,50]
  predictions$q95[start:end] <- model$fitted.values[,95]
  predictions$q99[start:end] <- model$fitted.values[,99]
  
  cols <- c("Q50" = "red",
            "Q05-Q95" = "blue",
            "Q01-Q99" = "lightblue",
            "price" = "black")
  
  p <- 
    ggplot(predictions) +
    geom_line(aes(date, price)) +
    geom_line(aes(date, q50), color="red") +
    geom_line(aes(date, q05), color="blue") +
    geom_line(aes(date, q95), color="blue") +
    geom_line(aes(date, q01), color="lightblue") +
    geom_line(aes(date, q99), color="lightblue") +
    scale_x_date(date_labels = "%d-%m-%Y") +
    labs(x="Date", y="Price (€/MWh)", colour="model") 
  
  return(p)
  
}



qreg_complete <- dynrq(price_zoo ~ L(price_zoo, 1) + L(price_zoo, 2)
              + data$trend + data$seasonal
              + data$generation + data$load
              + data$solar + data$wind_onshore + data$wind_offshore
              + data$sunhours
              + data$generationFR + data$generationNL + data$generationDE
               + data$loadNL + data$loadDE,
              tau=1:9/10)
summary(qreg_complete)

qreg_basic <- rq(price ~
              + trend + seasonal
              + generation + load
              + solar + wind_onshore + wind_offshore
              + sunhours,
              #+ generationFR + generationNL + generationDE
              #+ loadNL + loadDE,
              data=data,
              tau=1:99/100)

p_predictions <- plot_predictions(qreg_basic,data)
ggsave(p_predictions, width=16, height=5, units="in", filename="p_predictions.png")


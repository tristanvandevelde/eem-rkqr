library(ggplot2)
library(forecast)
library(quantreg)
library(tidyverse)
library(dplyr)
library(lubridate)
require(rms)
theme_set(theme_bw())

# import data
hour = 17
data <- read.csv(paste0("~/Documents/Github/eem-rkqr/data/final_", hour, ".csv"))
data$datetime <- as.POSIXct(data$datetime, format = "%Y-%m-%d")

# make lagged variables
# BE
data$priceBE_lag1 <- lag(data$priceBE, n=1, default=NA)
data$priceBE_lag2 <- lag(data$priceBE, n=2, default=NA)
data$priceBE_lag3 <- lag(data$priceBE, n=3, default=NA)
data$priceBE_lag4 <- lag(data$priceBE, n=4, default=NA)
data$priceBE_lag5 <- lag(data$priceBE, n=5, default=NA)
# NL
data$priceNL_lag1 <- lag(data$priceNL, n=1, default=NA)
data$priceNL_lag2 <- lag(data$priceNL, n=2, default=NA)
data$priceNL_lag3 <- lag(data$priceNL, n=3, default=NA)
data$priceNL_lag4 <- lag(data$priceNL, n=4, default=NA)
data$priceNL_lag5 <- lag(data$priceNL, n=5, default=NA)
data$priceNL <- NULL
# FR
data$priceFR_lag1 <- lag(data$priceFR, n=1, default=NA)
data$priceFR_lag2 <- lag(data$priceFR, n=2, default=NA)
data$priceFR_lag3 <- lag(data$priceFR, n=3, default=NA)
data$priceFR_lag4 <- lag(data$priceFR, n=4, default=NA)
data$priceFR_lag5 <- lag(data$priceFR, n=5, default=NA)
data$priceFR <- NULL
# DE
data$priceDE_lag1 <- lag(data$priceDE, n=1, default=NA)
data$priceDE_lag2 <- lag(data$priceDE, n=2, default=NA)
data$priceDE_lag3 <- lag(data$priceDE, n=3, default=NA)
data$priceDE_lag4 <- lag(data$priceDE, n=4, default=NA)
data$priceDE_lag5 <- lag(data$priceDE, n=5, default=NA)
data$priceDE <- NULL

# make month variables
data[paste0("M", 1:12)] <- as.data.frame(t(sapply(month(data$datetime), tabulate, 12)))

# cleanup
data <- subset(data, select = -c(`loadFR`))
data <- na.omit(data)
data <- data %>%
  arrange(datetime) %>%
  filter(duplicated(datetime) == FALSE)

# make trend/seasonal kernel smoothers
data$trend <- ksmooth(as.Date(data$datetime), data$priceBE, 'normal', bandwidth=365)$y
data$seasonal <- ksmooth(as.Date(data$datetime), data$priceBE-data$trend, 'normal', bandwidth = 30)$y

# train/test split
data <- subset(data, format(data$datetime, "%Y") < 2022 )
data_train <- subset(data, format(data$datetime, "%Y") < 2021 )
data_test <- subset(data, format(data$datetime, format = "%Y") == 2021)

data_train$datetime <- as.Date(data_train$datetime)
data_test$datetime <- as.Date(data_test$datetime)



### MODEL 1
###########

formula1 <- priceBE ~ 
  scale(priceBE_lag1) + scale(priceBE_lag2) 

model1 <- rq(formula1, 
             data=data_train,
             tau = 1:99/100) 

predictions_model1 <- predict(model1, 
                              subset(data_test, select = -c(priceBE)),
                              tau = 1:99/100)

write.csv(predictions_model1, "predictions_model1.csv", row.names=FALSE)

### MODEL 2
###########

formula2 <- priceBE ~ 
  scale(priceBE_lag1) + scale(priceBE_lag2) +
  scale(generationBE) + scale(generationNL) + scale(generationFR) + scale(generationDE) +
  scale(loadBE) + scale(loadNL) + scale(loadDE)
  
model2 <- rq(formula2, 
             data=data_train,
             tau = 1:99/100) 

predictions_model2 <- predict(model2, 
                              subset(data_test, select = -c(priceBE)),
                              tau = 1:99/100)

write.csv(predictions_model2, "predictions_model2.csv", row.names=FALSE)

### MODEL 3
###########
  
formula3 <- priceBE ~ 
  scale(priceBE_lag1) + scale(priceBE_lag2) +
  scale(generationBE) + scale(generationNL) + scale(generationFR) + scale(generationDE) +
  scale(loadBE) + scale(loadNL) + scale(loadDE) +
  scale(solarBE) + scale(wind_onshoreBE) + scale(wind_offshoreBE)
  
model3 <- rq(formula3, 
             data=data_train,
             tau = 1:99/100) 

predictions_model3 <- predict(model3, 
                              subset(data_test, select = -c(priceBE)),
                              tau = 1:99/100)

write.csv(predictions_model3, "predictions_model3.csv", row.names=FALSE)

### MODEL 4
###########  

formula4 <- priceBE ~ 
  scale(priceBE_lag1) + scale(priceBE_lag2) +
  scale(priceNL_lag1) +
  scale(priceFR_lag1) +
  scale(priceDE_lag1) +
  scale(generationBE) + scale(generationNL) + scale(generationFR) + scale(generationDE) +
  scale(loadBE) + scale(loadNL) + scale(loadDE) +
  scale(solarBE) + scale(wind_onshoreBE) + scale(wind_offshoreBE)
  
model4 <- rq(formula4, 
             data=data_train,
             tau = 1:99/100)   

predictions_model4 <- predict(model4, 
                              subset(data_test, select = -c(priceBE)),
                              tau = 1:99/100)

write.csv(predictions_model4, "predictions_model4.csv", row.names=FALSE)

### MODEL 5
###########
  
formula5 <- priceBE ~ 
  scale(priceBE_lag1) + scale(priceBE_lag2) + scale(priceBE_lag3) + scale(priceBE_lag4) + scale(priceBE_lag5) +
  scale(priceNL_lag1) + scale(priceNL_lag2) + scale(priceNL_lag3) + scale(priceNL_lag4) + scale(priceNL_lag5) +
  scale(priceFR_lag1) + scale(priceFR_lag2) + scale(priceFR_lag3) + scale(priceFR_lag4) + scale(priceFR_lag5) +
  scale(priceDE_lag1) + scale(priceDE_lag2) + scale(priceDE_lag3) + scale(priceDE_lag4) + scale(priceDE_lag5) +
  scale(generationBE) + scale(generationNL) + scale(generationFR) + scale(generationDE) +
  scale(loadBE) + scale(loadNL) + scale(loadDE) +
  scale(solarBE) + scale(wind_onshoreBE) + scale(wind_offshoreBE)

model5 <- rq(formula5, 
             data=data_train,
             tau = 1:99/100) 

predictions_model5 <- predict(model5, 
                              subset(data_test, select = -c(priceBE)),
                              tau = 1:99/100)

write.csv(predictions_model5, "predictions_model5.csv", row.names=FALSE)

### MODEL 6
###########
  
formula6 <- priceBE ~ 
  scale(priceBE_lag1) + scale(priceBE_lag2) +
  scale(priceNL_lag1) +
  scale(priceFR_lag1) +
  scale(priceDE_lag1) +
  scale(generationBE) + scale(generationNL) + scale(generationFR) + scale(generationDE) +
  scale(loadBE) + scale(loadNL) + scale(loadDE) +
  scale(solarBE) + scale(wind_onshoreBE) + scale(wind_offshoreBE) +
  M1 + M2 + M3 + M4 + M5 + M6 + M7 + M8 + M9 + M10  + M11 + M12 - 1

model6 <- rq(formula6, 
             data=data_train,
             tau = 1:99/100) 


predictions_model6 <- predict(model6, 
                              subset(data_test, select = -c(priceBE)),
                              tau = 1:99/100)

write.csv(predictions_model6, "predictions_model6.csv", row.names=FALSE)

### MODEL 7
###########
  
formula7 <- priceBE ~ 
    scale(priceBE_lag1) + scale(priceBE_lag2) + scale(priceBE_lag3) + scale(priceBE_lag4) + scale(priceBE_lag5) +
    scale(priceNL_lag1) + scale(priceNL_lag2) + scale(priceNL_lag3) + scale(priceNL_lag4) + scale(priceNL_lag5) +
    scale(priceFR_lag1) + scale(priceFR_lag2) + scale(priceFR_lag3) + scale(priceFR_lag4) + scale(priceFR_lag5) +
    scale(priceDE_lag1) + scale(priceDE_lag2) + scale(priceDE_lag3) + scale(priceDE_lag4) + scale(priceDE_lag5) +
    scale(generationBE) + scale(generationNL) + scale(generationFR) + scale(generationDE) +
    scale(loadBE) + scale(loadNL) + scale(loadDE) +
    scale(solarBE) + scale(wind_onshoreBE) + scale(wind_offshoreBE) +
    M1 + M2 + M3 + M4 + M5 + M6 + M7 + M8 + M9 + M10  + M11 + M12 - 1

model7 <- rq(formula7, 
             data=data_train,
             tau = 1:99/100)  

predictions_model7 <- predict(model7, 
                              subset(data_test, select = -c(priceBE)),
                              tau = 1:99/100)

write.csv(predictions_model7, "predictions_model7.csv", row.names=FALSE)


### MODEL 8
###########

formula8 <- priceBE ~ 
  scale(priceBE_lag1) + scale(priceBE_lag2) + scale(priceBE_lag3) + scale(priceBE_lag4) + scale(priceBE_lag5) +
  scale(priceNL_lag1) + scale(priceNL_lag2) + scale(priceNL_lag3) + scale(priceNL_lag4) + scale(priceNL_lag5) +
  scale(priceFR_lag1) + scale(priceFR_lag2) + scale(priceFR_lag3) + scale(priceFR_lag4) + scale(priceFR_lag5) +
  scale(priceDE_lag1) + scale(priceDE_lag2) + scale(priceDE_lag3) + scale(priceDE_lag4) + scale(priceDE_lag5) +
  scale(generationBE) + scale(generationNL) + scale(generationFR) + scale(generationDE) +
  scale(loadBE) + scale(loadNL) + scale(loadDE) +
  scale(solarBE) + scale(wind_onshoreBE) + scale(wind_offshoreBE) +
  seasonal + trend

model8 <- rq(formula8, 
             data=data_train,
             tau = 1:99/100) 

predictions_model8 <- predict(model8, 
                              subset(data_test, select = -c(priceBE)),
                              tau = 1:99/100)

write.csv(predictions_model8, "predictions_model8.csv", row.names=FALSE)


library(ggplot2)
library(forecast)
library(quantreg)
library(zoo)
library(atsd)
library(tidyverse)
library(hqreg)
library(conquer)
library(rqPen)

# import data
hour = 17
data <- read.csv(paste0("~/Documents/Github/eem-rkqr/data/final_", hour, ".csv"))
data$datetime <- as.POSIXct(data$datetime, format = "%Y-%m-%d")

# make lagged variables
data$priceBE_lag1 <- sapply(1:nrow(data), function(x) data$priceBE[x+1])
data$priceBE_lag2 <- sapply(1:nrow(data), function(x) data$priceBE[x+2])
data$priceBE_lag3 <- sapply(1:nrow(data), function(x) data$priceBE[x+3])
data$priceBE_lag4 <- sapply(1:nrow(data), function(x) data$priceBE[x+4])
data$priceBE_lag5 <- sapply(1:nrow(data), function(x) data$priceBE[x+5])

# cleanup
data <- subset(data, select = -c(`loadFR`))
data <- na.omit(data)

# train/test split
data_total <- subset(data, format(data$datetime, "%Y") < 2022 )
data_train <- subset(data_total, format(data$datetime, "%Y") < 2021 )
data_test <- subset(data_total, format(data$datetime, "%Y") == 2021)
data_train$datetime <- as.Date(data_train$datetime)
data_test$datetime <- as.Date(data_test$datetime)

### REGULAR MODEL
#################

data_train$trend <- ksmooth(data_train$datetime, data_train$priceBE, 'normal', bandwidth=365)$y
data_train$seasonal <- ksmooth(data_train$datetime, data_train$priceBE-data_train$trend, 'normal', bandwidth = 30)$y
#data_train$residual <- data$price - data$trend - data$seasonal


### PENALIZED MODELS
####################


rq(priceBE ~ .^2, data=data_train, method="scad")


y_train = as.matrix(subset(data_train, select = priceBE))
X_train = as.matrix(subset(data_train, select = -c(datetime, priceBE)))
y_test = as.matrix(subset(data_test, select = priceBE))
X_test = as.matrix(subset(data_test, select = -c(datetime, priceBE)))
#X = subset(data_train, select = c(loadBE, priceBE_lag1))

## LASSO

# grid based search
# quantile loss is used to optimize the regularization parameters
predictions_lasso = df = data.frame(matrix(nrow = 1786, ncol = 9)) 

for (t in 1:9){
  lasso <- rq.pen.cv(X_train, y_train,
                     tau = t/10,
                     nfolds = 10,
                     penalty = "LASSO")
  predictions_lasso[,c(t)] <- predict(lasso, X_test)
  print(t)
  
}

write.csv(predictions_lasso, "predictions_lasso.csv", row.names=FALSE)

crps_lasso <- crps(predictions_lasso, y_test)
pinball_lasso <- pinball(predictions_lasso, y_test)


## SCAD
predictions_scad = df = data.frame(matrix(nrow = 1786, ncol = 9)) 

for (t in 1:9){
  scad <- rq.pen.cv(poly(X_train, 2), y_train,
                     tau = t/10,
                     nfolds = 10,
                     penalty = "SCAD")
  predictions_scad[,c(t)] <- predict(scad, X_test)
  print(t)
  
}

write.csv(predictions_scad, "predictions_scad.csv", row.names=FALSE)


pb_score <- function(y, q, tau){
  # y = actual value; q = quantile forecasted value; tau = quantile level
  indicator <- ifelse(y - q < 0, 1, 0)
  score <- (y - q) * (tau - indicator)
  return(score)
}

crps <- function(predictions, observations){
  result = 0
  for(i in 1:99){
    q = predictions[,i]
    tau = i/100
    delta = q - observations
    if (delta < 0){
      difference = 0
    }else{
      difference = 1
    }
    result = result + sum((tau - difference)^2)
  }
  return(result)
}

pinball <- function(predictions, observations){
  for(i in 1:99){
    results <- c()
    results <- append(results,
                      mean(pb_score(observations, predictions[,i], i/100)))
  }
  return(results)
}




library(ggplot2)
library(forecast)
library(quantreg)
library(tidyverse)

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
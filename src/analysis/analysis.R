library(ggplot2)
library(forecast)
library(quantreg)
library(zoo)
library(atsd)
library(tidyverse)

data_17 <- read.csv("~/Documents/Github/eem-rkqr/data/final_17.csv")

data_17$datetime <- as.POSIXct(data_17$datetime, format = "%Y-%m-%d")

data_total <- subset(data_17, format(data_17$datetime, "%Y") < 2022 )
data_train <- subset(data_total, format(data_17$datetime, "%Y") < 2021 )
data_test <- subset(data_total, format(data_17$datetime, "%Y") == 2021)

data_train$datetime <- as.Date(data_train$datetime)
data_test$datetime <- as.Date(data_test$datetime)

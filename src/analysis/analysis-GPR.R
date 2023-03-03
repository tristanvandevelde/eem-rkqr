library(quantreg.nonpar)
library(fda)

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
data_test <- subset(data, format(data$datetime, format = "%Y") == 2021 
                    & format(data$datetime, format = "%m") < 7)
# fix months subset
data_test <- data_test[1:174,]

data_train$datetime <- as.Date(data_train$datetime)
data_test$datetime <- as.Date(data_test$datetime)

basis.bsp <- create.bspline.basis(breaks=quantile(data_train$loadBE,c(0:10)/10))

npqr(priceBE ~ generationBE,
     data=data_train,
     taus = c(0.05, 0.5, 0.95),
     basis = basis.bsp,
      var = "loadBE")

npqr(priceBE ~ 1,
     data=data_train,
     taus = c(0.05, 0.5, 0.95))

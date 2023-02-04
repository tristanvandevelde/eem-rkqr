library(ggplot2)
library(lubridate)

theme_set(theme_bw())

#################
## DATA IMPORT ##
#################

quantityDAH <- read.csv("~/Documents/Education/current/thesis/dev/data/quantityDAH.csv")
quantityDAH$datetime <- as.POSIXct(quantityDAH$datetime, format = "%Y-%m-%d %H:%M:%S")
quantityDAH$difference <- quantityDAH$generation - quantityDAH$load

sunlight <- read.csv("~/Documents/Education/current/thesis/dev/data/sunlight.csv")
sunlight$day <- as.POSIXct(sunlight$day, format = "%Y-%m-%d")
sunlight$sunhours <- as.difftime(sunlight$sunhours, format = "0 days %H:%M:%S")

priceDAH <- read.csv("~/Documents/Education/current/thesis/dev/data/priceDAH.csv")
priceDAH$datetime <- as.POSIXct(priceDAH$datetime, format = "%Y-%m-%d %H:%M:%S")

data <- merge(quantityDAH, priceDAH, by="datetime")

data[is.na(data),]
# last correct observation is 2022 - 11 - 15 22:00:00
# check why the last ones are NA

data <- data[complete.cases(data),]

##############################
## EXPLORATION 1 - OVERVIEW ##
##############################

p1<- ggplot(data) +
  geom_line(aes(datetime, price)) +
  #geom_vline(xintercept=as.POSIXct(strptime("2021-01-01", "%Y-%m-%d")), color="red")
  labs(x="Date", y="Price")


#ggplot(data) +
#  geom_line(aes(datetime, load))


#####################################
## EXPLORATION 2 - LOAD/GENERATION ##
#####################################

# most probably, this will not be included


ggplot(data, aes(datetime, difference)) +
  geom_line() +
  geom_smooth() +
  labs(x = "Date", y="Quantity (MW)")

# ggplot(data) +
#   geom_line(aes(datetime, load), color="red") +
#   geom_line(aes(datetime, generation), color="green") +
#   labs(x = "Date", y="Quantity (MW)")

yearlyDifference <- do.call(data.frame,
                            aggregate(difference~format(yday(datetime)), 
                                      data=data, 
                                      FUN=function(x) c(avg=mean(x), med=median(x), q05=quantile(x, probs=0.05), q05=quantile(x, probs=0.95))))
colnames(yearlyDifference) <- c("day", "avg", "med", "q05", "q95")

ggplot(data=yearlyDifference, aes(as.Date(strtoi(day), origin="2000-01-01"))) +
  geom_line(aes(y=avg, group=1), color="red") +
  geom_line(aes(y=med, group=1), color="blue") +
  geom_line(aes(y=q05, group=1), color="black") +
  geom_line(aes(y=q95, group=1), color="black") +
  scale_x_date(date_labels = "%b") +
  labs(x="Date", y="Quantity (MW)")


###########################
## EXPLORATION 3 - DAILY ##
###########################

dailyDAHprice <- do.call(data.frame, 
                    aggregate(price~format(datetime,"%H"), 
                              data=data, 
                              FUN=function(x) c(avg=mean(x), med=median(x), q05=quantile(x, probs=0.05), q05=quantile(x, probs=0.95))))
colnames(dailyDAHprice) <- c("hour", "avg", "med", "q05", "q95")

ggplot(data=dailyDAHprice, aes(hour)) +
  geom_line(aes(y=avg, group=1), color="red") +
  geom_line(aes(y=med, group=1), color="blue") +
  labs(x="Hour", y="Price")


dailyDAHload <- do.call(data.frame, 
                    aggregate(load~format(datetime,"%H"), 
                              data=data, 
                              FUN=function(x) c(avg=mean(x), med=median(x), q05=quantile(x, probs=0.05), q05=quantile(x, probs=0.95))))
colnames(dailyDAHload) <- c("hour", "avg", "med", "q05", "q95")

ggplot(data=dailyDAHload, aes(hour)) +
  geom_line(aes(y=avg, group=1), color="red") +
  geom_line(aes(y=med, group=1), color="blue") +
  labs(x="Hour", y="Quantity (MW)")


############################
## EXPLORATION 4 - PRICE ###
############################

data17 <- subset(data, format(datetime, "%H") == "17")
data02 <- subset(data, format(datetime, "%H") == "02")

ggplot(data17, aes(datetime, price)) +
  geom_line() +
  geom_vline(xintercept=as.POSIXct(strptime("2021-01-01", "%Y-%m-%d")), color="red")

ggplot(data02, aes(datetime, price)) +
  geom_line() +
  geom_vline(xintercept=as.POSIXct(strptime("2021-01-01", "%Y-%m-%d")), color="red")



#################################
## EXPLORATION 5 - PRICE 17 A ###
#################################

data17left <- subset(data17, format(datetime, "%Y") < 2021)


data17left$returns[2:2188] <- diff(data17left$price) / lag(data17left$price[-length(data17left$price)])

#data17left$return <- 
#diff(data17left$price) / lag(data17left$price)
#plot(data17left$datetime, data17left$price)

p2 <- ggplot(data17left) +
  geom_line(aes(datetime, price)) +
  labs(x="Date", y="Price")


ggplot(data17left) +
  geom_line(aes(datetime, returns))

#################################
## EXPLORATION 6 - PRICE 17 B ###
#################################


yearly17 <- do.call(data.frame,
                    aggregate(price~format(yday(datetime)), 
                                    data=data17left, 
                                    FUN=function(x) c(avg=mean(x), med=median(x), q05=quantile(x, probs=0.05), q05=quantile(x, probs=0.95))))
colnames(yearly17) <- c("day", "avg", "med", "q05", "q95")

p3 <- ggplot(data=yearly17, aes(as.Date(strtoi(day), origin="2000-01-01"))) +
  #geom_line(aes(y=avg, group=1), color="red") +
  geom_line(aes(y=med, group=1), color="blue") +
  #geom_line(aes(y=q05, group=1), color="black") +
  #geom_line(aes(y=q95, group=1), color="black") +
  scale_x_date(date_labels = "%b") +
  xlab("Date") +
  ylab("Price")


p4 <- ggplot(data=yearly17, aes(as.Date(strtoi(day), origin="2000-01-01"))) +
  #geom_line(aes(y=avg, group=1), color="red") +
  geom_line(aes(y=med, group=1), color="blue") +
  geom_line(aes(y=q05, group=1), color="black") +
  geom_line(aes(y=q95, group=1), color="black") +
  #geom_vline(xintercept = as.Date("2000-11-12", origin="2000-01-01"), color="red") +
  scale_x_date(date_labels = "%b") +
  xlab("Date") +
  ylab("Price")

yearly17 <- do.call(data.frame,
                    aggregate(returns~format(yday(datetime)), 
                              data=data17left, 
                              FUN=function(x) c(avg=mean(x), med=median(x), q05=quantile(x, probs=0.05), q05=quantile(x, probs=0.95))))
colnames(yearly17) <- c("day", "avg", "med", "q05", "q95")

ggplot(data=yearly17, aes(as.Date(strtoi(day), origin="2000-01-01"))) +
  #geom_line(aes(y=avg, group=1), color="red") +
  geom_line(aes(y=med, group=1), color="blue") +
  #geom_line(aes(y=q05, group=1), color="black") +
  #geom_line(aes(y=q95, group=1), color="black") +
  scale_x_date(date_labels = "%b") +
  xlab("Date") +
  ylab("Returns")

#################################
## EXPLORATION 7 - SUNLIGHT #####
#################################

yearlySun <- aggregate(sunhours~format(yday(day)), 
                              data=sunlight, 
                              FUN=mean)
colnames(yearlySun) <- c("day", "avg")

ggplot(data=yearlySun, aes(as.Date(strtoi(day), origin="2000-01-01"))) +
  geom_line(aes(y=avg, group=1)) +
  geom_vline(xintercept = as.Date("2000-12-20", origin="2000-01-01"), color="red") +
  #geom_vline(xintercept = as.Date("2000-11-12", origin="2000-01-01"), color="red") +
  scale_x_date(date_labels = "%b") +
  xlab("Date") +
  ylab("Sunlight")

as.Date("2000-12-20", origin="2000-01-01") - as.Date("2000-11-12", origin="2000-01-01")
# 38 days







ggsave(plot=p1, width=16, height=5, units="in", filename="ts_complete.png")
ggsave(plot=p2, width=16, height=5, units="in", filename="ts_peak.png")
ggsave(plot=p3, width=8, height=5, units="in", filename="yearly_01.png")
ggsave(plot=p4, width=8, height=5, units="in", filename="yearly_02.png")

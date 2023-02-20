library(ggplot2)
theme_set(theme_bw())
library(lubridate)

#################
## DATA IMPORT ##
#################

import_entsoe <- function(data) {
  
  url <- sprintf("~/Documents/Github/eem-rkqr/data/%s", data)
  df <- read.csv(url)
  
  df$datetime <- as.POSIXct(df$datetime, format = "%Y-%m-%d %H:%M:%S")
  
  return(df)
  
}

priceBE <- import_entsoe("priceDAH_BE.csv")
priceNL <- import_entsoe("priceDAH_NL.csv")
priceFR <- import_entsoe("priceDAH_FR.csv")
priceDE <- import_entsoe("priceDAH_DE.csv")

priceBE17 <- subset(priceBE, format(datetime, "%H") == "17")
priceBE02 <- subset(priceBE, format(datetime, "%H") == "02")

priceBE17left <- subset(priceBE17, format(datetime, "%Y") < 2021)
priceBE02left <- subset(priceBE02, format(datetime, "%Y") < 2021)


################################
## EXPLORATION 1 - CWE PRICES ##
################################

p_ts_complete_BE <- ggplot(priceBE) +
  geom_line(aes(as.Date(datetime), price)) +
  ylim(-500, 3000) +
  scale_x_date(date_labels = "%d-%m-%Y") +
  labs(x="Date", y="Price (€/MWh)")

p_ts_complete_NL <- ggplot(priceNL) +
  geom_line(aes(as.Date(datetime), price)) +
  ylim(-500, 3000) +
  scale_x_date(date_labels = "%d-%m-%Y") +
  labs(x="Date", y="Price (€/MWh)")

p_ts_complete_FR <- ggplot(priceFR) +
  geom_line(aes(as.Date(datetime), price)) +
  ylim(-500, 3000) +
  scale_x_date(date_labels = "%d-%m-%Y") +
  labs(x="Date", y="Price (€/MWh)")

p_ts_complete_DE <- ggplot(priceDE) +
  geom_line(aes(as.Date(datetime), priceDE)) +
  ylim(-500, 3000) +
  scale_x_date(date_labels = "%d-%m-%Y") +
  labs(x="Date", y="Price (€/MWh)")
# fix price variable

ggsave(plot=p_ts_complete_BE, width=20, height=5, units="in", filename="p_ts_complete_BE.png")
ggsave(plot=p_ts_complete_NL, width=20, height=5, units="in", filename="p_ts_complete_NL.png")
ggsave(plot=p_ts_complete_FR, width=20, height=5, units="in", filename="p_ts_complete_FR.png")
ggsave(plot=p_ts_complete_DE, width=20, height=5, units="in", filename="p_ts_complete_DE.png")


##############################################
## EXPLORATION 2 - BELGIAN PEAK/LOW PRICES ###
##############################################

p_ts_left_BE_17 <- ggplot(priceBE17left) +
  geom_line(aes(as.Date(datetime), price)) +
  ylim(-200, 700) +
  scale_x_date(date_labels = "%d-%m-%Y") +
  labs(x="Date", y="Price (€/MWh)")

p_ts_left_BE_02 <- ggplot(priceBE02left) +
  geom_line(aes(as.Date(datetime), price)) +
  ylim(-200, 700) +
  scale_x_date(date_labels = "%d-%m-%Y") +
  labs(x="Date", y="Price (€/MWh)")

ggsave(plot=p_ts_left_BE_17, width=20, height=5, units="in", filename="p_ts_left_BE_17.png")
ggsave(plot=p_ts_left_BE_02, width=20, height=5, units="in", filename="p_ts_left_BE_02.png")

###############################
## EXPLORATION 3 - PROFILES ###
###############################

yearly17 <- do.call(data.frame,
                    aggregate(price~format(yday(datetime)), 
                              data=priceBE17left, 
                              FUN=function(x) c(med=median(x), min=min(x), max=max(x))))
colnames(yearly17) <- c("day", "med", "min", "max")

p_yearly_BE_middle <- ggplot(data=yearly17, aes(as.Date(strtoi(day), origin="2000-01-01"))) +
  geom_line(aes(y=med, group=1, color="median")) +
  scale_color_manual(values=c("blue")) +
  scale_x_date(date_labels = "%b") +
  labs(x="Day of the year", y="Price (€/MWh)") +
  theme(legend.position = c(.085,.9),
        legend.title = element_blank(),
        #panel.border = element_rect(colour = "black", fill=NA),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")
  )

p_yearly_BE_extrema <- ggplot(data=yearly17, aes(as.Date(strtoi(day), origin="2000-01-01"))) +
  geom_line(aes(y=min, group=1, color="minimum")) +
  geom_line(aes(y=med, group=1, color="median")) +
  geom_line(aes(y=max, group=1, color="maximum")) +
  scale_color_manual(values=c("red",  "blue", "red")) +
  scale_x_date(date_labels = "%b") +
  labs(x="Day of the year", y="Price (€/MWh)") +
  theme(legend.position = c(.085,.85),
        legend.title = element_blank(),
        #panel.border = element_rect(colour = "black", fill=NA),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")
        )

ggsave(plot=p_yearly_BE_middle, width=8, height=5, units="in", filename="p_yearly_BE_middle.png")
ggsave(plot=p_yearly_BE_extrema, width=8, height=5, units="in", filename="p_yearly_BE_extrema.png")


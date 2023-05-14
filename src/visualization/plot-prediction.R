library(qgam)
library(ggplot2)
theme_set(theme_bw())
library(dplyr)

# import data
hour = "03"
data <- read.csv(paste0("~/Documents/Github/eem-rkqr/data/final_", hour, ".csv"))
data$datetime <- as.POSIXct(data$datetime, format = "%Y-%m-%d")

# cleanup
data <- subset(data, select = -c(`loadFR`))
data <- na.omit(data)
data <- data %>%
  arrange(datetime) %>%
  filter(duplicated(datetime) == FALSE)

# train/test split
data <- subset(data, format(data$datetime, "%Y") < 2022 )
data_train <- subset(data, format(data$datetime, "%Y") < 2021 )
data_test <- subset(data, format(data$datetime, format = "%Y") == 2021) 

# import predictions
predictions_GB1 <- read.csv("~/Documents/GitHub/eem-rkqr/results/predictions_03_GB1.csv")
predictions_GB1$datetime <- as.Date(data_test$datetime)
predictions_GB1$priceBE <- data_test$priceBE
predictions_model1 <- read.csv("~/Documents/GitHub/eem-rkqr/results/predictions_03_model1b.csv")
predictions_model1$datetime <- as.Date(data_test$datetime)
predictions_model1$priceBE <- data_test$priceBE
predictions_model5 <- read.csv("~/Documents/GitHub/eem-rkqr/results/predictions_03_model5.csv")
predictions_model5$datetime <- as.Date(data_test$datetime)
predictions_model5$priceBE <- data_test$priceBE


## plot GB 1


p_predictions_GB1 <- 
ggplot(predictions_GB1) +
  geom_line(aes(datetime, priceBE, color = "Price")) +
  geom_line(aes(datetime, X0.50, color = "Q50")) +
  geom_line(aes(datetime, X0.05, color = "Q05 & Q95")) +
  geom_line(aes(datetime, X0.95, color = "Q05 & Q95")) +
  scale_color_manual(values=c("black", "#619CFF", "red")) +
  scale_x_date(date_labels = "%d-%m-%Y") +
  labs(x="Date", y="Price (€/MWh)", colour="") 

## plot model 1
p_predictions_model5 <-
ggplot(predictions_model5) +
  geom_line(aes(datetime, priceBE, color = "Price")) +
  geom_line(aes(datetime, tau..0.50, color = "Q50")) +
  geom_line(aes(datetime, tau..0.05, color = "Q05 & Q95")) +
  geom_line(aes(datetime, tau..0.95, color = "Q05 & Q95")) +
  scale_color_manual(values=c("black", "#619CFF", "red")) +
  scale_x_date(date_labels = "%d-%m-%Y") +
  labs(x="Date", y="Price (€/MWh)", colour="") 

ggsave(p_predictions_GB1, width=16, height=5, units="in", filename="p_predictions_03_GB1.png")
ggsave(p_predictions_model5, width=16, height=5, units="in", filename="p_predictions_03_model5.png")

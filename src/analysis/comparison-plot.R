library(qgam)
library(ggplot2)
theme_set(theme_bw())
library(dplyr)

# import data
hour = 17
data <- read.csv(paste0("~/Documents/Github/eem-rkqr/data/final_", hour, ".csv"))
data$datetime <- as.POSIXct(data$datetime, format = "%Y-%m-%d")

pinball2 <- read.csv("~/Documents/Github/eem-rkqr/results/pinball2.csv")


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
#data_test <- subset(data_test, format(data_test$datetime, format = "%m") < 7) 

# fix months subset
#data_test <- data_test[1:174,]

# import predictions
predictions_GB1 <- read.csv("~/Documents/GitHub/eem-rkqr/results/predictions_GB1.csv")
predictions_GB2 <- read.csv("~/Documents/GitHub/eem-rkqr/results/predictions_GB2.csv")
predictions_model1 <- read.csv("~/Documents/GitHub/eem-rkqr/results/predictions_model1.csv")
predictions_model2 <- read.csv("~/Documents/GitHub/eem-rkqr/results/predictions_model2.csv")
predictions_model3 <- read.csv("~/Documents/GitHub/eem-rkqr/results/predictions_model3.csv")

# add datetime and actual price 
predictions_GB1$datetime <- as.Date(data_test$datetime)
predictions_GB1$priceBE <- data_test$priceBE
predictions_GB2$datetime <- as.Date(data_test$datetime)
predictions_GB2$priceBE <- data_test$priceBE
predictions_model1$datetime <- as.Date(data_test$datetime)
predictions_model1$priceBE <- data_test$priceBE
predictions_model2$datetime <- data_test$datetime
predictions_model2$priceBE <- data_test$priceBE
predictions_model3$datetime <- as.Date(data_test$datetime)
predictions_model3$priceBE <- data_test$priceBE



insert_pinball <- function(predictions_df) {
  pinball_scores = list()
  for (t in 1:99){
    pinball_scores <- append(pinball_scores, pinLoss(data_test$priceBE[1:length(predictions_df[,1])], predictions_df[,t], qu=t/100, add=TRUE))
  }  
  return(as.numeric(pinball_scores))
}

# pinball 1 = first two quarters
pinball_1 <- data.frame(tau = 1:99)
pinball_1$tau <- pinball_1$tau/100
pinball_1$GB1 <- insert_pinball(predictions_GB1[1:175,])
pinball_1$GB2 <- insert_pinball(predictions_GB2[1:175,])
pinball_1$Model1 <- insert_pinball(predictions_model1[1:175,])
pinball_1$Model2 <- insert_pinball(predictions_model2[1:175,])
pinball_1$Model3 <- insert_pinball(predictions_model3[1:175,])

# pinball 2 = second two quarters
pinball_2 <- data.frame(tau = 1:99)
pinball_2$tau <- pinball_2$tau/100
pinball_2$GB1 <- insert_pinball(predictions_GB1[176:358,])
pinball_2$GB2 <- insert_pinball(predictions_GB2[176:358,])
pinball_2$Model1 <- insert_pinball(predictions_model1[176:358,])
pinball_2$Model2 <- insert_pinball(predictions_model2[176:358,])
pinball_2$Model3 <- insert_pinball(predictions_model3[176:358,])


p_pinball_1 <- ggplot(pinball_1) +
  geom_line(aes(tau, GB1, color="Gradient boosting (order 1)")) +
  geom_line(aes(tau, GB2, color="Gradient boosting (order 2)")) +
  geom_line(aes(tau, Model1, color="Linear quantile regression: model 1")) +
  geom_line(aes(tau, Model2, color="Linear quantile regression: model 2")) +
  geom_line(aes(tau, Model3, color="Linear quantile regression: model 3")) +
  scale_color_manual(values=c("red",  "pink", "lightblue", "blue", "black")) +
  labs(x="Quantile", y="Pinball loss", colour="") +
  theme(legend.position = "right")

#p_pinball_2 <- 
ggplot(pinball_2) +
  geom_line(aes(tau, GB1, color="Gradient boosting (order 1)")) +
  geom_line(aes(tau, GB2, color="Gradient boosting (order 2)")) +
  geom_line(aes(tau, Model1, color="Linear quantile regression: model 1")) +
  geom_line(aes(tau, Model2, color="Linear quantile regression: model 2")) +
  geom_line(aes(tau, Model3, color="Linear quantile regression: model 3")) +
  scale_color_manual(values=c("red", "pink", "lightblue", "blue", "black")) +
  labs(x="Quantile", y="Pinball loss", colour="") +
  theme(legend.position = "right")
  #theme(legend.position="bottom", legend.box = "vertical")

p_pinball_2 <- ggplot(pinball2) +
  geom_line(aes(tau, GB1, color="Gradient boosting (order 1)")) +
  geom_line(aes(tau, GB2, color="Gradient boosting (order 2)")) +
  geom_line(aes(tau, model1, color="Linear quantile regression: model 1")) +
  geom_line(aes(tau, model2, color="Linear quantile regression: model 2")) +
  geom_line(aes(tau, model3, color="Linear quantile regression: model 3")) +
  scale_color_manual(values=c("red", "pink", "lightblue", "blue", "black")) +
  labs(x="Quantile", y="Pinball loss", colour="") +
  theme(legend.position = "right")
#theme(legend.position="bottom", legend.box = "vertical")

ggsave(p_pinball_1, width=10, height=5, units="in", filename="p_pinball_1.png")
ggsave(p_pinball_2, width=10, height=5, units="in", filename="p_pinball_2.png")


## plot GB 1
#p_predictions_GB1 <- 
ggplot(predictions_GB1) +
  geom_line(aes(datetime, priceBE, color = "Price")) +
  geom_line(aes(datetime, X0.50, color = "Q50")) +
  geom_line(aes(datetime, X0.05, color = "Q05 & Q95")) +
  geom_line(aes(datetime, X0.95, color = "Q05 & Q95")) +
  scale_color_manual(values=c("black", "#619CFF", "red")) +
  scale_x_date(date_labels = "%d-%m-%Y") +
  labs(x="Date", y="Price (€/MWh)", colour="") 

## plot model 1
#p_predictions_model1 <-
ggplot(predictions_model1) +
  geom_line(aes(datetime, priceBE, color = "Price")) +
  geom_line(aes(datetime, tau..0.50, color = "Q50")) +
  geom_line(aes(datetime, tau..0.05, color = "Q05 & Q95")) +
  geom_line(aes(datetime, tau..0.95, color = "Q05 & Q95")) +
  scale_color_manual(values=c("black", "#619CFF", "red")) +
  scale_x_date(date_labels = "%d-%m-%Y") +
  labs(x="Date", y="Price (€/MWh)", colour="") 

ggsave(p_predictions_GB1, width=16, height=5, units="in", filename="p_predictions_GB1.png")
ggsave(p_predictions_model1, width=16, height=5, units="in", filename="p_predictions_model1.png")

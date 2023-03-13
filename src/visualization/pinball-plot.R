library(ggplot2)
theme_set(theme_bw())

pinball1 <- read.csv("~/Documents/Github/eem-rkqr/results/03_pinball1.csv")
pinball2 <- read.csv("~/Documents/Github/eem-rkqr/results/03_pinball2.csv")

ggplot(pinball1) +
  geom_line(aes(tau, GB1, color="Gradient boosting (order 1)")) +
  geom_line(aes(tau, GB2, color="Gradient boosting (order 2)")) +
  geom_line(aes(tau, model1, color="Linear quantile regression: model 1")) +
  geom_line(aes(tau, model2, color="Linear quantile regression: model 2")) +
  geom_line(aes(tau, model3, color="Linear quantile regression: model 3")) +
  geom_line(aes(tau, model4, color="Linear quantile regression: model 4")) +
  geom_line(aes(tau, model5, color="Linear quantile regression: model 5")) +
  geom_line(aes(tau, model6, color="Linear quantile regression: model 6")) +
  geom_line(aes(tau, model7, color="Linear quantile regression: model 7")) +
  geom_line(aes(tau, model8, color="Linear quantile regression: model 8")) +
  #scale_color_manual(values=c("red",  "pink", "lightblue", "blue", "black", "cyan", "#FF00FF")) +
  labs(x="Quantile", y="Pinball loss", colour="") +
  theme(legend.position = "right")

ggplot(pinball2) +
  geom_line(aes(tau, GB1, color="Gradient boosting (order 1)")) +
  geom_line(aes(tau, GB2, color="Gradient boosting (order 2)")) +
  geom_line(aes(tau, model1, color="Linear quantile regression: model 1")) +
  geom_line(aes(tau, model2, color="Linear quantile regression: model 2")) +
  geom_line(aes(tau, model3, color="Linear quantile regression: model 3")) +
  geom_line(aes(tau, model4, color="Linear quantile regression: model 4")) +
  geom_line(aes(tau, model5, color="Linear quantile regression: model 5")) +
  geom_line(aes(tau, model6, color="Linear quantile regression: model 6")) +
  geom_line(aes(tau, model7, color="Linear quantile regression: model 7")) +
  geom_line(aes(tau, model8, color="Linear quantile regression: model 8")) +
  #scale_color_manual(values=c("red",  "pink", "lightblue", "blue", "black", "cyan", "#FF00FF")) +
  labs(x="Quantile", y="Pinball loss", colour="") +
  theme(legend.position = "right")
  #theme(legend.position = "bottom") +
  #guides(color=guide_legend(nrow=2, byrow=TRUE)) 

ggsave(p_pinball1, width=8, height=5, units="in", filename="p_pinball1_03.png")
ggsave(p_pinball2, width=8, height=5, units="in", filename="p_pinball2_03.png")

colMeans(pinball1)
colMeans(pinball2)

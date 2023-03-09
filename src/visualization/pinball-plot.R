library(ggplot2)
theme_set(theme_bw())

pinball1 <- read.csv("~/Documents/Github/eem-rkqr/results/pinball1.csv")
pinball2 <- read.csv("~/Documents/Github/eem-rkqr/results/pinball2.csv")

ggplot(pinball1) +
  geom_line(aes(tau, GB1, color="Gradient boosting (order 1)")) +
  geom_line(aes(tau, GB2, color="Gradient boosting (order 2)")) +
  geom_line(aes(tau, model1, color="Linear quantile regression: model 1")) +
  geom_line(aes(tau, model2, color="Linear quantile regression: model 2")) +
  geom_line(aes(tau, model3, color="Linear quantile regression: model 3")) +
  scale_color_manual(values=c("red",  "pink", "lightblue", "blue", "black")) +
  labs(x="Quantile", y="Pinball loss", colour="") +
  theme(legend.position = "right")

ggplot(pinball2) +
  geom_line(aes(tau, GB1, color="Gradient boosting (order 1)")) +
  geom_line(aes(tau, GB2, color="Gradient boosting (order 2)")) +
  geom_line(aes(tau, model1, color="Linear quantile regression: model 1")) +
  geom_line(aes(tau, model2, color="Linear quantile regression: model 2")) +
  geom_line(aes(tau, model3, color="Linear quantile regression: model 3")) +
  scale_color_manual(values=c("red",  "pink", "lightblue", "blue", "black")) +
  labs(x="Quantile", y="Pinball loss", colour="") +
  theme(legend.position = "right")
  #theme(legend.position = "bottom") +
  #guides(color=guide_legend(nrow=2, byrow=TRUE)) 

ggsave(p_pinball1, width=8, height=5, units="in", filename="p_pinball1.png")
ggsave(p_pinball2, width=8, height=5, units="in", filename="p_pinball2.png")
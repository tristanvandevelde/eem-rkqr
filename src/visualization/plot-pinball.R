library(ggplot2)
theme_set(theme_bw())

pinball1 <- read.csv("~/Documents/Github/eem-rkqr/results/03_pinball1.csv")
pinball2 <- read.csv("~/Documents/Github/eem-rkqr/results/03_pinball2.csv")

p_pinball1 <- ggplot(pinball1) +
  geom_line(aes(tau, model1, color="(LQR) Model 1")) +
  geom_line(aes(tau, model2, color="(LQR) Model 2")) +
  geom_line(aes(tau, model3, color="(LQR) Model 3")) +
  geom_line(aes(tau, model4, color="(LQR) Model 4")) +
  geom_line(aes(tau, model5, color="(LQR) Model 5")) +
  geom_line(aes(tau, model6, color="(LQR) Model 6")) +
  geom_line(aes(tau, model7, color="(LQR) Model 7")) +
  geom_line(aes(tau, model8, color="(LQR) Model 8")) +
  geom_line(aes(tau, GB1, color="(QGB) Model 9 ")) +
  geom_line(aes(tau, GB2, color="(QGB) Model 10 ")) +
  labs(x="Quantile", y="Pinball loss", colour="") +
  scale_fill_discrete(breaks=c('(LQR) Model 1', 
                               '(LQR) Model 2',
                               '(LQR) Model 3',
                               '(LQR) Model 4',
                               '(LQR) Model 5',
                               '(LQR) Model 6',
                               '(LQR) Model 7',
                               '(LQR) Model 8',
                               '(QGB) Model 9',
                               '(QGB) Model 10'))
  #theme(legend.position = "")

p_pinball2 <- ggplot(pinball2) +
  geom_line(aes(tau, model1, color="(LQR) Model 1")) +
  geom_line(aes(tau, model2, color="(LQR) Model 2")) +
  geom_line(aes(tau, model3, color="(LQR) Model 3")) +
  geom_line(aes(tau, model4, color="(LQR) Model 4")) +
  geom_line(aes(tau, model5, color="(LQR) Model 5")) +
  geom_line(aes(tau, model6, color="(LQR) Model 6")) +
  geom_line(aes(tau, model7, color="(LQR) Model 7")) +
  geom_line(aes(tau, model8, color="(LQR) Model 8")) +
  geom_line(aes(tau, GB1, color="(QGB) Model 9")) +
  geom_line(aes(tau, GB2, color="(QGB) Model 10")) +
  labs(x="Quantile", y="Pinball loss", colour="") 

ggsave(p_pinball1, width=8, height=5, units="in", filename="p_pinball1_03.png")
ggsave(p_pinball2, width=8, height=5, units="in", filename="p_pinball2_03.png")

colMeans(pinball1)
colMeans(pinball2)

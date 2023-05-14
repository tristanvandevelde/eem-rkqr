library(ggplot2)
library(forecast)
library(quantreg)
library(tidyverse)
library(dplyr)
library(lubridate)
require(rms)
library(broom)
theme_set(theme_bw())

# import data
hour = "03"
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

# train/test split
data_train <- subset(data, format(data$datetime, "%Y") < 2021 )

data_train$datetime <- as.Date(data_train$datetime)


formula5 <- priceBE ~ 
  scale(priceBE_lag1) + scale(priceBE_lag2) + scale(priceBE_lag3) + scale(priceBE_lag4) + scale(priceBE_lag5) +
  scale(priceNL_lag1) + scale(priceNL_lag2) + scale(priceNL_lag3) + scale(priceNL_lag4) + scale(priceNL_lag5) +
  scale(priceFR_lag1) + scale(priceFR_lag2) + scale(priceFR_lag3) + scale(priceFR_lag4) + scale(priceFR_lag5) +
  scale(priceDE_lag1) + scale(priceDE_lag2) + scale(priceDE_lag3) + scale(priceDE_lag4) + scale(priceDE_lag5) +
  scale(generationBE) + scale(generationNL) + scale(generationFR) + scale(generationDE) +
  scale(loadBE) + scale(loadNL) + scale(loadDE) +
  scale(solarBE) + scale(wind_onshoreBE) + scale(wind_offshoreBE)

model_viz <- rq(formula5, 
                 data=data_train,
                 tau = 1:99/100)
#lm_viz <- lm(formula5,
#             data=data_train)
#ols <- as.data.frame(coef(lm_viz))
#ols.ci <- as.data.frame(confint(lm_viz))
#ols <- cbind(ols, ols.ci)
#ols <- tibble::rownames_to_column(ols, var="term")

data_viz <- model_viz %>%
  broom::tidy(se.type = "boot", conf.int = TRUE) 

#data_viz <- merge(data_viz, ols,
#                  by = "term",
#                  all = TRUE)

plot_coef <- function(variable, title) {
  p <- ggplot(subset(data_viz, term == variable)) +
    geom_point(aes(x=tau,y=estimate), color="#27408b", size = 3) +
    geom_line(aes(x=tau,y=estimate), color="#27408b", linewidth = 1) +
    geom_ribbon(aes(x=tau, ymin=conf.low,ymax=conf.high),alpha=0.25, fill="#27408b") +
    #geom_hline(aes(yintercept = `coef(lm_viz)`), lty=1, color="red", linewidth = 1) +
    #geom_hline(aes(yintercept= `2.5 %`), lty=2, color="red", linewidth=1)+
    #geom_hline(aes(yintercept= `97.5 %`), lty=2, color="red", linewidth=1) +
    geom_hline(aes(yintercept= 0), lty=1, color="black", linewidth=1) +
    labs(x = "Quantile", y = "Estimate")
    #labs(x = "Quantile", y = "Estimate", title = title) +
    #ylim(-10, 25) +
    #theme(plot.title = element_text(hjust = 0.5))
  return(p)
}

p_coef_loadBE <- plot_coef("scale(loadBE)", "Load Belgium") 
p_coef_loadNL <- plot_coef("scale(loadNL)", "Load Netherlands") # -2  5
p_coef_loadDE <- plot_coef("scale(loadDE)", "Load Germany") # -4  6
ggsave(p_coef_loadBE, width=8, height=5, units="in", filename=paste0("p_coef_loadBE_", hour, ".png"))
ggsave(p_coef_loadNL, width=8, height=5, units="in", filename=paste0("p_coef_loadNL_", hour, ".png"))
ggsave(p_coef_loadDE, width=8, height=5, units="in", filename=paste0("p_coef_loadDE_", hour, ".png"))

p_coef_generationBE <- plot_coef("scale(generationBE)", "Generation Belgium") # -4 2
p_coef_generationNL <- plot_coef("scale(generationNL)", "Generation Netherlands") # -5 1
p_coef_generationDE <- plot_coef("scale(generationDE)", "Generation Germany") # -5 3
p_coef_generationFR <- plot_coef("scale(generationFR)", "Generation France") # -5 3
ggsave(p_coef_generationBE, width=8, height=5, units="in", filename=paste0("p_coef_generationBE_", hour, ".png"))
ggsave(p_coef_generationNL, width=8, height=5, units="in", filename=paste0("p_coef_generationNL_", hour, ".png"))
ggsave(p_coef_generationDE, width=8, height=5, units="in", filename=paste0("p_coef_generationDE_", hour, ".png"))
ggsave(p_coef_generationFR, width=8, height=5, units="in", filename=paste0("p_coef_generationFR_", hour, ".png"))


p_coef_solarBE <- plot_coef("scale(solarBE)", "Solar generation Belgium") # -4 2
p_coef_onshoreBE <- plot_coef("scale(wind_onshoreBE)", "Wind onshore generation Belgium") # -5 1
p_coef_offshoreBE <- plot_coef("scale(wind_offshoreBE)", "Wind offshore generation Belgium") # -5 1
ggsave(p_coef_solarBE, width=8, height=5, units="in", filename=paste0("p_coef_solarBE_", hour, ".png"))
ggsave(p_coef_onshoreBE, width=8, height=5, units="in", filename=paste0("p_coef_onshoreBE_", hour, ".png"))
ggsave(p_coef_offshoreBE, width=8, height=5, units="in", filename=paste0("p_coef_offshoreBE_", hour, ".png"))


p_coef_priceBElag1 <- plot_coef("scale(priceBE_lag1)", "Price Belgium (lag 1)") #
p_coef_priceBElag2 <- plot_coef("scale(priceBE_lag2)", "Price Belgium (lag 2)") #
p_coef_priceBElag3 <- plot_coef("scale(priceBE_lag3)", "Price Belgium (lag 3)") #
p_coef_priceBElag4 <- plot_coef("scale(priceBE_lag4)", "Price Belgium (lag 4)") #
p_coef_priceBElag5 <- plot_coef("scale(priceBE_lag5)", "Price Belgium (lag 5)") #
ggsave(p_coef_priceBElag1, width=8, height=5, units="in", filename=paste0("p_coef_priceBElag1_", hour, ".png"))
ggsave(p_coef_priceBElag2, width=8, height=5, units="in", filename=paste0("p_coef_priceBElag2_", hour, ".png"))
ggsave(p_coef_priceBElag3, width=8, height=5, units="in", filename=paste0("p_coef_priceBElag3_", hour, ".png"))
ggsave(p_coef_priceBElag4, width=8, height=5, units="in", filename=paste0("p_coef_priceBElag4_", hour, ".png"))
ggsave(p_coef_priceBElag5, width=8, height=5, units="in", filename=paste0("p_coef_priceBElag5_", hour, ".png"))



p_coef_priceFRlag1 <- plot_coef("scale(priceFR_lag1)", "Price France (lag 1)") #
p_coef_priceNLlag1 <- plot_coef("scale(priceNL_lag1)", "Price Netherlands (lag 1)") #
p_coef_priceDElag1 <- plot_coef("scale(priceDE_lag1)", "Price Germany (lag 1)") #
ggsave(p_coef_priceFRlag1, width=8, height=5, units="in", filename=paste0("p_coef_priceFRlag1_", hour, ".png"))
ggsave(p_coef_priceNLlag1, width=8, height=5, units="in", filename=paste0("p_coef_priceNLlag1_", hour, ".png"))
ggsave(p_coef_priceDElag1, width=8, height=5, units="in", filename=paste0("p_coef_priceDElag1_", hour, ".png"))

p_coef_priceFRlag2 <- plot_coef("scale(priceFR_lag2)", "Price France (lag 2)") #
p_coef_priceNLlag2 <- plot_coef("scale(priceNL_lag2)", "Price Netherlands (lag 2)") #
p_coef_priceDElag2 <- plot_coef("scale(priceDE_lag2)", "Price Germany (lag 2)") #
ggsave(p_coef_priceFRlag2, width=8, height=5, units="in", filename=paste0("p_coef_priceFRlag2_", hour, ".png"))
ggsave(p_coef_priceNLlag2, width=8, height=5, units="in", filename=paste0("p_coef_priceNLlag2_", hour, ".png"))
ggsave(p_coef_priceDElag2, width=8, height=5, units="in", filename=paste0("p_coef_priceDElag2_", hour, ".png"))

p_coef_priceFRlag3 <- plot_coef("scale(priceFR_lag3)", "Price France (lag 3)") #
p_coef_priceNLlag3 <- plot_coef("scale(priceNL_lag3)", "Price Netherlands (lag 3)") #
p_coef_priceDElag3 <- plot_coef("scale(priceDE_lag3)", "Price Germany (lag 3)") #
ggsave(p_coef_priceFRlag3, width=8, height=5, units="in", filename=paste0("p_coef_priceFRlag3_", hour, ".png"))
ggsave(p_coef_priceNLlag3, width=8, height=5, units="in", filename=paste0("p_coef_priceNLlag3_", hour, ".png"))
ggsave(p_coef_priceDElag3, width=8, height=5, units="in", filename=paste0("p_coef_priceDElag3_", hour, ".png"))

p_coef_priceFRlag4 <- plot_coef("scale(priceFR_lag4)", "Price France (lag 4)") #
p_coef_priceNLlag4 <- plot_coef("scale(priceNL_lag4)", "Price Netherlands (lag 4)") #
p_coef_priceDElag4 <- plot_coef("scale(priceDE_lag4)", "Price Germany (lag 4)") #
ggsave(p_coef_priceFRlag4, width=8, height=5, units="in", filename=paste0("p_coef_priceFRlag4_", hour, ".png"))
ggsave(p_coef_priceNLlag4, width=8, height=5, units="in", filename=paste0("p_coef_priceNLlag4_", hour, ".png"))
ggsave(p_coef_priceDElag4, width=8, height=5, units="in", filename=paste0("p_coef_priceDElag4_", hour, ".png"))

p_coef_priceFRlag5 <- plot_coef("scale(priceFR_lag5)", "Price France (lag 5)") #
p_coef_priceNLlag5 <- plot_coef("scale(priceNL_lag5)", "Price Netherlands (lag 5)") #
p_coef_priceDElag5 <- plot_coef("scale(priceDE_lag5)", "Price Germany (lag 5)") #
ggsave(p_coef_priceFRlag5, width=8, height=5, units="in", filename=paste0("p_coef_priceFRlag5_", hour, ".png"))
ggsave(p_coef_priceNLlag5, width=8, height=5, units="in", filename=paste0("p_coef_priceNLlag5_", hour, ".png"))
ggsave(p_coef_priceDElag5, width=8, height=5, units="in", filename=paste0("p_coef_priceDElag5_", hour, ".png"))






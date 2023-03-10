# plots
lm <- lm(data=mtcars, 
         formula =  hp ~  disp + mpg + I(mpg^2) + qsec + am)

ols <- as.data.frame(coef(lm))
ols.ci <- as.data.frame(confint(lm))
ols2 <- cbind(ols, ols.ci)
ols2 <- tibble::rownames_to_column(ols2, var="term")


model1_viz <- rq(formula1, 
                 data=data_train,
                 tau = 1:9/10)
lm_viz <- lm(formula1,
             data=data_train)
ols <- as.data.frame(coef(lm_viz))
ols.ci <- as.data.frame(confint(lm_viz))
ols <- cbind(ols, ols.ci)
ols <- tibble::rownames_to_column(ols, var="term")

data_viz <- model1_viz %>%
  broom::tidy(se.type = "boot", conf.int = TRUE) 

data_viz <- merge(data_viz, ols,
                  by = "term",
                  all = TRUE)

plot_coef <- function(variable, title) {
  p <- ggplot(subset(data_viz, term == variable)) +
    geom_point(aes(x=tau,y=estimate), color="#27408b", size = 3) +
    geom_line(aes(x=tau,y=estimate), color="#27408b", linewidth = 1) +
    geom_ribbon(aes(x=tau, ymin=conf.low,ymax=conf.high),alpha=0.25, fill="#27408b") +
    geom_hline(aes(yintercept = `coef(lm_viz).x`), lty=1, color="red", linewidth = 1) +
    geom_hline(aes(yintercept= `2.5 %`), lty=2, color="red", linewidth=1)+
    geom_hline(aes(yintercept= `97.5 %`), lty=2, color="red", linewidth=1) +
    geom_hline(aes(yintercept= 0), lty=1, color="black", linewidth=1) +
    labs(x = "Quantile", y = "Estimate", title = title) +
    #ylim(-10, 25) +
    theme(plot.title = element_text(hjust = 0.5))
  return(p)
}

plot_coef("scale(loadBE)", "Load Belgium") # 5  21
plot_coef("scale(loadNL)", "Load Netherlands") # -2  5
plot_coef("scale(loadDE)", "Load Germany") # -4  6

plot_coef("scale(generationBE)", "Generation Belgium") # -4 2
plot_coef("scale(generationNL)", "Generation Netherlands") # -5 1
plot_coef("scale(generationFR)", "Generation France") # -10 0
plot_coef("scale(generationDE)", "Generation Germany") # -5 3

plot_coef("scale(solarBE)", "Solar generation Belgium") # -4 2
plot_coef("scale(wind_onshoreBE)", "Wind onshore generation Belgium") # -5 1
plot_coef("scale(wind_offshoreBE)", "Wind offshore generation Belgium") # -5 1

plot_coef("scale(priceBE_lag1)", "Price Belgium (lag 1)") #
plot_coef("scale(priceBE_lag2)", "Price Belgium (lag 2)") #

plot_coef("scale(priceFR_lag1)", "Price France (lag 1)") #
plot_coef("scale(priceNL_lag1)", "Price Netherlands (lag 1)") #
plot_coef("scale(priceDE_lag1)", "Price Germany (lag 1)") #
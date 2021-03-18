#### Regression - Test liquidity measures #### 

trace <- read.csv("TRACE.csv")
trades <- aggregate(type~cusip_id+trd_exctn_dt, data = trace, FUN = function(x){NROW(x)})
trades <- trades[with(trades, order(cusip_id, trd_exctn_dt)), ]
names(trades)[3] <- paste("trades")

### Create dataset ###
data.full <- read.csv("DAILY_all.csv")
names(data.full)[1] <- paste("cusip_id")
data.full$trd_exctn_dt <- as.Date.character(data.full$trd_exctn_dt, format = "%Y%m%d")
data.full$trades <- trades$trades


data <- aggregate(data.full[,c(23, 43, 15, 20, 19, 29,27,25)], list(data.full$cusip_id), mean, na.rm = TRUE, na.action = NULL)
names(data)[1] <- paste("cusip_id")
data$rating <- aggregate(data.full[,7], list(data.full$cusip_id), first)[,2]
data$type <- aggregate(data.full[,8], list(data.full$cusip_id), first)[,2]


### HHI measure ###

## HHI ~ p_avg ##

model.1 <- lm(HHI ~ p_avg, data = data)
summary(model.1)
confint(model.1)
pred.1 <- data.frame(HHI = data$HHI,
                     p_avg = data$p_avg,
                     pred = predict(model.1, interval = "prediction"),
                     conf = predict(model.1, interval = "confidence"),
                     e = residuals(model.1))

ggplot(data = data, aes(x = p_avg, y = HHI)) + 
  geom_point(size = 0.5) + 
  geom_smooth(data = data, method = lm, formula = y~x) +
  geom_line(data = pred.1, aes(y = pred.lwr), color = "red", linetype = "dashed") +
  geom_line(data = pred.1, aes(y = pred.upr), color = "red", linetype = "dashed")


## HHI ~ vol_tot ##

model.2 <- lm(HHI ~ vol_tot, data = data)
summary(model.2)
confint(model.2)
pred.2 <- data.frame(HHI = data$HHI,
                     vol_tot = data$vol_tot,
                     pred = predict(model.2, interval = "prediction"),
                     conf = predict(model.2, interval = "confidence"),
                     e = residuals(model.2))

ggplot(data = data, aes(x = vol_tot, y = HHI)) + 
  geom_point(size = 0.5) + 
  geom_smooth(data = data, method = lm, formula = y~x) +
  geom_line(data = pred.2, aes(y = pred.lwr), color = "red", linetype = "dashed") +
  geom_line(data = pred.2, aes(y = pred.upr), color = "red", linetype = "dashed")


## HHI ~ spread ##

model.3 <- lm(HHI ~ spread, data = data)
summary(model.3)
confint(model.3)
pred.3 <- data.frame(HHI = data$HHI,
                     spread = data$spread,
                     pred = predict(model.3, interval = "prediction"),
                     conf = predict(model.3, interval = "confidence"),
                     e = residuals(model.3))

ggplot(data = data, aes(x = spread, y = HHI)) + 
  geom_point(size = 0.5) + 
  geom_smooth(data = data, method = lm, formula = y~x) +
  geom_line(data = pred.3, aes(y = pred.lwr), color = "red", linetype = "dashed") +
  geom_line(data = pred.3, aes(y = pred.upr), color = "red", linetype = "dashed")


## HHI ~ p_diff ## 
model.4 <- lm(HHI ~ p_diff, data = data)
summary(model.4)
confint(model.4)
pred.4 <- data.frame(HHI = data$HHI,
                     diff = data$p_diff,
                     pred = predict(model.4, interval = "prediction"),
                     conf = predict(model.4, interval = "confidence"),
                     e = residuals(model.4))

ggplot(data = pred.4, aes(x = diff, y = HHI)) + 
  geom_point(size = 0.5) + 
  geom_smooth(data = pred.4, method = lm, formula = y~x) +
  geom_line(data = pred.4, aes(y = pred.lwr), color = "red", linetype = "dashed") +
  geom_line(data = pred.4, aes(y = pred.upr), color = "red", linetype = "dashed")


## HHI ~ intraday_trades ##   

model.5 <- lm(HHI ~ trades, data = data)
summary(model.5)
confint(model.5)
pred.5 <- data.frame(HHI = data$HHI,
                     trades = data$trades,
                     pred = predict(model.5, interval = "prediction"),
                     conf = predict(model.5, interval = "confidence"),
                     e = residuals(model.5))

ggplot(data = data, aes(x = trades, y = HHI)) + 
  geom_point(size = 0.5) + 
  geom_smooth(data = data, method = lm, formula = y~x) +
  geom_line(data = pred.5, aes(y = pred.lwr), color = "red", linetype = "dashed") +
  geom_line(data = pred.5, aes(y = pred.upr), color = "red", linetype = "dashed")



## HHI ~ p_avg * vol_tot ## 

model.6 <- lm(HHI ~ vol_tot*p_avg, data = data)
summary(model.6)
confint(model.6)
pred.6 <- data.frame(pred = predict(model.6, interval = "prediction"),
                     conf = predict(model.6, interval = "confidence"),
                     e = residuals(model.6))


## log(HHI) ~ p_avg ##

model.7 <- lm(log(HHI) ~ p_avg, data = data)
summary(model.7)
confint(model.7)
pred.7 <- data.frame(HHI = data$HHI,
                     p_avg = data$p_avg,
                     pred = predict(model.7, interval = "prediction"),
                     conf = predict(model.7, interval = "confidence"),
                     e = residuals(model.7))

ggplot(data = data, aes(x = p_avg, y = HHI)) + 
  geom_point(size = 0.5) + 
  geom_line(data = pred.7, aes(y = exp(pred.lwr)), color = "red", linetype = "dashed") +
  geom_line(data = pred.7, aes(y = exp(pred.upr)), color = "red", linetype = "dashed") + 
  geom_line(data = pred.7, aes(y = exp(conf.fit)), color = "blue") +
  geom_ribbon(data = pred.7, aes(ymin = exp(conf.lwr),
                                 ymax = exp(conf.upr)), alpha = 0.3, fill = "green")


## log(HHI) ~ vol_tot ##

model.8 <- lm(log(HHI) ~ vol_tot, data = data)
summary(model.8)
confint(model.8)
pred.8 <- data.frame(HHI = data$HHI,
                     vol_tot = data$vol_tot,
                     pred = predict(model.8, interval = "prediction"),
                     conf = predict(model.8, interval = "confidence"),
                     e = residuals(model.8))

ggplot(data = data, aes(x = vol_tot, y = HHI)) + 
  geom_point(size = 0.5) + 
  geom_line(data = pred.8, aes(y = exp(pred.lwr)), color = "red", linetype = "dashed") +
  geom_line(data = pred.8, aes(y = exp(pred.upr)), color = "red", linetype = "dashed") + 
  geom_line(data = pred.8, aes(y = exp(conf.fit)), color = "blue") +
  geom_ribbon(data = pred.8, aes(ymin = exp(conf.lwr),
                                 ymax = exp(conf.upr)), alpha = 0.3, fill = "green")


## log(HHI) ~ spread ##

model.9 <- lm(log(HHI) ~ spread, data = data)
summary(model.9)
confint(model.9)
pred.9 <- data.frame(HHI = data$HHI,
                     spread = data$spread,
                     pred = predict(model.9, interval = "prediction"),
                     conf = predict(model.9, interval = "confidence"),
                     e = residuals(model.9))

ggplot(data = data, aes(x = spread, y = HHI)) + 
  geom_point(size = 0.5) + 
  geom_line(data = pred.9, aes(y = exp(pred.lwr)), color = "red", linetype = "dashed") +
  geom_line(data = pred.9, aes(y = exp(pred.upr)), color = "red", linetype = "dashed") + 
  geom_line(data = pred.9, aes(y = exp(conf.fit)), color = "blue") +
  geom_ribbon(data = pred.9, aes(ymin = exp(conf.lwr),
                                 ymax = exp(conf.upr)), alpha = 0.3, fill = "green")

## log(HHI) ~ p_diff ##

model.10 <- lm(log(HHI) ~ p_diff, data = data)
summary(model.10)
confint(model.10)
pred.10 <- data.frame(HHI = data$HHI,
                      p_diff = data$p_diff,
                      pred = predict(model.10, interval = "prediction"),
                      conf = predict(model.10, interval = "confidence"),
                      e = residuals(model.10))

ggplot(data = data, aes(x = p_diff, y = HHI)) + 
  geom_point(size = 0.5) + 
  geom_line(data = pred.10, aes(y = exp(pred.lwr)), color = "red", linetype = "dashed") +
  geom_line(data = pred.10, aes(y = exp(pred.upr)), color = "red", linetype = "dashed") + 
  geom_line(data = pred.10, aes(y = exp(conf.fit)), color = "blue") +
  geom_ribbon(data = pred.10, aes(ymin = exp(conf.lwr),
                                  ymax = exp(conf.upr)), alpha = 0.3, fill = "green")


## log(HHI) ~ trades ##

model.11 <- lm(log(HHI) ~ trades, data = data)
summary(model.11)
confint(model.11)
pred.11 <- data.frame(HHI = data$HHI,
                      trades = data$trades,
                      pred = predict(model.11, interval = "prediction"),
                      conf = predict(model.11, interval = "confidence"),
                      e = residuals(model.11))

ggplot(data = data, aes(x = trades, y = HHI)) + 
  geom_point(size = 0.5) + 
  geom_line(data = pred.11, aes(y = exp(pred.lwr)), color = "red", linetype = "dashed") +
  geom_line(data = pred.11, aes(y = exp(pred.upr)), color = "red", linetype = "dashed") + 
  geom_line(data = pred.11, aes(y = exp(conf.fit)), color = "blue") +
  geom_ribbon(data = pred.11, aes(ymin = exp(conf.lwr),
                                  ymax = exp(conf.upr)), alpha = 0.3, fill = "green")


## log(HHI) ~ vol_tot*p_avg ## 
model.12 <- lm(log(HHI) ~ vol_tot*p_avg, data = data)
summary(model.12)
confint(model.12)
pred.12 <- data.frame(pred = predict(model.12, interval = "prediction"),
                      conf = predict(model.12, interval = "confidence"),
                      e = residuals(model.12))



## HHI ~ log(p_avg) ##

model.13 <- lm(HHI ~ log(p_avg), data = data)
summary(model.13)
confint(model.13)
pred.13 <- data.frame(HHI = data$HHI,
                      p_avg = data$p_avg,
                      pred = predict(model.13, interval = "prediction"),
                      conf = predict(model.13, interval = "confidence"),
                      e = residuals(model.13))

ggplot(data = data, aes(x = p_avg, y = HHI)) + 
  geom_point(size = 0.5) + 
  geom_line(data = pred.13, aes(y = pred.lwr), color = "red", linetype = "dashed") +
  geom_line(data = pred.13, aes(y = pred.upr), color = "red", linetype = "dashed") + 
  geom_line(data = pred.13, aes(y = conf.fit), color = "blue") +
  geom_ribbon(data = pred.13, aes(ymin = conf.lwr,
                                  ymax = conf.upr), alpha = 0.3, fill = "green")


## HHI ~ log(vol_tot) ##

model.14 <- lm(HHI ~ log(vol_tot), data = data)
summary(model.14)
confint(model.14)
pred.14 <- data.frame(HHI = data$HHI,
                      vol_tot = data$vol_tot,
                      pred = predict(model.14, interval = "prediction"),
                      conf = predict(model.14, interval = "confidence"),
                      e = residuals(model.14))

ggplot(data = data, aes(x = vol_tot, y = HHI)) + 
  geom_point(size = 0.5) + 
  geom_line(data = pred.14, aes(y = pred.lwr), color = "red", linetype = "dashed") +
  geom_line(data = pred.14, aes(y = pred.upr), color = "red", linetype = "dashed") + 
  geom_line(data = pred.14, aes(y = conf.fit), color = "blue") +
  geom_ribbon(data = pred.14, aes(ymin = conf.lwr,
                                  ymax = conf.upr), alpha = 0.3, fill = "green")


## HHI ~ log(spread) ##

model.15 <- lm(HHI ~ log(spread), data = data[which(data$spread>0), ])
summary(model.15)
confint(model.15)
pred.15 <- data.frame(HHI =  data[which(data$spread>0), ]$HHI,
                      spread =  data[which(data$spread>0), ]$spread,
                      pred = predict(model.15, interval = "prediction"),
                      conf = predict(model.15, interval = "confidence"),
                      e = residuals(model.15))

ggplot(data = pred.15, aes(x = spread, y = HHI)) + 
  geom_point(size = 0.5) + 
  geom_line(data = pred.15, aes(y = pred.lwr), color = "red", linetype = "dashed") +
  geom_line(data = pred.15, aes(y = pred.upr), color = "red", linetype = "dashed") + 
  geom_line(data = pred.15, aes(y = conf.fit), color = "blue") +
  geom_ribbon(data = pred.15, aes(ymin = conf.lwr,
                                  ymax = conf.upr), alpha = 0.3, fill = "green")


## HHI ~ log(p_diff) ##

model.16 <- lm(HHI ~ log(p_diff), data = data)
summary(model.16)
confint(model.16)
pred.16 <- data.frame(HHI = data$HHI,
                      p_diff = data$p_diff,
                      pred = predict(model.16, interval = "prediction"),
                      conf = predict(model.16, interval = "confidence"),
                      e = residuals(model.16))

ggplot(data = data, aes(x = p_diff, y = HHI)) + 
  geom_point(size = 0.5) + 
  geom_line(data = pred.16, aes(y = pred.lwr), color = "red", linetype = "dashed") +
  geom_line(data = pred.16, aes(y = pred.upr), color = "red", linetype = "dashed") + 
  geom_line(data = pred.16, aes(y = conf.fit), color = "blue") +
  geom_ribbon(data = pred.16, aes(ymin = conf.lwr,
                                  ymax = conf.upr), alpha = 0.3, fill = "green")


## HHI ~ log(trades) ##

model.17 <- lm(HHI ~ log(trades), data = data)
summary(model.17)
confint(model.17)
pred.17 <- data.frame(HHI = data$HHI,
                      trades = data$trades,
                      pred = predict(model.17, interval = "prediction"),
                      conf = predict(model.17, interval = "confidence"),
                      e = residuals(model.17))

ggplot(data = data, aes(x = trades, y = HHI)) + 
  geom_point(size = 0.5) + 
  geom_line(data = pred.17, aes(y = pred.lwr), color = "red", linetype = "dashed") +
  geom_line(data = pred.17, aes(y = pred.upr), color = "red", linetype = "dashed") + 
  geom_line(data = pred.17, aes(y = conf.fit), color = "blue") +
  geom_ribbon(data = pred.17, aes(ymin = conf.lwr,
                                  ymax = conf.upr), alpha = 0.3, fill = "green")



## HHI ~ log(vol_tot)*log(p_avg) ##

model.18 <- lm(HHI ~ log(vol_tot)*log(p_avg), data = data)
summary(model.18)
confint(model.18)
pred.18 <- data.frame(pred = predict(model.18, interval = "prediction"),
                      conf = predict(model.18, interval = "confidence"),
                      e = residuals(model.18))


### SUMMARY HHI measure ### 
results.HHI <- data.frame(model = 1:18,
                            description = c("HHI~p_avg", "HHI~vol_tot", "HHI~spread", "HHI~p_diff", "HHI~trades", "HHI~vol_tot*p_avg", "log(HHI)~p_avg", "log(HHI)~vol_tot", "log(HHI)~spread", "log(HHI)~p_diff", "log(HHI)~trades", "log(HHI)~vol_tot*p_avg", "HHI~log(p_avg)", "HHI~log(vol_tot)", "HHI~log(spread)", "HHI~log(p_diff)", "HHI~log(trades)", "HHI~log(vol_tot)*log(p_avg)"),
                            beta0 = c(model.1$coefficients[1], model.2$coefficients[1], model.3$coefficients[1], model.4$coefficients[1], model.5$coefficients[1], model.6$coefficients[1], model.7$coefficients[1], model.8$coefficients[1], model.9$coefficients[1], model.10$coefficients[1], model.11$coefficients[1], model.12$coefficients[1], model.13$coefficients[1], model.14$coefficients[1], model.15$coefficients[1], model.16$coefficients[1], model.17$coefficients[1], model.18$coefficients[1]),
                            beta1 =  c(model.1$coefficients[2], model.2$coefficients[2], model.3$coefficients[2], model.4$coefficients[2], model.5$coefficients[2], model.6$coefficients[2], model.7$coefficients[2], model.8$coefficients[2], model.9$coefficients[2], model.10$coefficients[2], model.11$coefficients[2], model.12$coefficients[2], model.13$coefficients[2], model.14$coefficients[2], model.15$coefficients[2], model.16$coefficients[2], model.17$coefficients[2], model.18$coefficients[2]),
                            beta2 = c(NA, NA, NA, NA, NA, model.6$coefficients[3],NA,NA,NA,NA,NA, model.12$coefficients[3], NA, NA, NA, NA, NA, model.18$coefficients[3]),
                            beta3 = c(NA, NA, NA, NA, NA, model.6$coefficients[4],NA,NA,NA,NA,NA, model.12$coefficients[4], NA, NA, NA, NA, NA, model.18$coefficients[4]),
                            p_value1 = c(coef(summary(model.1))[,4][2], coef(summary(model.2))[,4][2], coef(summary(model.3))[,4][2], coef(summary(model.4))[,4][2], coef(summary(model.5))[,4][2], coef(summary(model.6))[,4][2], coef(summary(model.7))[,4][2], coef(summary(model.8))[,4][2], coef(summary(model.9))[,4][2], coef(summary(model.10))[,4][2], coef(summary(model.11))[,4][2], coef(summary(model.12))[,4][2], coef(summary(model.13))[,4][2], coef(summary(model.14))[,4][2], coef(summary(model.15))[,4][2], coef(summary(model.16))[,4][2], coef(summary(model.17))[,4][2], coef(summary(model.18))[,4][2]),
                            p_value2 = c(NA, NA, NA, NA, NA, coef(summary(model.6))[,4][3],NA,NA,NA,NA,NA, coef(summary(model.12))[,4][3], NA, NA, NA, NA, NA, coef(summary(model.18))[,4][3]),
                            p_value3 = c(NA, NA, NA, NA, NA, coef(summary(model.6))[,4][4],NA,NA,NA,NA,NA, coef(summary(model.12))[,4][4], NA, NA, NA, NA, NA, coef(summary(model.18))[,4][4]))


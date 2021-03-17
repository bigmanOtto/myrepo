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


### d measure ###

## d ~ p_avg ##

model.1 <- lm(d ~ p_avg, data = data)
summary(model.1)
confint(model.1)
pred.1 <- data.frame(d = data$d,
                     p_avg = data$p_avg,
                     pred = predict(model.1, interval = "prediction"),
                     conf = predict(model.1, interval = "confidence"),
                     e = residuals(model.1))

ggplot(data = data, aes(x = p_avg, y = d)) + 
  geom_point(size = 0.5) + 
  geom_smooth(data = data, method = lm, formula = y~x) +
  geom_line(data = pred.1, aes(y = pred.lwr), color = "red", linetype = "dashed") +
  geom_line(data = pred.1, aes(y = pred.upr), color = "red", linetype = "dashed")


## d ~ vol_tot ##

model.2 <- lm(d ~ vol_tot, data = data)
summary(model.2)
confint(model.2)
pred.2 <- data.frame(d = data$d,
                     vol_tot = data$vol_tot,
                     pred = predict(model.2, interval = "prediction"),
                     conf = predict(model.2, interval = "confidence"),
                     e = residuals(model.2))

ggplot(data = data, aes(x = vol_tot, y = d)) + 
  geom_point(size = 0.5) + 
  geom_smooth(data = data, method = lm, formula = y~x) +
  geom_line(data = pred.2, aes(y = pred.lwr), color = "red", linetype = "dashed") +
  geom_line(data = pred.2, aes(y = pred.upr), color = "red", linetype = "dashed")


## d ~ spread ##

model.3 <- lm(d ~ spread, data = data)
summary(model.3)
confint(model.3)
pred.3 <- data.frame(d = data$d,
                     spread = data$spread,
                     pred = predict(model.3, interval = "prediction"),
                     conf = predict(model.3, interval = "confidence"),
                     e = residuals(model.3))

ggplot(data = data, aes(x = spread, y = d)) + 
  geom_point(size = 0.5) + 
  geom_smooth(data = data, method = lm, formula = y~x) +
  geom_line(data = pred.3, aes(y = pred.lwr), color = "red", linetype = "dashed") +
  geom_line(data = pred.3, aes(y = pred.upr), color = "red", linetype = "dashed")


## d ~ p_diff ## 
model.4 <- lm(d ~ p_diff, data = data)
summary(model.4)
confint(model.4)
pred.4 <- data.frame(d = data$d,
                      diff = data$p_diff,
                      pred = predict(model.4, interval = "prediction"),
                      conf = predict(model.4, interval = "confidence"),
                      e = residuals(model.4))

ggplot(data = pred.4, aes(x = diff, y = d)) + 
  geom_point(size = 0.5) + 
  geom_smooth(data = pred.4, method = lm, formula = y~x) +
  geom_line(data = pred.4, aes(y = pred.lwr), color = "red", linetype = "dashed") +
  geom_line(data = pred.4, aes(y = pred.upr), color = "red", linetype = "dashed")


## d ~ intraday_trades ##   

model.5 <- lm(d ~ trades, data = data)
summary(model.5)
confint(model.5)
pred.5 <- data.frame(d = data$d,
                     trades = data$trades,
                     pred = predict(model.5, interval = "prediction"),
                     conf = predict(model.5, interval = "confidence"),
                     e = residuals(model.5))

ggplot(data = data, aes(x = trades, y = d)) + 
  geom_point(size = 0.5) + 
  geom_smooth(data = data, method = lm, formula = y~x) +
  geom_line(data = pred.5, aes(y = pred.lwr), color = "red", linetype = "dashed") +
  geom_line(data = pred.5, aes(y = pred.upr), color = "red", linetype = "dashed")



## d ~ p_avg * vol_tot ## 

model.6 <- lm(d ~ vol_tot*p_avg, data = data)
summary(model.6)
confint(model.6)
pred.6 <- data.frame(pred = predict(model.6, interval = "prediction"),
                     conf = predict(model.6, interval = "confidence"),
                     e = residuals(model.6))


## log(d) ~ p_avg ##

model.7 <- lm(log(d) ~ p_avg, data = data)
summary(model.7)
confint(model.7)
pred.7 <- data.frame(d = data$d,
                     p_avg = data$p_avg,
                     pred = predict(model.7, interval = "prediction"),
                     conf = predict(model.7, interval = "confidence"),
                     e = residuals(model.7))

ggplot(data = data, aes(x = p_avg, y = d)) + 
  geom_point(size = 0.5) + 
  geom_line(data = pred.7, aes(y = exp(pred.lwr)), color = "red", linetype = "dashed") +
  geom_line(data = pred.7, aes(y = exp(pred.upr)), color = "red", linetype = "dashed") + 
  geom_line(data = pred.7, aes(y = exp(conf.fit)), color = "blue") +
  geom_ribbon(data = pred.7, aes(ymin = exp(conf.lwr),
                                ymax = exp(conf.upr)), alpha = 0.3, fill = "green")
            

## log(d) ~ vol_tot ##

model.8 <- lm(log(d) ~ vol_tot, data = data)
summary(model.8)
confint(model.8)
pred.8 <- data.frame(d = data$d,
                     vol_tot = data$vol_tot,
                     pred = predict(model.8, interval = "prediction"),
                     conf = predict(model.8, interval = "confidence"),
                     e = residuals(model.8))

ggplot(data = data, aes(x = vol_tot, y = d)) + 
  geom_point(size = 0.5) + 
  geom_line(data = pred.8, aes(y = exp(pred.lwr)), color = "red", linetype = "dashed") +
  geom_line(data = pred.8, aes(y = exp(pred.upr)), color = "red", linetype = "dashed") + 
  geom_line(data = pred.8, aes(y = exp(conf.fit)), color = "blue") +
  geom_ribbon(data = pred.8, aes(ymin = exp(conf.lwr),
                                 ymax = exp(conf.upr)), alpha = 0.3, fill = "green")


## log(d) ~ spread ##

model.9 <- lm(log(d) ~ spread, data = data)
summary(model.9)
confint(model.9)
pred.9 <- data.frame(d = data$d,
                     spread = data$spread,
                     pred = predict(model.9, interval = "prediction"),
                     conf = predict(model.9, interval = "confidence"),
                     e = residuals(model.9))

ggplot(data = data, aes(x = spread, y = d)) + 
  geom_point(size = 0.5) + 
  geom_line(data = pred.9, aes(y = exp(pred.lwr)), color = "red", linetype = "dashed") +
  geom_line(data = pred.9, aes(y = exp(pred.upr)), color = "red", linetype = "dashed") + 
  geom_line(data = pred.9, aes(y = exp(conf.fit)), color = "blue") +
  geom_ribbon(data = pred.9, aes(ymin = exp(conf.lwr),
                                 ymax = exp(conf.upr)), alpha = 0.3, fill = "green")

## log(d) ~ p_diff ##

model.10 <- lm(log(d) ~ p_diff, data = data)
summary(model.10)
confint(model.10)
pred.10 <- data.frame(d = data$d,
                     p_diff = data$p_diff,
                     pred = predict(model.10, interval = "prediction"),
                     conf = predict(model.10, interval = "confidence"),
                     e = residuals(model.10))

ggplot(data = data, aes(x = p_diff, y = d)) + 
  geom_point(size = 0.5) + 
  geom_line(data = pred.10, aes(y = exp(pred.lwr)), color = "red", linetype = "dashed") +
  geom_line(data = pred.10, aes(y = exp(pred.upr)), color = "red", linetype = "dashed") + 
  geom_line(data = pred.10, aes(y = exp(conf.fit)), color = "blue") +
  geom_ribbon(data = pred.10, aes(ymin = exp(conf.lwr),
                                 ymax = exp(conf.upr)), alpha = 0.3, fill = "green")


## log(d) ~ trades ##

model.11 <- lm(log(d) ~ trades, data = data)
summary(model.11)
confint(model.11)
pred.11 <- data.frame(d = data$d,
                      trades = data$trades,
                      pred = predict(model.11, interval = "prediction"),
                      conf = predict(model.11, interval = "confidence"),
                      e = residuals(model.11))

ggplot(data = data, aes(x = trades, y = d)) + 
  geom_point(size = 0.5) + 
  geom_line(data = pred.11, aes(y = exp(pred.lwr)), color = "red", linetype = "dashed") +
  geom_line(data = pred.11, aes(y = exp(pred.upr)), color = "red", linetype = "dashed") + 
  geom_line(data = pred.11, aes(y = exp(conf.fit)), color = "blue") +
  geom_ribbon(data = pred.11, aes(ymin = exp(conf.lwr),
                                  ymax = exp(conf.upr)), alpha = 0.3, fill = "green")


## log(d) ~ vol_tot*p_avg ## 
model.12 <- lm(log(d) ~ vol_tot*p_avg, data = data)
summary(model.12)
confint(model.12)
pred.12 <- data.frame(pred = predict(model.12, interval = "prediction"),
                     conf = predict(model.12, interval = "confidence"),
                     e = residuals(model.12))



## d ~ log(p_avg) ##

model.13 <- lm(d ~ log(p_avg), data = data)
summary(model.13)
confint(model.13)
pred.13 <- data.frame(d = data$d,
                      p_avg = data$p_avg,
                      pred = predict(model.13, interval = "prediction"),
                      conf = predict(model.13, interval = "confidence"),
                      e = residuals(model.13))

ggplot(data = data, aes(x = p_avg, y = d)) + 
  geom_point(size = 0.5) + 
  geom_line(data = pred.13, aes(y = pred.lwr), color = "red", linetype = "dashed") +
  geom_line(data = pred.13, aes(y = pred.upr), color = "red", linetype = "dashed") + 
  geom_line(data = pred.13, aes(y = conf.fit), color = "blue") +
  geom_ribbon(data = pred.13, aes(ymin = conf.lwr,
                                  ymax = conf.upr), alpha = 0.3, fill = "green")


## d ~ log(vol_tot) ##

model.14 <- lm(d ~ log(vol_tot), data = data)
summary(model.14)
confint(model.14)
pred.14 <- data.frame(d = data$d,
                      vol_tot = data$vol_tot,
                      pred = predict(model.14, interval = "prediction"),
                      conf = predict(model.14, interval = "confidence"),
                      e = residuals(model.14))

ggplot(data = data, aes(x = vol_tot, y = d)) + 
  geom_point(size = 0.5) + 
  geom_line(data = pred.14, aes(y = pred.lwr), color = "red", linetype = "dashed") +
  geom_line(data = pred.14, aes(y = pred.upr), color = "red", linetype = "dashed") + 
  geom_line(data = pred.14, aes(y = conf.fit), color = "blue") +
  geom_ribbon(data = pred.14, aes(ymin = conf.lwr,
                                  ymax = conf.upr), alpha = 0.3, fill = "green")


## d ~ log(spread) ##

model.15 <- lm(d ~ log(spread), data = data[which(data$spread>0), ])
summary(model.15)
confint(model.15)
pred.15 <- data.frame(d =  data[which(data$spread>0), ]$d,
                      spread =  data[which(data$spread>0), ]$spread,
                      pred = predict(model.15, interval = "prediction"),
                      conf = predict(model.15, interval = "confidence"),
                      e = residuals(model.15))

ggplot(data = pred.15, aes(x = spread, y = d)) + 
  geom_point(size = 0.5) + 
  geom_line(data = pred.15, aes(y = pred.lwr), color = "red", linetype = "dashed") +
  geom_line(data = pred.15, aes(y = pred.upr), color = "red", linetype = "dashed") + 
  geom_line(data = pred.15, aes(y = conf.fit), color = "blue") +
  geom_ribbon(data = pred.15, aes(ymin = conf.lwr,
                                  ymax = conf.upr), alpha = 0.3, fill = "green")


## d ~ log(p_diff) ##

model.16 <- lm(d ~ log(p_diff), data = data)
summary(model.16)
confint(model.16)
pred.16 <- data.frame(d = data$d,
                      p_diff = data$p_diff,
                      pred = predict(model.16, interval = "prediction"),
                      conf = predict(model.16, interval = "confidence"),
                      e = residuals(model.16))

ggplot(data = data, aes(x = p_diff, y = d)) + 
  geom_point(size = 0.5) + 
  geom_line(data = pred.16, aes(y = pred.lwr), color = "red", linetype = "dashed") +
  geom_line(data = pred.16, aes(y = pred.upr), color = "red", linetype = "dashed") + 
  geom_line(data = pred.16, aes(y = conf.fit), color = "blue") +
  geom_ribbon(data = pred.16, aes(ymin = conf.lwr,
                                  ymax = conf.upr), alpha = 0.3, fill = "green")


## d ~ log(trades) ##

model.17 <- lm(d ~ log(trades), data = data)
summary(model.17)
confint(model.17)
pred.17 <- data.frame(d = data$d,
                      trades = data$trades,
                      pred = predict(model.17, interval = "prediction"),
                      conf = predict(model.17, interval = "confidence"),
                      e = residuals(model.17))

ggplot(data = data, aes(x = trades, y = d)) + 
  geom_point(size = 0.5) + 
  geom_line(data = pred.17, aes(y = pred.lwr), color = "red", linetype = "dashed") +
  geom_line(data = pred.17, aes(y = pred.upr), color = "red", linetype = "dashed") + 
  geom_line(data = pred.17, aes(y = conf.fit), color = "blue") +
  geom_ribbon(data = pred.17, aes(ymin = conf.lwr,
                                  ymax = conf.upr), alpha = 0.3, fill = "green")



## d ~ log(vol_tot)*log(p_avg) ##

model.18 <- lm(d ~ log(vol_tot)*log(p_avg), data = data)
summary(model.18)
confint(model.18)
pred.18 <- data.frame(pred = predict(model.18, interval = "prediction"),
                      conf = predict(model.18, interval = "confidence"),
                      e = residuals(model.18))

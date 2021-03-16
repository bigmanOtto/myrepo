#### Regression - Test liquidity measures #### 


### Create dataset ###
data.full <- read.csv("DAILY_all.csv")
names(data.full)[1] <- paste("cusip_id")

data <- aggregate(data.full[,c(15, 20, 19, 29,27,25)], list(data.full$cusip_id), mean, na.rm = TRUE, na.action = NULL)
names(data)[1] <- paste("cusip")
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



## d ~ p_avg * vol_tot ## 

model.4 <- lm(d ~ vol_tot*p_avg, data = data)
summary(model.4)
confint(model.4)
pred.4 <- data.frame(pred = predict(model.4, interval = "prediction"),
                     conf = predict(model.4, interval = "confidence"),
                     e = residuals(model.4))


## log(d) ~ p_avg ##

model.5 <- lm(log(d) ~ p_avg, data = data)
summary(model.5)
confint(model.5)
pred.5 <- data.frame(d = data$d,
                     p_avg = data$p_avg,
                     pred = predict(model.5, interval = "prediction"),
                     conf = predict(model.5, interval = "confidence"),
                     e = residuals(model.5))

ggplot(data = data, aes(x = p_avg, y = d)) + 
  geom_point(size = 0.5) + 
  geom_line(data = pred.5, aes(y = exp(pred.lwr)), color = "red", linetype = "dashed") +
  geom_line(data = pred.5, aes(y = exp(pred.upr)), color = "red", linetype = "dashed") + 
  geom_line(data = pred.5, aes(y = exp(conf.fit)), color = "blue") +
  geom_ribbon(data = pred.5, aes(ymin = exp(conf.lwr),
                                ymax = exp(conf.upr)), alpha = 0.3, fill = "green")
            


####      MODELLING       ####
library(lme4)

data <- read.csv("DAILY_all.csv")
names(data)[1] <- paste("cusip_id")
subset <- data[1:502,]

### (lin) d - liquidity measure ###

##  (lin) d ~ p_avg   ##
model.1 <- lm(data = subset, d ~ p_avg)
model.1$coefficients
summary(model.1)
confint(model.1)
pred.1 <- data.frame(d = subset$d,
                p_avg = subset$p_avg,
                pred = predict(model.1, interval = "prediction"),
                conf = predict(model.1, interval = "confidence"),
                e = residuals(model.1))

ggplot(data = subset, aes(x = p_avg, y = d)) + 
  geom_point(data = subset, size = 0.5) + 
  geom_smooth(data = subset, method = lm, formula = y~x) +
  geom_line(data = pred.1, aes(y = pred.lwr), color = "red", linetype = "dashed") +
  geom_line(data = pred.1, aes(y = pred.upr), color = "red", linetype = "dashed")

##  (lin) d ~ vol_tot   ##
model.2 <- lm(data = subset, d ~ vol_tot)
model.2$coefficients
summary(model.2)
confint(model.2)
pred.2 <- data.frame(d = subset$d,
                     vol_tot = subset$vol_tot,
                     pred = predict(model.2, interval = "prediction"),
                     conf = predict(model.2, interval = "confidence"),
                     e = residuals(model.2))

ggplot(data = subset, aes(x = vol_tot, y = d)) + 
  geom_point(data = subset, size = 0.5) + 
  geom_smooth(data = subset, method = lm, formula = y~x) +
  geom_line(data = pred.2, aes(y = pred.lwr), color = "red", linetype = "dashed") +
  geom_line(data = pred.2, aes(y = pred.upr), color = "red", linetype = "dashed")


##  (lin) d ~ spread   ##
model.3 <- lm(data = subset, d ~ spread)
model.3$coefficients
summary(model.3)
confint(model.3)
pred.3 <- data.frame(d = subset$d,
                     spread = subset$spread,
                     pred = predict(model.3, interval = "prediction"),
                     conf = predict(model.3, interval = "confidence"),
                     e = residuals(model.3))

ggplot(data = subset, aes(x = spread, y = d)) + 
  geom_point(data = subset, size = 0.5) + 
  geom_smooth(data = subset, method = lm, formula = y~x) +
  geom_line(data = pred.3, aes(y = pred.lwr), color = "red", linetype = "dashed") +
  geom_line(data = pred.3, aes(y = pred.upr), color = "red", linetype = "dashed")


## MULTI REGRESSION test ## 
model <- lmList(d ~ p_avg | cusip_id, data = data)
test <- data.frame(coef = coefficients(model),
                   conf = confint(model),
                   p_value = summary(model)$coef[,4,2],
                   r_squared = summary(model)$r.squared)
test$cusip <- row.names(test)






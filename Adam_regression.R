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

## (lin) d ~ sp500 ##
model.4 <- lm(data = subset, d ~ sp500_close)
model.4$coefficients
summary(model.4)
confint(model.4)
pred.4 <- data.frame(d = subset$d,
                     sp500 = subset$sp500_close,
                     pred = predict(model.4, interval = "prediction"),
                     conf = predict(model.4, interval = "confidence"),
                     e = residuals(model.4))

ggplot(data = pred.4, aes(x = sp500, y = d)) + 
  geom_point(data = pred.4, size = 0.5) + 
  geom_smooth(data = pred.4, method = lm, formula = y~x) +
  geom_line(data = pred.4, aes(y = pred.lwr), color = "red", linetype = "dashed") +
  geom_line(data = pred.4, aes(y = pred.upr), color = "red", linetype = "dashed")


## (lin) d ~ russell3000 ##
model.5 <- lm(data = subset, d ~ russell_close)
model.5$coefficients
summary(model.5)
confint(model.5)
pred.5 <- data.frame(d = subset$d,
                     russell = subset$russell_close,
                     pred = predict(model.5, interval = "prediction"),
                     conf = predict(model.5, interval = "confidence"),
                     e = residuals(model.5))

ggplot(data = pred.5, aes(x = russell, y = d)) + 
  geom_point(data = pred.5, size = 0.5) + 
  geom_smooth(data = pred.5, method = lm, formula = y~x) +
  geom_line(data = pred.5, aes(y = pred.lwr), color = "red", linetype = "dashed") +
  geom_line(data = pred.5, aes(y = pred.upr), color = "red", linetype = "dashed")


## (lin) d ~ Smallcap ##
model.6 <- lm(data = subset, d ~ smallcap_close)
model.6$coefficients
summary(model.6)
confint(model.6)
pred.6 <- data.frame(d = subset$d,
                     smallcap = subset$smallcap_close,
                     pred = predict(model.6, interval = "prediction"),
                     conf = predict(model.6, interval = "confidence"),
                     e = residuals(model.6))

ggplot(data = pred.6, aes(x = smallcap, y = d)) + 
  geom_point(data = pred.6, size = 0.5) + 
  geom_smooth(data = pred.6, method = lm, formula = y~x) +
  geom_line(data = pred.6, aes(y = pred.lwr), color = "red", linetype = "dashed") +
  geom_line(data = pred.6, aes(y = pred.upr), color = "red", linetype = "dashed")


## (lin) d ~ S&P Bond ##
model.7 <- lm(data = subset, d ~ spbond_close)
model.7$coefficients
summary(model.7)
confint(model.7)
pred.7 <- data.frame(d = subset$d,
                     smallcap = subset$smallcap_close,
                     pred = predict(model.6, interval = "prediction"),
                     conf = predict(model.6, interval = "confidence"),
                     e = residuals(model.6))

ggplot(data = pred.6, aes(x = smallcap, y = d)) + 
  geom_point(data = pred.6, size = 0.5) + 
  geom_smooth(data = pred.6, method = lm, formula = y~x) +
  geom_line(data = pred.6, aes(y = pred.lwr), color = "red", linetype = "dashed") +
  geom_line(data = pred.6, aes(y = pred.upr), color = "red", linetype = "dashed")


## MULTI REGRESSION test ## 
model <- lmList(d ~ p_avg | cusip_id, data = data)
test <- data.frame(coef = coefficients(model),
                   conf = confint(model),
                   p_value = summary(model)$coef[,4,2],
                   r_squared = summary(model)$r.squared)
test$cusip <- row.names(test)

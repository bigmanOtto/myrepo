trace <- TRACE
trades <- aggregate(type~cusip_id+trd_exctn_dt, data = trace, FUN = function(x){NROW(x)})
trades <- trades[with(trades, order(cusip_id, trd_exctn_dt)), ]
names(trades)[3] <- paste("trades")

data <- DAILY_all
data$trades <- trades$trades
data$spread1 <- unlist(kalman)
data <- data[!is.na(data$illiq_mid),]
names(data)[1] <- paste("cusip_id")
data$trd_exctn_dt <- as.Date.character(data$trd_exctn_dt, format = "%Y%m%d")
is_significant <- 0.05

##illiq_mid~vol_tot
model.1 <- lm(illiq_mid ~ vol_tot, data=data)

model.1.data <- data.frame(coef = coefficients(model.1),
                           conf = confint(model.1),
                           p_value = summary(model.1)$coef[2],
                           r_squared = summary(model.1)$r.squared)

##illiq_mid~vol_tot
model.1 <- lm(illiq_mid ~ vol_tot, data=data)

model.1.data <- data.frame(coef = coefficients(model.1),
                           conf = confint(model.1),
                           p_value = summary(model.1)$coef[2,4],
                           r_squared = summary(model.1)$r.squared)
##illiq_mid~p_avg
model.2 <- lm(illiq_mid ~ p_avg, data=data)

model.2.data <- data.frame(coef = coefficients(model.2),
                           conf = confint(model.2),
                           p_value = summary(model.2)$coef[2,4],
                           r_squared = summary(model.2)$r.squared)

##illiq_mid~spread
model.3 <- lm(illiq_mid ~ spread1, data=data)

model.3.data <- data.frame(coef = coefficients(model.3),
                           conf = confint(model.3),
                           p_value = summary(model.3)$coef[2,4],
                           r_squared = summary(model.3)$r.squared)


##illiq_mid~spread+vol_tot
model.4 <- lm(illiq_mid ~ spread1+vol_tot, data=data)

model.4.data <- data.frame(coef = coefficients(model.4),
                           conf = confint(model.4),
                           p_value = summary(model.4)$coef[2,4],
                           r_squared = summary(model.4)$r.squared)

##illiq_mid~spread+log(vol_tot)
model.5 <- lm(illiq_mid ~ spread1+log(vol_tot), data=data)

model.5.data <- data.frame(coef = coefficients(model.5),
                           conf = confint(model.5),
                           p_value = summary(model.5)$coef[2,4],
                           r_squared = summary(model.5)$r.squared)

##illiq_mid~spread+sqrt(vol_tot)
model.6 <- lm(illiq_mid ~ spread1+sqrt(vol_tot), data=data)

model.6.data <- data.frame(coef = coefficients(model.6),
                           conf = confint(model.6),
                           p_value = summary(model.6)$coef[2,4],
                           r_squared = summary(model.6)$r.squared)
##illiq_mid~rating
model.7 <- lm(illiq_mid ~ credit, data=data)

model.7.data <- data.frame(coef = coefficients(model.7),
                           conf = confint(model.7),
                           p_value = summary(model.7)$coef[2,4],
                           r_squared = summary(model.7)$r.squared)
##illiq_mid~rating+sqrt(vol_tot)+spread
model.8 <- lm(illiq_mid ~ credit+sqrt(vol_tot)+spread, data=data)

model.8.data <- data.frame(coef = coefficients(model.8),
                           conf = confint(model.8),
                           p_value = summary(model.8)$coef[2,4],
                           r_squared = summary(model.8)$r.squared)

##illiq_mid~rating+log(vol_tot)+spread
model.9 <- lm(illiq_mid ~ credit+log(vol_tot)+spread, data=data)

model.9.data <- data.frame(coef = coefficients(model.9),
                           conf = confint(model.9),
                           p_value = summary(model.9)$coef[2,4],
                           r_squared = summary(model.9)$r.squared)




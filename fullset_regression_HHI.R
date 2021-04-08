trace <- TRACE
trades <- aggregate(type~cusip_id+trd_exctn_dt, data = trace, FUN = function(x){NROW(x)})
trades <- trades[with(trades, order(cusip_id, trd_exctn_dt)), ]
names(trades)[3] <- paste("trades")

data <- DAILY_all
data$trades <- trades$trades
data$spread1 <- unlist(kalman)



data <- data[!is.na(data$HHI),]
data <- data[data$HHI<10.488,]
dataHY <- data[data$type=="HY",]
dataIG <- data[data$type=="IG",]

names(data)[1] <- paste("cusip_id")
data$trd_exctn_dt <- as.Date.character(data$trd_exctn_dt, format = "%Y%m%d")
is_significant <- 0.05


##HHI~vol_tot
model.1 <- lm(HHI ~ vol_tot, data=subdata_HY1)

model.1.data <- data.frame(coef = coefficients(model.1),
                           conf = confint(model.1),
                           p_value = summary(model.1)$coef[2,4],
                           r_squared = summary(model.1)$r.squared)
res.1<-residuals(model.1)
hist(res.1)
qqnorm(res.1)
qqline(res.1)
##HHI~p_avg
model.2 <- lm(HHI ~ p_avg, data=data)

model.2.data <- data.frame(coef = coefficients(model.2),
                           conf = confint(model.2),
                           p_value = summary(model.2)$coef[2,4],
                           r_squared = summary(model.2)$r.squared)
res.2<-residuals(model.2)
hist(res.2)
qqnorm(res.2)
qqline(res.2)
##HHI~spread
model.3 <- lm(HHI ~ spread1, data=subdata_HY3)

model.3.data <- data.frame(coef = coefficients(model.3),
                           conf = confint(model.3),
                           p_value = summary(model.3)$coef[2,4],
                           r_squared = summary(model.3)$r.squared)
res.3<-residuals(model.3)
hist(res.3)
qqnorm(res.3)
qqline(res.3)

##HHI~spread+vol_tot
model.4 <- lm(HHI ~ spread1+vol_tot, data=data)

model.4_sub <- lm(HHI ~ spread1+vol_tot, data=subdata_HY1)

model.4.data <- data.frame(coef = coefficients(model.4),
                           conf = confint(model.4),
                           p_value = summary(model.4)$coef[2,4],
                           r_squared = summary(model.4)$r.squared)
res.4<-residuals(model.4_sub)
hist(res.4)
qqnorm(res.4)
qqline(res.4)

##HHI~spread+log(vol_tot)
model.5 <- lm(HHI ~ spread1+log(vol_tot), data=data)

model.5_sub <- lm(HHI ~ spread1+log(vol_tot), data=subdata_HY1)

model.5.data <- data.frame(coef = coefficients(model.5),
                           conf = confint(model.5),
                           p_value = summary(model.5)$coef[2,4],
                           r_squared = summary(model.5)$r.squared)
res.5<-residuals(model.5_sub)
hist(res.5)
qqnorm(res.5)
qqline(res.5)

##HHI~spread+sqrt(vol_tot)
model.6 <- lm(HHI ~ spread1+sqrt(vol_tot), data=data)

model.6_sub <- lm(HHI ~ spread1+sqrt(vol_tot), data=subdata_HY3)

model.6.data <- data.frame(coef = coefficients(model.6),
                           conf = confint(model.6),
                           p_value = summary(model.6)$coef[2,4],
                           r_squared = summary(model.6)$r.squared)
res.6<-residuals(model.6_sub)
hist(res.6)
qqnorm(res.6)
qqline(res.6)


##HHI~rating
model.7 <- lm(HHI ~ credit, data=data)

model.7.data <- data.frame(coef = coefficients(model.7),
                           conf = confint(model.7),
                           p_value = summary(model.7)$coef[2,4],
                           r_squared = summary(model.7)$r.squared)
res.7<-residuals(model.7)
hist(res.7)
qqnorm(res.7)
qqline(res.7)

##HHI~rating+sqrt(vol_tot)+spread
model.8 <- lm(HHI ~ credit+sqrt(vol_tot)+spread, data=data)

model.8.data <- data.frame(coef = coefficients(model.8),
                           conf = confint(model.8),
                           p_value = summary(model.8)$coef[2,4],
                           r_squared = summary(model.8)$r.squared)

res.8<-residuals(model.8)
hist(res.8)
qqnorm(res.8)
qqline(res.8)

##HHI~rating+log(vol_tot)+spread
model.9 <- lm(HHI ~ credit+log(vol_tot)+spread, data=data)

model.9.data <- data.frame(coef = coefficients(model.9),
                           conf = confint(model.9),
                           p_value = summary(model.9)$coef[2,4],
                           r_squared = summary(model.9)$r.squared)
res.9<-residuals(model.9)
hist(res.9)
qqnorm(res.9)
qqline(res.9)

##HHI~spread+sqrt(vol_tot)+log(trades)
model.10 <- lm(HHI ~ spread1+sqrt(vol_tot)+log(trades), data=data)

model.10_sub <- lm(HHI ~ spread1+sqrt(vol_tot)+log(trades), data=subdata_HY3)

model.10.data <- data.frame(coef = coefficients(model.10),
                            conf = confint(model.10),
                            p_value = summary(model.10)$coef[2,4],
                            r_squared = summary(model.10)$r.squared)
res.10<-residuals(model.10_sub)
hist(res.10)
qqnorm(res.10)
qqline(res.10)

##HHI~spread+log(vol_tot)+trades
model.11 <- lm(HHI ~ spread1+log(vol_tot)+trades, data=data)

model.11_sub <- lm(HHI ~ spread1+log(vol_tot)+trades, data=subdata_HY1)

model.11.data <- data.frame(coef = coefficients(model.11),
                            conf = confint(model.11),
                            p_value = summary(model.11)$coef[2,4],
                            r_squared = summary(model.11)$r.squared)
res.11<-residuals(model.11_sub)
hist(res.11)
qqnorm(res.11)
qqline(res.11)

##HHI~spread+log(vol_tot)+log(trades)
model.12 <- lm(HHI ~ spread1+log(vol_tot)+log(trades), data=data)

model.12_sub <- lm(HHI ~ spread1+log(vol_tot)+log(trades), data=subdata_HY2)

model.12.data <- data.frame(coef = coefficients(model.12),
                            conf = confint(model.12),
                            p_value = summary(model.12)$coef[2,4],
                            r_squared = summary(model.12)$r.squared)
res.12<-residuals(model.12_sub)
hist(res.12)
qqnorm(res.12)
qqline(res.12)

##HHI~spread+log(vol_tot)+sqrt(trades)
model.13 <- lm(HHI ~ spread1+sqrt(vol_tot), data=data)

model.13_sub <- lm(HHI ~ spread1+sqrt(vol_tot), data=subdata_HY2)

model.13.data <- data.frame(coef = coefficients(model.13),
                            conf = confint(model.13),
                            p_value = summary(model.13)$coef[2,4],
                            r_squared = summary(model.13)$r.squared)
res.13<-residuals(model.13_sub)
hist(res.13)
qqnorm(res.13)
qqline(res.13)

##HHI~type
model.14 <- lm(HHI ~ type, data=data)

model.14.data <- data.frame(coef = coefficients(model.14),
                           conf = confint(model.14),
                           p_value = summary(model.14)$coef[2,4],
                           r_squared = summary(model.14)$r.squared)
res.14<-residuals(model.14)
hist(res.14)
qqnorm(res.14)
qqline(res.14)

##HHI~type+log(vol_tot)+spread
model.15_HY <- lm(HHI ~ log(vol_tot)+spread, data=dataHY)

model.15_IG <- lm(HHI ~ log(vol_tot)+spread, data=dataIG)

model.15 <- lm(HHI ~ type + log(vol_tot)+spread, data=data)

model.15.data <- data.frame(coef = coefficients(model.15),
                           conf = confint(model.15),
                           p_value = summary(model.15)$coef[2,4],
                           r_squared = summary(model.15)$r.squared)
res.15_HY<-residuals(model.15_HY)
res.15_IG<-residuals(model.15_IG)

hist(res.15_HY)
hist(res.15_IG)

qqnorm(res.15_HY)
qqline(res.15_HY)

qqnorm(res.15_IG)
qqline(res.15_IG)


##HHI~spread+log(vol_tot)+log(trades)+russel
model.16 <- lm(HHI ~ spread1+log(vol_tot)+log(trades)+russell_logreturn, data=data)



model.16.data <- data.frame(coef = coefficients(model.16),
                            conf = confint(model.16),
                            p_value = summary(model.16)$coef[2,4],
                            r_squared = summary(model.16)$r.squared)
res.16<-residuals(model.16)
hist(res.16)
qqnorm(res.16)
qqline(res.16)

##HHI~rating+spread+log(vol_tot)+log(trades)
model.17 <- lm(HHI ~ credit+spread+log(vol_tot)+log(trades), data=data)


model.17.data <- data.frame(coef = coefficients(model.17),
                            conf = confint(model.17),
                            p_value = summary(model.17)$coef[2,4],
                            r_squared = summary(model.17)$r.squared)
res.17<-residuals(model.17)

hist(res.17)
qqnorm(res.17)
qqline(res.17)


##HHI~type+spread+log(vol_tot)+log(trades)
model.18 <- lm(HHI ~ type+spread+log(vol_tot)+trades, data=data)


model.18.data <- data.frame(coef = coefficients(model.18),
                            conf = confint(model.18),
                            p_value = summary(model.18)$coef[2,4],
                            r_squared = summary(model.18)$r.squared)
res.18<-residuals(model.18)

hist(res.18)
qqnorm(res.18)
qqline(res.18)

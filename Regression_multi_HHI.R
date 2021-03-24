library(lme4)

trace <- TRACE
trades <- aggregate(type~cusip_id+trd_exctn_dt, data = trace, FUN = function(x){NROW(x)})
trades <- trades[with(trades, order(cusip_id, trd_exctn_dt)), ]
names(trades)[3] <- paste("trades")

data <- DAILY_all
data <- data[!is.na(data$HHI),]
data$trades <- trades$trades
names(data)[1] <- paste("cusip_id")
data$trd_exctn_dt <- as.Date.character(data$trd_exctn_dt, format = "%Y%m%d")
is_significant <- 0.05

HHI <- DAILY_all$HHI


### HHI liquidity ###

## HHI ~ p_avg ## 
model.1 <- lmList(HHI ~ p_avg | cusip_id, data = data)
model.1.data <- data.frame(coef = coefficients(model.1),
                           conf = confint(model.1),
                           p_value = summary(model.1)$coef[,4,2],
                           r_squared = summary(model.1)$r.squared)
model.1.data$cusip <- row.names(model.1.data)

model.1.hist <- hist(model.1.data$coef.p_avg, xlim = c(-6*10^-9, 1*10^-9))
model.1.significant <- data.frame(significant = sum(model.1.data$p_value < is_significant),
                                  non_significant = sum(model.1.data$p_value >= is_significant))

model.1.data$dnorm <- dnorm(model.1.data$coef.p_avg, mean = mean(model.1.data$coef.p_avg), sd = sd(model.1.data$coef.p_avg))
ggplot(data = model.1.data, aes(x = coef.p_avg, y = dnorm)) +
  geom_point()

## HHI ~ vol_tot ## 
model.2 <- lmList(HHI ~ vol_tot | cusip_id, data = data)
model.2.data <- data.frame(coef = coefficients(model.2),
                           conf = confint(model.2),
                           p_value = summary(model.2)$coef[,4,2],
                           r_squared = summary(model.2)$r.squared)
model.2.data$cusip <- row.names(model.2.data)

model.2.hist <- hist(model.2.data$coef.vol_tot)
model.2.significant <- data.frame(significant = sum(model.2.data$p_value < is_significant),
                                  non_significant = sum(model.2.data$p_value >= is_significant))

model.2.data$dnorm <- dnorm(model.2.data$coef.vol_tot, mean = mean(model.2.data$coef.vol_tot), sd = sd(model.2.data$coef.vol_tot))
ggplot(data = model.2.data, aes(x = coef.vol_tot, y = dnorm)) +
  geom_point()


## HHI ~ spread ##  Fungerar inte eftersom vi inte har all spread data ... 
model.3 <- lmList(HHI ~ spread | cusip_id, data = data)
model.3.data <- data.frame(coef = coefficients(model.3),
                           conf = confint(model.3),
                           p_value = summary(model.3)$coef[,4,2],
                           r_squared = summary(model.3)$r.squared)
model.3.data$cusip <- row.names(model.3.data)

model.3.hist <- hist(model.3.data$coef.vol_tot)
model.3.significant <- data.frame(significant = sum(model.3.data$p_value < is_significant),
                                  non_significant = sum(model.3.data$p_value >= is_significant))

model.3.data$dnorm <- dnorm(model.3.data$coef.spread, mean = mean(model.3.data$coef.spread), sd = sd(model.3.data$coef.spread))
ggplot(data = model.3.data, aes(x = coef.spread, y = dnorm)) +
  geom_point()

##HHI~log(vol_tot)

model.4 <- lmList(HHI ~ log(vol_tot) | cusip_id, data = data)
model.4.data <- data.frame(coef = coefficients(model.4),
                           conf = confint(model.4),
                           p_value = summary(model.4)$coef[,4,2],
                           r_squared = summary(model.4)$r.squared)
model.4.data$cusip <- row.names(model.4.data)

model.4.hist <- hist(model.4.data$log(coef.log.vol_tot))
model.4.significant <- data.frame(significant = sum(model.4.data$p_value < is_significant),
                                  non_significant = sum(model.4.data$p_value >= is_significant))

model.4.data$dnorm <- dnorm(model.4.data$coef.log.vol_tot, mean = mean(model.4.data$coef.log.vol_tot), sd = sd(model.4.data$coef.vol_tot))
ggplot(data = model.4.data, aes(x = coef.vol_tot, y = dnorm)) +
  geom_point()

##HHI~trades##
model.5 <- lmList(HHI ~ trades | cusip_id, data = data)
model.5.data <- data.frame(coef = coefficients(model.5),
                           conf = confint(model.5),
                           p_value = summary(model.5)$coef[,4,2],
                           r_squared = summary(model.5)$r.squared)
model.5.data$cusip <- row.names(model.5.data)

model.5.hist <- hist(model.5.data$coef.trades)
model.5.significant <- data.frame(significant = sum(model.5.data$p_value < is_significant),
                                  non_significant = sum(model.5.data$p_value >= is_significant))

model.5.data$dnorm <- dnorm(model.5.data$coef.vol_tot, mean = mean(model.5.data$coef.vol_tot), sd = sd(model.5.data$coef.vol_tot))
ggplot(data = model.5.data, aes(x = coef.vol_tot, y = dnorm)) +
  geom_point()

##HHI~sqrt(vol_tot)##
model.6 <- lmList(HHI ~ sqrt(vol_tot) | cusip_id, data = data)
model.6.data <- data.frame(coef = coefficients(model.6),
                           conf = confint(model.6),
                           p_value = summary(model.6)$coef[,4,2],
                           r_squared = summary(model.6)$r.squared)
model.6.data$cusip <- row.names(model.6.data)

model.6.hist <- hist(model.6.data$coef.vol_tot)
model.6.significant <- data.frame(significant = sum(model.6.data$p_value < is_significant),
                                  non_significant = sum(model.6.data$p_value >= is_significant))

model.6.data$dnorm <- dnorm(model.6.data$coef.vol_tot, mean = mean(model.6.data$coef.vol_tot), sd = sd(model.6.data$coef.vol_tot))
ggplot(data = model.6.data, aes(x = coef.vol_tot, y = dnorm)) +
  geom_point()


##HHI~p_avg+vol_tot##
model.7 <- lmList(HHI ~ vol_tot + p_avg | cusip_id, data = data)
model.7.data <- data.frame(coef = coefficients(model.7),
                           conf = confint(model.7),
                           p_value = summary(model.7)$coef[,4,2],
                           r_squared = summary(model.7)$r.squared)
model.7.data$cusip <- row.names(model.7.data)

model.7.hist <- hist(model.7.data$coef.vol_tot)
model.7.significant <- data.frame(significant = sum(model.7.data$p_value < is_significant),
                                  non_significant = sum(model.7.data$p_value >= is_significant))

model.7.data$dnorm <- dnorm(model.7.data$coef.vol_tot, mean = mean(model.7.data$coef.vol_tot), sd = sd(model.7.data$coef.vol_tot))
ggplot(data = model.7.data, aes(x = coef.vol_tot, y = dnorm)) +
  geom_point()

##HHI~p_avg+sqrt(vol_tot)##

model.9 <- lmList(HHI ~ sqrt(vol_tot) + p_avg | cusip_id, data = data)
model.9.data <- data.frame(coef = coefficients(model.9),
                           conf = confint(model.9),
                           p_value = summary(model.9)$coef[,4,2],
                           r_squared = summary(model.9)$r.squared)
model.9.data$cusip <- row.names(model.9.data)

model.9.hist <- hist(model.9.data$coef.sqrt.vol_tot)
model.9.significant <- data.frame(significant = sum(model.9.data$p_value < is_significant),
                                  non_significant = sum(model.9.data$p_value >= is_significant))

model.9.data$dnorm <- dnorm(model.9.data$coef.vol_tot, mean = mean(model.9.data$coef.sqrt.vol_tot), sd = sd(model.9.data$coef.vol_tot))
ggplot(data = model.9.data, aes(x = coef.vol_tot, y = dnorm)) +
  geom_point()

##HHI~sqrt(p_avg)+sqrt(vol_tot)##

model.10 <- lmList(HHI ~ sqrt(vol_tot) + sqrt(p_avg) | cusip_id, data = data)
model.10.data <- data.frame(coef = coefficients(model.10),
                            conf = confint(model.10),
                            p_value = summary(model.10)$coef[,4,2],
                            r_squared = summary(model.10)$r.squared)
model.10.data$cusip <- row.names(model.10.data)

model.10.hist <- hist(model.10.data$coef.sqrt.vol_tot)
model.10.significant <- data.frame(significant = sum(model.10.data$p_value < is_significant),
                                   non_significant = sum(model.10.data$p_value >= is_significant))

model.10.data$dnorm <- dnorm(model.10.data$coef.vol_tot, mean = mean(model.10.data$coef.sqrt.vol_tot), sd = sd(model.10.data$coef.vol_tot))
ggplot(data = model.10.data, aes(x = coef.vol_tot, y = dnorm)) +
  geom_point()

##HHI~p_avg+log(vol_tot)##

model.11 <- lmList(HHI ~ log(vol_tot) + p_avg | cusip_id, data = data)
model.11.data <- data.frame(coef = coefficients(model.11),
                            conf = confint(model.11),
                            p_value = summary(model.11)$coef[,4,2],
                            r_squared = summary(model.11)$r.squared)
model.11.data$cusip <- row.names(model.11.data)

model.11.hist <- hist(model.11.data$coef.log.vol_tot)
model.11.significant <- data.frame(significant = sum(model.11.data$p_value < is_significant),
                                   non_significant = sum(model.11.data$p_value >= is_significant))

model.11.data$dnorm <- dnorm(model.11.data$coef.log.vol_tot, mean = mean(model.11.data$coef.log.vol_tot), sd = sd(model.11.data$coef.vol_tot))
ggplot(data = model.11.data, aes(x = coef.vol_tot, y = dnorm)) +
  geom_point()

##HHI~log(p_avg)+log(vol_tot)##

model.12 <- lmList(HHI ~ log(vol_tot) + log(p_avg) | cusip_id, data = data)
model.12.data <- data.frame(coef = coefficients(model.12),
                            conf = confint(model.12),
                            p_value = summary(model.12)$coef[,4,2],
                            r_squared = summary(model.12)$r.squared)
model.12.data$cusip <- row.names(model.12.data)

model.12.hist <- hist(model.12.data$coef.log.vol_tot)
model.12.significant <- data.frame(significant = sum(model.12.data$p_value < is_significant),
                                   non_significant = sum(model.12.data$p_value >= is_significant))

model.12.data$dnorm <- dnorm(model.12.data$coef.log.vol_tot, mean = mean(model.12.data$coef.log.vol_tot), sd = sd(model.12.data$coef.vol_tot))
ggplot(data = model.12.data, aes(x = coef.vol_tot, y = dnorm)) +
  geom_point()

##HHI~vol_tot by rating##
model.13 <- lmList(HHI ~ vol_tot | credit, data = data)
model.13.data <- data.frame(coef = coefficients(model.13),
                           conf = confint(model.13),
                           p_value = summary(model.13)$coef[,4,2],
                           r_squared = summary(model.13)$r.squared)
model.13.data$rating <- row.names(model.13.data)

model.13.hist <- hist(model.13.data$coef.vol_tot)
model.13.significant <- data.frame(significant = sum(model.13.data$p_value < is_significant),
                                  non_significant = sum(model.13.data$p_value >= is_significant))

model.13.data$dnorm <- dnorm(model.13.data$coef.vol_tot, mean = mean(model.13.data$coef.vol_tot), sd = sd(model.13.data$coef.vol_tot))
ggplot(data = model.13.data, aes(x = coef.vol_tot, y = dnorm)) +
  geom_point()

##HHI~log(vol_tot) by rating##
model.14 <- lmList(HHI ~ log(vol_tot) | credit, data = data)
model.14.data <- data.frame(coef = coefficients(model.14),
                            conf = confint(model.14),
                            p_value = summary(model.14)$coef[,4,2],
                            r_squared = summary(model.14)$r.squared)
model.14.data$rating <- row.names(model.14.data)

model.14.hist <- hist(model.14.data$coef.log.vol_tot)
model.14.significant <- data.frame(significant = sum(model.14.data$p_value < is_significant),
                                   non_significant = sum(model.14.data$p_value >= is_significant))

model.14.data$dnorm <- dnorm(model.14.data$coef.log.vol_tot, mean = mean(model.14.data$coef.log.vol_tot), sd = sd(model.14.data$coef.vol_tot))
ggplot(data = model.14.data, aes(x = coef.vol_tot, y = dnorm)) +
  geom_point()

##HHI~sqrt(vol_tot) by rating##
model.15 <- lmList(HHI ~ sqrt(vol_tot) | credit, data = data)
model.15.data <- data.frame(coef = coefficients(model.15),
                            conf = confint(model.15),
                            p_value = summary(model.15)$coef[,4,2],
                            r_squared = summary(model.15)$r.squared)
model.15.data$rating <- row.names(model.15.data)

model.15.hist <- hist(model.15.data$coef.sqrt.vol_tot)
model.15.significant <- data.frame(significant = sum(model.15.data$p_value < is_significant),
                                   non_significant = sum(model.15.data$p_value >= is_significant))

model.15.data$dnorm <- dnorm(model.15.data$coef.sqrt.vol_tot, mean = mean(model.15.data$coef.vol_tot), sd = sd(model.15.data$coef.sqrt.vol_tot))
ggplot(data = model.15.data, aes(x = coef.vol_tot, y = dnorm)) +
  geom_point()

##HHI~p_avg by rating##
model.16 <- lmList(HHI ~ p_avg | credit, data = data)
model.16.data <- data.frame(coef = coefficients(model.16),
                            conf = confint(model.16),
                            p_value = summary(model.16)$coef[,4,2],
                            r_squared = summary(model.16)$r.squared)
model.16.data$rating <- row.names(model.16.data)

model.16.hist <- hist(model.16.data$coef.p_avg)
model.16.significant <- data.frame(significant = sum(model.16.data$p_value < is_significant),
                                   non_significant = sum(model.16.data$p_value >= is_significant))

model.16.data$dnorm <- dnorm(model.16.data$coef.p_avg, mean = mean(model.16.data$coef.p_avg), sd = sd(model.16.data$coef.p_avg))
ggplot(data = model.16.data, aes(x = coef.vol_tot, y = dnorm)) +
  geom_point()

##HHI~log(p_avg) by rating##
model.17 <- lmList(HHI ~ log(p_avg) | credit, data = data)
model.17.data <- data.frame(coef = coefficients(model.17),
                            conf = confint(model.17),
                            p_value = summary(model.17)$coef[,4,2],
                            r_squared = summary(model.17)$r.squared)
model.17.data$rating <- row.names(model.17.data)

model.17.hist <- hist(model.17.data$coef.log.p_avg)
model.17.significant <- data.frame(significant = sum(model.17.data$p_value < is_significant),
                                   non_significant = sum(model.17.data$p_value >= is_significant))

model.17.data$dnorm <- dnorm(model.17.data$coef.log.p_avg, mean = mean(model.17.data$coef.log.p_avg), sd = sd(model.17.data$coef.log.p_avg))
ggplot(data = model.17.data, aes(x = coef.vol_tot, y = dnorm)) +
  geom_point()

##HHI~sqrt(p_avg) by rating##
model.18 <- lmList(HHI ~ sqrt(vol_tot) | credit, data = data)
model.18.data <- data.frame(coef = coefficients(model.18),
                            conf = confint(model.18),
                            p_value = summary(model.18)$coef[,4,2],
                            r_squared = summary(model.18)$r.squared)
model.18.data$rating <- row.names(model.18.data)

model.18.hist <- hist(model.18.data$coef.sqrt.p_avg)
model.18.significant <- data.frame(significant = sum(model.18.data$p_value < is_significant),
                                   non_significant = sum(model.18.data$p_value >= is_significant))

model.18.data$dnorm <- dnorm(model.18.data$coef.sqrt.p_avg, mean = mean(model.18.data$coef.p_avg), sd = sd(model.18.data$coef.log.p_avg))
ggplot(data = model.18.data, aes(x = coef.vol_tot, y = dnorm)) +
  geom_point()


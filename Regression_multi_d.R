library(lme4)

trace <- TRACE
trades <- aggregate(type~cusip_id+trd_exctn_dt, data = trace, FUN = function(x){NROW(x)})
trades <- trades[with(trades, order(cusip_id, trd_exctn_dt)), ]
names(trades)[3] <- paste("trades")

data <- DAILY_all
data$trades <- trades$trades
names(data)[1] <- paste("cusip_id")
data$trd_exctn_dt <- as.Date.character(data$trd_exctn_dt, format = "%Y%m%d")
is_significant <- 0.05

### d liquidity ###

## d ~ p_avg ## 
model.1 <- lmList(d ~ p_avg | cusip_id, data = data)
model.1.data <- data.frame(coef = coefficients(model.1),
                           conf = confint(model.1),
                           p_value = summary(model.1)$coef[,4,2],
                           r_squared = summary(model.1)$r.squared)
model.1.data$cusip <- row.names(model.1.data)

model.1.hist <- hist(model.1.data$coef.p_avg)
model.1.significant <- data.frame(significant = sum(model.1.data$p_value < is_significant),
                                  non_significant = sum(model.1.data$p_value >= is_significant))

model.1.data$dnorm <- dnorm(model.1.data$coef.p_avg, mean = mean(model.1.data$coef.p_avg), sd = sd(model.1.data$coef.p_avg))
ggplot(data = model.1.data, aes(x = coef.p_avg, y = dnorm)) +
  geom_point()


## d ~ vol_tot ## 
model.2 <- lmList(d ~ vol_tot | cusip_id, data = data)
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


## d ~ spread ##  Fungerar inte eftersom vi inte har all spread data ... 
model.3 <- lmList(d ~ spread | cusip_id, data = data)
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

##d~log(vol_tot)

model.4 <- lmList(d ~ log(vol_tot) | cusip_id, data = data)
model.4.data <- data.frame(coef = coefficients(model.4),
                           conf = confint(model.4),
                           p_value = summary(model.4)$coef[,4,2],
                           r_squared = summary(model.4)$r.squared)
model.4.data$cusip <- row.names(model.4.data)

model.4.hist <- hist(model.4.data$log(coef.vol_tot))
model.4.significant <- data.frame(significant = sum(model.4.data$p_value < is_significant),
                                  non_significant = sum(model.4.data$p_value >= is_significant))

model.4.data$dnorm <- dnorm(model.4.data$coef.vol_tot, mean = mean(model.4.data$coef.vol_tot), sd = sd(model.4.data$coef.vol_tot))
ggplot(data = model.4.data, aes(x = coef.vol_tot, y = dnorm)) +
  geom_point()

##d~trades##
model.5 <- lmList(d ~ trades | cusip_id, data = data)
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

##d~sqrt(vol_tot)##
model.6 <- lmList(d ~ sqrt(vol_tot) | cusip_id, data = data)
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

##d~p_avg+vol_tot##
model.7 <- lmList(d ~ vol_tot + p_avg | cusip_id, data = data)
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

##d~rating## hur gör man det här? byta rating mot integer

model.8 <- lmList(d ~ credit | cusip_id, data = data)
model.8.data <- data.frame(coef = coefficients(model.8),
                           conf = confint(model.8),
                           p_value = summary(model.8)$coef[,4,2],
                           r_squared = summary(model.8)$r.squared)
model.8.data$cusip <- row.names(model.8.data)

model.8.hist <- hist(model.8.data$coef.credit)
model.8.significant <- data.frame(significant = sum(model.8.data$p_value < is_significant),
                                  non_significant = sum(model.8.data$p_value >= is_significant))

model.8.data$dnorm <- dnorm(model.8.data$coef.vol_tot, mean = mean(model.8.data$coef.vol_tot), sd = sd(model.8.data$coef.vol_tot))
ggplot(data = model.8.data, aes(x = coef.vol_tot, y = dnorm)) +
  geom_point()

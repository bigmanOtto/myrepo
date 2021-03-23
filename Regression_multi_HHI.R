library(lme4)

trace <- read.csv("TRACE.csv")
trades <- aggregate(type~cusip_id+trd_exctn_dt, data = trace, FUN = function(x){NROW(x)})
trades <- trades[with(trades, order(cusip_id, trd_exctn_dt)), ]
names(trades)[3] <- paste("trades")

data <- read.csv("DAILY_all.csv")
data$trades <- trades$trades
names(data)[1] <- paste("cusip_id")
data$trd_exctn_dt <- as.Date.character(data$trd_exctn_dt, format = "%Y%m%d")
is_significant <- 0.05


### HHI liquidity ###

## HHI ~ p_avg ##

model.1 <- lmList(HHI ~ p_avg | cusip_id, data = data)
model.1.data <- data.frame(coef = coefficients(model.1),
                           conf = confint(model.1),
                           p_value = summary(model.1)$coef[,4,2],
                           r_squared = summary(model.1)$r.squared)
model.1.data$cusip <- row.names(model.1.data)
model.1.data <- model.1.data[!is.na(model.1.data$coef.p_avg),]

model.1.hist <- hist(model.1.data$coef.p_avg)
model.1.significant <- data.frame(significant = sum(model.1.data$p_value < is_significant),
                                  non_significant = sum(model.1.data$p_value >= is_significant))

model.1.data$dnorm <- dnorm(model.1.data$coef.p_avg, mean = mean(model.1.data$coef.p_avg), sd = sd(model.1.data$coef.p_avg))
ggplot(data = model.1.data, aes(x = coef.p_avg, y = dnorm)) +
  geom_point(data = model.1.data)


## HHI ~ vol_tot ##

model.2 <- lmList(HHI ~ vol_tot | cusip_id, data = data)
model.2.data <- data.frame(coef = coefficients(model.2),
                           conf = confint(model.2),
                           p_value = summary(model.2)$coef[,4,2],
                           r_squared = summary(model.2)$r.squared)
model.2.data$cusip <- row.names(model.2.data)
model.2.data <- model.2.data[!is.na(model.2.data$coef.vol_tot),]

model.2.hist <- hist(model.2.data$coef.vol_tot)
model.2.significant <- data.frame(significant = sum(model.2.data$p_value < is_significant),
                                  non_significant = sum(model.2.data$p_value >= is_significant))

model.2.data$dnorm <- dnorm(model.2.data$coef.vol_tot, mean = mean(model.2.data$coef.vol_tot), sd = sd(model.2.data$coef.vol_tot))
ggplot(data = model.2.data, aes(x = coef.vol_tot, y = dnorm)) +
  geom_point(data = model.2.data)


## HHI ~ vol_tot ##

model.3 <- lmList(HHI ~ spread | cusip_id, data = data)
model.3.data <- data.frame(coef = coefficients(model.3),
                           conf = confint(model.3),
                           p_value = summary(model.3)$coef[,4,2],
                           r_squared = summary(model.3)$r.squared)
model.3.data$cusip <- row.names(model.3.data)
model.3.data <- model.3.data[!is.na(model.3.data$coef.spread),]

model.3.hist <- hist(model.3.data$coef.spread)
model.3.significant <- data.frame(significant = sum(model.3.data$p_value < is_significant),
                                  non_significant = sum(model.3.data$p_value >= is_significant))

model.3.data$dnorm <- dnorm(model.3.data$coef.spread, mean = mean(model.3.data$coef.spread), sd = sd(model.3.data$coef.spread))
ggplot(data = model.3.data, aes(x = coef.spread, y = dnorm)) +
  geom_point(data = model.3.data)


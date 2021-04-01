library(lme4)

trace <- TRACE
trades <- aggregate(type~cusip_id+trd_exctn_dt, data = trace, FUN = function(x){NROW(x)})
trades <- trades[with(trades, order(cusip_id, trd_exctn_dt)), ]
names(trades)[3] <- paste("trades")

data <- DAILY_all
data$trades <- trades$trades
data$spread1 <- unlist(kalman)
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
model.3 <- lmList(d ~ spread1 | cusip_id, data = data)
model.3.data <- data.frame(coef = coefficients(model.3),
                           conf = confint(model.3),
                           p_value = summary(model.3)$coef[,4,2],
                           r_squared = summary(model.3)$r.squared)
model.3.data$cusip <- row.names(model.3.data)


model.3.hist <- hist(model.3.data$coef.spread1)
model.3.significant <- data.frame(significant = sum(model.3.data$p_value < is_significant),
                                  non_significant = sum(model.3.data$p_value >= is_significant))

model.3.data$dnorm <- dnorm(model.3.data$coef.spread1, mean = mean(model.3.data$coef.spread1), sd = sd(model.3.data$coef.spread1))
ggplot(data = model.3.data, aes(x = coef.spread1, y = dnorm)) +
  geom_point()


##d~log(vol_tot)

model.4 <- lmList(d ~ log(vol_tot) | cusip_id, data = data)
model.4.data <- data.frame(coef = coefficients(model.4),
                           conf = confint(model.4),
                           p_value = summary(model.4)$coef[,4,2],
                           r_squared = summary(model.4)$r.squared)
model.4.data$cusip <- row.names(model.4.data)

model.4.hist <- hist(model.4.data$coef.log.vol_tot)
model.4.significant <- data.frame(significant = sum(model.4.data$p_value < is_significant),
                                  non_significant = sum(model.4.data$p_value >= is_significant))

model.4.data$dnorm <- dnorm(model.4.data$coef.log.vol_tot, mean = mean(model.4.data$coef.log.vol_tot), sd = sd(model.4.data$coef.log.vol_tot))
ggplot(data = model.4.data, aes(x = coef.log.vol_tot., y = dnorm)) +
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

model.5.data$dnorm <- dnorm(model.5.data$coef.trades, mean = mean(model.5.data$coef.trades), sd = sd(model.5.data$coef.trades))
                                                                                                    
ggplot(data = model.5.data, aes(x = coef.trades, y = dnorm)) +
  geom_point()

##d~sqrt(vol_tot)##
model.6 <- lmList(d ~ sqrt(vol_tot) | cusip_id, data = data)
model.6.data <- data.frame(coef = coefficients(model.6),
                           conf = confint(model.6),
                           p_value = summary(model.6)$coef[,4,2],
                           r_squared = summary(model.6)$r.squared)
model.6.data$cusip <- row.names(model.6.data)

model.6.hist <- hist(model.6.data$coef.sqrt.vol_tot)
model.6.significant <- data.frame(significant = sum(model.6.data$p_value < is_significant),
                                  non_significant = sum(model.6.data$p_value >= is_significant))

model.6.data$dnorm <- dnorm(model.6.data$coef.sqrt.vol_tot, mean = mean(model.6.data$coef.sqrt.vol_tot), sd = sd(model.6.data$coef.sqrt.vol_tot))
ggplot(data = model.6.data, aes(x = coef.sqrt.vol_tot., y = dnorm)) +
  geom_point()

##d~p_avg+vol_tot##
model.7 <- lmList(d ~ p_avg + vol_tot| cusip_id, data = data)
model.7.data <- data.frame(coef = coefficients(model.7),
                           conf = confint(model.7),
                           p_value = summary(model.7)$coef[,4,2:3],
                           r_squared = summary(model.7)$r.squared)
model.7.data$cusip <- row.names(model.7.data)

model.7.significant <- data.frame(significant.p_avg = sum(model.7.data$p_value.p_avg < is_significant),
                                  non_significant.p_avg = sum(model.7.data$p_value.p_avg >= is_significant),
                                  significant.vol_tot = sum(model.7.data$p_value.vol_tot < is_significant),
                                  non_significant.vol_tot = sum(model.7.data$p_value.vol_tot >= is_significant))


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

##d~p_avg+sqrt(vol_tot)##

model.9 <- lmList(d ~ p_avg + sqrt(vol_tot) | cusip_id, data = data)
model.9.data <- data.frame(coef = coefficients(model.9),
                           conf = confint(model.9),
                           p_value = summary(model.9)$coef[,4,2:3],
                           r_squared = summary(model.9)$r.squared)
model.9.data$cusip <- row.names(model.9.data)

model.9.significant <- data.frame(significant = sum(model.9.data$p_value.p_avg < is_significant),
                                  non_significant = sum(model.9.data$p_value.p_avg >= is_significant),
                                  significant = sum(model.9.data$p_value.sqrt.vol_tot. < is_significant),
                                  non_significant = sum(model.9.data$p_value.sqrt.vol_tot. >= is_significant))

##d~sqrt(p_avg)+sqrt(vol_tot)##

model.10 <- lmList(d ~ sqrt(p_avg) + sqrt(vol_tot)  | cusip_id, data = data)
model.10.data <- data.frame(coef = coefficients(model.10),
                           conf = confint(model.10),
                           p_value = summary(model.10)$coef[,4,2:3],
                           r_squared = summary(model.10)$r.squared)
model.10.data$cusip <- row.names(model.10.data)

model.10.significant <- data.frame(significant.sqrt.p_avg = sum(model.10.data$p_value.sqrt.p_avg. < is_significant),
                                  non_significant.sqrt.p_avg = sum(model.10.data$p_value.sqrt.p_avg. >= is_significant),
                                  significant.sqrt.vol_tot = sum(model.10.data$p_value.sqrt.vol_tot. < is_significant),
                                  non_significant.sqrt.vol_tot = sum(model.10.data$p_value.sqrt.vol_tot. >= is_significant))

##d~p_avg+log(vol_tot)##

model.11 <- lmList(d ~ p_avg + log(vol_tot) | cusip_id, data = data)
model.11.data <- data.frame(coef = coefficients(model.11),
                            conf = confint(model.11),
                            p_value = summary(model.11)$coef[,4,2:3],
                            r_squared = summary(model.11)$r.squared)
model.11.data$cusip <- row.names(model.11.data)

model.11.significant <- data.frame(significant.p_avg = sum(model.11.data$p_value.p_avg < is_significant),
                                   non_significant.p_avg = sum(model.11.data$p_value.p_avg >= is_significant),
                                   significant.log.vol_tot = sum(model.11.data$p_value.log.vol_tot. < is_significant),
                                   non_significant.log.vol_tot = sum(model.11.data$p_value.log.vol_tot. >= is_significant))

##d~log(p_avg)+log(vol_tot)##

model.12 <- lmList(d ~ log(p_avg) + log(vol_tot) | cusip_id, data = data)
model.12.data <- data.frame(coef = coefficients(model.12),
                            conf = confint(model.12),
                            p_value = summary(model.12)$coef[,4,2:3],
                            r_squared = summary(model.12)$r.squared)
model.12.data$cusip <- row.names(model.12.data)

model.12.significant <- data.frame(significant.log.p_avg = sum(model.12.data$p_value.log.p_avg. < is_significant),
                                   non_significant.log.p_avg = sum(model.12.data$p_value.log.p_avg. >= is_significant),
                                   significant.log.vol_tot = sum(model.12.data$p_value.log.vol_tot. < is_significant),
                                   non_significant.log.vol_tot = sum(model.12.data$p_value.log.vol_tot. >= is_significant))



##d~vol_tot by rating##
model.13 <- lmList(d ~ vol_tot | credit, data = data)
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
##d~log(vol_tot) by rating##
model.14 <- lmList(d ~ log(vol_tot) | credit, data = data)
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

##d~sqrt(vol_tot) by rating##
model.15 <- lmList(d ~ sqrt(vol_tot) | credit, data = data)
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

##d~p_avg by rating##
model.16 <- lmList(d ~ p_avg | credit, data = data)
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

##d~log(p_avg) by rating##
model.17 <- lmList(d ~ log(p_avg) | credit, data = data)
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

##d~sqrt(p_avg) by rating##
model.18 <- lmList(d ~ sqrt(vol_tot) | credit, data = data)
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


##d~log(trades)##
model.19 <- lmList(d ~ log(trades) | cusip_id, data = data)
model.19.data <- data.frame(coef = coefficients(model.19),
                            conf = confint(model.19),
                            p_value = summary(model.19)$coef[,4,2],
                            r_squared = summary(model.19)$r.squared)
model.19.data$cusip <- row.names(model.19.data)

model.19.hist <- hist(model.19.data$coef.log.trades)
model.19.significant <- data.frame(significant = sum(model.19.data$p_value < is_significant),
                                   non_significant = sum(model.19.data$p_value >= is_significant))

model.19.data$dnorm <- dnorm(model.19.data$coef.log.trades, mean = mean(model.19.data$coef.log.trades), sd = sd(model.19.data$coef.log.trades))
ggplot(data = model.19.data, aes(x = coef.log.trades., y = dnorm)) +
  geom_point()

##d~sqrt(trades)##
model.20 <- lmList(d ~ sqrt(trades) | cusip_id, data = data)
model.20.data <- data.frame(coef = coefficients(model.20),
                            conf = confint(model.20),
                            p_value = summary(model.20)$coef[,4,2],
                            r_squared = summary(model.20)$r.squared)
model.20.data$cusip <- row.names(model.20.data)

model.20.hist <- hist(model.20.data$coef.sqrt.trades)
model.20.significant <- data.frame(significant = sum(model.20.data$p_value < is_significant),
                                   non_significant = sum(model.20.data$p_value >= is_significant))

model.20.data$dnorm <- dnorm(model.20.data$coef.sqrt.trades, mean = mean(model.20.data$coef.sqrt.trades), sd = sd(model.20.data$coef.sqrt.trades))
ggplot(data = model.20.data, aes(x = coef.sqrt.trades., y = dnorm)) +
  geom_point()

##d~p_avg + log(vol_tot) + log(trades)##
model.21 <- lmList(d ~ p_avg + log(vol_tot) + log(trades) | cusip_id, data = data)
model.21.data <- data.frame(coef = coefficients(model.21),
                            conf = confint(model.21),
                            p_value = summary(model.21)$coef[,4,2:4],
                            r_squared = summary(model.21)$r.squared)
model.21.data$cusip <- row.names(model.21.data)

model.21.significant <- data.frame(significant.p_avg = sum(model.21.data$p_value.p_avg < is_significant),
                                   non_significant.p_avg = sum(model.21.data$p_value.p_avg >= is_significant),
                                   significant.log.vol_tot = sum(model.21.data$p_value.log.vol_tot. < is_significant),
                                   non_significant.log.vol_tot = sum(model.21.data$p_value.log.vol_tot. >= is_significant),
                                   significant.log.trades = sum(model.21.data$p_value.log.trades. < is_significant),
                                   non_significant.log.trades = sum(model.21.data$p_value.log.trades. >= is_significant))

##d~p_avg + log(vol_tot) + trades##
model.22 <- lmList(d ~ p_avg + log(vol_tot) + trades | cusip_id, data = data)
model.22.data <- data.frame(coef = coefficients(model.22),
                            conf = confint(model.22),
                            p_value = summary(model.22)$coef[,4,2:4],
                            r_squared = summary(model.22)$r.squared)
model.22.data$cusip <- row.names(model.22.data)

model.22.significant <- data.frame(significant.p_avg = sum(model.22.data$p_value.p_avg < is_significant),
                                   non_significant.p_avg = sum(model.22.data$p_value.p_avg >= is_significant),
                                   significant.log.vol_tot = sum(model.22.data$p_value.log.vol_tot. < is_significant),
                                   non_significant.log.vol_tot = sum(model.22.data$p_value.log.vol_tot. >= is_significant),
                                   significant.trades = sum(model.22.data$p_value.trades < is_significant),
                                   non_significant.trades = sum(model.22.data$p_value.trades >= is_significant))


##d~ p_avg + log(vol_tot) + sqrt(trades)##
model.23 <- lmList(d ~ p_avg + log(vol_tot) + sqrt(trades)| cusip_id, data = data)
model.23.data <- data.frame(coef = coefficients(model.23),
                            conf = confint(model.23),
                            p_value = summary(model.23)$coef[,4,2:4],
                            r_squared = summary(model.23)$r.squared)
model.23.data$cusip <- row.names(model.23.data)

model.23.significant <- data.frame(significant.p_avg = sum(model.23.data$p_value.p_avg < is_significant),
                                   non_significant.p_avg = sum(model.23.data$p_value.p_avg >= is_significant),
                                   significant.log.vol_tot = sum(model.23.data$p_value.log.vol_tot. < is_significant),
                                   non_significant.log.vol_tot = sum(model.23.data$p_value.log.vol_tot. >= is_significant),
                                   significant.sqrt.trades = sum(model.23.data$p_value.sqrt.trades < is_significant),
                                   non_significant.sqrt.trades = sum(model.23.data$p_value.sqrt.trades >= is_significant))


##d~p_avg+log(vol_tot)+spread##
model.24 <- lmList(d ~ p_avg + log(vol_tot) + spread1 | cusip_id, data = data)
model.24.data <- data.frame(coef = coefficients(model.24),
                            conf = confint(model.24),
                            p_value = summary(model.24)$coef[,4,2:4],
                            r_squared = summary(model.24)$r.squared)
model.24.data$cusip <- row.names(model.24.data)

model.24.significant <- data.frame(significant.p_avg = sum(model.24.data$p_value.p_avg < is_significant),
                                   non_significant.p_avg = sum(model.24.data$p_value.p_avg >= is_significant),
                                   significant.log.vol_tot = sum(model.24.data$p_value.log.vol_tot. < is_significant),
                                   non_significant.log.vol_tot = sum(model.24.data$p_value.log.vol_tot. >= is_significant),
                                   significant.spread = sum(model.24.data$p_value.spread1 < is_significant),
                                   non_significant.spread = sum(model.24.data$p_value.spread1 >= is_significant))


##d~p_avg+log(vol_tot)+sqrt(spread)##

model.25 <- lmList(d ~ p_avg + log(vol_tot) + sqrt(spread1) | cusip_id, data = data)
model.25.data <- data.frame(coef = coefficients(model.25),
                            conf = confint(model.25),
                            p_value = summary(model.25)$coef[,4,2:4],
                            r_squared = summary(model.25)$r.squared)
model.25.data$cusip <- row.names(model.25.data)

model.25.significant <- data.frame(significant.p_avg = sum(model.25.data$p_value.p_avg < is_significant),
                                   non_significant.p_avg = sum(model.25.data$p_value.p_avg >= is_significant),
                                   significant.log.vol_tot = sum(model.25.data$p_value.log.vol_tot. < is_significant),
                                   non_significant.log.vol_tot = sum(model.25.data$p_value.log.vol_tot. >= is_significant),
                                   significant.sqrt.spread = sum(model.25.data$p_value.sqrt.spread1. < is_significant),
                                   non_significant.sqrt.spread = sum(model.25.data$p_value.sqrt.spread1. >= is_significant))


##d~p_avg+log(vol_tot)+spread^2##

model.26 <- lmList(d ~ log(vol_tot) + p_avg + (spread1)^2 | cusip_id, data = data)
model.26.data <- data.frame(coef = coefficients(model.26),
                            conf = confint(model.26),
                            p_value = summary(model.26)$coef[,4,2],
                            r_squared = summary(model.26)$r.squared)
model.26.data$cusip <- row.names(model.26.data)

model.26.hist <- hist(model.26.data$coef.log.vol_tot)
model.26.significant <- data.frame(significant = sum(model.26.data$p_value < is_significant),
                                   non_significant = sum(model.26.data$p_value >= is_significant))

model.26.data$dnorm <- dnorm(model.26.data$coef.log.vol_tot, mean = mean(model.26.data$coef.log.vol_tot), sd = sd(model.26.data$coef.vol_tot))
ggplot(data = model.26.data, aes(x = coef.vol_tot, y = dnorm)) +
  geom_point()







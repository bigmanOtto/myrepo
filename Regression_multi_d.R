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
                           r_squared = summary(model.1)$r.squared,
                           r_squared_adj = matrix(unlist(summary(model.1)$adj.r.squared), nrow=length(summary(model.1)$adj.r.squared), byrow=TRUE))
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
                           r_squared = summary(model.2)$r.squared,
                           r_squared_adj = matrix(unlist(summary(model.2)$adj.r.squared), nrow=length(summary(model.2)$adj.r.squared), byrow=TRUE))
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
                           r_squared = summary(model.3)$r.squared,
                           r_squared_adj = matrix(unlist(summary(model.3)$adj.r.squared), nrow=length(summary(model.3)$adj.r.squared), byrow=TRUE))
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
                           r_squared = summary(model.4)$r.squared,
                           r_squared_adj = matrix(unlist(summary(model.4)$adj.r.squared), nrow=length(summary(model.4)$adj.r.squared), byrow=TRUE))
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
                           r_squared = summary(model.5)$r.squared,
                           r_squared_adj = matrix(unlist(summary(model.5)$adj.r.squared), nrow=length(summary(model.5)$adj.r.squared), byrow=TRUE))
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
                           r_squared = summary(model.6)$r.squared,
                           r_squared_adj = matrix(unlist(summary(model.6)$adj.r.squared), nrow=length(summary(model.6)$adj.r.squared), byrow=TRUE))
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
                           r_squared = summary(model.7)$r.squared,
                           r_squared_adj = matrix(unlist(summary(model.7)$adj.r.squared), nrow=length(summary(model.7)$adj.r.squared), byrow=TRUE))
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
                           r_squared = summary(model.9)$r.squared,
                           r_squared_adj = matrix(unlist(summary(model.9)$adj.r.squared), nrow=length(summary(model.9)$adj.r.squared), byrow=TRUE))
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
                           r_squared = summary(model.10)$r.squared,
                           r_squared_adj = matrix(unlist(summary(model.10)$adj.r.squared), nrow=length(summary(model.10)$adj.r.squared), byrow=TRUE))
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
                            r_squared = summary(model.11)$r.squared,
                            r_squared_adj = matrix(unlist(summary(model.11)$adj.r.squared), nrow=length(summary(model.11)$adj.r.squared), byrow=TRUE))
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
                            r_squared = summary(model.12)$r.squared,
                            r_squared_adj = matrix(unlist(summary(model.12)$adj.r.squared), nrow=length(summary(model.12)$adj.r.squared), byrow=TRUE))
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
                            r_squared = summary(model.19)$r.squared,
                            r_squared_adj = matrix(unlist(summary(model.19)$adj.r.squared), nrow=length(summary(model.19)$adj.r.squared), byrow=TRUE))
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
                            r_squared = summary(model.20)$r.squared,
                            r_squared_adj = matrix(unlist(summary(model.20)$adj.r.squared), nrow=length(summary(model.20)$adj.r.squared), byrow=TRUE))
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
                            r_squared = summary(model.21)$r.squared,
                            r_squared_adj = matrix(unlist(summary(model.21)$adj.r.squared), nrow=length(summary(model.21)$adj.r.squared), byrow=TRUE))
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
                            r_squared = summary(model.22)$r.squared,
                            r_squared_adj = matrix(unlist(summary(model.22)$adj.r.squared), nrow=length(summary(model.22)$adj.r.squared), byrow=TRUE))
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
                            r_squared = summary(model.23)$r.squared,
                            r_squared_adj = matrix(unlist(summary(model.23)$adj.r.squared), nrow=length(summary(model.23)$adj.r.squared), byrow=TRUE))
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
                            r_squared = summary(model.24)$r.squared,
                            r_squared_adj = matrix(unlist(summary(model.24)$adj.r.squared), nrow=length(summary(model.24)$adj.r.squared), byrow=TRUE))
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
                            r_squared = summary(model.25)$r.squared,
                            r_squared_adj = matrix(unlist(summary(model.25)$adj.r.squared), nrow=length(summary(model.25)$adj.r.squared), byrow=TRUE))
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
                            r_squared = summary(model.26)$r.squared,
                            r_squared_adj = matrix(unlist(summary(model.26)$adj.r.squared), nrow=length(summary(model.26)$adj.r.squared), byrow=TRUE))
model.26.data$cusip <- row.names(model.26.data)

model.26.hist <- hist(model.26.data$coef.log.vol_tot)
model.26.significant <- data.frame(significant = sum(model.26.data$p_value < is_significant),
                                   non_significant = sum(model.26.data$p_value >= is_significant))

model.26.data$dnorm <- dnorm(model.26.data$coef.log.vol_tot, mean = mean(model.26.data$coef.log.vol_tot), sd = sd(model.26.data$coef.vol_tot))
ggplot(data = model.26.data, aes(x = coef.vol_tot, y = dnorm)) +
  geom_point()



## d ~ log(p_avg) ## 
model.27 <- lmList(d ~ log(p_avg) | cusip_id, data = data)
model.27.data <- data.frame(coef = coefficients(model.27),
                           conf = confint(model.27),
                           p_value = summary(model.27)$coef[,4,2],
                           r_squared = summary(model.27)$r.squared,
                           r_squared_adj = matrix(unlist(summary(model.27)$adj.r.squared), nrow=length(summary(model.27)$adj.r.squared), byrow=TRUE))
model.27.data$cusip <- row.names(model.27.data)

model.27.hist <- hist(model.27.data$coef.log.p_avg)
model.27.significant <- data.frame(significant = sum(model.27.data$p_value < is_significant),
                                  non_significant = sum(model.27.data$p_value >= is_significant))

model.27.data$dnorm <- dnorm(model.27.data$coef.log.p_avg., mean = mean(model.27.data$coef.log.p_avg.), sd = sd(model.27.data$coef.log.p_avg.))
ggplot(data = model.27.data, aes(x = coef.log.p_avg., y = dnorm)) +
  geom_point()

## d ~ sqrt(p_avg) ## 
model.28 <- lmList(d ~ sqrt(p_avg) | cusip_id, data = data)
model.28.data <- data.frame(coef = coefficients(model.28),
                            conf = confint(model.28),
                            p_value = summary(model.28)$coef[,4,2],
                            r_squared = summary(model.28)$r.squared,
                            r_squared_adj = matrix(unlist(summary(model.28)$adj.r.squared), nrow=length(summary(model.28)$adj.r.squared), byrow=TRUE))
model.28.data$cusip <- row.names(model.28.data)

model.28.hist <- hist(model.28.data$coef.sqrt.p_avg.)
model.28.significant <- data.frame(significant = sum(model.28.data$p_value < is_significant),
                                   non_significant = sum(model.28.data$p_value >= is_significant))

model.28.data$dnorm <- dnorm(model.28.data$coef.sqrt.p_avg., mean = mean(model.28.data$coef.sqrt.p_avg.), sd = sd(model.28.data$coef.sqrt.p_avg.))
ggplot(data = model.28.data, aes(x = coef.sqrt.p_avg., y = dnorm)) +
  geom_point()


## d ~ log(spread) ## 
model.29 <- lmList(d ~ log(spread1) | cusip_id, data = data)
model.29.data <- data.frame(coef = coefficients(model.29),
                            conf = confint(model.29),
                            p_value = summary(model.29)$coef[,4,2],
                            r_squared = summary(model.29)$r.squared,
                            r_squared_adj = matrix(unlist(summary(model.29)$adj.r.squared), nrow=length(summary(model.29)$adj.r.squared), byrow=TRUE))
model.29.data$cusip <- row.names(model.29.data)

model.29.hist <- hist(model.29.data$coef.log.spread1.)
model.29.significant <- data.frame(significant = sum(model.29.data$p_value < is_significant),
                                   non_significant = sum(model.29.data$p_value >= is_significant))

model.29.data$dnorm <- dnorm(model.29.data$coef.log.spread1., mean = mean(model.29.data$coef.log.spread1.), sd = sd(model.29.data$coef.log.spread1.))
ggplot(data = model.29.data, aes(x = coef.log.spread1., y = dnorm)) +
  geom_point()

## d ~ sqrt(spread) ## 
model.30 <- lmList(d ~ sqrt(spread1) | cusip_id, data = data)
model.30.data <- data.frame(coef = coefficients(model.30),
                            conf = confint(model.30),
                            p_value = summary(model.30)$coef[,4,2],
                            r_squared = summary(model.30)$r.squared,
                            r_squared_adj = matrix(unlist(summary(model.30)$adj.r.squared), nrow=length(summary(model.30)$adj.r.squared), byrow=TRUE))
model.30.data$cusip <- row.names(model.30.data)

model.30.hist <- hist(model.30.data$coef.sqrt.spread1.)
model.30.significant <- data.frame(significant = sum(model.30.data$p_value < is_significant),
                                   non_significant = sum(model.30.data$p_value >= is_significant))

model.30.data$dnorm <- dnorm(model.30.data$coef.sqrt.spread1., mean = mean(model.30.data$coef.sqrt.spread1.), sd = sd(model.30.data$coef.sqrt.spread1.))
ggplot(data = model.30.data, aes(x = coef.sqrt.spread1., y = dnorm)) +
  geom_point()


## d ~ spread + vol_tot ## 
model.31 <- lmList(d ~ spread1 + vol_tot | cusip_id, data = data)
model.31.data <- data.frame(coef = coefficients(model.31),
                            conf = confint(model.31),
                            p_value = summary(model.31)$coef[,4,2:3],
                            r_squared = summary(model.31)$r.squared,
                            r_squared_adj = matrix(unlist(summary(model.31)$adj.r.squared), nrow=length(summary(model.31)$adj.r.squared), byrow=TRUE))
model.31.data$cusip <- row.names(model.31.data)

model.31.significant <- data.frame(significant.spread = sum(model.31.data$p_value.spread1 < is_significant),
                                   non_significant.spread = sum(model.31.data$p_value.spread1 >= is_significant),
                                   significant.vol_tot = sum(model.31.data$p_value.vol_tot < is_significant),
                                   non_significant.spread.vol_tot = sum(model.31.data$p_value.vol_tot >= is_significant))

## d ~ spread + log(vol_tot) ## 
model.32 <- lmList(d ~ spread1 + log(vol_tot) | cusip_id, data = data)
model.32.data <- data.frame(coef = coefficients(model.32),
                            conf = confint(model.32),
                            p_value = summary(model.32)$coef[,4,2:3],
                            r_squared = summary(model.32)$r.squared,
                            r_squared_adj = matrix(unlist(summary(model.32)$adj.r.squared), nrow=length(summary(model.32)$adj.r.squared), byrow=TRUE))
model.32.data$cusip <- row.names(model.32.data)

model.32.significant <- data.frame(significant.spread = sum(model.32.data$p_value.spread1 < is_significant),
                                   non_significant.spread = sum(model.32.data$p_value.spread1 >= is_significant),
                                   significant.log.vol_tot = sum(model.32.data$p_value.log.vol_tot. < is_significant),
                                   non_significant.spread.log.vol_tot = sum(model.32.data$p_value.log.vol_tot. >= is_significant))


## d ~ spread + sqrt(vol_tot) ## 
model.33 <- lmList(d ~ spread1 + sqrt(vol_tot) | cusip_id, data = data)
model.33.data <- data.frame(coef = coefficients(model.33),
                            conf = confint(model.33),
                            p_value = summary(model.33)$coef[,4,2:3],
                            r_squared = summary(model.33)$r.squared,
                            r_squared_adj = matrix(unlist(summary(model.33)$adj.r.squared), nrow=length(summary(model.33)$adj.r.squared), byrow=TRUE))
model.33.data$cusip <- row.names(model.33.data)

model.33.significant <- data.frame(significant.spread = sum(model.33.data$p_value.spread1 < is_significant),
                                   non_significant.spread = sum(model.33.data$p_value.spread1 >= is_significant),
                                   significant.sqrt.vol_tot = sum(model.33.data$p_value.sqrt.vol_tot. < is_significant),
                                   non_significant.spread.sqrt.vol_tot = sum(model.33.data$p_value.sqrt.vol_tot. >= is_significant))



## d ~ spread + log(vol_tot) + trades## 
model.34 <- lmList(d ~ spread1 + log(vol_tot) + trades | cusip_id, data = data)
model.34.data <- data.frame(coef = coefficients(model.34),
                            conf = confint(model.34),
                            p_value = summary(model.34)$coef[,4,2:4],
                            r_squared = summary(model.34)$r.squared,
                            r_squared_adj = matrix(unlist(summary(model.34)$adj.r.squared), nrow=length(summary(model.34)$adj.r.squared), byrow=TRUE))
model.34.data$cusip <- row.names(model.34.data)

model.34.significant <- data.frame(significant.spread = sum(model.34.data$p_value.spread1 < is_significant),
                                   non_significant.spread = sum(model.34.data$p_value.spread1 >= is_significant),
                                   significant.log.vol_tot = sum(model.34.data$p_value.log.vol_tot. < is_significant),
                                   non_significant.log.sqrt.vol_tot = sum(model.34.data$p_value.log.vol_tot. >= is_significant),
                                   significant.trades = sum(model.34.data$p_value.trades < is_significant),
                                   non_significant.trades = sum(model.34.data$p_value.trades >= is_significant))


## d ~ spread + log(vol_tot) + log(trades)## 
model.35 <- lmList(d ~ spread1 + log(vol_tot) + log(trades) | cusip_id, data = data)
model.35.data <- data.frame(coef = coefficients(model.35),
                            conf = confint(model.35),
                            p_value = summary(model.35)$coef[,4,2:4],
                            r_squared = summary(model.35)$r.squared,
                            r_squared_adj = matrix(unlist(summary(model.35)$adj.r.squared), nrow=length(summary(model.35)$adj.r.squared), byrow=TRUE))
model.35.data$cusip <- row.names(model.35.data)

model.35.significant <- data.frame(significant.spread = sum(model.35.data$p_value.spread1 < is_significant),
                                   non_significant.spread = sum(model.35.data$p_value.spread1 >= is_significant),
                                   significant.log.vol_tot = sum(model.35.data$p_value.log.vol_tot. < is_significant),
                                   non_significant.log.sqrt.vol_tot = sum(model.35.data$p_value.log.vol_tot. >= is_significant),
                                   significant.log.trades = sum(model.35.data$p_value.log.trades < is_significant),
                                   non_significant.log.trades = sum(model.35.data$p_value.log.trades >= is_significant))


## d ~ spread + log(vol_tot) + sqrt(trades)## 
model.36 <- lmList(d ~ spread1 + log(vol_tot) + sqrt(trades) | cusip_id, data = data)
model.36.data <- data.frame(coef = coefficients(model.36),
                            conf = confint(model.36),
                            p_value = summary(model.36)$coef[,4,2:4],
                            r_squared = summary(model.36)$r.squared,
                            r_squared_adj = matrix(unlist(summary(model.36)$adj.r.squared), nrow=length(summary(model.36)$adj.r.squared), byrow=TRUE))
model.36.data$cusip <- row.names(model.36.data)

model.36.significant <- data.frame(significant.spread = sum(model.36.data$p_value.spread1 < is_significant),
                                   non_significant.spread = sum(model.36.data$p_value.spread1 >= is_significant),
                                   significant.log.vol_tot = sum(model.36.data$p_value.log.vol_tot. < is_significant),
                                   non_significant.log.sqrt.vol_tot = sum(model.36.data$p_value.log.vol_tot. >= is_significant),
                                   significant.sqrt.trades = sum(model.36.data$p_value.sqrt.trades < is_significant),
                                   non_significant.sqrt.trades = sum(model.36.data$p_value.sqrt.trades >= is_significant))


## d ~ spread + sqrt(vol_tot) + log(trades)## 
model.37 <- lmList(d ~ spread1 + sqrt(vol_tot) + log(trades) | cusip_id, data = data)
model.37.data <- data.frame(coef = coefficients(model.37),
                            conf = confint(model.37),
                            p_value = summary(model.37)$coef[,4,2:4],
                            r_squared = summary(model.37)$r.squared,
                            r_squared_adj = matrix(unlist(summary(model.37)$adj.r.squared), nrow=length(summary(model.37)$adj.r.squared), byrow=TRUE))
model.37.data$cusip <- row.names(model.37.data)

model.37.significant <- data.frame(significant.spread = sum(model.37.data$p_value.spread1 < is_significant),
                                   non_significant.spread = sum(model.37.data$p_value.spread1 >= is_significant),
                                   significant.sqrt.vol_tot = sum(model.37.data$p_value.sqrt.vol_tot. < is_significant),
                                   non_significant.sqrt.vol_tot = sum(model.37.data$p_value.sqrt.vol_tot. >= is_significant),
                                   significant.log.trades = sum(model.37.data$p_value.log.trades < is_significant),
                                   non_significant.log.trades = sum(model.37.data$p_value.log.trades >= is_significant))


















## d ~ spread + log(vol_tot) + log(trades):p_avg## 
model.test <- lmList(d ~ spread1 + log(vol_tot) + log(trades)| cusip_id, data = data)
model.test.data <- data.frame(row = 1:167,
                            coef = coefficients(model.test),
                            conf = confint(model.test),
                            p_value = summary(model.test)$coef[,4,2:4],
                            r_squared = summary(model.test)$r.squared,
                            r_squared_adj = matrix(unlist(summary(model.test)$adj.r.squared), nrow=length(summary(model.test)$adj.r.squared), byrow=TRUE))
model.test.data$cusip <- row.names(model.test.data)

model.test.significant <- data.frame(significant.spread = sum(model.test.data$p_value.spread1 < is_significant),
                                   non_significant.spread = sum(model.test.data$p_value.spread1 >= is_significant),
                                   significant.log.vol_tot = sum(model.test.data$p_value.log.vol_tot. < is_significant),
                                   non_significant.log.vol_tot = sum(model.test.data$p_value.log.vol_tot. >= is_significant),
                                   significant.log.trades = sum(model.test.data$p_value.log.trades. < is_significant),
                                   non_significant.log.trades = sum(model.test.data$p_value.log.trades. >= is_significant))

model.test.p <- data.frame(cusip_id = row.names(model.test.data),
                           spread = 0,
                           log.vol_tot = 0,
                           log.trades = 0,
                           total = 0)

rows.spread <- model.test.data[model.test.data$cusip %in% row.names(model.test.data[model.test.data$p_value.spread1 > is_significant, ]), ]
rows.log.vol_tot <- model.test.data[model.test.data$cusip %in% row.names(model.test.data[model.test.data$p_value.log.vol_tot. > is_significant, ]), ]
rows.log.trades <- model.test.data[model.test.data$cusip %in% row.names(model.test.data[model.test.data$p_value.log.trades. > is_significant, ]), ]
model.test.p[rows.spread$row, 2] <- 1
model.test.p[rows.log.vol_tot$row, 3] <- 1
model.test.p[rows.log.trades$row, 4] <- 1
model.test.p$total <- model.test.p$spread + model.test.p$log.vol_tot + model.test.p$log.trades

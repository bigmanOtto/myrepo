####      MODELLING       ####
library(lme4)

data <- read.csv("DAILY_all.csv")
names(data)[1] <- paste("cusip_id")
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



### Illiq liquidity ###

## illiq ~ p_avg ## 
model.4 <- lmList(illiq_mid ~ p_avg | cusip_id, data = data)
model.4.data <- data.frame(coef = coefficients(model.4),
                           conf = confint(model.4),
                           p_value = summary(model.4)$coef[,4,2],
                           r_squared = summary(model.4)$r.squared)
model.4.data$cusip <- row.names(model.4.data)

model.4.hist <- hist(model.4.data$coef.p_avg, xlim = c(-6*10^-9, 1*10^-9))
model.4.significant <- data.frame(significant = sum(model.4.data$p_value < is_significant),
                                  non_significant = sum(model.4.data$p_value >= is_significant))


## Illiq ~ vol_tot ## 
model.5 <- lmList(illiq_mid ~ vol_tot | cusip_id, data = data)
model.5.data <- data.frame(coef = coefficients(model.5),
                           conf = confint(model.5),
                           p_value = summary(model.5)$coef[,4,2],
                           r_squared = summary(model.5)$r.squared)
model.5.data$cusip <- row.names(model.5.data)

model.5.hist <- hist(model.5.data$coef.vol_tot)
model.5.significant <- data.frame(significant = sum(model.5.data$p_value < is_significant),
                                  non_significant = sum(model.5.data$p_value >= is_significant))


## Illiq ~ spread ##  Fungerar inte eftersom vi inte har all spread data ... 
model.6 <- lmList(illiq_mid ~ spread | cusip_id, data = data)
model.6.data <- data.frame(coef = coefficients(model.6),
                           conf = confint(model.6),
                           p_value = summary(model.6)$coef[,4,2],
                           r_squared = summary(model.6)$r.squared)
model.6.data$cusip <- row.names(model.6.data)

model.6.hist <- hist(model.6.data$coef.vol_tot)
model.6.significant <- data.frame(significant = sum(model.6.data$p_value < is_significant),
                                  non_significant = sum(model.6.data$p_value >= is_significant))








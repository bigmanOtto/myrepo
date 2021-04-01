#### CALCULATE SPREAD #### 
library(imputeTS)

trace <- read.csv("TRACE.csv")
trades <- aggregate(type~cusip_id+trd_exctn_dt, data = trace, FUN = function(x){NROW(x)})
trades <- trades[with(trades, order(cusip_id, trd_exctn_dt)), ]
names(trades)[3] <- paste("trades")

data <- read.csv("DAILY_all.csv")
names(data)[1] <- paste("cusip_id")
data$trd_exctn_dt <- as.Date.character(data$trd_exctn_dt, format = "%Y%m%d")
data$trades <- trades$trades

## S_median - B_median  ## 
median <- aggregate(rptd_pr~cusip_id+trd_exctn_dt+rpt_side_cd, data = trace, FUN = function(x){median(x)})
median_B <- median[which(median$rpt_side_cd=='B'),]
median_S <- median[which(median$rpt_side_cd=='S'),]
median <- merge(median_B, median_S, by.x = c("cusip_id", "trd_exctn_dt"), by.y = c("cusip_id", "trd_exctn_dt"))
names(median)[3] <- paste("Buyer")
names(median)[4] <- paste("Bid") 
names(median)[5] <- paste("Seller")
names(median)[6] <- paste("Ask")
median$spread_median <- median$Ask - median$Bid
median$trd_exctn_dt <- as.Date.character(median$trd_exctn_dt, format = "%Y%m%d")
median <- median[median$spread_median > 0, ]
median <- median[median$spread_median <= 1.5, ]
median <- median[!is.na(median$spread_median), ]

data <- merge(data, median, by.x = c("cusip_id", "trd_exctn_dt"), by.y = c("cusip_id", "trd_exctn_dt"), all = TRUE)
statsNA(data$spread_median)
histogram <- hist(data$spread_median)


kalman <- data.frame(kalman = na_kalman(data$spread_median, model = "auto.arima", smooth = TRUE))
data$spread_kalman <- kalman$kalman

ggplot_na_imputations(bond$spread_median,
                    bond$spread_kalman,
                    x_axis_labels = bond$trd_exctn_dt,
                    title = "Imputation with Kalman filter and ARIMA",
                    subtitle = "Cusip Id: 382550BG5",
                    ylab = "Spread",
                    color_points = "black",
                    color_imputations = "red",
                    label_known = "Known values",
                    label_imputations = "Imputed values",
                    theme = ggplot2::aes())










## S_min - B_max ## 
spread_max <- aggregate(rptd_pr~cusip_id+trd_exctn_dt+rpt_side_cd, data = trace, max)
spread_min <- aggregate(rptd_pr~cusip_id+trd_exctn_dt+rpt_side_cd, data = trace, min)
spread_max <- spread_max[which(spread_max$rpt_side_cd=='B'),]
spread_min <- spread_min[which(spread_min$rpt_side_cd=='S'),]
spread_max <- spread_max[with(spread_max, order(cusip_id, trd_exctn_dt)), ]
spread_min <- spread_min[with(spread_min, order(cusip_id, trd_exctn_dt)), ]
spread_minmax <- merge(spread_max, spread_min, by.x = c("cusip_id", "trd_exctn_dt"), by.y = c("cusip_id", "trd_exctn_dt"))
names(spread_minmax)[4] <- paste("Bid")
names(spread_minmax)[6] <- paste("Ask")
spread_minmax$spread <- spread_minmax$Ask-spread_minmax$Bid
spread_minmax<- spread_minmax[spread_minmax$spread>=0,]
spread_minmax$trd_exctn_dt <- as.Date.character(spread_minmax$trd_exctn_dt, format = "%Y%m%d")
#data.full <- merge(data.full, spread, by.x = c("cusip_id", "trd_exctn_dt"), by.y = c("cusip_id", "trd_exctn_dt"), all = TRUE) 


## S_mean - B_mean ## 
spread_mean <- aggregate(rptd_pr~cusip_id+trd_exctn_dt+rpt_side_cd, data = trace, mean)
spread_mean_B <- spread_mean[which(spread_mean$rpt_side_cd=='B'),]
spread_mean_S <- spread_mean[which(spread_mean$rpt_side_cd=='S'),]
spread_mean <- merge(spread_mean_B, spread_mean_S, by.x = c("cusip_id", "trd_exctn_dt"), by.y = c("cusip_id", "trd_exctn_dt"))
names(spread_mean)[4] <- paste("Bid") 
names(spread_mean)[6] <- paste("Ask")
spread_mean$spread <- spread_mean$Ask - spread_mean$Bid
spread_mean$trd_exctn_dt <- as.Date.character(spread_mean$trd_exctn_dt, format = "%Y%m%d")
spread_mean <- spread_mean[spread_mean$spread > 0, ]
spread_mean <- spread_mean[!is.na(spread_mean$spread), ]

data <- merge(data, spread_mean, by.x = c("cusip_id", "trd_exctn_dt"), by.y = c("cusip_id", "trd_exctn_dt"), all = TRUE)
statsNA(data$spread.y)

ggplot(data = spread_mean[1:500,], aes(x = trd_exctn_dt, y = spread)) + 
  geom_line() +
  labs(x = "Date", y = "Spread", title = "Vår estimering")


test <- data.frame(test = aggregate(spread.y~cusip_id, data = data, FUN = function(x){na_kalman(x, model = "auto.arima")}))

kalman <- data.frame(spread = na_kalman(data$spread.y, model = "auto.arima"))
data$kalman <- kalman$spread


Not <- data[is.na(data$spread.y), ]
bond <- data[data$cusip_id == '03674XAC0',]
bond <- bond[!is.na(bond$spread.y), ]
bond1 <- data[data$cusip_id == '00287YAQ2',]
bond1 <- bond1[!is.na(bond$spread.y), ]



ggplot() + 
  geom_point(data = data[7606:8099, ], aes(x = trd_exctn_dt, y = kalman), color = "red", size = 1) +
  geom_point(data = bond, aes(x = trd_exctn_dt, y = spread.y), size = 1) + 
  geom_line(data = bond, aes(x = trd_exctn_dt, y = spread.y)) 

  
ggplot() + 
  geom_point(data = data[4008:4508, ], aes(x = trd_exctn_dt, y = kalman), color = "red", size = 1) +
  geom_point(data = bond1, aes(x = trd_exctn_dt, y = spread.y), size = 1) + 
  geom_line(data = bond1, aes(x = trd_exctn_dt, y = spread.y))  

h <- hist(data$kalman, main = "Histogram Kalman smoothing")
h1 <- hist(data$spread.y, main = "Histogram mean diff")

test <- aggregate(spread.y~cusip_id, data = data, FUN = function(x){na_kalman(x)})


temp <- data[data$spread>0,]
temp <- temp[!is.na(temp$spread),]
temp <- merge(temp, spread_mean, by.x = c("cusip_id", "trd_exctn_dt"), by.y = c("cusip_id", "trd_exctn_dt"))
temp <- temp[which(temp$cusip_id == c('00206RBD3', '00206RCL4', '00206RCM2', 'U8810LAA1')),]

ggplot() +
  geom_line(data = temp, aes(x = trd_exctn_dt, y = spread.x, color = cusip_id)) +
  geom_line(data = temp, aes(x = trd_exctn_dt, y = spread.y, color = cusip_id), linetype = "dashed" ) +
  ylim(0,2)

######## TEST

upr <- aggregate(rptd_pr~cusip_id+trd_exctn_dt+rpt_side_cd, data = trace, FUN = function(x){quantile(x, 0.9)})
upr_B <- upr[which(upr$rpt_side_cd=='B'),]
names(upr_B)[3] <- paste("upr_B_ind")
names(upr_B)[4] <- paste("upr_B")
upr_S <- upr[which(upr$rpt_side_cd=='S'),]
names(upr_S)[3] <- paste("upr_S_ind")
names(upr_S)[4] <- paste("upr_S")
test <- merge(trace, upr_B, by.x = c("cusip_id", "trd_exctn_dt", "rpt_side_cd"), by.y = c("cusip_id", "trd_exctn_dt", "upr_B_ind"), all = TRUE)
test <- merge(test, upr_S, by.x = c("cusip_id", "trd_exctn_dt", "rpt_side_cd"), by.y = c("cusip_id", "trd_exctn_dt", "upr_S_ind"), all = TRUE)


lwr <- aggregate(rptd_pr~cusip_id+trd_exctn_dt+rpt_side_cd, data = trace, FUN = function(x){quantile(x, 0.1)})
lwr_B <- lwr[which(lwr$rpt_side_cd=='B'),]
names(lwr_B)[3] <- paste("lwr_B_ind")
names(lwr_B)[4] <- paste("lwr_B")
lwr_S <- lwr[which(lwr$rpt_side_cd=='S'),]
names(lwr_S)[3] <- paste("lwr_S_ind")
names(lwr_S)[4] <- paste("lwr_S")
test <- merge(test, lwr_B, by.x = c("cusip_id", "trd_exctn_dt", "rpt_side_cd"), by.y = c("cusip_id", "trd_exctn_dt", "lwr_B_ind"), all = TRUE)
test <- merge(test, lwr_S, by.x = c("cusip_id", "trd_exctn_dt", "rpt_side_cd"), by.y = c("cusip_id", "trd_exctn_dt", "lwr_S_ind"), all = TRUE)


test$upr <- ifelse(is.na(test$upr_B), test$upr_S, test$upr_B)
test$lwr <- ifelse(is.na(test$lwr_B), test$lwr_S, test$lwr_B)
test <- test[test$rptd_pr < test$upr, ]
test <- test[test$rptd_pr > test$lwr, ]

mean <- aggregate(rptd_pr~cusip_id+trd_exctn_dt+rpt_side_cd, data = test, mean)
mean_B <- mean[which(mean$rpt_side_cd=='B'),]
mean_S <- mean[which(mean$rpt_side_cd=='S'),]
mean <- merge(mean_B, mean_S, by.x = c("cusip_id", "trd_exctn_dt"), by.y = c("cusip_id", "trd_exctn_dt"))
names(mean)[4] <- paste("Bid") 
names(mean)[6] <- paste("Ask")
mean$spread <- mean$Ask - mean$Bid
mean$trd_exctn_dt <- as.Date.character(mean$trd_exctn_dt, format = "%Y%m%d")
mean <- mean[mean$spread > 0, ]
mean <- mean[!is.na(mean$spread), ]
names(mean)[7] <- paste("spread_quantile")

data <- merge(data, mean, by.x = c("cusip_id", "trd_exctn_dt"), by.y = c("cusip_id", "trd_exctn_dt"), all = TRUE)
statsNA(data$spread_quantile)
h3 <- hist(mean$spread_quantile)

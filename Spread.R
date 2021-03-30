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

ggplot(data = spread_mean[1:500,], aes(x = trd_exctn_dt, y = spread)) + 
  geom_line() +
  labs(x = "Date", y = "Spread", title = "Vår estimering")


test <- data.frame(test = na_kalman(data$spread.y))
Not <- data[is.na(data$spread.y), ]
bond <- data[data$cusip_id == '03674XAC0',]
bond <- bond[!is.na(bond$spread.y), ]
bond1 <- data[data$cusip_id == '00287YAQ2',]
bond1 <- bond1[!is.na(bond$spread.y), ]
data$test <- test$test


ggplot() + 
  geom_point(data = data[7606:8099, ], aes(x = trd_exctn_dt, y = test), color = "red", size = 1) +
  geom_point(data = bond, aes(x = trd_exctn_dt, y = spread.y), size = 1) + 
  geom_line(data = data[7606:8099,], aes(x = trd_exctn_dt, y = test), color = "red") +
  geom_line(data = bond, aes(x = trd_exctn_dt, y = spread.y))  

  
ggplot() + 
  geom_point(data = data[4008:4508, ], aes(x = trd_exctn_dt, y = test), color = "red", size = 1) +
  geom_point(data = bond1, aes(x = trd_exctn_dt, y = spread.y), size = 1) + 
  geom_line(data = data[4008:4508,], aes(x = trd_exctn_dt, y = test), color = "red") +
  geom_line(data = bond1, aes(x = trd_exctn_dt, y = spread.y))  

h <- hist(data$test)
h1 <- hist(data$spread.y)

test <- aggregate(spread.y~cusip_id, data = data, FUN = function(x){na_kalman(x)})


temp <- data[data$spread>0,]
temp <- temp[!is.na(temp$spread),]
temp <- merge(temp, spread_mean, by.x = c("cusip_id", "trd_exctn_dt"), by.y = c("cusip_id", "trd_exctn_dt"))
temp <- temp[which(temp$cusip_id == c('00206RBD3', '00206RCL4', '00206RCM2', 'U8810LAA1')),]

ggplot() +
  geom_line(data = temp, aes(x = trd_exctn_dt, y = spread.x, color = cusip_id)) +
  geom_line(data = temp, aes(x = trd_exctn_dt, y = spread.y, color = cusip_id), linetype = "dashed" ) +
  ylim(0,2)
  

## Roll 1984 ## 
test <- data.frame(cusip_id = data$cusip_id,
                   date = data$trd_exctn_dt,
                   p_avg = data$p_avg)
test$p_avg_lag <- NA
test$p_avg_lag[2:77206] <- diff(test$p_avg, lag = 1)

acf <- acf(test$p_avg_lag[2:502], lag = 501, type = "correlation", plot = TRUE)



#### CALCULATE SPREAD #### 

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


## Roll 1984 ## 

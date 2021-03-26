library(chron)

sptick <- read.csv("SP.csv")
which(grepl("07/03/2017", sptick$date))
which(grepl("07/01/2019", sptick$date))
sptick <- sptick[13721636:13794918, ]

sptick$newdate <- strptime(as.character(sptick$date), "%m/%d/%Y")
sptick$txtdate <- format(sptick$newdate, "%Y-%m-%d") 


trace <- read.csv("TRACE.csv")
bond1 <- trace[trace$cusip_id == '00206RBD3', ]

day1 <- bond1[bond1$trd_exctn_dt == 20170703, ]
day1 <- day1[order(day1$trd_exctn_tm), ]
day1 <- day1[,c(1,12:13,17)]
day1$newdate <- strptime(as.character(day1$trd_exctn_dt), "%Y%m%d")
day1$txtdate <- format(day1$newdate, "%Y-%m-%d") 


sptick$chron <- chron(dates = sptick$txtdate, times = sptick$time, format = c(dates = "Y-m-d", times = "h:m:s")) 
day1$chron <- chron(dates = day1$txtdate, times = day1$trd_exctn_tm, format = c(dates = "Y-m-d", times = "h:m:s")) 


which.min(abs(sptick$chron - day1$chron[1]))
delta <- (sptick$price[which.min(abs(sptick$chron - day1$chron[2]))] - sptick$price[which.min(abs(sptick$chron - day1$chron[1]))])/sptick$price[which.min(abs(sptick$chron - day1$chron[1]))]
day1$rptd_pr[2]
day1$rptd_pr[2]/(1+delta)


delta <- (sptick$price[which.min(abs(sptick$chron - day1$chron[4]))] - sptick$price[which.min(abs(sptick$chron - day1$chron[3]))])/sptick$price[which.min(abs(sptick$chron - day1$chron[3]))]
day1$rptd_pr[4]
day1$rptd_pr[4]/(1+delta)

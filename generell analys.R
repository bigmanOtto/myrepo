data <- DAILY_all

data$trd_exctn_dt <- as.Date.character(data$trd_exctn_dt, format = "%Y%m%d")

dataHY <- data[data$type=="HY",]
dataIG <- data[data$type=="IG",]

avg_d=mean(data$d)

avg_d_HY=mean(dataHY$d)
avg_d_IG=mean(dataIG$d)

library(plyr)
par(mfrow=c(1,1))
plot(ddply(data, .(trd_exctn_dt), summarize, daily_mean_d = mean(d)))

plot(ddply(dataIG, .(trd_exctn_dt), summarize, daily_mean_d = mean(d)))
points(ddply(dataHY, .(trd_exctn_dt), summarize, daily_mean_d = mean(d)),col="red")
     



data_HHI <- data[!is.na(data$HHI),]

dataHY_HHI <- data_HHI[data_HHI$type=="HY",]
dataIG_HHI <- data_HHI[data_HHI$type=="IG",]


avg_HHI=mean(data_HHI$HHI)

avg_HHI_HY=mean(dataHY_HHI$HHI)
avg_HHI_IG=mean(dataIG_HHI$HHI)

plot(ddply(data_HHI, .(trd_exctn_dt), summarize, daily_mean_d = mean(HHI)))

plot(ddply(dataIG_HHI, .(trd_exctn_dt), summarize, daily_mean_HHI = mean(HHI)),ylim=c(0,10))
points(ddply(dataHY_HHI, .(trd_exctn_dt), summarize, daily_mean_HHI = mean(HHI)),col="red")

data_illiq_mid <- data[!is.na(data$illiq_mid),]

dataHY_illiq_mid <- data_illiq_mid[data_illiq_mid$type=="HY",]
dataIG_illiq_mid <- data_illiq_mid[data_illiq_mid$type=="IG",]

avg_ILLIQ=mean(data_illiq_mid$illiq_mid)

avg_ILLIQ_HY=mean(dataHY_illiq_mid$illiq_mid)
avg_ILLIQ_IG=mean(dataIG_illiq_mid$illiq_mid)

plot(ddply(data_illiq_mid, .(trd_exctn_dt), summarize, daily_mean_d = mean(illiq_mid)))

plot(ddply(dataIG_illiq_mid, .(trd_exctn_dt), summarize, daily_mean_ILLIQ = mean(illiq_mid)), ylim=c(0,0.0000000004))
points(ddply(dataHY_illiq_mid, .(trd_exctn_dt), summarize, daily_mean_ILLIQ = mean(illiq_mid)),col="red")



## correlation matrix
corrtest <- data[,c(15,43)]
corrtest$trades<-unlist(trades[,3])
corrtest$spread<-unlist(kalman)

corrtest=corrtest[!is.na(corrtest$turnover),]
corrtest=corrtest[!is.nan(corrtest$turnover),]
corrtest=corrtest[!is.infinite(corrtest$turnover),]
matrix<-cor(corrtest)




##plots

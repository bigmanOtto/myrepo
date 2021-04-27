data <- DAILY_all
data$spread1<-unlist(kalman)
data$trades<-unlist(trades[,3])
data$trd_exctn_dt <- as.Date.character(data$trd_exctn_dt, format = "%Y%m%d")


stressed <- data[data$trd_exctn_dt<20181231,]
stressed <- stressed[stressed$trd_exctn_dt>20181130,]

stressedHY <-stressed[stressed$type=="HY",]
stressedIG <- stressed[stressed$type=="IG",]



normal <- data[data$trd_exctn_dt<20181002,]
normal <- normal[normal$trd_exctn_dt>20180905,]

normalHY <-normal[normal$type=="HY",]
normalIG <- normal[normal$type=="IG",]

##stressed regression
model1<-lm(d~log(vol_tot)+spread1+log(trades)+credit, data=stressed)

q<-mean(stressed$d)
shy<-mean(stressedHY$d)
f<-mean(stressedIG$d)

##normal regression
model2<-lm(d~log(vol_tot)+spread1+log(trades)+credit, data=normal)

u<-mean(normal$d)
nhy<-mean(normalHY$d)
g<-mean(normalIG$d)

##volume analysis
vol_stressedHY<-sum(stressedHY$vol_tot)
vol_normalHY<-sum(normalHY$vol_tot)

ratioHY<-vol_stressedHY/vol_normalHY

vol_stressedIG<-sum(stressedIG$vol_tot)
vol_normalIG<-sum(normalIG$vol_tot)

ratioIG<-vol_stressedIG/vol_normalIG

##p_avg analysis
p_avg_stressedHY<-mean(stressedHY$p_avg)
p_avg_normalHY<-mean(normalHY$p_avg)

pratioHY<-p_avg_stressedHY/p_avg_normalHY

p_avg_stressedIG<-mean(stressedIG$p_avg)
p_avg_normalIG<-mean(normalIG$p_avg)

pratioIG<-p_avg_stressedIG/p_avg_normalIG

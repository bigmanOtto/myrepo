trace <- TRACE
trades <- aggregate(type~cusip_id+trd_exctn_dt, data = trace, FUN = function(x){NROW(x)})
trades <- trades[with(trades, order(cusip_id, trd_exctn_dt)), ]
names(trades)[3] <- paste("trades")

data <- DAILY_all
data$trades <- trades$trades
data$spread1 <- unlist(kalman)

subdata_IG1<-data[5:502,] ##00206RBD3
subdata_IG1<-subdata_IG1[subdata_IG1$HHI<10,]

subdata_IG2<-data[507:1002,] ##00206RCL4
subdata_IG2<-subdata_IG2[subdata_IG2$HHI<7,]

subdata_IG3<-data[1009:1503,] ##00206RCM2
subdata_IG3<-subdata_IG3[subdata_IG3$HHI<11,]

subdata_HY1<-data[62362:62857,] ##81180WAH4
subdata_HY1<-subdata_HY1[subdata_HY1$HHI<8,]

subdata_HY2<-data[61369:61859,] ##767754AJ3
subdata_HY2<-subdata_HY2[subdata_HY2$HHI<30,]

subdata_HY3<-data[61864:62357,] ##78412FAP9
subdata_HY3<-subdata_HY3[subdata_HY3$HHI<3,]

names(data)[1] <- paste("cusip_id")
data$trd_exctn_dt <- as.Date.character(data$trd_exctn_dt, format = "%Y%m%d")
is_significant <- 0.05

##model 1
model.1_IG1<-lm(HHI~vol_tot,data=subdata_IG1)
model.1_IG2<-lm(HHI~vol_tot,data=subdata_IG2)
model.1_IG3<-lm(HHI~vol_tot,data=subdata_IG3)

model.1_HY1<-lm(HHI~vol_tot,data=subdata_HY1)
model.1_HY2<-lm(HHI~vol_tot,data=subdata_HY2)
model.1_HY3<-lm(HHI~vol_tot,data=subdata_HY3)

res.1_IG1<-residuals(model.1_IG1)
res.1_IG2<-residuals(model.1_IG2)
res.1_IG3<-residuals(model.1_IG3)

res.1_HY1<-residuals(model.1_HY1)
res.1_HY2<-residuals(model.1_HY2)
res.1_HY3<-residuals(model.1_HY3)

par(mfrow=c(2,3))
hist(res.1_IG1)
hist(res.1_IG2)
hist(res.1_IG3)
hist(res.1_HY1)
hist(res.1_HY2)
hist(res.1_HY3)

par(mfrow=c(2,3))
qqnorm(res.1_IG1)
qqline(res.1_IG1)
qqnorm(res.1_IG2)
qqline(res.1_IG2)
qqnorm(res.1_IG3)
qqline(res.1_IG3)
qqnorm(res.1_HY1)
qqline(res.1_HY1)
qqnorm(res.1_HY2)
qqline(res.1_HY2)
qqnorm(res.1_HY3)
qqline(res.1_HY3)


##model 2
model.2_IG1<-lm(HHI~vol_tot,data=subdata_IG1)
model.2_IG2<-lm(HHI~vol_tot,data=subdata_IG2)
model.2_IG3<-lm(HHI~vol_tot,data=subdata_IG3)

model.2_HY1<-lm(HHI~vol_tot,data=subdata_HY1)
model.2_HY2<-lm(HHI~vol_tot,data=subdata_HY2)
model.2_HY3<-lm(HHI~vol_tot,data=subdata_HY3)

res.2_IG1<-residuals(model.2_IG1)
res.2_IG2<-residuals(model.2_IG2)
res.2_IG3<-residuals(model.2_IG3)

res.2_HY1<-residuals(model.2_HY1)
res.2_HY2<-residuals(model.2_HY2)
res.2_HY3<-residuals(model.2_HY3)

par(mfrow=c(2,3))
hist(res.2_IG1)
hist(res.2_IG2)
hist(res.2_IG3)
hist(res.2_HY1)
hist(res.2_HY2)
hist(res.2_HY3)

par(mfrow=c(2,3))
qqnorm(res.2_IG1)
qqline(res.2_IG1)
qqnorm(res.2_IG2)
qqline(res.2_IG2)
qqnorm(res.2_IG3)
qqline(res.2_IG3)
qqnorm(res.2_HY1)
qqline(res.2_HY1)
qqnorm(res.2_HY2)
qqline(res.2_HY2)
qqnorm(res.2_HY3)
qqline(res.2_HY3)


##model 3
model.3_IG1<-lm(HHI~vol_tot,data=subdata_IG1)
model.3_IG2<-lm(HHI~vol_tot,data=subdata_IG2)
model.3_IG3<-lm(HHI~vol_tot,data=subdata_IG3)

model.3_HY1<-lm(HHI~vol_tot,data=subdata_HY1)
model.3_HY2<-lm(HHI~vol_tot,data=subdata_HY2)
model.3_HY3<-lm(HHI~vol_tot,data=subdata_HY3)

res.3_IG1<-residuals(model.3_IG1)
res.3_IG2<-residuals(model.3_IG2)
res.3_IG3<-residuals(model.3_IG3)

res.3_HY1<-residuals(model.3_HY1)
res.3_HY2<-residuals(model.3_HY2)
res.3_HY3<-residuals(model.3_HY3)

par(mfrow=c(2,3))
hist(res.3_IG1)
hist(res.3_IG2)
hist(res.3_IG3)
hist(res.3_HY1)
hist(res.3_HY2)
hist(res.3_HY3)

par(mfrow=c(2,3))
qqnorm(res.3_IG1)
qqline(res.3_IG1)
qqnorm(res.3_IG2)
qqline(res.3_IG2)
qqnorm(res.3_IG3)
qqline(res.3_IG3)
qqnorm(res.3_HY1)
qqline(res.3_HY1)
qqnorm(res.3_HY2)
qqline(res.3_HY2)
qqnorm(res.3_HY3)
qqline(res.3_HY3)

trace <- TRACE
trades <- aggregate(type~cusip_id+trd_exctn_dt, data = trace, FUN = function(x){NROW(x)})
trades <- trades[with(trades, order(cusip_id, trd_exctn_dt)), ]
names(trades)[3] <- paste("trades")

data <- DAILY_all
data$trades <- trades$trades
data$spread1 <- unlist(kalman)

subdata_IG1<-data[1:502,] ##00206RBD3
subdata_IG1<-subdata_IG1[subdata_IG1$d<0.8,]

subdata_IG2<-data[502:1002,] ##00206RCL4
subdata_IG2<-subdata_IG2[subdata_IG2$d<0.4,]

subdata_IG3<-data[1002:1503,] ##00206RCM2
subdata_IG3<-subdata_IG3[subdata_IG3$d<0.7,]

subdata_HY1<-data[62362:62857,] ##81180WAH4
subdata_HY1<-subdata_HY1[subdata_HY1$d<0.9,]

subdata_HY2<-data[61365:61859,] ##767754AJ3
subdata_HY2<-subdata_HY2[subdata_HY2$d<2.0,]

subdata_HY3<-data[61860:62357,] ##78412FAP9
subdata_HY3<-subdata_HY3[subdata_HY3$d<1.5,]

names(data)[1] <- paste("cusip_id")
data$trd_exctn_dt <- as.Date.character(data$trd_exctn_dt, format = "%Y%m%d")
is_significant <- 0.05

##model 1
model.1_IG1<-lm(d~spread + log(vol_tot) + log(trades),data=subdata_IG1)
model.1_IG2<-lm(d~spread + log(vol_tot) + log(trades),data=subdata_IG2)
model.1_IG3<-lm(d~spread + log(vol_tot) + log(trades),data=subdata_IG3)

model.1_HY1<-lm(d~spread + log(vol_tot) + log(trades),data=subdata_HY1)
model.1_HY2<-lm(d~spread + log(vol_tot) + log(trades),data=subdata_HY2)
model.1_HY3<-lm(d~spread + log(vol_tot) + log(trades),data=subdata_HY3)

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
model.2_IG1<-lm(d~spread + log(vol_tot) + sqrt(trades),data=subdata_IG1)
model.2_IG2<-lm(d~spread + log(vol_tot) + sqrt(trades),data=subdata_IG2)
model.2_IG3<-lm(d~spread + log(vol_tot) + sqrt(trades),data=subdata_IG3)

model.2_HY1<-lm(d~spread + log(vol_tot) + sqrt(trades),data=subdata_HY1)
model.2_HY2<-lm(d~spread + log(vol_tot) + sqrt(trades),data=subdata_HY2)
model.2_HY3<-lm(d~spread + log(vol_tot) + sqrt(trades),data=subdata_HY3)

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
model.3_IG1<-lm(d~vol_tot,data=subdata_IG1)
model.3_IG2<-lm(d~vol_tot,data=subdata_IG2)
model.3_IG3<-lm(d~vol_tot,data=subdata_IG3)

model.3_HY1<-lm(d~vol_tot,data=subdata_HY1)
model.3_HY2<-lm(d~vol_tot,data=subdata_HY2)
model.3_HY3<-lm(d~vol_tot,data=subdata_HY3)

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

data <- read.csv("DAILY_all.csv")
data$trd_exctn_dt <- as.Date.character(data$trd_exctn_dt, format = "%Y%m%d")
agg_hhi <- aggregate(HHI~trd_exctn_dt+type, data = data[!is.na(data$HHI), ], FUN = function(x){mean(x)})
agg_d <- aggregate(d~trd_exctn_dt+type, data = data[data$d>0, ], FUN = function(x){mean(x)})
agg_illiq <- aggregate(illiq_mid~trd_exctn_dt+type, data = data[!is.na(data$illiq_mid), ], FUN = function(x){mean(x)})

hhi <-ggplot() +
  geom_point(data = agg_hhi[agg_hhi$type == 'IG', ], aes(x=trd_exctn_dt, y = HHI), size= 1 ) +
  geom_point(data = agg_hhi[agg_hhi$type == 'HY', ], aes(x=trd_exctn_dt, y = HHI), color = "red", size=1) +
  labs(x = "Date")

d<-ggplot() +
  geom_point(data = agg_d[agg_d$type == 'IG', ], aes(x=trd_exctn_dt, y = d), size= 1) +
  geom_point(data = agg_d[agg_d$type == 'HY', ], aes(x=trd_exctn_dt, y = d), color = "red", size=1) +
  labs(x = "Date", caption = "Green area = Period of stressed market", y = "Illiquidity") +
  annotate("rect", xmin=VIX[2864,1], xmax=VIX[2902,1], ymin=0, ymax=Inf, alpha=0.2, fill="green") 
  

illiq<-ggplot() +
  geom_point(data = agg_illiq[agg_illiq$type == 'IG', ], aes(x=trd_exctn_dt, y = illiq_mid), size=1) +
  geom_point(data = agg_illiq[agg_illiq$type == 'HY', ], aes(x=trd_exctn_dt, y = illiq_mid), color = "red", size= 1) +
  labs(x = "Date", y = "ILLIQ") +
  ylim(0, 4e-10)

ggarrange(ggarrange(hhi, illiq, nrow=1, ncol=2), d, nrow = 2, ncol =1)





install.packages("ggplot2")
library(ggplot2)
library(readxl)
library(ggpubr)
library(plyr)
library(zoo)
library(dplyr)

data <- read.csv("DAILY_all.csv")
data.full <- read.csv("TRACE.csv")
names(data)[names(data) == "ï..cusip_id"] <- "cusip_id"

data$trd_exctn_dt <- as.Date.character(data$trd_exctn_dt, format = "%Y%m%d")
subset <- data.frame(p = unlist(data$p_avg[1:10000]),  
                     date = unlist(data$trd_exctn_dt[1:10000]),
                     cusip = unlist(data$cusip_id[1:10000]))

ggplot(data = subset, aes(x=date, y=p, color=cusip)) +
    geom_line(data = subset, size = 0.5)


# correlation first bond and index
index <- data.frame(sp500close = data$sp500_close[1:502],
                    sp500_logreturn = data$sp500_logreturn[1:502],
                    date = data$trd_exctn_dt[1:502])

bond <- data[1:502,]

plot.sp5 <- ggplot(data = index, aes(x = date, y = sp500close))+
  geom_line(data = index, color = "red") + 
  coord_cartesian(ylim = c(2200, 3000))

plot.b <- ggplot(data = bond, aes(x=trd_exctn_dt, y=p_mid))+
  geom_line(data = bond) 

ggarrange(plot.sp5, plot.b)

cor.test(bond$p_mid, index$sp500close)

# Correlation all bonds by XXX
data.corr <-data.frame(cusip = data$cusip_id,
                       type = data$type,
                       rating = data$credit,
                       d = data$d, 
                       index = data$spbond_close)

ddply(data.corr,"type",summarise,
      corr=cor.test(d,index))
 
#rolling correlation first bond and index
rolling <- data.frame(date = data$trd_exctn_dt[1:502],
                       bond = data$p_mid[1:502],
                       index = data$spbond_close[1:502],
                       corr = NA)
width <- 30
rolling$corr[width:502]<-rollapply(rolling, width = width, function(x) cor(as.numeric(x[,2]), as.numeric(x[,3]), use = "pairwise.complete.obs"), by.column=FALSE)
ggplot(data=rolling, aes(x = date, y = corr)) + 
  geom_point() + 
  geom_hline(yintercept = 0)

#Plot indices logreturns together
indices <- data.frame(Date = data$trd_exctn_dt[1:502],
                      sp500 = data$sp500_logreturn[1:502],
                      spbond = data$spbond_logreturn[1:502],
                      russell3000 = data$russell_logreturn[1:502],
                      smallcap = data$smallcap_logreturn[1:502])
plot.sp500 <- ggplot(data=indices, aes(x=Date, y = sp500)) +
  geom_line(data = indices) +
  geom_hline(yintercept = 0) + 
  labs(y = "log-return", title = "S&P 500")
plot.spbond <- ggplot(data=indices, aes(x=Date, y=spbond))+
  geom_line(data=indices) + 
  geom_hline(yintercept = 0) + 
  labs(y = "log-return", title = "S&P Bond index")
plot.russell <- ggplot(data=indices, aes(x=Date, y=russell3000))+
  geom_line(data=indices) + 
  geom_hline(yintercept = 0) +
  labs(y = "log-return", title = "Russell 3000")
plot.smallcap <- ggplot(data=indices, aes(x=Date, y=smallcap))+
  geom_line(data=indices) + 
  geom_hline(yintercept = 0) + 
  labs(y = "log-return", title = "Dow Jones U.S. Small cap index")

ggarrange(plot.sp500, plot.russell, plot.smallcap, plot.spbond)

# log-returns bonds, problem is to delete first log-ret for each cusip? 
n <- length(unlist(data$p_mid))
lreturn <- log(data$p_mid[-1]/data$p_mid[-n])
data$logreturns <- 0
data$logreturns[2:nrow(data)] <- lreturn 

test <- data[1:502,]

ggplot(data=test, aes(x=trd_exctn_dt, y=logreturns, color = cusip_id)) + 
  geom_line(data = test)

log(data$p_avg[-1]/data$p_avg[-n])


# Bid-ask plot
subset$spread <- data$spread[1:10000]
subsubset <- subset[1:5000,]
ggplot(data = subsubset, aes(x = date, y = spread, color = cusip)) +
  geom_line(data = subsubset)

#d plot
subsubset$d <- data$d[1:5000]
subsubset$dbasis <- data$d_basis[1:5000]
subsubset$log_d <- log(subsubset$d)
ggplot(data = subsubset, aes(x = date, y = d, color = cusip)) +
  geom_line(data = subsubset)
ggplot(data = subsubset, aes(x = date, y = dbasis, color = cusip)) +
  geom_line(data = subsubset)
ggplot(data = subsubset, aes(x = date, y = log_d, color = cusip)) +
  geom_line(data = subsubset)

# Illiq plot
subsubset$illiq <- data$illiq_mid[1:5000]
ggplot(data = subsubset, aes(x = date, y = illiq, color = cusip)) +
  geom_line(data = subsubset) + 
  coord_cartesian(ylim = c(0, 1e-9))

# HHI plot
subsubset$HHI <- data$HHI[1:5000]
ggplot(data = subsubset, aes(x = date, y = HHI, color = cusip))+
  geom_line(data = subsubset)

# return index plot
subsubset$rindex <- data$return_index[1:5000]
ggplot(data = subsubset, aes(x = date, y = rindex, color = cusip)) + 
  geom_line(data = subsubset ) + 
  coord_cartesian(ylim = c(80, 150))

# Counting number of trades for each bond (cusip) data.full = intraday data, data = daily data. 
data %>% count(cusip_id, sort = TRUE)
data.full %>% count(cusip_id, sort = TRUE)


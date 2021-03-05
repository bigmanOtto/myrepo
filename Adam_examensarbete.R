install.packages("ggplot2")
library(ggplot2)
library(readxl)
library(ggpubr)

data <- read.csv("DAILY_all.csv")

data$trd_exctn_dt <- as.Date.character(data$trd_exctn_dt, format = "%Y%m%d")
subset <- data.frame(p = unlist(data$p_avg[1:10000]),  
                     date = unlist(data$trd_exctn_dt[1:10000]),
                     cusip = unlist(data$ï..cusip_id[1:10000]))

ggplot(data = subset, aes(x=date, y=p, color=cusip)) +
    geom_line(data = subset, size = 0.5)


# correlation first bond and index
index <- data.frame(sp500close = data$sp500_close[1:502],
                    sp500_logreturn = data$sp500_logreturn[1:502],
                    date = data$trd_exctn_dt[1:502])

bond <- data[1:502,1:42]

plot.sp500 <- ggplot(data = index, aes(x = date, y = sp500close))+
  geom_line(data = index, color = "red") + 
  coord_cartesian(ylim = c(2200, 3000))

plot.bond <- ggplot(data = bond, aes(x=trd_exctn_dt, y=p_mid))+
  geom_line(data = bond) 

ggarrange(plot.sp500, plot.bond)

cor.test(bond$p_mid, index$close)
 
#rolling correlation first bond and index
bond_index_correlation <- function(x, window) {
  merged_xts <- merge(x, index$close)
  merged_xts$rolling_test <- rollapply(merged_xts, window,
                                       function(x) cor(x[,1], x[,2], use = "pairwise.complete.obs"),
                                       by.column = FALSE)
  names(merged_xts)<-c("Bond price", "Index price", "Correlation")
  merged_xts
}

bond_price <- data.frame(price = bond$p_mid,
                         close = Index_data$close)
bond_index_correlation(bond_price,20)

install.packages("ggplot2")
library(ggplot2)
library(readxl)

data <- read.csv("TRACE_datastream_DAILY1.csv")

data$trd_exctn_dt <- as.Date.character(data$trd_exctn_dt, format = "%Y%m%d")
subset <- data.frame(p = unlist(data$p_avg[1:10000]),  
                     date = unlist(data$trd_exctn_dt[1:10000]),
                     cusip = unlist(data$ï..cusip_id[1:10000]))

ggplot(data = subset, aes(x=date, y=p, color=cusip))+
ggplot(data = subset, aes(x=date, y=p, color=cusip)) +
    geom_line(data = subset, size = 0.5)


# correlation first bond and index
index <- Index_data[1:502,]
index$date <- as.Date.character(index$date, format = "%Y%m%d")
index$logreturn <- log(index$return)

bond <- data[1:502,1:29]

ggplot(data = index, aes(x = date, y = close))+
  geom_point(data = index, size = 0.5) + 
  geom_smooth(data = index)

ggplot(data = bond, aes(x=trd_exctn_dt, y=p_mid))+
  geom_point(data = bond, size = 0.5)+
  geom_smooth(data = bond)

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

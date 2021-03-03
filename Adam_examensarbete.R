install.packages("ggplot2")
library(ggplot2)

data <- read.csv("TRACE_datastream_DAILY1.csv")

data$trd_exctn_dt <- as.Date.character(data$trd_exctn_dt, format = "%Y%m%d")
subset <- data.frame(p = unlist(data$p_avg[1:10000]),  
                     date = unlist(data$trd_exctn_dt[1:10000]),
                     cusip = unlist(data$ï..cusip_id[1:10000]))

ggplot(data = subset, aes(x=date, y=p, color=cusip))+
    geom_point(data = subset, size = 0.5)



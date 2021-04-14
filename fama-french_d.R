ff <- FamaF[126:627,]

smb <- ff$SMB

d <- DAILY_all$d[1:502]

##merge ff and daily
data <- merge(DAILY_all,ff, by.x="trd_exctn_dt", by.y="Date",sort=FALSE)

data <- data[order(data$cusip_id),]

##calculate returns
n <- length(unlist(data$p_avg))
returns <- (data$p_avg[-1]/data$p_avg[-n]- data$Rf[-n])-1 
data <- data[2:77084,]
data$returns <- returns

data <- data %>% group_by(cusip_id) %>%
  mutate( returns = case_when(row_number() < 2 ~ NA_real_, TRUE ~ returns))

names(data)[names(data) == "Rm-Rf"] <- "Rm_Rf"

names(data)[1] <- paste("cusip_id")
data$trd_exctn_dt <- as.Date.character(data$trd_exctn_dt, format = "%Y%m%d")
is_significant <- 0.05


##ff regression
model.1 <- lmList(returns ~ SMB+HML+Rm_Rf | cusip_id, data = data)
model.1.data <- data.frame(coef = coefficients(model.1),
                           conf = confint(model.1),
                           p_value = summary(model.1)$coef[,4,2],
                           r_squared = summary(model.1)$r.squared,
                           r_squared_adj = matrix(unlist(summary(model.1)$adj.r.squared), nrow=length(summary(model.1)$adj.r.squared), byrow=TRUE))
model.1.data$cusip <- row.names(model.1.data)

model.1.hist <- hist(model.1.data$coef.p_avg, xlim = c(-6*10^-9, 1*10^-9))
model.1.significant <- data.frame(significant = sum(model.1.data$p_value < is_significant),
                                  non_significant = sum(model.1.data$p_value >= is_significant))

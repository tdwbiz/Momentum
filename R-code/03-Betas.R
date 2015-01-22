# Market betas

library(dplyr)
library(ggplot2)
library(reshape2)
library(quantmod)
source('~/GitHub/Momentum/R-code/utils.R')

# mkt <- read.table("~/GitHub/Momentum/Data/F-F_Research_Data_Factors_daily.txt", header=TRUE, quote="\"")
# mkt$Date <- as.Date(as.character(mkt$Date), format="%Y%m%d")

getSymbols("^GSPC", src = "yahoo", from = as.Date("2012-01-01"), to = as.Date("2014-12-31")) 
getSymbols("AAPL",src="yahoo", from = as.Date("2012-01-01"), to = as.Date("2014-12-31")) 
getSymbols("GOOG",src="google", from = as.Date("2012-01-01"), to = as.Date("2014-12-31")) 

aapl_betas <- getBetas(AAPL,GSPC$GSPC.Close)
goog_betas <- getBetas(GOOG, GSPC$GSPC.Close)

aapl_betas %>% inner_join(goog_betas, by="Date") %>%
  transmute(Date = as.Date(Date, format="%Y-%m-%d"),
            AAPL = betas.x,
            GOOG = betas.y) %>%
  melt(id.vars="Date", value.name="Beta") %>%
  ggplot(aes(Date, Beta)) + geom_line(aes(colour=variable)) +
  labs(colour="Company") + theme_bw()

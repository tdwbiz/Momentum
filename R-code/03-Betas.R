# Market betas

library(dplyr)
library(ggplot2)
library(reshape2)
library(quantmod)
source('~/GitHub/Momentum/R-code/utils.R')

mkt <- read.table("~/GitHub/Momentum/Data/F-F_Research_Data_Factors_daily.txt", header=TRUE, quote="\"")
mkt$Date <- as.Date(as.character(mkt$Date), format="%Y%m%d")

getSymbols("AAPL",src="yahoo", from = as.Date("2012-01-01"), to = as.Date("2014-12-31")) 
getSymbols("^GSPC", src = "yahoo", from = as.Date("2012-01-01"), to = as.Date("2014-12-31")) 

AAPL$Close <- AAPL$AAPL.Close

x <- getBetas(AAPL,GSPC$GSPC.Close)

x %>% ggplot(aes(Date, betas)) + 
  geom_line() + theme_bw() 
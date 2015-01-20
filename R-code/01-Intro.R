# Introduction

source('~/GitHub/Momentum/R-code/utils.R')

# First two figures in introduction - log cumulative return of small and big momentum stocks
####################################################################################################

mom <- read.table("~/GitHub/Momentum/Data/25_Portfolios_ME_Prior_12_2.txt", header=TRUE, quote="\"")
size <- read.table("~/GitHub/Momentum/Data/Portfolios_Formed_on_ME.txt", header=TRUE, quote="\"")
head(mom)

# demonstrates how to convert to date
date <- "192703"
gsub('^([0-9]{4})([0-9]+)', '\\1-\\2-01', date)

mom$Date <- gsub('^([0-9]{4})([0-9]+)', '\\1-\\2-01', mom$Date)

library(dplyr)
mom <- mom %>% select(c(1, 6, 26))
names(mom) <- c("Date", "SH", "BH")

size <- size %>% select(c(1,6,10)) %>% slice(7:1057)
size$Date <- gsub('^([0-9]{4})([0-9]+)', '\\1-\\2-01', size$Date)
#size <- size[7:1057,]

# join sets together
mom <- inner_join(size,mom, by="Date")

# create column of cumulative returns and cumulative log returns
mom <- mom %>% transmute(Date = as.Date(Date, format="%Y-%m-%d"),
                         Lo20 = Lo20 / 100,
                         Hi20 = Hi20 / 100,
                         SH = SH / 100,
                         BH = BH / 100)

mom$CumLo20 <- cumret(mom$Lo20)
mom$CumHi20 <- cumret(mom$Hi20)
mom$CumSH <- cumret(mom$SH)
mom$CumBH <- cumret(mom$BH)

# convert to log cumulative returns
mom <- mom %>% mutate(LogCumLo20 = log(CumLo20),
                      LogCumHi20 = log(CumHi20),
                      LogCumSH = log(CumSH),
                      LogCumBH = log(CumBH))

# plot log cumulative returns
library(ggplot2)
library(reshape2)

# 01-Lo20
mom %>% select(c(1,10,12)) %>% melt(id.vars="Date", value.name="CumReturn") %>%
  ggplot(aes(Date, CumReturn)) + 
  geom_line(aes(colour=factor(variable, labels=c("Low 20 ME","Momentum")))) +
  labs(colour="Portfolio") + theme_bw() + ylab("Log cumulative return")

# 02-Hi20
mom %>% select(c(1,11,13)) %>% melt(id.vars="Date", value.name="CumReturn") %>%
  ggplot(aes(Date, CumReturn)) + 
  geom_line(aes(colour=factor(variable, labels=c("High 20 ME","Momentum")))) +
  labs(colour="Portfolio") + theme_bw() + ylab("Log cumulative return")

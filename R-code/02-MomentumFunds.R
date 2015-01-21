# momentum funds
library(dplyr)
library(reshape2)
library(ggplot2)

# AMOMX 
########################################################################
SPDR <- read.csv("~/GitHub/Momentum/Data/SPDR.csv")
AMOMX <- read.csv("~/GitHub/Momentum/Data/AMOMX.csv")

AMOMX$Date <- as.Date(AMOMX$Date, format="%m/%d/%Y")
SPDR$Date <- as.Date(SPDR$Date, format="%m/%d/%Y")

# join data and calculate returns

mom <- AMOMX %>% inner_join(SPDR, by="Date") %>%
  select(c(1,5,10)) %>%
  rename(AMOMXclose = Close.x,
         SPDRclose = Close.y)

mom <- mom[order(mom$Date),]

N <- nrow(mom)
mom$AMOMXr <- c(0, rep(NA, N-1))
mom$SPDRr <- c(0, rep(NA, N-1))
for (i in 2:N) {
  mom$AMOMXr[i] <- (mom$AMOMXclose[i] - mom$AMOMXclose[i-1]) / mom$AMOMXclose[i-1]
  mom$SPDRr[i] <- (mom$SPDRclose[i] - mom$SPDRclose[i-1]) / mom$SPDRclose[i-1]
}

mom$AMOMXcr <- cumret(mom$AMOMXr)
mom$SPDRcr <- cumret(mom$SPDRr)

mom %>% select(c(1,6,7)) %>% melt(id.vars="Date", value.name="CumReturn") %>%
  ggplot(aes(Date, CumReturn)) + 
  geom_line(aes(colour=variable)) +
  labs(colour="Portfolio") + theme_bw() + ylab("Cumulative return")

# BRSMX
################################################################
SPDR <- read.csv("~/GitHub/Momentum/Data/SPDR.csv")
BRSMX <- read.csv("~/GitHub/Momentum/Data/BRSMX.csv")

BRSMX$Date <- as.Date(BRSMX$Date, format="%m/%d/%Y")
SPDR$Date <- as.Date(SPDR$Date, format="%m/%d/%Y")

# join data and calculate returns

mom <- BRSMX %>% inner_join(SPDR, by="Date") %>%
  select(c(1,5,10)) %>%
  rename(BRSMXclose = Close.x,
         SPDRclose = Close.y)

mom <- mom[order(mom$Date),]

N <- nrow(mom)
mom$BRSMXr <- c(0, rep(NA, N-1))
mom$SPDRr <- c(0, rep(NA, N-1))
for (i in 2:N) {
  mom$BRSMXr[i] <- (mom$BRSMXclose[i] - mom$BRSMXclose[i-1]) / mom$BRSMXclose[i-1]
  mom$SPDRr[i] <- (mom$SPDRclose[i] - mom$SPDRclose[i-1]) / mom$SPDRclose[i-1]
}

mom$BRSMXcr <- cumret(mom$BRSMXr)
mom$SPDRcr <- cumret(mom$SPDRr)

mom %>% select(c(1,6,7)) %>% melt(id.vars="Date", value.name="CumReturn") %>%
  ggplot(aes(Date, CumReturn)) + 
  geom_line(aes(colour=variable)) +
  labs(colour="Portfolio") + theme_bw() + ylab("Cumulative return")

# PDP
################################################################
SPDR <- read.csv("~/GitHub/Momentum/Data/SPDR.csv")
PDP <- read.csv("~/GitHub/Momentum/Data/PDP.csv")

PDP$Date <- as.Date(PDP$Date, format="%m/%d/%Y")
SPDR$Date <- as.Date(SPDR$Date, format="%m/%d/%Y")

# join data and calculate returns

mom <- PDP %>% inner_join(SPDR, by="Date") %>%
  select(c(1,5,10)) %>%
  rename(PDPclose = Close.x,
         SPDRclose = Close.y)

mom <- mom[order(mom$Date),]

N <- nrow(mom)
mom$PDPr <- c(0, rep(NA, N-1))
mom$SPDRr <- c(0, rep(NA, N-1))
for (i in 2:N) {
  mom$PDPr[i] <- (mom$PDPclose[i] - mom$PDPclose[i-1]) / mom$PDPclose[i-1]
  mom$SPDRr[i] <- (mom$SPDRclose[i] - mom$SPDRclose[i-1]) / mom$SPDRclose[i-1]
}

mom$PDPcr <- cumret(mom$PDPr)
mom$SPDRcr <- cumret(mom$SPDRr)

mom %>% select(c(1,6,7)) %>% melt(id.vars="Date", value.name="CumReturn") %>%
  ggplot(aes(Date, CumReturn)) + 
  geom_line(aes(colour=variable)) +
  labs(colour="Portfolio") + theme_bw() + ylab("Cumulative return")

# RYAMX
################################################################
SPDR <- read.csv("~/GitHub/Momentum/Data/SPDR.csv")
RYAMX <- read.csv("~/GitHub/Momentum/Data/RYAMX.csv")

RYAMX$Date <- as.Date(RYAMX$Date, format="%m/%d/%Y")
SPDR$Date <- as.Date(SPDR$Date, format="%m/%d/%Y")

# join data and calculate returns

mom <- RYAMX %>% inner_join(SPDR, by="Date") %>%
  select(c(1,5,10)) %>%
  rename(RYAMXclose = Close.x,
         SPDRclose = Close.y)

mom <- mom[order(mom$Date),]

N <- nrow(mom)
mom$RYAMXr <- c(0, rep(NA, N-1))
mom$SPDRr <- c(0, rep(NA, N-1))
for (i in 2:N) {
  mom$RYAMXr[i] <- (mom$RYAMXclose[i] - mom$RYAMXclose[i-1]) / mom$RYAMXclose[i-1]
  mom$SPDRr[i] <- (mom$SPDRclose[i] - mom$SPDRclose[i-1]) / mom$SPDRclose[i-1]
}

mom$RYAMXcr <- cumret(mom$RYAMXr)
mom$SPDRcr <- cumret(mom$SPDRr)

mom %>% select(c(1,6,7)) %>% melt(id.vars="Date", value.name="CumReturn") %>%
  ggplot(aes(Date, CumReturn)) + 
  geom_line(aes(colour=variable)) +
  labs(colour="Portfolio") + theme_bw() + ylab("Cumulative return")

# momentum funds
library(dplyr)
library(reshape2)
library(ggplot2)

SPDR <- read.csv("~/GitHub/Momentum/Data/SPDR.csv")
AMOMX <- read.csv("~/GitHub/Momentum/Data/AMOMX.csv")

AMOMX$Date <- as.Date(AMOMX$Date, format="%m/%d/%Y")
SPDR$Date <- as.Date(SPDR$Date, format="%m/%d/%Y")

# join data and calculate returns

mom <- AMOMX %>% inner_join(SPDR, by="Date") %>%
  select(c(1,5,10)) %>%
  rename(AMOMXclose = Close.x,
         SPDRclose = Close.y)
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
  labs(colour="Portfolio") + theme_bw() + ylab("Log cumulative return")
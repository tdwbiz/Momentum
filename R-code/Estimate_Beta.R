MomMktDaily2 <- read.csv("P:/Momentum/Data/MomMktDaily2.csv")
attach(MomMktDaily2)
allBetaL=c() #loser portfolio
allBetaH=c() #winner portfolio
myDates=c()
Date <- as.Date(Date,"%m/%d/%Y")
for (t in 1235:5428) #1235:5428 (1931-1945), 19261:22280 (1999-2011) 21819:22280 22926
{
  T=(t-125)
  myMkt=Mkt[T:t]
  myMkt1=Mkt1[T:t]
  myMkt2=Mkt2[T:t]
  myMkt3=Mkt3[T:t]
  myMkt4=Mkt4[T:t]
  myMkt5=Mkt5[T:t]
  myMkt6=Mkt6[T:t]
  myMkt7=Mkt7[T:t]
  myMkt8=Mkt8[T:t]
  myMkt9=Mkt9[T:t]
  myMkt10=Mkt10[T:t]
  myLow10=Low10[T:t]
  myHigh10=High10[T:t]
  regL=lm(myLow10~myMkt+myMkt1+myMkt2+myMkt3+myMkt4+myMkt5+myMkt6+myMkt7+myMkt8+myMkt9+myMkt10)
  regH=lm(myHigh10~myMkt+myMkt1+myMkt2+myMkt3+myMkt4+myMkt5+myMkt6+myMkt7+myMkt8+myMkt9+myMkt10)
  betaL=sum(regL$coeff[2:11]) #sum of coeff. except intercept
  betaH=sum(regH$coeff[2:11])
  myDates=append(myDates,Date[t])
  allBetaL=append(allBetaL,betaL)
  allBetaH=append(allBetaH,betaH)
}
len=length(myDates)
####Plotting#######
plot(myDates,allBetaL,type="l",col="blue",xaxt="n",ann="F") #"xaxt='n'" for no axis values
axis.Date(1,at=seq(myDates[1],myDates[len],length.out=25),
          format="%b-%Y",las=2) #las=2 for vertical labels
title(ylab="Rolling 6 Month Estimated Beta")
par(new=T) #for adding new data to existing plot
plot(allBetaH,type="l",axes=F,col="red",ann=F) #"ann=F" for no axis label
legend("topleft","Legend",c("Highest Decile","Lowest Decile"),
       lty=c(1,1),col=c("red","blue"),cex=0.7) #"cex" for scale factor
par(new=F)
detach(MomMktDaily2)
#write.csv(allBetaH,file="P:/Momentum/High10Betas.csv")
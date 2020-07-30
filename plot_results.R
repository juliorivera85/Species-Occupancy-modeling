##this bit of code is to plot the output from Mark and plot the 
##parameter estimates for the results of the 3 top models. 
##using SE and not CI for the plots. 
## p = detection and psi= occupancy

setwd("C:/Users/julio/Dropbox/Postdoc/Mexico27/May Trip 2020/AZ occu modeling/mark")

#read in the data files
p_mod1 <- read.csv("mod1_p.csv")
p_mod2 <- read.csv("mod2_p.csv")
p_mod3 <- read.csv("mod3_p.csv")
psi_mod1 <- read.csv("mod1_psi.csv")
psi_mod2 <- read.csv("mod2_psi.csv")
psi_mod3 <- read.csv("mod3_psi.csv")

##make the  bounds of the SE
#model 1
p_mod1$SE.top <- p_mod1$p + p_mod1$SE
p_mod1$SE.bot <- p_mod1$p - p_mod1$SE

psi_mod1$SE.top <- psi_mod1$psi + psi_mod1$SE
psi_mod1$SE.bot <- psi_mod1$psi - psi_mod1$SE

#model 2
p_mod2$SE.top <- p_mod2$p + p_mod2$SE
p_mod2$SE.bot <- p_mod2$p - p_mod2$SE

psi_mod2$SE.top <- psi_mod2$psi + psi_mod2$SE
psi_mod2$SE.bot <- psi_mod2$psi - psi_mod2$SE

#model 3
p_mod3$SE.top <- p_mod3$p + p_mod3$SE
p_mod3$SE.bot <- p_mod3$p - p_mod3$SE

psi_mod3$SE.top <- psi_mod3$psi + psi_mod3$SE
psi_mod3$SE.bot <- psi_mod3$psi - psi_mod3$SE

######
#For psi model 1, we are going to truncate the data because 
#probability doesn't increase past 800 cm of tree height
######
psi_mod1 <- psi_mod1[c(1:65),]

#plot the data in one chart
#this will plot 3 rows and 2 columns so that the p and psi 
#are on the same row and different rows represent different
#columns
par(mfrow=c(3,2))

#model 1
plot(p_mod1$RH, p_mod1$p, type="l", lwd=2, xlab="Relative humidity", 
ylab="Detection probability", main = expression(paste(italic('p'),"(RH)",psi,"(TH)")))
text(9, 0.8, labels=expression(paste(Delta,"AICc=0.00")))
text(9, 0.7, labels="AICc Weight=0.71")
lines(p_mod1$RH, p_mod1$SE.top, type="l", col="red", lty=5)
lines(p_mod1$RH, p_mod1$SE.bot, type="l", col="red", lty=5)


plot(psi_mod1$TH, psi_mod1$psi, type="l", lwd=2, xlab="Tree height (cm)", 
ylab="Occupancy probability",  main = expression(paste(italic('p'),"(RH)",psi,"(TH)")), ylim=c(0,1.2))
lines(psi_mod1$TH, psi_mod1$SE.top, type="l", col="red", lty=5)
lines(psi_mod1$TH, psi_mod1$SE.bot, type="l", col="red", lty=5)


#model 2
plot(p_mod2$RH, p_mod2$p, type="l", lwd=2, xlab="Relative humidity", 
ylab="Detection probability", main = expression(paste(italic('p'),"(RH)",psi,"(EL)")))
text(9, 0.8, labels=expression(paste(Delta,"AICc=2.6" )))
text(9, 0.7, labels="AICc Weight=0.19")
lines(p_mod2$RH, p_mod2$SE.top, type="l", col="red", lty=5)
lines(p_mod2$RH, p_mod2$SE.bot, type="l", col="red", lty=5)


plot(psi_mod2$EL, psi_mod2$psi, type="l", lwd=2, xlab="Elevation (m)", 
ylab="Occupancy probability", main = expression(paste(italic('p'),"(RH)",psi,"(EL)")), ylim=c(-0.1,1.2))
lines(psi_mod2$EL, psi_mod2$SE.top, type="l", col="red", lty=5)
lines(psi_mod2$EL, psi_mod2$SE.bot, type="l", col="red", lty=5)

#model 3
plot(p_mod3$RH, p_mod3$p, type="l", lwd=2, xlab="Relative humidity", 
ylab="Detection probability", main = expression(paste(italic('p'),"(RH)",psi,"(TC)")))
text(9, 0.8, labels=expression(paste(Delta,"AICc=4.8" )))
text(9, 0.7, labels="AICc Weight=0.07")
lines(p_mod3$RH, p_mod3$SE.top, type="l", col="red", lty=5)
lines(p_mod3$RH, p_mod3$SE.bot, type="l", col="red", lty=5)


plot(psi_mod3$TC, psi_mod3$psi, type="l", lwd=2, xlab="Tree cover", 
ylab="Occupancy probability",  main = expression(paste(italic('p'),"(RH)",psi,"(TC)")), ylim=c(0,1.2))
lines(psi_mod3$TC, psi_mod3$SE.top, type="l", col="red", lty=5)
lines(psi_mod3$TC, psi_mod3$SE.bot, type="l", col="red", lty=5)




##################################
# Bootstrap
##################################

data(trees)
help(trees)
dim(trees)
trees[1:10,]

plot(trees$Volume~trees$Height, main="Black Cherry Tres", xlab="Height", ylab="volume", pch=16, col='blue')
plot(trees$Volume~trees$Girth, main="Black Cherry Tres", xlab="Girth", ylab="volume", pch=16, col='blue')

mean(trees$Volume)
sd(trees$Volume)

R=1000
boot_out=numeric(R)
for(i in 1:R){
  print(i)
  #i=1
  temp=sample(trees$Volume,size=31, replace=T)
  boot_out[i]=mean(temp)
}
hist(boot_out)
quantile(boot_out, c(0.05, 0.95))



##################################
# Permutation
##################################
data(sleep)
sleep
gr1=sleep[1:10,1]
gr2=sleep[11:20,1]
mean(gr1)
mean(gr2)

tapply(sleep$extra, sleep$group,mean)
tapply(sleep$extra, sleep$group,sd)

obst=t.test(sleep$extra~sleep$group, var.eq=T)$statistic
t.test(gr1, gr2, var.eq=T)

R=100
t.val=numeric(R)
for(i in 1:R){
  index=sample(1:20,10,replace=F)
  g1=sleep$extra[index]
  g2=sleep$extra[-index]
  t.val[i]=t.test(g1,g2,var.eq=T)$statistic
}

#obst=t.test(sleep$extra~sleep$group, var.eq=T)$statistic
#length(which(abs(t.val)>abs(obst)))/R
mean(abs(t.val)>abs(obst))


##################################
# Markov Chains
##################################
#install.packages("markovchain")
library('markovchain')

weatherStates=c("sunny","cloudy", "rain")
weatherMatrix=matrix(data=c(0.7, 0.2, 0.1, 0.3, 0.4, 0.3, 0.2, 0.45, 0.35), nrow=3, byrow=T)
dimnames=list(weatherStates, weatherStates)
mcWeather=new('markovchain', states=weatherStates,byrow=T, transitionMatrix=weatherMatrix, name="weather")
plot(mcWeather)

init=c(0,1,0)
day2=init*mcWeather^2
day7=init*mcWeather^7
steadyStates(mcWeather)

rainTS=matrix(0, nrow=30, ncol=3)
for(i in 1:30){
  rainTS[i,]=init*mcWeather^i
}
plot(1:30, rainTS[,3])
abline(h=steadyStates(mcWeather)[3], col=2)


#building MC from real data
data(rain)
rain.ts=rain$rain
states=unique(rain.ts)
smat=createSequenceMatrix(rain.ts)
apply(smat,1,sum)
length(which(rain.ts=='0'))
Tprob=markovchainFit(data=rain.ts, confidencelevel = 0.95)$estimate
simrain=rmarkovchain(n=365, object=Tprob, t0='1-5')

##################################
# linear regression
##################################
data(trees)

lm1=lm(trees$Volume~trees$Girth) #y~x means y as a funciton of x
summary(lm1)
attributes(summary(lm1))
summary(lm1)$r.squared
lm1$coefficients
summary(lm1)$coefficients

dev.off()  #this turns off the plottig window from the MC plot so that when a new plot window opens it will reset the plotting parameters
plot(trees$Volume~trees$Girth, main='Black Cherry Tree', 
     xlab="Girth (inches)", ylab="Volume (cubic feet)",
     pch=8, col='blue', cex=0.5, cex.main=0.7, 
     cex.lab=0.9,axes=F)
atpoint=c(10, 15, 20)
axis(1, at=atpoint, labels=atpoint, ticks=T, las=2)
axis(2)
box()
abline(lm(trees$Volume~trees$Girth), col=2, lwd=2, lty=3)
points(trees$Girth, trees$Volume, pch=8, cex=0.5, col='blue')
text1=paste("Intercept = ", round(lm1$coefficients[1],1))
text(8, 73, text1, cex=0.6, pos=4)

lm1$residuals
plot(lm1$residuals~trees$Girth, main='Black Cherry Tree', 
     xlab="Girth (inches)", ylab="Volume (cubic feet)",
     pch=8, col='blue', cex=0.5, cex.main=0.7, 
     cex.lab=0.9,axes=T)
lm2=lm(lm1$residuals~trees$Girth)
abline(lm2, col=4, lty=4)

# R - session 3/21
l=50 #length of our ts
ts1=rnorm(l,0,1)
plot(1:l, ts1, type="l", xlab="time", ylab='value')
abline(h=0, lty=3, col='red')

#Adding autocorrelation
#par(mfrow=c(3,1))
beta=0.9
ts2=numeric(l)
for(i in 2:l){
  ts2[i]=ts2[i-1]*beta+ts1[i]
}
plot(1:l, ts2, type='l', xlab="time", ylab='value')


#Now make this an AR(2) model
par(mfrow=c(1,1))
beta1=0.8
beta2=0.19
ts2=numeric(l)
for(i in 3:l){
  ts2[i]=ts2[i-1]*beta1+ts2[i-2]*beta2+ts1[i]
}
plot(1:l, ts2, type='l', xlab="time", ylab='value')

### Fitting to real data
data(Nile)
class(Nile)

#making our own timeseries
dataN=as.vector(Nile)
Nilets=ts(dataN, start=c(1871,1), end=c(1970,1), frequency=1)

#Try fitting a linear model with the lagged data
plot(Nilets[2:100]~Nilets[1:99], ylab="x(t+1)", xlab="x(t)")
lm3=lm(Nilets[2:100]~Nilets[1:99])
abline(lm3, col=4)
summary(lm3)
cor(Nilets[2:100], Nilets[1:99])

#plot the residuals vs fit and look at autocorrelation in the residuals
plot(lm3$residuals~lm3$fit)

#Fitting AR models with AR function
ARmod1=ar.ols(Nilets, order.max=1, demean=T)
Armod=ar(Nilets, demean=T, aic=T, order.max=50)
Armod$ar
Armod$order
Armod$aic
Armod$var.pred #variacne of the errors
mean(Armod$resid, na.rm=T)

#Fitting an ARMA Model:
zz=arima0(Nilets, order=c(2,0,0)) #AR(2) model
zz=arima0(Nilets, order=c(2,0,1)) #ARMA(2,1)
zz$coef
zz$sigma2  #variance of residuals using MLE estimate

#### Spectral analysis
l=128
t=seq(0,128,0.1)
k=3
a=1
y=a*sin(2*pi*k/l*t)
plot(t,y, type='l')

k=2
a=0.25
y=a*sin(2*pi*k/l*t)
lines(t,y, col='blue')

klist=c(5,7,19,36)
ampS=c(2,0.5, 1, 5)
ampC=c(5,1,1,1)

outputS=outputC=matrix(0,nrow=length(t), ncol=4)
#par(mfrow=c(length(klist)+1,1), mar=c(1,1,1,1))
for(i in 1:(length(klist))){
	outputS[,i]=ampS[i]*sin(2*pi*klist[i]/l*t)
	outputC[,i]=ampC[i]*cos(2*pi*klist[i]/l*t)
	#quartz()
	plot(t,outputS[,i], type='l', col='blue', ylim=c(-5,5), ylab="amp")
	lines(t, outputC[,i], col="green")
}

result=apply(outputS,1,sum)+apply(outputC,1,sum)
plot(t, result, type='l', col='black', ylab="amp")


#fourier transform to make a periodogram
fft=abs(fft(result)/sqrt(128))^2
P=1/128*fft[1:65]
f=(0:64)/128
plot(f,P, type='l')
abline(v=klist/128, col=2)


data(Nile)
quartz()
spectrum(Nile, log='no', taper=0)






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














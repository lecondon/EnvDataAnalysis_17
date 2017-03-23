# R - session 3/21
l=50 #length of our ts
ts1=rnorm(l,0,1)
plot(1:l, ts1, type="l", xlab="time", ylab='value')
abline(h=0, lty=3, col='red')

#Adding autocorrelation
par(mfrow=c(1,1))
beta=1.1
ts2=numeric(l)
for(i in 2:l){
  ts2[i]=ts2[i-1]*beta+ts1[i]
}
plot(1:l, ts2, type='l', xlab="time", ylab='value')


#Now make this an AR(2) model
par(mfrow=c(1,1))
beta1=0.9
beta2=0.1
ts2=numeric(l)
for(i in 3:l){
  ts2[i]=ts2[i-1]*beta1+ts2[i-2]*beta2+ts1[i]
}
plot(1:l, ts2, type='l', xlab="time", ylab='value')

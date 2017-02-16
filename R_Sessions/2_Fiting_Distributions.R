# Qualitatively and quantitatively evaluating parametric PDFs fit to data
# Adapted from courses course material by Dr. Balaji Rajagopalan

library(MASS)

####################################################################################
# Read in Data
####################################################################################
data=matrix(scan("aismr.txt"),ncol=13,byrow=T)
#Indian monthly rainfall
#first column is the year next columns are monthly rainfall values
xdata=data[,11] #selecting one month of data 
N=length(xdata)		#number of data points.

#make a series of points at which to estimate the PDF from
xeval=seq(min(xdata)-sd(xdata),max(xdata)+sd(xdata),length=100)
neval=length(xeval)

####################################################################################
# Estimate the PDF of the selected model at at the evaluation points 
####################################################################################
#### Select a model (i.e., PDF) to fit to the data and 
# diagnostics and goodness of fit exercise - using visual and KS tests

### If you decide on Gamma or Weibull you have to fit it to the data
# to obtain the parameters

#zgamma=fitdistr(xdata,"gamma")
#zweibull=fitdistr(xdata,"weibull")


#lognormal
xdensityorig=dlnorm(xeval,meanlog=mean(log(xdata)), sdlog=sd(log(xdata)))

#normal
#xdensityorig=dnorm(xeval,meanlog=mean(xdata), sdlog=sd(xdata))

#exponential
#xdensityorig=dexp(xeval,rate=1/mean(xdata))

# Gamma
#xdensityorig=dgamma(xeval,shape=zgamma$estimate[1],scale=1/zgamma$estimate[2])

# Weibull
#xdensityorig=dweibull(xeval,shape=zweibull$estimate[1],scale=zweibull$estimate[2])

####################################################################################
# Compare the parametric distribution to the data
####################################################################################
par(mfrow=c(2,2))

zz=hist(xdata,probability=T,plot=FALSE)
hist(xdata,xlab="Monthly rainfall", ylab="", probability=T,
main="",ylim=range(c(zz$density,xdensityorig)))
lines(xeval, xdensityorig, col="red")
title(main="Fitted PDF")

# Empirical or SAMPLE quantiles and Empirical Percentiles..
empquant = sort(xdata)
emppercent = 1:N/(N+1)		#Weibull plotting position

# Get the quantiles corresponding to the empirical percentiles from the fitted
# PDF model

# Also get the model percentiles corresponding to the empirical quantiles 
# - i.e., (sorted data) from the fitted PDF model.
xdatasort = sort(xdata)		#Sorted original data

#If lognormal
modquant = qlnorm(emppercent,meanlog=mean(log(xdata)), sdlog=sd(log(xdata)))
modpercent = plnorm(xdatasort,meanlog=mean(log(xdata)), sdlog=sd(log(xdata)))

#normal
#modquant = qnorm(emppercent,mean=mean(xdata), sd=sd(xdata))
#modpercent = pnorm(xdatasort,mean=mean(xdata), sd=sd(xdata))

#exponential
#modquant = qexp(emppercent,rate=1/mean(xdata))
#modpercent = pexp(xdatasort,rate=1/mean(xdata))

# Gamma
#modquant=qgamma(emppercent,shape=zgamma$estimate[1],scale=1/zgamma$estimate[2])
#modpercent=pgamma(xdatasort,shape=zgamma$estimate[1],scale=1/zgamma$estimate[2])

# Weibull
#modquant=qweibull(emppercent,shape=zweibull$estimate[1],scale=zweibull$estimate[2])
#modpercent=pweibull(xdatasort,shape=zweibull$estimate[1],scale=zweibull$estimate[2])

# Plot the Empirical CDF with the Model CDF
plot(xdatasort, emppercent, xlab="Rainfall", ylab="CDF (F(x))")
lines(xdatasort, modpercent, col="red")

#Quantile plot..
# For a good fit the scatterplots should be on the 1:1 line
plot(modquant, empquant, xlab="Model (or Theoretical) Quantiles", ylab="Emp.
Quantiles")
lines(modquant, modquant)

#Probability Plot..
plot(modpercent, emppercent, xlab="Model (or Theoretical) Percentiles", ylab="Emp.
Percentiles")
lines(modpercent, modpercent)

# Alternate approach: 
# Normal Q-Q plot with the qqnorm function
# Plots the theoretical (or model) quantiles from a Standard Normal PDF
# vs the sample quantiles
#quartz()
#zz = qqnorm(xdata, xlab="Theoretical Quantiles", ylab="Empirical Quantiles")
#points(zz$x, zz$y)

####################################################################################
# K-S test for fit
####################################################################################
#Doing K-S tests for fit
#do a help on ks.test 
#ks.test command requires that you give the
#appropriate parameters of the distribution

## do a K-S test for Normality
zz=ks.test(xdata,"pnorm",mean=mean(xdata), sd=sd(xdata))

zz$statistic # This is the D calculated from the K-S test
zz$p.value #gives you the p-value of the test. If this is less than alpha
	   # reject the null hypothesis - i.e. data is not from the model

## do a K-S test for lognormal
zz=ks.test(xdata,"plnorm",mean=mean(log(xdata)), sd=sd(log(xdata)))

## do a K-S test for Gamma
zgamma=fitdistr(xdata, dgamma, list(shape = 1, rate = 0.1), lower = 0.01)
zz=ks.test(xdata,"pgamma",shape=zgamma$estimate[1], scale=1/zgamma$estimate[2])
zz$p.value 

## do a K-S test for Weibull
zweibull=fitdistr(xdata, "weibull", list(shape = 1, scale = 0.1), lower = 0.001)
zz=ks.test(xdata,"pweibull",shape=zweibull$estimate[1], scale=zweibull$estimate[2])
zz$p.value

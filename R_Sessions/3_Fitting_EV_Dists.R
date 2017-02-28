# Fit extreme value distribution to 
# sample maximum or minimum data
# Here we use the annual peak flow data
library(fExtremes)

rain_data = read.csv("http://www.isse.ucar.edu/extremevalues/docs/9619.csv")

#Extract the daily rain data and corresponding year only from months May-October (5th-10th month) for all years
winter_rain = rain_data$Rain[rain_data$M >= 5 & rain_data$M <= 10]

annual_winter_maxrain = blockMaxima(winter_rain,block=184)
maxrain=annual_winter_maxrain

#Install and load the library fExtremes
gevmodel = gevFit(maxrain)		#fit a Generalized Extreme Value Distribution
zgev=slot(gevmodel, "fit")

gumbelmodel = gumbelFit(maxrain)	#Fit a Gumbel distribution
zgumb = slot(gumbelmodel, "fit")


#### Plot them with histograms..
par(mfrow=c(2,1))

hist(maxrain, xlab="Max Rain", ylab="PDF", probability=T)
ydata=dgev(sort(maxrain), xi=zgev$par.ests[1], mu=zgev$par.ests[2], beta=zgev$par.ests[3])
lines(sort(maxrain), ydata, col="red")

lines(sort(maxrain), dgev(sort(maxrain), xi=0, mu=zgumb$par.ests[1],
beta=zgumb$par.ests[2]), col="blue")

#Lognormal
lines(sort(maxrain), dlnorm(sort(maxrain), meanlog=mean(log(maxrain)), sdlog=sd(log(maxrain))), col="green")

#Logpearson III = Gamma PDF on the Log of the data..

library(MASS)
zgamma = fitdistr(log(maxrain), "gamma")

hist(log(maxrain), probability=T, xlab="Log (Peak Flow)", ylab="PDF")

lines(log(sort(maxrain)), dgamma(log(sort(maxrain)), shape=zgamma$estimate[1], scale=1/zgamma$estimate[2]),col="purple")

################

#Find the Quantiles 50-year, 100-year and 500-year return period.
#GEV

gevquant = qgev(c((1-1/50), (1-1/100), (1-1/500)), xi=zgev$par.ests[1], mu=zgev$par.ests[2], beta=zgev$par.ests[3])


#gumbell
gumbquant = qgev(c((1-1/50), (1-1/100), (1-1/500)), xi=0, mu=zgumb$par.ests[1], beta=zgumb$par.ests[2])

#Lognormal
lnorquant = qlnorm(c((1-1/50), (1-1/100), (1-1/500)), meanlog=mean(log(maxrain)), sdlog=sd(log(maxrain)))

#Log Pearson III
gammaquant = qgamma(c((1-1/50), (1-1/100), (1-1/500)), shape=zgamma$estimate[1], scale=1/zgamma$estimate[2])
lp3quant = exp(gammaquant)
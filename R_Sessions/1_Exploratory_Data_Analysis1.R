###############################################################
#Read in our example data
###############################################################
rainfall = read.csv("./rainfall.csv")

###############################################################
# Understand the propertis of the dataset in R
###############################################################
#There are a lot of quick ways to check out what you just read in
class(rainfall) #you can see its a dataframe by typing class
summary(rainfall) #summarize it
head(rainfall)  #look at just the first few lines
head(rainfall)  #or the last few lines
names(rainfall) #see the column names
rainfall$precip #grab out variables by their column names ** this only works if you have your data as a dataframe
dim(rainfall) # Get the dimensions
length(rainfall) #Get the total length of the dataset (this will be the number of columns for a datafram)
length(rainfall$precip) #Get just the lenght of one variable (this is the number of rows)

#You can also use indices to pull out any part of the data you are interested in
rainfall[,1] #all rows first column
rainfall[1,] #all columns first row
rainfall[1:5,] #rows 1:5, all columns
rainfall[1:5,1:2] #rows 1:5, all columns 1:2


###############################################################
# Quick plotting 
###############################################################
#Make a histogram
hist(rainfall$precip) #histogram with default parmeters, look at ?hist to see what R did
hist(rainfall$precip, breaks=3) #we can control the number of breaks
hist(rainfall$precip, breaks=seq(0,8000,1000)) #or exactly where breaks happen
hist(rainfall$precip, breaks=seq(0,8000,1000), xlab="Annual Precip [mm]", ylab="Count", main="Annual Precipitation") #And we can make it look nicer
hist1=hist(rainfall$precip, breaks=seq(0,8000,1000),plot=T, xlab="Annual Precip [mm]", ylab="Count", main="Annual Precipitation")  # you can also save the infomration from your histogram
hist1 # and then look at it like this
hist1$counts	#and grab out relevant pieces like this
sum(hist1$counts) #see that the counts add up to your total number of data points
plot(hist1)     # and then replot it again later like this


#Make a boxplot
boxplot(rainfall$precip) #boxplot with default parmeters, look at ?boxplot to see what R did
boxplot(rainfall$precip, range=0) #making the whiskers extend to the max and min
bp1=boxplot(rainfall$precip) #same as with histograms you can save the information from the plot


#Make an emperical cdf using the ecdf function
precip_ecdf=ecdf(rainfall$precip)
plot.ecdf(precip_ecdf)

# Or calculcualt it yourself
precip_sort=sort(rainfall$precip) #sort your values ascending
#creat another vector of cumulative probabilities
np=length(rainfall$precip)     
precip_prob=seq(from=1/np,by=1/np, length.out=np)
#add a line to your last plot and see that they are the same
lines(precip_sort, precip_prob, col='red', lty=3, lwd=2)

###############################################################
#Central Tendancy
###############################################################
#mean
mean=mean(rainfall$precip) 
mean1=sum(rainfall$precip)/length(rainfall$precip)
mean
mean1

#trimmed mean
meanT10=mean(rainfall$precip, 0.1)
meanT20=mean(rainfall$precip, 0.2)

#median
median=median(rainfall$precip)
median1=quantile(rainfall$precip, 0.5)
if(np%%2==0){
	print("length is even, taking the average of the center")
	median2=(precip_sort[np/2]+precip_sort[(np/2+1)])/2
}else{
	print("length is odd, grabbing the central point")
	precip_sort[(np+1)/2]
}
median
median1
median2

# look at the central tendancy metrics on top of our histogram
hist(rainfall$precip, xlab="Annual Precip [mm]", ylab="Count")
box()
abline(v=mean, col='#045a8d', lty=3, lwd=3) #add a vertical line for the mean
abline(v=meanT10, col='#2b8cbe', lty=3, lwd=3) #add a vertical line for the mean
abline(v=meanT20, col='#74a9cf', lty=3, lwd=3) #add a vertical line for the mean
abline(v=median, col='#2ca25f', lty=3, lwd=3) #add a vertical line for the mean
legend('topright', legend=c("mean", "mean trim 10%", "mean trim 20%", "median"), lty=rep(3,4), lwd=rep(3,4), col=c("#045a8d","#2b8cbe", "#74a9cf", "#2ca25f" ))

#FYI here are some links for colors in R
# http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
# http://colorbrewer2.org/#type=sequential&scheme=BuGn&n=3

###############################################################
#Spread
###############################################################
#variance
var=var(rainfall$precip) 
var1=sum((rainfall$precip-mean(rainfall$precip))^2)/(np-1)
var
var1

#Standard deviation
sd= sd(rainfall$precip) 
sd1=sqrt(var)
sd2=var^0.5
sd
sd1
sd2

#Interquartile range
iqrP=IQR(rainfall$precip)
iqrP1=quantile(rainfall$precip, 0.75)-quantile(rainfall$precip, 0.25)
iqrP2=bp1$stats[4]-bp1$stats[2] 
iqrP
iqrP1
iqrP2

#why is iqrP2 different?
# compare just 25% value to see
bp1$stats[2] 
quantile(rainfall$precip, 0.25)
#the first quartile should fall here
(np+1)/4
#between these two values
precip_sort[430]
precip_sort[431]
#if you average the two you get
mean(c(precip_sort[430], precip_sort[431])) # this is the boxplot value
# if you linearly interpolate 3/4 of the way between the two you get:
(precip_sort[431] - precip_sort[430])*0.75 +precip_sort[430] # this is the quartile value


#Mean absolute deviation
library('lsr') #to use this funtion we first need to load the library it comes from
madP=aad(rainfall$precip)
madP1=mean(abs(rainfall$precip-mean(rainfall$precip)))
madP
madP1

#Median absolute devaition
meadP=mad(rainfall$precip)
meadP1=median(abs(rainfall$precip-median(rainfall$precip)))
meadP
meadP1
#trouble not matching again. 
meadP2=mad(rainfall$precip, constant=1)
meadP2


#show spread on the histogram
hist(rainfall$precip, xlab="Annual Precip [mm]", ylab="Count")
box()
lines(x=quantile(rainfall$precip, c(0.25, 0.75)), y=c(30,30), col='red', lty=1, lwd=3) #Horizontal line for IQR
lines(x=c(mean+sd, mean-sd), y=c(50,50), col='blue', lty=1, lwd=3) #Horizontal line for mean +/- 1 SD
legend('topright', legend=c("mean +/- 1 stdev", "IQR"), lty=rep(1,2), lwd=rep(1,3), col=c("blue", "Red" ))

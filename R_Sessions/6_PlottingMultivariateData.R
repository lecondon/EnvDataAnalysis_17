###############################################################
#Read in our example data
correlation = read.csv("./correlation.csv")
attach(correlation) #recognize all the variables by column heading
print(correlation) #make sure the dataframe contians all the input variables
summary(correlation) #summarize the main statistics
#use the complete observation to calculate pearson/kendall/spearman correlation coeff
cor(precip_mm, flow_mm, use= "complete.obs", method= "pearson") 
# the scatter plot of two variables, their corr. coeff. already calculated
plot(precip_mm, flow_mm, xlim=c(0,1500), ylim=c(0,1500))
plot(year, flow_mm, xlim=c(1900,2020), ylim=c(0,1500))
# corr. coeff. eprformed with Kendal and spearman methods
cor(precip_mm, flow_mm, use= "complete.obs", method= "kendal")
cor(precip_mm, flow_mm, use= "complete.obs", method= "spearman")
#the Hmisc package contains the function to return significance level for correlation coeff
#install.packages("Hmisc")
#library(Hmisc)
#Only return sign. level for pearson and spearman 
#rcorr(precip_mm, flow_mm, type = "pearson")
#rcorr(precip_mm, flow_mm, type = "spearman")
#rcorr(precip_mm, amo, type = "pearson")
#rcorr(precip_mm, nao, type = "pearson")
#return the covariance of the variables
#cov(precip_mm, flow_mm, use="complete.obs")
# packages for adding trend line
#install.packages("mblm")
#library(mblm)
#install.packages("rkt")
#library(rkt)
#install.packages("zyp")
#library(zyp)
#lm: fit a line to precip-flow relationship
# used ordianry least square regression method and store informaiton in fit_OLS
#require(stats)
#fit_OLS <- lm(flow_mm ~ year)
# equation of the line read slope and intercept: 
#coef_OLT=coefficients(fit_OLS)
#eq = paste0("y = ", round(coef_OLT[2],1), "*x ", round(coef_OLT[1],1))
# plot all the residual_related graphs
#plot(fit_OLS, main=eq)
#abline(fit_OLS, col="blue")
# Kendall Theil Line perforemd with zyp.sen package
# Question: I am not able to draw the line with abline when I am using zyp.sen function
#fit_KT <- zyp.sen(flow_mm ~ year)
#coef_KT=coefficients(fit_KT)
#abline(fit_KT, col="red")
# Kendall Theil Line perforemd with rkt package
# Question: I am not able to draw the line with abline when I am using rkt funciton
#fit_RKT <- rkt(year, flow_mm)
#coef_RKT=coefficients(fit_RKT)
#abline(fit_KT, col="yellow")
# the second section: Analysis with more variables
library(data.table)
dt = data.table(ID=c(1), temp_min_c, temp_max_c, precip_mm, flow_mm, amo, nao)
#.SD means a subset of D calculate the mean and variance
# QUESTION: I do not know how to add mean and variance to the column name
dt[, c(mean = lapply(.SD, mean), sd = lapply(.SD, sd)), by = ID]
# correlation matrix
corr.matrix <- data.frame(temp_min_c, temp_max_c, precip_mm, flow_mm, amo, nao)
head(corr.matrix)
corr_coeff <- round(cor(corr.matrix), 2)
# correlation matrix scatter plot
install.packages("corrplot")
library(corrplot)
corrplot(corr_coeff, method="circle")
corrplot(corr_coeff, method="square")
corrplot(corr_coeff, method="ellipse")
corrplot(corr_coeff, method="number")
corrplot.mixed(corr_coeff, lower="number", upper="circle")
# scatter plot matrix
pairs(~temp_min_c+temp_max_c+precip_mm+flow_mm+amo+nao, data=correlation, 
      main="Simple Scatterplot Matrix")
# 3D scatter plot matrix
install.packages("scatterplot3d")
library(scatterplot3d) 
scatterplot3d(precip_mm,flow_mm,temp_max_c, main="3D Scatterplot")
scatterplot3d(precip_mm,flow_mm,temp_max_c, pch=16, highlight.3d=TRUE,
              type="h", main="3D Scatterplot")

-------------------------------------------------------------------
#3D plotting
install.packages("plotly")
library(plotly)
plot_ly(x=flow_mm,y=precip_mm,z=temp_max_c, type="scatter3d")
plot_ly(x=flow_mm,y=precip_mm,z=temp_max_c, type="surface")
#3D plotting
install.packages("rgl")
library(rgl)
# this function is expecting increasing values for 'x' and 'y' values 
persp3d(temp_min_c, temp_max_c, flow_mm, col="skyblue")
# lattice graphic package
install.packages("lattice")
library(lattice)
wireframe(temp_max_c ~ precip_mm*flow_mm, data = correlation,
          xlab = "precip mm", ylab = "flow mm", main = "temp",
          drape = TRUE, colorkey = TRUE, screen = list(z = -60, x = -60))


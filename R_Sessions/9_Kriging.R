#R session 5-2: Kriging and spatial analysis
library(sp)
library(gstat)
library(dplyr)
library(ggplot2)
library(fields)

data(meuse)
summary(meuse)
glimpse(meuse)

#plotting zinc with standard approach
pal=colorRampPalette(c('red', 'blue'))
colors=pal(10)
collist=colors[as.numeric(cut(meuse$zinc, breaks=10))]
plot(meuse$x, meuse$y, pch=16, col=collist)

#plotting with ggplot
meuse %>% as.data.frame %>%
ggplot(aes(x,y))+ geom_point(aes(size=zinc), color='blue', alpha=1/4) + ggtitle("Zinc Concentration (ppm)") +coord_equal() +theme_bw()

#convert to a spatial dataframe
class(meuse)
coordinates(meuse)=~x+y
str(meuse)
coordinates(meuse)
proj4string(meuse)

#Fitting a variogram
lzn.vgm=variogram(log(zinc)~1, meuse)
lzn.fit=fit.variogram(lzn.vgm, model=vgm(1, "Sph", 900,1))
plot(lzn.vgm, lzn.fit)

##Perform Kriging
#get a grid to predict values for
data("meuse.grid")

#plot the grid and the original data points
plot1 <- meuse %>% as.data.frame %>%
  ggplot(aes(x, y)) + geom_point(size=1) + coord_equal() + 
  ggtitle("Points with measurements")

plot2 <- meuse.grid %>% as.data.frame %>%
  ggplot(aes(x, y)) + geom_point(size=1) + coord_equal() + 
  ggtitle("Points at which to estimate")

library(gridExtra)
grid.arrange(plot1, plot2, ncol = 2)

#Now perform the kriging
coordinates(meuse.grid) <- ~ x + y # first turn it into a spatial dataframe
lzn.kriged <- krige(log(zinc) ~ 1, meuse, meuse.grid, model=lzn.fit)

#look at the results
lzn.kriged %>% as.data.frame %>%
  ggplot(aes(x=x, y=y)) + geom_tile(aes(fill=var1.pred)) + coord_equal() +
  scale_fill_gradient(low = "yellow", high="red") + theme_bw()













#PCA R session

# Two main functions for PCA in R princomp() and prcomp()
# They use different methods to derive the PCs
# princomp is more stable this is what we will use

#sepal length and width and petal length and width for 50 iris samples of 3 different species
data(iris) 
head(iris,3)
dim(iris)

#Look at the data!
collist=as.numeric(iris[,5])
plot(iris[,1:4], pch=16, col=collist)

#Log transform to adjust for skew
log.ir=(log(iris[,1:4]))
ir.species=iris[,5]

#calculate principal components
ir.pca=prcomp(log.ir, center=T, scale=T)  #centering data and adjusting var to 1
print(ir.pca)
ir.pca$center #the mean for each variable
ir.pca$scale  #the variance for each variable
ir.pca$x  #the pc scores for each observation
ir.pca$rotation #the loading you see when you print

ir.pca=prcomp(log.ir, center=T, scale=T)  #centering data and 

#plot
plot(ir.pca, type='l')
barplot(ir.pca$sdev/ir.pca$sdev[1])
abline(h=0.1, col=2)

#what fraction of variance is explained by the first PC?
ir.pca$sdev[1]^2/sum(ir.pca$sdev^2)
#how about the first 2?
sum(ir.pca$sdev[1:2]^2)/sum(ir.pca$sdev^2)

summary(ir.pca)

#plot the result
biplot(ir.pca)
plot(ir.pca$x[,1], ir.pca$x[,2], col=collist, pch=16)

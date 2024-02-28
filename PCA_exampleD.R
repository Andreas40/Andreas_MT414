#Principal components 

#install.packages("corpcor")
#install.packages("GPArotation"); 
#install.packages("psych")
#install.packages("DSUR")
#install.packages("corrplot")
#install.packages("jpeg")
#install.packages("imager")
library(corpcor); library(GPArotation); library(psych);  library(corrplot)

faData<-read.csv("faexample.csv", header = TRUE)
#summaries and plot of data
summary(faData)
str(faData)
plot(faData)
#find correlation
cor_faData<-cor(faData)


corrplot(cor_faData, method="circle")
corrplot(cor_faData, method="pie")
corrplot(cor_faData, method="color")
corrplot(cor_faData, method="number")

cor_faData


#determinant and KMO test of data quality
det(cor_faData)
KMO(cor_faData)
#find eigenvalues and eigenvectors
eigen(cor_faData)

#parallel analysis scree (eigenvalues) plot
fa.parallel(cor_faData, fm = 'minres', fa = 'pc')

#PCA 
principal_faData<- principal(faData,nfactors=2,rotate = "varimax", residuals = TRUE)
principal_faData
#residuals histogram
hist(residuals(principal_faData))
fa.diagram(principal_faData)

biplot(principal_faData)

whichcomp<-cbind(principal_faData$scores, faData)
whichcomp<-cor(whichcomp)
whichcomp


#Principal components example from imaging-Start

library(jpeg)
library(imager)
plot(load.image("cat.jpg"))
plot(load.image("rgbc.jpg"))
cat <- readJPEG('cat.jpg')
r <- cat[,,1]
g <- cat[,,2]
b <- cat[,,3]
cat.r.pca <- prcomp(r, center = FALSE)
cat.g.pca <- prcomp(g, center = FALSE)
cat.b.pca <- prcomp(b, center = FALSE)
rgb.pca <- list(cat.r.pca, cat.g.pca, cat.b.pca)

for (i in seq.int(from=3, to=ncol(cat)-10, length.out = 20)) {
  pca.img <- sapply(rgb.pca, function(j) {
    compressed.img <- j$x[,1:i] %*% t(j$rotation[,1:i])
  }, simplify = 'array')
  writeJPEG(pca.img, paste('cat_compressed_', round(i,0), '_components.jpg', sep = ''))
}
#Principal components example from imaging-End

#

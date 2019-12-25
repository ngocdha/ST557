# ST 557: Applied Multivariate Analysis
# Clustering Example Script File

###########
# Step 2  #
###########

# Set working directory 

setwd('/Users/scemerson/Documents/Old Computer Documents/ST 557 2019/Datasets')

# Install and load the 'mclust' library

install.packages('mclust')
library(mclust)

# Read in 'MammalMilkData.csv' dataset 

milk <- read.csv('MammalMilkData.csv')
head(milk)
dim(milk)

###########
# Step 3  #
###########

# Perform Hierarchical Clustering: 'hclust()'

help(dist)
milk.distEuc <- dist(milk[,-1])

milk.hcEuc <- hclust(milk.distEuc, method="complete")
milk.haEuc <- hclust(milk.distEuc, method="average")
milk.hsEuc <- hclust(milk.distEuc, method="single")

names(milk.hcEuc)

par(mfrow=c(1,3))
plot(milk.hcEuc, labels=milk[,1], hang=-1, sub="", main="Complete Linkage")
plot(milk.haEuc, labels=milk[,1], hang=-1, sub="", main="Average Linkage")
plot(milk.hsEuc, labels=milk[,1], hang=-1, sub="", main="Single Linkage")

# Distances on scaled data

milk.sc <- scale(milk[,-1])
milk.distEucSc <- dist(milk.sc)

milk.hcEucSc <- hclust(milk.distEucSc, method="complete")
milk.haEucSc <- hclust(milk.distEucSc, method="average")
milk.hsEucSc <- hclust(milk.distEucSc, method="single")

par(mfrow=c(1,3))
plot(milk.hcEucSc, labels=milk[,1], hang=-1)
plot(milk.haEucSc, labels=milk[,1], hang=-1)
plot(milk.hsEucSc, labels=milk[,1], hang=-1)

cutree(milk.hcEucSc, k=3)

pairs(milk[,-1], col=cutree(milk.hcEucSc,k=3)+1)

# Compute Mahalanobis distance matrix

milk.cov <- cov(milk[,-1])
milk.sph <- as.matrix(milk[,-1]) %*% t(chol(solve(milk.cov)))
milk.distMah <- dist(milk.sph)

# Confirm that the above computations are doing what we expect

milk.mat <- as.matrix(milk[,-1])
sqrt((milk.mat[1,] - milk.mat[2,])%*% solve(milk.cov)%*%(milk.mat[1,]-milk.mat[2,]))
as.matrix(milk.distMah)[1,2]

# Cluster using Mahalanobis Distance

milk.hcMah <- hclust(milk.distMah, method="complete")
milk.haMah <- hclust(milk.distMah, method="average")
milk.hsMah <- hclust(milk.distMah, method="single")

par(mfrow=c(1,3))
plot(milk.hcMah, labels=milk[,1], hang=-1)
plot(milk.haMah, labels=milk[,1], hang=-1)
plot(milk.hsMah, labels=milk[,1], hang=-1)

# Correlation Distance

milk.distCor <- 1-cor(t(milk[,-1]))^2

milk.hcCor <- hclust(as.dist(milk.distCor), method="complete")
milk.haCor <- hclust(as.dist(milk.distCor), method="average")
milk.hsCor <- hclust(as.dist(milk.distCor), method="single")

par(mfrow=c(1,3))
plot(milk.hcCor, labels=milk[,1], hang=0.5)
plot(milk.haCor, labels=milk[,1], hang=0.5)
plot(milk.hsCor, labels=milk[,1], hang=0.5)


###########
# Step 4  #
###########

# Perform k-means Clustering: 'kmeans()'

help(kmeans)

milk.km3 <- kmeans(milk[,-1], centers=3, nstart=10)

names(milk.km3)

# Print the membership for each cluster:

milk[milk.km3$clus==1,1]
milk[milk.km3$clus==2,1]
milk[milk.km3$clus==3,1]


# Produce pairs plot with points colored by k-means cluster assignment

pairs(milk[,-1], col=milk.km3$clus+1)

# Compare the k-means clustering to the complete-linkage hierarchical 
# clustering with Euclidean distance cut to produce 3 clusters:

table(milk.km3$clus, cutree(milk.hcEuc, k=3))

# Demonstrate that if we run k-means multiple times we can get different
# results (not just different cluster lables, but different cluster sizes)

milk.km5.a <- kmeans(milk[,-1], centers=5, nstart=1)
milk.km5.b <- kmeans(milk[,-1], centers=5, nstart=1)

table(milk.km5.a$clus, milk.km5.b$clus)

###########
# Step 5  #
###########

# Perform Model-based Clustering: 'Mclust()'

help(Mclust)

# Fit a model-based clustering allowing the function to select
# the best number of clusters according to the BIC.

milk.mc <- Mclust(milk[,-1])
milk.mc
names(milk.mc)
pairs(milk[,-1], col=milk.mc$clas+1)

# Fit a model-based clustering with a specified number of clusters
# (in this case, 3 clusters)

milk.mc3 <- Mclust(milk[,-1], G=3)

pairs(milk[,-1], col=milk.mc3$clas+1)

# Compare the results from k-means to the results from model-based 
# clustering, both with 3 clusters:

table(milk.mc3$clas, milk.km3$clus)
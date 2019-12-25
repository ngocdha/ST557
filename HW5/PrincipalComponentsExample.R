# ST 557: Applied Multivariate Analysis
# Principal Components Example Script File

###########
# Step 2  #
###########

# Set working directory 

setwd('/Users/scemerson/Documents/Old Computer Documents/ST 557 2019/Datasets')

# Read in 'TestScoreData.csv' dataset 

exam <- read.csv('TestScoreData.csv')


###########
# Step 3  #
###########

# PCA

# Use the exam data for PCA exercises

exam.cov <- cov(exam)
exam.cor <- cor(exam)

# Example linear combination:
aVec1 <- c(2, 2, 1, 1, 1, 1, 1, 1)

exam.linComb1 <- as.matrix(exam) %*% aVec1
exam.linComb1[1:10,]
var(exam.linComb1)

t(aVec1) %*% exam.cov %*% aVec1


aVec2 <- 10*aVec1

exam.linComb2 <- as.matrix(exam) %*% aVec2
exam.linComb2[1:10,]
var(exam.linComb2)

aVec3 <- 1:8
exam.linComb3 <- as.matrix(exam) %*% aVec3

cov(exam.linComb1, exam.linComb3)
t(aVec1) %*% exam.cov %*% aVec3
t(aVec3) %*% exam.cov %*% aVec1

# Eigendecompositions of Cov and Cor matrices

exam.cov.eig <- eigen(exam.cov)
exam.cor.eig <- eigen(exam.cor)

exam.cov.eig

exam.cor.eig

# Loadings plots with unstandardized data

par(mfrow=c(3,3), oma=c(0,0,2,0))
for(i in 1:8){
	plot(1:8, exam.cov.eig$vec[,i], xlab="Variable", ylab="Loading", main=paste("Principal Component ", i, sep=""), ylim=c(-1, 1))
	abline(h=0)
}
mtext("Raw Principal Components", outer=T)

# Loadings plots with standardized data

par(mfrow=c(3,3), oma=c(0,0,2,0))
for(i in 1:8){
	plot(1:8, exam.cor.eig$vec[,i], xlab="Variable", ylab="Loading", main=paste("Principal Component ", i, sep=""), ylim=c(-1, 1))
	abline(h=0)
}
mtext("Standardized Principal Components", outer=T)

# Scores plots for unstandardized data

par(mfrow=c(1,1), oma=c(0,0,0,0))

exam.pcscores.raw12 <- as.matrix(exam) %*% exam.cov.eig$vec[,1:2]
plot(exam.pcscores.raw12, xlab="First Principal Component Scores", ylab="Second Principal Component Scores", main="(Raw) Score Plot")

exam.pcscores.cent12 <- scale(exam, center=T, scale=F) %*% exam.cov.eig$vec[,1:2]
plot(exam.pcscores.cent12, xlab="First Principal Component Scores", ylab="Second Principal Component Scores", main="(Centered) Score Plot")

# Scree plot for unstandardized data

par(mfrow=c(1,1), oma=c(0,0,0,0))
plot(1:8, exam.cov.eig$val, type="b", xlab="Component", ylab="Variance", main="Scree Plot for NYSE Data")

# Cumulative variance explained plot for unstandardized data

exam.cov.eig$val
cumsum(exam.cov.eig$val)
cumsum(exam.cov.eig$val)/sum(exam.cov.eig$val)

plot(1:8, cumsum(exam.cov.eig$val)/sum(exam.cov.eig$val), type="b", xlab="# Components", ylab="Cumulative Variance Explained")

###########
# Step 4  #
###########

# prcomp function

help(prcomp)

exam.pc1 <- prcomp(exam)

names(exam.pc1)

# The 'sdev' component is the square roots of the variances of
# the principal components.

exam.pc1$sdev
sqrt(exam.cov.eig$val)

exam.pc1$rotation
exam.cov.eig$vec

# Note that some vectors may have signs flipped

exam.pc1$x[1:8,1:2]
exam.pcscores.cent12[1:8,]

# Scores will also be flipped if principal components are flipped

exam.pc1.sc <- prcomp(exam, scale=T)

exam.pc1.sc$sdev
sqrt(exam.cor.eig$val)

exam.pc1.sc$rotation
exam.cor.eig$vec

# Note that some vectors may have signs flipped


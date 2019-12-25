# ST 557: Applied Multivariate Analysis
# Canonical Correlation Example Script File

###########
# Step 2  #
###########

# Set working directory 

setwd('/Users/scemerson/Documents/Old Computer Documents/ST 557 2019/Datasets')

# Install and load the 'CCA' R library

install.packages('CCA')
library(CCA)

# Read in 'NYSEData.csv' dataset 

nyse <- read.csv('NYSEData.csv')

###########
# Step 3  #
###########

x1 <- as.matrix(nyse[,1:3])
x2 <- as.matrix(nyse[,4:5])

sig11 <- cov(x1)
sig22 <- cov(x2)
sig12 <- cov(x1, x2)

# Compute canonical variates:

# Square root matrices:

sig11.eig <- eigen(sig11)
sig11.5 <- sig11.eig$vec %*% diag(sqrt(sig11.eig$val)) %*% 
	t(sig11.eig$vec)
	
sig11
sig11.5 %*% sig11.5
	
sig22.eig <- eigen(sig22)
sig22.5 <- sig22.eig$vec %*% diag(sqrt(sig22.eig$val)) %*% 
	t(sig22.eig$vec)
	
sig22
sig22.5 %*% sig22.5


# Calculate matrices A1 and A2

A1 <- solve(sig11.5) %*% sig12 %*% solve(sig22) %*% t(sig12) %*% solve(sig11.5)
A2 <- solve(sig22.5) %*% t(sig12) %*% solve(sig11) %*% sig12 %*% solve(sig22.5)

A1.eig <- eigen(A1)
A2.eig <- eigen(A2)

# First canonical variates loadings:

e1 <- A1.eig$vec[,1]
f1 <- A2.eig$vec[,1]

a1 <- e1 %*% solve(sig11.5)
b1 <- f1 %*% solve(sig22.5)

a1
b1

# First canonical variables/scores

u1 <- x1 %*% t(a1)
v1 <- x2 %*% t(b1)

# First canonical correlation

sqrt(A1.eig$val[1])
sqrt(A2.eig$val[1])
cor(u1, v1)

# Second canonical variates: loadings

e2 <- A1.eig$vec[,2]
f2 <- A2.eig$vec[,2]

a2 <- e2 %*% solve(sig11.5)
b2 <- f2 %*% solve(sig22.5)

a2
b2

# Second canonical variables/scores

u2 <- x1 %*% t(a2)
v2 <- x2 %*% t(b2)

# Second canonical correlation

sqrt(A1.eig$val[2])
sqrt(A2.eig$val[2])
cor(u2, v2)

a2 <- -a2
a2
u2 <- x1 %*% t(a2)
cor(u2, v2)

###########
# Step 4  #
###########

# R function to perform CCA

nyse.cc <- cc(nyse[,1:3], nyse[,4:5])

names(nyse.cc)

# Canonical variate loadings

nyse.cc$xcoef

a1
a2

nyse.cc$ycoef

b1
b2

# Canonical variate scores 

plot(nyse.cc$scores$xscores[,1], nyse.cc$scores$yscores[,1], xlab="First Canonical Variate U1 Scores", ylab="First Canonical Variate V1 Scores")

plot(nyse.cc$scores$xscores[,2], nyse.cc$scores$yscores[,2], xlab="Second Canonical Variate U2 Scores", ylab="Second Canonical Variate V2 Scores")

# Note that the cc results center the scores!!!

u1[1:10]
nyse.cc$scores$xscores[1:10,1]

u1.sc <- u1 - mean(u1)
u1.sc[1:10]

nyse.cc$scores$yscores[1:10,1]
v1.sc <- v1 - mean(v1)
v1.sc[1:10]


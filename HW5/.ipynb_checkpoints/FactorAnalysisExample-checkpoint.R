# ST 557: Applied Multivariate Analysis
# Factor Analysis Example Script File

###########
# Step 2  #
###########

# Set working directory 

setwd('/Users/scemerson/Documents/Old Computer Documents/ST 557 2019/Datasets')

# Read in 'NYSEData.csv' dataset 

nyse <- read.csv('NYSEData.csv')
head(nyse)

# Shorten names so that they fit better in printouts:

names(nyse) <- c("Shell", "Exxon", "JPM", "Citi", "WF")

###########
# Step 3  #
###########

# Perform Factor Analysis: PCA

nyse.cor <- cor(nyse)

nyse.eig <- eigen(nyse.cor)

# Compute the loadings for all factors at once by multiplying
# each eigenvector by the square-root of the corresponding eigenvalue

load.pcfa <- nyse.eig$vec %*% diag(sqrt(nyse.eig$val))
load.pcfa

# Confirm:

nyse.eig$vec[,1] * sqrt(nyse.eig$val[1])
nyse.eig$vec[,2] * sqrt(nyse.eig$val[2])

# Compute the uniquenesses for the one-factor solution by subtracting
# LL^T from the sample correlation matrix, and taking the diagonal
# elements of this difference. Here, L is the p x m matrix of loadings
# (so the first m columns of the loadings matrix computed above).

m <- 1
uni.pcfa1 <- diag(nyse.cor - load.pcfa[,1:m] %*% t(load.pcfa[,1:m]))
uni.pcfa1

# Construct the fitted correlation matrix for the one-factor solution by 
# computing LL^T + Psi, where Psi is the diagonal matrix of uniquenesses.

fit.pcfa1 <- load.pcfa[,1:m] %*% t(load.pcfa[,1:m]) + diag(uni.pcfa1)
fit.pcfa1

# Compute the residual matrix by subtracting the fitted correlation matrix
# from the observed sample correlation matrix
 
res.pcfa1 <- nyse.cor - fit.pcfa1
res.pcfa1

# Do the same as above, but now for the two-factor solution:

m <- 2

uni.pcfa2 <- diag(nyse.cor - load.pcfa[,1:m] %*% t(load.pcfa[,1:m]))
uni.pcfa2

fit.pcfa2 <- load.pcfa[,1:m] %*% t(load.pcfa[,1:m]) + diag(uni.pcfa2)
fit.pcfa2

res.pcfa2 <- nyse.cor - fit.pcfa2
res.pcfa2

###########
# Step 4  #
###########

# Perform Factor Analysis: MLE
# Note: this function always performs factor analysis using the standardized
# data (i.e. the correlation matrix)!

help(factanal)

# Fit a one-factor MLE factor analysis model, with no rotation applied
# to the computed loadings:

nyse.mlfa1 <- factanal(x=nyse, factors=1, rotation="none")
nyse.mlfa1
names(nyse.mlfa1)

nyse.mlfa1$load

nyse.mlfa1$load[1:5,1]

nyse.mlfa1$uni

fit.mle1 <- nyse.mlfa1$load %*% t(nyse.mlfa1$load) + diag(nyse.mlfa1$uni)
fit.mle1

res.mle1 <- nyse.cor - fit.mle1
res.mle1

nyse.mlfa2 <- factanal(x=nyse, factors=2, rotation="none")
nyse.mlfa2

fit.mle2 <- nyse.mlfa2$load %*% t(nyse.mlfa2$load) + diag(nyse.mlfa2$uni)
fit.mle2

res.mle2 <- nyse.cor - fit.mle2
res.mle2

# Test the null hypothesis that two factors is sufficient to explain
# this correlation matrix.

n <- nrow(nyse)
p <- ncol(nyse)
m <- 2

stat <- (n - 1 - (2*p + 4*m + 5)/6)*log(det(fit.mle2)/det(nyse.cor))
pval <- 1- pchisq(stat, ((p - m)^2 - p - m)/2)

stat
pval

nyse.mlfa2$STAT
nyse.mlfa2$PVAL

###########
# Step 5  #
###########

# Perform MLE Factor Analysis with varimax rotation

nyse.mlfa2.rot <- factanal(x=nyse, factors=2, rotation="varimax", scores="regression")

nyse.mlfa2.rot

nyse.mlfa2.rot$load[,1:2]
nyse.mlfa2$load[,1:2]

apply(nyse.mlfa2.rot$load[,1:2]^2, 2, var)
apply(nyse.mlfa2$load[,1:2]^2, 2, var)

nyse.mlfa2.rot$scores[1:10,]
scale(nyse, center=T, scale=F)[1:10,]






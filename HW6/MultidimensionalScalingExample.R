# ST 557: Applied Multivariate Analysis
# Multidimensional Scaling Example Script File

###########
# Step 2  #
###########

# Set working directory 

setwd('/Users/scemerson/Documents/Old Computer Documents/ST 557 2019/Datasets')

# Install and load the 'psych' library

install.packages('psych')

library(psych)

###########
# Step 3  #
###########

# Multidimensional Scaling

# isoMDS
# cmdscale

help(cmdscale)

# The 'cities' data is in the 'psych' package, and contains the 
# airline flight distances for 11 US cities.

data(cities)
cities

# Fit an MDS on the cities data
city.loc.mds0 <- cmdscale(cities, k=2) #ask for a 2 dimensional solution 
city.loc.mds0

# Note that when we plot these points, the map is upside-down and flipped
# east to west. 

plot(city.loc.mds0, pch=16)
text(city.loc.mds0, labels=row.names(cities), pos=1, cex=0.5)

# We can fix this by flipping each axis by multiplying by -1.

city.loc.mds <- -city.loc.mds0

plot(city.loc.mds, pch=16)
text(city.loc.mds, row.names(cities), pos=1, cex=0.5)

# # # # # 

city.loc.mds1 <- isoMDS(cities, k=2) #ask for a 2 dimensional solution 


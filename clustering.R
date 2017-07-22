# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

 install.packages(c("cluster", "rattle","NbClust"))
 
# Now load the data and look at the first few rows
data(wine, package="rattle.data")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function
wine.data <- subset(wine,select = -c(1))
wine.data <- scale(wine.data)
# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc=15, seed=1234){
	              wss <- (nrow(data)-1)*sum(apply(data,2,var))
               	      for (i in 2:nc){
		        set.seed(seed)
	                wss[i] <- sum(kmeans(data, centers=i)$withinss)}
	                
		      plot(1:nc, wss, type="b", xlab="Number of Clusters",
	                        ylab="Within groups sum of squares")
	   }

wssplot(wine.data)

# Exercise 2:
#   * How many clusters does this method suggest?
#    This method suggests 3 clusters beacuse there is a distinct drop within-groups the sum of squares when moving from 1 to 3.
#   * Why does this method work? What's the intuition behind it?
#     This method works because it specifies the number of clusters to etract in advance. The bend in the graph helps in identifying the number of clusters.
#   * Look at the code for wssplot() and figure out how it works

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(wine.data, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Numer of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?
#This method suggests a 3-cluster solution.

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

 fit.km <- kmeans( wine.data,3,nstart = 25 )

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?
 tb <- table(wine$Type,fit.km$cluster)
 tb
 library(flexclust)
 randIndex(tb)
 # randIndex of 0.89 suggests a good agreement between wine$type and kmeans algorithm, thus this could be considered a good clustering!!

 # Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?
library(cluster)
 clusplot( wine.data,fit.km$cluster,main = "Cusplot" )
# The clusplot suggests that the distribution od data points within the three clusters is compact thus suggesting good clustering.
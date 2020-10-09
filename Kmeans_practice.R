###################################################
#
#	AN INTRODUCTION TO DATA ANALYSIS
#	AND CLUSTERING USING R
#
#	THE K-MEANS MODEL
#
#	This script performs the K-means model: the
#	partition of n observations into K clusters
#	by minimizing the sum of squared distances
#	between every point and its nearest cluster
#	mean (centroid)
#
#	The K-means model is popular for two reasons:
#	It is simple to implement and its limitations
#	are known 
#
#	(for more information visit 
#	 Fränti & Sieranoja 2019, 
#	 Pattern Recognition 93, or ask google ;)
#
#	We implement the model following the steps
#	as it is typically done for any machine
#	learning model:
#	
#	step 1: Data preparation
#	step 2: Train test data split
#	step 3: Build the model
#	step 4: Train and test the model
#	step *: Cross-validation
#
###################################################

#### 		SET WORKING DIRECTORY		     ####

getwd() # current working directory (wd)

setwd("new_address") # set new working directory

#-------------------------------------------------#

####	     STEP 1: DATA PREPARATION		     ####

# Import data



### FEATURE SCALING



#-------------------------------------------------#


####	     STEP 2: TRAIN TEST SPLIT		     ####



#-------------------------------------------------#

####	     STEP 3: BUILD THE MODEL		     ####

# import the "stats" library, which contains the
# kmeans method

library(stats)

?kmeans() # kmeans documentation

km <- kmeans(x =		# a data frame
		,centers =  # number of clusters k
		,iter.max = 
		,nstart =  # nr of random starts
		)

#-------------------------------------------------#

####	     STEP 4: TRAIN AND TEST		     ####


# plot first two principal components
library(cluster) # for the clusplot method

clusplot(x = 		# a data frame 
	   ,km$cluster	# column with cluster values
	   ,color = TRUE
	   ,lines = 0	
	   ,main = "The clusters in my data" # plot title 
	   )

# update df with cluster values


# export data as csv file
write.csv(data = 		# data frame
	   ,file = 		# "filename" with extension
	   ,quote = FALSE
	   ,row.names = FALSE
	   )

#-------------------------------------------------#

### USING A CUSTOM FUNCTION
# Based on the Sillhoutte Score or the Knee method,
# this function automatically determines the optimal
# number of clusters

# The Sillhouette score is a measure of how well
# separated clusters are. It is a bit conservative

km = KmeansElbow(x = 		# a data frame 
		     ,rep = 
		     ,max.clusters = 
		     ,nstars = 
		     ,scaling.method = "na"
		     ,decision.method = "knee" # sillhouette
		     ,plot.pca = TRUE)

# save clustering plot
rgl.snapshot(filename = "my_clusters.png") # you can change the extension


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
list.files() # files in directory

file.edit("name") # open a script


# if recovering a working space
ls() # check the current objects

#-------------------------------------------------#

####	     STEP 1: DATA PREPARATION		     ####

# Import data
#df <- read.csv("data.csv"
#		  , sep = ";"
#		  , header = TRUE
#		  )

# let's look at the data
head(df) # overview of first rows

dim(df)  # dimension / shape of data frame
	 # How many features do we have?

str(df)  # column names and data types

# Question:
# are all columns meaninful for the analysis?

# select relevant columns
df_bkup <- df # a backup, just in case

#df <- df[,-c(1:6)]
# a more general way to select numerical columns
selection <- unlist(lapply(df, is.numeric))

df <- df[, selection]

# It is important to decide what to do with the missing
# values
# In this case, we omit them
# df <- na.omit(df)

# are there nas?
dim(df_bkup) == dim(df)

head(df) # new data frame

# Question: are we done? or is there left to do
#		with the preparation?

# let's take a look at the characteristics of the
# features
summary(df)

# from the summary, it is possible to see that the
# range (scale) differs between features. 
# This can affect the clustering, since some features 
# will weigth more than others when calculating the 
# distances

# Converting all variables or features to similar scale
# is often important for machine learning and
# statistical analyses!

### FEATURE SCALING
# Data standardization using a for-loop

# There are many possibilites. Here we standardize
# the data based on the z-score, which consists in
# substracting the mean and dividing by std

# perform the loop until the top value is reached

top <- length(df[1,]) # number of columns (i.e., features)
# top <- length(names(df)) # many possibilities

for (i in 1:top)
{
	df[,i] <- ( df[,i] - mean(df[,i]) ) / sd(df[,i])
}

head(df)
summary(df) # check the means

#-------------------------------------------------#


####	     STEP 2: TRAIN TEST SPLIT		     ####



#-------------------------------------------------#

####	     STEP 3: BUILD THE MODEL		     ####

# import the "stats" library, which contains the
# kmeans method

library(stats)

?kmeans() # kmeans documentation

km <- kmeans(df		# a data frame
		,centers = 3 # number of clusters k
		,iter.max = 100
		,nstart = 30 # nr of random starts
		)

# check the structure of the kmeans object
str(km)

# the clusters
km$cluster

#-------------------------------------------------#

####	     STEP 4: TRAIN AND TEST		     ####


# plot first two principal components
library(cluster) # for the clusplot method

clusplot(df 
	   ,km$cluster
	   ,color = TRUE
	   ,lines = 0
	   ,main = "The clusters in my data" 
	   )

# update df with cluster values
df_new <- cbind(km$cluster, df)

head(df_new)

# change name of first column
names(df_new)[1] <- "cluster"

head(df_new)

# export data as csv file
write.csv(df_new
	   ,file = "clustered_data.csv"
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

km = KmeansElbow(df 
		     ,rep = 500
		     ,max.clusters = 100
		     ,nstarts = 100
		     ,scaling.method = "z-score" #
		     ,decision.method = "knee"#"sillhouette"
		     ,plot.pca = TRUE)

# save clustering plot
rgl.snapshot(filename = "clusters_per_location.png") 
# you can change the extension


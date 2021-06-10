###################################################
#
#	AN INTRODUCTION TO DATA ANALYSIS
#	AND CLUSTERING USING R
#
#	THE DISCRIMINANT FUNCTION ANALYSIS
#
#	This script performs a discriminant analysis
#	(DFA) on the data
#
#	Linear discriminant analysis (LDA): Uses linear 
#	combinations of predictors to predict the class 
#	of a given observation. Assumes that the 
#	predictor variables (p) are normally distributed
#	and the classes have identical variances 
#	(for univariate analysis, p = 1) or identical 
#	covariance matrices (for multivariate analysis,
#	p > 1).
#
#	DFA generates a linear combination of variables
#	that maximizes the probability of correctly
#	assigning observations to their pre-determined
#	groups and can be used to classify new
#	observations into one of the groups
#
#	DFA can be used for dimensionality reduction
#	and test which variables discriminate the best
#	between groups
#
#	Question we address: Can we successfully
#	classify - new - observations into the
#	correct group or class?
#
#	(for more information check 
#	 Quinn & Keough 2002, 
#	 Experimental Design and Data Analysis for
#	 Biologists, or ask google ;)
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

#getwd() # current working directory (wd)

#setwd("new_address") # set new working directory

#-------------------------------------------------#

####	     STEP 1: DATA PREPARATION		     ####

# install packages
#install.packages("MASS") # lda function
#install.packages("caret")

# load packages
library(MASS) 
library(caret)

# import data_example.csv
df <- read.csv("data_example.csv", header = TRUE)

head(df) # it is already standardized

# Important! standardize your data / feature scaling
# before the analysis

#-------------------------------------------------#

####	     STEP 2: TRAIN TEST SPLIT		     ####

# linear discriminant analysis lda
# target variable (i.e., groups or classes)
# attach column with categories from the original 
# data frame (here called df_bup) to the standardized
# numerical df

# read data
df <- read.csv("alexandra.csv", sep = ";", header = TRUE)

# do a backup
bup <- df

# select only numerical columns
selection <- unlist(lapply(df, is.numeric))
df <- df[,selection] # only selected numerical columns

# remove call index and individual categories
df <- df[,-c(1,2)]

# standardize data
for (i in 1:dim(df)[2]) # number of features (many possibilities)
{
	df[,i] <- ( df[,i] - mean(df[,i]) ) / sd(df[,i])
}

label <- bup$location # in case, your predicted class are 
			 	# individuals (this is an example)

# attach categorical column to df
df <- cbind(label,df)

# train test split
training <- createDataPartition(label    # a vector to perform the split
					 ,times = 1 # how many splits?
					 ,list = FALSE# return results in matrix
					 , p = 0.8    # 80% of data goes to training set
					 )

df_train <- df[training, ]	# data frame for training (80% of original data)
df_test  <- df[-training, ]	# data frame for validation (20%)

print(dim(df))
print(dim(df_train))
print(dim(df_test))

# you can test by your self
print(dim(df)[1] * 0.8 ) # 80% of df size

#-------------------------------------------------#

####	     STEP 3: BUILD THE MODEL		     ####

# explanatory variables X1, ... , Xn
# are the features (columns) in df

#model <- lda(label ~ sinuosity + power, data = df_train # poor performance
#model <- lda(label ~ low_freq + high_freq, data = df_train # good performance
model <- lda(label ~ ., data = df_train # to use all features # all features
	    ,na.action = "na.omit"
	    )

print(model)

# the proportion of trace is the percentage separation 
# achieved by each discriminant function

# Variable selection:
# Note that, if the predictor variables are standardized 
# before computing LDA, the discriminator weights can be 
# used as measures of variable importance for feature selection.
#----------------

# let us check the ability of each discriminant 
# function separating / classifying the data

predictions = predict(model)

# The predict() function returns the following elements:
# class: predicted classes of observations.
# posterior: is a matrix whose columns are the groups, 
# rows are the individuals and values are the posterior 
# probability that the corresponding observation belongs 
# to the groups.
# x: contains the linear discriminants, described above


# plot only first discriminant function:
ldahist(predictions$x[,1], g = df_train$label)

# second discriminant
ldahist(predictions$x[,2], g = df_train$label)

# Hint: # If the histograms are not overlapping,
# means a good separation was achieved by that
# given discriminant function for those non-overlapping
# classes / labels / or categories

# scatter plot:
# Two possibilities: 
# 1) 2d plot using first two discriminant 
#    functions (LD1 and LD2)
#
# 2) 3d plot, using the first three 

# # get the scores of each function (see head(predictions$x) )
x = predictions$x[,1]	# LD1
y = predictions$x[,2]	# LD2
z = predictions$x[,3]	# LD3

# custom color palette: five classes, five colors 
# (you can change this!. Check google "R colors", under images tab)
mypal <- c("black", "magenta3", "blue", "red", "sienna")

# plotting first and second discriminant functions
plot(x, y      
    ,col = mypal[as.factor(df_train$label)]
    ,pch = 16 # filled circles. There are more shapes, ask google!
    ,main = "DFA, scatter plot"
    ,xlab = "Score, First Discriminant Function"
    ,ylab = "Score, Second Discrimiinant Function"
    )

# add legend:
legend(x = "bottomright"
	, legend = levels(as.factor(df_train$label)) 
	, col = mypal
	, pch = c(16)
 	#, bty = "n" # no box lines (only if you want)
	)
#-----------------
# 3d scatter plot:
# does not display for me in linux
library(rgl)# 3d plot

plot3d(x, y, z
	,type = "s"
	,size = 1 
	,lit = TRUE
	,box = FALSE	   # no box   
	#,col = df_test$label # color according to categories 
	)

# save clustering plot
rgl.snapshot(filename = "alexandra_scatter3d.png")
#-----------------

# plot the first three discriminant functions:

# scatterplot3d
library(scatterplot3d)
scatterplot3d(x,y,z
			 ,mypal[as.factor(df_train$label)]
			 )
#------------------
#-------------------------------------------------#

####	     STEP 4: TEST THE MODEL		     ####

# predict test data
predictions <- predict(model, df_test)

# Model accuracy 
mean(predictions$class==df_test$label)


# DFA can also help to understand what features or
# explanatory variables are important to separate
# classify the data

# We can use boxplots to have an idea of the
# variation of a given feature per each class

 boxplot(df$high_freq ~ df$label)

#-------------------------------------------------#


### SOME ADDITIONAL INFORMATION ABOUT DFAs


# On the Warning: Variables are collinear

# Answered by gui11aume 
#(https://stats.stackexchange.com/users/10849/gui11aume)

# Multicollinearity means that your predictors are correlated. 
# Why is this bad?
# Because LDA, like regression techniques involves computing a 
# matrix inversion, which is inaccurate if the determinant is 
# close to 0 (i.e. two or more variables are almost a linear 
# combination of each other).

# More importantly, it makes the estimated coefficients impossible 
# to interpret. If an increase in X1
# , say, is associated with an decrease in X2 and they both increase 
# variable Y, every change in X1 will be compensated by a change in 
# X2 and you will underestimate the effect of X1 on Y. In LDA, you 
# would underestimate the effect of X1

# on the classification.

# If all you care for is the classification per se, and that after 
# training your model on half of the data and testing it on the other 
# half you get 85-95% accuracy I'd say it is fine.

#--------------
# && # returns only one value, either true or false
# &  # a vector
# if(0){
#	dummy <- c(1, 2, 3)
#	dummy == 1
#	dummy == 1 &  dummy == 2
#	dummy == 1 && dummy == 2
#}

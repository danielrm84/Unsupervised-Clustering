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
label <- bup$Individual # in case, your predicted class are 
			 	# individuals (this is an example)

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

# let us check the ability of each discriminant 
# function separating / classifying the data

predictions = predict(model)

# first discriminant
ldahist(predictions$x[,1], g = df_train$label)

# second discriminant
ldahist(predictions$x[,2], g = df_train$label)

# Hint: # If the histograms are not overlapping,
# means a good separation was achieved by that
# given discriminant function for those non-overlapping
# classes / labels / or categories
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
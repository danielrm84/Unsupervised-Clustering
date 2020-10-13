###################################################
#
#	SCRIPT TO PLOT HISTOGRAM OF YOUR CLUSTER
#	RESULTS PER CATEGORY
#
#	Here a category or class refers to,
#	for example, 
#
#	your species (when your're insterested in
#	interspecific variation) 
#	
#	or 
#	
#	the ID of your individuals (when studying
#	intraspecific variation)
#
#	by Daniel Romero Mujalli
#	email: danielrm84@gmail.com
#	
###################################################


# These lines of code assume that you already worked
# your data (as shown by the "kmeans_solved" script)
# so that you have a km object (the results of the
# kmeans clustering analysis) and / or your data
# with the cluster columns attached

# Before using the histogram plot function, we need
# to store our clustering results (i.e., column with 
# the cluster values) and our categories into vectors:

# create vector to store our clusters
# pick one!
cluster <- km$cluster # if using the km object

cluster <- df$cluster # if using a your dataframe

# vector of categories or classes
# here you need to provide the name of the column
# containing your categories.

names(df) # run this to see the names in your df

# for example:
class <- df$species # if the column name is "species"

# we need to convert class to a factor, for better
# manipulation by the histogram function
class <- as.factor(class)

# We're almost there. We need to resize our plotting
# section according to our class factor
# For example, if we have three species, we expect to
# have three histograms (one per species)
# Thus,
# if I want to have the plots occurring in different
# columns I write:

# to check how many species occur in class factor:

levels(class)

# question: do the levels correspond to your expected
# categories? e.g., nr of species?

par(mfrow=c(1,3)) # three plots, column-wise

# alternatively, by row

par(mfrow=c(3,1)) # three plots, row-wise

# if you want to change the name of the title, please change
# the text argument given to the "main" argument inside the
# plot function (see below)

# Now we can plot the histogram:
for (i in c(levels(class)))
{
	x = cluster[class == i]
	h = hist(x, plot = FALSE
		   , breaks = seq(0,length(levels(as.factor(cluster))))) 
	h$density = h$counts/sum(h$counts)*100
	plot(h,freq=FALSE, col = "grey", las = 1
	, main = bquote("Category " ~ .(i)) # Main title
	, ylim = c(0, 100)
	, ylab = "percentage"
	, xlab = "cluster"
	)

}

### PLOT HISTOGRAMS TO COMPARE CLUSTERS WITH AGE CLASSES

########### FUNCTION PLOT_HIST 		#####################
#
# this function plot a histogram of cluster per class
# gruop
plot_hist <- function(cluster, class, max_cluster)
{
	par(mfrow=c(1, length(levels(class))))
	for (i in c(levels(class)))
	{
		x = cluster[class == i]
		h = hist(x, plot = FALSE
			   , breaks = seq(0, max_cluster)
			   ) 
		h$density = h$counts/sum(h$counts)*100
		plot(h,freq=FALSE, col = "grey", las = 1
		, main = bquote("sp = " ~ .(i))
		, ylab = "percentage"
		#, h$breaks = 3
		, ylim = c(0, 110)
		#, xlim = c(0,5)
		#,xaxt = "n"
		)

		# add text: nr of data points
		n <- tapply(class[class == i] 
		     ,cluster[class == i] 
		     ,length) # class i, cluster x
		text(x = as.numeric(names(n))- 0.5 #h$breaks   #seq(0:10)
		   , y = h$density[as.numeric(names(n))] + 5 #50
     		   , labels = n
		   , cex = 0.9
    		    )


		## add x axis
		#axis(1, at = c(1:max_cluster), 
		 #labels = c(1:max_cluster))
	}
}

########### END OF FUNCTION PLOT_HIST	#####################

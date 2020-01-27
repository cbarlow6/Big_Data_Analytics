rm(list = ls())
setwd("C:/Users/lxue5/Dropbox/2019Sp/CIS8695/5 Dimension Reduction")

utilities.df <- read.csv("CSV_Utilities.csv")

# set row names to the utilities column
row.names(utilities.df) <- utilities.df[,1]
# remove the utility column
utilities.df <- utilities.df[,-1]

# compute Euclidean distance
d <- dist(utilities.df, method = "euclidean")

# normalize input variables
utilities.df.norm <- sapply(utilities.df, scale)

# add row names: utilities
row.names(utilities.df.norm) <- row.names(utilities.df) 

# compute normalized distance based on Sales (column 6) and Fuel Cost (column 8)
d.norm <- dist(utilities.df.norm[,c(6,8)], method = "euclidean")


# in hclust() set argument method =  
# to "ward.D", "single", "complete", "average", "median", or "centroid"
hc1 <- hclust(d.norm, method = "single")
plot(hc1, hang = -1, ann = FALSE)
hc2 <- hclust(d.norm, method = "average")
plot(hc2, hang = -1, ann = FALSE)

Class_Single <- cutree(hc1, k = 6)
Class_Average <- cutree(hc2, k = 6)
utilities.df.norm<-data.frame(utilities.df.norm,Class_Single,Class_Average)


# set labels as cluster membership and utility name
row.names(utilities.df.norm) <- paste(memb, ": ", row.names(utilities.df), sep = "")


# plot heatmap 
utilities.df.norm$Class_Average<-NULL
utilities.df.norm$Class_Single<-NULL
# rev() reverses the color mapping to large = dark
heatmap(as.matrix(utilities.df.norm), Colv = NA, hclustfun = hclust, 
        col=rev(paste("gray",1:99,sep="")))



# run kmeans algorithm: 6 clusters 
km <- kmeans(utilities.df.norm, 6)

# show cluster membership
km$cluster

# centroids
km$centers


# plot an empty scatter plot
plot(c(0), xaxt = 'n', ylab = "", type = "l", 
     ylim = c(min(km$centers), max(km$centers)), xlim = c(0, 8))

# label x-axes
axis(1, at = c(1:8), labels = names(utilities.df))

# plot centroids
for (i in c(1:6))
  lines(km$centers[i,], lty = i, lwd = 2, col = ifelse(i %in% c(1, 3, 5),
                                                       "black", "dark grey"))

# name clusters
text(x = 0.5, y = km$centers[, 1], labels = paste("Cluster", c(1:6)))


dist(km$centers)

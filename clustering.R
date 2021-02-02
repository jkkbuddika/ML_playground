## Unsupervised Machine Learning Training
## This script contain practice exercises to learn clustering
## Learn Hierarchical and K-means clustering
## Require data from compGenomRData on GitHub 

# Download required data from compGenomRData on GitHub
# Use the downloader library for this task
library(downloader)

# Construct the url to download
dir <- "https://github.com/compgenomr/compGenomRData/raw/master/inst/extdata/"
file_name <- "leukemiaExpressionSubset.rds"
Url <- paste0(dir, file_name)

# Download data file
if (!file.exists(file_name))
  download(Url, destfile = file_name)

# Open the data file (an RDS/R data object in this example)
dat <- readRDS(file_name)

#############################
## Hierarchical Clustering ##
#############################

# Following generating the distance metrics, you can use the base hclust()
# function to perform Hierarchical Clustering
# The method function used in hclust() determines how the sub-dlusters are 
# merged
# Options: "complete", "single", "average", "ward.D2"/"ward.D"
# Can be visualized as a Dendrogram
dat %>%
  t() %>%
  dist(method = "euclidean") %>%
  hclust(method = "complete") %>%
  plot(labels = FALSE, hang = -1)

# Often visualized as a heat map. Favorite package: "pheatmap"
# Load library
library(pheatmap)
library(viridis)

# Set the leukemia type annotation for each sample
# Extract the leukemia type from column names: AML_GSM330532.CEL --> AML
annotation_col = data.frame(
  LeukemiaType =substr(colnames(dat), start = 1, stop = 3))

# Generate the data metrics by assigning column names to row names of
# annotation_col
rownames(annotation_col)=colnames(dat)
annotation_col

# Generate the heat map using pheatmap package
# This package internally generates distance matrices and can perform
# hierarchical clustering
pheatmap(dat, show_rownames = FALSE, show_colnames = FALSE,
         color = viridis(100),
         annotation_col = annotation_col,
         scale = "none", clustering_method = "ward.D2",
         clustering_distance_cols = "euclidean")

# The pheatmap can introduce breaks in the heat map 
# You can break up the heat map by specifying how many clusters you want from 
# the dendrograms (You can also manually define where you want breaks too)
pheatmap(dat, show_rownames = FALSE, show_colnames = FALSE,
         color = viridis(100),
         annotation_col = annotation_col,
         scale = "none", clustering_method = "ward.D2",
         clustering_distance_cols = "euclidean",
         # I chose 5 as I know there are 5 classes of leukemia in the dataset
         # Note that the clusters are different than what you expected
         cutree_cols = 5)

# You can manually cut the tree accordingly by specifying the gaps_col condition

# In addition you can manually cluster based on the height of dendrogram
# You can specify the number of clusters you want and the height at which the
# clustering has to happen
hcl <- dat %>%
  t() %>%
  dist(method = "euclidean") %>%
  hclust(method = "complete")

# Generate a Dendrogram
# Change 80 to change the height
# Note that only 4 clusters have been identified
plot(hcl,labels = FALSE, hang= -1)
rect.hclust(hcl, h = 80, border = "blue")

# You can define the number of clusters as follows:
clu_5 <- cutree(hcl, k=5)
table(clu_5)


########################
## K-means clustering ##
########################

# A second very common clustering algorithm
# Most commonly used unsupervised machine learning algorithm for partitioning a 
# given data set into a set of k groups (k clusters)
# k is defined by the analyst
# objects within the same cluster are as similar as possible 
# (i.e., high intra-class similarity), whereas objects from different clusters 
# are as dissimilar as possible (i.e., low inter-class similarity)

# For reproducibility
set.seed(101)

# You have to transpose the matrix t()
# so that you can calculate distances between patients
# centers = 5 defines the k = 5
kclu=kmeans(t(dat), centers = 5) 
str(kclu)

# Number of data points in each cluster
table(kclu$cluster)

# View results using fviz_cluster function in factoextra library
library(factoextra)
fviz_cluster(kclu, data = t(dat), geom = "point")

# Because the k needs to be defined before performing k-means clustering
# it is advantageous to try different k values and examine results
k2 <- kmeans(t(dat), centers = 2)
k3 <- kmeans(t(dat), centers = 3)
k4 <- kmeans(t(dat), centers = 4)
k5 <- kmeans(t(dat), centers = 5)

# Plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = t(dat)) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point", data = t(dat)) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point", data = t(dat)) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point", data = t(dat)) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

# These plots tell you what the distribution of each cluster is
# But this analysis does not tell you the optimal number of clusters
# There are methods to get this: The factoextra package make this easy
# Methods: Elbow method, Silhouette method, Gap statistic
# The fviz_nbclust() function calculate and visualize optimal # of clusters

# Elbow method/Total within-cluster sum of square
fviz_nbclust(t(dat), kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2) +
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(t(dat), kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")

# Gap statistic
fviz_nbclust(t(dat), kmeans, method = "gap_stat", nboot = 50) +
  labs(subtitle = "Gap statistic method")

# In this example three different methods recommend three different k values
# Based on these you can pick a clustering value and re-run k-means and
# clustering visualization as above
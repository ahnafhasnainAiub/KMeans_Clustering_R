# load the required libraries
library(dplyr)
library(ggplot2)
library(cluster)

# load the data
data <- read.csv("F:/New folder/DS Final Project/fetal_health.csv",
                 header = TRUE, sep = ",")

# Check for missing values
sum(is.na(data))

# Check for duplicate values
sum(duplicated(data))

# Remove the duplicate values
data <- data[!duplicated(data),]

# Remove the target variable from the data set
FetalHealth <- data[,-22]

# Normalize the data
normali <- function(x) {
  (x-min(x))/(max(x)-min(x))
}

FetalHealth <- as.data.frame(lapply(FetalHealth, normali))

# K-Means Clustering
kmeans_cluster <- kmeans(FetalHealth, centers = 3, nstart = 25)

# Add the cluster labels to the data set
data$cluster <- as.factor(kmeans_cluster$cluster)

# print the cluster centers
kmeans_cluster$centers

# print the number of individuals in each cluster
table(kmeans_cluster$cluster)

# Plot the clusters
plot_cluster <- function(data, cluster, var1, var2) {
  ggplot(data, aes_string(x=var1, y=var2, color=cluster)) + geom_point() +
    labs(title = paste("K-means Clustering Results with", var1, "and", var2), x = var1, y = var2)
}

plot_cluster(data, "cluster", "baseline.value", "accelerations")
plot_cluster(data, "cluster", "baseline.value", "fetal_movement")
plot_cluster(data, "cluster", "baseline.value", "uterine_contractions") 
plot_cluster(data, "cluster", "baseline.value", "light_decelerations")




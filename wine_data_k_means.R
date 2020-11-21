# Brady Lange
# CSIS 239
# 5/2/18
# Program 5
# This program uses the k-means algorithm to decipher what attributes determine the type of wine and explores the most appropriate k value.

graphics.off()
rm(list = ls())
setwd("C:/Users/brady/Documents/CSIS 239/Assignment 5")
library("ggplot2")

# 1.)
# Downloading the wine data set from UCI's machine learning repository
d <- read.csv("wine.data.csv", header = F)

# 2.)
# Dropping the first column
d <- d[ ,-1]

# 3.)
# Giving the columns header names
names(d) <- c("Alcohol", "Malic Acid", "Ash", "Alcalinity of Ash",
              "Magnesium", "Total Phenols", "Flavanoids", 
              "Nonflavanoid Phenols", "Proanthocyanins", 
              "Color Intensity", "Hue", "OD280/OD315 of Diluted Wines",
              "Proline")

# 4.)
# Creating a summary of the data
summary(d)

# 5.)
# Running tests to determine if the hypothesis (H1) of a relationship between
# alcohol content of wine and its color intensity is supported
dlm <- lm(d$Alcohol ~ d$`Color Intensity`)
summary(dlm)
# There is a siginificant relationship between alcohol content and the color intensity 
cor.test(x = d$Alcohol, y = d$`Color Intensity`)

# 6.)
# Rescaling the data
d[,1:2] <- scale(d[,1:2])

# 7.)
# Running a k-means algorithm using 5 clusters and 1 random centroid starting placement
results1 <- kmeans(d, centers = 5, nstart = 1)

# 8.)
# Running a k-means algorithm a second time to compare results with the first instance since it will vary because
# k-means is a non-deterministic algorithm
results2 <- kmeans(d, centers = 5, nstart = 1)
# Examining the results
results
results2
which(results1$cluster == results2$cluster)
results1$cluster == results2$cluster
# The results are completely different

# 9.)
# Repeating steps 7 and 8, this time using 20 random centroid-starting placements for both
new_results1 <- kmeans(d, centers = 5, nstart = 20)
new_results2 <- kmeans(d, centers = 5, nstart = 20)
# Examining the results
new_results1
new_results2
which(new_results1$cluster == new_results2$cluster)
new_results1$cluster == new_results2$cluster
# The data has become more accurate compared to each other

# 10.)
# Determining how many elements belong to each cluster
names(new_results1$size) <- 1:5
new_results1$size
names(new_results2$size) <- 1:5
new_results2$size

# 11.)
# Determining the coordinates of the second cluster
new_results1$centers[2, ]

# 12.)
# Determining the within-cluster sum of squares for each of the five clusters
# Then determining the total WCSS for the model
# WCSS
wcss <- new_results1$withinss
# Total WCSS
twcss <- new_results1$tot.withinss

# 13.)
# Optimizing k for the model, testing the value from 1 to 20 to find the best result
wss = 0
for(i in 1:20) 
{
  wss[i] <- sum(kmeans(d[,1:4], centers = i, nstart = 20)$withinss)
}

# 14.)
# Creating an "elbow" plot of the within-cluster sum of squares
d2 <- data.frame(NumCluster = 1:length(wss), WithinSS = wss)
ggplot(d2, aes(x = NumCluster, y = WithinSS)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = 1:20) +
  theme(panel.grid.minor = element_blank()) + 
  ggtitle("WithinSS by Number of clusters")

# 15.)
# Determining the reasonable number of clusters based upon the "elbow" plot
# 3 seems like the reasonable amount of clusters because
# it will not underfit or overfit the data.

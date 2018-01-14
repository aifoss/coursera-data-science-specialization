################################################################################
## Source: Coursera
## Specialization: Data Science
## Course: 4 Exploratory Data Analysis
## Week: Week 3
## File: week-03-lessons.R
## Date: 2016-02-01
################################################################################


################################################################################
## Lesson 1: Hiearchical Clustering
################################################################################

set.seed(1234)
par(mar=c(0,0,0,0))
x <- rnorm(12, mean = rep(1:3, each=4), sd=0.2)
y <- rnorm(12, mean = rep(c(1,2,1), each=4), sd=0.2)
plot(x, y, col="blue", pch=19, cex=2)
text(x+0.05, y+0.05, labels=as.character(1:12))

## calculate pairwise distance between all data points
data_frame <- data.frame(x=x, y=y)
dist_xy <- dist(data_frame)

## draw dendrogram using hierachical clustering
h_clustering <- hclust(dist_xy)
plot(h_clustering)
plot(as.dendrogram(h_clustering))

# add lines
abline(h=1.5, col="blue")
abline(h=.4, col="red")
abline(h=0.05, col="green")

## heatmap
data_frame <- data.frame(x=x, y=y)
set.seed(143)
data_matrix <- as.matrix(data_frame)[sample(1:12), ]
print(heatmap(data_matrix))
print(heatmap(data_matrix, col=cm.colors(25)))

################################################################################
## Lesson 2: K-Means Clustering & Dimension
################################################################################

############################################################
## K-means clustering
############################################################

## create k-means clustering
data_frame <- data.frame(x, y)
kmeans_obj <- kmeans(data_frame, centers=3)
print(names(kmeans_obj))
print(kmeans_obj$cluster)

## plot k-means clustering
par(mar = rep(0.2,4))
plot(x, y, col=kmeans_obj$cluster, pch=19, cex=2)
points(kmeans_obj$centers, col=1:3, pch=3, cex=3, lwd=3)

## create k-means heatmap
set.seed(1234)
data_matrix <- as.matrix(data_frame)[sample(1:12), ]
kmeans_obj_2 <- kmeans(data_matrix, centers=3)
par(mfrow=c(1,2), mar=c(2, 4, 0.1, 0.1))
image(t(data_matrix)[, nrow(data_matrix):1], yaxt="n")
image(t(data_matrix)[, order(kmeans_obj_2$cluster)], yaxt="n")

############################################################
## Principal components analysis (PCA)
## and singular value decomposition (SVD)
############################################################

## cluster random matrix data
set.seed(12345)
data_matrix <- matrix(rnorm(400), nrow=40)

par(mar=rep(0.2,4))
image(1:10, 1:40, t(data_matrix)[, nrow(data_matrix):1])

par(mar=rep(0.2,4))
heatmap(data_matrix)

## what if we add a pattern?
set.seed(678910)
for (i in 1:40) {
    # flip a coin
    coin_flip <- rbinom(1, size=1, prob=0.5)
    # if coin is head, add a common pattern to that row
    if (coin_flip) {
        data_matrix[i, ] <- data_matrix[i, ] + rep(c(0,3), each=5)
    }
}

par(mar=rep(0.2,4))
image(1:10, 1:40, t(data_matrix)[, nrow(data_matrix):1])

par(mar=rep(0.2,4))
heatmap(data_matrix)

## patterns in rows and columns
hh <- hclust(dist(data_matrix))
data_matrix_ordered <- data_matrix[hh$order, ]

par(mfrow=c(1,3))
image(t(data_matrix_ordered)[, nrow(data_matrix_ordered):1])
plot(rowMeans(data_matrix_ordered), 40:1, xlab="Row Mean", ylab="Row", pch=19)
plot(colMeans(data_matrix_ordered), xlab="Column", ylab="Column Mean", pch=19)

##################################################
## SVD and PCA
##################################################

## problems
# You have multivariate variables X1, ..., Xn, so X1 = (X11, ..., X1n).
# 1) Find a new set of multivariate variables that are uncorrelated
#       and explain as much variance as possible.
# 2) If you put all the variables together in one matrix, find the best matrix
#       created with fewer variables that explains the original data.

## SVD and PCA
# If X is a matrix with each variable in a column and each observations in a row
#   then SVD is a matrix decomposition that represents X as a matrix product
#   of 3 matrices:
#       X = UDV^t
#   where U (left singular vectors) are orthogonal, columns of V (right singular
#       vectors) are orthogonal, and D is a diagonal matrix of singular values.
# PCA is an application of SVD.
# The principal components are equal to the right singular values
#   if you first scale the data by subtracting the column mean
#   and dividing each column by its standard deviation.

##################################################
## Components of SVD - u and v
##################################################

svd1 <- svd(scale(data_matrix_ordered))

par(mfrow=c(1,3))
image(t(data_matrix_ordered)[, nrow(data_matrix_ordered):1])
plot(svd1$u[, 1], 40:1, xlab="Row", ylab="First left singular vector",
     pch=19)
plot(svd1$v[, 1], xlab="Column", ylab="First right singular vector",
     pch=19)

##################################################
## Components of SVD - Variance explained
##################################################

par(mfrow=c(1,2))
plot(svd1$d, xlab="Column", ylab="Singular value", pch=19)
plot(svd1$d^2/sum(svd1$d^2), xlab="Column", ylab="Prop. of variance explained",
     pch=19)

##################################################
## Relationship to principal components
##################################################

svd1 <- svd(scale(data_matrix_ordered))
pca1 <- prcomp(data_matrix_ordered, scale=TRUE)
plot(pca1$rotation[, 1], svd1$v[, 1], pch=19, xlab="Principal Component 1",
     ylab="Right Singular Vector 1")
abline(c(0,1))

##################################################
## Components of SVD - Variance explained (2)
##################################################

constant_matrix <- data_matrix_ordered*0
for (i in 1:dim(data_matrix_ordered)[1]) {
    constant_matrix[i, ] <- rep(c(0,1), each=5)
}
svd1 <- svd(constant_matrix)
par(mfrow=c(1,3))
image(t(constant_matrix)[, nrow(constant_matrix):1])
plot(svd1$d, xlab="Column", ylab="Singular value", pch=19)
plot(svd1$d^2/sum(svd1$d^2), xlab="Column", ylab="Prop. of variance explained",
     pch=19)

##################################################
## What if we add a second pattern?
##################################################

set.seed(678910)
for (i in 1:40) {
    coin_flip_1 <- rbinom(1, size=1, prob=0.5)
    coin_flip_2 <- rbinom(1, size=1, prob=0.5)
    if (coin_flip) {
        data_matrix[i, ] <- data_matrix[i, ] + rep(c(0,3), each=5)
    }
    if (coin_flip_2) {
        data_matrix[i, ] <- data_matrix[i, ] + rep(c(0,5), 5)
    }
}

hh <- hclust(dist(data_matrix))
data_matrix_ordered <- data_matrix[hh$order, ]

##################################################
## SVD - true patterns
##################################################

svd2 <- svd(scale(data_matrix_ordered))
par(mfrow=c(1,3))
image(t(data_matrix_ordered)[, nrow(data_matrix_ordered):1])
plot(rep(c(0,1), each=5), pch=19, xlab="Column", ylab="Pattern 1")
plot(rep(c(0,1), 5), pch=19, xlab="Column", ylab="Pattern 2")

##################################################
## v and patterns of variance in rows
##################################################

svd2 <- svd(scale(data_matrix_ordered))
par(mfrow=c(1,3))
image(t(data_matrix_ordered)[, nrow(data_matrix_ordered):1])
plot(svd2$v[, 1], pch=19, xlab="Column", ylab="First right singular vector")
plot(svd2$v[, 2], pch=19, xlab="Column", ylab="Second right singular vector")

##################################################
## d and variance explained
##################################################

svd1 <- svd(scale(data_matrix_ordered))
par(mfrow=c(1,2))
plot(svd1$d, xlab="Column", ylab="Singular value", pch=19)
plot(svd1$d^2/sum(svd1$d^2), xlab="Column", ylab="Percent of variance explained",
     pch=19)

##################################################
## Missing values
##################################################

data_matrix_2 <- data_matrix_ordered
## randomly insert some missing data
data_matrix_2[sample(1:100, size=40, replace=FALSE)] <- NA
#svd1 <- svd(scale(data_matrix_2)) ## doesn't work

##################################################
## Imputing
##################################################

#source("https://bioconductor.org/biocLite.R")
#biocLite()
#biocLite("impute")

library(impute)

data_matrix_2 <- data_matrix_ordered
data_matrix_2[sample(1:100, size=40, replace=FALSE)] <- NA
data_matrix_2 <- impute.knn(data_matrix_2)$data
svd1 <- svd(scale(data_matrix_ordered))
svd2 <- svd(scale(data_matrix_2))

par(mfrow=c(1,2))
plot(svd1$v[,1], pch=19)
plot(svd2$v[,1], pch=19)

################################################################################
## Lesson 3: Working with Color
################################################################################

##################################################
## Color utilities in R
##################################################

# The grDevices package has two functions:
# (1) colorRamp
#       takes a palette of colors and returns a function that takes values
#       between 0 and 1, indicating the extremes of the color palette
# (2) colorRampPalette
#       takes a palette of colors and returns a function that takes integer
#       arguments and returns a vector of color interpolating the palette

# Function colors() lists the names of colors that can be used.

##################################################
## colorRamp
##################################################

pal <- colorRamp(c("red","blue"))
print(pal(0)) # red
print(pal(1)) # blue
print(pal(0.5)) # 50% red, 50% blue

print(pal(seq(0,1,len=10)))

##################################################
## colorRampPalette
##################################################

pal <- colorRampPalette(c("red","yellow"))
print(pal(2))
print(pal(10))

##################################################
## RColorBrewer package
##################################################

# There are 3 types of palettes:
#   (1) Sequential
#   (2) Diverging
#   (3) Qualitative

##################################################
## RColorBrewer and colorRampPalette
##################################################

library(RColorBrewer)
cols <- brewer.pal(3, "BuGn") # number of colors and name of palette
print(cols)
pal <- colorRampPalette(cols)
image(volcano, col=pal(20))

##################################################
## smoothScatter
##################################################

x <- rnorm(10000)
y <- rnorm(10000)
smoothScatter(x, y)

##################################################
## Some other plotting notes
##################################################

# rgb function can be used to produce any color via RGB proportions.
# Color transparency can be added via alpha parameters to rgb.
# colorspace package can be used for a different control over colors.

x = rnorm(1000)
y = rnorm(1000)
plot(x, y, pch=19)
plot(x, y, col=rgb(0,0,0,0.2), pch=19)

################################################################################
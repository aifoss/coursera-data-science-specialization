################################################################################
## Source: Coursera
## Specialization: Data Science
## Course: 4 Exploratory Data Analysis
## Week: Week 4
## File: week-04-lessons.R
## Date: 2016-02-06
################################################################################


################################################################################
## Lesson 1: Case Studies
################################################################################

############################################################
## Clustering case study
############################################################

load("data/samsungData.rd")
names(samsungData)[1:12]
table(samsungData$activity)

## Plotting average acceleration for first subject
par(mfrow=c(1,2), mar=c(5,4,1,1))
samsungData <- transform(samsungData, activity=factor(activity))
sub1 <- subset(samsungData, subject==1)
plot(sub1[, 1], col=sub1$activity, ylab=names(sub1)[1])
plot(sub1[, 2], col=sub1$activity, ylab=names(sub1)[2])
legend("bottomright", 
       legend=unique(sub1$activity), 
       col=unique(sub1$activity),
       pch=1)

## Clustering based just on average acceleration
source("myplclust.R")
distanceMatrix <- dist(sub1[, 1:3])
hclustering <- hclust(distanceMatrix)
myplcluster(hclustering, lab.col=unclass(sub1$activity))

## Plotting max acceleration for the first subject
par(mfrow=c(1,2))
plot(sub1[, 10], pch=19, col=sub1$activity, ylab=names(sub1)[10])
plot(sub1[, 11], pch=19, col=sub1$activity, ylab=names(sub1)[11])

## Clustering based on maximum acceleration
source("myplclust.R")
distanceMatrix <- dist(sub1[, 10:12])
hclustering <- hclust(distanceMatrix)
myplclust(hclustering, lab.col=unclass(sub1$activity))

## Singular value decomposition
svd1 <- svd(scale(sub1[, -c(562,563)]))
par(mfrow=c(1,2))
plot(svd1$u[, 1], col=sub1$activity, pch=19)
plot(svd1$u[, 2], col=sub1$activity, pch=19)

## Find maximum contributor
plot(svd1$v[, 2], pch=19)

## New clustering with maximum contributor
maxContrib <- which.max(svd$v[, 2])
names(samsungData)[maxContrib]
distanceMatrix <- dist[sub1[, c(10:12, maxContrib)]]
hclustering <- hclust(distanceMatrix)
myplclust(hclustering, lab.col=unclass(sub1$activity))

## K-means clustering (nstart=1)
kClust <- kmeans(sub1[, -c(562,563)], centers=6)
table(kClust$cluster, sub1$activity)

## K-means clustering (nstart=100)
kClust <- kmeans(sub1[, -c(562,563)], centers=6, nstart=100)
table(kClust$cluster, sub1$activity)

## Cluster 1 variable centers (laying)
plot(kClust$center[1, 1:10], pch=19, ylab="Cluster Center", xlab="")

## Cluster 2 vairable centers (walking)
plot(kClust$center[4, 1:10], pch=19, ylab="Cluster Center", xlab="")

############################################################
## Air pollution case study
############################################################

## Read datasets
pm0 <- read.table("RD_501_88101_1999-0.txt",
                  comment.char="#",
                  header=FALSE,
                  sep="|",
                  na.strings="")
dim(pm0)
head(pm0)

pm1 <- read.table("RD_501_88101_2012-0.txt",
                  comment.char="#",
                  header=FALSE,
                  sep="|",
                  na.strings="")

## Get column names
cnames <- readLines("RD_501_88101_1999-0.txt", 1)
cnames <- strsplit(cnames, "|", fixed=TRUE)
names(pm0) <- make.names(cnames[[1]])
names(pm1) <- make.names(cnames[[1]])

## Take out sample value column
x0 <- pm0$Sample.Value
class(x0)
str(x0)
summary(x0)
mean(is.na(x0))

x1 <- pm1$Sample.Value
str(x1)
summary(x1)
mean(is.na(x1))

## Create boxplots
boxplot(x0, x1)
boxplot(log10(x0), log10(x1))

## Investigate negative values
negative <- x1 < 0
str(negative)
sum(negative, na.rm=TRUE)
mean(negative, na.rm=TRUE)

## Investigate dates
dates <- pm1$Date
str(dates)
dates <- as.Date(as.character(date), "%Y%m%d")

## Draw histograms for negative months
hist(dates[negative], "month")

## Take out a site from each dataset
site0 <- unique(subset(pm0, State.Code==36, c(County.Code, Site.ID)))
site1 <- unique(subset(pm0, State.Code==36, c(County.Code, Site.ID)))

site0 <- paste(site0[,1], site0[,2], sep=".")
site1 <- paste(site1[,1], site1[,2], sep=".")

str(site0)
str(site1)

## Get monitors that are in both sites
both <- intersect(site0, site1)

pm0$County.Site <- with(pm0, paste(County.Code, Site.ID), sep=".")
pm1$County.Site <- with(pm1, paste(County.Code, Site.ID), sep=".")

cnt0 <- subset(pm0, State.Code==36 & County.Site %in% both)
cnt1 <- subset(pm1, State.Code==36 & County.Site %in% both)

## Split 
sapply(split(cnt0, cnt0$County.Site), nrow)
sapply(split(cnt1, cnt1$County.Site), nrow)

pm0sub <- subset(pm0, State.Code==36 & County.Code==63 & Site.ID=2008)
pm1sub <- subset(pm1, State.Code==36 & County.Code==63 & Site.ID=2008)

## Create scatter plots
dates0 < pm0sub$Date
dates0 <- as.Date(as.character(dates1), "%Y%m%d")
x0sub <- pm0sub$Sample.Value
plot(dates0, x-sub)

dates1 < pm1sub$Date
dates1 <- as.Date(as.character(dates1), "%Y%m%d")
x1sub <- pm1sub$Sample.Value
plot(dates1, x1sub)

par(mfrow=c(1,2), mar=c(4,4,2,1))
plot(dates0, x0sub, pch=20)
abline(h=median(x0sub, na.rm=T))
plot(dates1, x1sub, pch=20)
abline(h=median(x1sub, na.rm=T))

rng <- range(x0sub, x1sub, na.rm=T)
par(mfrow=c(1,2))
plot(dates0, x0sub, pch=20, ylim=rng)
abline(h=median(x0sub, na.rm=T))
plot(dates1, x1sub, pch=20, ylim=rng)
abline(h=median(x1sub, na.rm=T))

## Calculate average sample value by state
mn0 <- with(pm0, tapply(Sample.Value, State.Code, mean, na.rm=T))
summary(mn0)
mn1 <- with(pm1, tapply(Sample.Value, State.Code, mean, na.rm=T))
summary(mn1)

d0 <- data.frame(state=names(mn0), mean=mn0)
d1 <- data.frame(state=names(mn1), mean=mn1)

head(d0)
head(d1)

## Merge
mrg <- merge(d0, d1, by="state")

with(mrg, plot(rep(1,52), mrg[,2], xlim=c(.5,2.5)))
with(mrg, points(rep(2,52), mrg[,3]))
segments(rep(1,52), mrg[,2], rep(2,52), mrg[,3])

mrg[mrg$mean.x < mrg$mean.y, ]
################################################################################
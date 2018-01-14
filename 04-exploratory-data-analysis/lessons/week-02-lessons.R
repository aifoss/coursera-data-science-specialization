################################################################################
## Source: Coursera
## Specialization: Data Science
## Course: 4 Exploratory Data Analysis
## Week: Week 2
## File: week-02-lessons.R
## Date: 2016-02-01
################################################################################


################################################################################
## Lesson 1: Lattice Plotting System
################################################################################

############################################################
## lattice plotting system (part 1)
############################################################

## lattice functions generally take a formula for their first argument
## xyplot(y ~ x | f * g, data)
## y is y-axis variable
## x is x-axis variable
## f and g are optional categorical conditioning variables
## * indicates interaction between two variables
## data is the data frame or list from which the variables should be looked up

##################################################
## Simple lattice plot
##################################################

## simple scatterplot
library(lattice)
library(datasets)
print(xyplot(Ozone~Wind, data=airquality))

## multi-panel scatterplot
airquality <- transform(airquality, Month=factor(Month))
print(xyplot(Ozone~Wind | Month, data=airquality, layout=c(5,1)))

##################################################
## lattice behavior
##################################################

## Base graphics functions plot data directly to the graphics device.
## Lattice graphics functions return an object of class trellis.
## The print methods for lattice functions actually do the work of plotting
##      the data on the graphics device.
## Lattice functions return plot objects that can be stored.

p <- xyplot(Ozone~Wind, data=airquality) ## nothing happens
print(p) # plot appears

xyplot(Ozone~Wind, data=airquality) ## auto-printing

############################################################
## lattice plotting system (part 2)
############################################################

##################################################
## lattice panel functions
##################################################

## Lattice functions have a panel function which controls what happens
##      inside each panel of the plot.
## Panel functions receive the x/y coordinates of the data points in their panel.

## built-in panel function
set.seed(10)
x <- rnorm(100)
f <- rep(0:1, each=50)
y <- x + f - f * x + rnorm(100, sd=0.5)
f <- factor(f, labels=c("Group 1", "Group 2"))
print(xyplot(y~x | f, layout=c(2,1))) ## plot with 2 panels

## custom panel function adding median line
print(xyplot(y~x | f, panel = function(x, y, ...) {
    panel.xyplot(x, y, ...) ## first call the default panel function for 'xyplot'
    panel.abline(h=median(y), lty=2) ## add a horizontal line at the median
}))

## custom panel function adding regression line
print(xyplot(y~x | f, panel = function(x, y, ...) {
    panel.xyplot(x, y, ...) ## first call default panel function
    panel.lmline(x, y, col=2) ## overlay a simple linear regression line
}))

################################################################################
## Lesson 2: ggplot2
################################################################################

############################################################
## qplot
############################################################

## Factors are important for indicating subsets of the data.
## - Facets should be labeled.

library(ggplot2)
str(mpg)

print(qplot(displ, hwy, data=mpg))

## coloring by factor variable group
print(qplot(displ, hwy, data=mpg, color=drv))

## adding a geom
print(qplot(displ, hwy, data=mpg, geom=c("point", "smooth")))

## histogram by group
print(qplot(hwy, data=mpg, fill=drv))

## facets
print(qplot(displ, hwy, data=mpg, facets=.~drv))
print(qplot(hwy, data=mpg, facets=drv~., binwidth=2))

## density smooth
qplot(log(eno), data=maacs, geom="density")
qplot(log(eno), data=maacs, geom="density", color=mopos)

## scatterplots
qplot(log(pm25), log(eno), data=maacs)
qplot(log(pm25), log(eno), data=maacs, shape=mopos)
qplot(log(pm25), log(eno), data=maacs, color=mopos)
qplot(log(pm25), log(eno), data=maacs, 
      color=mopos, geom=c("point","smooth"), method="lm")
qplot(log(pm25), log(eno), data=maacs, 
      color=mopos, geom=c("point","smooth"), method="lm",
      facets=.~mopos)

############################################################
## ggplot
############################################################

## building layers
g <- qqplot(maacs, aes(logpm25, NocturnalSympt))
g + geom_point()
g + geom_point() + geom_smooth()
g + geom_point() + geom_smooth(method="lm")
g + geom_point() + facet_grid(.~bmicat) + geom_smooth(method="lm")

## modifying aesthetics
g + geom_point(color="steelblue", ## constant value
               size=4, alpha=1/2) 
g + geom_point(aes(color=bmicat), ## data variable
               size=4, alpha=1/2)

## modifying labels
g + geom_point(aes(color=bmicat), size=4, alpha=0.5) +
    labs(title="MAACS Cohor") + 
    labs(x = expression("log " * PM[2.5]), y = "Nocturnal Symptoms")

## customizing smooth
g + geom_point(aes(color=bmicat), size=4, alpha=0.5) +
    geom_smooth(size=4, linetype=3, method="lm", se=F)

## changing the theme
g + geom_point(aes(color=bmicat)) +
    theme_bw(base_family="Times")

## axis limits
g + geom_line() + ylim(-3, 3)
g + geom_line() + coord_cartesian(ylim=c(-3,3))

## cutting data
cutpoints <- quantile(maacs$logno2_new, seq(0, 1, length=4), na.rm=TRUE)
maacs$no2dec <- cut(maacs$lgno2_new, cutpoints)
levels(maacs$no2dec)

## final plot
g <- ggplot(maacs, aes(logpm25, NocturnalSympt))
g + 
    geom_point(alpha=1/3) +
    facet_wrap(bmicat ~ no2dec, nrow=2, ncol=4) +
    geom_smooth(method="lm", se=F, col="steelblue") +
    them_bw(base_family="Avenir", base_size=10) +
    labs(x = expression("log " * PM[2.5])) +
    labs(y = "Nocturnal Symptoms") +
    labs(title = "MAACs Cohor")

################################################################################
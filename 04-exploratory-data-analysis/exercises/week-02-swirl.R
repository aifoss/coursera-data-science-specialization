################################################################################
## Source: Coursera
## Specialization: Data Science
## Course: 4 Exploratory Data Analysis
## Week: Week 2
## File: week-02-swirl.R
## Date: 2016-02-01
################################################################################


################################################################################
## 6: Latice Plotting System
################################################################################

xyplot(Ozone~Wind, data=airquality)
xyplot(Ozone~Wind, data=airquality, col="red", pch=8, main="Big Apple Data")
xyplot(Ozone~Wind | as.factor(Month), data=airquality, layout=c(5,1))

p <- xyplot(Ozone~Wind, data=airquality)
p

xyplot(y~x|f, layout=c(2,1))

p <- xyplot(y ~ x | f, panel = function(x, y, ...) {
    panel.xyplot(x, y, ...)  ## First call the default panel function for 'xyplot'
    panel.abline(h = median(y), lty = 2)  ## Add a horizontal line at the median
})
print(p)
invisible()

p2 <- xyplot(y ~ x | f, panel = function(x, y, ...) {
    panel.xyplot(x, y, ...)  ## First call default panel function
    panel.lmline(x, y, col = 2)  ## Overlay a simple linear regression line
})
print(p2)
invisible()

xyplot(price~carat|color*cut, data=diamonds, 
       strip=FALSE, pch=20, 
       xlab=myxlab, ylab=myylab, main=mymain)

################################################################################
## 7: Working with Colors
################################################################################

sample(colors())

pal <- colorRamp(c("red","blue"))
pal(0)

pal(seq(0,1,len=6))

p1 <- colorRampPalette(c("red","blue"))
p1(2)

p2 <- colorRampPalette(c("red","yellow"))
p2(2)

plot(x, y, pch=19, col=rgb(0,.5,.5))
plot(x, y, pch=19, col=rgb(0,.5,.5,.3))

cols <- brewer.pal(3, "BuGn")
pal <- colorRampPalette(cols)
image(volcano, col=pal(20))

################################################################################
## 8: GGPlot2 Part1
################################################################################

qplot(displ, hwy, data=mpg)
qplot(displ, hwy, data=mpg, color=drv)
qplot(displ, hwy, data=mpg, color=drv, geom=c("point","smooth"))

# specifying the y parameter only, without an x argument, 
# plots the values of the y argument in the order in which they occur 
# in the data
qplot(y=hwy, data=mpg, color=drv)

qplot(drv, hwy, data=mpg, geom="boxplot")
qplot(drv, hwy, data=mpg, geom="boxplot", color=manufacturer)

qplot(hwy, data=mpg, fill=drv)

qplot(displ, hwy, data=mpg, facets=.~drv)
qplot(hwy, data=mpg, facets=drv~., binwidth=2)

################################################################################
## 9: GGPlot2 Part2
################################################################################


################################################################################
## 10: GGPlot2 Extras
################################################################################


################################################################################
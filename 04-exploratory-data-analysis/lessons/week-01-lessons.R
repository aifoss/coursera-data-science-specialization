################################################################################
## Source: Coursera
## Specialization: Data Science
## Course: 4 Exploratory Data Analysis
## Week: Week 1
## File: week-01-lessons.R
## Date: 2016-01-26
################################################################################


################################################################################
## Lesson 1: Graphs
################################################################################

graphs <- function() {

    ############################################################
    ## Exploratory Graphs (part 1)
    ############################################################
    
    pollution <- read.csv("data/avgpm25.csv", 
                          colClasses=c("numeric","character","factor",
                                       "numeric","numeric"))
    head(pollution)
    
    ##################################################
    ## Simple summaries of data (1 dimension)
    ##################################################
    
    ## Five-number summary
    summary(pollution$pm25)
    
    ## Boxplots
    boxplot(pollution$pm25, col="blue")
    abline(h=12)
    
    ## Histograms
    hist(pollution$pm25, col="green", breaks=100)
    rug(pollution$pm25)
    abline(v=12, lwd=2)
    abline(v=median(pollution$pm25), col="magenta", lwd=4)
    
    ## Density plot
    
    ## Barplot
    barplot(table(pollution$region), 
            col="wheat", main="Number of Counties in East Region")
    
    ############################################################
    ## Exploratory Graphs (part 2)
    ############################################################
    
    ##################################################
    ## >=2 dimensions
    ##################################################
    
    ## Multiple boxplots
    boxplot(pm25 ~ region, data=pollution, col="red")
    
    ## Multiple histograms
    par(mfrow=c(2,1), mar=c(4,4,2,1))
    hist(subset(pollution, region="east")$pm25, col="green")
    hist(subset(pollution, region="west")$pm25, col="green")
    
    ## Scatterplots
    with(pollution, plot(latitude, pm25, col=region))
    abline(h=12, lwd=2, lty=2)
    
    ## Multiple scatterplots
    par(mfrow=c(1,2), mar=c(5,4,2,1))
    with(subset(pollution, region="west"), plot(latitude, pm25, main="West"))
    with(subset(pollution, reigion="east"), plot(latitude, pm25, main="East"))

}

################################################################################
## Lesson 2: Plotting
################################################################################

plot_systems <- function() {

    ############################################################
    ## Plotting Systems in R
    ############################################################
    
    ## Base plotting system
    library(datasets)
    data(cars)
    with(cars, plot(speed, dist))
    
    ## Lattice system
    library(lattice)
    state <- data.frame(state.x77, region=state.region)
    xyplot(Life.Exp ~ Income | region, data=state, layout=c(4,1))
    
    ## ggplot2 system
    library(ggplot2)
    data(mpg)
    qplot(displ, hwy, data=mpg)
    
    ############################################################
    ## Base Plotting System (part 1)
    ############################################################
    
    library(datasets)
    
    ## Histogram
    hist(airquality$Ozone)
    
    ## Scatterplot
    with(airquality, plot(Wind, Ozone))
    
    ## Boxplot
    airquality <- transform(airquality, Month=factor(Month))
    boxplot(Ozone ~ Month, airquality, xlab="Month", ylab="Ozone (ppb)")
    
    ############################################################
    ## Base Plotting System (part 2)
    ############################################################
    
    ## Base plot with annotation
    library(datasets)
    with(airquality, plot(Wind, Ozone, 
                          main="Ozone and Wind in New York City", 
                          type="n"))
    with(subset(airquality, Month==5), points(Wind, Ozone, col="blue"))
    with(subset(airquality, Month!=5), points(Wind, Ozone, col="red"))
    legend("topright", pch=1, col=c("blue","red"), legend=c("May","Other Months"))
    
    ## Base plot with regression line
    with(airquality, plot(Wind, Ozone, 
                          main="Ozone and Wind in New York City",
                          pch=20))
    model <- lm(Ozone ~ Wind, airquality)
    abline(model, lwd=2)
    
    ## Multiple base plots
    par(mfrow=c(1,2))
    with(airquality, {
        plot(Wind, Ozone, main="Ozone and Wind")
        plot(Solar.R, Ozone, main="Ozone and Solar Radiation")
    })
    
    par(mfrow=c(1,3), mar=c(4,4,2,1), oma=c(0,0,2,0))
    with(airquality, {
        plot(Wind, Ozone, main="Ozone and Wind")
        plot(Solar.R, Ozone, main="Ozone and Solar Radiation")
        plot(Temp, Ozone, main="Ozone and Temperature")
        mtext("Ozone and Weather in New York City", outer=TRUE)
    })

}

################################################################################
## Lesson 3: Graphics Devices
################################################################################

graphics_devices <- function() {

    ############################################################
    ## Graphics Devices in R (part 1)
    ############################################################
    
    library(datasets)
    if (!file.exists("output")) {
        dir.create("output")
    }
    pdf(file="output/myplot.pdf")
    with(faithful, plot(eruptions, waiting))
    title(main="Old Faithful Geyser data")
    dev.off()
    
    ############################################################
    # Graphics Devices in R (part 2)
    ############################################################
    
    #dev.cur()
    #dev.set()
    
    # Copying plots - dev.copy(), dev.copy2pdf()
    library(datasets)
    with(faithful, plot(eruptions, waiting))
    title(main="Old Faithful Geyser data")
    dev.copy(png, file="output/geyserplot.png")
    dev.off()

}

################################################################################
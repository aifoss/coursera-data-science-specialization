################################################################################
## Source: Coursera
## Specialization: Data Science
## Course: 4 Exploratory Data Analysis
## Week: Week 4
## Project: Project 2
## File: plot1.R
## Date: 2016-02-06
################################################################################


################################################################################
## Question 1:
## Have total emissions from PM2.5 decreased in the United States 
## from 1999 to 2008? 
## Using the base plotting system, make a plot showing the total PM2.5 emission 
## from all sources for each of the years 1999, 2002, 2005, and 2008.
################################################################################

################################################################################
NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

library(dplyr)
total_emissions_by_year <- NEI %>%
    group_by(year) %>%
    summarise(total.emissions = sum(Emissions)) 

png(file="output/plot1.png", width=480, height=480)
par(mfrow=c(1,1), mar=c(5,5,4,2))

plot(total_emissions_by_year$year, 
     total_emissions_by_year$total.emissions,
     xlab="Year",
     ylab="Total Emissions (in tons)",
     main="Total Emissions in the United States during 1999-2008",
     type="l",
     col="red",
     xaxt="n")
axis(1, at=seq(1999, 2008, by=3), las=2)

dev.off()
################################################################################
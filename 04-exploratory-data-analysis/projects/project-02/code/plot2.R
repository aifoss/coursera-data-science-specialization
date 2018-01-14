################################################################################
## Source: Coursera
## Specialization: Data Science
## Course: 4 Exploratory Data Analysis
## Week: Week 4
## Project: Project 2
## File: plot2.R
## Date: 2016-02-06
################################################################################


################################################################################
## Question 2:
## Have the total emissions from PM2.5 decreased in the Baltimore City, Maryland
## (fips == "24510") from 1999 to 2008?
## Use the base plotting system to make a plot answering this question.
################################################################################

################################################################################
NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

library(dplyr)
baltimore_data <- subset(NEI, fips=="24510")
total_emissions_by_year <- baltimore_data %>%
    group_by(year) %>%
    summarise(total.emissions = sum(Emissions)) 

png(file="output/plot2.png", width=480, height=480)
par(mfrow=c(1,1), mar=c(5,5,4,2))

plot(total_emissions_by_year$year, 
     total_emissions_by_year$total.emissions,
     xlab="Year",
     ylab="Total Emissions (in tons)",
     main="Total Emissions in Baltimore City, Maryland 1999-2008",
     type="l",
     col="red",
     xaxt="n")
axis(1, at=seq(1999, 2008, by=3), las=2)

dev.off()
################################################################################
################################################################################
## Source: Coursera
## Specialization: Data Science
## Course: 4 Exploratory Data Analysis
## Week: Week 4
## Project: Project 2
## File: plot6.R
## Date: 2016-02-06
################################################################################


################################################################################
## Question 6:
## Compare emissions from motor vehicle sources in Baltimore City 
## with emissions from motor vehicle sources in Los Angeles County, California
## (fips=="06037").
## Which city has seen greater changes over time in motor vehicle emissions?
################################################################################

################################################################################
NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

library(dplyr)
vehicle_scc_subset <- SCC %>%
    filter(grepl("Vehicle", SCC.Level.Two))
scc_codes <- vehicle_scc_subset$SCC
baltimore_data <- subset(NEI, fips=="24510")
baltimore_emissions <- baltimore_data %>%
    filter(SCC %in% scc_codes) %>%
    group_by(year) %>%
    summarise(city="Baltimore",
              total.emissions=sum(Emissions))
los_angeles_data <- subset(NEI, fips=="06037")
los_angeles_emissions <- los_angeles_data %>%
    filter(SCC %in% scc_codes) %>%
    group_by(year) %>%
    summarise(city="Los Angeles",
              total.emissions=sum(Emissions))
emissions <- rbind(baltimore_emissions, los_angeles_emissions)

library(ggplot2)
ggplot(emissions, 
       aes(x=year, y=total.emissions, 
           group=city, colour=city)) + 
    geom_line() + 
    ggtitle("Vehicle Emissions Baltimore City vs. Los Angeles 1999-2008") +
    labs(x="Year", y="Emissions from Vehicle-Related Sources (in tons)") +
    scale_x_continuous(breaks=seq(1999, 2008, by=3))

ggsave(file="output/plot6.png", width=6, height=4.5)
################################################################################
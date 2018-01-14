################################################################################
## Source: Coursera
## Specialization: Data Science
## Course: 4 Exploratory Data Analysis
## Week: Week 4
## Project: Project 2
## File: plot5.R
## Date: 2016-02-06
################################################################################


################################################################################
## Question 5:
## How have emissions from motor vehicle sources changed from 1999–2008 
## in Baltimore City?
################################################################################

################################################################################
NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

library(dplyr)
vehicle_scc_subset <- SCC %>%
    filter(grepl("Vehicle", SCC.Level.Two))
scc_codes <- vehicle_scc_subset$SCC
baltimore_data <- subset(NEI, fips=="24510")
vehicle_emissions_by_year <- baltimore_data %>%
    filter(SCC %in% scc_codes) %>%
    group_by(year) %>%
    summarise(total.emissions=sum(Emissions))

library(ggplot2)
ggplot(vehicle_emissions_by_year, 
       aes(x=year, y=total.emissions)) + 
    geom_line(colour="red") + 
    ggtitle("Vehicle Emissions in Baltimore City 1999-2008") +
    labs(x="Year", y="Emissions from Vehicle-Related Sources (in tons)") +
    scale_x_continuous(breaks=seq(1999, 2008, by=3))

ggsave(file="output/plot5.png", width=4.5, height=4.5)
################################################################################
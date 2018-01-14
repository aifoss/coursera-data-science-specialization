################################################################################
## Source: Coursera
## Specialization: Data Science
## Course: 4 Exploratory Data Analysis
## Week: Week 4
## Project: Project 2
## File: plot3.R
## Date: 2016-02-06
################################################################################


################################################################################
## Question 3:
## Of the four types of sources indicated by the type (point, nonpoint, onroad, 
## nonroad) variable, which of these four sources have seen decreases 
## in emissions from 1999–2008 for Baltimore City? Which have seen increases 
## in emissions from 1999–2008? 
## Use the ggplot2 plotting system to make a plot answer this question.
################################################################################

################################################################################
NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

library(dplyr)
baltimore_data <- subset(NEI, fips=="24510")
total_emissions_by_year_and_type <- baltimore_data %>%
    group_by(year, type) %>%
    summarise(total.emissions = sum(Emissions)) 
total_emissions_by_year_and_type

library(ggplot2)
ggplot(total_emissions_by_year_and_type, 
       aes(x=year, y=total.emissions)) + 
    geom_line() + 
    facet_grid(. ~ type) +
    ggtitle("Total Emissions Per Source Type During 1999-2008") +
    labs(x="Year", y="Total Emissions (in tons)") +
    scale_x_continuous(breaks=seq(1999, 2008, by=3))

ggsave(file="output/plot3.png", width=9.5, height=4.5)
################################################################################
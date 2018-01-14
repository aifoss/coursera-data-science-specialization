################################################################################
## Source: Coursera
## Specialization: Data Science
## Course: 4 Exploratory Data Analysis
## Week: Week 4
## Project: Project 2
## File: plot4.R
## Date: 2016-02-06
################################################################################


################################################################################
## Question 4:
## Across the United States, how have emissions from coal combustion-related 
## sources changed from 1999–2008?
################################################################################

################################################################################
NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

library(dplyr)
coal_scc_subset <- SCC %>%
    filter(grepl("Coal", EI.Sector))
scc_codes <- coal_scc_subset$SCC
coal_emissions_by_year <- NEI %>%
    filter(SCC %in% scc_codes) %>%
    group_by(year) %>%
    summarise(total.emissions=sum(Emissions))

library(ggplot2)
ggplot(coal_emissions_by_year, 
       aes(x=year, y=total.emissions)) + 
    geom_line(colour="red") + 
    ggtitle("Coal Emissions in the US during 1999-2008") +
    labs(x="Year", y="Emissions from Coal-Related Sources (in tons)") +
    scale_x_continuous(breaks=seq(1999, 2008, by=3))

ggsave(file="output/plot4.png", width=4.5, height=4.5)
################################################################################
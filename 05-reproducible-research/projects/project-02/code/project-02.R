################################################################################
## Source: Coursera
## Specialization: Data Science
## Course: 5 Reproducible Research
## Week: Week 4
## Project: Project 2
## File: project-02.R
## Date: 2016-02-28
################################################################################


################################################################################
## Questions:
## 1. Across the United States, which types of events (as indicated in the EVTYPE
##  variable) are most harmful with respect to population health?
## 2. Across the United States, which types of events have the greatest
##  economic consequences?
################################################################################

################################################################################
## Section 1: Data Processing
################################################################################

## read from raw data file
data <- read.csv(bzfile("../data/repdata-data-StormData.csv.bz2"))

library(dplyr)

## select variables relevant to answering the research questions
selected_data <- select(data, 
                        EVTYPE, FATALITIES, INJURIES, 
                        PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)

## rename columns using more descriptive names
renamed_data <- selected_data %>%
    rename(event.type = EVTYPE,
           fatalities = FATALITIES,
           injuries = INJURIES,
           property.damage = PROPDMG,
           property.damage.unit = PROPDMGEXP,
           crop.damage = CROPDMG,
           crop.damage.unit = CROPDMGEXP)

## filter data to exclude rows that have no info on health/economic damages
filtered_data <- renamed_data %>%
    filter(!(fatalities == 0 && injuries == 0 &&
                 property.damage == 0 && crop.damage == 0))

## convert factor variables to character variables to compute actual damage values
filtered_data$property.damage.unit <- as.character(filtered_data$property.damage.unit)
filtered_data$crop.damage.unit <- as.character(filtered_data$crop.damage.unit)

## add 2 additional columns to store economic damage values, 
## taking into account the specified unit values 
## (i.e., K/k = 1000, M/m = 1000000, B/b = 1000000000)
mutated_data <- filtered_data %>%
    mutate(property.damage.val = property.damage,
           crop.damage.val = crop.damage)

## compute economic damage values (in $)
mutated_data[mutated_data$property.damage.unit == "H", ]$property.damage.val <- 
    mutated_data[mutated_data$property.damage.unit == "H", ]$property.damage.val * 
    10^2
mutated_data[mutated_data$property.damage.unit == "h", ]$property.damage.val <- 
    mutated_data[mutated_data$property.damage.unit == "h", ]$property.damage.val * 
    10^2
mutated_data[mutated_data$property.damage.unit == "K", ]$property.damage.val <- 
    mutated_data[mutated_data$property.damage.unit == "K", ]$property.damage.val * 
    10^3
mutated_data[mutated_data$property.damage.unit == "k", ]$property.damage.val <- 
    mutated_data[mutated_data$property.damage.unit == "k", ]$property.damage.val * 
    10^3
mutated_data[mutated_data$property.damage.unit == "M", ]$property.damage.val <- 
    mutated_data[mutated_data$property.damage.unit == "M", ]$property.damage.val * 
    10^6
mutated_data[mutated_data$property.damage.unit == "m", ]$property.damage.val <- 
    mutated_data[mutated_data$property.damage.unit == "m", ]$property.damage.val * 
    10^6
mutated_data[mutated_data$property.damage.unit == "B", ]$property.damage.val <- 
    mutated_data[mutated_data$property.damage.unit == "B", ]$property.damage.val * 
    10^9
mutated_data[mutated_data$property.damage.unit == "b", ]$property.damage.val <- 
    mutated_data[mutated_data$property.damage.unit == "b", ]$property.damage.val * 
    10^9

mutated_data[mutated_data$crop.damage.unit == "H", ]$crop.damage.val <- 
    mutated_data[mutated_data$crop.damage.unit == "H", ]$crop.damage.val * 
    10^2
mutated_data[mutated_data$crop.damage.unit == "h", ]$crop.damage.val <- 
    mutated_data[mutated_data$crop.damage.unit == "h", ]$crop.damage.val * 
    10^2
mutated_data[mutated_data$crop.damage.unit == "K", ]$crop.damage.val <- 
    mutated_data[mutated_data$crop.damage.unit == "K", ]$crop.damage.val * 
    10^3
mutated_data[mutated_data$crop.damage.unit == "k", ]$crop.damage.val <- 
    mutated_data[mutated_data$crop.damage.unit == "k", ]$crop.damage.val * 
    10^3
mutated_data[mutated_data$crop.damage.unit == "M", ]$crop.damage.val <- 
    mutated_data[mutated_data$crop.damage.unit == "M", ]$crop.damage.val * 
    10^6
mutated_data[mutated_data$crop.damage.unit == "m", ]$crop.damage.val <- 
    mutated_data[mutated_data$crop.damage.unit == "m", ]$crop.damage.val * 
    10^6
mutated_data[mutated_data$crop.damage.unit == "B", ]$crop.damage.val <- 
    mutated_data[mutated_data$crop.damage.unit == "B", ]$crop.damage.val * 
    10^9
mutated_data[mutated_data$crop.damage.unit == "b", ]$crop.damage.val <- 
    mutated_data[mutated_data$crop.damage.unit == "b", ]$crop.damage.val * 
    10^9

## select variables indicating health/economic damages 
adjusted_data <- mutated_data %>%
    select(event.type, fatalities, injuries, property.damage.val, crop.damage.val)

## group data by event type and summarize total damages
grouped_by_event_type <- adjusted_data %>%
    group_by(event.type) %>%
    summarise(total.fatalities = as.integer(sum(fatalities)),
              total.injuries = as.integer(sum(injuries)),
              total.health.damage = total.fatalities + total.injuries,
              total.property.damage = as.integer(sum(property.damage.val)),
              total.crop.damage = as.integer(sum(crop.damage.val)),
              total.economic.damage = total.property.damage + total.crop.damage)

## sort data in terms of total health damages and total economic damages,
## respectively
sorted_by_health_damages <- grouped_by_event_type %>%
    arrange(desc(total.health.damage)) %>%
    select(event.type, total.fatalities, total.injuries, 
           total.health.damage) 
    
sorted_by_economic_damages <- grouped_by_event_type %>%
    arrange(desc(total.economic.damage)) %>%
    select(event.type, total.property.damage, total.crop.damage,
           total.economic.damage) 

## make event type an ordered factor column for x-axis on plots
sorted_by_health_damages$event.type <- 
    factor(sorted_by_health_damages$event.type, 
           levels = sorted_by_health_damages$event.type)

sorted_by_economic_damages$event.type <- 
    factor(sorted_by_economic_damages$event.type, 
           levels = sorted_by_economic_damages$event.type)

################################################################################
## Section 2: Results 
################################################################################

## display 5 events that are most harmful with respect to population health
head(sorted_by_health_damages, n=5)[, c("event.type", "total.health.damage")]

library(ggplot2)

## create a plot indicating top 5 severe weather events that are most harmful
## with respect to total population health
p1 <- ggplot(head(sorted_by_health_damages, n=5),
             aes(x=event.type, y=total.health.damage)) +
    geom_bar(stat="identity") +
    ggtitle("Top 5 Severe Weather Events for Population Health") +
    labs(x="Event Type", y="Total Fatalities and Injuries (in person)") 

ggsave("../output/plot1.png", width=6, height=4)

## display 5 events that are most harmful with respect to economic consequences
head(sorted_by_economic_damages, n=5)[, c("event.type", "total.economic.damage")]

## create a plot indicating top 5 severe weather events that are most harmful
## with respect to economic consequences
p2 <- ggplot(head(sorted_by_economic_damages, n=5),
             aes(x=event.type, y=total.economic.damage)) +
    geom_bar(stat="identity") +
    ggtitle("Top 5 Severe Weather Events for Economic Damage") +
    labs(x="Event Type", y="Total Crop and Property Damage (in $)") +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggsave("../output/plot2.png", width=6, height=4)

################################################################################
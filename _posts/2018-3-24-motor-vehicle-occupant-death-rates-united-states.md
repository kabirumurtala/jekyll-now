---
layout: post
title: Motor vehicle occupant death rates in the United States
---

Data from the [National Highway Traffic Safety Administration](https://www.nhtsa.gov/), showed that 32,675 people in the 
United States of America died in motor vehicle crashes in 2014. There was a slight (0.7%) decline in motor 
vehicle occupant deaths in 2014, compared to the previous year. The data also revealed the following sobering statistics:
* There were approximately 90 deaths and 6,400 injuries from motor vehicle crashes every day.
* Motor vehicle crashes were the leading cause of death for young adults between the ages of 16 and 24.
* Approximately half of the passenger vehicle occupants that died (16,010) had no seat belts on.
* Alcohol-impaired driving was responsible for approximately 30% (9,967) of all the motor vehicle occupant deaths.
* Distracted driving was responsible for approximately 10% (3,179) of all motor vehicle occupant deaths

Data showed significant differences in the motor vehicle occupant death rate among different age groups and between 
males and females. Also, the motor vehicle occupant death rate varied widely among the 50 states.

## Motor Vehicle Occupant Death Rates, by Age and Gender, 2014
In 2014, people aged 21-34 had the highest motor vehicle occupant death rate with 10.1 deaths per 100,000 population. 
Males (9.2 deaths per 100,000 population) were also more likely to die from motor vehicle crashes, compared to 
females (4.5 deaths per 100,000 population).

```{r, eval=FALSE, echo=FALSE}

#Rate of deaths by age/gender (per 100,000 population) 
#Source: Fatality Analysis Reporting System (FARS) 
#Note: Blank cells indicate data are suppressed. 
#Note: Fatality rates based on fewer than 20 deaths are suppressed.


library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(tidyr)

death <- read.csv("~/DevHealthReport R FIles/Motor Vehicle Occupant Death/MVO.csv", 
                  stringsAsFactors = FALSE)
names(death)

#Remove "Location" which is the same as "State"
death <- death[,-2]
names(death)
death.final <- death
names(death.final) <- c("State", "All.2012", "All.2014", "0-20.2012", "0-20.2014",
                        "21-34.2012", "21-34.2014", "35-54.2012", "35-54.2014",
                        "55+.2012", "55+.2014", "Male.2012", "Male.2014",
                        "Female.2012", "Female.2014")

#Select national data only
usa.death <- death.final[death.final$State == "United States",]
usa.death.2012 <- usa.death[,c(1,2,4,6,8,10,12,14)]
usa.death.2014 <- usa.death[,c(1,3,5,7,9,11,13,15)]


#Select 2014 national data by age
usa.age <- usa.death.2014[,2:6]
usa.age
names(usa.age) <- c("All", "0-20","21-34","35-54","55+")
usa.age
usa.age <- gather(usa.age, AgeGroup, Value)
usa.age$AgeGroup <- factor(usa.age$AgeGroup, levels = c("All", "0-20", "21-34",
                            "35-54", "55+"))
usa.age


#Select 2014 national data by gender
usa.gender <- usa.death.2014[,7:8]
usa.gender
names(usa.gender) <- c("Male","Female")
usa.gender
usa.gender <- gather(usa.gender, Gender, Value)
usa.gender$Gender <- factor(usa.gender$Gender, levels = c("Male","Female"))
usa.gender



##Plot graph by age
plotAge <- ggplot(usa.age, aes(AgeGroup,Value)) + 
           geom_bar(stat="identity", 
                fill=c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00')) +
           geom_text(label=usa.age$Value, fontface="bold", col="black", size=5, vjust=1.3) +
           scale_y_continuous("Rate per 100,000 population \n", breaks = seq(0,12,1)) + xlab("") +
           ggtitle("Motor Vehicle Occupant Death Rates, by Age, 2014")


plotAge <- plotAge + theme_stata() + theme(
    plot.title=element_text(size=16,margin=margin(0,0,10,0)),
    strip.text.x=element_text(size=16,margin=margin(4,0,4,0), colour="darkred"),    
    legend.title=element_blank(),
    legend.text=element_text(size=14),
    axis.title=element_text(size=16),
    axis.text.x=element_text(size=14),
    axis.text.y=element_text(size=14,angle=0,hjust=0),
    legend.margin=unit(1,"lines"),
    panel.margin.x=unit(0,"lines"),
    panel.margin.y=unit(1.2,"lines"),
    panel.margin=unit(0,"lines"))


##Plot graph by gender
plotGender <- ggplot(usa.gender, aes(Gender,Value)) + 
    geom_bar(stat="identity", width = 0.5,
             fill=c("#66c2a5","#fbb4ae")) +
    geom_text(label=usa.gender$Value, fontface="bold", col="black", size=5, vjust=1.3) +
    scale_y_continuous("Rate per 100,000 population \n", breaks = seq(0,12,1)) + xlab("") +
    ggtitle("Motor Vehicle Occupant Death Rates, by Gender, 2014")

plotGender <- plotGender + theme_stata() + theme(
    plot.title=element_text(size=16,margin=margin(0,0,10,0)),
    strip.text.x=element_text(size=16,margin=margin(4,0,4,0), colour="darkred"),    
    legend.title=element_blank(),
    legend.text=element_text(size=14),
    axis.title=element_text(size=16),
    axis.text.x=element_text(size=14),
    axis.text.y=element_text(size=14,angle=0,hjust=0),
    legend.margin=unit(1,"lines"),
    panel.margin.x=unit(0,"lines"),
    panel.margin.y=unit(1.2,"lines"),
    panel.margin=unit(0,"lines"))

```

```{r, echo=FALSE}
grid.arrange(plotAge, plotGender,ncol=2)
```

## Motor Vehicle Occupant Death Rates, by States, 2014
In 2014, the fatality rate per 100,000 people ranged from 2.3 in Rhode Island to 21.9 in Wyoming. Rhode Island, Hawaii, New York, Massachusetts, New Jersey, Connecticut, California, Maryland, Vermont, New Hampshire, and Illinois all had fatality rates less than 5.1 per 100,000 people. Mississippi and Wyoming occupied the opposite end of the spectrum with fatality rates above 15.0 per 100,000 people.

## Data Source
Data was obtained from Fatality Analysis Reporting System (FARS), National Highway Traffic Safety Administration. More information on the data collection methods and procedures can be found [here](https://www.nhtsa.gov/research-data).

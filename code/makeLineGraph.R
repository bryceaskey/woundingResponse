# Install and load necessary packages
packageList <- c("ggplot2", "dplyr", "cowplot", "tidyr")
newPackages <- packageList[!(packageList %in% installed.packages()[,"Package"])]
if(length(newPackages) > 0){
  install.packages(newPackages)
}
library(ggplot2)
library(dplyr)
library(cowplot)
library(tidyr)

# Set main path
mainPath <- "C:/Users/Bryce/Documents/woundingResponse"
if(getwd() != mainPath){
  setwd(mainPath)
}

# Function to make line graph of day 1-5 rib averaged pinking data for a given variety and
# treatment method. For data in aStar format.
makeLineGraph_aStar <- function(rawData, variety, treatment, lowerLim=-15, upperLim=15){
  varData <- rawData[rawData$variety==variety & rawData$treatment==treatment, ]
  varData_RA <- varData %>%
    dplyr::group_by(day, rib) %>%
    dplyr::summarise(aStarMean_RA=mean(aStarMean))
  
  lineGraph <- ggplot(data=varData_RA, mapping=aes(x=day, y=aStarMean_RA, group=rib)) +
    geom_line(aes(color=rib), size=1) +
    geom_point(aes(shape=rib, color=rib), size=3) +
    ylim(lowerLim, upperLim) +
    labs(title=paste("Rib averaged a* values for", treatment, variety, "ribs")
    )
  return(lineGraph)
}

# Function to make line graph of day 1-5 rib averaged pinking data for a given variety and
# treatment method. For data in hue format.
makeLineGraph_hue <- function(rawData, variety, treatment, lowerLim=0, upperLim=360){
  varData <- rawData[rawData$variety==variety & rawData$treatment==treatment, ]
  varData_RA <- varData %>%
    dplyr::group_by(day, rib) %>%
    dplyr::summarise(hueMean_RA=mean(hueMean))
  
  lineGraph <- ggplot(data=varData_RA, mapping=aes(x=day, y=hueMean_RA, group=rib)) +
    geom_line(aes(color=rib), size=1) +
    geom_point(aes(shape=rib, color=rib), size=3) +
    ylim(lowerLim, upperLim) +
    labs(title=paste("Rib averaged hue values for", treatment, variety, "ribs")
    )
  return(lineGraph)
}
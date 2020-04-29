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

# Load mean a* data
rawData <- read.csv("data/v3/rawData.csv")
rawData <- rawData[ , 2:ncol(rawData)]

# Function to make line graph of day 1-5 rib averaged pinking data for a given variety and treatment method
makeLineGraph <- function(rawData, variety, treatment, lowerLim=-5, upperLim=10){
  varData <- rawData[rawData$variety==variety & rawData$treatment==treatment, ]
  varData_RA <- varData %>%
    dplyr::group_by(day, rib) %>%
    dplyr::summarise(aStarMean_RA=mean(aStarMean))
  
  if(treatment=="control"){
    bgColor <- "#EFC9BD"
  }else if(treatment=="cys500"){
    bgColor <- "#ECEFBD"
  }else{
    bgColor <- "#BDEFBF"
  }
  
  lineGraph <- ggplot(data=varData_RA, mapping=aes(x=day, y=aStarMean_RA, group=rib)) +
    geom_line(aes(color=rib), size=1) +
    geom_point(aes(shape=rib, color=rib), size=3) +
    ylim(lowerLim, upperLim) +
    labs(title=paste("Rib averaged a* values for", treatment, variety, "ribs")
    )
  return(lineGraph)
}

library(tidyr)
library(dplyr)
library(ggpubr)

# Set main path
mainPath <- "C:/Users/Bryce/Documents/woundingResponse"
if(getwd() != mainPath){
  setwd(mainPath)
}

# Load aStar data from .csv file
allData <- read.csv("data/v3/aStar_rawData.csv")[, 2:7]
allData[colnames(allData)[1:5]] <- lapply(allData[colnames(allData)[1:5]], factor)

subsetData <- function(allData, var, aStarMin, aStarMax){
  # Subset allData by filtering out samples with initial aStarMean outside of specified bounds
  subsetData <- allData %>%
    filter(variety==var) %>%
    pivot_wider(names_from=day, values_from=aStarMean, names_prefix="aStarMean_day") %>%
    filter(aStarMean_day1>aStarMin & aStarMean_day1<aStarMax)
  
  # Recursively filter samples until all treatment groups have the same number of samples
  # Samples are filtered in order of distance from mean of treatment group with the fewest samples
  samplesPerGroup <- subsetData %>%
    group_by(treatment) %>%
    tally()
  limitingGroup <- as.character(samplesPerGroup$treatment[samplesPerGroup$n==min(samplesPerGroup$n)])[1]
  limitingGroupMean <- mean(subsetData$aStarMean_day1[subsetData$treatment==limitingGroup])
  
  subsetData <- subsetData %>%
    mutate(meanDiff=abs(aStarMean_day1-limitingGroupMean))
  
  for(treatmentGroup in samplesPerGroup$treatment){
    while(sum(subsetData$treatment==treatmentGroup) > sum(subsetData$treatment==limitingGroup)){
      maxDiff <- max(subsetData[subsetData$treatment==treatmentGroup, ]$meanDiff)
      subsetData <- subsetData[!(subsetData$treatment==treatmentGroup & subsetData$meanDiff==maxDiff), ]
    }
  }
  
  subsetData <- subsetData[ , 1:ncol(subsetData)-1]
  subsetData <- subsetData %>%
    pivot_longer(cols=c("aStarMean_day1", "aStarMean_day3", "aStarMean_day5"), names_to="day",
                 names_prefix="aStarMean_day", values_to="aStarMean")
  
  return(subsetData)
}
  
prepareData <- function(subsetData) {
  # Define function to calculate standard error
  stError <- function(data){
    return(sd(data)/sqrt(length(data)))
  }
  
  # Format data for graphing with ggplot
  graphData <- subsetData %>%
    group_by(variety, treatment, day) %>%
    summarise(aStarMean_TA=mean(aStarMean), SE=stError(aStarMean))
  
  graphData$treatment <- as.character(graphData$treatment)
  graphData$treatment[graphData$treatment=="control"] <- "No treatment"
  graphData$treatment[graphData$treatment=="cys500"] <- "L-cysteine 500 ppm"
  graphData$treatment[graphData$treatment=="mel1000"] <- "Melatonin 1000 ppm"
  graphData$treatment <- factor(graphData$treatment, levels=c("No treatment", "L-cysteine 500 ppm", "Melatonin 1000 ppm"))
  
  graphData$day <- as.numeric(graphData$day)
  graphData$day <- graphData$day - 1
  graphData$day <- factor(graphData$day)
  
  # Calculate significance values @ each time point via unpaired t-test
  # Compare each experimental group to control
  significance <- compare_means(aStarMean~treatment, data=subsetData, method="t.test", paired=FALSE, group.by="day", ref.group="control")
  significance <- setorder(significance, group2)
  print(significance)
  pValues <- c(NA, NA, NA, significance$p)
  pSymbols <- c(NA, NA, NA, significance$p.signif)

  graphData <- setorder(graphData, treatment)
  print(graphData)
  graphData$p <- pValues
  graphData$pSym <- pSymbols
  graphData$pSym[graphData$pSym=="ns"] <- NA

  return(graphData)
}
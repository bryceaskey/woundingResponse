# !diagnostics off

packageList <- c("tidyr", "dplyr", "ggplot2")
newPackages <- packageList[!(packageList %in% installed.packages()[,"Package"])]
if(length(newPackages) > 0){
  install.packages(newPackages)
}

library(tidyr)
library(dplyr)
library(ggplot2)

# Set main path
mainPath <- "C:/Users/Bryce/Documents/woundingResponse"
if(getwd() != mainPath){
  setwd(mainPath)
}

# Load hue data from .csv file
allData <- read.csv("data/v3/hue_rawData.csv")[, 2:7]
allData[colnames(allData)[1:5]] <- lapply(allData[colnames(allData)[1:5]], factor)

# Specify variety and bounds for subsetting
var <- "romaine"
hueMin <- 50
hueMax <- 55

# Subset allData by filtering out samples with initial hueMean outside of specified bounds
subsetData <- allData %>%
  filter(variety==var) %>%
  pivot_wider(names_from=day, values_from=hueMean, names_prefix="hueMean_day") %>%
  filter(hueMean_day1>hueMin & hueMean_day1<hueMax)

# Print summary of samples remaining after subsetting
print(paste("# of control samples remaining:", sum(subsetData$treatment=="control")))
print(paste("# of cys500 samples remaining:", sum(subsetData$treatment=="cys500")))
print(paste("# of mel1000 samples reminaing:", sum(subsetData$treatment=="mel1000")))

# Recursively filter samples until all treatment groups have the same number of samples
# Samples are filtered in order of distance from mean of treatment group with the fewest samples
samplesPerGroup <- subsetData %>%
  group_by(treatment) %>%
  tally()
limitingGroup <- as.character(samplesPerGroup$treatment[samplesPerGroup$n==min(samplesPerGroup$n)])[1]
limitingGroupMean <- mean(subsetData$hueMean_day1[subsetData$treatment==limitingGroup])

subsetData <- subsetData %>%
  mutate(meanDiff=abs(hueMean_day1-limitingGroupMean))

for(treatmentGroup in samplesPerGroup$treatment){
  while(sum(subsetData$treatment==treatmentGroup) > sum(subsetData$treatment==limitingGroup)){
    maxDiff <- max(subsetData[subsetData$treatment==treatmentGroup, ]$meanDiff)
    subsetData <- subsetData[!(subsetData$treatment==treatmentGroup & subsetData$meanDiff==maxDiff), ]
  }
}

subsetData <- subsetData[ , 1:ncol(subsetData)-1]
subsetData <- subsetData %>%
  pivot_longer(cols=c("hueMean_day1", "hueMean_day3", "hueMean_day5"), names_to="day",
               names_prefix="hueMean_day", values_to="hueMean")

# Define function to calculate standard error
stError <- function(data){
  return(sd(data)/sqrt(length(data)))
}

# Make line graph of output data
graphData <- subsetData %>%
  group_by(variety, treatment, day) %>%
  summarise(hueMean_TA=mean(hueMean), SE=stError(hueMean))

lineGraph <- ggplot(data=graphData, mapping=aes(x=day, y=hueMean_TA, group=treatment)) +
  geom_line(mapping=aes(color=treatment), size=1) +
  geom_point(mapping=aes(shape=treatment, color=treatment), size=2.5) +
  geom_errorbar(mapping=aes(ymin=hueMean_TA-SE, ymax=hueMean_TA+SE, color=treatment), width=0.05, size=0.75) +
  labs(title=paste("Treatment-averaged a* values for", var, "ribs with initial a* at cut sites between", hueMin, "and", hueMax)) +
  theme(plot.title.position="plot")
print(lineGraph)
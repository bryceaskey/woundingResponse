# !diagnostics off

library(tidyr)
library(dplyr)
library(ggplot2)

# Set main path
mainPath <- "C:/Users/Bryce/Documents/woundingResponse"
if(getwd() != mainPath){
  setwd(mainPath)
}

# Load aStar data from .csv file
allData <- read.csv("data/v3/aStar_rawData.csv")[, 2:7]
allData[colnames(allData)[1:5]] <- lapply(allData[colnames(allData)[1:5]], factor)

# Specify variety and bounds for subsetting
var <- "romaine"
aStarMin <- -6
aStarMax <- -3

# Subset allData by filtering out samples with initial aStarMean outside of specified bounds
subsetData <- allData %>%
  filter(variety==var) %>%
  pivot_wider(names_from=day, values_from=aStarMean, names_prefix="aStarMean_day") %>%
  filter(aStarMean_day1>aStarMin & aStarMean_day1<aStarMax)

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

# Define function to calculate standard error
stError <- function(data){
  return(sd(data)/sqrt(length(data)))
}

# Make line graph of output data
graphData <- subsetData %>%
  group_by(variety, treatment, day) %>%
  summarise(aStarMean_TA=mean(aStarMean), SE=stError(aStarMean))

lineGraph <- ggplot(data=graphData, mapping=aes(x=day, y=aStarMean_TA, group=treatment)) +
  geom_line(mapping=aes(color=treatment), size=1) +
  geom_point(mapping=aes(shape=treatment, color=treatment), size=2.5) +
  geom_errorbar(mapping=aes(ymin=aStarMean_TA-SE, ymax=aStarMean_TA+SE, color=treatment), width=0.05, size=0.75) +
  labs(title=paste("Treatment-averaged a* values for", var, "ribs with initial a* at cut sites between", aStarMin, "and", aStarMax)) +
  theme(plot.title.position="plot")
print(lineGraph)
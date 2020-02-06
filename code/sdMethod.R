# Install and load necessary packages
packageList <- c("OpenImageR", "ggplot2", "dplyr", "cowplot", "tidyr")
newPackages <- packageList[!(packageList %in% installed.packages()[,"Package"])]
if(length(newPackages) > 0){
  install.packages(newPackages)
}
library(OpenImageR)
library(ggplot2)
library(dplyr)
library(cowplot)
library(tidyr)

# Set main path
mainPath <- "C:/Users/Bryce/Documents/woundingResponse"
if(getwd() != mainPath){
  setwd(mainPath)
}

# Set path to processed images
imagePath <- paste(mainPath, "/data/woundingSites/v2", sep="")

# Load necessary functions
source("code/pctPink.R")

pinkData <- read.csv(paste(mainPath, "/data/v2/pinkData_v2.csv", sep=""))
colnames(pinkData)[1] <- "variety"

# Initialize empty lists to store image data
varietyList <- vector(mode="character", length=length(dir(imagePath)))
treatmentList <- vector(mode="character", length=length(dir(imagePath)))
dayList <- vector(mode="character", length=length(dir(imagePath)))
ribList <- vector(mode="character", length=length(dir(imagePath)))
cutSiteList <- vector(mode="character", length=length(dir(imagePath)))
pctPinkList <- vector(mode="character", length=length(dir(imagePath)))

# Loop through all images in specified path and calculate a* mean value for each
for(i in 1:length(dir(imagePath))){
  imageName <- dir(imagePath)[i]
  print(paste("Now processing image", imageName))
  # Parse image names, calculate mean aStar, and add to lists of data
  varietyList[i] <- strsplit(imageName, "_")[[1]][1]
  treatmentList[i] <- strsplit(imageName, "_")[[1]][2]
  dayList[i] <- strsplit(imageName, "_")[[1]][3]
  ribList[i] <- strsplit(strsplit(strsplit(imageName, "_")[[1]][4], ".", fixed=TRUE)[[1]][1], "-")[[1]][1]
  cutSiteList[i] <- strsplit(strsplit(strsplit(imageName, "_")[[1]][4], ".", fixed=TRUE)[[1]][1], "-")[[1]][2]
  pctPinkList[i] <- pctPink(paste(imagePath, "/", imageName, sep=""), pinkData)
}

# Save data into a dataframe
rawData <- data.frame(variety=varietyList, treatment=treatmentList, day=dayList, rib=ribList, cutSite=cutSiteList, percentPink=pctPinkList)

diffData <- spread(rawData, day, percentPink)
colnames(diffData)[5:7] <- c("day1", "day3", "day5")
diffData$day1 <- as.numeric(as.character(diffData$day1))
diffData$day3 <- as.numeric(as.character(diffData$day3))
diffData$day5 <- as.numeric(as.character(diffData$day5))
diffData <- mutate(diffData, day1_3Diff=day3-day1, day1_5Diff=day5-day1)

meanDiffData <- diffData %>%
  dplyr::group_by(variety, treatment) %>%
  dplyr::summarise(mean1_3=mean(day1_3Diff), mean1_5=mean(day1_5Diff), stError1_3=sd(day1_3Diff)/sqrt(length(day1_3Diff)), stError1_5=sd(day1_5Diff)/sqrt(length(day1_3Diff)))

day1_3Plot <- ggplot(data=meanDiffData, mapping=aes(x=variety, y=mean1_3, fill=treatment)) +
  geom_col(position="dodge") +
  geom_errorbar(aes(ymin=mean1_3-stError1_3, ymax=mean1_3+stError1_3), width=0.4, position=position_dodge(.9)) +
  labs(title="Change in pctPink from day 1 to day 3", x="Variety", y="Mean difference in average pctPink of wounding site") +
  ylim(-2, 50) +
  theme(plot.title=element_text(size=18),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text=element_text(size=12))
print(day1_3Plot)

day1_5Plot <- ggplot(data=meanDiffData, mapping=aes(x=variety, y=mean1_5, fill=treatment)) +
  geom_col(position="dodge") +
  geom_errorbar(aes(ymin=mean1_5-stError1_5, ymax=mean1_5+stError1_5), width=0.4, position=position_dodge(.9)) +
  labs(title="Change in pctPink from day 1 to day 5", x="Variety", y="Mean difference in average pctPink of wounding site") +
  ylim(-2, 50) +
  theme(plot.title=element_text(size=18),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text=element_text(size=12))
print(day1_5Plot)
# Install and load necessary packages
packageList <- c("OpenImageR", "ggplot2", "dplyr", "cowplot")
newPackages <- packageList[!(packageList %in% installed.packages()[,"Package"])]
if(length(newPackages) > 0){
  install.packages(newPackages)
}
library(OpenImageR)
library(ggplot2)
library(dplyr)
library(cowplot)

# Set main path
mainPath <- "C:/Users/Bryce/Documents/woundingResponse"
if(getwd() != mainPath){
  setwd(mainPath)
}

# Set path to processed images
imagePath <- paste(mainPath, "/data/woundingSites", sep="")

# Load necessary functions
source("code/pctPink.R")

pinkData <- read.csv(paste(mainPath, "/data/pinkData.csv", sep=""))
colnames(pinkData)[1] <- "variety"


# Install and load necessary packages
packageList <- c("OpenImageR", "ggplot2")
newPackages <- packageList[!(packageList %in% installed.packages()[,"Package"])]
if(length(newPackages) > 0){
  install.packages(newPackages)
}
library(OpenImageR)
library(ggplot2)

# Set main path
mainPath <- "C:/Users/bca08_000/Documents/woundingResponse"
if(getwd() != mainPath){
  setwd(mainPath)
}

# Set path to processed images
imagePath <- "C:/Users/bca08_000/Documents/woundingResponse/data/processedImages"

# Load necessary functions
source("code/aStarMean.R")

# Loop through all images in specified path, calculate a* mean value for each, and save as a dataframe


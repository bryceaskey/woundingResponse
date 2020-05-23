library(tidyr)
library(dplyr)
library(ggplot2)
library(ggsci)
library(ggpubr)
library(cowplot)

# Set main path
mainPath <- "C:/Users/Bryce/Documents/woundingResponse"
if(getwd() != mainPath){
  setwd(mainPath)
}

# Load aStar data from .csv file
allData <- read.csv("data/v3/aStar_rawData.csv")[, 2:7]
allData[colnames(allData)[1:5]] <- lapply(allData[colnames(allData)[1:5]], factor)

source("code/aStarSubset.R")

icebergSubset <- subsetData(allData, "iceberg", -3, 0)
romaineSubset <- subsetData(allData, "romaine", -6, -3)

icebergSignificance <- compare_means(aStarMean~treatment, data=icebergSubset, method="t.test", paired=FALSE, group.by="day", ref.group="control")
romaineSignificance <- compare_means(aStarMean~treatment, data=romaineSubset, method="t.test", paired=FALSE, group.by="day", ref.group="control")
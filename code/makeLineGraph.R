library(tidyr)
library(dplyr)
library(ggplot2)
library(ggsci)
library(cowplot)
library(ggpubr)

# Set main path
mainPath <- "C:/Users/Bryce/Documents/woundingResponse"
if(getwd() != mainPath){
  setwd(mainPath)
}

# Load aStar data from .csv file
allData <- read.csv("data/v3/aStar_rawData.csv")[, 2:7]
allData[colnames(allData)[1:5]] <- lapply(allData[colnames(allData)[1:5]], factor)

# Load functions to subset and prepare data for graphing
source("code/aStarSubset.R")

icebergData <- prepareData(subsetData(allData, "iceberg", -3, 0))
romaineData <- prepareData(subsetData(allData, "romaine", -6, -3))

# Make line graph of output data
legendGraph <- ggplot(data=icebergData, mapping=aes(x=day, y=aStarMean_TA, group=treatment)) +
  geom_line(mapping=aes(color=treatment), size=1) +
  geom_errorbar(mapping=aes(ymin=aStarMean_TA-SE, ymax=aStarMean_TA+SE, color=treatment), width=0.15, size=0.5) +
  geom_point(mapping=aes(shape=treatment, color=treatment), size=3) +
  labs(color="Treatment:", shape="Treatment:") +
  scale_color_npg() +
  theme_bw() +
  theme(legend.direction="horizontal", legend.spacing.x=unit(15, "pt"), legend.text=element_text(size=11, margin=margin(r=50, unit="pt")),
        legend.title=element_text(size=12, face="bold"))
legend <- get_legend(legendGraph)

icebergGraph <- ggplot(data=icebergData, mapping=aes(x=day, y=aStarMean_TA, group=treatment)) +
  geom_line(mapping=aes(color=treatment), size=1) +
  geom_errorbar(mapping=aes(ymin=aStarMean_TA-SE, ymax=aStarMean_TA+SE, color=treatment), width=0.15, size=0.5) +
  geom_point(mapping=aes(shape=treatment, color=treatment), size=3) +
  labs(x="Days after wounding", y="Mean a* of cut site", color="Treatment", shape="Treatment", tag="A") +
  ylim(-6, 9) +
  scale_color_npg() +
  theme_bw() +
  theme(legend.position="none", 
        axis.title=element_text(size=12, face="bold"), 
        axis.text=element_text(size=11),
        plot.tag=element_text(face="bold"))

romaineGraph <- ggplot(data=romaineData, mapping=aes(x=day, y=aStarMean_TA, group=treatment)) +
  geom_line(mapping=aes(color=treatment), size=1) +
  geom_errorbar(mapping=aes(ymin=aStarMean_TA-SE, ymax=aStarMean_TA+SE, color=treatment), width=0.15, size=0.5) +
  geom_point(mapping=aes(shape=treatment, color=treatment), size=3) +
  labs(x="Days after wounding", y="Mean a* of cut site", color="Treatment", shape="Treatment", tag="B") +
  ylim(-6, 9) +
  scale_color_npg() +
  theme_bw() +
  theme(legend.position="none",
        axis.title=element_text(size=12, face="bold"), 
        axis.text=element_text(size=11),
        plot.tag=element_text(face="bold"))

bothGraphs <- plot_grid(icebergGraph, romaineGraph, nrow=1)
finalFigure <- plot_grid(bothGraphs, legend, nrow=2, rel_heights=c(1, 0.1))
print(finalFigure)
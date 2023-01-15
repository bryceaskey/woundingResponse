# Generates line graphs in Fig 1 C & F showing change in mean a*. 
# Load necessary packages.
library(tidyverse)
library(data.table)
library(cowplot)
library(ggpubr)

# Load helper functions.
# subsetData - subsets a* data to include only cut sites with comparable initial color.
source("C:/Users/Bryce/Research/woundingResponse/code/helpers/subsetData.R")
# summarizeData - summarizes data for plotting by calculating mean and SE, and performing significance testing. 
source("C:/Users/Bryce/Research/woundingResponse/code/helpers/summarizeData.R")

# Load aStar data from .csv file and set column data types.
allData <- read.csv("C:/Users/Bryce/Research/woundingResponse/data/aStarData.csv")
allData[colnames(allData)[1:5]] <- lapply(allData[colnames(allData)[1:5]], factor)

# Subset and prepare iceberg data.
icebergData <- subsetData(allData, "iceberg", -3, 0)
icebergData <- summarizeData(icebergData)

# Subset and prepare romaine data.
romaineData <- subsetData(allData, "romaine", -6, -3)
romaineData <- summarizeData(romaineData)

# Make line graphs of output data.
# Graph to extract legend from.
legendGraph <- ggplot(data=icebergData, mapping=aes(x=day, y=aStarMeanChange, group=treatment)) +
  geom_line(mapping=aes(color=treatment), size=1) +
  geom_point(mapping=aes(shape=treatment, color=treatment), size=3) +
  scale_shape_manual(values=c(15, 17, 18)) +
  scale_color_manual(values=c("#c00000", "#00b251", "#0073c6")) +
  theme_bw() +
  theme(legend.direction="horizontal",
        legend.text=element_text(size=10),
        legend.title=element_blank(),
        plot.margin=margin(t=1000, unit="pt"))
legend <- get_legend(legendGraph)

# Graph iceberg data.
icebergGraph <- ggplot(data=icebergData, mapping=aes(x=day, y=aStarMeanChange, group=treatment)) +
  geom_line(mapping=aes(color=treatment), size=1) +
  geom_errorbar(mapping=aes(ymin=aStarMeanChange-SE, ymax=aStarMeanChange+SE), width=0.08, size=0.4) +
  geom_point(mapping=aes(shape=treatment, color=treatment), size=3) +
  labs(x="Storage time (d)", y="a* change", tag="C") +
  ylim(0, 15) +
  scale_shape_manual(values=c(15, 17, 18)) +
  scale_color_manual(values=c("#c00000", "#00b251", "#0073c6")) +
  theme_bw() +
  theme(legend.position="none", 
        axis.title=element_text(size=10), 
        axis.text=element_text(size=10, color="black"),
        axis.ticks=element_blank(),
        panel.grid.major.x=element_blank(),
        plot.tag=element_text(size=16, face="bold"))

# Graph romaine data.
romaineGraph <- ggplot(data=romaineData, mapping=aes(x=day, y=aStarMeanChange, group=treatment)) +
  geom_line(mapping=aes(color=treatment), size=1) +
  geom_errorbar(mapping=aes(ymin=aStarMeanChange-SE, ymax=aStarMeanChange+SE), width=0.08, size=0.4) +
  geom_point(mapping=aes(shape=treatment, color=treatment), size=3) +
  labs(x="Storage time (d)", y="a* change", tag="F") +
  ylim(0, 15) +
  scale_shape_manual(values=c(15, 17, 18)) +
  scale_color_manual(values=c("#c00000", "#00b251", "#0073c6")) +
  theme_bw() +
  theme(legend.position="none",
        axis.title=element_text(size=10), 
        axis.text=element_text(size=10, color="black"),
        axis.ticks=element_blank(),
        panel.grid.major.x=element_blank(),
        plot.tag=element_text(size=16, face="bold"))

# Combine graphs and legend into final figure.
bothGraphs <- plot_grid(icebergGraph, romaineGraph, nrow=2, ncol=1)
finalFigure <- plot_grid(bothGraphs, legend, nrow=2, rel_heights=c(2, 0.3))

ggsave("C:/Users/Bryce/Research/woundingResponse/figures/fig1CF.tif", plot=finalFigure,
       device="tiff", width=8, height=15, units="cm", dpi=300)
dev.off()

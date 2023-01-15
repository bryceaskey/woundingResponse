# Generates scatterplot in Fig S1 comparing a* and visual rating results.
# Load necessary packages.
library(tidyverse)
library(ggpmisc)

# Load helper functions.
# subsetData - subsets a* data to include only cut sites with comparable initial color.
source("C:/Users/Bryce/Research/woundingResponse/code/helpers/subsetData.R")
# summarizeData - summarizes data for plotting by calculating mean and SE, and performing significance testing. 
source("C:/Users/Bryce/Research/woundingResponse/code/helpers/summarizeData.R")

# Load aStar data.
allData <- read.csv("C:/Users/Bryce/Research/woundingResponse/data/aStarData.csv")
allData[colnames(allData)[1:5]] <- lapply(allData[colnames(allData)[1:5]], factor)

# Subset and prepare iceberg data.
icebergData <- subsetData(allData, "iceberg", -3, 0)
icebergData <- summarizeData(icebergData)

# Subset and prepare romaine data.
romaineData <- subsetData(allData, "romaine", -6, -3)
romaineData <- summarizeData(romaineData)

# Combine iceberg and romaine data frames, and drop unused columns.
allData <- rbind(icebergData, romaineData)
allData <- allData[, 1:5]
colnames(allData)[4:5] <- c("aStar", "aStar_SE")

# Rename treatment levels to match those in ratingData.
allData$treatment <- as.character(allData$treatment)
allData$treatment <- gsub(" ", "", tolower(allData$treatment))
allData$treatment <- factor(allData$treatment)

# Load rating data.
ratingData <- read.csv("C:/Users/Bryce/Research/woundingResponse/data/ratingData.csv")
ratingData[colnames(ratingData)[1:3]] <- lapply(ratingData[colnames(ratingData)[1:3]], factor)
colnames(ratingData)[4:5] <- c("rating", "rating_SE")

# Merge aStar and rating data into a single data frame.
mergedData <- merge(allData, ratingData, by=c("variety", "treatment", "day"))

# Generate scatterplot.
scatter <- ggplot(data=mergedData, mapping=aes(x=rating, y=aStar)) +
  geom_point(size=2) +
  stat_poly_line(method="lm", se=FALSE, color="black") +
  stat_poly_eq(use_label(c("R2", "P")), size=4.5, p.digits=5) +
  labs(x="Pink rib rating (1 - 5 scale)", y="a* value") +
  theme_bw() +
  theme(axis.text=element_text(color="black", size=12),
        axis.title=element_text(color="black", size=12))

ggsave("C:/Users/Bryce/Research/woundingResponse/figures/figS1.tif", plot=scatter,
       device="tiff", width=10, height=10, units="cm", dpi=300)

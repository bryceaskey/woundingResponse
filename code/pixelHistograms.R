library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)

pixelData <- read.csv("C:/Users/Bryce/Documents/woundingResponse/pixelValues.csv")
colnames(pixelData)[1] <- "treatment"
pixelData <- pixelData %>% gather(component, value, R:B)
pixelData$component <- factor(pixelData$component, levels=c("R", "G", "B"))
pixelMeans <- ddply(pixelData, c("treatment", "component"), summarise, meanValue=mean(value), stDeviation=sd(value))
pixelMeans <- transform(pixelMeans, lower=meanValue-stDeviation, upper=meanValue+stDeviation)

control <- ggplot(data=filter(pixelData, treatment=="control"), mapping=aes(x=value)) +
  geom_histogram(aes(y=..density.., fill=component), alpha=0.6, binwidth=2) +
  geom_density(mapping=aes(x=value, fill=component, color=component), alpha=0.8) +
  geom_vline(data=filter(pixelMeans, treatment=="control"), mapping=aes(xintercept=meanValue, color=component), linetype="dashed", size=1.2) +
  scale_fill_manual(values=c("#F8766D", "#00BA38", "#619CFF")) +
  scale_x_continuous(name="Pixel Value", limits=c(0, 255)) +
  scale_y_continuous(name="Relative frequency", limits=c(0, 0.06)) +
  theme(legend.position="none")


mel_1000ppm <- ggplot(data=filter(pixelData, treatment=="mel_1000ppm"), mapping=aes(x=value)) +
  geom_histogram(aes(y=..density.., fill=component), alpha=0.6, binwidth=2) +
  geom_density(mapping=aes(x=value, fill=component, color=component), alpha=0.8) +
  geom_vline(data=filter(pixelMeans, treatment=="mel_1000ppm"), mapping=aes(xintercept=meanValue, color=component), linetype="dashed", size=1.2) +
  scale_fill_manual(values=c("#F8766D", "#00BA38", "#619CFF")) +
  scale_x_continuous(name="Pixel Value", limits=c(0, 255)) +
  scale_y_continuous(name="Relative frequency", limits=c(0, 0.06)) +
  theme(legend.position="none")


print(plot_grid(control, mel_1000ppm, nrow=2, ncol=1))

controlThresholds <- ggplot(data=filter(pixelData, treatment=="control"), mapping=aes(x=value)) +
  geom_histogram(aes(y=..density.., fill=component), alpha=0.2, binwidth=2) +
  geom_density(mapping=aes(x=value, fill=component, color=component), alpha=0.3) +
  geom_vline(data=filter(pixelMeans, treatment=="control"), mapping=aes(xintercept=meanValue, color=component), size=1.2) +
  geom_vline(data=filter(pixelMeans, treatment=="control"), mapping=aes(xintercept=lower, color=component), linetype="dashed", size=1) +
  geom_vline(data=filter(pixelMeans, treatment=="control"), mapping=aes(xintercept=upper, color=component), linetype="dashed", size=1) +
  scale_fill_manual(values=c("#F8766D", "#00BA38", "#619CFF")) +
  scale_x_continuous(name="Pixel Value", limits=c(0, 255)) +
  scale_y_continuous(name="Relative frequency", limits=c(0, 0.06)) +
  theme(legend.position="none")

sampleData <- read.csv("C:/Users/Bryce/Documents/woundingResponse/sampleData.csv")
colnames(sampleData)[1] <- "day"
sampleData <- sampleData %>% gather(treatment, percentage, control:serotonin_1000ppm)

pinkingProgress <- ggplot(data=sampleData, mapping=aes(x=day, y=percentage, color=treatment)) +
  geom_line(size=1.5) +
  geom_point(mapping=aes(shape=treatment), size=4, show.legend=FALSE) +
  scale_x_discrete(name="Days since wounding") +
  scale_y_continuous(name="Percentage of pink pixels") +
  scale_color_manual(name="Treatment", values=c("#003300", "#cc9900", "#009999"))

print(pinkingProgress)
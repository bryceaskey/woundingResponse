# Define function to summarize data for plotting by calculating mean and SE, and performing significance testing. 
summarizeData <- function(rawData) {
  # Summarize data into mean and SE.
  sumData <- rawData %>%
    group_by(variety, treatment, day) %>%
    summarise(aStarMean=mean(aStar), SE=sd(aStar)/sqrt(length(aStar)))
  
  # Add column with change in aStarMean for each treatment relative to day 1. 
  sumData$aStarMeanChange <- NA
  for(treatment in levels(sumData$treatment)){
    aStarMean_day1 <- sumData$aStarMean[sumData$day==1 & sumData$treatment==treatment]
    sumData$aStarMeanChange[sumData$treatment==treatment] <- sumData$aStarMean[sumData$treatment==treatment] - aStarMean_day1
  }
  
  # Calculate significance values @ each time point via unpaired t-test.
  # Compare each experimental group to control.
  significance <- compare_means(aStar~treatment, data=rawData, method="t.test", paired=FALSE, group.by="day", ref.group="control")
  sumData <- merge(sumData, significance, by.x=c("treatment", "day"), by.y=c("group2", "day"), all.x=TRUE)
  sumData[, c(".y.", "group1", "p.adj", "p.format", "method")] <- NULL
  sumData$p.signif[sumData$p.signif=="ns"] <- NA
  sumData[sumData$day==1, c("p", "p.signif")] <- NA
  
  # Correct treatment labels.
  sumData$treatment <- as.character(sumData$treatment)
  sumData$treatment[sumData$treatment=="control"] <- "Wounded  "
  sumData$treatment[sumData$treatment=="mel1000"] <- "Melatonin  "
  sumData$treatment[sumData$treatment=="cys500"] <- "Cysteine  "
  sumData$treatment <- factor(sumData$treatment, levels=c("Wounded  ", "Melatonin  ",  "Cysteine  "))
  
  return(sumData)
}
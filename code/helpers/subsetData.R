# Define function to subset a* data to include only cut sites with comparable initial color.
subsetData <- function(allData, var, aStarMin, aStarMax){
  # Subset allData by filtering out samples with initial aStar outside of specified bounds.
  subsetData <- allData %>%
    filter(variety==var) %>%
    pivot_wider(names_from=day, values_from=aStar, names_prefix="aStar_day") %>%
    filter(aStar_day1>aStarMin & aStar_day1<aStarMax)
  
  # Recursively filter samples until all treatment groups have the same number of samples.
  # Filter in order of distance from mean of treatment group with the fewest samples.
  samplesPerGroup <- subsetData %>%
    group_by(treatment) %>%
    tally()
  limitingGroup <- as.character(samplesPerGroup$treatment[samplesPerGroup$n==min(samplesPerGroup$n)])[1]
  limitingGroupMean <- mean(subsetData$aStar_day1[subsetData$treatment==limitingGroup])
  
  subsetData <- subsetData %>%
    mutate(meanDiff=abs(aStar_day1-limitingGroupMean))
  
  for(treatmentGroup in samplesPerGroup$treatment){
    while(sum(subsetData$treatment==treatmentGroup) > sum(subsetData$treatment==limitingGroup)){
      maxDiff <- max(subsetData[subsetData$treatment==treatmentGroup, ]$meanDiff)
      subsetData <- subsetData[!(subsetData$treatment==treatmentGroup & subsetData$meanDiff==maxDiff), ]
    }
  }
  
  subsetData <- subsetData[, 1:ncol(subsetData)-1]
  subsetData <- subsetData %>%
    pivot_longer(cols=c("aStar_day1", "aStar_day3", "aStar_day5"), names_to="day",
                 names_prefix="aStar_day", values_to="aStar")
  
  return(subsetData)
}
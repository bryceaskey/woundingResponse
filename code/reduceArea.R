# Function to reduce the area considered of cut site
# Delete pixels some % of distance away from the center of the cut site
# Images are different sizes, so average height of cut site must be calculated first

#reduceArea <- function(image, pctReduce){
  colHeight <- vector(mode="numeric", length=ncol(image))
  colAvg <- vector(mode="numeric", length=ncol(image))
  for(col in 1:ncol(image)){
    colPixels <- vector(mode="numeric", length=sum(image[ , col, 4] == 1))
    pixelCount <- 0
    for(row in 1:nrow(image)){
      if(image[row, col, 4] == 1){
        pixelCount <- pixelCount + 1
        colPixels[pixelCount] <- row
      }
    }
    if(pixelCount > 0){
      colHeight[col] <- max(colPixels) - min(colPixels)
      colAvg[col] <- mean(colPixels)
    }else{
      colHeight[col] <- NA
      colAvg[col] <- NA
    }
  }
  heightData <- data.frame(colHeight, colAvg)
  reducedHeight <- mean(heightData$colHeight, na.rm=TRUE)*0.8 #replace with pctReduce
  for(col in 1:ncol(image)){
    if(sum(image[ , col, 4] == 1) > 0){
      upperLim <- heightData$colAvg[[col]] - reducedHeight/2
      lowerLim <- heightData$colAvg[[col]] + reducedHeight/2
      for(row in 1:nrow(image)){
        if(row < upperLim | row > lowerLim){
          image[row, col, 1] <- 0
          image[row, col, 2] <- 0
          image[row, col, 3] <- 0
          image[row, col, 4] <- 0
        }
      }
    }
  }
#}
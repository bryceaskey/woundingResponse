# Given a path to an RGB image, determine the % of pixels that fall within 1 SD of "pink"
# Pink is defined by a set of RGB pixel values in a dataframe selected from wounding sites on untreated ribs
pctPink <- function(imagePath, pinkData){
  # Read RGB image into environment
  image <- readImage(imagePath)
  
  # Calculate mean and sd for pixel values in pinkData for appropriate variety
  sumPinkData <- pinkData %>%
    group_by(variety) %>%
    summarise(meanR=mean(R), sdR=sd(R), meanG=mean(G), sdG=sd(G), meanB=mean(B), sdB=sd(B))
  
  ribVariety <- strsplit(tail(strsplit(imagePath, "/")[[1]], n=1), "_")[[1]][1]
  sumPinkData <- sumPinkData[sumPinkData$variety==ribVariety, ]
  
  uLim <- c((sumPinkData$meanR+sumPinkData$sdR)/255, (sumPinkData$meanG+sumPinkData$sdG)/255, (sumPinkData$meanB+sumPinkData$sdB)/255)
  lLim <- c((sumPinkData$meanR-sumPinkData$sdR)/255, (sumPinkData$meanG-sumPinkData$sdG)/255, (sumPinkData$meanB-sumPinkData$sdB)/255)
  
  # Ignore background pixels
  pinkPixelCount <- 0
  for(row in 1:nrow(image)){
    for(col in 1:ncol(image)){
      if(image[row, col, 4] == 1){
        R <- image[row, col, 1]
        G <- image[row, col, 2]
        B <- image[row, col, 3]
        if(R<uLim[1] & R>lLim[1] & G<uLim[2] & G>lLim[2] & B<uLim[3] & B>lLim[3]){
          pinkPixelCount <- pinkPixelCount + 1
        }
      }
    }
  }
  
  pixelCount <- sum(image[ , , 4] == 1)
  
  percentPink <- (pinkPixelCount/pixelCount)*100
                        
  return(percentPink)
}
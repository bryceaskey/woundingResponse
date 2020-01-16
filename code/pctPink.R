# Given a path to an RGB image, determine the % of pixels that fall within 1 SD of "pink"
# Pink is defined by a set of RGB pixel values in a dataframe selected from wounding sites on untreated ribs
#pctPink <- function(imagePath, pinkData){
  # Read RGB image into environment
  image <- readImage(imagePath)
  
  # Initialize empty vectors to store RGB values
  pixelCount <- sum(image[ , , 4] == 1)
  R <- vector(mode="numeric", length=pixelCount)
  G <- vector(mode="numeric", length=pixelCount)
  B <- vector(mode="numeric", length=pixelCount)
  
  # Create dataframe of RGB values for all pixels in image
  # Ignore background pixels
  pixelCount <- 1
  for(row in 1:nrow(image)){
    for(col in 1:ncol(image)){
      if(image[row, col, 4] == 1){
        R[pixelCount] <- image[row, col, 1]
        G[pixelCount] <- image[row, col, 2]
        B[pixelCount] <- image[row, col, 3]
        pixelCount <- pixelCount + 1
      }
    }
  }
  RGBpixels <- data.frame(R, G, B, stringsAsFactors=FALSE)
  
  # Calculate mean and sd for pixel values in pinkData for appropriate variety
  sumPinkData <- pinkData %>%
    group_by(variety) %>%
    summarise(meanR=mean(R), sdR=sd(R), meanG=mean(G), sdG=sd(G), meanB=mean(B), sdB=sd(B))
  
  ribVariety <- strsplit(tail(strsplit(imagePath, "/")[[1]], n=1), "_")[[1]][1]
  sumPinkData <- sumPinkData[sumPinkData$variety==ribVariety, ]
  
  
  
  # Not right, need to use a for loop to consider each pixel individually
  numPinkPixels <- sum(RGBpixels$R > (sumPinkData$meanR-sumPinkData$sdR)/255 & 
                      RGBpixels$R < (sumPinkData$meanR+sumPinkData$sdR)/255 &
                      RGBpixels$G > (sumPinkData$meanG-sumPinkData$sdG)/255 & 
                      RGBpixels$G < (sumPinkData$meanG+sumPinkData$sdG)/255 &
                      RGBpixels$B > (sumPinkData$meanB-sumPinkData$sdB)/255 & 
                      RGBpixels$B < (sumPinkData$meanB+sumPinkData$sdB)/255)
                        
  
#}
# Given a path to an RGB image, converts image into L*a*b* color space
# and returns mean value of a* component 
aStarMean <- function(imagePath){
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
  
  # Convert all RGB data into L*a*b* color space, and save a* component
  aStar <- vector(mode="numeric", length=nrow(RGBpixels))
  for(i in 1:nrow(RGBpixels)){
    RGBpixel <- RGBpixels[i, ]
    Labpixel <- as.numeric(convertColor(RGBpixel, from="sRGB", to="Lab"))
    aStar[i] <- Labpixel[[2]]
  }
  
  return(mean(aStar))
}
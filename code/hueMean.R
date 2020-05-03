# Given a path to an RGB image, converts image into HSV color space
# and returns mean value of hue component 
hueMean <- function(imagePath){
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
  
  # Convert all RGB data into HSV color space, and save hue component
  hue <- vector(mode="numeric", length=nrow(RGBpixels))
  for(i in 1:nrow(RGBpixels)){
    RGBpixel <- RGBpixels[i, ]
    HSVpixel <- rgb2hsv(RGBpixel[[1]], RGBpixel[[2]], RGBpixel[[3]], maxColorValue=1)
    hue[i] <- HSVpixel[[1]]*360
  }
  
  return(mean(hue))
}
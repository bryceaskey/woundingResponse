# Given a path to an RGB image, determine the % of pixels that fall within 1 SD of "pink"
# Pink is defined by a set of RGB pixel values in a .csv file selected from wounding sites on untreated ribs
pctPink <- function(imagePath, pinkPath){
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
  
  
}
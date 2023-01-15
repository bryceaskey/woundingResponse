# Reads in images of lettuce wounding sites with background pixels removed, parses image name to extract metadata,
# calculates mean a* of image, and collects all data in a data frame.
# Load necessary packages.
library(OpenImageR)
library(tidyverse)

# Define path to wounding site images.
imagePath <- "C:/Users/Bryce/Research/woundingResponse/bigData/v3/woundingSites/"

# Define function to convert an RGB image into L*a*b* and return its mean a*.
aStarCalc <- function(imagePath){
  # Read RGB image into environment
  image <- readImage(imagePath)
  
  # Initialize empty vectors to store RGB values.
  pixelCount <- sum(image[ , , 4] == 1)
  R <- vector(mode="numeric", length=pixelCount)
  G <- vector(mode="numeric", length=pixelCount)
  B <- vector(mode="numeric", length=pixelCount)
  
  # Create dataframe of RGB values for all pixels in image (ignoring background pixels).
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
  
  # Convert all RGB data into L*a*b* color space.
  aStar <- vector(mode="numeric", length=nrow(RGBpixels))
  for(i in 1:nrow(RGBpixels)){
    RGBpixel <- RGBpixels[i, ]
    Labpixel <- as.numeric(convertColor(RGBpixel, from="sRGB", to="Lab"))
    aStar[i] <- Labpixel[[2]]
  }
  
  # Return mean of a*.
  return(mean(aStar))
}

# Initialize empty lists to store image data.
variety <- vector(mode="character", length=length(dir(imagePath)))
treatment <- vector(mode="character", length=length(dir(imagePath)))
day <- vector(mode="character", length=length(dir(imagePath)))
rib <- vector(mode="character", length=length(dir(imagePath)))
cutSite <- vector(mode="character", length=length(dir(imagePath)))
aStar <- vector(mode="numeric", length=length(dir(imagePath)))

# Loop through all images in imagePath and store data in lists.
for(i in 1:length(dir(imagePath))){
  imageName <- dir(imagePath)[i]
  print(paste("Now processing image", imageName))
  variety[i] <- strsplit(imageName, "_")[[1]][1]
  treatment[i] <- strsplit(imageName, "_")[[1]][2]
  day[i] <- strsplit(imageName, "_")[[1]][3]
  rib[i] <- strsplit(strsplit(strsplit(imageName, "_")[[1]][4], ".", fixed=TRUE)[[1]][1], "-")[[1]][1]
  cutSite[i] <- strsplit(strsplit(strsplit(imageName, "_")[[1]][4], ".", fixed=TRUE)[[1]][1], "-")[[1]][2]
  aStar[i] <- aStarCalc(paste(imagePath, "/", imageName, sep=""))
}

# Combine lists into a data frame.
aStarData <- data.frame(variety=variety, treatment=treatment, day=day, rib=rib, cutSite=cutSite, aStar=aStar)

# Save data frame as a .csv file.
write.csv(aStarData, file="C:/Users/Bryce/Research/woundingResponse/data/v3/aStarData.csv", row.names=FALSE)
# Install and load necessary packages
packageList <- c("OpenImageR")
newPackages <- packageList[!(packageList %in% installed.packages()[,"Package"])]
if(length(newPackages) > 0){
  install.packages(newPackages)
}
library(OpenImageR)

makeHeatmap <- function(imagePath, outputPath, lowerLim=-2, upperLim=2){
  image <- readImage(imagePath)
  
  pixelCount <- 0
  aStarList <- vector(mode="numeric", length=pixelCount)
  
  heatmap <- array(0, dim=c(nrow(image), ncol(image), 4))
  for(row in 1:nrow(image)){
    for(col in 1:ncol(image)){
      if(image[row, col, 4] == 1){
        pixelCount = pixelCount + 1
        
        heatmap[row, col, 4] <- 1
        
        sRGBpixel <- data.frame(R=image[row, col, 1], G=image[row, col, 2], B=image[row, col, 3])
        Labpixel <- as.numeric(convertColor(sRGBpixel, from="sRGB", to="Lab"))
        aStar <- Labpixel[[2]]
        
        aStarList[pixelCount] <- aStar
        
        if(aStar < upperLim-((upperLim-lowerLim)*0.8)){
          heatmap[row, col, 1:3] <- c(255, 255, 255)/255
        }else if(aStar < upperLim-((upperLim-lowerLim)*0.6)){
          heatmap[row, col, 1:3] <- c(248, 205, 217)/255
        }else if(aStar < upperLim-((upperLim-lowerLim)*0.4)){
          heatmap[row, col, 1:3] <- c(242, 155, 180)/255
        }else if(aStar < upperLim-((upperLim-lowerLim)*0.2)){
          heatmap[row, col, 1:3] <- c(235, 105, 142)/255
        }else{
          heatmap[row, col, 1:3] <- c(229, 56, 105)/255
        }
      }
    }
  }
  
  writeImage(heatmap, outputPath)
  print(paste("Mean a* value:", mean(aStarList)))
}

# For testing: 
imagePath <- "C:/Users/Bryce/Documents/woundingResponse/data/v3/woundingSites/iceberg_control_5_A-1.png"
outputPath <- "C:/Users/Bryce/Documents/woundingResponse/data/v3/iceberg_control_5_A-1_heatmap.png"

makeHeatmap(imagePath, outputPath, lowerLim=-5, upperLim=10)
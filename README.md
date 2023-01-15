# woundingReponse
Image processing method for quantification of wounding response on lettuce leaves.

## code/
**processImages.R** - Reads in images of lettuce wounding sites with background pixels removed, parses image name to extract metadata, calculates mean a* of image, and collects all data in a data frame.

**helpers/** - Contains helper functions subsetData.R and summarizeData.R, which are used by other scripts. subsetData.R subsets a* data to include only cut sites with comparable initial color. summarizeData.R summarizes data for plotting by calculating mean and SE, and performing significance testing. 

**fig1CF.R** - Generates line graphs in Fig 1 C & F showing change in mean a*. 

**figS1.R** - Generates scatterplot in Fig S1 comparing a* and visual rating results.

## data/
**aStarData.csv** - Mean a* of all wounding site images.

**ratingData.csv** - Visual rating data used for comparison against a* results.

## figures/
**fig1CF.tif** - Figure 1, C & F. Line graphs of change in mean a*.

**figS1.tif** - Supplementary figure 1. Scatterplot comparing a* and visual rating results.

**pixelSelection/** - Representative images illustrating wounding site image extraction.
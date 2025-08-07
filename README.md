# lasRpipeline

Provides wrapper functions for parallel processing using lasR of typical forestry lidar workflow, from raw point cloud to tree segmentation.

Please install [lasR](https://github.com/r-lidar/lasR)

## Usage

``` r
if(!require("lasR")) install.packages('lasR', repos = 'https://r-lidar.r-universe.dev')

testFile <- system.file("extdata", "BL5_UTM_32_ort_0021.laz", package = "lasRpipeline")

f <- list.files("/archivio/shared/geodati/las/fvg", pattern="(?i)\\.la(s|z)$", full.names = T )

if(hasGroundPoints(f) < 0.00000000001){
  message_log("Please classify ground points with your favorite method. ") 
} else {
  
  message_log("Reference GRID, might be different CRS. ") 
  grid <- terra::rast("../wildfire/input/AT-IT_ScottBurganFuelMapClassV2.tif")
  if(terra::crs(grid$scottburgan_class)){
    
  }
}


t



 
```

## Acknowledgements

Part of this work is supported by the Wildfire CE Interreg Project, grant number CE0200934.

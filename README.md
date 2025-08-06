# lasRpipeline

Provides wrapper functions for parallel processing using lasR of typical forestry lidar workflow, from raw point cloud to tree segmentation.

Please install [lasR](https://github.com/r-lidar/lasR)

## Usage

``` R
if(!require("lasR")) install.packages('lasR', repos = 'https://r-lidar.r-universe.dev')

testFile <- system.file("extdata", "BL5_UTM_32_ort_0021.laz", package = "lasRpipeline")
```

## Acknowledgements

Part of this work is supported by the Wildfire CE Interreg Project, grant number CE0200934.

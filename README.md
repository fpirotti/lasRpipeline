# lasRpipeline

Provides wrapper functions for parallel processing using lasR of typical forestry 
lidar workflow, from raw point cloud to metrics in cells to allow for
canopy fuel mapping to support fire behaviour modelling for  
[Cells2Fire](https://github.com/cell2fire/Cell2Fire)

Please install [lasR](https://github.com/r-lidar/lasR), 
[lidR](https://github.com/r-lidar/lidR)

NB - this pipeline assumes you have a classified point cloud, without
any noise points, so please use responsible and provide a clean and 
classified point cloud.

Parallelization only in linux for now!

## Usage

``` r 

if(!require("lidR"))  install.packages("lidR")
if(!require("terra"))  install.packages("terra")
if(!require("lasR")) install.packages('lasR', repos = 'https://r-lidar.r-universe.dev')
if(!require("devtools"))  install.packages("devtools")
if(!require("lasRpipeline")) devtools::install_github("fpirotti/lasRpipeline")

## just define input files and output directory - the processing will check if 
## intermediate steps have already been taken care of and proceed to create a 
## canopy fuel and topography 
f <- list.files("/archivio/shared/geodati/las/fvg/tarvisio/", pattern="(?i)\\.la(s|z)$", full.names = T )
odir <- "/archivio/shared/geodati/las/fvg/tarvisiooutdir/norm"
gridfile <- "../wildfire/input/AT-IT_ScottBurganFuelMapClassV2.tif"

## check this function in this package... 
?lasRpipeline::process
process(f, odir, gridfile)


 
```

## Acknowledgements

Part of this work is supported by the Wildfire CE Interreg Project, grant number CE0200934.

# lasRpipeline

Provides wrapper functions for parallel processing using lasR of typical forestry lidar workflow, from raw point cloud to metrics in cells to allow for canopy fuel mapping to support fire behaviour modelling for\
[Cells2Fire](https://github.com/cell2fire/Cell2Fire)

Please install [lasR](https://github.com/r-lidar/lasR), [lidR](https://github.com/r-lidar/lidR)

NB - this pipeline assumes you have a classified point cloud, without any noise points, so please use responsibly and provide a clean and classified point cloud.

Parallelization only in linux for now.

WORK IN PROGRESS...

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
odir <- "/archivio/shared/geodati/las/fvg/tarvisiooutdir"

## The following will be the grid that acts as a template where all results will be saved
## so it leads the resolution and origin of the grid. Ideally it should be in the same
## CRS of the point cloud.
gridfile <- "/archivio/shared/R/wildfire/input/AT-IT_ScottBurganFuelMapClassV2.tif"

## check this function in this package... 

?lasRpipeline::process
## run one of the following:

# normalize + create DTM DSM CHM and metrics at same resolution of gridfile
process(f, odir, gridfile) 

# normalize + create DTM DSM CHM at 2 m resolution and metrics at same resolution of gridfile
process(f, odir, gridfile, T,T,T, 2)


 
```

## Acknowledgements

Part of this work is supported by the Wildfire CE Interreg Project, grant number CE0200934.

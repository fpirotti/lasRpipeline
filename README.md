# lasRpipeline

Provides wrapper functions for parallel processing using lasR of typical forestry 
lidar workflow, from raw point cloud to metrics in cells to allow for
canopy fuel mapping to support fire behaviour modelling for  
[Cells2Fire](https://github.com/cell2fire/Cell2Fire)

Please install [lasR](https://github.com/r-lidar/lasR), 
[lidR](https://github.com/r-lidar/lidR)

## Usage

``` r 

if(!require("lidR"))  install.packages("lidR")
if(!require("terra"))  install.packages("terra")
if(!require("lasR")) install.packages('lasR', repos = 'https://r-lidar.r-universe.dev')
if(!require("devtools"))  install.packages("devtools")
if(!require("lasRpipeline")) devtools::install_github("fpirotti/lasRpipeline")
# install.packages("devtools")
#if(!require("lidRviewer"))  install.packages('lidRviewer', repos = c('https://r-lidar.r-universe.dev'))
# testFile <- system.file("extdata", "BL5_UTM_32_ort_0021.laz", package = "lasRpipeline")

## just define input files and output directory - the processing will check if 
## intermediate steps have already been taken care of and proceed to create a 
## canopy fuel and topography 
f <- list.files("/archivio/shared/geodati/las/fvg/tarvisio/", pattern="(?i)\\.la(s|z)$", full.names = T )
odir <- "/archivio/shared/geodati/las/fvg/tarvisiooutdir/norm"

ctg <- lidR::catalog(f)
# plot(ctg)
process<-function(){
  if(hasGroundPoints(ctg) < 0.00000000001){
    message_log("Please classify ground points with your favorite method before continuing. ") 
    stop("No ground class")
  }  
  
  lasR::set_parallel_strategy(lasR::nested(ncores = 12L, ncores2 = 4L))
  message_log("Normalize") 
 
  exec(write_lax(), on = ctg,  
       with = list(progress = TRUE, 
                     ncores = min(length(ctg@data$filename), 
                                  as.integer(half_cores()/2) ) ) )
                                  
  normFiles <- normalize(ctg, odir)
   
  message_log("Reference GRID, might be different CRS. ") 
  
  grid <- terra::rast("../wildfire/input/AT-IT_ScottBurganFuelMapClassV2.tif")
  
  grid[values(grid,mat=F) < 180 | terra::values(grid,mat=F) > 190] <- NA
  cellids <- terra::cells(grid)
  centers <- terra::xyFromCell(grid,cellids)
  
  points_sf <- sf::st_as_sf(as.data.frame(centers), coords = c("x", "y"), crs = crs(grid) )
  
  points_sf_crsPoints <- sf::st_transform(points_sf, lidR::crs(ctg))  
 
  points_sf_crsPoints$cellID <- cellids
  
  ctg.norm <- lidR::catalog(normFiles[3:4])
  
  dd <- sf::st_intersects(points_sf_crsPoints, sf::st_union(ctg.norm@data$geometry), sparse =  FALSE)
  
  points_sf_crsPoints_overlap <-  points_sf_crsPoints[dd,]
  
  points_sf_crsPoints_overlap_coords <- sf::st_coordinates(points_sf_crsPoints_overlap) 
  
  lasR::set_parallel_strategy(lasR::sequential())
  
  # set_lidr_threads(1) ; data.table::setDTthreads(1) # for cran only

read = reader_circles(points_sf_crsPoints_overlap_coords[,1], 
                      points_sf_crsPoints_overlap_coords[,2], 5)
                      
metrics = summarise(metrics = c("z_mean", "z_p95",  "count", "HAG_p50" ))

pipeline = read + metrics

ans = exec(pipeline, on = ctg.norm )

                          
}

# plot(ctg, mapview = TRUE, map.type = "Esri.WorldImagery")
 


 
```

## Acknowledgements

Part of this work is supported by the Wildfire CE Interreg Project, grant number CE0200934.

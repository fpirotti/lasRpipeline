#' process
#'
#' @returns NA
#' @export
#'
#' @examples
#'  # use this to process example
process<-function(){

  f <- list.files("/archivio/shared/geodati/las/fvg/tarvisio/", pattern="(?i)\\.la(s|z)$", full.names = T )
  odir <- "/archivio/shared/geodati/las/fvg/tarvisiooutdir/norm"
  ctg <- lidR::catalog(f)
  if(hasGroundPoints(ctg) < 0.00000000001){
    message_log("Please classify ground points with your favorite method before continuing. ")
    stop("No ground class")
  }

  lasR::set_parallel_strategy(lasR::nested(ncores = 12L, ncores2 = 4L))
  message_log("Normalize")

  lasR::exec(lasR::write_lax(), on = ctg,
       with = list(progress = TRUE,
                   ncores = min(length(ctg@data$filename),
                                as.integer(lasR::half_cores()/2) ) ) )

  normFiles <- lasRpipeline::normalize(ctg, odir)

  message_log("Reference GRID, might be different CRS. ")

  grid <- terra::rast("../wildfire/input/AT-IT_ScottBurganFuelMapClassV2.tif")

  grid[terra::values(grid,mat=F) < 180 | terra::values(grid,mat=F) > 190] <- NA
  cellids <- terra::cells(grid)
  centers <- terra::xyFromCell(grid,cellids)

  points_sf <- sf::st_as_sf(as.data.frame(centers), coords = c("x", "y"), crs = terra::crs(grid) )

  points_sf_crsPoints <- sf::st_transform(points_sf, lidR::crs(ctg))

  points_sf_crsPoints$cellID <- cellids

  ctg.norm <- lidR::catalog(normFiles[3:4])

  dd <- sf::st_intersects(points_sf_crsPoints, sf::st_union(ctg.norm@data$geometry), sparse =  FALSE)

  points_sf_crsPoints_overlap <-  points_sf_crsPoints[dd,]

  points_sf_crsPoints_overlap_coords <- sf::st_coordinates(points_sf_crsPoints_overlap)

  ##STRATEGY FOR PARALLELIZE AS INJECTED R CODE (function fuelMetrics)
  ## will not be parallel in lasR - but we need to split the many cells not the
  ## lidar chunck so we do it ourselves

  chunkProcess <- function(points_sf_crsPoints_overlap_coords_chunk){

    read = lasR::reader_circles(points_sf_crsPoints_overlap_coords_chunk[,1],
                          points_sf_crsPoints_overlap_coords_chunk[,2], 5)
    metrics =  lasR::callback(fuelMetrics, expose = "xyzcE",
                        drop_buffer = T, no_las_update = T)
    pipeline = read + metrics

    message_log("Starting")
    ans = lasR::exec(pipeline, on = ctg.norm@data$filename, progress=T, ncores=1)
    message_log("END")
    ans
  }

}


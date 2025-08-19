#' process
#' @description
#' This will launch optimized processes using 20 plots per chunk and parallelization
#'
#' Works only in linux for now!
#'
#' Plots are the centers of the raster cell where we want to calculate topography
#' and canopy information.
#'
#' @param f path with input las/laz files or specific list of las/laz files
#' @param odir path to output directory
#' @param gridfile path to raster that is the template of your output - MUST
#' overlap the lidar area of course
#' @param createDTM default = TRUE - creates a raster with DTM in the same
#' resolution of the gridfile
#' @param createDSM default = TRUE - creates a raster with DSM in the same
#' resolution of the gridfile
#' @param createCHM default = TRUE - creates a raster with CHM in the same
#' resolution of the gridfile
#' @param forceRes default = NULL - if number is given, it will force resolution of DTM DSM CHM
#' @param check default  = FALSE - it will check if DTM is already present if
#' TRUE. If false, it will NOT overwrite the DTM / DSM / CHM, but set out a
#' warning. This is important to use this function as background job, which
#' often helps to avoid crashing from multi-thread processing.
#' @param concurrent_files default = NULL - if NULL it will calculate how many
#' concurrent LAS files to process by trying to check the RAM and number of cores
#' and divide it by the maximum size of LAS files.
#'
#' @returns a terra raster object also written to a file tif with time and date,
#' written to the odir e.g.  "gridOut20250811_094743.tif"
#' The TIF file will have seven layers, with:
#'  - terrain height/slope/aspect
#'  - canopy cover (0-100 percent)
#'  - canopy height (95th percentile)
#'  - crown base height (m)
#'  - canopy bulk density (kg/m3)
#'
#' @export
#'
#' @examples
#'  # use this to process example
process <- function(f="/archivio/shared/geodati/las/fvg/tarvisio/",
                    odir= "/archivio/shared/geodati/las/fvg/tarvisiooutdir",
                    gridfile="../wildfire/input/AT-IT_ScottBurganFuelMapClassV2.tif",
                    createDTM = TRUE,
                    createDSM = TRUE,
                    createCHM = TRUE,
                    forceRes = NULL,
                    check = FALSE,
                    concurrent_files=8) {

  if(dir.exists(f[[1]])) {
    f <- list.files(f,
                    pattern = lasRpipeline::get_cache("laspattern"),# "(?i)\\.la(s|z)$",
                    full.names = T   )
  }

  if(!dir.exists(odir)){
    message_log("Creo cartella di output ", odir)
    dir.create(odir)
  }
  if(!dir.exists(file.path(odir,"norm") ) ){
    message_log("Creo cartella di output ", file.path(odir,"norm") )
    dir.create( file.path(odir,"norm") )
  }

  if(createDTM && !dir.exists(file.path(odir,"dtm") ) ){
    message_log("Creo cartella di output ", file.path(odir,"dtm") )
    dir.create( file.path(odir,"dtm") )
  }
  if(createDSM && !dir.exists(file.path(odir,"dsm") ) ){
    message_log("Creo cartella di output ", file.path(odir,"dsm") )
    dir.create( file.path(odir,"dsm") )
  }
  if(createCHM && !dir.exists(file.path(odir,"chm") ) ){
    message_log("Creo cartella di output ", file.path(odir,"chm") )
    dir.create( file.path(odir,"chm") )
  }
  ## creato catalog lidR ----
  message_log("Creo un catalog lidR. ")
  ctg <- lidR::catalog(f)
  if (hasGroundPoints(ctg) < 0.00000000001) {
    message_log("Please classify ground points with your favorite method before continuing. ")
    stop("No ground class")
  }

  # lasR::set_parallel_strategy(lasR::nested(ncores = 12L, ncores2 = 4L))

  ## Add LAX files if missing ----
  message_log("Add LAX files if missing")
  lasR::exec(lasR::write_lax(),
             on = ctg,
             with = list(
               progress = TRUE,
               ncores = min(
                 length(ctg@data$filename),
                 as.integer(lasR::half_cores() / 2)
               )
             ))

  ## Normalize keeping z  ----
  message_log("Normalize keeping z but adding height above ground info in extra byte")
  normFiles <- lasRpipeline::normalize(ctg, file.path(odir, "norm") )


  grid <- terra::rast(gridfile)
  sizeOfGrid <- terra::res(grid)[[1]]

  ## Creating base output raster  ----
  message_log("Creating base output raster")
  gridOut <- terra::rast(grid, nlyrs = 7)
  gridOut[] <- NA
  names(gridOut) <- c("height", "slope", "aspect",
                      "canopyCover", "canopyHeight",
                      "CBH", "CBD")

  ## boundaries and/or DTM -----------
  # here is a bit complex as we pipe them both if both are requested,
  # to save memory

  ## this is to check if to add DTM DSM CHM -----------
  createDTMfun <- function(pipelineIn){

    if(!is.null(forceRes)) res <- forceRes  else res <- sizeOfGrid

    if (file.exists(file.path(odir, "DTM.vrt"))){
      vrt <- terra::rast(file.path(odir, "DTM.vrt"))
      # if(terra::res(vrt)[[1]]==res){
      if (ask_user(paste0("DTM with resolution of ", terra::res(vrt)[[1]]," m) exists, you want to overwrite?"), default = FALSE)) {
          message_log("Adding creation of DTM to pipeline")
          dtm = lasR::rasterize(res, tri,ofile = file.path(odir, "dtm", "*.tif") )
          pipelineIn <- pipelineIn + dtm
        } else {
          message_log("Skipping creation of DTM - if you want to force it just remove the files in DTM folder and the DTM.vrt file")
        }
    } else {
      dtm = lasR::rasterize(res, tri,ofile = file.path(odir, "dtm", "*.tif") )
      pipelineIn <- pipelineIn + dtm
    }

    if (file.exists(file.path(odir, "DSM.vrt"))){
      vrt <- terra::rast(file.path(odir, "DSM.vrt"))
      # if(terra::res(vrt)[[1]]==res){
      if (ask_user(paste0("DSM with resolution of ", terra::res(vrt)[[1]]," m) exists, you want to overwrite?"), default = FALSE)) {
        message_log("Adding creation of DSM to pipeline")
        dsm = lasR::dsm(res, tin = TRUE, ofile = file.path(odir, "dsm", "*.tif") )
        pipelineIn <- pipelineIn + dsm
      } else {
        message_log("Skipping creation of DSM - if you want to force it just remove the files in DSM folder and the DSM.vrt file")
      }
    } else {
      dsm = lasR::dsm(res, tin = TRUE, ofile = file.path(odir, "dsm", "*.tif") )
      pipelineIn <- pipelineIn + dsm
    }

    if (file.exists(file.path(odir, "CHM.vrt"))){
      vrt <- terra::rast(file.path(odir, "CHM.vrt"))
      # if(terra::res(vrt)[[1]]==res){
      if (ask_user(paste0("CHM with resolution of ", terra::res(vrt)[[1]]," m) exists, you want to overwrite?"), default = FALSE)) {
        message_log("Adding creation of CHM to pipeline - if you want to force it just remove the files in CHM folder and the CHM.vrt file")
        chm = lasR::chm(res, tin = TRUE, ofile = file.path(odir, "chm", "*.tif") )
        pipelineIn <- pipelineIn + chm
      } else {
        message_log("Skipping creation of CHM - if you want to force it just remove the files in CHM folder and the CHM.vrt file")
      }
    } else {
      chm = lasR::chm(res, tin = TRUE, ofile = file.path(odir, "chm", "*.tif") )
      pipelineIn <- pipelineIn + chm
    }


    if(length(pipelineIn)>2){





      message_log("Starting process on n.",
                  cli::style_bold(length(normFiles)),
                  " files, with n.",
                  cli::style_bold(lasR::half_cores()*2),
                  " cores, using  n.",
                  cli::style_bold(concurrent_files),
                  " concurrent files, on a raster with resolution of ",
                  cli::style_bold(res),
                  " m might take some time ttt...")

      # read <- lasR::reader()
      # tri <- lasR::triangulate(max(10, res*3), filter = lasR::keep_ground())
      # pipelineIn <- read + tri + dsm + chm

      lasR::set_parallel_strategy( concurrent_files(concurrent_files)  )
      ans <- lasR::exec(pipelineIn, on = normFiles )
      # lasR::unset_parallel_strategy()

      if(is.character(ans)){
        resRas <- ans
      } else{
        if(is.list(ans)) resRas <- ans$rasterize
        else message_log("Problem here", isWarning = T)
      }

      vrt <- terra::vrt(resRas, file.path(odir, filename="DTM.vrt"), overwrite=T)
      message_log("Tiles DTM nella cartella 'dtm' e File DTM.vrt creato nella cartella ",
                  cli::style_hyperlink(file.path(odir),
                                       paste0("file://",file.path(odir) ) ) )


      if(!is.null(ans$hulls)){
        sf::write_sf(sf::st_union( ans$hulls),
                     file.path(odir, "boundaries.gpkg"), append = FALSE)

        message_log("Boundary file boundaries.gpkg in output directory: ",
                    cli::style_hyperlink(file.path(odir,"boundaries.gpkg"),
                                         paste0("file://",file.path(odir,"boundaries.gpkg") ) ) )
      }
    } else {
      message_log("Boundary and DTM already available, remove them from output directory if you want to re-create them")
    }

  }


  read <- lasR::reader()
  tri <- lasR::triangulate(max(10, sizeOfGrid*3), filter = lasR::keep_ground())
  pipeline <- read + tri
  ## Creating Boundary  ----
  if (!file.exists(file.path(odir, "boundaries.gpkg"))) {
    message_log("Creating boundary, might take some time")
    contour <- lasR::hulls(tri)
    pipeline <- pipeline + contour
    message_log(
      "Boundary is strictly necessary because lasR crashes if plots are inside a tile but without any points inside! "
    )
  }

  if(!createDTM && length(pipeline)>2){
    ans <- lasR::exec(pipeline, on = normFiles, with = list(ncores = lasR::half_cores()))
    sf::write_sf(sf::st_union( ans),
                 file.path(odir, "boundaries.gpkg"), append = FALSE)
  } else{
    createDTMfun(pipeline)
  }
  message_log(  "Reading boundary"  )
  boundary <- sf::read_sf(file.path(odir, "boundaries.gpkg"))
  message_log("Getting values of centers of cells")
  # grid2 <- terra::disagg(grid, 3)



  message_log("START")
  tv <- terra::values(grid, mat = F)
  grid[tv < 100 | tv > 190] <- NA
  cellids <- terra::cells(grid)

  message_log(cli::style_bold(format(length(cellids),big.mark = "'") ) , " cells to process!")
  centers <- terra::xyFromCell(grid, cellids)


  message_log("Converting to simple features to transform between CRS")
  points_sf <- sf::st_as_sf(as.data.frame(centers),
                            coords = c("x", "y"),
                            crs = terra::crs(grid))

  message_log("Transforming")
  points_sf_crsPoints <- sf::st_transform(points_sf, lidR::crs(ctg))

  points_sf_crsPoints$cellID <- cellids

  ctg.norm <- lidR::catalog(normFiles)

  message_log("Add LAX files to normalized points if missing")
  lasR::exec(lasR::write_lax(),
             on = normFiles,
             with = list(
               progress = F,
               ncores = min(length(normFiles), as.integer(lasR::half_cores() /
                                                            2))
             ))

  message_log("Keeping only points that intersect the hull of the points")
  dd <- sf::st_intersects(points_sf_crsPoints, boundary, sparse =  FALSE)

  points_sf_crsPoints_overlap <-  points_sf_crsPoints[dd[,1], ]

  points_sf_crsPoints_overlap_coords <- as.data.frame(sf::st_coordinates(points_sf_crsPoints_overlap))
  points_sf_crsPoints_overlap_coords$id <- points_sf_crsPoints_overlap$cellID
  ##STRATEGY FOR PARALLELIZE AS INJECTED R CODE (function fuelMetrics)
  ## will not be parallel in lasR - but we need to split the many cells not the
  ## lidar chunck so we do it ourselves


  message_log("DONE")

  return(NULL)
  chunkProcess <- function(points_sf_crsPoints_overlap_coords_chunk) {

    read <- lasR::reader_circles(
      points_sf_crsPoints_overlap_coords_chunk[, 1],
      points_sf_crsPoints_overlap_coords_chunk[, 2],
      sizeOfGrid / 1.8
    )

    metrics <-  lasR::callback(
      fuelMetrics,
      expose = "xyzcE",
      drop_buffer = T,
      no_las_update = T
    )

    pipeline <- read  + metrics

    ans <- lasR::exec(
      pipeline,
      on = ctg.norm@data$filename,
      progress = F,
      ncores = 1
    )

    # plot(points_sf_crsPoints_overlap[1:10,], add=T)
    # plot(ans)
    ans
  }

  pointsPerChunk <- 20

  chunks <- split(points_sf_crsPoints_overlap_coords,
                  ceiling(seq_len(
                    nrow(points_sf_crsPoints_overlap_coords)
                  ) / pointsPerChunk))


  are.null <- rep(T, length(chunks))
  finalRes <- list()
  oldAreNulls <- 0
  tries <- 0
  while (sum(are.null) != 0) {
    if (oldAreNulls == sum(are.null)) {
      tries <- tries + 1
      if (tries > 2) {
        break
      }
      message_log("Still same remaining chunks ...",
                  sum(are.null),
                  " will try one more time")
    }
    message_log("Remaining chunks ...", sum(are.null))

    fres <-  parallel::mclapply(names(chunks[are.null]),
                                mc.cores = min(length(chunks),
                                               lasR::half_cores()-1),
                                function(chunkID) {
      chunk <- chunks[[as.character(chunkID)]]
      res <- chunkProcess(chunk)
      res <- cbind(do.call(rbind, res), chunk$id)
      res
    })

    names(fres) <- names(chunks[are.null])
    are.null <- sapply(fres, function(x) {
      is.null(x) || inherits(x, "error") || inherits(x, "try-error")
    })

    finalRes[names(fres)[which(!are.null)]] <- fres[which(!are.null)]
  }

  fres2 <-  lapply(finalRes, function(x) {
    tryCatch({
      as.data.frame(x)
    }, warning = function(e){
      browser()
    })
  })

  fres3 <- data.table::rbindlist(fres2)

  points_sfOut <- sf::st_as_sf(as.data.frame(fres3),
                            coords = c("V1", "V2"),
                            crs= lidR::crs(ctg)  )

  points_sf_crsPoints_out <- sf::st_transform(points_sfOut, crs = terra::crs(grid))
  cc<-sf::st_coordinates(points_sf_crsPoints_out)

  gridOut[[1]][fres3$V6] <- cc[,1]
  gridOut[[2]][fres3$V6] <- cc[,2]
  gridOut[[3]][fres3$V6] <- fres3$V3
  gridOut[[4]][fres3$V6] <- fres3$V4
  gridOut[[5]][fres3$V6] <- fres3$V5
  gridOut[[6]][fres3$V6] <- fres3$V6
  terra::writeRaster(gridOut,
                     file.path(odir,
                               paste0("gridOut",
                                      format(Sys.time(), "%Y%m%d_%H%M%S"),
                                      ".tif") ), overwrite=T)
  gridOut
}


if(!require("lidR"))  install.packages("lidR")
if(!require("terra"))  install.packages("terra")
if(!require("lasR")) install.packages('lasR', repos = 'https://r-lidar.r-universe.dev')
if(!require("devtools"))  install.packages("devtools")
if(!require("lasRpipeline")) devtools::install_github("fpirotti/lasRpipeline")


f <- list.files("/archivio/shared/geodati/las/fvg/tarvisio/", pattern="(?i)\\.la(s|z)$", full.names = T )
odir <- "/archivio/shared/geodati/las/fvg/tarvisiooutdir"

## The following will be the grid that acts as a template where all results will be saved
## so it leads the resolution and origin of the grid. Ideally it should be in the same
## CRS of the point cloud.
gridfile <- "/archivio/shared/R/wildfire/input/AT-IT_ScottBurganFuelMapClassV2.tif"

## check this function in this package...

# normalize + create DTM DSM CHM at 2 m resolution and metrics at same resolution of gridfile
process(f, odir, gridfile, T,T,T, 2)

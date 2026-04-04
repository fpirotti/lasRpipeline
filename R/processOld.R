#' check_vrt_validity
#'
#' @param filevrt input files or directory with LAS/LAZ files
#' @returns raster pointer if all OK, FALSE otherwise
#'
#' @examples
#' #
check_vrt_validity <- function(filevrt){

  warns <- list()

  # Capture warnings only
  vrt <- withCallingHandlers(
    tryCatch(
      terra::rast(filevrt),
      error = function(e) {
        # Capture the error message
        warns <<- c(warns, e$message)
        return(NULL)  # return NULL if error occurs
      }
    ),
    warning = function(w) {
      # Capture all warnings
      warns <<- c(warns, w$message)
      invokeRestart("muffleWarning")  # works here inside withCallingHandlers
    }
  )

  if(length(warns )!=0){
    message_log("Problem with VRT file ", filevrt,
                ", please remove it before continuing or figure out what is wrong with it\n",
                paste(warns , collapse="\n<br>\n"),isWarning = T)
    return(FALSE)
  }
  return(terra::rast(file.path(filevrt)))
}

#' check_dir_structure
#'
#' @param ifiles input files or directory with LAS/LAZ files
#' @param odir output directory
#'
#' @returns TRUE if all OK
#'
#' @examples
#' #
check_dir_structure <- function(ifiles=NULL,
                                odir=NULL ) {

  if(is.null(ifiles) || (!dir.exists(ifiles[[1]]) && !file.exists(ifiles[[1]]))){
    message_log("Sorry but value of ",
                cli::style_italic("ifiles") ,
                " argument is neither valid files or valid directory",  isWarning = T)
    return(NULL)
  }

  if(dir.exists(ifiles[[1]])) {
    idir <- ifiles[[1]]
    ifiles <- list.files(ifiles,
                         pattern = lasRpipeline::get_cache("laspattern"),# "(?i)\\.la(s|z)$",
                         full.names = T   )
  } else {
    idir <- dirname(ifiles[[1]])
  }

  if(length(ifiles)==0 ){
    message_log("Zero LAS/LAZ files found in directory ", idir, isWarning = T )
    stop("Zero LAS/LAZ files found in directory ", idir)
  }
  if(length(ifiles)!=length(file.exists(ifiles)) ){
    message_log("Not all  LAS/LAZ files in exist - check that all files are in the right path ", isWarning = T )
    stop("Not all  LAS/LAZ files in exist - check that all files are in the right path ")
  }

  if(!dir.exists(odir)){
    if(basename(odir)==odir){
      message_log("You provided only name, not path, to output directory, thus
creating output directory ", odir, " in current working directory  ", getwd() )
      dir.create(odir)
    } else {
      message_log("Creating output directory ", odir, " in current working directory  ", getwd() )
      dir.create(odir)
    }
  } else {
    message_log("Output directory ", odir, " exists, using it." )
  }

  if(!dir.exists(file.path(odir,"norm") ) ){
    dir.create( file.path(odir,"norm") )
    message_log("Creating  'norm' subdir in output directory." )
  } else {
    message_log("'norm' subdir in output directory exists, will overwrite contents only if user confirms." )
  }
  if(!dir.exists(file.path(odir,"dtm") ) ){
    dir.create( file.path(odir,"dtm") )
  }else {
    message_log("'dtm' subdir in output directory exists, will overwrite contents only if user confirms." )
  }
  # if(!dir.exists(file.path(odir,"dsm") ) ){
  #   dir.create( file.path(odir,"dsm") )
  #   message_log("Creating  'dsm' subdir in output directory." )
  # }else {
  #   message_log("'dsm' subdir in output directory exists, will overwrite contents only if user confirms." )
  # }
  if(!dir.exists(file.path(odir,"chm") ) ){
    dir.create( file.path(odir,"chm") )
    message_log("Creating  'chm' subdir in output directory." )
  }else {
    message_log("'chm' subdir in output directory exists, will overwrite contents only if user confirms." )
  }

  return(TRUE)
}



#' process
#' @description
#' This will launch optimized processes using 20 plots per chunk and parallelization
#'
#' Works only in linux for now!
#'
#' Plots are the centers of the raster cell where we want to calculate topography
#' and canopy information.
#'
#' @param ifiles path with input las/laz files or specific list of las/laz files' path
#' if not provided it will search the current working directory.
#' @param odir path to output directory - if not provided it will create an "odir"
#' directory with name "odir" in the current working directory.
#' @param gridfile path to raster that is the template of your output - MUST
#' overlap the lidar area of course
#' @param create list with following
#'   Must include:
#'   \describe{
#'     \item{createDTM}{Bolean: default = TRUE}
#'     \item{createCHM}{Bolean: default = TRUE}
#'   }
#' @param  maxh integer, if set it will set a maximum height above the ground
#' to consider it as vegetation.
#'
#' @param forceRes default = NULL - if number is given, it will force resolution
#' of the output intermediate rasters (DTM CHM) to this value, otherwise it will
#'  use the resolution of raster in \code{gridfile}
#'
#' @param verbose Boolean default = TRUE - print lots of messages.
#'
#' @param ncores An object returned by one of \code{sequential()},
#'   \code{concurrent_points()}, \code{concurrent_files()}, or \code{nested()}.
#'   If \code{NULL} the default it will calculate how many
#' concurrent LAS files to process by trying to check the RAM and number of cores
#' and divide it by the maximum size of LAS files. If a simple integer is provided
#'   it corresponds to \code{concurrent_files(ncores)}.
#'
#' @param progress Boolean. Displays a progress bar.
#'
#' @param buffer Numeric. Each file is read with a buffer. The default is
#'   \code{NULL}, which does not mean that the file will not be buffered.
#'   It means that the internal routine determines whether a buffer is needed
#'   and will pick the greatest value between its internal suggestion and this
#'   user-supplied value.
#'
#' @param chunk Numeric. By default, files are processed one by one
#'   (\code{chunk = NULL} or \code{chunk = 0}). It is possible to process in
#'   arbitrary-sized chunks. This is useful for processing collections with
#'   large files or for handling massive \code{copc} files.
#'
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
#'  #
#'
#'  create = list(
#'  createDTM = TRUE,
#'  createCHM = TRUE
#'  )
#'  maxh = 60
#'  forceRes = 1
#'  verbose=TRUE
#'  buffer=NULL
#'  ncores=NULL
#'  chunk=NULL
#'  progress=TRUE
#'
process <- function(ifiles=NULL,
                    odir=NULL,
                    gridfile=NULL,
                    create = list(
                      createDTM = TRUE,
                      createCHM = TRUE
                    ),
                    maxh = NULL,
                    forceRes = NULL,

                    verbose=TRUE,
                    progress=TRUE,
                    ncores=NULL,
                    buffer=NULL,
                    chunk=NULL
                    ) {

  if(is.null(check_dir_structure(ifiles, odir))) return(NULL)
  ## check input las files ----

  ## creato catalog lidR ----
  if(file.exists(file.path(odir,"projectData.rda"))){
    message_log("Loading cataloque from past processing:  " , file.path(odir,"projectData.rda") , " - remove file if it is not intended", verbose=verbose)
    load(file.path(odir,"projectData.rda"))
  } else{
    message_log("Creating and saving cataloque with " , length(ifiles) , " files.",verbose = verbose)
    ctg <- lidR::catalog(ifiles)
    save(ctg, file=file.path(odir,"projectData.rda"))
  }

  ## Checking Ground points exist ----
  message_log("Checking that point cloud contains ground points (class=2)...",verbose = verbose)
  gp.ratio <- hasGroundPoints(ctg)
  message_log("Ground points ", round(gp.ratio*100, 3), "%",verbose = verbose)
  if (gp.ratio< 0.000000000001) {
    message_log("No points with ground class found, or too liPlease classify ground points with your favorite method before continuing. This tool requires that the point cloud contains classified ground points (class=2) ",verbose = verbose)
    stop("No ground class found")
  }

  ## set number of cores ----
  ## depending on the size of the files
  sc <- ifelse( is.null(ncores),
                auto_set_lasR_cores(ctg, verbose=verbose),   # auto-detect safe number of cores
                ncores)


  lasR::set_exec_options(
    progress = progress,
    ncores   = lasR::concurrent_files(sc),
    buffer=buffer,
    chunk=chunk
  )

  ## Add LAX files if missing ----
  message_log("Adding LAX files if they are missing... LAX files are indices that improve processing of LAS point clouds ",verbose = verbose)
  res <- lasR::exec(lasR::write_lax(), on = ctg )


  ## Normalize keeping z in HAG ----
  message_log("Normalizing keeping z but adding height above ground info in extra byte!",verbose = verbose)
  normFiles <- lasRpipeline::normalize(ctg, file.path(odir, "norm") )

  ctg.norm <- lidR::catalog(normFiles)
  ctg_summary <- utils::capture.output(print(ctg.norm))
  pretty_message <- paste(ctg_summary, collapse = "\n")
  message_log(cli::style_bold("\nDATA BEING PROCESSED IN LIDAR FILES:\n"), pretty_message )


  message_log("Adding LAX files to normalized points if missing",verbose = verbose)
  res <- lasR::exec(lasR::write_lax(), on = normFiles )



  ## Define Grid from template ogrid ----
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
# applyPipelineFun <- function(){

    ## we keep two separate pipelines, one for original LAS files,
    ## and one for normalized LAS files
    outnames <- c()
    outnamesn <- c()

    read <- lasR::reader()
    tri <- lasR::triangulate(0, filter = lasR::keep_ground())
    pipeline <- read
    pipelinen <- read

    if(!is.null(forceRes)) res <- forceRes  else res <- sizeOfGrid

    ## DTM ----
    if(create$createDTM){
      dtm = lasR::rasterize(res, tri, ofile = file.path(odir, "dtm", "*.tif") )

      if (file.exists(file.path(odir, "DTM.vrt")) ){

        vrt <- check_vrt_validity(file.path(odir, "DTM.vrt"))

        if(!inherits(vrt, "SpatRaster")) stop()

        if (ask_user(paste0("DTM with resolution of ", terra::res(vrt)[[1]]," m  exists, you want to overwrite?") )) {
          message_log("Adding creation of DTM to pipeline!")
          pipeline <- pipeline + tri + dtm
          outnames<- c(outnames , "dtm")
        } else {
          message_log("Skipping creation of DTM - if you want to force it just remove the files in DTM folder and the DTM.vrt file")
        }
      } else {
        pipeline <- pipeline + tri + dtm
        outnames<- c(outnames , "dtm")
        message_log("Adding DTM to pipeline", verbose=verbose )
      }

    }

    ## CHM ----
    if(create$createCHM){
      # chm = lasR::chm(res, tin = TRUE, ofile = file.path(odir, "chm", "*.tif") )
      if(!hasHAGinfo(normFiles)){
        message_log("Cannot create CHM because there is no 'HAG' (height above ground) attribute. Please run normalize first", isWarning = T )
        return(NULL)
      }

      if(!is.null(maxh)){
        if(is.na(as.numeric(maxh))){
          message_log( sprintf("maxh parameter (%s) should be numeric!", maxh), isWarning = T )
          return(NULL)
        }
        chm =  lasR::rasterize(res, operators = c("HAG_max"), filter = sprintf("HAG < %f", maxh)  )
      } else {
        chm =  lasR::rasterize(res, operators = c("HAG_max")  )
      }

      chm2 =  lasR::pit_fill(chm, ofile = file.path(odir, "chm", "*.tif") )

      if (file.exists(file.path(odir, "CHM.vrt")) ){

        vrt <- check_vrt_validity(file.path(odir, "CHM.vrt"))
        if(!inherits(vrt, "SpatRaster")) stop()

        if(!is.null(vrt)){

          if (ask_user(paste0("CHM with resolution of ", terra::res(vrt)[[1]]," m  exists, you want to overwrite?") )) {
            message_log("Adding creation of CHM to pipeline - if you want to force it just remove the files in CHM folder and the CHM.vrt file")
            pipeline <- pipeline  + chm + chm2
            outnames<- c(outnames ,"intermediateCHM", "chm")
            message_log("Adding pit-fill CHM to pipeline", verbose=verbose )
          } else {
            message_log("Skipping creation of CHM - if you want to force it just remove the files in CHM folder and the CHM.vrt file")
          }
        } else {
          pipeline <- pipeline + chm + chm2
          outnames<- c(outnames ,"intermediateCHM", "chm")
          message_log("Adding pit-fill CHM to pipeline", verbose=verbose )
        }

      } else {
        pipeline <- pipeline + chm + chm2
        outnames<- c(outnames , "intermediateCHM", "chm")
        message_log("Adding pit-filled-CHM to pipeline", verbose=verbose )
      }

    }


    ## Creating Boundary  ----
    if (!file.exists(file.path(odir, "boundaries.gpkg"))) {
      message_log("Creating boundary, might take some time")
      if(!exists("tri")){
        contour <- lasR::hulls(tri, file.path(odir, "boundaries.gpkg") )
      }
      pipeline <- pipeline + tri + contour
      outnames<- c(outnames , "hulls")
    }

    ## PROCESSING!!  ----
    # if(length(pipeline)==2){
    #   message_log("Nothing to create, all already processed.")
    #   return(NULL)
    # }

      message_log("Starting process on n.",
                  cli::style_bold(length(normFiles)),
                  " files, on a machine with n.",
                  cli::style_bold(lasR::half_cores()*2),
                  " cores, using  n.",
                  cli::style_bold(sc),
                  " concurrent files, on a raster with resolution of ",
                  cli::style_bold(res),
                  " m might take some time ...")

      # read <- lasR::reader()
      # tri <- lasR::triangulate(max(10, res*3), filter = lasR::keep_ground())
      # pipelineIn2 <- read + tri + dsm + chm


      # normFilesCtg <- lidR::catalog(normFiles)
#
#       download.file("https://www.cirgeo.unipd.it/shared/lazs.zip",
#                     destfile = "lazs.zip")
#       unzip("lazs.zip")
#       f <- list.files(pattern = "(?i)\\.la(s|z)$")
#       chm =  lasR::rasterize(2, operators = c("HAG_max"),
#                              ofile = file.path("*.tif") )
   pipeline <- lasR::reader() + chm
   ans <- lasR::exec(pipeline, on = ctg.norm   )
#
#         ansn <- lasR::exec(pipelinen,
#                           on = ctg.norm  )

# browser()

      if(is.list(ans)) {
        if(length(outnames)!=length(ans)){
          message_log("Problem with output of parallel execution: ", ans,
                      isWarning = T)
          browser()
          return(NULL)
        }
        names(ans) <- outnames

      }

      for(i in names(ans)){
        if(i=="hulls") {
          sf::write_sf(sf::st_union( ans$hulls),
                       file.path(odir, "boundaries.gpkg"), append = FALSE)

          message_log("Boundary file boundaries.gpkg in output directory: ",
                      cli::style_hyperlink(file.path(odir,"boundaries.gpkg"),
                                           paste0("file://",file.path(odir,"boundaries.gpkg") ) ) )
          next
        }
        if( length(ans[[i]]) > 1 &&
            length( unique( tools::file_ext(ans[[i]]) ) ) == 1 &&
            tolower( unique( tools::file_ext(ans[[i]]) ) )=="tif" ){
          oo <- file.path(odir, filename=sprintf("%s.vrt",toupper(i) ) )
          vrt <- terra::vrt(ans[[i]], oo, overwrite=T)
          message_log("Tiles ",toupper(i)," nella cartella '",i,"' e File ",toupper(i),".vrt creato nella cartella ",
                      cli::style_hyperlink(file.path(odir), paste0("file://",file.path(odir) ) ) )


          if(inherits(check_vrt_validity(oo), "SpatRaster") ){
            ovr <- c(2, 4, 8, 16, 32, 64)
            sf::gdal_addo(oo, overviews = ovr, read_only = TRUE)
          } else{
            message_log("File ", oo  ," is not valid raster! ",
                        isWarning = T)
          }

        }

      }

  # }
## applyPipelineFun end

  # ccc <- function(data )
  # {
  #   summary(data$HAG)
  #   browser()
  #   i <- data
  #   return(data)
  # }
  # call <- callback(ccc, expose = "E")


  # ans <- lasR::exec(pipeline, on = normFiles[2])

  message_log(  "finished!!!!!!!!!!!!!!!!!!!!!!!"  )
  return(NULL)

  ### nothing to do below here ---------
  # applyPipelineFun()

  message_log(  "Reading boundary"  )
  boundary <- sf::read_sf(file.path(odir, "boundaries.gpkg"))
  message_log("Getting values of centers of cells")
  # grid2 <- terra::disagg(grid, 3)


  return(NULL)

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



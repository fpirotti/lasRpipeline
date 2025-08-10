#' process
#'
#' @returns NA
#' @export
#'
#' @examples
#'  # use this to process example
process <- function() {
  sizeOfGrid <- 30
  f <- list.files(
    "/archivio/shared/geodati/las/fvg/tarvisio/",
    pattern = "(?i)\\.la(s|z)$",
    full.names = T
  )
  odir <- "/archivio/shared/geodati/las/fvg/tarvisiooutdir/norm"
  ctg <- lidR::catalog(f)
  if (hasGroundPoints(ctg) < 0.00000000001) {
    message_log("Please classify ground points with your favorite method before continuing. ")
    stop("No ground class")
  }

  # lasR::set_parallel_strategy(lasR::nested(ncores = 12L, ncores2 = 4L))
  message_log("Normalize")

  lasR::exec(lasR::write_lax(),
             on = ctg,
             with = list(
               progress = TRUE,
               ncores = min(
                 length(ctg@data$filename),
                 as.integer(lasR::half_cores() / 2)
               )
             ))

  normFiles <- lasRpipeline::normalize(ctg, odir)

  if (!file.exists(file.path(odir, "boundaries.gpkg"))) {
    message_log(
      "Boundary is strictly necessary because lasR crashes if plots are inside a tile but without any points inside! "
    )
    read <- reader()
    tri <- triangulate(20, filter = keep_ground())
    contour <- hulls(tri)
    pipeline <- read + tri + contour
    ans <- exec(pipeline, on = normFiles, with = list(ncores = 50))
    write_sf(sf::st_union(ans), file.path(odir, "boundaries.gpkg"))
  }
  boundary <- sf::st_buffer(sf::read_sf(file.path(odir, "boundaries.gpkg")), -0.6 *
                              sizeOfGrid)

  message_log("Reference GRID, might be different CRS. ")

  grid <- terra::rast("../wildfire/input/AT-IT_ScottBurganFuelMapClassV2.tif")
  tv <- terra::values(grid, mat = F)
  grid[tv < 100 | tv > 190] <- NA
  cellids <- terra::cells(grid)
  centers <- terra::xyFromCell(grid, cellids)

  gridOut <- terra::rast(grid, nlyrs = 3)
  gridOut[] <- NA
  names(gridOut) <- c("height", "slope", "aspect")
  points_sf <- sf::st_as_sf(as.data.frame(centers),
                            coords = c("x", "y"),
                            crs = terra::crs(grid))

  points_sf_crsPoints <- sf::st_transform(points_sf, lidR::crs(ctg))

  points_sf_crsPoints$cellID <- cellids

  ctg.norm <- lidR::catalog(normFiles)

  lasR::exec(lasR::write_lax(),
             on = normFiles,
             with = list(
               progress = TRUE,
               ncores = min(length(normFiles), as.integer(lasR::half_cores() /
                                                            2))
             ))

  dd <- sf::st_intersects(points_sf_crsPoints, boundary, sparse =  FALSE)

  points_sf_crsPoints_overlap <-  points_sf_crsPoints[dd[,1], ]

  points_sf_crsPoints_overlap_coords <- as.data.frame(sf::st_coordinates(points_sf_crsPoints_overlap))
  points_sf_crsPoints_overlap_coords$id <- points_sf_crsPoints_overlap$cellID
  ##STRATEGY FOR PARALLELIZE AS INJECTED R CODE (function fuelMetrics)
  ## will not be parallel in lasR - but we need to split the many cells not the
  ## lidar chunck so we do it ourselves

  chunkProcess <- function(points_sf_crsPoints_overlap_coords_chunk) {

    read <- lasR::reader_circles(
      points_sf_crsPoints_overlap_coords_chunk[, 1],
      points_sf_crsPoints_overlap_coords_chunk[, 2],
      sizeOfGrid / 2
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

    fres <-  parallel::mclapply(names(chunks[are.null]), mc.cores = 120, function(chunkID) {
      chunk <- chunks[[as.character(chunkID)]]
      res <- chunkProcess(chunk)
      # res2 <- tryCatch({
      #   do.call(rbind,res) },
      #          error = function(e) {
      #            res
      #          } )
      res
    })

    names(fres) <- names(chunks[are.null])
    are.null <-   sapply(fres, function(x) {
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
  points_sf_crsPoints_out$x <- cc[,1]
  points_sf_crsPoints_out$y <- cc[,2]

  gridOut[[1]][points_sf_crsPoints_overlap$cellID] <- cc[,1]
  gridOut[[2]][points_sf_crsPoints_overlap$cellID] <- cc[,2]
  gridOut[[3]][points_sf_crsPoints_overlap$cellID] <- fres3$V3
  writeRaster(gridOut, "gridOut.tif", overwrite=T)

}

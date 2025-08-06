#' Title
#'
#' @param dtm terra raster with DTM
#' @param dtm_cols colors - default is grDevices::terrain.colors(100)
#' @param main the title - default is ""
#'
#' @returns NULL
#' @export
#'
#' @examples
#' # none
plotShade <- function(dtm, dtm_cols=grDevices::terrain.colors(100), main = "DTM with Hillshade"){
  hillshade <- terra::shade(terra::terrain(dtm, "slope", unit="radians"),
                            terra::terrain(dtm, "aspect", unit="radians"),
                     angle = 45, direction = 315)
  # if(is.null(dtm_cols)) dtm_cols <- grDevices::terrain.colors(100)
  # if(is.null(main))  main = "DTM with Hillshade"
  dtm_cols_alpha <- grDevices::adjustcolor(dtm_cols, alpha.f = 0.5)  # alpha.f from 0 (transparent) to 1 (opaque)

  plot(hillshade, col = grDevices::gray.colors(100), main = main, legend=F)
  plot(dtm, col = dtm_cols_alpha, add = TRUE )
  NULL
}

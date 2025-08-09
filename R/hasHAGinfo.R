#' hasGroundPoints
#' @description
#' Will return TRUE if a sampled las file has Height Above Ground information,
#' which means it has been normalized already.
#'
#' @param f file name or character vector with file names to process
#' @param verbose if TRUE will provide verbose messages, if FALSE then it will be silent.
#' @returns FALSE if no classified ground points are present or
#' a numeric with the ratio of points classified as ground in the file
#' @export
#'
#' @examples
#' # testFile <- system.file("extdata", "BL5_UTM_32_ort_0021.laz", package = "lasRpipeline")
#' # hasGroundPoints(testFile)
hasHAGinfo <- function(f, verbose=T ){

  if(verbose) message_log("Starting function ", sys.call()[[1]] )
  if(inherits(f, "LAScatalog")){
    f <- f@data$filename
  }
  fileInfo <- lidR::readLASheader(f[[1]])
  !is.null(fileInfo$Extra_Bytes) &&
    is.element("HAG",
               names(fileInfo$Extra_Bytes$`Extra Bytes Description`))

}

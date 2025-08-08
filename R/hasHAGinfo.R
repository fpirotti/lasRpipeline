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
#' testFile <- system.file("extdata", "BL5_UTM_32_ort_0021.laz", package = "lasRpipeline")
#' hasGroundPoints(testFile)
hasHAGinfo <- function(f, verbose=T ){

  if(verbose) message_log("Starting function ", sys.call()[[1]] )

  fsz <- file.size(f)/1000000 ## file size in MB

  mm <- which(fsz > 1 )
  if(length(mm)==0){
    mm <- which.max(fsz)
  } else {
    mm <- which.min(mm)
  }

  file <- f[[mm]]
  id <- get_file_id(file)
  hasGround <- get_cache(id)
  if(force || is.null(hasGround)) {
    if(verbose) message_log("Getting information from a sample file to
 understand if ground class is available and also to check for CRS!" )
    hasGround = lasR::exec( lasR::summarise(), on = file,  progress = progress)
    set_cache(id, hasGround)
  } else {

  }
  if(verbose) message_log("Finished function ", sys.call()[[1]] )
  (hasGround$npoints_per_class[["2"]])/hasGround$npoints
}

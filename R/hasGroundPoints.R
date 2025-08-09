#' hasGroundPoints
#' @description
#' Will return FALSE if no ground class is present in one
#' of the files or number between 0 and 1 with ratio of ground points.
#' To make this faster some simple heuristics are used, such as
#' checking a single file as it is reasonable to assume that the files come
#' from the same processing pipeline.
#'
#' @param f file name or character vector with file names to process
#' @param verbose if TRUE will provide verbose messages, if FALSE then it will be silent.
#' @param progress if TRUE lasR will show progress.
#' @param force if TRUE cache will be ignored and overwritten - cache basically
#' keeps in memory the information of the file.
#'
#' @returns FALSE if no classified ground points are present or
#' a numeric with the ratio of points classified as ground in the file
#' @export
#'
#' @examples
#' # testFile <- system.file("extdata", "BL5_UTM_32_ort_0021.laz", package = "lasRpipeline")
#' # hasGroundPoints(testFile)
hasGroundPoints <- function(f, verbose=T,  progress = TRUE, force=FALSE){

  if(verbose) message_log("## Starting function ", cli::style_bold(sys.call()[[1]]) )
  if(inherits(f, "LAScatalog")){
    f <- f@data$filename
  }
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
    if(verbose) message_log("Getting information from a sample file to understand if ground class is available and also to check for CRS!" )
    hasGround = lasR::exec( lasR::summarise(), on = file,  progress = progress)
    set_cache(id, hasGround)
  }

  if(verbose) message_log("## Finished function ", cli::style_bold(sys.call()[[1]]) )
  (hasGround$npoints_per_class[["2"]])/hasGround$npoints
}

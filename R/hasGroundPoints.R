#' hasGroundPoints
#' @description
#' Will return TRUE if ground class is present in one
#' of the files or FALSE if no ground point are found. To make this faster
#' some euristics are used, such as check a single file which is the smallest
#' but  larger than 1 MB, or the largest file if none are larger than
#' 1 MB.
#'
#' @param f file name or character vector with file names to process
#' @param verbose if TRUE will provide verbose messages, if FALSE then it will be silent.
#'
#' @returns FALSE if no classified ground points are present or
#' a numeric with the ratio of points classified as ground in the file
#' @export
#'
#' @examples
#' testFile <- system.file("extdata", "BL5_UTM_32_ort_0021.laz", package = "lasRpipeline")
#' hasGroundPoints(testFile)
hasGroundPoints <- function(f, verbose=T){

  fsz <- file.size(f)/1000000 ## file size in MB
  mm <- which(fsz > 1 )
  if(length(mm)==0){
    mm <- which.max(fsz)
  } else {
    mm <- which.min(mm)
  }

  pipe <- lasR::reader(filter=lasR::keep_class(2)) + lasR::write_las( ofile = "tmp.las")
  ans = lasR::exec(pipe, on = f[[mm]],  progress = TRUE)
  suppressWarnings(file.remove("tmp.las"))
}

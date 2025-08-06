#' hasGroundPoints will return TRUE if ground class is present in one
#' of the files or FALSE if no ground point are found. To make this faster
#' some euristics are used, such as check a single file which is the smallest
#' but  larger than 1 MB, or the largest file if none are larger than
#' 1 MB
#'
#' @param f file name or character vector with file names to process
#'
#' @returns FALSE if no classified ground points are present or
#' a numeric with the ratio of points classified as ground in the file
#' @export
#'
#' @examples
#' # none
hasGroundPoints <- function(f){
  pipe <- lasR::reader(filter=lasR::keep_class(2)) + lasR::write_las( ofile = "tmp.las")
  ans = lasR::exec(pipe, on = f[[1]],  progress = TRUE)
  suppressWarnings(file.remove("tmp.las"))
}

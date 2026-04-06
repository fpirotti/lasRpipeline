#' checkVRTvalidity
#'
#' @param filevrt path to VRT raster file
#' @returns TRUE if all OK, FALSE otherwise
#'
#' @examples
#' #
checkVRTvalidity <- function(filevrt){

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
  return(TRUE)
}

#' auto_set_lasR_cores
#'
#' @param ctg - a LAScatalog
#' @param safety - percent of RAM you allow to be used (0.8 = 80%)
#' @param inflate - multiplier for memory inflation (metadata, temp objects, overhead)
#' @param verbose bolean default = TRUE - print lots of messages.

#' @export
#'
#' @returns    the number of safe cores
#' @examples
#' # NONE
auto_set_lasR_cores <- function(ctg, safety = 0.7, inflate = 3, verbose=FALSE ) {
  # ctg: a LAScatalog
  # safety: fraction of total RAM you allow to use (0.8 = 80%)
  # inflate: multiplier for LAS/LAZ expansion + overhead
  # progress: lasR progress bar
  files <- ctg@data$filename
  sizes <- file.info(files)$size

  if (length(sizes) == 0) stop("LAScatalog contains no file paths.")

  max_file <- max(sizes, na.rm = TRUE)
  est_ram_per_file <- max_file * inflate   # bytes

  if (.Platform$OS.type == "windows") {
    total_ram <- utils::memory.limit() * 1024^2
  } else {
    meminfo <- system("grep MemTotal /proc/meminfo | awk '{print $2}'",
                      intern = TRUE)
    total_ram <- as.numeric(meminfo) * 1024  # bytes
  }

  usable_ram <- total_ram * safety
  safe_cores <- floor(usable_ram / est_ram_per_file)
  safe_cores <- max(safe_cores, 1)

  message_log("  Available cores       : ",  lasR::ncores(), verbose = verbose)
  message_log("  Safe potential cores       : ", safe_cores, verbose = verbose)
  message_log("  RAM per file est : ", round(est_ram_per_file / 1024^2, 1), " MB", verbose = verbose)
  message_log("  Usable RAM (", safety*100, "%): ",
              round(usable_ram / 1024^3, 1), " GB", verbose = verbose)

  return(min(lasR::ncores(), safe_cores))
}


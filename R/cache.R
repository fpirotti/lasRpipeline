set_cache <- function(key, value) {
  assign(key, value, envir = .lasRpipeline_cache)
  saveRDS(as.list(.lasRpipeline_cache), file.path(cache_dir, "cache.rds"))
}

#' get_cache
#'
#' @param key character with key of cache
#'
#' @description
#' Return cache using key
#'
#' @returns value of  cached  key
#' @export
#'
#' @examples
#' get_cache("laspattern")
get_cache <- function(key) {
  if (exists(key, envir = .lasRpipeline_cache, inherits = FALSE)) {
    get(key, envir = .lasRpipeline_cache)
  } else {
    NULL
  }
}
get_cache_env <- function() {
  .lasRpipeline_cache
}

#' get_cache_env_ls
#' @description
#' Return some cached info related to point clouds so that they don't have to
#' be recalculated.
#'
#' @returns environment cache of package
#' @export
#'
#' @examples
#' get_cache_env_ls()
get_cache_env_ls <- function() {
  env <-  get_cache_env()
  ls(env)
}

cache_clear <- function() {
  rm(list = ls(envir = .lasRpipeline_cache), envir = .lasRpipeline_cache)
  unlink(file.path(cache_dir, "cache.rds"))
}

# get file id
get_file_id <- function(filepath) {
  if (!file.exists(filepath)) stop("File not found.")
  info <- file.info(filepath)
  id_string <- paste(filepath, info$size, info$mtime, sep = "_")
  digest::digest(id_string, algo = "sha1")
}

# will return list of names of files that were not processed
#' Title
#'
#' @param f original files, can be a lidR catalogue or a folder with LAS/LAZ or a vector of full paths to LAS/LAZ files
#' @param odir output directory where supposedly there are the processed files, e.g. for normalized files it could be "output/norm"
#' @param inverse return files that have been already processed instead of files to process
#'
#' @returns a vector with full paths to files that still have to be processed, or that have been processed (if inverse=T)
#'
#' @examples
#' # none
list_files_still_to_process <- function(f, odir, inverse=F) {
  if(inherits(f, "LAScatalog")){
    f <- f@data$filename
  }
  if( any(dir.exists(f)) ){
    f <- list.files(f, pattern= .lasRpipeline_cache$laspattern)
  }
  if(!dir.exists(odir)){
    message_log("Output directory ",odir, " does not exist yet, so no files were processed to this directory", TRUE)
    return(f)
  }

  f0 <- tools::file_path_sans_ext(basename(f))
  f1 <- tools::file_path_sans_ext(list.files(odir, pattern=.lasRpipeline_cache$laspattern))
  f2 <- tools::file_path_sans_ext(list.files(odir, pattern=.lasRpipeline_cache$tifpattern))

  processed <- (f0 %in% union(f2,f1))
  yesProcessed <- which(processed)
  notProcessed <- which(!processed)
  ret <-  f[notProcessed]
  if(inverse) ret <-  f[yesProcessed]
  ret

}

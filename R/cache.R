set_cache <- function(key, value) {
  assign(key, value, envir = .pkg_cache)
  saveRDS(as.list(.pkg_cache), file.path(cache_dir, "cache.rds"))
}

get_cache <- function(key) {
  if (exists(key, envir = .pkg_cache, inherits = FALSE)) {
    get(key, envir = .pkg_cache)
  } else {
    NULL
  }
}
get_cache_env <- function() {
  .pkg_cache
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
  env <- mypkg::get_cache_env()
  ls(env)
}

cache_clear <- function() {
  rm(list = ls(envir = .pkg_cache), envir = .pkg_cache)
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
list_files_not_processed <- function(f, odir) {
  if(inherits(f, "LAScatalog")){
    f <- f@data$filename
  }
  if( any(dir.exists(f)) ){
    f <- list.files(f, pattern= .pkg_cache$laspattern)
  }

  if(!dir.exists(odir)){
    message_log("Output directory ",odir, " does not exist, will create!", TRUE)
    dir.create(odir,recursive = T)
    return(f)
  }

  f0 <- tools::file_path_sans_ext(basename(f))
  f1 <- tools::file_path_sans_ext(list.files(odir, pattern=.pkg_cache$laspattern))
  f2 <- tools::file_path_sans_ext(list.files(odir, pattern=.pkg_cache$tifpattern))

  toProcess <- which(!(f0 %in% union(f2,f1)) )
  f[toProcess]
}

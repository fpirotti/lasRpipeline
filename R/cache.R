.my_cache_env <- new.env(parent = emptyenv())
laspattern <- "(?i)\\.la(s|z)$"
tifpattern <- "(?i)\\.tif$"
odir <-   "output"
# Set cache
set_cache <- function(key, value) {
  assign(key, value, envir = .my_cache_env)
}

# Get cache
get_cache <- function(key) {
  if (exists(key, envir = .my_cache_env)) {
    get(key, envir = .my_cache_env)
  } else {
    NULL
  }
}

# Clear cache
clear_cache <- function() {
  rm(list = ls(envir = .my_cache_env), envir = .my_cache_env)
}



# get file id
get_file_id <- function(filepath) {
  if (!file.exists(filepath)) stop("File not found.")

  info <- file.info(filepath)
  id_string <- paste(filepath, info$size, info$mtime, sep = "_")
  digest::digest(id_string, algo = "sha1")
}

# will return list of names of files that were not processed
check_files_processed <- function(f, odir) {

  if( any(dir.exists(f)) ){
    f <- list.files(f, pattern=laspattern)
  }

  if(!dir.exists(odir)){
    message_log("Output directory ",odir, " does not exist, will create!", TRUE)
    dir.create(odir,recursive = T)
    return(f)
  }

  f1 <- tools::file_path_sans_ext(list.files(odir, pattern=laspattern))
  f2 <- tools::file_path_sans_ext(list.files(odir, pattern=tifpattern))

  setdiff(f, union(f2,f1) )

}

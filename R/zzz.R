.lasRpipeline_cache <- new.env(parent = emptyenv())
cache_dir <- tools::R_user_dir("lasRpipeline", which = "cache")

.onLoad <- function(libname, pkgname) {
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  cache_file <- file.path(cache_dir, "cache.rds")

  packageStartupMessage("\U0001F6A9 Verifica sistema operativo" )
  cores <- parallel::detectCores(logical = TRUE)
  packageStartupMessage("\U0001F6A9  CPU threads: ", cores)


  if (!requireNamespace("lasR", quietly = TRUE)) {
    packageStartupMessage(
      "\u2705 Package lasR non trovato, lo installo!"
    )
    utils::install.packages("lasR")
  }

  assign("laspattern", "(?i)\\.la(s|z)$", envir = .lasRpipeline_cache)
  assign("tifpattern", "(?i)\\.tif$", envir = .lasRpipeline_cache)
  assign("odir", "output", envir = .lasRpipeline_cache)

  if (file.exists(cache_file)) {
    cache_data <- readRDS(cache_file)
    list2env(cache_data, envir = .lasRpipeline_cache)
  }
}

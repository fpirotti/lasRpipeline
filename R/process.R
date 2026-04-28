# Authors: Francesco Pirotti, Shunjia Li
#
# START =====
# Forest Fire Metrics from LiDAR Point Cloud → 20m Raster Output
# Metrics: height, slope, aspect, canopyCover, canopyHeight, CBH, CBD
# Method: Beer-Lambert / PAD vertical profile (Martin-Ducup et al. 2025)

pkgs <- c("terra", "stars", "ggplot2", "lidR", "sf", "cli",   "tools", "future", "data.table", "this.path")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    warning("Package \033[1m", p, "\033[0m not found... installing it.")
    install.packages(p)
  }
  message("Package \033[1m", p, "\033[0m  found... loading it.")
  library(p, character.only = TRUE, quietly = TRUE)
}

getPixelArea <- function(x){
  pixel_area <- abs(res(x)[[1]] * res(x)[[2]])
  pixel_area
}
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

# 0. PARAMETERS ########

output_dir   <- "output_rasters"

dir.create(output_dir, showWarnings = FALSE)

res          <- 10        # raster resolution (m) - strongly advised to be
# between 1 and 30 m depending on lidar point spacing
resDTM       <- 1         # resolution for terrain and for base for metrics
layer_depth  <- 0.5       # vertical slice thickness (m)
G_theta      <- 0.5       # leaf projection ratio (spherical assumption)
omega        <- 0.77      # clumping index (Martin-Ducup et al. 2025)
FMA          <- 0.25      # Fuel Mass Area kg/m²
BD_threshold <- 0.02      # bulk density threshold for CBH (kg/m³)

# 1. FUNCTION: compute canopy metrics for a single  pixel  ########
compute_pixel_metrics <- function(X,Y,Z) {
  z <- Z
  nn <- floor(log10(length(z)))
  if (nn <2) q = 0.90
  if (nn==2) q = 0.95
  if (nn==3) q = 0.99
  if (nn==4) q = 0.999
  if (nn >4) q = 0.9995

  canopy_height <- as.numeric(quantile(z, q))
  high <- which(z> canopy_height+0.5 )
  if(length(high)>0){
    X = X[-high]
    Y = Y[-high]
    Z = Z[-high]
    z <- Z
  }
  ## 1x1 cubes
  # lastmp <- suppressMessages( lidR::LAS(data.frame(X=X,Y=Y,Z=Z)) )
  # vox_met <- voxel_metrics(lastmp, ~list(N = length(Z)), 1)
  dt <- data.table::data.table(X=as.integer(X),
                               Y=as.integer(Y),
                               Z=as.integer(Z) )

  vox_met <- dt[, .N, by = .(X,Y,Z)]

  totArea <- nrow(unique(vox_met[,c("X","Y")]))
  totAreaWithCanopy <- nrow(unique(vox_met[vox_met$Z>2,c("X","Y")]))
  # zVox <- vox_met[vox_met$Z>=2,c("Z")][[1]]
  # write.csv(vox_met, "voxmet.xyz", row.names = F)
  #  plot(vox_met, color="N",  opacity=0.5, pal = colors,   voxel = TRUE)
  canopy_cover  <- totAreaWithCanopy / totArea * 100

  CBH <- NA_real_
  CBD <- NA_real_

  if (length(z) >= 10 && ceiling(max(z)) >= 2) {

    max_h    <- ceiling(max(z))
    # breaks   <- seq(0, max_h, by = layer_depth)
    # Change to slicing starting from 1m, completely skipping the near-surface layer
    breaks <- seq(1, max_h, by = layer_depth)
    n_layers <- length(breaks) - 1

    if (n_layers >= 3) {
      layer_ids   <- cut(z, breaks = breaks, labels = FALSE, include.lowest = TRUE)
      N_per_layer <- tabulate(layer_ids, nbins = n_layers)
      N_cum <- cumsum(N_per_layer)
      N_cum[N_cum == 0] <- NA
      NRD         <- N_per_layer / N_cum
      NRD[is.na(NRD)] <- 0

      Gf  <- pmax(1 - NRD, 1e-6)
      Gf  <- pmin(Gf, 1 - 1e-6)
      PAD <- -log(Gf) / (G_theta * omega * layer_depth)
      BD  <- PAD * FMA

      heights       <- breaks[-length(breaks)] + layer_depth / 2

      above1m       <- heights >= 1.0
      BD_above      <- BD[above1m]
      heights_above <- heights[above1m]
      canopy        <- heights_above[BD_above >= BD_threshold]

      if (length(canopy) > 0) {
        ## @Shujia have to improve here, we are just taking the min value,
        # CBH <- min(canopy)
        CBH <- min(canopy)
        #CBD <- max(BD_above[BD_above >= BD_threshold])
        canopy_idx <- which(heights_above >= CBH)
        if (length(canopy_idx) > 0) {
          CFL <- sum(BD_above[canopy_idx]) * layer_depth
          crown_length <- canopy_height - CBH
          if (crown_length > 0) {
            CBD <- CFL / crown_length
          }
        }
      }
    }
  }

  return(list(
    canopyCover  = canopy_cover,
    canopyHeight = canopy_height,
    CBH          = CBH,
    CBD          = CBD
  ))
}

# 2. FUNCTION: process one plot folder → one .tif with 7 bands ----

#' process_plot
#'
#' @param las_folder the folder with
#' @param template a raster defining the output  grid or NULL or FALSE
#' @param force re-computes all intermediate steps!
#'
#' @returns a DTM in tiles with a VRT file, normalized LAZ files and metrics
#' @export
#'
#' @examples
process_plot <- function(las_folder, template=NULL, force=FALSE) {


  if(!exists("res")) {
    stop("Setup parameters in head of script")
  }
  plot_name <- basename(las_folder)
  out_file  <- file.path(output_dir, paste0(plot_name, "_metrics.tif"))
  tmp_Folder  <- file.path(las_folder, "tmp")
  tmpFolders <- list(
    tmp_Folder_DTMs  = file.path(tmp_Folder, "DTMs"),
    tmp_Folder_Norm  = file.path(tmp_Folder, "NormLAS"),
    tmp_Folder_Metrics  = file.path(tmp_Folder, "Metrics")
  )

  for(fold in names(tmpFolders)){
    message("Checking for existance of ",  tmpFolders[[fold]] )
    if (!dir.exists(tmpFolders[[fold]])) {
      dir.create(tmpFolders[[fold]],mode = "777", recursive = T, showWarnings = FALSE)
    } else {
      if(force){
        message("Removing ", fold)
        unlink(list.files(tmpFolders[[fold]], full.names = TRUE) )
      }
    }
  }

  if (file.exists(out_file) && !force) {
    message( "\033[32mSkipping (already done): ", plot_name, "
...results available in:
", file.path(getwd(), out_file) , ".
Remove or use force=TRUE if you want to recalculate.\033[0m")
    return(invisible(NULL))
  }

  message("Processing: ", plot_name)

  message("READ CATALOG: ")

  # --- Read catalog ----
  ctg_local <- tryCatch(
    readLAScatalog(las_folder),
    error = function(e) { message("  Error reading catalog: ", e$message); return(NULL) }
  )
  if (is.null(ctg_local)) return(invisible(NULL))

  if (isTRUE(st_is_longlat(st_crs(ctg_local)))) {
    message("LAS is in geographic coordinates (lat/long) — not suitable - please provide projected points (work in progress for automatic conversion).")
    return(invisible(NULL))
  }


  cores <- min(100, ceiling(abs(future::availableCores()/2-1)), nrow(ctg_local@data))

  message("Running on ", Sys.info()["sysname"])
  if (supportsMulticore()) {
    message("Using MultiCore as it is supported ")
    plan(multicore, workers = cores)
  } else {
    message("Using multisession not using multiCore -
\033[1;31mif on linux and you want to use multiCore then make sure you don't run from RStudio\033[0m")
    plan(multisession, workers = cores)
  }

  message("resDTM * 2 buffer: ", resDTM*2)
  opt_chunk_buffer(ctg_local) <- resDTM*2

  # DTM ----
  message("Computing DTM...")
  dtm <- file.path(tmpFolders$tmp_Folder_DTMs, "rasterize_terrain.vrt")
  if(!file.exists(dtm)) {
    message("DTM NOT found - creating ...")
    opt_output_files(ctg_local) <- paste0(tmpFolders$tmp_Folder_DTMs, "/z{*}_DTM")
    dtm <- tryCatch(
      rasterize_terrain(ctg_local, res = resDTM, algorithm =  tin()),
      error = function(e) { message("  DTM create error: ", e$message); return(NULL) }
    )
     # terra::vrt(list.files(tmpFolders$tmp_Folder_DTMs, full.names = T),
     #            filename=dtm)
    dtm <- file.path(tmpFolders$tmp_Folder_DTMs, "rasterize_terrain.vrt")
  } else {
    message("##### DTM already available! ...")
  }

  if(!checkVRTvalidity(dtm)){
    stop()
  }
  dtm <-   stars::read_stars(dtm)
  # --- Height normalization ---
  message("Normalize heights...")
  opt_output_files(ctg_local) <- paste0(tmpFolders$tmp_Folder_Norm, "/{*}_Norm")
  las <- tryCatch(
    suppressWarnings(lidR::catalog( tmpFolders$tmp_Folder_Norm )),
    warning = function(e) { message("Cannot read normalized folder: ", e$message); return(NULL) },
    error = function(e) { message("Cannot read normalized folder: ", e$message); return(NULL) }
  )

  # NORMALIZE -----
  if (is.null(las) || nrow(ctg_local@data)!=nrow(las@data) ) {
    message("Normalize start...")
    opt_laz_compression(ctg_local) <- TRUE
    las <- tryCatch(
      normalize_height(ctg_local, algorithm = tin(),
                       dtm = dtm), #algorithm = knnidw() ),
      error = function(e) { message("  Normalization error: ", e$message); return(NULL) }
    )
    if (is.null(las)) {
      plan(sequential)
      return(invisible(NULL))
    }
  } else {
    message("#### Normalization already available! ...")
  }

  # --- Canopy metrics at 20m ----

  metrics <- list.files(pattern=".*\\.vrt$", tmpFolders$tmp_Folder_Metrics, full.names = T)
  if(length(metrics)==0) {
    message("Metrics NOT found - creating ...")
    opt_output_files(las) <- paste0( tmpFolders$tmp_Folder_Metrics , "/z{*}_Met")
    filter <- "-drop_z_below 0 -drop_z_above 60 -keep_class 2 3 4 5 9 11"
    opt_filter(las) <- filter
    if(is.null(template) || template==FALSE){
      metrics      <- pixel_metrics(las, ~compute_pixel_metrics(X,Y,Z),  res = res)
    } else{
      ## VERY IMPORTANT -
      # IF YOU PROVIDE A RASTER TEMPLATE, IT FORCES TO CALCULATE CANOPY FUELS OVER
      # EACH PIXEL.
      pathtemplate <- template
      template <- terra::rast(template)
      message("Converting to points, might take a while...")

      extsLAS <- lidR::ext(las) |>  terra::vect() |> st_as_sf() |> sf::st_set_crs(lidR::crs(las))
      crsLAS <- st_crs(extsLAS, parameters=T)
      extsLAS <-  extsLAS |>  sf::st_transform(sf::st_crs(template))
      extsRast <- terra::ext(template) |>  terra::vect() |> st_as_sf() |> sf::st_set_crs( sf::st_crs(template))
      extsLAS$name <- sprintf("LASCatalog CRS %s", crsLAS[["srid"]])
      extsRast$name <- sprintf("raster CRS %s", sf::st_crs(extsRast, parameters=T)[["srid"]])

      png("extents.png")
      p<-ggplot(data = rbind(extsLAS, extsRast)) +
        geom_sf(aes(color =name), fill = NA, linewidth = 1) +
        theme_light() +
        labs(color = "Class Borders")
      print(p)
      dev.off()

      pp <- file.path(getwd(), "extents.png")
       cli_inform(
        col_green(" ### plotted extents to file you can see it in {.run [{pp}](utils::browseURL('{pp}'))}")
        )


       if( st_area(extsRast) > st_area(extsLAS) ) {
         clipped <- sprintf("%s_clipped.tif", file.path(
           dirname(pathtemplate),
            tools::file_path_sans_ext(
              basename(pathtemplate)    )
                            ) )
         cli_inform("Area of raster bigger so we clip it - you can find clipped version in {.run [{clipped}](utils::browseURL('{clipped}'))} ")
         template  <- terra::crop(template, extsLAS)
         writeRaster(template, clipped, overwrite=T )
         plot(template )
       }

      p_sfs <- terra::as.data.frame( template, cells=TRUE, xy=T, na.rm=T)
      p_sf_in <- sf::st_as_sf(p_sfs[, c("x","y", "cell")], coords=c("x","y"),
                           crs=sf::st_crs(template) )

      message(nrow(p_sf_in), " points    overlap extents of raster." )
      if(st_crs(p_sf_in) != st_crs(las)){
         p_sf_in <- p_sf_in |> sf::st_transform(st_crs(las))
      }

      opt_output_files(las) <- ""
      message("## STARTING FUEL METRICS " )
      metrics      <- lidR::plot_metrics(las, ~compute_pixel_metrics(X,Y,Z),
                                          geometry=p_sf_in,
                                          radius= (getPixelArea(template)/pi)^0.5 )

      browser()
    }
  } else {
    metrics <- list.files(pattern=".*\\.vrt$", tmpFolders$tmp_Folder_Metrics, full.names = T)
    message("Metrics found...")
  }

  if(!checkVRTvalidity(metrics)){
    plan(sequential)
    stop()
  }


  ## convert metrics result to terra raster
  metrics_rast <- terra::rast(metrics)

  # --- Assemble all bands ---
  if (!is.null(dtm)) {
    slope_r  <- terrain(terra::rast(dtm),  v = "slope",  unit = "degrees")
    aspect_r <- terrain(terra::rast(dtm),  v = "aspect", unit = "degrees")

    dtm_r    <- resample(terra::rast(dtm),      metrics_rast, method = "bilinear")
    slope_r  <- resample(slope_r,  metrics_rast, method = "bilinear")
    aspect_r <- resample(aspect_r, metrics_rast, method = "bilinear")

    out_rast        <- c(dtm_r, slope_r, aspect_r, metrics_rast)
    names(out_rast) <- c("height", "slope", "aspect",
                         "canopyCover", "canopyHeight", "CBH", "CBD")
  } else {
    message("  DTM unavailable, saving 4 canopy bands only...")
    out_rast        <- metrics_rast
    names(out_rast) <- c("canopyCover", "canopyHeight", "CBH", "CBD")
  }

  message("  Bands: ", nlyr(out_rast), " → ", paste(names(out_rast), collapse = ", "))
  writeRaster(out_rast, out_file, overwrite = TRUE)
  message( "\033[32mSaved: ", file.path(getwd(), out_file) , "\033[0m")
  plan(sequential)
  return(NULL)
}

# 3. RUN -----

#
las_folder <- "/archivio/shared/geodati/las/fvg/tarvisio/"
wd <- getwd()
setwd(las_folder)
## this below in case we have a raster to use as template
## NB CRS should be the same as LAS!
template <- "/archivio/shared/geodati/raster/wildfire/pilotRegions_AT-IT-SI_canopy_canopyBaseHeight.tif"
process_plot("/archivio/shared/geodati/las/fvg/tarvisio/", template = template,force = F)
setwd(wd)


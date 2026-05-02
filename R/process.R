# Authors: Francesco Pirotti, Shunjia Li
#
# START =====
# Forest Fire Metrics from LiDAR Point Cloud → 20m Raster Output
# Metrics: height, slope, aspect, canopyCover, canopyHeight, CBH, CBD
# Method: Beer-Lambert / PAD vertical profile (Martin-Ducup et al. 2025)

pkgs <- c("terra", "stars", "patchwork", "tidyterra", "ggplot2", "lidR", "sf", "cli", "parallel",  "tools", "future", "data.table", "this.path")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    warning("Package \033[1m", p, "\033[0m not found... installing it.")
    install.packages(p)
  }
  message("Package \033[1m", p, "\033[0m  found... loading it.")
  library(p, character.only = TRUE, quietly = TRUE)
}


log_file <- "parallel_log.txt"

plot_vertical_profile <- function(X,Y,Z,
                                  CBH, CBD, CC,ladder_fuel_ratio, crr, z_skewness,
                                  binwidth = 1) {
  suppressMessages({
    suppressWarnings({
     library(ggplot2)
     library(tidyterra)
     library(patchwork)
   })
  })

  # Remove NAs
  Z_clean <- Z[!is.na(Z)]
  df <- data.frame(Z = Z_clean)

  p1 <- ggplot(df, aes(x = Z)) +
    geom_histogram(binwidth = binwidth,
                   fill = "seagreen4",
                   color = "white",
                   alpha = 0.8) +
    geom_vline(xintercept = CBH, color = "red", linetype = "dashed", size = 1) +
    coord_flip() +  # Flips the axes so Z is vertical
    labs(title = "A. Vertical Canopy Profile",
         subtitle = "Point density distribution by height",
         x = "Height Above Ground (Z)",
         y = "Number of Returns") +
    theme_classic(base_size = 14) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold")
    )

  rr <- range(Y)
  dd <- diff(rr)

  slice <- X> (mean(X)-2) & X<(mean(X)+2)

  p2 <- ggplot() +
    stat_summary_2d(aes(x = X, y = Y, z = Z), fun = max, bins = 60) +
    scale_fill_viridis_c(
      option = "viridis",
      name = "Canopy\nHeight (m)",
      na.value = "transparent",      # Makes the background/ground invisible
      limits = c(5, max(Z)), # Ignores values from 0 to 5m
      labels = function(x) paste0(x, "m") # Adds 'm' suffix to legend numbers
    ) +

    coord_fixed() +
    theme_classic(base_size = 14) +
    labs(title = "B. Canopy Height > 5 m", subtitle = "Copernicus HRL Forest layers Definition",
         x = "Coordinate X (m)", y = "Coordinate Y  (m)") +
  theme(plot.title = element_text(face = "bold"))

  p3 <- ggplot(  ) +
    # Using ti  ny points and extreme transparency simulates density intuitively
    geom_point(aes(x = X, y = Z), alpha = 0.05, size = 0.3, color = "darkgreen") +
    geom_hline(yintercept = CBH, color = "red", linetype = "dashed", size = 1) +
    scale_y_continuous(expand = c(0, 0)) + # Align ground to axis
    labs(title = "C. Stand Side Profile", subtitle = "X vs Z Lidar Echoes",
         x = "Coordinate X (m)", y = "Height (m)") +
    theme_classic(base_size = 14) +
    theme(plot.title = element_text(face = "bold"))

  p4 <- ggplot(  ) +
    # Using tiny points and extreme transparency simulates density intuitively
    geom_point(aes(x =Y, y = Z), alpha = 0.05, size = 0.3, color = "darkgreen") +
    geom_hline(yintercept = CBH, color = "red", linetype = "dashed", size = 1) +
    scale_y_continuous(expand = c(0, 0)) + # Align ground to axis
    labs(title = "D. Stand Side Profile", subtitle = "Y vs Z Lidar Echoes",
         x = "Coordinate Y (m)", y = "Height (m)") +
    theme_classic(base_size = 14) +
    theme(plot.title = element_text(face = "bold"))

  final_dashboard <- (p1 + p2) / (p3 + p4) +
    plot_annotation(
      title = "Forest Stand Metrics Summary",
      subtitle = sprintf("CBH=%.2f CBD=%.4f CC=%.0f%% Ladder Fuel Ratio=%.3f Canopy Relief Ratio=%.3f Vertical Skewness=%.3f", CBH, CBD, CC, ladder_fuel_ratio, crr, z_skewness),
      theme = theme(plot.title = element_text(size = 20, face = "bold"))
    )

  ggsave(file.path(plotdir, sprintf("metricsPlot_%.0f_%.0f_v3.png",   mean(X), mean(Y) ) ), final_dashboard,
         width = 12, height = 12, dpi = 200)
  # --- Export as high-res PNG ---


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
layer_depth  <- 1.0       # vertical slice thickness (m)
G_theta      <- 0.5       # leaf projection ratio (spherical assumption)
omega        <- 0.77      # clumping index (Martin-Ducup et al. 2025)
FMA          <- 0.25      # Fuel Mass Area kg/m²
BD_threshold <- 0.02      # bulk density threshold for CBH (kg/m³)

# A. FUNCTION: compute canopy metrics for a single  pixel  ########
plotCellMetrics <- TRUE
global.count <- 0
compute_pixel_metrics <- function(X, Y, Z,
                                  layer_depth = 1.0,
                                  G_theta = 0.5,
                                  omega = 0.77,
                                  FMA = 0.25,
                                  BD_threshold = 0.02 ) { # Added plotdir to arguments

  # 0. Initialize Output Metrics ----
  CBH <- NA_real_
  CBD <- NA_real_
  ladder_fuel_ratio <- NA_real_
  crr <- NA_real_
  z_skewness <- NA_real_

  # 1. Handle NAs and Empty Data -----
  valid_idx <- !is.na(X) & !is.na(Y) & !is.na(Z)
  if (!all(valid_idx)) {
    X <- X[valid_idx]
    Y <- Y[valid_idx]
    Z <- Z[valid_idx]
  }

  n_pts <- length(Z)
  if (n_pts == 0) {
    return(list(canopyCover = NA_real_, canopyHeight = NA_real_,
                CBH = NA_real_, CBD = NA_real_, ladderFuelRatio = NA_real_,
                CRR = NA_real_, z_skewness = NA_real_))
  }

  # 2. Dynamic Quantile Selection (Removes outliers/birds) ----
  # Adjusts the percentile threshold based on the density of the point cloud
  nn <- floor(log10(n_pts))
  q <- if (nn < 2) 0.90 else if (nn == 2) 0.95 else if (nn == 3) 0.99 else if (nn == 4) 0.999 else 0.9995

  canopy_height <- as.numeric(quantile(Z, probs = q, na.rm = TRUE))

  # 3. Filter Noise & Compute Basic Structural Metrics ----
  # Keep only points below the dynamically calculated canopy height + 0.5m buffer
  keep_idx <- Z <= (canopy_height + 0.5)
  if (!all(keep_idx)) {
    X <- X[keep_idx]
    Y <- Y[keep_idx]
    Z <- Z[keep_idx]
  }

  # A. Ladder Fuel Ratio (Proportion of points between 1m and 4m)
  pts_above_1m <- sum(Z > 1.0, na.rm = TRUE)
  if (pts_above_1m > 10) {
    ladder_fuel_ratio <- sum(Z > 1.0 & Z <= 4.0, na.rm = TRUE) / pts_above_1m
  } else {
    ladder_fuel_ratio <- 0
  }

  # B. Canopy Relief Ratio (CRR) and Z-Skewness (Focusing on vegetation > 2m)
  Z_veg <- Z[Z > 2.0]
  n_veg <- length(Z_veg)

  if (n_veg > 5) {
    min_z  <- min(Z_veg, na.rm = TRUE)
    max_z  <- max(Z_veg, na.rm = TRUE)
    mean_z <- mean(Z_veg, na.rm = TRUE)

    # CRR: Canopy "Fullness" metric (range 0-1)
    if (max_z > min_z) {
      crr <- (mean_z - min_z) / (max_z - min_z)
    }

    # Z-Skewness: Vertical distribution shape (Positive = bottom heavy, Negative = top heavy)
    sd_z <- sd(Z_veg, na.rm = TRUE)
    if (!is.na(sd_z) && sd_z > 0) {
      z_skewness <- (sum((Z_veg - mean_z)^3) / n_veg) / (sd_z^3)
    }
  }

  # 4. Fast Area and Canopy Cover Computation ----
  # Using integer coercion to grid points into 1x1m cells for area/cover math
  dt <- data.table::data.table(
    X = as.integer(X),
    Y = as.integer(Y),
    Z = as.integer(Z)
  )

  # Fraction of canopy points (Z > 5m) per 1x1m cell
  cell_metrics <- dt[, .(cover_frac = sum(Z > 5) / .N), by = .(X, Y)]
  canopy_cover <- if (nrow(cell_metrics) > 0) mean(cell_metrics$cover_frac) * 100 else 0

  # 5. Profile Metrics & CBH/CBD Calculation ------
  max_z_val <- max(Z, na.rm = TRUE)

  if (length(Z) >= 10 && ceiling(max_z_val) >= 5) {
    max_h <- ceiling(max_z_val)
    breaks <- seq(0, max_h, by = layer_depth)
    n_layers <- length(breaks) - 1

    # Bin points into vertical layers
    layer_ids <- cut(Z, breaks = breaks, labels = FALSE, include.lowest = TRUE)
    N_per_layer <- tabulate(layer_ids, nbins = n_layers)
    heights <- breaks[-length(breaks)] + (layer_depth / 2)

    ## A. Canopy Base Height (CBH) Logic ----
    valid_layers <- heights >= 1.0
    N_sub <- N_per_layer[valid_layers]
    H_sub <- heights[valid_layers]

    if (length(N_sub) > 0 && any(!is.na(N_sub))) {
      peak_val <- max(N_sub, na.rm = TRUE)

      if (peak_val > 0) {
        target_val <- 0.15 * peak_val
        cbh_idx <- which(N_sub >= target_val)[1]

        if (!is.na(cbh_idx)) {
          # Interpolate fine-scale CBH
          if (cbh_idx > 1) {
            count_prev <- N_sub[cbh_idx - 1]
            count_curr <- N_sub[cbh_idx]
            h_prev     <- H_sub[cbh_idx - 1]
            h_curr     <- H_sub[cbh_idx]

            CBH <- h_prev + (h_curr - h_prev) * ((target_val - count_prev) / (count_curr - count_prev))
          } else {
            # Fallback if triggered on the lowest layer
            CBH <- H_sub[cbh_idx] - (layer_depth / 4)
          }
        }
      }
    }

    ## B. Canopy Bulk Density (CBD) Logic (IMPROVED) ----
    # Top-down cumulative sum of points (how many pulses successfully reach this layer)
    N_cum <- rev(cumsum(rev(N_per_layer)))

    # Calculate NRD avoiding division by 0.
    # FIX: Divide by N_cum (pulses entering the layer), NOT N_total.
    # This accounts for occlusion and prevents severe underestimation of lower-canopy density.
    NRD <- numeric(length(N_per_layer))
    safe_div <- N_cum > 0
    NRD[safe_div] <- N_per_layer[safe_div] / N_cum[safe_div]
    NRD[is.na(NRD) | is.nan(NRD)] <- 0

    # Beer-Lambert transformation to Plant Area Density (PAD) and Bulk Density (BD)
    Gf  <- pmin(pmax(1 - NRD, 1e-6), 1 - 1e-6) # Clamp to avoid log(0)
    PAD <- -log(Gf) / (G_theta * omega * layer_depth)
    BD  <- PAD * FMA

    # Calculate MEAN CBD strictly within the active crown
    if (!is.na(CBH) && !is.na(canopy_height)) {
      crown_mask <- heights >= CBH & heights <= canopy_height
      crown_BD <- BD[crown_mask]

      if (length(crown_BD) > 0 && any(!is.na(crown_BD))) {
        calculated_mean <- mean(crown_BD, na.rm = TRUE)
        if (!is.nan(calculated_mean) && is.finite(calculated_mean)) {
          CBD <- calculated_mean
        }
      }
    }
  }

  # 6. Optional Plotting ------
  if (!is.null(plotdir) && max_z_val > 30) {
    current_plots <- list.files(plotdir, pattern = "*.png")
    if (length(current_plots) < 300) {
      plot_vertical_profile(X, Y, Z, CBH, CBD, canopy_cover,
                            ladder_fuel_ratio, crr, z_skewness)
    }
  }

  # 7. Final Return ------
  return(list(
    canopyCover     = canopy_cover,
    canopyHeight    = canopy_height,
    CBH             = CBH,
    CBD             = CBD,
    ladderFuelRatio = ladder_fuel_ratio,
    CRR             = crr,
    z_skewness      = z_skewness
  ))
}


# B. FUNCTION: process one plot folder → one .tif with 7 bands ----

#' process_plot
#'
#' @param las_folder the folder with
#' @param template a raster defining the output  grid or NULL or FALSE
#' @param force re-computes all intermediate steps!
#' @param plotOut create some nice plots
#'
#' @returns a DTM in tiles with a VRT file, normalized LAZ files and metrics
#' @export
#'
#' @examples
process_plot <- function(las_folder, template=NULL, force=FALSE, plotOut=FALSE) {

  pathtemplate <- template
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

#   if (file.exists(out_file) && !force) {
#     message( "\033[32mSkipping (already done): ", plot_name, "
# ...results available in:
# ", file.path(getwd(), out_file) , ".
# Remove or use force=TRUE if you want to recalculate.\033[0m")
#     return(invisible(NULL))
#   }

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


  cores <- min(80, ceiling(abs(future::availableCores()/2-1)), nrow(ctg_local@data))

  message("Running on ", Sys.info()["sysname"])
  if (supportsMulticore()) {
    message("\033[1mUsing MultiCore as it is supported! \033[0m ")
    plan(multicore, workers = cores)
  } else {
    message("Using multisession not using multiCore -
\033[1;31mif on linux and you want to use multiCore then make sure you don't run from RStudio but from Rscript\033[0m")
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
      return(invisible(NULL))
    }
    message("#### adding index ...")
    gg <- mclapply(las@data$filename, function(f) {
      rlas::writelax(f)
    }, mc.cores =  cores)
  } else {
    message("#### Normalization already available! ...")
  }

  # --- Canopy metrics at 20m ----
  # writeLines("Log started...", log_file)

  metrics <- list.files(pattern=".*\\.vrt$", tmpFolders$tmp_Folder_Metrics, full.names = T)

  if(length(metrics)==0) {
    message("Metrics NOT found - creating ...")
    filter <- "-drop_z_below 0 -drop_z_above 60 -keep_class 2 3 4 5 9 11"
    opt_filter(las) <- filter
    opt_output_files(las) <- paste0( tmpFolders$tmp_Folder_Metrics , "/z{*}_Met")


    if(is.null(template) || template==FALSE){
      metrics      <- pixel_metrics(las, ~compute_pixel_metrics(X,Y,Z),  res = res)
    } else {

      template <- terra::rast(template)
      extsLASori <- lidR::ext(las) |>  terra::vect() |> st_as_sf() |> sf::st_set_crs(lidR::crs(las))
      crsLAS <- st_crs(extsLASori, parameters=T)
      extsLAS <-  extsLASori |>  sf::st_transform(sf::st_crs(template))
      extsRast <- terra::ext(template) |>  terra::vect() |> st_as_sf() |> sf::st_set_crs( sf::st_crs(template))
      extsLAS$name <- sprintf("LAS Files \nCRS %s", crsLAS[["srid"]])
      extsRast$name <- sprintf("Template raster \nCRS %s", sf::st_crs(extsRast, parameters=T)[["srid"]])
      # browser()
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
         # writeRaster(template, clipped, overwrite=T )
         # plot(template )
       }

      template <- terra::project(template, crsLAS$Wkt)
      opt_select(las)      <- "xyz"
      # opt_chunk_size(las)   <- 0
      # opt_chunk_buffer(las) <- 0
      opt_progress(las)     <- TRUE # Watch the progress bar jump!

      message("## STARTING FUEL METRICS " )
      p1 <- Sys.time()
      message(p1)
     # plan(sequential)

      metrics <-   lidR::pixel_metrics(las,
                                   func = ~compute_pixel_metrics(X,Y,Z),
                                   grid=stars::st_as_stars(template))

      p2 <- Sys.time()
      message(p2)
      message("Time elapsed", p2-p1)

    }
  }


  if(is.character(metrics)) metrics  <- terra::rast(metrics)

  names(metrics) <- c("canopyCover" ,
  "canopyHeight",
  "CBH"       ,
  "CBD"    ,
  "ladderFuelRatio"  ,
  "CRR"     ,
  "z_skewness"  )

  if(!checkVRTvalidity(metrics)){
    stop()
  }



    if (file.exists(out_file) && !force) {
      message( "\033[32mMetrics already done:   ...results available in:
  ", file.path(getwd(), out_file) , ".
  Remove or use force=TRUE if you want to recalculate.\033[0m")

    }

  message("  convert metrics result to terra raster ")
  ## convert metrics result to terra raster
  if(is.character(metrics)) metrics_rast <- terra::rast(metrics)
  else metrics_rast <- metrics


  # --- Assemble all bands ---
  if (!is.null(dtm)) {
    slope_r  <- terrain(terra::rast(dtm),  v = "slope",  unit = "degrees")
    aspect_r <- terrain(terra::rast(dtm),  v = "aspect", unit = "degrees")

    dtm_r    <- resample(terra::rast(dtm),      metrics_rast, method = "bilinear")
    slope_r  <- resample(slope_r,  metrics_rast, method = "bilinear")
    aspect_r <- resample(aspect_r, metrics_rast, method = "bilinear")

    out_rast        <- c(dtm_r, slope_r, aspect_r, metrics_rast)
    names(out_rast) <-  c("height", "slope", "aspect", "canopyCover" ,
         "canopyHeight",
         "CBH"       ,
         "CBD"    ,
         "ladderFuelRatio"  ,
         "CRR"     ,
         "z_skewness"  )

  } else {
    message("  DTM unavailable, saving 4 canopy bands only...")
    out_rast        <- metrics_rast
    names(out_rast) <- c("canopyCover", "canopyHeight", "CBH", "CBD"  ,
                         "ladderFuelRatio"  ,
                         "CRR"     ,
                         "z_skewness")
  }

  if(is.null(pathtemplate) || pathtemplate==FALSE){
    message("  Bands: ", nlyr(out_rast), " → ", paste(names(out_rast), collapse = ", "))
    writeRaster(out_rast, out_file, overwrite = TRUE)
    message( "\033[32mSaved to file: ", file.path(getwd(), out_file) , "\033[0m")
  } else {
    message("TRANSFORMING COORDINATES ")
    out_rast <- terra::project(out_rast, terra::crs(terra::rast(pathtemplate) ) )
    message("  Bands: ", nlyr(out_rast), " → ", paste(names(out_rast), collapse = ", "))
    writeRaster(out_rast, out_file, overwrite = TRUE)
    message( "\033[32mSaved to file: ", file.path(getwd(), out_file) , "\033[0m")
  }
  return(NULL)
}

# C. RUN -----
## parameters -----
plotdir= "/mnt/archivioVSIX/tmp/wildfire/"

las_folder <- "/archivio/shared/geodati/las/fvg/tarvisio/"
wd <- getwd()
setwd(las_folder)
## this below in case we have a raster to use as template
## NB CRS should be the same as LAS!
template <- "/archivio/shared/geodati/raster/wildfire/pilotRegions_AT-IT-SI_canopy_canopyBaseHeight.tif"
process_plot("/archivio/shared/geodati/las/fvg/tarvisio/", template = template,force = F )
setwd(wd)


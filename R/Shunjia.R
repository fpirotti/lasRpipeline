# =============================================================================
# Forest Fire Metrics from LiDAR Point Cloud → 20m Raster Output
# Metrics: height, slope, aspect, canopyCover, canopyHeight, CBH, CBD
# Method: Beer-Lambert / PAD vertical profile (Martin-Ducup et al. 2025)
# =============================================================================

library(lidR)
library(terra)
library(sp)

# =============================================================================
# 0. PARAMETERS ----
# =============================================================================

output_dir   <- "output_rasters"
dir.create(output_dir, showWarnings = FALSE)

res          <- 20        # raster resolution (m)
layer_depth  <- 0.5       # vertical slice thickness (m)
G_theta      <- 0.5       # leaf projection ratio (spherical assumption)
omega        <- 0.77      # clumping index (Martin-Ducup et al. 2025)
FMA          <- 0.25      # Fuel Mass Area kg/m²
BD_threshold <- 0.02      # bulk density threshold for CBH (kg/m³)

# =============================================================================
# 1. FUNCTION: compute canopy metrics for a single 20x20m pixel ----
# =============================================================================

compute_pixel_metrics <- function(z) {

  canopy_cover  <- sum(z > 2) / length(z)
  canopy_height <- as.numeric(quantile(z, 0.95))
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

# =============================================================================
#### 2. FUNCTION: process one plot folder → one .tif with 7 bands ----
# =============================================================================

process_plot <- function(plot_folder) {

  browser()
  plot_name <- basename(plot_folder)
  out_file  <- file.path(output_dir, paste0(plot_name, "_metrics.tif"))

  if (file.exists(out_file)) {
    message("Skipping (already done): ", plot_name)
    return(invisible(NULL))
  }

  message("Processing: ", plot_name)

  # --- Read catalog ---
  ctg_local <- tryCatch(
    readLAScatalog(plot_folder),
    error = function(e) { message("  Error reading catalog: ", e$message); return(NULL) }
  )
  if (is.null(ctg_local)) return(invisible(NULL))

  # --- Read ground points only for DTM ---
  message("  Computing DTM...")
  las_ground <- tryCatch(
    readLAS(ctg_local, filter = "-keep_class 2"),
    error = function(e) { message("  Error reading ground: ", e$message); return(NULL) }
  )

  dtm <- NULL
  if (!is.null(las_ground) && !is.empty(las_ground)) {
    dtm <- tryCatch(
      rasterize_terrain(las_ground, res = res, algorithm = knnidw()),
      error = function(e) { message("  DTM error: ", e$message); return(NULL) }
    )
    rm(las_ground)  # free memory
  }

  # --- Read full point cloud for canopy metrics ---
  las <- tryCatch(
    readLAS(ctg_local, filter = "-keep_class 2 3 4 5"),
    error = function(e) { message("  Error loading LAS: ", e$message); return(NULL) }
  )
  if (is.null(las) || is.empty(las)) return(invisible(NULL))

  # --- Ground classification if missing ---
  if (sum(las$Classification == 2L) == 0) {
    message("  No ground points, running CSF...")
    las <- classify_ground(las, algorithm = csf())
  }

  # --- Height normalization ---
  message("  Normalizing heights...")
  las <- tryCatch(
    normalize_height(las, algorithm = knnidw()),
    error = function(e) { message("  Normalization error: ", e$message); return(NULL) }
  )
  if (is.null(las)) return(invisible(NULL))
  las <- filter_poi(las, Z >= 0 & Z < 60)

  # --- Canopy metrics at 20m ---
  message("  Computing canopy metrics...")
  metrics      <- grid_metrics(las, ~compute_pixel_metrics(Z), res = res)
  metrics_rast <- rast(metrics)

  # --- Assemble all bands ---
  if (!is.null(dtm)) {
    slope_r  <- terrain(dtm,  v = "slope",  unit = "degrees")
    aspect_r <- terrain(dtm,  v = "aspect", unit = "degrees")

    dtm_r    <- resample(dtm,      metrics_rast, method = "bilinear")
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
  message("  Saved: ", out_file)

  return(invisible(NULL))
}

# =============================================================================
# 3. RUN -----
# =============================================================================

# Single plot test:put your data here
process_plot("/archivio/shared/geodati/las/fvg/tarvisio/")

# All 60 plots:
# plot_folders <- list.dirs("path/to/parent/folder", full.names = TRUE, recursive = FALSE)
# for (folder in plot_folders) process_plot(folder)


# file.remove("output_rasters/download-20260305185221040_metrics.tif")
# process_plot("D:/wildfire/pilot_test/input/download-20260305185221040")

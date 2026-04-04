# =============================================================================
# Forest Fire Metrics from LiDAR Point Cloud → 20m Raster Output
# Metrics: height, slope, aspect, canopyCover, canopyHeight, CBH, CBD
# Method: Beer-Lambert / PAD vertical profile (Martin-Ducup et al. 2025)
# =============================================================================

library(lidR)
library(terra)
library(sp)
library(future)
#multicore ??
plan(multisession, workers = 10)

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
alpha <- 0.1  # 0 = fully transparent, 1 = opaque
colors <- rgb(t(col2rgb(heat.colors(10)))/255, alpha=alpha)

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
  }
  lastmp <- suppressMessages( lidR::LAS(data.frame(X=X,Y=Y,Z=Z)) )
  ## 1x1 cubes
  vox_met <- voxel_metrics(lastmp, ~list(N = length(Z)), 1)
  totArea <- nrow(unique(vox_met[,c("X","Y")]))
  totAreaWithCanopy <- nrow(unique(vox_met[vox_met$Z>=2,c("X","Y")]))
  # zVox <- vox_met[vox_met$Z>=2,c("Z")][[1]]
  # write.csv(vox_met, "voxmet.xyz", row.names = F)
  #  plot(vox_met, color="N",  opacity=0.5, pal = colors,   voxel = TRUE)
  canopy_cover  <- totAreaWithCanopy / totArea
  # tt <- table(zVox)
  # plot(diff( tt/sum(tt) ))
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

# =============================================================================
#### 2. FUNCTION: process one plot folder → one .tif with 7 bands ----
# =============================================================================

process_plot <- function(plot_folder, template=NULL) {

  plot_name <- basename(plot_folder)
  out_file  <- file.path(output_dir, paste0(plot_name, "_metrics.tif"))
  tmp_Folder  <- file.path(plot_folder, "tmp")
  tmp_Folder_DTMs  <- file.path(tmp_Folder, "DTMs")
  tmp_Folder_Norm  <- file.path(tmp_Folder, "NormLAS")
  tmp_Folder_Metrics  <- file.path(tmp_Folder, "Metrics")

  if (!dir.exists(tmp_Folder_DTMs)) {
    dir.create(tmp_Folder_DTMs,mode = "777")
  }
  if (!dir.exists(tmp_Folder_Norm)) {
    dir.create(tmp_Folder_Norm,mode = "777")
  }
  if (!dir.exists(tmp_Folder_Metrics)) {
    dir.create(tmp_Folder_Metrics,mode = "777")
  }

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

  opt_chunk_buffer(ctg_local) <- 10

  # --- Read ground points only for DTM ---
  message("  Computing DTM...")
  # las_ground <- tryCatch(
  #   readLAS(ctg_local, filter = "-keep_class 2"),
  #   error = function(e) { message("  Error reading ground: ", e$message); return(NULL) }
  # )

  dtm <- NULL

  dtm <-  tryCatch(
    terra::rast(file.path(tmp_Folder_DTMs, "rasterize_terrain.vrt")),
    warning = function(e) { message("  DTM error: ", e$message); return(NULL) },
    error = function(e) { message("  DTM error: ", e$message); return(NULL) }
  )

  resDTM <- min(2, res)
  if (is.null(dtm)) {
    opt_output_files(ctg_local) <- paste0(tmp_Folder_DTMs, "/{*}_DTM")
    dtm <- tryCatch(
      rasterize_terrain(ctg_local, res = resDTM, algorithm =  tin()),
      error = function(e) { message("  DTM create error: ", e$message); return(NULL) }
    )
    # rm(las_ground)  # free memory
    dtm <-  tryCatch(
      terra::rast(file.path(tmp_Folder_DTMs, "rasterize_terrain.vrt")),
      warning = function(e) { message("  DTM read error: ", e$message); return(NULL) },
      error = function(e) { message("  DTM read  error: ", e$message); return(NULL) }
    )
  }
  ## read again because VRT has pointer problems


  # --- Read full point cloud for canopy metrics ---
  # las <- tryCatch(
  #   readLAS(ctg_local, filter = "-keep_class 2 3 4 5"),
  #   error = function(e) { message("  Error loading LAS: ", e$message); return(NULL) }
  # )
  # if (is.null(las) || is.empty(las)) return(invisible(NULL))

  # --- Ground classification if missing ---
  # if (sum(las$Classification == 2L) == 0) {
  #   message("  No ground points, running CSF...")
  #   las <- classify_ground(las, algorithm = csf())
  # }

  # --- Height normalization ---
  message("  Normalizing heights...")
  opt_output_files(ctg_local) <- paste0(tmp_Folder_Norm, "/{*}_Norm")
  las <- tryCatch(
    lidR::catalog( tmp_Folder_Norm ),
    warning = function(e) { message("Cannot read normalized folder: ", e$message); return(NULL) },
    error = function(e) { message("Cannot read normalized folder: ", e$message); return(NULL) }
  )

  # NORMALIZE ----
  if (is.null(las)) {
    las <- tryCatch(
      normalize_height(ctg_local, algorithm = tin(),
                       dtm = dtm), #algorithm = knnidw() ),
      error = function(e) { message("  Normalization error: ", e$message); return(NULL) }
    )
    if (is.null(las)) return(invisible(NULL))
  }

  # --- Canopy metrics at 20m ----
  message("  Computing canopy metrics...")
  # ll <- readLAS(las@data$filename[[23]], filter=filter)
  opt_output_files(ctg_local) <- paste0(tmp_Folder_Metrics, "/{*}_Met")
  filter <- "-drop_z_below 0 -drop_z_above 60 -keep_class 2 3 4 5"
  opt_filter(ctg_local) <- filter
  if (is.null(template)){
    metrics      <- pixel_metrics(las, ~compute_pixel_metrics(X,Y,Z),  res = res)
  } else{
    ## have to check CRS !!! -----
    metrics      <- template_metrics(las, ~compute_pixel_metrics(Z),  template = template,
                                  filter="-drop_z_below 0 -drop_z_above 60  -keep_class 2 3 4 5")
  }
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
plot_folder <- "/archivio/shared/geodati/las/fvg/tarvisio/"
templateGrid <- "/archivio/shared/R/wildfire/input/AT-IT_ScottBurganFuelMapClassV2.tif"
template <- terra::rast(templateGrid)
process_plot("/archivio/shared/geodati/las/fvg/tarvisio/", template)

# All 60 plots:
# plot_folders <- list.dirs("path/to/parent/folder", full.names = TRUE, recursive = FALSE)
# for (folder in plot_folders) process_plot(folder)


# file.remove("output_rasters/download-20260305185221040_metrics.tif")
# process_plot("D:/wildfire/pilot_test/input/download-20260305185221040")

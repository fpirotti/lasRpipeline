
fuelMetrics<-function (data)
{
  if(nrow(data)==0){
    return(c(NA, NA,NA, NA, NA))
  }

  ground <- data[data$Classification==2,]
  if(nrow(ground)<10 ){

      return(c(mean(data$X),
               mean(data$Y),
               median(data$Z), NA,NA))

  }

  fit <- tryCatch(
    {
      suppressWarnings(MASS::rlm(Z ~ X + Y, data = ground))
    },
    error = function(e) {
      NA
    }
  )
  if(!inherits(fit,"lm")){
    return(c(mean(ground$X),
             mean(ground$Y),
             median(ground$Z), NA,NA))
  }


  a <- stats::coef(fit)["X"]
  b <- stats::coef(fit)["Y"]

  # slope (degrees)
  slope_deg <- atan(sqrt(a^2 + b^2)) * 180 / pi

  # aspect (degrees, 0° = North, clockwise)
  aspect_rad <- atan2(b, -a) # note the sign flip to match GIS convention
  aspect_deg <- (aspect_rad * 180 / pi) %% 360

  # metrics <- list(slopeRobust = slope_deg[[1]], aspect_deg[[1]])
  metrics <- c( mean(ground$X),
                mean(ground$Y),
                median(ground$Z),
                slope_deg[[1]], aspect_deg[[1]])

  return(metrics)
}




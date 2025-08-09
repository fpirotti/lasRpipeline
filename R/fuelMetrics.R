
#' slope_aspect
#' @description
#' Return interpolated slope and aspect from linear model and
#' robust linear model
#'
#' @param x x coordinate
#' @param y y coordinate
#' @param z z coordinate
#'
#' @returns a list with slope and aspect in degrees, both with
#' lme and wi
#'
#' @examples
#' # none
slope_aspect <- function(x, y, z) {
  m <- stats::lm(z ~ x + y)
  a <- stats::coef(m)[2]  # slope in x
  b <- stats::coef(m)[3]  # slope in y


  slope  <- atan(sqrt(a^2 + b^2)) * 180 / pi
  aspect <- atan2(b, -a) * 180 / pi

  if (aspect < 0) aspect <- aspect + 360

  fit <- MASS::rlm(z ~ x + y)
  a <- stats::coef(fit)["x"]
  b <- stats::coef(fit)["y"]

  # slope (degrees)
  slope_deg <- atan(sqrt(a^2 + b^2)) * 180 / pi

  # aspect (degrees, 0° = North, clockwise)
  aspect_rad <- atan2(b, -a) # note the sign flip to match GIS convention
  aspect_deg <- (aspect_rad * 180 / pi) %% 360


  return(list(slope = slope, aspect = aspect,
              slopeRob = slope_deg,
              aspectRob = aspect_deg))
}

fuelMetrics<-function (data)
{
  browser()
  ground <- data[data$Classification==2,]
  sa <- slope_aspect(ground$X, ground$Y, ground$Z)

  metrics <- list(sa)
  return(metrics)
}




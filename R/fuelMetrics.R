
slope_aspect <- function(x, y, z) {
  m <- lm(z ~ x + y)
  a <- coef(m)[2]  # slope in x
  b <- coef(m)[3]  # slope in y

  slope  <- atan(sqrt(a^2 + b^2)) * 180 / pi
  aspect <- atan2(b, -a) * 180 / pi

  if (aspect < 0) aspect <- aspect + 360

  return(list(slope = slope, aspect = aspect))
}

fuelMetrics<-function (data)
{
  browser()
  ground <- data[data$Classification==2,]
  sa <- slope_aspect(ground$X, ground$Y, ground$Z)

  metrics <- list(Z = max(z), npoints = length(x),
                  height = mean(ground$Z),
                  slope = sa$slope,
                  aspect = sa$aspect
                  )
  return(metrics)
}




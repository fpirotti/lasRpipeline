fuelMetrics<-function (x, y, z)
{
  metrics <- list(Z = max(z), npoints = length(x))
  return(metrics)
}

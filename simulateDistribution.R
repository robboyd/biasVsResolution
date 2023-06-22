library(raster)

## internal function

source("W:/PYWELL_SHARED/Pywell Projects/BRC/Rob Boyd/UK-SCAPE/resolution_vs_bias/R/mvrn.R")

simulateDistribution <- function(prevalence, phi, gridSize) {
  
  # Set up a square lattice region
  simgrid <- expand.grid(1:gridSize, 1:gridSize)
  
  n <- nrow(simgrid)

  # Set up distance matrix
  distance <- as.matrix(dist(simgrid))
  
  # Generate random variable
  X <- rmvn(1, rep(0, n), exp(-phi * distance))
  
  X <- X+abs(min(X))

  X <- X/max(X)

  thresh <- as.numeric(quantile(X, probs = (1-prevalence)))

  print(thresh)
  
  X <- ifelse(X>thresh, 1, 0)

  # Visualize results
  Xraster <- rasterFromXYZ(cbind(simgrid[, 1:2] - 0.5, X))
  
  par(mfrow = c(1, 2))
  
  plot(1:100, exp(-phi * 1:100), type = "l", xlab = "Distance", ylab = "Correlation")
  
  plot(Xraster)

  return(Xraster)
  
}
  


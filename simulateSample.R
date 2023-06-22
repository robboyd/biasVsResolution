simulateSample <- function(distribution, rho, f) {

  #v <- getValues(distribution) 

  v <- data.frame(raster::extract(distribution, 
               extent(distribution),
               cellnumbers = TRUE))

  R <- generate_correlated_vector(v[,2], rho, f)
  
  v$R <- R

  sample <- distribution

  values(sample) <- NA

  values(sample) <- R

  par(mfrow=c(1,2))
  plot(distribution, main = "dist.")
  plot(sample, main = "sample")

  print(paste("f=", mean(R)))
        
  return(sample)
  
}



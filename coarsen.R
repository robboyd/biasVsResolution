coarsen <- function(distribution, samples, fact, id) {
  
  if (!fact == 1) {
    distribution <- aggregate(distribution,
                              fact = c(fact),
                              fun = max,
                              expand = T)
    
    for (i in 1:length(samples)) {
      samples[[i]] <- aggregate(samples[[i]],
                                fact = c(fact),
                                fun = max,
                                expand = T)
    }
  }
  
  par(mfrow=c(1, length(samples) + 1))
  plot(distribution)
  
  for (i in 1:length(samples)) {
    plot(samples[[i]])
  }
  
  vals1 <- getValues(distribution)
  
  p_dist <- mean(vals1)
  
  sd1 <- sqrt(mean(vals1) * (1 - mean(vals1)))
  
  results <- data.frame()
  
  for (i in 1:length(samples)) {
    vals2 <- getValues(samples[[i]])
    error <- mean(distribution[samples[[i]] == 1]) - cellStats(distribution, stat = mean)

    relError <- (error/p_dist) * 100
    
    result <- data.frame(fact = fact,
                         p_dist = p_dist,
                         p_sample = cellStats(samples[[i]], stat = mean),
                         sd_dist = sd1,
                         error = error,
                         rel_error = relError,
                         ddc = cor(vals1, vals2),
                         id = id)
    
    results <- rbind(results, result)
  }
  
  return(results)
}

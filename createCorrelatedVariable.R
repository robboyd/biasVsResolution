generate_correlated_vector <- function(original_vector, correlation, proportion_ones) {
  n <- length(original_vector)
  
  # Generate a new vector with the specified proportion of 1s
  new_vector <- rbinom(n, 1, proportion_ones)
  
  # Calculate the correlation between the original and new vectors
  current_correlation <- cor(original_vector, new_vector)
  
  # Calculate the proportion of ones in new_vector
  current_proportion_ones <- sum(new_vector) / n
  
  # Adjust the correlation and proportion simultaneously
  while (abs(current_correlation - correlation) > 0.02 || abs(current_proportion_ones - proportion_ones) > 0.025) {
    # Find the indices where values differ between the original and new vectors
    differing_indices <- which(original_vector != new_vector)
    
    # Randomly select an index from differing_indices
    index <- sample(differing_indices, 1)
    
    # Flip the value at the selected index in the new vector
    new_vector[index] <- 1 - new_vector[index]
    
    # Recalculate the correlation and proportion
    current_correlation <- cor(original_vector, new_vector)
    current_proportion_ones <- sum(new_vector) / n
  }
  
  print(current_correlation)
  
  return(new_vector)
}

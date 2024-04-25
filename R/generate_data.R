# Function to generate new data with the same structure as the input data
generate_data <- function(Data, density_radius, Cls = NULL, gen_per_data = 10) {
  ## Initialize parameters
  # Maximum distance for data generation (relative to density_radius)
  max_distance <- 2.0

  # Limits for the sigmoid function
  limit_ab <- 0.72
  limit_bc <- 1.22

  # Fraction of data in each set (A, B, C)
  percent_a <- 0.80
  percent_b <- 0.15
  percent_c <- 1 - (percent_a + percent_b)

  # Radius for data generation (shrink to below half of the intercluster distance)
  r <- 0.4 * density_radius

  # Number of data points and dimensionality
  n <- nrow(Data)
  d <- ncol(Data)

  ## Assign class labels if not provided
  if (is.null(Cls)) {
    Cls <- rep(1, n)
  } else if (length(Cls) != n) {
    stop("Unequal number of cases and class labels.")
  }

  ## Generate new data points
  # Number of generated data points
  n_generated <- n * gen_per_data

  # Generate jitter (noise) for the data points
  jitter <- matrix(rnorm(n_generated * d, mean = 0, sd = 1.5), n_generated, d) * r
  jitter_lengths <- sqrt(rowSums(jitter^2))
  jitter_indices <- which(jitter_lengths > 0)
  jitter[jitter_indices, ] <- jitter[jitter_indices, ] / jitter_lengths[jitter_indices]

  # Determine the number of data points in each set (A, B, C)
  n_a <- round(n_generated * percent_a)
  n_b <- round(n_generated * percent_b)
  n_c <- round(n_generated * percent_c)

  # Generate the sigmoid lengths for each set
  sigmoid_lengths <- c(
    runif(n_a, 0, limit_ab),
    runif(n_b, limit_ab, limit_bc),
    runif(n_c, limit_bc, max_distance)
  ) * r

  # Create the sigmoid matrix and generate the new data
  sigmoid_matrix <- matrix(rep(sigmoid_lengths, d), ncol = d)
  generated_data <- Data[rep(1:n, gen_per_data), ] + jitter * sigmoid_matrix
  generated_classes <- rep(Cls, times = gen_per_data)

  # Return the original and generated data and classes
  return(list(
    original_data = Data,
    original_classes = Cls,
    generated_data = generated_data,
    generated_classes = generated_classes
  ))
}

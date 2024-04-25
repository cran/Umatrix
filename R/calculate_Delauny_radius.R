# Function to calculate the radius for data generation
calculate_Delauny_radius <- function(Data, BestMatches, Columns = 80,  Lines = 50,  Toroid = TRUE) {
  # Calculate the distance matrix for the data points
  distancesAsMatrix <- as.matrix(stats::dist(Data))

  # Extract the lower triangle of the distance matrix (excluding the diagonal)
  distances <- distancesAsMatrix[lower.tri(distancesAsMatrix, diag = FALSE)]

  # Calculate the minimum and maximum distances
  PradiusMinimum <- signif(min(distances), digits = 2)
  PradiusMaximum <- signif(max(distances), digits = 2)

  # Initialize the RadiusByEM variable
  RadiusByEM <- NULL

  # Determine the subset of BestMatches to use
  if (nrow(BestMatches) <= 1000) {
    bestmatchId <- 1:nrow(BestMatches)
  } else {
    bestmatchId <- sample(1:nrow(BestMatches), 1000)
  }

  # Calculate the Delaunay triangulation for the subset of BestMatches
  V <- Delaunay4BestMatches(BestMatches[bestmatchId, ], MatrixOrSize = c(Lines, Columns), IsToroid = Toroid)
  Delaunay <- V$Delaunay
  ToroidDelaunay <- V$ToroidDelaunay

  # Calculate the Delaunay distances for the subset of BestMatches
  DelaunayDistances <- Delaunay * distancesAsMatrix[bestmatchId, bestmatchId]
  neighbourDistances <- DelaunayDistances[DelaunayDistances != 0]

  # Estimate the radius using the Expectation-Maximization (EM) algorithm
  RadiusByEM <- 1
  tryCatch({
    V <- EMGauss(neighbourDistances, 2)
    V <- BayesDecisionBoundaries(V$Means, V$SDs, V$Weights)
    if (length(V) > 1) {
      print("Multiple Decision boundaries found. max(Distances)/2 chosen a Radius")
      RadiusByEM <- signif(max(distances)/2, 2)
    } else {
      RadiusByEM <- signif(V * 0.8, 2)
    }
  }, error = function(e) {
    print("No Decision boundaries found. max(Distances)/2 chosen a Radius")
    RadiusByEM <- signif(max(distances)/2, 2)
  })

  # Return the list of results
  return(list(neighbourDistances = neighbourDistances, RadiusByEM = RadiusByEM))
}

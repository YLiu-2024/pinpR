#' Query K-Nearest Neighbors
#'
#' This internal utility function finds the k-nearest neighbors in `X_train`
#' for each point in `X_new`. It serves as a wrapper around the `FNN::get.knnx`
#' function.
#'
#' @param X_train A matrix or data frame of training data features.
#' @param X_new A matrix or data frame of new data points for which to find
#'   neighbors.
#' @param k The number of nearest neighbors to find.
#'
#' @return A list containing two components:
#'   \item{nn.index}{A matrix of indices of the nearest neighbors.}
#'   \item{nn.dist}{A matrix of distances to the nearest neighbors.}
#'
#' @importFrom FNN get.knnx
#'
query_knn <- function(X_train, X_new, k) {
  # Ensure k is not larger than the number of training points
  if (k > nrow(X_train)) {
    stop("k cannot be larger than the number of rows in X_train.")
  }

  # Find the k-nearest neighbors
  nn_results <- FNN::get.knnx(data = X_train,
                              query = X_new,
                              k = k)

  # Return the indices and distances
  return(list(nn.index = nn_results$nn.index,
              nn.dist = nn_results$nn.dist))
}

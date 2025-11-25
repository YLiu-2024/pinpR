#' Simulate: Heteroskedastic Data
#'
#' @param n Sample size
#' @param seed Random seed
#' @return A data.frame
#' @export
sim_hetero <- function(n = 500, seed = 101) {
  set.seed(seed)
  X1 <- runif(n, 0, 1)
  # Error standard deviation increases linearly with X1
  error_sd <- 0.1 + 0.4 * X1
  y <- 2 * X1 + rnorm(n, 0, error_sd)
  return(data.frame(X1 = X1, y = y))
}

#' Simulate: Multimodal (Mix) Data
#'
#' @param n Sample size
#' @param seed Random seed
#' @return A data.frame
#' @export
sim_mix <- function(n = 500, seed = 102) {
  set.seed(seed)
  X1 <- runif(n, 0, 1)
  # Mix two models
  y1 <- 2 * X1 + rnorm(n, 0, 0.1)
  y2 <- 2 * X1 + 4 * (X1 - 0.5)^2 + rnorm(n, 1, 0.1)
  # Randomly assign
  mask <- rbinom(n, 1, 0.5)
  y <- ifelse(mask == 1, y1, y2)
  return(data.frame(X1 = X1, y = y))
}

#' Simulate: Skewed Data
#'
#' @param n Sample size
#' @param seed Random seed
#' @return A data.frame
#' @export
sim_skew <- function(n = 500, seed = 103) {
  set.seed(seed)
  X1 <- runif(n, 0, 1)
  # Error follows a (shifted) exponential distribution
  error <- rexp(n, rate = 2) - 0.5
  y <- 2 * X1 + error
  return(data.frame(X1 = X1, y = y))
}


#' Simulate: Sparse Region Data (for robustness testing)
#'
#' Data is very sparse in the X > 0.5 region.
#'
#' @param n_dense Number of points in the dense region
#' @param n_sparse Number of points in the sparse region
#' @param seed Random seed
#' @return A data.frame
#' @export
sim_sparse <- function(n_dense = 500, n_sparse = 5, seed = 104) {
  set.seed(seed)
  
  # Dense region
  X1_dense <- runif(n_dense, 0, 0.5)
  y_dense <- 2 * X1_dense + rnorm(n_dense, 0, 0.2)
  
  # Sparse region
  X1_sparse <- runif(n_sparse, 0.51, 1)
  y_sparse <- 2 * X1_sparse + rnorm(n_sparse, 0, 0.2)
  
  X1 <- c(X1_dense, X1_sparse)
  y <- c(y_dense, y_sparse)
  
  return(data.frame(X1 = X1, y = y))
}

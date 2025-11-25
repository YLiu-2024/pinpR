#' Fit kNN Equal-Tailed Prediction Interval Model
#'
#' This internal function "fits" a kNN-based model for generating equal-tailed
#' prediction intervals. For kNN, fitting primarily involves storing the
#' training data and the number of neighbors `k`.
#'
#' @param X A matrix or data frame of predictor variables from the training data.
#' @param y A numeric vector of the response variable from the training data.
#' @param k The number of neighbors. Defaults to 50.
#' @param ... Additional arguments (not used in this method).
#'
#' @return A list representing the fitted kNN model, containing:
#'   \item{X_train}{The training predictor variables.}
#'   \item{y_train}{The training response variable.}
#'   \item{k}{The number of neighbors used.}
knn_eqt_fit <- function(X, y, k = 50, ...) {
  # For kNN, "fitting" means storing the training data and parameters
  model <- list(
    X_train = X,
    y_train = y,
    k = k
  )
  class(model) <- "pinp_knn_eqt" # Assign a class for S3 dispatch
  return(model)
}

#' Predict with kNN Equal-Tailed Prediction Interval Model
#'
#' This internal function generates predictions using the fitted kNN model.
#' It finds the k-nearest neighbors for each new data point, performs a
#' diagnostic check on the distance to the k-th neighbor, and computes
#' equal-tailed prediction intervals either from the local neighbors or by
#' falling back to a global linear model.
#'
#' @param model A fitted model object from `knn_eqt_fit`.
#' @param X_new A matrix or data frame of new predictor data.
#' @param alpha The significance level for the prediction intervals.
#' @param fallback_model A fitted linear model object (e.g., from `lm()`)
#'   to be used for fallback predictions.
#' @param use_fallback A logical value indicating whether to enable the
#'   fallback mechanism. Defaults to `TRUE`.
#' @param max_dist_fallback The maximum distance allowed for the k-th neighbor.
#'   If the k-th neighbor's distance exceeds this, and `use_fallback` is `TRUE`,
#'   the function will fall back to the `fallback_model`. Defaults to 0.1.
#'
#' @return A list containing:
#'   \item{L}{A numeric vector of lower prediction bounds.}
#'   \item{U}{A numeric vector of upper prediction bounds.}
#'   \item{info}{A data frame with diagnostic information, including `dist_k`
#'     (distance to k-th neighbor) and any warnings.}
knn_eqt_predict <- function(model, X_new, alpha, fallback_model, use_fallback = TRUE, max_dist_fallback = 0.1) {
  n_new <- nrow(X_new)
  neighbors <- query_knn(X_train = model$X_train, X_new = X_new, k = model$k)

  # Initialize vectors to store results and diagnostics
  lower_bounds <- numeric(n_new)
  upper_bounds <- numeric(n_new)
  info_dist_k <- numeric(n_new) # Changed from info_eff_n
  info_warning <- character(n_new)
  
  probs <- c(alpha / 2, 1 - alpha / 2)

  # Loop through each new observation to calculate its prediction interval
  for (i in 1:n_new) {
    # --- Diagnostic Check (distance to k-th neighbor) ---
    k_dist <- neighbors$nn.dist[i, model$k]
    info_dist_k[i] <- k_dist
    trigger_fallback <- k_dist > max_dist_fallback

    # --- Fallback Logic ---
    if (trigger_fallback && use_fallback) {
      fb <- fallback_lm_predict(fallback_model, X_new[i, , drop = FALSE], alpha)
      lower_bounds[i] <- fb$L
      upper_bounds[i] <- fb$U
      info_warning[i] <- "Fallback: LM (dist > thresh)"
    } else {
      # Get the y-values of the neighbors
      neighbor_indices <- neighbors$nn.index[i, ]
      y_neighbors <- model$y_train[neighbor_indices]

      # For simple kNN, local weights are equal.
      local_weights <- rep(1, model$k)
      
      # Calculate the weighted quantiles for the neighbors' y-values
      quantiles <- weighted_quantile(
        values = y_neighbors,
        weights = local_weights,
        probs = probs
      )

      lower_bounds[i] <- quantiles[1]
      upper_bounds[i] <- quantiles[2]
      info_warning[i] <- NA_character_
    }
  }

  # Combine diagnostic information into a data frame
  info_df <- data.frame(dist_k = info_dist_k, warning = info_warning) # Changed from eff_n

  # Return the results in the specified list format
  list(
    L = lower_bounds,
    U = upper_bounds,
    info = info_df
  )
}
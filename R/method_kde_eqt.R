#' Fit KDE Equal-Tailed Prediction Interval Model
#'
#' This internal function "fits" a KDE-based model for generating equal-tailed
#' prediction intervals. It handles multi-dimensional predictor variables by
#' preparing to use a product kernel.
#'
#' @param X A matrix or data frame of predictor variables from the training data.
#' @param y A numeric vector of the response variable from the training data.
#' @param bw_x A numeric vector specifying the bandwidth for each predictor
#'   variable. If `NULL`, bandwidths are calculated for each column of `X`
#'   using `stats::bw.nrd0`.
#' @param bw_y A numeric scalar specifying the bandwidth for the response
#'   variable. If `NULL`, it is calculated using `stats::bw.nrd0`.
#' @param ... Additional arguments (not used in this method).
#'
#' @return A list representing the fitted KDE model.
#' @importFrom stats bw.nrd0
kde_eqt_fit <- function(X, y, bw_x = NULL, bw_y = NULL, ...) {
  # Ensure X is a data frame
  X <- as.data.frame(X)

  # 1. Handle multi-dimensional bandwidth for X
  if (is.null(bw_x)) {
    bw_x <- apply(X, 2, stats::bw.nrd0)
  }
  
  if (is.null(bw_y)) {
    bw_y <- stats::bw.nrd0(y)
  }

  model <- list(
    X_train = X,
    y_train = y,
    bw_x = bw_x,
    bw_y = bw_y
  )
  class(model) <- "pinp_kde_eqt"
  return(model)
}

#' Predict with KDE Equal-Tailed Prediction Interval Model (Product Kernel)
#'
#' This internal function generates predictions using the fitted KDE model,
#' supporting multi-dimensional `X` via a product kernel. It performs a
#' diagnostic check on the effective sample size and can fall back to a
#' global linear model.
#'
#' @param model A fitted model object from `kde_eqt_fit`.
#' @param X_new A data frame of new predictor data.
#' @param alpha The significance level for the prediction intervals.
#' @param fallback_model A fitted linear model object for fallback.
#' @param use_fallback A logical value to enable/disable fallback.
#' @param min_eff_n The minimum effective sample size for a local estimate.
#'
#' @return A list containing the lower and upper bounds and diagnostic info.
#' @importFrom stats dnorm
kde_eqt_predict <- function(model, X_new, alpha, fallback_model, use_fallback = TRUE, min_eff_n = 5.0) {
  X_train <- model$X_train
  y_train <- model$y_train
  n_train <- nrow(X_train)
  n_new <- nrow(X_new)
  n_dim <- ncol(X_train)

  # Initialize storage
  lower_bounds <- numeric(n_new)
  upper_bounds <- numeric(n_new)
  info_eff_n <- numeric(n_new)
  info_warning <- character(n_new)
  
  probs <- c(alpha / 2, 1 - alpha / 2)

  # Loop through each new observation
  for (i in 1:n_new) {
    
    # --- Product Kernel Weight Calculation ---
    # Initialize weights for all training points for the current new point
    weights_x <- rep(1, n_train)
    
    # Loop through each dimension (predictor)
    for (j in 1:n_dim) {
      # Calculate 1D Gaussian kernel weights for the j-th dimension
      w_j <- dnorm(
        (X_train[, j] - X_new[i, j]) / model$bw_x[j]
      )
      # Multiply into the total product kernel weight
      weights_x <- weights_x * w_j
    }

    # --- Diagnostic Check ---
    eff_n <- effective_n(weights_x)
    info_eff_n[i] <- eff_n
    trigger_fallback <- eff_n < min_eff_n

    # --- Fallback Logic ---
    if (use_fallback && trigger_fallback && !is.null(fallback_model)) {
      fb <- fallback_lm_predict(fallback_model, X_new[i, , drop = FALSE], alpha)
      lower_bounds[i] <- fb$L
      upper_bounds[i] <- fb$U
      info_warning[i] <- paste0("Fallback: eff_n (", round(eff_n, 1), ") < ", min_eff_n)
    } else {
      # --- Standard KDE Logic (using weighted quantiles) ---
      if (sum(weights_x) > 1e-9) {
        w_norm <- weights_x / sum(weights_x)
        quantiles <- weighted_quantile(
          values = y_train,
          weights = w_norm,
          probs = probs
        )
        lower_bounds[i] <- quantiles[1]
        upper_bounds[i] <- quantiles[2]
        info_warning[i] <- NA_character_
      } else {
        lower_bounds[i] <- NA
        upper_bounds[i] <- NA
        info_warning[i] <- "Conditional density is zero; cannot compute interval."
      }
    }
  }

  # Final Return
  info_df <- data.frame(eff_n = info_eff_n, warning = info_warning)
  list(
    L = lower_bounds,
    U = upper_bounds,
    info = info_df
  )
}
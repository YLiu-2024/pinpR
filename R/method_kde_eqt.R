#' Fit KDE Equal-Tailed Prediction Interval Model
#'
#' This internal function "fits" a KDE-based model for generating equal-tailed
#' prediction intervals. For KDE, fitting primarily involves storing the
#' training data and bandwidth parameters.
#'
#' @param X A matrix or data frame of predictor variables from the training data.
#' @param y A numeric vector of the response variable from the training data.
#' @param bw_x A numeric vector or scalar specifying the bandwidth(s) for the
#'   predictor variables. If `NULL`, bandwidth selection will be performed later.
#' @param bw_y A numeric scalar specifying the bandwidth for the response
#'   variable. If `NULL`, bandwidth selection will be performed later.
#' @param ... Additional arguments (not used in this method).
#'
#' @return A list representing the fitted KDE model, containing:
#'   \item{X_train}{The training predictor variables.}
#'   \item{y_train}{The training response variable.}
#'   \item{bw_x}{The bandwidth(s) for X, or `NULL` if not specified.}
#'   \item{bw_y}{The bandwidth for y, or `NULL` if not specified.}
kde_eqt_fit <- function(X, y, bw_x = NULL, bw_y = NULL, ...) {
  # For KDE, "fitting" means storing the training data and parameters
  model <- list(
    X_train = X,
    y_train = y,
    bw_x = bw_x,
    bw_y = bw_y
  )
  class(model) <- "pinp_kde_eqt" # Assign a class for S3 dispatch
  return(model)
}

#' Predict with KDE Equal-Tailed Prediction Interval Model
#'
#' This internal function generates predictions using the fitted KDE model.
#' It estimates the conditional density of Y|X for each new data point,
#' performs a diagnostic check on the effective sample size, and computes
#' equal-tailed prediction intervals either from the local density or by
#' falling back to a global linear model.
#'
#' @param model A fitted model object from `kde_eqt_fit`.
#' @param X_new A matrix or data frame of new predictor data.
#' @param alpha The significance level for the prediction intervals.
#' @param fallback_model A fitted linear model object to be used for fallback.
#' @param use_fallback A logical value indicating whether to enable fallback.
#' @param min_eff_n The minimum effective sample size required for a local estimate.
#'
#' @return A list containing:
#'   \item{L}{A numeric vector of lower prediction bounds.}
#'   \item{U}{A numeric vector of upper prediction bounds.}
#'   \item{info}{A data frame with diagnostic information.}
kde_eqt_predict <- function(model, X_new, alpha, fallback_model, use_fallback = TRUE, min_eff_n = 5.0) {
  # Extract model components
  X_train <- model$X_train
  y_train <- model$y_train
  bw_x <- model$bw_x
  bw_y <- model$bw_y

  # Handle NULL bandwidths
  if (is.null(bw_x)) {
    bw_x <- apply(X_train, 2, bw.nrd0)
  }
  if (is.null(bw_y)) {
    bw_y <- bw.nrd0(y_train)
  }

  # Initialize storage
  n_new <- nrow(X_new)
  lower_bounds <- numeric(n_new)
  upper_bounds <- numeric(n_new)
  info_eff_n <- numeric(n_new)
  info_warning <- character(n_new)
  
  probs <- c(alpha / 2, 1 - alpha / 2)

  # Loop through each new observation
  for (i in 1:n_new) {
    current_x <- X_new[i, , drop = FALSE]

    # Calculate product kernel weights
    kernel_vals_x <- sapply(1:ncol(X_train), function(j) {
      dnorm((X_train[, j] - current_x[, j]) / bw_x[j])
    })
    weights_x <- if (is.matrix(kernel_vals_x)) apply(kernel_vals_x, 1, prod) else kernel_vals_x

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
      # --- Standard KDE Logic ---
      # This is a simplified approach. A more robust implementation would
      # use the weights directly on a grid of y values.
      # For now, we use the weights on the original y_train values.
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
        # If weights are all zero, we can't form an interval
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
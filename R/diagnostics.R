#' Calculate Global Quantiles for Fallback
#'
#' This internal utility function calculates global empirical quantiles from
#' the training response variable `y_train`. These quantiles serve as a
#' fallback mechanism when local estimates are deemed unreliable.
#'
#' @param y_train A numeric vector of the response variable from the training data.
#' @param alpha A numeric value between 0 and 1, representing the significance
#'   level for the prediction interval.
#'
#' @return A list containing two components:
#'   \item{L}{The lower global quantile.}
#'   \item{U}{The upper global quantile.}
#'
#' @importFrom stats quantile
fallback_global_quantile <- function(y_train, alpha) {
  probs <- c(alpha / 2, 1 - alpha / 2)
  q <- stats::quantile(y_train, probs = probs, na.rm = TRUE)
  list(L = q[1], U = q[2])
}

#' Calculate Effective Sample Size
#'
#' This internal utility function calculates the effective sample size from a
#' vector of weights using Kish's approximation. It is used as a diagnostic
#' to assess the reliability of a local estimate.
#'
#' @param weights A numeric vector of weights.
#'
#' @return A numeric value representing the effective sample size. Returns 0
#'   if the sum of squared weights is zero to avoid division by zero.
#'
effective_n <- function(weights) {
  sum_w_sq <- sum(weights^2)
  if (sum_w_sq == 0) {
    return(0)
  }
  (sum(weights))^2 / sum_w_sq
}

#' Determine if Fallback to Global Quantiles is Needed
#'
#' This internal helper function checks if the effective sample size `eff_n`
#' is below a specified threshold `min_eff_n`, indicating that the local
#' estimate is unreliable and a fallback to global quantiles should be used.
#'
#' @param eff_n The effective sample size for a local estimate.
#' @param min_eff_n The minimum required effective sample size.
#'
#' @return A logical value: `TRUE` if fallback is needed, `FALSE` otherwise.
#'
should_fallback <- function(eff_n, min_eff_n) {
  eff_n < min_eff_n
}

#' Predict with Fallback Linear Model
#'
#' This internal utility function uses a pre-fitted linear model to generate
#' prediction intervals. It serves as a fallback mechanism when nonparametric
#' local estimates are deemed unreliable.
#'
#' @param lm_model A fitted linear model object (e.g., from `lm()`).
#' @param X_new_row A data frame or matrix containing a single new observation
#'   for which to predict.
#' @param alpha A numeric value between 0 and 1, representing the significance
#'   level for the prediction interval.
#'
#' @return A list containing two components:
#'   \item{L}{The lower prediction bound.}
#'   \item{U}{The upper prediction bound.}
#'
#' @importFrom stats predict
fallback_lm_predict <- function(lm_model, X_new_row, alpha) {
  # Ensure X_new_row is a data frame for predict.lm
  if (!is.data.frame(X_new_row)) {
    X_new_row <- as.data.frame(X_new_row)
  }

  # Predict intervals using the linear model
  preds <- stats::predict(lm_model, newdata = X_new_row,
                          interval = "prediction", level = 1 - alpha)

  list(L = preds[1, "lwr"], U = preds[1, "upr"])
}

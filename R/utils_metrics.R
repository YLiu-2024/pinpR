#' Evaluate Prediction Interval Performance
#'
#' Calculates key performance metrics for a set of prediction intervals,
#' including coverage, average width, and the Winkler score.
#'
#' @param y_true A numeric vector of true response values.
#' @param L A numeric vector of lower prediction interval bounds.
#' @param U A numeric vector of upper prediction interval bounds.
#' @param alpha A numeric value representing the significance level at which the
#'   intervals were constructed. This is used for calculating the Winkler score.
#'
#' @return A data frame with one row and four columns:
#'   \item{Coverage}{The proportion of true values that fall within their
#'     corresponding prediction interval.}
#'   \item{Avg_Width}{The average width of the prediction intervals.}
#'   \item{Winkler_Score}{The mean Winkler score, which penalizes both for
#'     wide intervals and for lack of coverage.}
#'   \item{Target_Alpha}{The significance level `alpha` provided.}
#'
#' @export
#'
#' @examples
#' y_true <- 1:10
#' L <- y_true - 2
#' U <- y_true + 2
#' evaluate_pinp(y_true, L, U, alpha = 0.1)
#'
#' # Example with some out-of-bounds points
#' y_true <- c(5, 6, 7, 8, 15)
#' L <- c(4, 5, 6, 7, 8)
#' U <- c(6, 7, 8, 9, 10)
#' evaluate_pinp(y_true, L, U, alpha = 0.2)
evaluate_pinp <- function(y_true, L, U, alpha = 0.1) {
  # 1. Calculate Coverage
  coverage <- mean((y_true >= L) & (y_true <= U), na.rm = TRUE)

  # 2. Calculate Average Width
  avg_width <- mean(U - L, na.rm = TRUE)

  # 3. Calculate Winkler Score
  interval_width <- U - L
  
  # Penalty for observations below the lower bound
  penalty_below <- (2 / alpha) * (L - y_true) * (y_true < L)
  
  # Penalty for observations above the upper bound
  penalty_above <- (2 / alpha) * (y_true - U) * (y_true > U)
  
  # The score is the width plus any penalties
  winkler_scores <- interval_width + penalty_below + penalty_above
  mean_winkler_score <- mean(winkler_scores, na.rm = TRUE)

  # 4. Return a data.frame with the metrics
  data.frame(
    Coverage = coverage,
    Avg_Width = avg_width,
    Winkler_Score = mean_winkler_score,
    Target_Alpha = alpha
  )
}

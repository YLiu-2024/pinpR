#' Fit a Nonparametric Prediction Interval Model
#'
#' `fit_pinp` is a generic function for fitting a model to be used for
#' generating nonparametric prediction intervals. The function invokes the
#' appropriate method based on the class of the input object.
#'
#' @param object An object to be used for fitting the model.
#' @param ... Additional arguments to be passed to the specific method.
#'
#' @return A fitted object of class `pinp_fit`.
#'
#' @export
fit_pinp <- function(object, ...) {
  UseMethod("fit_pinp")
}

#' @rdname fit_pinp
#' @export
fit_pinp.default <- function(object, ...) {
  stop("The `fit_pinp` function does not have a default method. ",
       "Please provide a supported object type (e.g., a formula or data.frame).",
       call. = FALSE)
}

#' @rdname fit_pinp
#' @param formula A formula specifying the model, e.g., `y ~ x1 + x2`.
#' @param data A data frame containing the variables in the formula.
#' @param method A character string specifying the method to use. Default is "knn_eqt".
#' @param fallback_formula An optional formula to use for the fallback linear model.
#'   If `NULL`, the main `formula` is used.
#' @param ... Additional arguments passed to the underlying fitting function,
#'   such as `k` for the kNN method.
#' @export
fit_pinp.formula <- function(formula, data, method = "knn_eqt", fallback_formula = NULL, ...) {
  # 1. Parse formula to get X and y
  mf <- model.frame(formula = formula, data = data)
  y <- model.response(mf)
  X <- model.matrix(attr(mf, "terms"), data = mf)

  # Remove intercept and convert to data.frame
  X <- as.data.frame(X[, colnames(X) != "(Intercept)", drop = FALSE])

  # 2. Determine the formula for the fallback linear model
  f_fallback <- if (is.null(fallback_formula)) {
    formula
  } else {
    fallback_formula
  }
  
  # Fit the fallback linear model
  fallback_model <- lm(f_fallback, data = data)

  # 3. Call the correct engine based on `method`
  model_internal <- switch(
    method,
    "knn_eqt" = {
      knn_eqt_fit(X = X, y = y, ...)
    },
    "kde_eqt" = {
      kde_eqt_fit(X = X, y = y, ...)
    },
    stop("Method '", method, "' not recognized.", call. = FALSE)
  )

  # 4. Return the final "pinp_fit" object
  fit <- list(
    method = method,
    model_internal = model_internal,
    fallback_model = fallback_model,
    formula = formula,
    call = match.call()
  )

  class(fit) <- "pinp_fit"
  return(fit)
}

#' @rdname fit_pinp
#' @param y A numeric vector of the response variable.
#' @param fallback_formula An optional formula to use for the fallback linear model.
#'   If `NULL`, a formula `y ~ .` is constructed from the provided `X` and `y`.
#' @export
fit_pinp.data.frame <- function(object, y, method = "knn_eqt", fallback_formula = NULL, ...) {
  # Ensure x is a data frame
  X <- as.data.frame(object)

  # Determine the formula for the fallback linear model
  f_fallback <- if (is.null(fallback_formula)) {
    y_name <- deparse(substitute(y))
    x_names <- names(X)
    as.formula(paste(y_name, "~", paste(x_names, collapse = " + ")))
  } else {
    fallback_formula
  }

  # Fit the fallback linear model
  fallback_data <- data.frame(y = y, X)
  fallback_model <- lm(f_fallback, data = fallback_data)

  # Call the correct engine based on `method`
  model_internal <- switch(
    method,
    "knn_eqt" = {
      knn_eqt_fit(X = X, y = y, ...)
    },
    "kde_eqt" = {
      kde_eqt_fit(X = X, y = y, ...)
    },
    stop("Method '", method, "' not recognized.", call. = FALSE)
  )

  # Create a formula for completeness (using the main formula if available, otherwise construct)
  # This part needs to be careful as 'object' is X, not a formula
  # For data.frame method, we construct a formula for the main model as well
  main_formula <- as.formula(paste("y ~", paste(names(X), collapse = " + ")))

  # Return the final "pinp_fit" object
  fit <- list(
    method = method,
    model_internal = model_internal,
    fallback_model = fallback_model,
    formula = main_formula, # Store the constructed main formula
    call = match.call()
  )

  class(fit) <- "pinp_fit"
  return(fit)
}

#' Predict Nonparametric Intervals
#'
#' `predict_intervals` is a generic function for predicting nonparametric
#' prediction intervals from a fitted `pinp` object.
#'
#' @param object A fitted object of class `pinp_fit`.
#' @param ... Additional arguments to be passed to the specific method.
#'
#' @return A data frame containing the predicted intervals and diagnostics.
#'
#' @export
predict_intervals <- function(object, ...) {
  UseMethod("predict_intervals")
}

#' @rdname predict_intervals
#' @param new_data A data frame containing the predictor variables for which
#'   to generate prediction intervals.
#' @param alpha A numeric value between 0 and 1 representing the significance
#'   level. The prediction interval will be of size `1 - alpha`.
#' @param use_fallback A logical value indicating whether to enable the
#'   fallback mechanism. Defaults to `TRUE`.
#' @export
predict_intervals.pinp_fit <- function(object, new_data, alpha = 0.1, use_fallback = TRUE, ...) {
  if (missing(new_data)) {
    stop("Argument 'new_data' must be provided.", call. = FALSE)
  }

  method <- object$method
  model_internal <- object$model_internal

  # Create the predictor matrix from new_data using the stored formula
  formula_terms <- delete.response(terms(object$formula))
  X_new <- model.matrix(formula_terms, data = new_data)
  
  # Remove intercept and convert to data.frame
  X_new <- as.data.frame(X_new[, colnames(X_new) != "(Intercept)", drop = FALSE])

  # Capture ... and filter them for the appropriate engine to avoid errors
  dot_args <- list(...)

  # Dispatch to the correct prediction engine
  results_list <- switch(
    method,
    "knn_eqt" = {
      engine_arg_names <- names(formals(knn_eqt_predict))
      filtered_dot_args <- dot_args[names(dot_args) %in% engine_arg_names]
      all_args <- c(list(model = model_internal, X_new = X_new, alpha = alpha, 
                         fallback_model = object$fallback_model, use_fallback = use_fallback), 
                    filtered_dot_args)
      do.call(knn_eqt_predict, all_args)
    },
    "kde_eqt" = {
      engine_arg_names <- names(formals(kde_eqt_predict))
      filtered_dot_args <- dot_args[names(dot_args) %in% engine_arg_names]
      all_args <- c(list(model = model_internal, X_new = X_new, alpha = alpha, 
                         fallback_model = object$fallback_model, use_fallback = use_fallback), 
                    filtered_dot_args)
      do.call(kde_eqt_predict, all_args)
    },
    stop("Method '", method, "' not recognized.", call. = FALSE)
  )

  # Combine the results into a single data frame
  results_df <- data.frame(
    lower = results_list$L,
    upper = results_list$U,
    width = results_list$U - results_list$L
  )
  
  # Add the diagnostic info, which has method-specific columns
  results_df <- cbind(results_df, results_list$info)

  return(results_df)
}

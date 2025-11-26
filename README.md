# pinpR: Robust Nonparametric Prediction Intervals in R

`pinpR` is an R package for generating nonparametric prediction intervals. It is designed to be a practical and interpretable tool for creating robust intervals that adapt to complex data patterns, such as non-constant variance (heteroscedasticity).

A key feature of `pinpR` is its diagnostic safety layer. When local neighborhood estimates are deemed unreliable (e.g., in data-sparse regions), the package automatically falls back to a more stable global linear model, ensuring the generated intervals are always sensible.

## Features

- **Two Main Engines**: k-Nearest Neighbors (`knn_eqt`) and Kernel Density Estimation (`kde_eqt`).
- **Robust Fallback**: Automatically switches from a local to a global linear model when local estimates are unreliable.
- **Customizable Models**: Supports custom formulas for the fallback linear model (e.g., `y ~ poly(x, 2)`).
- **Evaluation and Visualization**: Includes powerful functions for plotting (`plot`, `plot_compare`) and evaluating interval quality (`evaluate_pinp`).

## Installation

You can install the development version of `pinpR` from GitHub using `devtools`:

```r
# install.packages("devtools")
devtools::install_github("your-username/pinpR") # Replace with the actual repo path
```

## How It Works

`pinpR` estimates the conditional distribution of `Y` given `X` (`P(Y|X)`) to form prediction intervals.

### The kNN Engine (`method = "knn_eqt"`)

For each new point `x_new`, the kNN engine:

1. Finds the `k` nearest neighbors in the training data.
2. Calculates the distance to the farthest neighbor (`dist_k`).
3. **Checks for fallback**: If `dist_k` is greater than a threshold (`max_dist_fallback`), it assumes the neighbors are too far away to be reliable and switches to the global `lm` model for the prediction.
4. If no fallback is triggered, it calculates the `alpha/2` and `1-alpha/2` quantiles from the `y` values of the `k` neighbors to form the interval.

### The KDE Engine (`method = "kde_eqt"`)

For each new point `x_new`, the KDE engine:

1. Calculates kernel-based weights for all training points, where points closer to `x_new` receive higher weights.
2. Calculates the effective sample size (`eff_n`) from these weights.
3. **Checks for fallback**: If `eff_n` is less than a threshold (`min_eff_n`), it assumes the local sample is too small to be reliable and switches to the global `lm` model.
4. If no fallback is triggered, it uses the weighted training samples to estimate the conditional quantiles and form the interval.

---

## API Reference

### `fit_pinp()`

Fits the nonparametric model.

**Usage:**

```r
fit_pinp(formula, data, method = "knn_eqt", fallback_formula = NULL, ...)
fit_pinp(X, y, method = "knn_eqt", fallback_formula = NULL, ...)
```

**Parameters:**

- `formula`: A model formula (e.g., `y ~ X1`).
- `data`: A data frame containing the variables for the formula interface.
- `X`, `y`: A data frame of predictors and a vector of responses for the data frame interface.
- `method`: The engine to use. Either `"knn_eqt"` (default) or `"kde_eqt"`.
- `fallback_formula`: An optional formula for the global fallback `lm` model. If `NULL`, a standard linear model (`y ~ .`) is used.
- `...`: Additional arguments passed to the specific engine (e.g., `k` for kNN).

**Returns:** A `pinp_fit` object.

### `predict_intervals()`

Generates prediction intervals from a fitted model.

**Usage:**

```r
predict_intervals(object, new_data, alpha = 0.1, use_fallback = TRUE, ...)
```

**Parameters:**

- `object`: A fitted `pinp_fit` object.
- `new_data`: A data frame of new observations to predict on.
- `alpha`: The significance level for the `(1-alpha)` intervals. Defaults to `0.1`.
- `use_fallback`: A global switch to enable or disable the fallback mechanism. Defaults to `TRUE`.
- `...`: Additional arguments passed to the specific prediction engine (e.g., `max_dist_fallback` for kNN or `min_eff_n` for KDE).

**Returns:** A data frame containing the `lower` and `upper` bounds, interval `width`, and diagnostic information (`dist_k` or `eff_n`, and `warning`).

### `plot()`

Visualizes the prediction intervals from a single model.

**Usage:**

```r
plot(x, n_grid = 100, show_fallback = TRUE, ...)
```

**Parameters:**

- `x`: A fitted `pinp_fit` object.
- `n_grid`: The number of points to use for the prediction grid.
- `show_fallback`: If `TRUE`, highlights points where the fallback was triggered.
- `...`: Additional arguments passed to `predict_intervals` (e.g., `alpha`).

### `plot_compare()`

Visually compares the intervals from two different models.

**Usage:**

```r
plot_compare(model1, model2, data, n_grid = 100, ..., 
             model1_name = "Model 1", model2_name = "Model 2")
```

### `evaluate_pinp()`

Calculates performance metrics for prediction intervals.

**Usage:**

```r
evaluate_pinp(y_true, L, U, alpha = 0.1)
```

**Returns:** A data frame with `Coverage`, `Avg_Width`, and `Winkler_Score`.

---

## Full Example

```r
# 1. Generate Data (The "Wiggly" Parabola with a Gap)
# -----------------------------------------------------------
# We generate data following a trend: y = 0.5*x^2 + sin(2*x)
# We artificially create a sparse "gap" to force fallback behavior.
n_total <- 600
x_all <- runif(n_total, -3, 3)

# Create a gap: remove 95% of points between X1 = 0.5 and X1 = 2.0
keep_mask <- !((x_all > 0.5) & (x_all < 2.0)) | (runif(n_total) < 0.05)
x_train <- x_all[keep_mask]
x_train <- sort(x_train)

# Generate Training Y with heteroskedastic noise
# Noise increases as X moves away from 0
true_trend <- 0.5 * x_train^2 + sin(2 * x_train)
noise_sd <- 0.3 + 0.1 * abs(x_train)
y_train <- true_trend + rnorm(length(x_train), 0, noise_sd)
data_train <- data.frame(X1 = x_train, y = y_train)

# Generate Test Data (Uniformly covering the range to evaluate coverage)
# We include noise in test data because we are evaluating Prediction Intervals (PI)
n_test <- 500
x_test <- seq(-3, 3, length.out = n_test)
true_trend_test <- 0.5 * x_test^2 + sin(2 * x_test)
noise_sd_test <- 0.3 + 0.1 * abs(x_test)
y_test <- true_trend_test + rnorm(n_test, 0, noise_sd_test)
data_test <- data.frame(X1 = x_test, y = y_test)


# 2. Fit Models (with custom quadratic fallback)
# The fallback_formula is crucial for accurate predictions in the sparse region.
fit_knn <- fit_pinp(y ~ X1, data = data_train, method = "knn_eqt", k = 30,
                    fallback_formula = y ~ poly(X1, 2))
fit_kde <- fit_pinp(y ~ X1, data = data_train, method = "kde_eqt",
                    fallback_formula = y ~ poly(X1, 2))

# 3. Visualize
# The red 'x' marks clearly show where the fallback was triggered in the sparse data region.
plot(fit_knn, alpha = 0.1, main = "kNN Intervals with Fallback in Sparse Region")
plot_compare(fit_knn, fit_kde, data = data_train, alpha = 0.1)

# 4. Evaluate
preds_knn <- predict_intervals(fit_knn, new_data = data_test, alpha = 0.1)
eval_knn <- evaluate_pinp(data_test$y, preds_knn$lower, preds_knn$upper, alpha = 0.1)
print(eval_knn)s_knn$lower, preds_knn$upper, alpha = 0.1)
print(eval_knn)
```

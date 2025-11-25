test_that("API runs without errors (knn_eqt)", {
  set.seed(123)
  dat <- sim_hetero(n = 100)
  
  # fit_pinp.formula expects a formula and data
  fit <- fit_pinp(y ~ X1, data = dat, method = "knn_eqt", k = 15)
  
  X_new <- data.frame(X1 = seq(0, 1, len = 20))
  pi <- predict_intervals(fit, new_data = X_new, alpha = 0.1)
  
  # Corrected expectations based on predict_intervals.pinp_fit returning a data.frame
  expect_true(is.data.frame(pi))
  expect_equal(nrow(pi), nrow(X_new))
  expect_true("lower" %in% colnames(pi))
  expect_true("upper" %in% colnames(pi))
  expect_true(all(pi$lower <= pi$upper))
})

test_that("API runs without errors (kde_eqt)", {
  set.seed(123)
  dat <- sim_hetero(n = 100)
  
  # fit_pinp.formula expects a formula and data
  # k is not needed for KDE
  fit <- fit_pinp(y ~ X1, data = dat, method = "kde_eqt")
  
  X_new <- data.frame(X1 = seq(0, 1, len = 20))
  pi <- predict_intervals(fit, new_data = X_new, alpha = 0.1)
  
  # Expectations based on predict_intervals.pinp_fit returning a data.frame
  expect_true(is.data.frame(pi))
  expect_equal(nrow(pi), nrow(X_new))
  expect_true("lower" %in% colnames(pi))
  expect_true("upper" %in% colnames(pi))
  expect_true(all(pi$lower <= pi$upper))
})

test_that("knn_eqt fallback is triggered when k=1", {
  set.seed(123)
  dat <- sim_hetero(n = 100)
  
  # X and y are extracted from the formula and data
  X_new <- data.frame(X1 = 0.5) # A single new point
  
  # With k=1, effective_n will be 1, triggering fallback with default min_eff_n=5
  fit <- fit_pinp(y ~ X1, data = dat, method = "knn_eqt", k = 1)
  pi <- predict_intervals(fit, new_data = X_new, alpha = 0.1)
  
  # Expectations for fallback behavior
  expect_equal(pi$eff_n, 1)
  expect_false(is.na(pi$warning))
  expect_equal(pi$warning, "Fallback: Effective_n too low")
})
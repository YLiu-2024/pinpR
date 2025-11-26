# --- pinpR Demo 2: High-Dimensional Support ---
# This script demonstrates how pinpR handles multi-dimensional inputs.
# Both kNN and KDE engines have mechanisms to detect sparsity in high dimensions.

devtools::load_all()
library(ggplot2)
library(gridExtra)
library(grid) # Required for text rendering

set.seed(2024)

print("--- Starting Demo 2: High-Dimensional Support ---")

# Define output file
pdf_file <- "demo_multidim.pdf"
pdf(pdf_file, width = 10, height = 8)

# ==============================================================================
# 1. Data Generation (2D "Hole")
# ==============================================================================
# We generate 2D data (y = x1^2 + x2) with a missing corner (sparse region).
n <- 1000
X <- data.frame(x1 = runif(n, -2, 2), x2 = runif(n, -2, 2))

# Remove data in the top-right corner to create a "hole" (x1 > 1.5 AND x2 > 1.5)
mask <- !((X$x1 > 1.5) & (X$x2 > 1.5))
X_train <- X[mask, ]
y_train <- X_train$x1^2 + X_train$x2 + rnorm(nrow(X_train), 0, 0.5)

data_2d <- data.frame(y = y_train, X_train)

print(paste("Generated", nrow(data_2d), "training points with a hole in the top-right corner."))


# ==============================================================================
# 2. Fitting Models
# ==============================================================================
print("Fitting 2D Models...")

# kNN: Will monitor Euclidean distance in 2D space.
# If the distance to the k-th neighbor is large, it indicates a sparse region.
fit_knn <- fit_pinp(y ~ x1 + x2, data = data_2d, method = "knn_eqt", k = 20)

# KDE: Will use Product Kernel.
# We force a small bandwidth here to ensure it detects the sharp boundary of the hole.
fit_kde <- fit_pinp(y ~ x1 + x2, data = data_2d, method = "kde_eqt", bw_x = c(0.1, 0.1))


# ==============================================================================
# 3. Diagnostics in the Sparse Region
# ==============================================================================
print("Predicting at a sparse point (1.8, 1.8)...")

# This point is deep inside the "hole" we created.
X_sparse <- data.frame(x1 = 1.8, x2 = 1.8)

# Predict
pi_knn <- predict_intervals(fit_knn, X_sparse)
pi_kde <- predict_intervals(fit_kde, X_sparse)

# --- Create PDF Page 1: Diagnostics Table ---

# kNN Table
knn_diag <- pi_knn[, c("lower", "upper", "dist_k", "warning")]
knn_diag[, sapply(knn_diag, is.numeric)] <- round(knn_diag[, sapply(knn_diag, is.numeric)], 3)
tbl_knn <- tableGrob(knn_diag, rows = NULL)

# KDE Table
kde_diag <- pi_kde[, c("lower", "upper", "eff_n", "warning")]
kde_diag[, sapply(kde_diag, is.numeric)] <- round(kde_diag[, sapply(kde_diag, is.numeric)], 3)
tbl_kde <- tableGrob(kde_diag, rows = NULL)

# Layout
grid.arrange(
  textGrob("Diagnostics in Sparse Region (1.8, 1.8)", gp = gpar(fontsize = 16, fontface = "bold")),
  textGrob("kNN Diagnostics (Expect large dist_k & warning):", x = 0, hjust = 0),
  tbl_knn,
  textGrob("KDE Diagnostics (Expect small eff_n & warning):", x = 0, hjust = 0),
  tbl_kde,
  ncol = 1,
  heights = c(0.1, 0.1, 0.2, 0.1, 0.2)
)


# ==============================================================================
# 4. Quantitative Coverage Check (Overall)
# ==============================================================================
print("Checking overall coverage across 2D space...")

# Generate random test points across the full square [-2, 2] x [-2, 2]
# This includes both the dense region and the sparse hole.
X_test_grid <- data.frame(x1 = runif(500, -2, 2), x2 = runif(500, -2, 2))
y_true <- X_test_grid$x1^2 + X_test_grid$x2 + rnorm(500, 0, 0.5)

# Robust prediction (using default use_fallback = TRUE)
preds <- predict_intervals(fit_knn, X_test_grid, use_fallback = TRUE)

# Metrics
metrics <- evaluate_pinp(y_true, preds$lower, preds$upper)
metrics[, sapply(metrics, is.numeric)] <- round(metrics[, sapply(metrics, is.numeric)], 3)

# --- Create PDF Page 2: Metrics Table ---
tbl_metrics <- tableGrob(metrics, rows = NULL)

grid.arrange(
  textGrob("Overall 2D Coverage Metrics", gp = gpar(fontsize = 16, fontface = "bold")),
  tbl_metrics,
  ncol = 1,
  heights = c(0.2, 0.8)
)

dev.off()
print(paste("Demo complete. Output saved to:", pdf_file))


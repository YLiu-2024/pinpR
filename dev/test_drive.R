library(ggplot2)
library(gridExtra)

# Define output file
pdf_file <- "dev/pinpR_comprehensive_demo.pdf"
pdf(pdf_file, width = 12, height = 8)

print("--- Starting Comprehensive Demo ---")

# ==============================================================================
# CASE 1: LINEAR SPARSE DATA
# Objective: Demonstrate the standard safety net (Linear Fallback).
# ==============================================================================
print("1. Running Linear Sparse Demo...")

# Generate data: Dense on left, very sparse on right
set.seed(101)
dat_linear <- sim_sparse(n_dense = 500, n_sparse = 5)

# Fit Robust kNN (detects distance > 0.1)
fit_linear <- fit_pinp(
  y ~ X1, 
  data = dat_linear, 
  method = "knn_eqt", 
  k = 15, 
  max_dist_fallback = 0.1
)

# Plot A: Fallback ON (Default) -> Safe, wide linear interval
p1_robust <- plot(fit_linear, show_fallback = TRUE) +
  ggtitle("Case 1A: Robust kNN (Fallback ON)", 
          subtitle = "Detects sparsity and falls back to a global linear interval (*).") +
  coord_cartesian(ylim = c(-1, 4))

# Plot B: Fallback OFF -> Naive, misleadingly narrow interval
p1_naive <- plot(fit_linear, use_fallback = FALSE) +
  ggtitle("Case 1B: Naive kNN (Fallback OFF)", 
          subtitle = "Fails to cover sparse points due to contaminated neighbors.") +
  coord_cartesian(ylim = c(-1, 4))

# Arrange side-by-side
grid.arrange(p1_robust, p1_naive, ncol = 2, top = "Robustness Check: Linear Data")


# ==============================================================================
# CASE 2: NON-LINEAR SPARSE DATA
# Objective: Demonstrate custom fallback formulas (Quadratic Fallback).
# ==============================================================================
print("2. Running Non-Linear (Quadratic) Demo...")

# Generate parabolic data with a sparse gap
set.seed(102)
X_dense <- runif(400, -2, 0)
y_dense <- X_dense^2 + rnorm(400, 0, 0.2)
X_sparse <- c(0.5, 1.0, 1.5, 2.0) # Sparse tail
y_sparse <- X_sparse^2 + rnorm(4, 0, 0.2)
dat_curved <- data.frame(X1 = c(X_dense, X_sparse), y = c(y_dense, y_sparse))

# Fit kNN with a CUSTOM fallback formula
fit_curved <- fit_pinp(
  y ~ X1, 
  data = dat_curved, 
  method = "knn_eqt", 
  k = 20, 
  max_dist_fallback = 0.2,
  fallback_formula = y ~ poly(X1, 2) # <--- The magic happens here
)

# Plot A: Linear Fallback (Simulation of default behavior on curved data)
# We fit a separate default model just for comparison
fit_default <- fit_pinp(y ~ X1, data = dat_curved, method = "knn_eqt", k = 20, max_dist_fallback = 0.2)
p2_linear <- plot(fit_default, show_fallback = TRUE) +
  ggtitle("Case 2A: Default Linear Fallback", 
          subtitle = "Fails to capture the parabolic trend.") +
  coord_cartesian(ylim = c(-1, 6))

# Plot B: Quadratic Fallback
p2_poly <- plot(fit_curved, show_fallback = TRUE) +
  ggtitle("Case 2B: Custom Quadratic Fallback", 
          subtitle = "Perfectly extrapolates the curve using 'y ~ poly(X1, 2)'.") +
  coord_cartesian(ylim = c(-1, 6))

grid.arrange(p2_linear, p2_poly, ncol = 2, top = "Flexibility Check: Non-Linear Data")


# ==============================================================================
# CASE 3: MODEL COMPARISON
# Objective: Compare kNN vs KDE on heteroskedastic data.
# ==============================================================================
print("3. Running kNN vs KDE Comparison...")

# Generate heteroskedastic data (variance increases with X)
set.seed(103)
dat_hetero <- sim_hetero(n = 500)

# Fit both engines
fit_knn_h <- fit_pinp(y ~ X1, data = dat_hetero, method = "knn_eqt", k = 50)
fit_kde_h <- fit_pinp(y ~ X1, data = dat_hetero, method = "kde_eqt")

# Plot comparison
p3 <- plot_compare(
  fit_knn_h, 
  fit_kde_h, 
  data = dat_hetero,
  model1_name = "kNN (Blue)",
  model2_name = "KDE (Red)"
) +
  ggtitle("Case 3: kNN vs KDE on Heteroskedastic Data",
          subtitle = "KDE (Red) captures changing variance better than fixed-k kNN (Blue).")

grid.arrange(p3, top = "Engine Comparison")

# Close PDF
dev.off()
print(paste("--- Demo Complete! Output saved to:", pdf_file, "---"))


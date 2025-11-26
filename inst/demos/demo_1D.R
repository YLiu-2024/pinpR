# --- pinpR Demo 1: Robustness & Flexibility in 1D (kNN & KDE) ---
# This script demonstrates how BOTH kNN and KDE engines handle sparse data.
# It contrasts "Robust" (Fallback ON) vs "Naive" (Fallback OFF) behavior.

devtools::load_all() 
library(ggplot2)
library(gridExtra)
library(grid) 

set.seed(2024)

# Increase PDF height to accommodate all comparison plots
pdf("demo_1d.pdf", width = 12, height = 16)

print("--- Starting Demo 1: 1D Robustness (kNN & KDE) ---")


# ==============================================================================
# SCENARIO A: LINEAR SPARSE DATA
# Objective: Show standard linear fallback for BOTH kNN and KDE.
# ==============================================================================
print("1. Running Scenario A: Linear Data...")

# 1.1 Data Generation
# Dense on left, sparse gap, then a few points on right
X_dense <- runif(400, 0, 0.5)
y_dense <- 2 * X_dense + rnorm(400, 0, 0.2)
X_sparse <- seq(0.6, 1.0, length.out = 5) # The Gap
y_sparse <- 2 * X_sparse + rnorm(5, 0, 0.2)
dat_linear <- data.frame(X1 = c(X_dense, X_sparse), y = c(y_dense, y_sparse))

# --- kNN Section ---
# Fit kNN with strict distance threshold
fit_knn_lin <- fit_pinp(y ~ X1, data = dat_linear, method = "knn_eqt", 
                        k = 15, max_dist_fallback = 0.1)

g1 <- plot(fit_knn_lin, show_fallback = TRUE) +
  ggtitle("A1: Robust kNN (Linear Fallback)", "Trigger: Distance > 0.1") + 
  coord_cartesian(ylim = c(-1, 3))

g2 <- plot(fit_knn_lin, use_fallback = FALSE) +
  ggtitle("A2: Naive kNN (No Fallback)", "Fails: Contaminated neighbors") + 
  coord_cartesian(ylim = c(-1, 3))

# --- KDE Section ---
# Note: We force a small bandwidth (bw_x=0.05) so KDE sees the sparsity gap.
# If bandwidth is too large, KDE smooths over the gap and doesn't trigger fallback.
fit_kde_lin <- fit_pinp(y ~ X1, data = dat_linear, method = "kde_eqt", 
                        bw_x = 0.05)

g3 <- plot(fit_kde_lin, show_fallback = TRUE) +
  ggtitle("A3: Robust KDE (Linear Fallback)", "Trigger: Effective Sample Size < 5") + 
  coord_cartesian(ylim = c(-1, 3))

g4 <- plot(fit_kde_lin, use_fallback = FALSE) +
  ggtitle("A4: Naive KDE (No Fallback)", "Fails: Unstable density estimate in gap") + 
  coord_cartesian(ylim = c(-1, 3))

# Arrange Scenario A Plots
grid.arrange(g1, g2, g3, g4, ncol = 2, 
             top = textGrob("SCENARIO A: LINEAR DATA (kNN vs KDE)", gp=gpar(fontsize=16, fontface="bold")))


# ==============================================================================
# SCENARIO B: NON-LINEAR SPARSE DATA
# Objective: Show CUSTOM quadratic fallback for BOTH kNN and KDE.
# ==============================================================================
print("2. Running Scenario B: Non-Linear Data...")

# 2.1 Data Generation (Parabolic)
X_d <- runif(400, -1, 0)
y_d <- X_d^2 + rnorm(400, 0, 0.2)
X_s <- c(0.5, 1.0, 1.5, 1.8, 2.0) # Sparse Tail
y_s <- X_s^2 + rnorm(5, 0, 0.2)
dat_quad <- data.frame(X1 = c(X_d, X_s), y = c(y_d, y_s))

# --- kNN Section ---
# We supply fallback_formula = y ~ poly(X1, 2)
fit_knn_quad <- fit_pinp(y ~ X1, data = dat_quad, method = "knn_eqt", 
                         k = 20, max_dist_fallback = 0.2,
                         fallback_formula = y ~ poly(X1, 2)) # <--- Custom Formula

g5 <- plot(fit_knn_quad, show_fallback = TRUE) +
  ggtitle("B1: Robust kNN (Quadratic Fallback)", "Extrapolates curve correctly (*)") + 
  coord_cartesian(ylim = c(-1, 5))

g6 <- plot(fit_knn_quad, use_fallback = FALSE) +
  ggtitle("B2: Naive kNN (No Fallback)", "Fails to extrapolate trend") + 
  coord_cartesian(ylim = c(-1, 5))

# --- KDE Section ---
fit_kde_quad <- fit_pinp(y ~ X1, data = dat_quad, method = "kde_eqt", 
                         bw_x = 0.1, # Small BW to detect gaps
                         fallback_formula = y ~ poly(X1, 2)) # <--- Custom Formula

g7 <- plot(fit_kde_quad, show_fallback = TRUE) +
  ggtitle("B3: Robust KDE (Quadratic Fallback)", "Extrapolates curve correctly (*)") + 
  coord_cartesian(ylim = c(-1, 5))

g8 <- plot(fit_kde_quad, use_fallback = FALSE) +
  ggtitle("B4: Naive KDE (No Fallback)", "Unstable/Wide in sparse region") + 
  coord_cartesian(ylim = c(-1, 5))

# Arrange Scenario B Plots
grid.arrange(g5, g6, g7, g8, ncol = 2, 
             top = textGrob("SCENARIO B: NON-LINEAR DATA (kNN vs KDE)", gp=gpar(fontsize=16, fontface="bold")))


# ==============================================================================
# Part 3: Quantitative Metrics Table
# Objective: Prove Robust methods win on coverage.
# ==============================================================================
print("3. Calculating Metrics...")

# Test Data (Quadratic Truth)
X_test <- seq(-1, 2, length.out = 100)
y_test <- X_test^2 + rnorm(100, 0, 0.2)
dat_test <- data.frame(X1 = X_test, y = y_test)

# 1. Predict kNN Robust
pred_knn_r <- predict_intervals(fit_knn_quad, dat_test, use_fallback = TRUE)
# 2. Predict KDE Robust
pred_kde_r <- predict_intervals(fit_kde_quad, dat_test, use_fallback = TRUE)
# 3. Predict kNN Naive
pred_knn_n <- predict_intervals(fit_knn_quad, dat_test, use_fallback = FALSE)

# Evaluate
m1 <- evaluate_pinp(dat_test$y, pred_knn_r$lower, pred_knn_r$upper)
m2 <- evaluate_pinp(dat_test$y, pred_kde_r$lower, pred_kde_r$upper)
m3 <- evaluate_pinp(dat_test$y, pred_knn_n$lower, pred_knn_n$upper)

results <- rbind("kNN Robust (Quad)" = m1, "KDE Robust (Quad)" = m2, "kNN Naive" = m3)
results[, sapply(results, is.numeric)] <- round(results[, sapply(results, is.numeric)], 3)

# Render Table to PDF
tbl <- tableGrob(results)
grid.arrange(tbl, top = textGrob("Part 3: Quantitative Metrics (Compare Winkler Scores)", gp=gpar(fontsize=16, fontface="bold")))

dev.off()
print("Demo complete. Saved to 'inst/demos/demo.pdf'")


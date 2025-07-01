## .predictive_hazard_ratio

test_that("basic functionality with appropriate input sizes", {
  x_pred <- c(1, 2, 3)
  beta_samples <- matrix(c(0.5, 0.2, -0.1), nrow = 1, ncol = 3)
  result <- .predictive_hazard_ratio(x_pred, beta_samples)
  expected <- exp(beta_samples %*% x_pred)
  expect_equal(result, expected)
})

test_that("non-numeric input is handled correctly", {
  x_pred <- c("a", "b", "c")
  beta_samples <- matrix(1:3, nrow = 1, ncol = 3)
  expect_error(.predictive_hazard_ratio(x_pred, beta_samples))
})

## .predictive_survival
test_that("non-numeric input is handled correctly", {
  grid_width <- "one"  # Non-numeric grid_width
  out_slam <- matrix(c(0.1, 0.2, 0.3), nrow = 1, ncol = 3)
  x_pred <- c(1, 2, 3)
  beta_samples <- matrix(c(0.5, 0.2, -0.1), nrow = 1, ncol = 3)
  expect_error(.predictive_survival(grid_width, out_slam, x_pred, beta_samples))
})

##.smooth_hazard
test_that("smooth_hazard computes correctly", {
  out_slam <- c(0.1, 0.2, 0.3)
  beta_samples <- c(-0.1, 0.0, 0.1)
  result <- .smooth_hazard(out_slam, beta_samples)
  expected <- out_slam * exp(beta_samples)
  expect_equal(result, expected)
})

test_that("handling single element inputs correctly", {
  out_slam <- 0.2
  beta_samples <- -0.1
  result <- .smooth_hazard(out_slam, beta_samples)
  expected <- out_slam * exp(beta_samples)
  expect_equal(result, expected)
})

# .smooth_survival
test_that("smooth_survival computes correctly with treatment effect", {
  grid_width <- 1
  out_slam <- c(0.1, 0.2, 0.3)
  beta_samples <- c(-0.1, 0.0, 0.1)
  result <- .smooth_survival(grid_width, out_slam, beta_samples)
  integrand <- as.matrix(-(out_slam * exp(beta_samples))) %*% diag(grid_width)
  expected <- exp(t(apply(integrand, 1, cumsum)))
  expect_equal(result, expected)
})

test_that("smooth_survival computes correctly without treatment effect", {
  grid_width <- 1
  out_slam <- c(0.1, 0.2, 0.3)
  result <- .smooth_survival(grid_width, out_slam)
  integrand <- as.matrix(-out_slam) %*% diag(grid_width)
  expected <- exp(t(apply(integrand, 1, cumsum)))
  expect_equal(result, expected)
})


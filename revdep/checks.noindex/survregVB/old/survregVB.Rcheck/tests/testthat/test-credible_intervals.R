test_that("test beta_ci", {
  mu <- c(1.5, 2.0)
  Sigma <- matrix(c(0.5, 0.1, 0.1, 0.3), nrow = 2)
  ci <- 0.95

  result <- beta_ci(mu, Sigma, ci)
  expected <- matrix(
    c(
      0.11409618, 0.92648351,
      2.88590382, 3.07351649
    ),
    nrow = 2, byrow = FALSE,
    dimnames = list(NULL, c("CI.Lower", "CI.Upper"))
  )
  expect_equal(result, expected, tolerance = 1e-8)

  mu <- c(0.8, 1.3, 2.5)
  Sigma <- matrix(
    c(
      0.2, 0.05, 0.1,
      0.05, 0.15, 0.2,
      0.1, 0.2, 0.25
    ),
    nrow = 3, byrow = TRUE
  )
  ci <- 0.01

  result <- beta_ci(mu, Sigma, ci)
  expected <- matrix(
    c(
      0.79439486, 1.29514581, 2.49373327,
      0.80560514, 1.30485419, 2.50626673
    ),
    nrow = 3, byrow = FALSE,
    dimnames = list(NULL, c("CI.Lower", "CI.Upper"))
  )
  expect_equal(result, expected, tolerance = 1e-8)
})

test_that("test b_ci", {
  alpha <- 3
  omega <- 2
  ci <- 0.95

  result <- b_ci(alpha, omega, ci)
  expected <- c(0.18543676, 2.46243336)
  expect_equal(result, expected, tolerance = 1e-8)

  alpha <- 500
  omega <- 501
  ci <- 0.01

  result <- b_ci(alpha, omega, ci)
  expected <- c(1.00143749, 1.00247316)
  expect_equal(result, expected, tolerance = 1e-8)
})

context("Handling exceptions")

test_that("Wrong distro and parameters are handled correctly", {
  expect_error(rtrunc(10, family = "Poison"))
  expect_error(rtrunc(10, family = "Poisson", mean = 10))
})

test_that("It's OK to miss truncation limits", {
  set.seed(90)
  samp <- list(
    "norm" = rtrunc(3, family = "normal", mean = 0, sd = 1),
    "gamm" = rtrunc(3, family = "gamma", shape = 5, rate = 1),
    "logn" = rtrunc(3, family = "lognormal", meanlog = 0, sdlog = 1),
    "pois" = rtrunc(3, family = "poisson", lambda = 1)
  )
  expect_equal(
    object   = as.vector(dtrunc(samp$norm, eta = c(10, -2))),
    expected = c(6.357392e-06, 6.271760e-07, 9.013280e-11),
    check.attributes = FALSE
  )
  expect_equal(
    object   = as.vector(dtrunc(samp$gamm, eta = c(10, -2))),
    expected = c(0.09401543, 0.23025945, 0.14131485)
  )
  expect_equal(
    object   = as.vector(dtrunc(samp$logn, eta = c(10, -2))),
    expected = c(2.094700e-03, 9.567618e-09, 7.123690e-02)
  )

  expect_equal(
    object = as.vector(dtrunc(samp$pois, eta = -10)),
    expected = c(9.999546e-01, 1.030530e-09, 4.539787e-05)
  )
})

test_that("Passing wrong parameters is handled correctly", {
  msg <- "Invalid parameter domain."
  expect_error(rtrunc(1, family = "beta", shape1 = -1, shape2 = 10), msg)
  expect_error(rtrunc(1, family = "beta", shape1 = 11, shape2 = -1), msg)
  expect_error(rtrunc(1, family = "beta", shape1 = -9, shape2 = -1), msg)
  expect_error(rtruncbeta(1, shape1 = -1, shape2 = 10), msg)
  expect_error(rtruncbeta(1, shape1 = 11, shape2 = -1), msg)
  expect_error(rtruncbeta(1, shape1 = -9, shape2 = -1), msg)
  expect_error(rtrunc(1, family = "binomial", size = 1.4, prob = .1), msg)
  expect_error(rtrunc(1, family = "binomial", size = 100, prob = 20), msg)
  expect_error(rtrunc(1, family = "binomial", size = -10, prob = 20), msg)
  expect_error(rtruncbinom(1, size = 1.4, prob = .1), msg)
  expect_error(rtruncbinom(1, size = 100, prob = 20), msg)
  expect_error(rtruncbinom(1, size = -10, prob = 20), msg)
  expect_error(rtrunc(1, family = "chisq", df = 1.4), msg)
  expect_error(rtrunc(1, family = "chisq", df = -14), msg)
  expect_error(rtruncchisq(1, df = 1.4), msg)
  expect_error(rtruncchisq(1, df = -14), msg)
  expect_error(rtrunc(1, family = "contbern", lambda = 1.4), msg)
  expect_error(rtrunc(1, family = "contbern", lambda = -14), msg)
  expect_error(rcontbern(1, lambda = 1.4), "lambda must be in \\(0, 1\\)")
  expect_error(rtrunccontbern(1, lambda = 1.4), msg)
  expect_error(rtrunccontbern(1, lambda = -14), msg)
  expect_error(rtrunc(1, family = "exp", rate = -14), msg)
  expect_error(rtruncexp(1, rate = -14), msg)
  expect_error(rtrunc(1, family = "gamma", shape = -1, rate = 1), msg)
  expect_error(rtrunc(1, family = "gamma", shape = 1, rate = -1), msg)
  expect_error(rtrunc(1, family = "gamma", shape = -1, scale = 1), msg)
  expect_error(rtrunc(1, family = "gamma", shape = 1, scale = -1), msg)
  expect_error(rtrunc(1, family = "gamma", shape = -1, rate = -1), msg)
  expect_error(rtrunc(1, family = "gamma", shape = -1, rate = -1), msg)
  expect_error(rtrunc(1, family = "gamma", shape = -1, scale = -1), msg)
  expect_error(rtrunc(1, family = "gamma", shape = -1, scale = -1), msg)
  expect_error(rtruncgamma(1, shape = -1, rate = 1), msg)
  expect_error(rtruncgamma(1, shape = 1, rate = -1), msg)
  expect_error(rtruncgamma(1, shape = -1, scale = 1), msg)
  expect_error(rtruncgamma(1, shape = 1, scale = -1), msg)
  expect_error(rtruncgamma(1, shape = -1, rate = -1), msg)
  expect_error(rtruncgamma(1, shape = -1, rate = -1), msg)
  expect_error(rtruncgamma(1, shape = -1, scale = -1), msg)
  expect_error(rtruncgamma(1, shape = -1, scale = -1), msg)
  expect_error(rtrunc(1, family = "invgamma", shape = -1, rate = 10), msg)
  expect_error(rtrunc(1, family = "invgamma", shape = 10, rate = -1), msg)
  expect_error(rtrunc(1, family = "invgamma", shape = -1, scale = 10), msg)
  expect_error(rtrunc(1, family = "invgamma", shape = 10, scale = -1), msg)
  expect_error(rtrunc(1, family = "invgamma", shape = -1, rate = -1), msg)
  expect_error(rtrunc(1, family = "invgamma", shape = -1, rate = -1), msg)
  expect_error(rtrunc(1, family = "invgamma", shape = -1, scale = -1), msg)
  expect_error(rtrunc(1, family = "invgamma", shape = -1, scale = -1), msg)
  expect_error(rtruncinvgamma(1, shape = -1, rate = 10), msg)
  expect_error(rtruncinvgamma(1, shape = 10, rate = -1), msg)
  expect_error(rtruncinvgamma(1, shape = -1, scale = 10), msg)
  expect_error(rtruncinvgamma(1, shape = 10, scale = -1), msg)
  expect_error(rtruncinvgamma(1, shape = -1, rate = -1), msg)
  expect_error(rtruncinvgamma(1, shape = -1, rate = -1), msg)
  expect_error(rtruncinvgamma(1, shape = -1, scale = -1), msg)
  expect_error(rtruncinvgamma(1, shape = -1, scale = -1), msg)
  expect_error(rtrunc(1, family = "invgauss", m = 10, s = -1), msg)
  expect_error(rtrunc(1, family = "invgauss", m = -1, s = 10), msg)
  expect_error(rtrunc(1, family = "invgauss", m = -1, s = -1), msg)
  expect_error(rtruncinvgauss(1, m = 10, s = -1), msg)
  expect_error(rtruncinvgauss(1, m = -1, s = 10), msg)
  expect_error(rtruncinvgauss(1, m = -1, s = -1), msg)
  expect_error(rtrunc(1, family = "lognormal", meanlog = 4i, sdlog = 10), msg)
  expect_error(rtrunc(1, family = "lognormal", meanlog = 40, sdlog = -1), msg)
  expect_error(rtrunc(1, family = "lognormal", meanlog = 4i, sdlog = -1), msg)
  expect_error(rtrunclnorm(1, meanlog = 4i, sdlog = 10), msg)
  expect_error(rtrunclnorm(1, meanlog = 40, sdlog = -1), msg)
  expect_error(rtrunclnorm(1, meanlog = 4i, sdlog = -1), msg)
  expect_error(rtrunc(1, family = "nbinom", size = 1.4, prob = .1), msg)
  expect_error(rtrunc(1, family = "nbinom", size = 100, prob = 20), msg)
  expect_error(rtrunc(1, family = "nbinom", size = -10, prob = 20), msg)
  expect_error(rtrunc(1, family = "nbinom", size = 1.4, prob = .1), msg)
  expect_error(rtrunc(1, family = "nbinom", size = 100, prob = 20), msg)
  expect_error(rtrunc(1, family = "nbinom", size = -10, prob = 20), msg)
  expect_error(rtrunc(1, family = "nbinom", size = 1.4, mu = 10), msg)
  expect_error(rtrunc(1, family = "nbinom", size = -10, mu = 10), msg)
  expect_error(rtrunc(1, family = "nbinom", size = 1.4, mu = 1i), msg)
  expect_error(rtrunc(1, family = "nbinom", size = 100, mu = 1i), msg)
  expect_error(rtrunc(1, family = "nbinom", size = -10, mu = 1i), msg)
  expect_error(rtruncnbinom(1, size = 1.4, prob = .1), msg)
  expect_error(rtruncnbinom(1, size = 100, prob = 20), msg)
  expect_error(rtruncnbinom(1, size = -10, prob = 20), msg)
  expect_error(rtruncnbinom(1, size = 1.4, prob = .1), msg)
  expect_error(rtruncnbinom(1, size = 100, prob = 20), msg)
  expect_error(rtruncnbinom(1, size = -10, prob = 20), msg)
  expect_error(rtruncnbinom(1, size = 1.4, mu = 10), msg)
  expect_error(rtruncnbinom(1, size = -10, mu = 10), msg)
  expect_error(rtruncnbinom(1, size = 1.4, mu = 1i), msg)
  expect_error(rtruncnbinom(1, size = 100, mu = 1i), msg)
  expect_error(rtruncnbinom(1, size = -10, mu = 1i), msg)
  expect_error(rtrunc(1, family = "normal", mean = 4i, sd = 10), msg)
  expect_error(rtrunc(1, family = "normal", mean = 40, sd = -1), msg)
  expect_error(rtrunc(1, family = "normal", mean = 4i, sd = -1), msg)
  expect_error(rtrunclnorm(1, mean = 4i, sd = 10), msg)
  expect_error(rtrunclnorm(1, mean = 40, sd = -1), msg)
  expect_error(rtrunclnorm(1, mean = 4i, sd = -1), msg)
  expect_error(rtrunc(1, family = "poisson", lambda = -14), msg)
  expect_error(rtruncpois(1, lambda = -14), msg)
})

test_that("Passing too many parameters", {
  expect_error(
    object = rtruncgamma(1, shape = 5, rate = 8, scale = 7),
    regexp = "specify 'rate' or 'scale' but not both"
    )
})

test_that("Only passing some parameters is OK", {
  set.seed(6)
  x1 <- rtrunc(n = 1e3, family = "gaussian", mean = 50, sd = 12, a = 40, b = 60)
  set.seed(6)
  x2 <- rtrunc(1e3, "gaussian", FALSE, 50, 12, 40, 60)
  set.seed(6)
  x3 <- rtrunc(1e3, family = "gaussian", mean = 50, sd = 12, a = 40, b = 60)
  set.seed(6)
  x4 <- rtrunc(n = 1e3, "gaussian", mean = 50, sd = 12, a = 40, b = 60)

  expect_warning(
    rtrunc(n = 1e3, family = "gaussian",  FALSE, 50, sd = 12, a = 40, b = 60)
  )
  expect_warning(
    rtrunc(n = 1e3, family = "gaussian",  FALSE, mean = 50, 12, a = 40, b = 60)
  )
  expect_warning(
    rtrunc(n = 1e3, family = "gaussian",  FALSE, mean = 50, sd = 12, 40, b = 60)
  )
  expect_warning(
    rtrunc(n = 1e3, family = "gaussian",  FALSE, mean = 50, sd = 12, a = 40, 60)
  )

  set.seed(6)
  x5 <- suppressWarnings(
    rtrunc(n = 1e3, family = "gaussian",  FALSE, 50, sd = 12, a = 40, b = 60)
  )
  set.seed(6)
  x6 <- suppressWarnings(
    rtrunc(n = 1e3, family = "gaussian",  FALSE, mean = 50, 12, a = 40, b = 60)
  )
  set.seed(6)
  x7 <- suppressWarnings(
    rtrunc(n = 1e3, family = "gaussian",  FALSE, mean = 50, sd = 12, 40, b = 60)
  )
  set.seed(6)
  x8 <- suppressWarnings(
    rtrunc(n = 1e3, family = "gaussian",  FALSE, mean = 50, sd = 12, a = 40, 60)
  )

  expect_equal(x1, x2)
  expect_equal(x1, x3)
  expect_equal(x1, x4)
  expect_equal(x1, x5)
  expect_equal(x1, x6)
  expect_equal(x1, x7)
  expect_equal(x1, x8)

  expect_equal(
    unclass(mlEstimationTruncDist(x1)), c("mean" = 50, "sd" = 12), tol = 1e-1
  )
})

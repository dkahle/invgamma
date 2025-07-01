context("Support validation")

test_that("Impossible truncation limits are rejected", {
  sub <- "must be a subset"
  higher <- "must be higher"
  same <- "Identical truncation limits"
  nocomplex <- "may not contain complex numbers"
  expect_error(
    rtrunc(1, family = "beta", shape1 = 8, shape2 = 2, a = .5, b = .5), same
  )
  expect_error(
    rtrunc(1, family = "beta", shape1 = 8, shape2 = 2, a = 1, b = .7), sub
  )
  expect_error(rtruncbeta(1, shape1 = 8, shape2 = 2, a = 1, b = .7), sub)
  expect_error(rtruncbeta(1, shape1 = 8, shape2 = 2, a = .7, b = .6), higher)
  expect_error(rtruncbeta(1, shape1 = 8, shape2 = 2, a = .6, b = .6), same)
  expect_error(rtruncbinom(1, size = 5, prob = .4, a = 6, b = 7), sub)
  expect_error(rtruncbinom(1, size = 5, prob = .4, a = 4, b = 3), higher)
  expect_error(rtruncbinom(1, size = 5, prob = .4, a = 7, b = 7), same)
  expect_error(rtruncchisq(1, df = 8, a = -5, b = -2), sub)
  expect_error(rtruncchisq(1, df = 8, a = 5, b = 2), higher)
  expect_error(rtruncchisq(1, df = 8, a = 5, b = 5), same)
  expect_error(rtrunccontbern(1, lambda = .3, a = -3, b = -2), sub)
  expect_error(rtrunccontbern(1, lambda = .3, a = .5, b = .3), higher)
  expect_error(rtrunccontbern(1, lambda = .3, a = 1), same)
  expect_error(rtruncexp(1, rate = 8, a = -1, b = 0), sub)
  expect_error(rtruncexp(1, rate = 8, a = 1, b = 0.5), higher)
  expect_error(rtruncexp(1, rate = 8, a = 0.5, b = 0.5), same)
  expect_error(rtruncgamma(1, shape = 5, rate = 2, a = -2, b = -1), sub)
  expect_error(rtruncgamma(1, shape = 5, rate = 2, a = 2, b = 1), higher)
  expect_error(rtruncgamma(1, shape = 5, rate = 2, a = 1, b = 1), same)
  expect_error(rtruncinvgamma(1, shape = 5, rate = 2, a = -2, b = -1), sub)
  expect_error(rtruncinvgamma(1, shape = 5, rate = 2, a = 2, b = 1), higher)
  expect_error(rtruncinvgamma(1, shape = 5, rate = 2, a = 1, b = 1), same)
  expect_error(rtruncinvgauss(1, m = 2, s = 5, a = -3, b = 0), sub)
  expect_error(rtruncinvgauss(1, m = 2, s = 5, a = 3, b = 2), higher)
  expect_error(rtruncinvgauss(1, m = 2, s = 5, a = 3, b = 3), same)
  expect_error(rtrunclnorm(1, meanlog = 2, sdlog = 5, a = -3, b = 0), sub)
  expect_error(rtrunclnorm(1, meanlog = 2, sdlog = 5, a = 3, b = 2), higher)
  expect_error(rtrunclnorm(1, meanlog = 2, sdlog = 5, a = 3, b = 3), same)
  expect_error(rtruncnorm(1, mean = 0, sd = 1, a = 1i, b = -1i), nocomplex)
  expect_error(rtruncnorm(1, mean = 0, sd = 1, a = 1, b = -1i), nocomplex)
  expect_error(rtruncnorm(1, mean = 0, sd = 1, a = 1i, b = 1), nocomplex)
  expect_error(rtruncnorm(1, mean = 0, sd = 1, a = 1, b = -1), higher)
  expect_error(rtruncnorm(1, mean = 0, sd = 1, a = 1, b = 1), same)
  expect_error(rtruncpois(1, lambda = 10, a = -10, b = -1), sub)
  expect_error(rtruncpois(1, lambda = 10, a = 2, b = 1), higher)
  expect_error(rtruncpois(1, lambda = 10, a = 10, b = 10), same)
})

test_that("Redundant truncation limits are detected", {
  msg <- "are not a subset"
  expect_warning(rtruncbeta(1, shape1 = 8, shape2 = 2, a = -0.1), msg)
  expect_warning(rtruncbeta(1, shape1 = 8, shape2 = 2, b = 1.1), msg)
  expect_warning(rtruncbinom(1, size = 5, prob = .4, a = -2), msg)
  expect_warning(rtruncbinom(1, size = 5, prob = .4, b = 6), msg)
  expect_warning(rtruncchisq(1, df = 8, a = -5), msg)
  expect_warning(rtrunccontbern(1, lambda = .4, a = -4), msg)
  expect_warning(rtruncexp(1, rate = 8, a = -7), msg)
  expect_warning(rtruncgamma(1, shape = 3, rate = 4, a = -5), msg)
  expect_warning(rtruncinvgamma(1, shape = 3, rate = 4, a = -5), msg)
  expect_warning(rtruncinvgauss(1, m = 0.3, s = 4, a = -5), msg)
  expect_warning(rtrunclnorm(1, meanlog = -1, sdlog = 6, a = -5), msg)
  expect_warning(rtruncpois(1, lambda = 10, a = -7), msg)
})

test_that("Edge cases are treated correctly", {
  n <- 10
  expect_setequal(suppressWarnings(rtruncbinom(n, 3, .4, -3, 0)), rep(0, n))
  expect_setequal(suppressWarnings(rtruncbinom(n, 3, .4, 3, 4)), rep(3, n))
  expect_setequal(
    suppressWarnings(
      rtruncnbinom(n, size = 3, prob = .4, a = -3, b = 0)
    ),
    rep(0, n)
  )
  expect_setequal(suppressWarnings(rtruncpois(10, 4, -1, 0)), rep(0, n))
})

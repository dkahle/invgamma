context("dtrunc")

sample_size <- 10
sample <- list(
  "beta"      = rtruncbeta(sample_size, shape1 = 15, shape2 = 4),
  "binomial"  = rtruncbinom(sample_size, prob = 0.6, size = 20),
  "chisq"     = rtruncchisq(sample_size, df = 50),
  "contbern"  = rtrunccontbern(sample_size, lambda = .4),
  "exp"       = rtruncexp(sample_size, rate = 6),
  "gamma"     = rtruncgamma(sample_size, shape = 6, rate = 2, a = 2),
  "invgamma"  = rtruncinvgamma(sample_size, shape = 23, rate = 24),
  "invgauss"  = rtruncinvgauss(sample_size, m = 3, s = 1),
  "lognormal" = rtrunclnorm(sample_size, meanlog = 2.5, sdlog = 0.5),
  "nbinom"    = rtruncnbinom(sample_size, size = 50, prob = .3),
  "normal"    = rtruncnorm(sample_size, mean = 2, sd = 1.5),
  "poisson"   = rtruncpois(sample_size, lambda = 10)
)

test_that("dtrunc* works like its stats counterpart", {
  expect_equal(dtrunc(sample$beta, 15, 4), dbeta(sample$beta, 15, 4))
  expect_equal(dtrunc(sample$binomial, 20, .6), dbinom(sample$binomial, 20, .6))
  expect_equal(dtrunc(sample$chisq, 50), dchisq(sample$chisq, 50))
  expect_equal(dtrunc(sample$contbern, .4), dcontbern(sample$contbern, .4))
  expect_equal(dtrunc(sample$exp), dexp(sample$exp))
  expect_equal(dtrunc(sample$gamma, 6, 2), dgamma(sample$gamma, 6, 2))
  expect_equal(
    dtrunc(sample$invgamma, 23, 24), dinvgamma(sample$invgamma, 23, 24)
  )
  expect_equal(dtrunc(sample$invgauss, 3, 1), dinvgauss(sample$invgauss, 3, 1))
  expect_equal(
    dtrunc(sample$lognormal, 2.5, .5), dlnorm(sample$lognormal, 2.5, .5)
  )
  expect_equal(dtrunc(sample$nbinom, 50, .3), dnbinom(sample$nbinom, 50, .3))
  expect_equal(dtrunc(sample$normal), dnorm(sample$normal))
  expect_equal(dtrunc(sample$poisson, 10), dpois(sample$poisson, 10))
})

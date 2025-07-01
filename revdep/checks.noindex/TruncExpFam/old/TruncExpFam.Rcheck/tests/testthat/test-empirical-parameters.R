context("Extracting empirical parameters from numeric class")

set.seed(1418193)
x <- rtrunc(n = 100, lambda = 2, family = "Poisson")
attributes(x) <- NULL
y <- x / 10

test_that("Parameters are properly named", {
  expect_error(empiricalParameters(x, family = "beta"), "outside of support")
  expect_named(empiricalParameters(y, family = "beta"), c("shape1", "shape2"))
  expect_error(
    empiricalParameters(x, family = "binomial", nsize = 3),
    "outside of support"
  )
  expect_named(
    empiricalParameters(x, family = "binomial", nsize = max(x)),
    c("size", "prob")
  )
  expect_named(empiricalParameters(x, family = "chisq"), "df")
  expect_error(
    empiricalParameters(x, family = "contbern"), "outside of support"
  )
  expect_named(empiricalParameters(x, family = "poisson"), "lambda")
})

test_that("Natural parameters are properly named", {
  expect_named(empiricalParameters(y, family = "beta", TRUE), c("eta1", "eta2"))
  expect_named(empiricalParameters(x, family = "binomial", TRUE), "eta")
  expect_named(empiricalParameters(x, family = "chisq", TRUE), "eta")
  expect_named(empiricalParameters(x, family = "poisson", TRUE), "eta")
})

context("Extracting empirical parameters from trunc_ class")

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

test_that("empiricalParameters are properly named", {
  for (distro in names(sample)) {
    expected_parms <- valid_fam_parm[[distro]][["parms"]]
    empirical_parameters <- switch(distro,
      "nbinom" = empiricalParameters(sample[[distro]], r = 50, k = 50),
      empiricalParameters(sample[[distro]])
    )
    expect_named(empirical_parameters, expected_parms)
  }
})

context("Estimating from numeric (classless) vectors")

set.seed(2828324)

cont01 <- rbeta(1e4, shape1 = 10, shape2 = 100)
cont01b <- cont01 / max(cont01)  # better resembles a contbern candidate
disc0inf <- rpois(1e4, lambda = 10)
cont0inf <- rchisq(1e4, df = 10)
cont0infb <- exp(-cont0inf)
contReal <- rnorm(1e4, mean = 10, sd = 10)

test_that("Estimation works, in general", {
  # Improper samples
  expect_error(
    mlEstimationTruncDist(contReal), "choose an underlying distribution"
  )
  expect_error(
    mlEstimationTruncDist(contReal, family = "beta"), "outside of support"
  )

  # Proper samples
  expect_named(
    mlEstimationTruncDist(cont01, family = "beta"), c("shape1", "shape2")
  )
  expect_named(
    mlEstimationTruncDist(disc0inf, family = "binomial"), "prob"
  )
  expect_named(
    mlEstimationTruncDist(cont0inf, family = "chisq"), "df"
  )
  expect_named(
    mlEstimationTruncDist(cont01b, family = "contbern"), "lambda"
  )
  expect_named(
    mlEstimationTruncDist(cont0infb, family = "exp"), "rate"
  )
  expect_named(
    mlEstimationTruncDist(cont0inf, family = "gamma"), c("shape", "rate")
  )
  expect_named(
    mlEstimationTruncDist(cont0inf, family = "invgamma"), c("shape", "rate")
  )
  expect_named(
    mlEstimationTruncDist(cont0inf, family = "invgauss", tol = .1), c("m", "s")
  )
  expect_named(
    mlEstimationTruncDist(cont0inf, family = "lognormal"), c("meanlog", "sdlog")
  )
  expect_named(
    mlEstimationTruncDist(disc0inf, family = "nbinom"), "mean"
  )
  expect_named(
    mlEstimationTruncDist(contReal, family = "normal"), c("mean", "sd")
  )
  expect_named(
    mlEstimationTruncDist(disc0inf, family = "poisson"), "lambda"
  )
})

test_that("Original parameters are retrieved", {
  mlBeta <- mlEstimationTruncDist(cont01, family = "beta")
  expect_equal(
    mlBeta,
    c("shape1" = 10, "shape2" = 100),
    tol = 1e-1, check.attributes = FALSE
  )
  expect_equal(mlBeta[["shape1"]] / sum(mlBeta), mean(cont01), tol = 1e-1)
  expect_equal(
    mlEstimationTruncDist(disc0inf, family = "binomial"),
    c("prob" = mean(disc0inf) / max(disc0inf)),
    tol = 1e-1, check.attributes = FALSE
  )
  expect_equal(
    mlEstimationTruncDist(cont0inf, family = "chisq"),
    c("df" = mean(disc0inf)),
    tol = 1e-1, check.attributes = FALSE
  )
  expect_equal(
    mlEstimationTruncDist(cont01b, family = "contbern"),
    c("lambda" = mean(cont01b)),
    tol = 1e-1, check.attributes = FALSE
  )
  expect_equal(
    mlEstimationTruncDist(cont0infb, family = "exp"),
    c("rate" = mean(cont0infb)),
    tol = 1e-1, check.attributes = FALSE
  )
  mlGamma <- mlEstimationTruncDist(cont0inf, family = "gamma")
  expect_equal(
    mlGamma,
    c("shape" = 4.961, "rate" = 0.498),
    tol = 1e-1, check.attributes = FALSE
  )
  expect_equal(
    mlGamma[["shape"]] / mlGamma[["rate"]], mean(cont0inf), tol = 1e-1
  )
  mlInvGamma <- mlEstimationTruncDist(cont0inf, family = "invgamma")
  expect_equal(
    mlInvGamma,
    c("shape" = 6.972, "rate" = 59.397),
    tol = 1e-1, check.attributes = FALSE
  )
  expect_equal(
    mlInvGamma[["rate"]] / (mlInvGamma[["shape"]] - 1), mean(cont0inf),
    tol = 1e-1
  )
  expect_equal(
    mlEstimationTruncDist(cont0inf, family = "invgauss", tol = .1),
    c("m" = mean(cont0inf), "s" = var(cont0inf) / mean(cont0inf) ^ 3),
    tol = 1e0, check.attributes = FALSE
  )
  expect_equal(
    mlEstimationTruncDist(cont0inf, family = "lognormal"),
    c("mean" = mean(log(cont0inf)), "sd" = sd(log(cont0inf))),
    tol = 1e-1, check.attributes = FALSE
  )
  expect_equal(
    mlEstimationTruncDist(disc0inf, family = "nbinom"),
    c("mean" = mean(disc0inf)),
    tol = 1e-1, check.attributes = FALSE
  )
  expect_equal(
    mlEstimationTruncDist(contReal, family = "normal"),
    c("mean" = mean(contReal), "sd" = sd(contReal)),
    tol = 1e-1, check.attributes = FALSE
  )
  expect_equal(
    mlEstimationTruncDist(disc0inf, family = "poisson"),
    c("lambda" = mean(contReal)),
    tol = 1e-1, check.attributes = FALSE
  )
})

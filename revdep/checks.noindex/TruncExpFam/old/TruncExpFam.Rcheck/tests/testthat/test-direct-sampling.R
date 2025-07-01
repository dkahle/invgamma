context("Sampling directly from the truncated distibution")

test_that("Original attributes are retrieved", {
  set.seed(2723347)

  # Normal
  norm <- list(
    rtrunc(1e4, mean = 1, sd = 2, faster = TRUE),
    rtruncnorm(1e4, mean = 1, sd = 2, faster = TRUE)
  )
  for (norm_1 in norm) {
    expect_equal(
      mlEstimationTruncDist(norm_1),
      c("mean" = 1, "sd" = 2),
      tolerance = 1e-2
    )
    expect_equal(
      attributes(norm_1),
      list(
        "class" = "trunc_normal",
        "parameters" = list("mean" = 1, "sd" = 2),
        "truncation_limits" = list("a" = -Inf, "b" = Inf),
        "continuous" = TRUE
      )
    )
  }

  # Beta
  beta <- list(
    rtrunc(1e4, shape1 = 3, shape2 = 9, faster = TRUE, family = "beta"),
    rtruncbeta(1e4, shape1 = 3, shape2 = 9, faster = TRUE)
  )
  for (beta_1 in beta) {
    expect_equal(
      mlEstimationTruncDist(beta_1),
      c("shape1" = 3, "shape2" = 9),
      tolerance = 1e-1
    )
    expect_equal(
      attributes(beta_1),
      list(
        "class" = "trunc_beta",
        "parameters" = list("shape1" = 3, "shape2" = 9),
        "truncation_limits" = list("a" = 0, "b" = 1),
        "continuous" = TRUE
      )
    )
  }

  # Chisq
  chisq <- list(
    rtrunc(1e4, df = 30, faster = TRUE, family = "chisq"),
    rtruncchisq(1e4, df = 30, faster = TRUE)
  )
  for (chisq_1 in chisq) {
    expect_equal(
      attributes(chisq_1),
      list(
        "class" = "trunc_chisq",
        "parameters" = list("df" = 30),
        "truncation_limits" = list("a" = 0, "b" = Inf),
        "continuous" = TRUE
      )
    )
    expect_equal(
      mlEstimationTruncDist(chisq_1),
      c("df" = 30),
      tolerance = 1e-2
    )
  }

  # Contbern
  contbern <- list(
    rtrunc(1e4, lambda = .6, faster = TRUE, family = "contbern"),
    rtrunccontbern(1e4, lambda = .6, faster = TRUE)
  )
  for (contbern_1 in contbern) {
    expect_equal(
      attributes(contbern_1),
      list(
        "class" = "trunc_contbern",
        "parameters" = list("lambda" = .6),
        "truncation_limits" = list("a" = 0, "b" = 1),
        "continuous" = TRUE
      )
    )
    expect_equal(
      mlEstimationTruncDist(contbern_1),
      c("lambda" = .6),
      tolerance = 1e-1
    )
  }

  # Exp
  exp <- list(
    rtrunc(1e4, rate = 64, faster = TRUE, family = "exp"),
    rtruncexp(1e4, rate = 64, faster = TRUE)
  )
  for (exp_1 in exp) {
    expect_equal(
      attributes(exp_1),
      list(
        "class" = "trunc_exp",
        "parameters" = list("rate" = 64),
        "truncation_limits" = list("a" = 0, "b" = Inf),
        "continuous" = TRUE
      )
    )
    expect_equal(
      mlEstimationTruncDist(exp_1),
      c("rate" = 1 / 64),
      tolerance = 1e-2
    )
  }

  # Gamma
  gamma <- list(
    rtrunc(1e4, shape = 4, rate = 3, faster = TRUE, family = "gamma"),
    rtruncgamma(1e4, shape = 4, rate = 3, faster = TRUE)
  )
  for (gamma_1 in gamma) {
    expect_equal(
      attributes(gamma_1),
      list(
        "class" = "trunc_gamma",
        "parameters" = list("shape" = 4, "rate" = 3),
        "truncation_limits" = list("a" = 0, "b" = Inf),
        "continuous" = TRUE
      )
    )
    expect_equal(
      mlEstimationTruncDist(gamma_1),
      c("shape" = 4, "rate" = 3),
      tolerance = 1e-1
    )
  }

  gamma <- list(
    rtrunc(1e4, scale = 3, shape = 4, faster = TRUE, family = "gamma"),
    rtruncgamma(1e4, scale = 3, shape = 4, faster = TRUE)
  )
  for (gamma_2 in gamma) {
    expect_equal(
      attributes(gamma_2),
      list(
        "class" = "trunc_gamma",
        "parameters" = list("shape" = 4, "rate" = 1 / 3),
        "truncation_limits" = list("a" = 0, "b" = Inf),
        "continuous" = TRUE
      )
    )
    expect_equal(
      mlEstimationTruncDist(gamma_2),
      c("shape" = 4, "rate" = 1 / 3),
      tolerance = 1e-1
    )
  }

  # Inv Gamma
  invgamma <- list(
    rtrunc(1e4, rate = 3, shape = 4, faster = TRUE, family = "invgamma"),
    rtruncinvgamma(1e4, rate = 3, shape = 4, faster = TRUE)
  )
  for (invgamma_1 in invgamma) {
    expect_equal(
      attributes(invgamma_1),
      list(
        "class" = "trunc_invgamma",
        "parameters" = list("shape" = 4, "rate" = 3),
        "truncation_limits" = list("a" = 0, "b" = Inf),
        "continuous" = TRUE
      )
    )
    expect_equal(
      mlEstimationTruncDist(invgamma_1),
      c("shape" = 4, "rate" = 3),
      tolerance = 1e-1
    )
  }

  invgamma <- list(
    rtrunc(1e4, scale = 3, shape = 4, faster = TRUE, family = "invgamma"),
    rtruncinvgamma(1e4, scale = 3, shape = 4, faster = TRUE)
  )
  for (invgamma_2 in invgamma) {
    expect_equal(
      attributes(invgamma_2),
      list(
        "class" = "trunc_invgamma",
        "parameters" = list("shape" = 4, "rate" = 1 / 3),
        "truncation_limits" = list("a" = 0, "b" = Inf),
        "continuous" = TRUE
      )
    )
    expect_equal(
      mlEstimationTruncDist(invgamma_2),
      c("shape" = 4, "rate" = 1 / 3),
      tolerance = 1e-1
    )
  }

  # Inv Gauss
  invgausses <- list(
    rtrunc(1e3, m = 61, s = 7, faster = TRUE, family = "invgauss"),
    rtruncinvgauss(1e3, m = 61, s = 7, faster = TRUE)
  )
  for (invgauss in invgausses) {
    expect_equal(
      attributes(invgauss),
      list(
        "class" = "trunc_invgauss",
        "parameters" = list("m" = 61, "s" = 7),
        "truncation_limits" = list("a" = 0, "b" = Inf),
        "continuous" = TRUE
      )
    )
    expect_equal(
      mlEstimationTruncDist(invgauss, delta = .01),
      c("m" = 61, "s" = 7),
      tolerance = 1e1
    )
  }

  # Log-normal
  invlnorm <- list(
    rtrunc(1e4, meanlog = 4, sdlog = 1, faster = TRUE, family = "lognormal"),
    rtrunclnorm(1e4, meanlog = 4, sdlog = 1, faster = TRUE)
  )
  for (iv in invlnorm) {
    expect_equal(
      attributes(iv),
      list(
        "class" = "trunc_lognormal",
        "parameters" = list("meanlog" = 4, "sdlog" = 1),
        "truncation_limits" = list("a" = 0, "b" = Inf),
        "continuous" = TRUE
      )
    )
    expect_equal(
      mlEstimationTruncDist(iv),
      c("meanlog" = 4, "sdlog" = 1),
      tolerance = 1e-1
    )
  }

  # Poisson
  lb <- 29L
  poissons <- list(
    rtrunc(1e4, lambda = lb, faster = TRUE, family = "poisson"),
    rtruncpois(1e4, lambda = lb, faster = TRUE)
  )
  for (smp in poissons) {
    expect_equal(
      attributes(smp),
      list(
        "class" = "trunc_poisson",
        "parameters" = list("lambda" = lb),
        "truncation_limits" = list("a" = 0, "b" = Inf),
        "continuous" = FALSE
      )
    )
    expect_equal(
      mlEstimationTruncDist(smp),
      c("lambda" = lb),
      tolerance = 1e-1
    )
  }

  # Binomial
  sz <- rpois(1, 20)
  pb <- runif(1)
  binomials <- list(
    rtrunc(1e4, size = sz, prob = pb, faster = TRUE, family = "binomial"),
    rtruncbinom(1e4, size = sz, prob = pb, faster = TRUE)
  )
  for (smp in binomials) {
    expect_equal(
      attributes(smp),
      list(
        "class" = "trunc_binomial",
        "parameters" = list("size" = sz, "prob" = pb),
        "truncation_limits" = list("a" = 0, "b" = sz),
        "continuous" = FALSE
      )
    )
    expect_equal(
      mlEstimationTruncDist(smp),
      c("prob" = pb),
      tolerance = 1e-1
    )
  }

  # Negative Binomial
  sz <- rpois(1, 20)
  pb <- runif(1)
  nbinomials <- list(
    rtrunc(1e4, size = sz, prob = pb, faster = TRUE, family = "nbinom"),
    rtruncnbinom(1e4, size = sz, prob = pb, faster = TRUE)
  )
  for (smp in nbinomials) {
    expect_equal(
      attributes(smp),
      list(
        "class" = "trunc_nbinom",
        "parameters" = list("size" = sz, "prob" = pb),
        "truncation_limits" = list("a" = 0, "b" = Inf),
        "continuous" = FALSE
      )
    )
    expect_equal(
      mlEstimationTruncDist(smp),
      c("mean" = sz * (1 - pb) / pb),
      tolerance = 1e-1
    )
  }
})

test_that("Tight truncation limits is not a speed limiter", {
  time_limit <- 2
  n <- 1e4L

  # Normal
  expect_error({
      setTimeLimit(time_limit)
      rtrunc(n, mean = 5, sd = .2, a = 5.5)
    },
    "reached CPU time limit"
  )
  expect_length(rtrunc(n, mean = 5, sd = .2, a = 5.5, faster = TRUE), n)

  # Beta
  expect_error({
      setTimeLimit(time_limit)
      rtrunc(n, family = "beta", shape1 = 4, shape2 = 5, b = .02)
    },
    "reached CPU time limit"
  )
  expect_length(
    rtrunc(n, family = "beta", shape1 = 4, shape2 = 5, b = .02, faster = TRUE),
    n
  )

  # Chisq
  expect_error({
      setTimeLimit(time_limit)
      rtrunc(n, family = "chisq", df = 10, b = 1)
    },
    "reached CPU time limit"
  )
  expect_length(
    rtrunc(n, family = "chisq", df = 10, b = 1, faster = TRUE),
    n
  )

  # Contbern
  expect_error({
      setTimeLimit(time_limit)
      rtrunc(n, family = "contbern", lambda = .8, a = .1, b = .2)
    },
    "reached CPU time limit"
  )
  expect_length(
    rtrunc(n, family = "contbern", lambda = .8, a = .1, b = .2, faster = TRUE),
    n
  )

  # Exp
  expect_error({
      setTimeLimit(time_limit)
      rtrunc(n, rate = .2, family = "exp", b = .1)
    },
    "reached CPU time limit"
  )
  expect_length(
    rtrunc(n, rate = .2, family = "exp", b = .1, faster = TRUE),
    n
  )

  # Gamma
  expect_error({
      setTimeLimit(time_limit)
      rtrunc(n, family = "gamma", shape = 5, rate = 4, a = 4)
    },
    "reached CPU time limit"
  )
  expect_length(
    rtrunc(n, family = "gamma", shape = 5, rate = 4, a = 4, faster = TRUE),
    n
  )

  # Inv Gamma
  expect_error({
      setTimeLimit(time_limit)
      rtrunc(n, family = "invgamma", shape = 5, rate = 4, a = 4)
    },
    "reached CPU time limit"
  )
  expect_length(
    rtrunc(n, family = "invgamma", shape = 5, rate = 4, a = 4, faster = TRUE),
    n
  )

  # Inv Gauss
  expect_error({
      setTimeLimit(time_limit)
      rtrunc(n, family = "invgauss", m = 5, s = 4, a = 4, b = 100)
    },
    "reached CPU time limit"
  )
  expect_length(
    rtrunc(n, family = "invgauss", m = 5, s = 4, a = 4, b = 100, faster = TRUE),
    n
  )

  # Lognormal
  expect_error({
      setTimeLimit(time_limit)
      rtrunc(n, family = "lognormal", meanlog = 5, sdlog = 2, a = 150, b = 160)
    },
    "reached CPU time limit"
  )
  expect_length(
    rtrunc(
      n, family = "lognormal", meanlog = 5, sdlog = 2, a = 150, b = 160,
      faster = TRUE
    ),
    n
  )

  # Poisson
  n <- 1e5L
  expect_error({
      setTimeLimit(time_limit)
      rtrunc(n, family = "poisson", lambda = 10, a = 8, b = 15)
    },
    "reached CPU time limit"
  )
  expect_length(
    rtrunc(
      n, family = "poisson", lambda = 10, a = 8, b = 15, faster = TRUE),
    n
  )

  # Binomial
  n <- 1e5L
  expect_error({
      setTimeLimit(time_limit)
      rtrunc(n, family = "binomial", size = 10, prob = .1, a = 5)
    },
    "reached CPU time limit"
  )
  expect_length(
    rtrunc(n, family = "binomial", size = 10, prob = .1, a = 5, faster = TRUE),
    n
  )

  # Negative Binomial
  n <- 1e5L
  expect_error({
      setTimeLimit(time_limit)
      rtrunc(n, family = "nbinom", size = 100, prob = .8, b = 10)
    },
    "reached CPU time limit"
  )
  expect_length(
    rtrunc(n, family = "nbinom", size = 100, prob = .8, b = 10, faster = TRUE),
    n
  )
})

setTimeLimit(Inf)

test_that("Truncation limits are respected for faster = TRUE", {
  sz <- 1e3L
  smp <- rtrunc(
    sz, "beta", shape1 = 4, shape2 = 5, faster = TRUE, a = .5, b = .7
  )
  expect_gte(min(smp), .5)
  expect_lte(max(smp), .7)

  smp <- rtrunc(
    sz, "binomial", size = 93, prob = .39, faster = TRUE, a = 33, b = 40
  )
  expect_gte(min(smp), 33)
  expect_lte(max(smp), 40)

  smp <- rtrunc(sz, "chisq", df = 61, a = 50, b = 70, faster = TRUE)
  expect_gte(min(smp), 50)
  expect_lte(max(smp), 70)

  smp <- rtrunc(sz, "contbern", lambda = .44, faster = TRUE, a = .5, b = .8)
  expect_gte(min(smp), .5)
  expect_lte(max(smp), .8)

  smp <- rtrunc(sz, "exp", rate = 4, faster = TRUE, a = .1, b = .2)
  expect_gte(min(smp), .1)
  expect_lte(max(smp), .2)

  smp <- rtrunc(
    sz, "gamma", shape = 18, rate = 70, faster = TRUE, a = .2, b = .5
  )
  expect_gte(min(smp), .2)
  expect_lte(max(smp), .5)

  smp <- rtrunc(
    sz, "invgamma", shape = 1, rate = 7, faster = TRUE, a = 10, b = 20
  )
  expect_gte(min(smp), 10)
  expect_lte(max(smp), 20)

  smp <- rtrunc(sz, "invgauss", m = 56, s = 3, faster = TRUE, a = 30, b = 40)
  expect_gte(min(smp), 30)
  expect_lte(max(smp), 40)

  smp <- rtrunc(
    sz, "lognormal", meanlog = 23, sdlog = 6, faster = TRUE, a = 10, b = 100
  )
  expect_gte(min(smp), 10)
  expect_lte(max(smp), 100)

  smp <- rtrunc(
    sz, "nbinom", size = 54, mu = 33, faster = TRUE, a = 60, b = 80
  )
  expect_gte(min(smp), 60)
  expect_lte(max(smp), 80)

  smp <- rtrunc(sz, "normal", mean = 23, sd = 6, faster = TRUE, a = 30, b = 40)
  expect_gte(min(smp), 30)
  expect_lte(max(smp), 40)

  smp <- rtrunc(sz, "poisson", lambda = 886, faster = TRUE, a = 900, b = 910)
  expect_gte(min(smp), 900)
  expect_lte(max(smp), 910)
})

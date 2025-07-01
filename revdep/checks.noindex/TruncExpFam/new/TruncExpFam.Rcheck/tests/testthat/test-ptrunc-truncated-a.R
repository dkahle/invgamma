context("ptrunc(), lower truncation")

test_that("lower truncation works as expected (normal)", {
  lt <- TRUE
  lg <- FALSE
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(3L)) {
        mn <- rnorm(1L, sd = 10)
        sg <- rchisq(1L, 5L)
        qt <- rnorm(i, mn, sg)
        a <- min(qt) - rchisq(1L, 5L)
        p_trunc <- ptrunc(
          qt, lower.tail = lt, log.p = lg, mean = mn, sd = sg, a = a
        )
        p_norm <- pnorm(qt, lower.tail = lt, log.p = lg, mean = mn, sd = sg)
        expect_length(qt, i)
        expect_length(p_trunc, i)
        for (q in seq_along(qt)) {
          if (!lg) {
            expect_gte(p_trunc[q], 0)
            expect_lte(p_trunc[q], 1)
            if (lt) {
              expect_lte(p_trunc[q], p_norm[q])
            } else {
              expect_gte(p_trunc[q], p_norm[q])
            }
          } else {
            expect_lte(p_trunc[q], 0)
          }
        }
      }
    }
  }
})

test_that("lower truncation works as expected (beta)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(3L)) {
        shp1 <- sample(1:10, 1L)
        shp2 <- sample(1:10, 1L)
        a <- rbeta(1L, shp1, shp2)
        qt <- replicate(i, max(rbeta(10L, shp1, shp2), a))
        p_trunc <- ptrunc(
          qt, "beta", shp1, shp2, a = a, lower.tail = lt, log.p = lg
        )
        p_beta <- pbeta(qt, shp1, shp2, ncp = 0, lt, lg)
        expect_length(qt, i)
        expect_length(p_trunc, i)
        for (q in seq_along(qt)) {
          if (!lg) {
            expect_gte(p_trunc[q], 0)
            expect_lte(p_trunc[q], 1)
            if (lt) {
              expect_lte(p_trunc[q], p_beta[q])
            } else {
              expect_gt(p_trunc[q], p_beta[q])
            }
          } else {
            expect_lte(p_trunc[q], 0)
          }
        }
      }
    }
  }
})

test_that("lower truncation works as expected (binomial)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(3L)) {
        size <- sample(10:30, 1L)
        prob <- runif(1L, .2, .8)
        a <- sample(1:(size - 4L), 1L)
        qt <- sample(seq(a + 1L, size - 1L), i, replace = TRUE)
        p_trunc <- ptrunc(
          qt, "binomial", size, prob, a = a, lower.tail = lt, log.p = lg
        )
        p_binom <- pbinom(qt, size, prob, lower.tail = lt, log.p = lg)
        expect_length(qt, i)
        expect_length(p_trunc, i)
        for (q in seq_along(qt)) {
          if (!lg) {
            expect_gte(p_trunc[q], 0)
            expect_lte(p_trunc[q], 1)
          } else {
            expect_lte(p_trunc[q], 0)
          }
        }
      }
    }
  }
})

test_that("lower truncation works as expected (poisson)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(3L)) {
        lambda <- sample(10:50, 1L)
        max_qt <- qpois(p = .99, lambda)
        a <- sample(seq(1L, max_qt - 3L), 1L)
        qt <- sample(seq(a + 1L, max_qt), i, replace = TRUE)
        p_trunc <- ptrunc(
          qt, "poisson", lambda, a = a, lower.tail = lt, log.p = lg
        )
        p_pois <- ppois(qt, lambda, lower.tail = lt, log.p = lg)
        expect_length(qt, i)
        expect_length(p_trunc, i)
        for (q in seq_along(qt)) {
          if (!lg) {
            expect_gte(p_trunc[q], 0)
            expect_lte(p_trunc[q], 1)
          } else {
            expect_lte(p_trunc[q], 0)
          }
        }
      }
    }
  }
})

test_that("lower truncation works as expected (chisq)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(3L)) {
        df <- sample(1:100, 1L)
        a <- min(rchisq(10L, df))
        qt <- replicate(i, max(rchisq(10L, df), a))
        p_trunc <- ptrunc(
          qt, "chisq", df, a = a, lower.tail = lt, log.p = lg
        )
        p_chisq <- pchisq(qt, df, ncp = 0, lower.tail = lt, log.p = lg)
        expect_length(qt, i)
        expect_length(p_trunc, i)
        for (q in seq_along(qt)) {
          if (!lg) {
            expect_gte(p_trunc[q], 0)
            expect_lte(p_trunc[q], 1)
          } else {
            expect_lte(p_trunc[q], 0)
          }
        }
      }
    }
  }
})

test_that("lower truncation works as expected (contbern)", {
  for (i in seq_len(3L)) {
    lambda <- runif(1L)
    a <- runif(1L)
    qt <- runif(i, a, 1L)
    p_trunc <- ptrunc(qt, "contbern", lambda, a = a)
    p_contbern <- pcontbern(qt, lambda)
    expect_length(qt, i)
    expect_length(p_trunc, i)
    for (q in seq_along(qt)) {
      expect_lte(p_trunc[q], p_contbern[q])
    }
  }
})

test_that("lower truncation works as expected (exp)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(3L)) {
        rate <- rchisq(1L, df = 10L)
        a <- rexp(1L, rate)
        qt <- replicate(i, max(rexp(10L, rate), a))
        p_trunc <- ptrunc(
          qt, "exp", rate, a = a, lower.tail = lt, log.p = lg
        )
        p_exp <- pexp(qt, rate, lower.tail = lt, log.p = lg)
        expect_length(qt, i)
        expect_length(p_trunc, i)
        for (q in seq_along(qt)) {
          if (!lg) {
            expect_gte(p_trunc[q], 0)
            expect_lte(p_trunc[q], 1)
          } else {
            expect_lte(p_trunc[q], 0)

          }
        }
      }
    }
  }
})

test_that("lower truncation works as expected (gamma)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(3L)) {
        shape <- rchisq(1L, df = 10L)
        rate <- rchisq(1L, df = 10L)
        a <- rgamma(1L, shape, rate)
        qt <- replicate(i, max(rgamma(10L, shape, rate), a))
        p_trunc <- ptrunc(
          qt, "gamma", shape, rate, a = a, lower.tail = lt, log.p = lg
        )
        p_trunc_2 <- ptrunc(
          qt, "gamma", shape, scale = 1 / rate, a = a, lower.tail = lt,
          log.p = lg
        )
        p_gamma <- pgamma(qt, shape, rate, lower.tail = lt, log.p = lg)
        expect_length(qt, i)
        expect_length(p_trunc, i)
        for (q in seq_along(qt)) {
          if (!lg) {
            expect_gte(p_trunc[q], 0)
            expect_lte(p_trunc[q], 1)
            expect_gte(p_trunc_2[q], 0)
            expect_lte(p_trunc_2[q], 1)
          } else {
            expect_lte(p_trunc[q], 0)
            expect_lte(p_trunc_2[q], 0)
          }
        }
        expect_equal(p_trunc, p_trunc_2)
      }
    }
  }
})

test_that("lower truncation works as expected (invgamma)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(3L)) {
        shape <- rchisq(1L, df = 10L)
        rate <- rchisq(1L, df = 10L)
        a <- rinvgamma(1L, shape, rate)
        qt <- replicate(i, max(rinvgamma(10L, shape, rate), a))
        p_trunc <- ptrunc(
          qt, "invgamma", shape, rate, a = a, lower.tail = lt, log.p = lg
        )
        p_trunc_2 <- ptrunc(
          qt, "invgamma", shape, scale = 1 / rate, a = a, lower.tail = lt,
          log.p = lg
        )
        p_invgamma <- pinvgamma(qt, shape, rate, lower.tail = lt, log.p = lg)
        expect_length(qt, i)
        expect_length(p_trunc, i)
        for (q in seq_along(qt)) {
          if (!lg) {
            expect_gte(p_trunc[q], 0)
            expect_lte(p_trunc[q], 1)
            expect_gte(p_trunc_2[q], 0)
            expect_lte(p_trunc_2[q], 1)
          } else {
            expect_lte(p_trunc[q], 0)
            expect_lte(p_trunc_2[q], 0)
          }
        }
        expect_equal(p_trunc, p_trunc_2)
      }
    }
  }
})

test_that("lower truncation works as expected (invgauss)", {
  for (i in seq_len(3L)) {
    m <- rchisq(1L, df = 10L)
    s <- rchisq(1L, df = 10L)
    a <- rinvgauss(1L, m, s)
    qt <- replicate(i, max(rinvgauss(10L, m, s), a))
    p_trunc <- ptrunc(qt, "invgauss", m, s, a)
    p_invgauss <- pinvgauss(qt, m, s)
    expect_length(qt, i)
    expect_length(p_trunc, i)
    for (q in seq_along(qt)) {
      expect_gte(p_trunc[q], 0)
      expect_lte(p_trunc[q], 1)
      expect_lte(p_trunc[q], p_invgauss[q])
    }
  }
})

test_that("lower truncation works as expected (lognormal)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(3L)) {
        meanlog <- rnorm(1L, sd = 10)
        sdlog <- rchisq(1L, 5L)
        qt <- rlnorm(i, meanlog, sdlog)
        a <- rlnorm(1L, meanlog, sdlog)
        while (any(a > qt)) {
          a <- rlnorm(1L, meanlog, sdlog)
        }
        p_trunc <- ptrunc(
          qt, "lognormal", meanlog, sdlog, a = a, lower.tail = lt, log.p = lg
        )
        p_ln <- plnorm(qt, meanlog, sdlog, lower.tail = lt, log.p = lg)
        expect_length(qt, i)
        expect_length(p_trunc, i)
        for (q in seq_along(qt)) {
          if (!lg) {
            expect_gte(p_trunc[q], 0)
            expect_lte(p_trunc[q], 1)
            if (lt) {
              expect_lte(p_trunc[q], p_ln[q])
            } else {
              expect_gte(p_trunc[q], p_ln[q])
            }
          } else {
            expect_lte(p_trunc[q], 0)
          }
        }
      }
    }
  }
})

test_that("lower truncation works as expected (negative binomial)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(3L)) {
        size <- sample(1:10, 1L)
        prob <- runif(1)
        mu <- size * (1 - prob) / prob
        qt <- rnbinom(i, size, prob)
        a <- rnbinom(1L, size, prob)
        while (any(a > qt)) {
          a <- rnbinom(1L, size, prob)
        }
        p_trunc <- ptrunc(
          qt, "nbinom", size, prob, lower.tail = lt, log.p = lg, a = a
        )
        p_trunc_2 <- ptrunc(
          qt, "nbinom", size, mu = mu, lower.tail = lt, log.p = lg, a = a
        )
        p_binom <- pnbinom(qt, size, prob, lower.tail = lt, log.p = lg)
        expect_length(qt, i)
        expect_length(p_trunc, i)
        expect_equal(p_trunc, p_trunc_2, tolerance = 1e-6)
        for (q in seq_along(qt)) {
          if (!lg) {
            expect_gte(p_trunc[q], 0)
            expect_lte(p_trunc[q], 1)
            if (lt) {
              expect_lte(p_trunc[q], p_binom[q])
            } else {
              expect_gte(round(p_trunc[q], 6), round(p_binom[q], 6))
            }
          } else {
            expect_lte(p_trunc[q], 0)
          }
        }
      }
    }
  }
})

context("ptrunc(), untruncated")

test_that("untruncated ptrunc() works as expected (normal)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(3L)) {
        mn <- rnorm(1L, sd = 10)
        sg <- rchisq(1L, 5L)
        qt <- rnorm(i, mn, sg)
        p_trunc <- ptrunc(qt, lower.tail = lt, log.p = lg, mean = mn, sd = sg)
        p_norm <- pnorm(qt, lower.tail = lt, log.p = lg, mean = mn, sd = sg)
        expect_length(qt, i)
        expect_length(p_trunc, i)
        for (q in seq_along(qt)) {
          if (!lg) {
            # because I couldn't figure out the relationship between p_trunc
            # and p_norm in the log.p = TRUE case
            expect_gte(p_trunc[q], 0)
            expect_lte(p_trunc[q], 1)
          }
          expect_equal(p_trunc[q], p_norm[q])
        }
      }
    }
  }
})

test_that("untruncated ptrunc() works as expected (beta)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(3L)) {
        shp1 <- sample(1:10, 1L)
        shp2 <- sample(1:10, 1L)
        qt <- rbeta(i, shp1, shp2)
        p_trunc <- ptrunc(qt, "beta", shp1, shp2, lower.tail = lt, log.p = lg)
        p_beta <- pbeta(qt, shp1, shp2, ncp = 0, lt, lg)
        expect_length(qt, i)
        expect_length(p_trunc, i)
        for (q in seq_along(qt)) {
          if (!lg) {
            expect_gte(p_trunc[q], 0)
            expect_lte(p_trunc[q], 1)
          }
          expect_equal(p_trunc[q], p_beta[q])
        }
      }
    }
  }
})

test_that("untruncated ptrunc() works as expected (binomial)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(3L)) {
        size <- sample(1:10, 1L)
        prob <- runif(1)
        qt <- rbinom(i, size, prob)
        p_trunc <- ptrunc(
          qt, "binomial", size, prob, lower.tail = lt, log.p = lg
        )
        p_binom <- pbinom(qt, size, prob, lower.tail = lt, log.p = lg)
        expect_length(qt, i)
        expect_length(p_trunc, i)
        for (q in seq_along(qt)) {
          if (!lg) {
            expect_gte(p_trunc[q], 0)
            expect_lte(p_trunc[q], 1)
          }
          expect_equal(p_trunc[q], p_binom[q])
        }
      }
    }
  }
})

test_that("untruncated ptrunc() works as expected (poisson)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(3L)) {
        lambda <- sample(1:50, 1L)
        qt <- rpois(i, lambda)
        p_trunc <- ptrunc(qt, "poisson", lambda, lower.tail = lt, log.p = lg)
        p_pois <- ppois(qt, lambda, lower.tail = lt, log.p = lg)
        expect_length(qt, i)
        expect_length(p_trunc, i)
        for (q in seq_along(qt)) {
          if (!lg) {
            expect_gte(p_trunc[q], 0)
            expect_lte(p_trunc[q], 1)
          }
          expect_equal(p_trunc[q], p_pois[q])
        }
      }
    }
  }
})

test_that("untruncated ptrunc() works as expected (chisq)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(3L)) {
        df <- sample(1:10, 1L)
        qt <- rchisq(i, df)
        p_trunc <- ptrunc(qt, "chisq", df, lower.tail = lt, log.p = lg)
        p_chisq <- pchisq(qt, df, lower.tail = lt, log.p = lg)
        expect_length(qt, i)
        expect_length(p_trunc, i)
        for (q in seq_along(qt)) {
          if (!lg) {
            expect_gte(p_trunc[q], 0)
            expect_lte(p_trunc[q], 1)
          }
          expect_equal(p_trunc[q], p_chisq[q])
        }
      }
    }
  }
})

test_that("untruncated ptrunc() works as expected (contbern)", {
  for (i in seq_len(3L)) {
    lambda <- runif(1)
    qt <- rcontbern(i, lambda)
    p_trunc <- ptrunc(qt, "contbern", lambda)
    p_contbern <- pcontbern(qt, lambda)
    expect_length(qt, i)
    expect_length(p_trunc, i)
    for (q in seq_along(qt)) {
      expect_equal(p_trunc[q], p_contbern[q])
    }
  }
})

test_that("untruncated ptrunc() works as expected (exp)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(3L)) {
        rate <- runif(1)
        qt <- rexp(i, rate)
        p_trunc <- ptrunc(qt, "exp", rate, lower.tail = lt, log.p = lg)
        p_exp <- pexp(qt, rate, lower.tail = lt, log.p = lg)
        expect_length(qt, i)
        expect_length(p_trunc, i)
        for (q in seq_along(qt)) {
          if (!lg) {
            expect_gte(p_trunc[q], 0)
            expect_lte(p_trunc[q], 1)
          }
          expect_equal(p_trunc[q], p_exp[q])
        }
      }
    }
  }
})

test_that("untruncated ptrunc() works as expected (gamma)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(3L)) {
        shp <- rchisq(1L, df = 10L)
        rate <- rchisq(1L, df = 10L)
        qt <- rgamma(i, shp, rate)
        p_trunc <- ptrunc(qt, "gamma", shp, rate, lower.tail = lt, log.p = lg)
        p_trunc_2 <- ptrunc(
          qt, "gamma", shp, scale = 1 / rate, lower.tail = lt, log.p = lg
        )
        p_gamma <- pgamma(qt, shp, rate, lower.tail = lt, log.p = lg)
        expect_length(qt, i)
        expect_length(p_trunc, i)
        expect_equal(p_trunc, p_trunc_2)
        for (q in seq_along(qt)) {
          if (!lg) {
            expect_gte(p_trunc[q], 0)
            expect_lte(p_trunc[q], 1)
            expect_gte(p_trunc_2[q], 0)
            expect_lte(p_trunc_2[q], 1)
          }
          expect_equal(p_trunc[q], p_gamma[q])
          expect_equal(p_trunc_2[q], p_gamma[q])
        }
        expect_equal(p_trunc, p_trunc_2)
      }
    }
  }
})

test_that("untruncated ptrunc() works as expected (invgamma)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(3L)) {
        shp <- rchisq(1L, df = 10L)
        rate <- rchisq(1L, df = 10L)
        qt <- rinvgamma(i, shp, rate)
        p_trunc <- ptrunc(
          qt, "invgamma", shp, rate, lower.tail = lt, log.p = lg
        )
        p_trunc_2 <- ptrunc(
          qt, "invgamma", shp, scale = 1 / rate, lower.tail = lt, log.p = lg
        )
        p_invgamma <- pinvgamma(qt, shp, rate, lower.tail = lt, log.p = lg)
        expect_length(qt, i)
        expect_length(p_trunc, i)
        for (q in seq_along(qt)) {
          if (!lg) {
            expect_gte(p_trunc[q], 0)
            expect_lte(p_trunc[q], 1)
            expect_gte(p_trunc_2[q], 0)
            expect_lte(p_trunc_2[q], 1)
          }
          expect_equal(p_trunc[q], p_invgamma[q])
          expect_equal(p_trunc_2[q], p_invgamma[q])
        }
        expect_equal(p_trunc, p_trunc_2)
      }
    }
  }
})

test_that("untruncated ptrunc() works as expected (invgauss)", {
  for (i in seq_len(3L)) {
    m <- rchisq(1L, df = 10L)
    s <- rchisq(1L, df = 10L)
    qt <- rinvgauss(i, m, s)
    p_trunc <- ptrunc(qt, "invgauss", m, s)
    p_invgauss <- pinvgauss(qt, m, s)
    expect_length(qt, i)
    expect_length(p_trunc, i)
    for (q in seq_along(qt)) {
      expect_gte(p_trunc[q], 0)
      expect_lte(p_trunc[q], 1)
      expect_equal(p_trunc[q], p_invgauss[q])
    }
  }
})

test_that("untruncated ptrunc() works as expected (lognormal)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(3L)) {
        meanlog <- rnorm(1L, sd = 10)
        sdlog <- rchisq(1L, 5L)
        qt <- rlnorm(i, meanlog, sdlog)
        p_trunc <- ptrunc(
          qt, "lognormal", meanlog, sdlog, lower.tail = lt, log.p = lg
        )
        p_lnorm <- plnorm(qt, meanlog, sdlog, lower.tail = lt, log.p = lg)
        expect_length(qt, i)
        expect_length(p_trunc, i)
        for (q in seq_along(qt)) {
          if (!lg) {
            expect_gte(p_trunc[q], 0)
            expect_lte(p_trunc[q], 1)
          }
          expect_equal(p_trunc[q], p_lnorm[q])
        }
      }
    }
  }
})

test_that("untruncated ptrunc() works as expected (negative binomial)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(3L)) {
        size <- sample(1:10, 1L)
        prob <- runif(1)
        mu <- size * (1 - prob) / prob
        qt <- rnbinom(i, size, prob)
        p_trunc <- ptrunc(qt, "nbinom", size, prob, lower.tail = lt, log.p = lg)
        p_trunc_2 <- ptrunc(
          qt, "nbinom", size, mu = mu, lower.tail = lt, log.p = lg
        )
        p_binom <- pnbinom(qt, size, prob, lower.tail = lt, log.p = lg)
        expect_length(qt, i)
        expect_length(p_trunc, i)
        expect_equal(p_trunc, p_trunc_2, tolerance = 1e-06)
        for (q in seq_along(qt)) {
          if (!lg) {
            expect_gte(p_trunc[q], 0)
            expect_lte(p_trunc[q], 1)
          }
          expect_equal(p_trunc[q], p_binom[q])
        }
      }
    }
  }
})

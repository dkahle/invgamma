context("ptrunc(), upper truncation")

test_that("upper truncation works as expected (normal)", {
  lt <- TRUE
  lg <- FALSE
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(3L)) {
        mn <- rnorm(1L, sd = 10)
        sg <- rchisq(1L, 5L)
        qt <- rnorm(i, mn, sg)
        b <- max(qt) + rchisq(1L, 5L)
        p_trunc <- ptrunc(
          qt, lower.tail = lt, log.p = lg, mean = mn, sd = sg, b = b
        )
        p_norm <- pnorm(qt, lower.tail = lt, log.p = lg, mean = mn, sd = sg)
        expect_length(qt, i)
        expect_length(p_trunc, i)
        for (q in seq_along(qt)) {
          if (!lg) {
            expect_gte(p_trunc[q], 0)
            expect_lte(p_trunc[q], 1)
            if (lt) {
              expect_gte(p_trunc[q], p_norm[q])
            } else {
              expect_lte(p_trunc[q], p_norm[q])
            }
          } else {
            expect_lte(p_trunc[q], 0)
          }
        }
      }
    }
  }
})

test_that("upper truncation works as expected (beta)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(3L)) {
        shp1 <- sample(1:10, 1L)
        shp2 <- sample(1:10, 1L)
        b <- runif(1)
        qt <- runif(i, 0, b)
        p_trunc <- ptrunc(
          qt, "beta", shp1, shp2, b = b, lower.tail = lt, log.p = lg
        )
        p_beta <- pbeta(qt, shp1, shp2, ncp = 0, lt, lg)
        expect_length(qt, i)
        expect_length(p_trunc, i)
        for (q in seq_along(qt)) {
          if (!lg) {
            expect_gte(p_trunc[q], 0)
            expect_lte(p_trunc[q], 1)
            if (lt) {
              expect_gte(p_trunc[q], p_beta[q])
            } else {
              expect_lte(p_trunc[q], p_beta[q])
            }
          } else {
            expect_lte(p_trunc[q], 0)
          }
        }
      }
    }
  }
})

test_that("upper truncation works as expected (binomial)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(3L)) {
        size <- sample(10:50, 1L)
        prob <- runif(1)
        b <- sample(2:(size - 1L), 1L)
        qt <- sample(0:(b - 1L), i, replace = TRUE)
        p_trunc <- ptrunc(
          qt, "binomial", size, prob, b = b, lower.tail = lt, log.p = lg
        )
        p_binom <- pbinom(qt, size, prob, lower.tail = lt, log.p = lg)
        expect_length(qt, i)
        expect_length(p_trunc, i)
        for (q in seq_along(qt)) {
          if (!lg) {
            expect_gte(p_trunc[q], 0)
            expect_lte(p_trunc[q], 1)
            if (abs(p_trunc[q] - p_binom[q]) > 1e-10) {  # adding tolerance
              if (lt) {
                expect_gte(p_trunc[q], p_binom[q])
              } else {
                expect_lte(p_trunc[q], p_binom[q])
              }
            }
          } else {
            expect_lte(p_trunc[q], 0)
          }
        }
      }
    }
  }
})

test_that("upper truncation works as expected (poisson)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(3L)) {
        lambda <- sample(10:50, 1L)
        max_qt <- qpois(p = .99, lambda)
        b <- sample(seq(lambda, max_qt), 1L)
        qt <- sample(seq(1L, b - 1L), i, replace = TRUE)
        p_trunc <- ptrunc(
          qt, "poisson", lambda, b = b, lower.tail = lt, log.p = lg
        )
        p_pois <- ppois(qt, lambda, lower.tail = lt, log.p = lg)
        expect_length(qt, i)
        expect_length(p_trunc, i)
        for (q in seq_along(qt)) {
          if (!lg) {
            expect_gte(p_trunc[q], 0)
            expect_lte(p_trunc[q], 1)
            if (abs(p_trunc[q] - p_pois[q]) > 1e-10) {  # adding tolerance
              if (lt) {
                expect_gte(p_trunc[q], p_pois[q])
              } else {
                expect_lte(p_trunc[q], p_pois[q])
              }
            }
          } else {
            expect_lte(p_trunc[q], 0)
          }
        }
      }
    }
  }
})

test_that("upper truncation works as expected (chisq)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(3L)) {
        df <- sample(1:100, 1L)
        b <- max(rchisq(10L, df))
        qt <- runif(i, 0, b)
        p_trunc <- ptrunc(
          qt, "chisq", df, b = b, lower.tail = lt, log.p = lg
        )
        p_chisq <- pchisq(qt, df, ncp = 0, lower.tail = lt, log.p = lg)
        expect_length(qt, i)
        expect_length(p_trunc, i)
        for (q in seq_along(qt)) {
          if (!lg) {
            expect_gte(p_trunc[q], 0)
            expect_lte(p_trunc[q], 1)
            if (abs(p_trunc[q] - p_chisq[q]) > 1e-10) {  # adding tolerance
              if (lt) {
                expect_gte(p_trunc[q], p_chisq[q])
              } else {
                expect_lte(p_trunc[q], p_chisq[q])
              }
            }
          } else {
            expect_lte(p_trunc[q], 0)
          }
        }
      }
    }
  }
})

test_that("upper truncation works as expected (contbern)", {
  for (i in seq_len(3L)) {
    lambda <- runif(1L)
    b <- runif(1L)
    qt <- runif(i, 0L, b)
    p_trunc <- ptrunc(qt, "contbern", lambda, b = b)
    p_contbern <- pcontbern(qt, lambda)
    expect_length(qt, i)
    expect_length(p_trunc, i)
    for (q in seq_along(qt)) {
      expect_gte(p_trunc[q], p_contbern[q])
    }
  }
})

test_that("upper truncation works as expected (exp)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(3L)) {
        rate <- rchisq(1L, df = 10L)
        b <- rexp(1L, rate)
        qt <- replicate(i, min(rexp(10L, rate), b))
        p_trunc <- ptrunc(
          qt, "exp", rate, b = b, lower.tail = lt, log.p = lg
        )
        p_exp <- pexp(qt, rate, lower.tail = lt, log.p = lg)
        expect_length(qt, i)
        expect_length(p_trunc, i)
        for (q in seq_along(qt)) {
          if (!lg) {
            expect_gte(p_trunc[q], 0)
            expect_lte(p_trunc[q], 1)
            if (abs(p_trunc[q] - p_exp[q]) > 1e-10) {  # adding tolerance
              if (lt) {
                expect_gte(p_trunc[q], p_exp[q])
              } else {
                expect_lte(p_trunc[q], p_exp[q])
              }
            }
          } else {
            expect_lte(p_trunc[q], 0)
          }
        }
      }
    }
  }
})

test_that("upper truncation works as expected (gamma)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(3L)) {
        shp <- rchisq(1L, df = 10L)
        rte <- rchisq(1L, df = 10L)
        b <- rgamma(1L, shp, rte)
        qt <- runif(i, 0, b)
        p_trunc <- ptrunc(
          qt, "gamma", shp, rate = rte, b = b, lower.tail = lt, log.p = lg
        )
        p_trunc_2 <- ptrunc(
          qt, "gamma", shp, scale = 1 / rte, b = b, lower.tail = lt, log.p = lg
        )
        p_gamma <- pgamma(qt, shp, rate = rte, lower.tail = lt, log.p = lg)
        expect_length(qt, i)
        expect_length(p_trunc, i)
        for (q in seq_along(qt)) {
          if (!lg) {
            expect_gte(p_trunc[q], 0)
            expect_lte(p_trunc[q], 1)
            expect_gte(p_trunc_2[q], 0)
            expect_lte(p_trunc_2[q], 1)
            if (abs(p_trunc[q] - p_gamma[q]) > 1e-10) {  # adding tolerance
              if (lt) {
                expect_gte(p_trunc[q], p_gamma[q])
                expect_gte(p_trunc_2[q], p_gamma[q])
              } else {
                expect_lte(p_trunc[q], p_gamma[q])
                expect_lte(p_trunc_2[q], p_gamma[q])
              }
            }
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

test_that("upper truncation works as expected (invgamma)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(3L)) {
        shp <- rchisq(1L, df = 10L)
        rte <- rchisq(1L, df = 10L)
        b <- rinvgamma(1L, shp, rte)
        qt <- runif(i, 0, b)
        p_trunc <- ptrunc(
          qt, "invgamma", shp, rate = rte, b = b, lower.tail = lt, log.p = lg
        )
        p_trunc_2 <- ptrunc(
          qt, "invgamma", shp, scale = 1 / rte, b = b, lower.tail = lt,
          log.p = lg
        )
        p_invgamma <- pinvgamma(
          qt, shp, rate = rte, lower.tail = lt, log.p = lg
        )
        expect_length(qt, i)
        expect_length(p_trunc, i)
        for (q in seq_along(qt)) {
          if (!lg) {
            expect_gte(p_trunc[q], 0)
            expect_lte(p_trunc[q], 1)
            expect_gte(p_trunc_2[q], 0)
            expect_lte(p_trunc_2[q], 1)
            if (abs(p_trunc[q] - p_invgamma[q]) > 1e-10) {  # adding tolerance
              if (lt) {
                expect_gte(p_trunc[q], p_invgamma[q])
                expect_gte(p_trunc_2[q], p_invgamma[q])
              } else {
                expect_lte(p_trunc[q], p_invgamma[q])
                expect_lte(p_trunc_2[q], p_invgamma[q])
              }
            }
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

test_that("upper truncation works as expected (invgauss)", {
  for (i in seq_len(3L)) {
    m <- rchisq(1L, df = 10L)
    s <- rchisq(1L, df = 10L)
    b <- rinvgauss(1L, m, s)
    qt <- replicate(i, min(rinvgauss(10L, m, s), b))
    p_trunc <- ptrunc(qt, "invgauss", m, s, b = b)
    p_invgauss <- pinvgauss(qt, m, s)
    expect_length(qt, i)
    expect_length(p_trunc, i)
    for (q in seq_along(qt)) {
      expect_gte(p_trunc[q], 0)
      expect_lte(p_trunc[q], 1)
      expect_gte(p_trunc[q], p_invgauss[q])
    }
  }
})

test_that("upper truncation works as expected (lognormal)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(3L)) {
        meanlog <- rnorm(1L, sd = 10)
        sdlog <- rchisq(1L, 5L)
        qt <- rlnorm(i, meanlog, sdlog)
        b <- rlnorm(1L, meanlog, sdlog)
        while (any(b < qt)) {
          b <- rlnorm(1L, meanlog, sdlog)
        }
        p_trunc <- ptrunc(
          qt, "lognormal", meanlog, sdlog, b = b, lower.tail = lt, log.p = lg
        )
        p_ln <- plnorm(qt, meanlog, sdlog, lower.tail = lt, log.p = lg)
        expect_length(qt, i)
        expect_length(p_trunc, i)
        for (q in seq_along(qt)) {
          if (!lg) {
            expect_gte(p_trunc[q], 0)
            expect_lte(p_trunc[q], 1)
            if (abs(p_trunc[q] - p_ln[q]) > 1e-10) {  # adding tolerance
              if (lt) {
                expect_gte(p_trunc[q], p_ln[q])
              } else {
                expect_lte(p_trunc[q], p_ln[q])
              }
            }
          } else {
            expect_lte(p_trunc[q], 0)
          }
        }
      }
    }
  }
})

test_that("upper truncation works as expected (negative binomial)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(3L)) {
        size <- sample(1:10, 1L)
        prob <- runif(1)
        mu <- size * (1 - prob) / prob
        qt <- rnbinom(i, size, prob)
        b <- rnbinom(1L, size, prob)
        while (any(b < qt)) {
          b <- rnbinom(1L, size, prob)
        }
        p_trunc <- ptrunc(
          qt, "nbinom", size, prob, lower.tail = lt, log.p = lg, b = b
        )
        p_trunc_2 <- ptrunc(
          qt, "nbinom", size, mu = mu, lower.tail = lt, log.p = lg, b = b
        )
        p_binom <- pnbinom(qt, size, prob, lower.tail = lt, log.p = lg)
        expect_length(qt, i)
        expect_length(p_trunc, i)
        expect_equal(p_trunc, p_trunc_2, tolerance = 1e-6)
        for (q in seq_along(qt)) {
          if (!lg) {
            expect_gte(p_trunc[q], 0)
            expect_lte(p_trunc[q], 1)
            if (abs(p_trunc[q] - p_binom[q]) > 1e-10) {  # adding tolerance
              if (lt) {
                expect_gte(p_trunc[q], p_binom[q])
              } else {
                expect_lte(p_trunc[q], p_binom[q])
              }
            }
          } else {
            expect_lte(p_trunc[q], 0)
          }
        }
      }
    }
  }
})

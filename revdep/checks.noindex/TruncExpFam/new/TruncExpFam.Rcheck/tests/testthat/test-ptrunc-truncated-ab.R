context("ptrunc(), doubly truncated")

test_that("doubly-truncated ptrunc works as expected (normal)", {
  lt <- TRUE
  lg <- FALSE
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(3L)) {
        mn <- rnorm(1L, sd = 10)
        sg <- rchisq(1L, 5L)
        qt <- rnorm(i, mn, sg)
        a <- min(qt) - rchisq(1L, 5L)
        b <- max(qt) + rchisq(1L, 5L)
        p_trunc <- ptrunc(
          qt, "gaussian", mn, sg, a, b, lower.tail = lt, log.p = lg
        )
        p_norm <- pnorm(qt, lower.tail = lt, log.p = lg, mean = mn, sd = sg)
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

test_that("doubly-truncated ptrunc() works as expected (beta)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(3L)) {
        shp1 <- sample(1:10, 1L)
        shp2 <- sample(1:10, 1L)
        b <- runif(1)
        a <- b * runif(1)
        qt <- runif(i, a, b)
        p_trunc <- ptrunc(
          qt, "beta", shp1, shp2, a, b, lower.tail = lt, log.p = lg
        )
        p_beta <- pbeta(qt, shp1, shp2, ncp = 0, lt, lg)
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

test_that("doubly-truncated ptrunc() works as expected (binomial)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(3L)) {
        size <- sample(10:50, 1L)
        prob <- runif(1L)
        qt <- rbinom(i, size, prob)
        a  <- rbinom(1L, size, prob)
        b  <- rbinom(1L, size, prob)
        while (any(a > qt)) {
          a <- rbinom(1L, size, prob)
        }
        while (any(b < qt)) {
          b <- rbinom(1L, size, prob)
        }
        p_trunc <- ptrunc(
          qt, "binomial", size, prob, a, b, lower.tail = lt, log.p = lg
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

test_that("doubly-truncated ptrunc() works as expected (poisson)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(3L)) {
        lambda <- sample(1:50, 1L)
        a <- sample(1:(lambda - 4L), 1L)
        b <- sample((a + 3L):lambda, 1L)
        qt <- sample(seq(a + 1L, b - 1L), i, replace = TRUE)
        p_trunc <- ptrunc(
          qt, "poisson", lambda, a, b, lower.tail = lt, log.p = lg
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

test_that("doubly-truncated ptrunc() works as expected (chisq)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(3L)) {
        df <- sample(1:100, 1L)
        a <- min(rchisq(10L, df))
        b <- max(rchisq(10L, df))
        qt <- runif(i, a, b)
        p_trunc <- ptrunc(
          qt, "chisq", df, a, b, lower.tail = lt, log.p = lg
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

test_that("doubly-truncated ptrunc() works as expected (contbern)", {
  for (i in seq_len(3L)) {
    lambda <- runif(1L)
    a <- runif(1L)
    b <- runif(1L, a, 1L)
    qt <- runif(i, a, b)
    p_trunc <- ptrunc(qt, "contbern", lambda, b = b)
    p_contbern <- pcontbern(qt, lambda)
    expect_length(qt, i)
    expect_length(p_trunc, i)
    for (q in seq_along(qt)) {
      expect_gte(p_trunc[q], p_contbern[q])
    }
  }
})

test_that("doubly-truncated ptrunc() works as expected (exp)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(3L)) {
        rate <- rchisq(1L, df = 10L)
        a <- rexp(1L, rate)
        b <- rexp(1L, rate)
        while (b <= a) {
          b <- rexp(1L, rate)
        }
        qt <- runif(i, a, b)
        p_trunc <- ptrunc(
          qt, "exp", rate, a, b, lower.tail = lt, log.p = lg
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

test_that("doubly-truncated ptrunc() works as expected (gamma)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(3L)) {
        shp <- rchisq(1L, df = 10L)
        rte <- rchisq(1L, df = 10L)
        ab <- rgamma(2L, shp, rte)
        a <- min(ab)
        b <- max(ab)
        qt <- runif(i, a, b)
        p_trunc <- ptrunc(
          qt, "gamma", shape = shp, rate = rte, a = a, b = b, lower.tail = lt,
          log.p = lg
        )
        p_trunc_2 <- ptrunc(
          qt, "gamma", shape = shp, scale = 1 / rte, a = a, b = b,
          lower.tail = lt, log.p = lg
        )
        p_gamma <- pgamma(qt, shp, rte, lower.tail = lt, log.p = lg)
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

test_that("doubly-truncated ptrunc() works as expected (invgamma)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(3L)) {
        shp <- rchisq(1L, df = 10L)
        rte <- rchisq(1L, df = 10L)
        ab <- rinvgamma(2L, shp, rte)
        a <- min(ab)
        b <- max(ab)
        qt <- runif(i, a, b)
        p_trunc <- ptrunc(
          qt, "invgamma", shape = shp, rate = rte, a = a, b = b,
          lower.tail = lt, log.p = lg
        )
        p_trunc_2 <- ptrunc(
          qt, "invgamma", shape = shp, scale = 1 / rte, a = a, b = b,
          lower.tail = lt, log.p = lg
        )
        p_invgamma <- pinvgamma(qt, shp, rte, lower.tail = lt, log.p = lg)
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

test_that("doubly-truncated ptrunc() works as expected (invgauss)", {
  for (i in seq_len(3L)) {
    m <- rchisq(1L, df = 10L)
    s <- rchisq(1L, df = 10L)
    a <- min(rinvgauss(10L, m, s))
    b <- max(rinvgauss(10L, m, s), a)
    qt <- runif(i, a, b)
    p_trunc <- ptrunc(qt, "invgauss", m, s, a = a, b = b)
    p_invgauss <- pinvgauss(qt, m, s)
    expect_length(qt, i)
    expect_length(p_trunc, i)
    for (q in seq_along(qt)) {
      expect_gte(p_trunc[q], 0)
      expect_lte(p_trunc[q], 1)
    }
  }
})

test_that("doubly-truncated ptrunc() works as expected (lognormal)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(3L)) {
        meanlog <- rnorm(1L, sd = 10)
        sdlog <- rchisq(1L, 5L)
        qt <- rlnorm(i, meanlog, sdlog)
        a <- rlnorm(1L, meanlog, sdlog)
        b <- rlnorm(1L, meanlog, sdlog)
        while (any(a > qt)) {
          a <- rlnorm(1L, meanlog, sdlog)
        }
        while (any(b < qt)) {
          b <- rlnorm(1L, meanlog, sdlog)
        }
        p_trunc <- ptrunc(
          qt, "lognormal", meanlog, sdlog, a = a, b = b, lower.tail = lt,
          log.p = lg
        )
        p_ln <- plnorm(qt, meanlog, sdlog, lower.tail = lt, log.p = lg)
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

test_that("doubly-truncated ptrunc() works as expected (negative binomial)", {
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(3L)) {
        size <- sample(1:10, 1L)
        prob <- runif(1)
        mu <- size * (1 - prob) / prob
        qt <- rnbinom(i, size, prob)
        b <- rnbinom(1L, size, prob)
        a <- rnbinom(1L, size, prob)
        while (any(a > qt)) {
          a <- rnbinom(1L, size, prob)
        }
        while (any(b < qt)) {
          b <- rnbinom(1L, size, prob)
        }
        p_trunc <- ptrunc(
          qt, "nbinom", size, prob, lower.tail = lt, log.p = lg, a = a, b = b
        )
        p_trunc_2 <- ptrunc(
          qt, "nbinom", size, mu = mu, lower.tail = lt, log.p = lg, a = a, b = b
        )
        p_binom <- pnbinom(qt, size, prob, lower.tail = lt, log.p = lg)
        expect_length(qt, i)
        expect_length(p_trunc, i)
        expect_equal(p_trunc, p_trunc_2, tolerance = 1e-6)
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

test_that("Basic errors are caught", {
  for (distro in valid_distros) {
    expect_error(ptrunc(2, distro, 1, 1, a = 3, b = 4), "must be in \\[a, b\\]")
    expect_error(ptrunc(2, distro, 1, 1, a = 0, b = 1), "must be in \\[a, b\\]")
    expect_error(ptrunc(2, distro, 1, 1, a = 3, b = 1), "a must be <= b")
  }
  expect_error(
    ptrunc(9, "gamma", scale = 2, rate = 3),
    "specify 'rate' or 'scale' but not both"
  )
  expect_error(
    ptrunc(9, "invgamma", scale = 2, rate = 3),
    "specify 'rate' or 'scale' but not both"
  )
})

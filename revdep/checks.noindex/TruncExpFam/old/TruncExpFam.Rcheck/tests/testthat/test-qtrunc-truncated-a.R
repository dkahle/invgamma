context("qtrunc, lower truncation")

test_that("qtrunc() works as expected (beta)", {
  fam <- "beta"
  for (lg in c(FALSE, TRUE)) {
    for (lt in c(TRUE, FALSE)) {
      for (i in seq_len(3L)) {
        shp1 <- sample(1:10, 1L)
        shp2 <- sample(1:10, 1L)
        pt <- runif(i)
        if (lg) pt <- log(pt)
        a <- qtrunc(min(pt) / 2, fam, shp1, shp2, lower.tail = lt, log.p = lg)
        q_trunc <- qtrunc(
          pt, fam, shp1, shp2, a = a, lower.tail = lt, log.p = lg
        )
        q_stats <- qbeta(pt, shp1, shp2, lower.tail = lt, log.p = lg)
        expect_length(pt, i)
        expect_length(q_trunc, i)
        for (ii in seq_along(pt)) {
          expect_gt(q_trunc[ii], q_stats[ii])
          # Working back to p from q
          ptr <- ptrunc(
            q_trunc[ii], fam, shp1, shp2, lower.tail = lt, log.p = lg, a = a
          )
          expect_equal(pt[ii], ptr)
        }
      }
    }
  }
})

test_that("qtrunc() works as expected (binomial)", {
  fam <- "binomial"
  for (lg in c(FALSE, TRUE)) {
    for (lt in c(TRUE, FALSE)) {
      for (i in seq_len(3L)) {
        sz <- sample(1:10, 1L)
        pb <- runif(1)
        pt <- runif(i)
        if (lg) pt <- log(pt)
        a <- qtrunc(min(pt) / 2, fam, sz, pb, lower.tail = lt, log.p = lg)
        q_trunc <- qtrunc(pt, fam, sz, pb, a = a, lower.tail = lt, log.p = lg)
        q_stats <- qbinom(pt, sz, pb, lower.tail = lt, log.p = lg)
        expect_length(pt, i)
        expect_length(q_trunc, i)
        for (ii in seq_along(pt)) {
          expect_gte(q_trunc[ii], q_stats[ii])
          # Working back to p from q
          q_lo <- max(q_trunc[ii] - 1L, 0L, a)
          q_hi <- min(q_trunc[ii] + 1L, sz)
          ptr_1 <- ptrunc(q_lo, fam, sz, pb, a = a, lower.tail = lt, log.p = lg)
          ptr_2 <- ptrunc(q_hi, fam, sz, pb, a = a, lower.tail = lt, log.p = lg)
          # because pt will have been rounded
          if (q_trunc[ii] > 0L && q_lo > a) {
            if (lt) {
              expect_gte(pt[ii], ptr_1)
              expect_lte(pt[ii], ptr_2)
            } else {
              expect_lte(pt[ii], ptr_1)
              expect_gte(pt[ii], ptr_2)
            }
          }
        }
      }
    }
  }
})

test_that("qtrunc() works as expected (chisq)", {
  fam <- "chisq"
  for (lg in c(FALSE, TRUE)) {
    for (lt in c(TRUE, FALSE)) {
      for (i in seq_len(3L)) {
        df <- sample(1:10, 1L)
        pt <- runif(i)
        if (lg) pt <- log(pt)
        a <- min(qtrunc(pt, fam, df, lower.tail = lt, log.p = lg) / 2)
        q_trunc <- qtrunc(pt, fam, df, lower.tail = lt, log.p = lg, a = a)
        q_stats <- qchisq(pt, df, lower.tail = lt, log.p = lg)
        expect_length(q_trunc, i)
        for (ii in seq_along(pt)) {
          expect_gte(q_trunc[ii] + 1e-9, q_stats[ii])
          # Working back to p from q
          ptr <- ptrunc(
            q_trunc[ii], fam, df, lower.tail = lt, log.p = lg, a = a
          )
          expect_equal(pt[ii], ptr)
        }
      }
    }
  }
})

test_that("qtrunc() works as expected (contbern)", {
  fam <- "contbern"
  for (i in seq_len(3L)) {
    lb <- runif(1L)
    pt <- runif(i)
    a <- runif(1L)
    q_trunc <- qtrunc(pt, fam, lb, a = a)
    q_stats <- qcontbern(pt, lb)
    expect_length(q_trunc, i)
    for (ii in seq_along(pt)) {
      expect_gt(q_trunc[ii], q_stats[ii])
      # Working back to p from q
      ptr <- ptrunc(q_trunc[ii], fam, lb, a = a)
      expect_equal(pt[ii], ptr)
    }
  }
})

test_that("qtrunc() works as expected (exp)", {
  fam <- "exp"
  for (lg in c(FALSE, TRUE)) {
    for (lt in c(TRUE, FALSE)) {
      for (i in seq_len(3L)) {
        rate <- runif(1)
        pt <- runif(i)
        if (lg) pt <- log(pt)
        a <- qtrunc(min(pt) / 2, fam, rate, lower.tail = lt, log.p = lg)
        q_trunc <- qtrunc(pt, fam, rate, a = a, lower.tail = lt, log.p = lg)
        q_stats <- pexp(pt, rate, lower.tail = lt, log.p = lg)
        expect_length(q_trunc, i)
        for (ii in seq_along(pt)) {
          expect_gt(q_trunc[ii], q_stats[ii])
          # Working back to p from q
          ptr <- ptrunc(
            q_trunc[ii], fam, rate, lower.tail = lt, log.p = lg, a = a
          )
          expect_equal(pt[ii], ptr)
        }
      }
    }
  }
})

test_that("qtrunc() works as expected (gamma)", {
  fam <- "gamma"
  for (lg in c(FALSE, TRUE)) {
    for (lt in c(TRUE, FALSE)) {
      for (i in seq_len(3L)) {
        shp <- rchisq(1L, df = 10L)
        rte <- rchisq(1L, df = 10L)
        skl <- 1 / rte
        pt <- runif(i)
        if (lg) pt <- log(pt)
        a <- rgamma(1L, shp, rte)
        q_trunc_sr <- qtrunc(
          pt, fam, shp, rte, a = a, lower.tail = lt, log.p = lg
        )
        q_trunc_ss <- qtrunc(
          pt, fam, shp, scale = skl, a = a, lower.tail = lt, log.p = lg
        )
        q_stats <- qgamma(pt, shp, rte, lower.tail = lt, log.p = lg)
        expect_length(q_trunc_sr, i)
        expect_equal(q_trunc_sr, q_trunc_ss)
        for (ii in seq_along(pt)) {
          expect_gt(q_trunc_sr[ii], q_stats[ii])
          # Working back to p from q
          ptr <- ptrunc(
            q_trunc_sr[ii], fam, shp, rte, lower.tail = lt, log.p = lg, a = a
          )
          expect_equal(pt[ii], ptr)
        }
      }
    }
  }
})

test_that("qtrunc() works as expected (invgamma)", {
  fam <- "invgamma"
  for (lg in c(FALSE, TRUE)) {
    for (lt in c(TRUE, FALSE)) {
      for (i in seq_len(3L)) {
        shp <- rchisq(1L, df = 10L)
        rte <- rchisq(1L, df = 10L)
        skl <- 1 / rte
        pt <- runif(i)
        if (lg) pt <- log(pt)
        a <- rinvgamma(1L, shp, rte)
        q_trunc_sr <- qtrunc(
          pt, fam, shp, rte, a = a, lower.tail = lt, log.p = lg
        )
        q_trunc_ss <- qtrunc(
          pt, fam, shp, scale = skl, a = a, lower.tail = lt, log.p = lg
        )
        expect_length(q_trunc_sr, i)
        expect_equal(q_trunc_sr, q_trunc_ss)
        for (ii in seq_along(pt)) {
          # Working back to p from q
          ptr <- ptrunc(
            q_trunc_sr[ii], fam, shp, rte, lower.tail = lt, log.p = lg, a = a
          )
          expect_equal(pt[ii], ptr)
        }
      }
    }
  }
})

test_that("qtrunc() works as expected (invgauss)", {
  fam <- "invgauss"
  for (i in seq_len(3L)) {
    m <- rchisq(1L, df = 10L)
    s <- rchisq(1L, df = 10L)
    a <- rinvgauss(1L, m, s)
    pt <- runif(i)
    q_trunc <- qtrunc(pt, fam, m, s, a)
    q_invgauss <- qinvgauss(pt, m, s)
    expect_length(pt, i)
    expect_length(q_trunc, i)
    for (ii in seq_along(pt)) {
      expect_gte(q_trunc[ii], q_invgauss[ii])
      # Working back to p from q
      ptr <- ptrunc(q_trunc[ii], fam, m, s, a)
      expect_equal(pt[ii], ptr, tolerance = 1e-3)
    }
  }
})

test_that("qtrunc() works as expected (lognormal)", {
  fam <- "lognormal"
  for (lg in c(FALSE, TRUE)) {
    for (lt in c(TRUE, FALSE)) {
      for (i in seq_len(3L)) {
        mn <- rnorm(1L, sd = 10)
        sg <- rchisq(1L, 5L)
        pt <- runif(i)
        if (lg) pt <- log(pt)
        a <- qtrunc(min(pt) / 2, fam, mn, sg, lower.tail = lt, log.p = lg)
        q_trunc <- qtrunc(pt, fam, mn, sg, a = a, lower.tail = lt, log.p = lg)
        q_norm <- qlnorm(pt, mn, sg, lower.tail = lt, log.p = lg)
        expect_length(q_trunc, i)
        for (ii in seq_along(pt)) {
          expect_gt(q_trunc[ii], q_norm[ii])
          # Working back to p from q
          ptr <- ptrunc(
            q_trunc[ii], fam, mn, sg, a = a, lower.tail = lt, log.p = lg
          )
          expect_equal(pt[ii], ptr)
        }
      }
    }
  }
})

test_that("qtrunc() works as expected (negbinom)", {
  fam <- "nbinom"
  for (lg in c(FALSE, TRUE)) {
    for (lt in c(TRUE, FALSE)) {
      for (i in seq_len(3L)) {
        sz <- sample(1:10, 1L)
        pb <- runif(1)
        mu <- sz * (1 - pb) / pb
        pt <- runif(i)
        if (lg) pt <- log(pt)
        a <- qtrunc(min(pt) / 2, fam, sz, pb, lower.tail = lt, log.p = lg)
        q_trunc_pb <- qtrunc(
          pt, fam, sz, pb, a = a, lower.tail = lt, log.p = lg
        )
        q_trunc_mu <- qtrunc(
          pt, fam, sz, mu = mu, a = a, lower.tail = lt, log.p = lg
        )
        q_stats <- qnbinom(pt, sz, pb, lower.tail = lt, log.p = lg)
        expect_length(q_trunc_pb, i)
        expect_equal(q_trunc_pb, q_trunc_mu, tolerance = 1e-6)
        for (ii in seq_along(pt)) {
          expect_gte(q_trunc_pb[ii], q_stats[ii])
          # Working back to p from q
          q_lo <- max(q_trunc_pb[ii] - 1L, 0L, a)
          q_hi <- min(q_trunc_pb[ii] + 1L)
          ptr_1 <- ptrunc(q_lo, fam, sz, pb, a = a, lower.tail = lt, log.p = lg)
          ptr_2 <- ptrunc(q_hi, fam, sz, pb, a = a, lower.tail = lt, log.p = lg)
          # because pt will have been rounded
          if (lt) {
            expect_lte(ptr_1, ptr_2)
          } else {
            expect_gte(ptr_1, ptr_2)
          }
        }
      }
    }
  }
})

test_that("qtrunc() works as expected (normal)", {
  for (lg in c(FALSE, TRUE)) {
    for (lt in c(TRUE, FALSE)) {
      for (i in seq_len(3L)) {
        mn <- rnorm(1L, sd = 10)
        sg <- rchisq(1L, 5L)
        pt <- runif(i)
        if (lg) pt <- log(pt)
        a <- qtrunc(
          min(pt) / 2, "normal", mean = mn, sd = sg, lower.tail = lt, log.p = lg
        )
        q_trunc <- qtrunc(
          pt, "normal", mean = mn, sd = sg, a = a, lower.tail = lt, log.p = lg
        )
        q_norm <- qnorm(pt, mean = mn, sd = sg, lower.tail = lt, log.p = lg)
        expect_length(q_trunc, i)
        for (ii in seq_along(pt)) {
          expect_gt(q_trunc[ii], q_norm[ii])
          # Working back to p from q
          ptr <- ptrunc(
            q_trunc[ii], "normal", mean = mn, sd = sg, a = a,
            lower.tail = lt, log.p = lg
          )
          expect_equal(pt[ii], ptr)
        }
      }
    }
  }
})

test_that("qtrunc() works as expected (poisson)", {
  fam <- "poisson"
  for (lt in c(TRUE, FALSE)) {
    for (lg in c(FALSE, TRUE)) {
      for (i in seq_len(3L)) {
        lambda <- sample(1:50, 1L)
        pt <- runif(i)
        a <- qtrunc(min(pt) / 2, fam, lambda, lower.tail = TRUE, log.p = FALSE)
        if (lg) pt <- log(pt)
        q_trunc <- qtrunc(pt, fam, lambda, a = a, lower.tail = lt, log.p = lg)
        q_stats <- qpois(pt, lambda, lower.tail = lt, log.p = lg)
        expect_length(q_trunc, i)
        for (ii in seq_along(pt)) {
          expect_gte(q_trunc[ii], q_stats[ii])
          q_hi <- min(q_trunc[ii] + 1L)
          q_lo <- max(q_trunc[ii] - 1L, 0L, a)
          ptr_1 <- ptrunc(q_lo, fam, lambda, a = a, lower.tail = lt, log.p = lg)
          ptr_2 <- ptrunc(q_hi, fam, lambda, a = a, lower.tail = lt, log.p = lg)
          # because pt will have been rounded
          if (lt) {
            expect_lte(ptr_1, ptr_2)
          } else {
            expect_gte(ptr_1, ptr_2)
          }
        }
      }
    }
  }
})

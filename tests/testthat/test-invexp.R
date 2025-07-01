rate <- 2

set.seed(1234)
n <- 1e4
al <- .01
draws <- rinvexp(n, rate = rate)


# rinvexp() and dinvexp() are consistent

test_that("`rinvexp()` and `dinvexp()` are consistent", {
  L <- 2; U <- 5
  p <- integrate(dinvexp, L, U, "rate" = rate)$value
  expect_gt(
    prop.test( sum(L < draws & draws <= U), n, p = p )$p.value,
    al
  )
})


# rinvexp() and pinvexp() are consistent

test_that("`rinvexp()` and `pinvexp()` are consistent", {
  expect_gt(
    ks.test(draws, pinvexp, "rate" = rate)$p.value,
    al
  )
})


# rinvexp() and qinvexp() are consistent

test_that("`rinvexp()` and `qinvexp()` are consistent", {
  p <- .95
  q <- qinvexp(p, "rate" = rate)
  expect_gt(
    prop.test(sum(draws <= q), n, p = p)$p.value,
    al
  )
})


# pinvexp(log.p = TRUE) == log(pinvexp())

test_that("`pinvexp(log.p = TRUE)` == `log(pinvexp())`", {
  expect_equal(
    pinvexp(2, "rate" = rate, log.p = TRUE),
    log(pinvexp(2, "rate" = rate, log.p = FALSE))
  )

  expect_equal(
    pinvexp(2, "rate" = rate, log.p = TRUE, lower.tail = FALSE),
    log(pinvexp(2, "rate" = rate, log.p = FALSE, lower.tail = FALSE))
  )
})


# qinvexp(log.p = TRUE) works properly

test_that("`qinvexp(log.p = TRUE)` works properly", {

  expect_equal(
    qinvexp(0.25, "rate" = rate),
    qinvexp(log(0.25), "rate" = rate, log.p = TRUE)
  )

  expect_equal(
    qinvexp(0.25, "rate" = rate, lower.tail = FALSE),
    qinvexp(log(0.25), "rate" = rate, lower.tail = FALSE, log.p = TRUE)
  )

})




test_that("`dinvexp()` handles x = 0 properly", {
  # this is more consistent with dgamma() and dgamma(log = TRUE)
  # dexp(0, 3)
  # dexp(0, 3, log = TRUE)
  # dexp(Inf, 3)
  # dexp(Inf, 3, log = TRUE)

  expect_equal(
    dinvexp(0 , "rate" = rate),
    0
  )

  expect_equal(
    dinvexp(0 , "rate" = rate, log = TRUE),
    -Inf
  )

})



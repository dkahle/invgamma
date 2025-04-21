
shape <- 3.5; rate <- 7

set.seed(1234)
n <- 1e4
al <- .01
draws <- rinvgamma(n, shape = shape, rate = rate)


test_that("`rinvgamma()` produces the correct mean and variance", {

  expect_gt(
    t.test(draws, mu = rate / (shape - 1))$p.value,
    al
  )

})


test_that("`rinvgamma()` produces the correct mean and variance", {

  expect_warning(
    rinvgamma(10, .01, rate),
    "`rinvgamma()` is unreliable for `shape` <= .01.",
    fixed = TRUE
  )

})



test_that("`rinvgamma()` and `dinvgamma()` are consistent", {

  L <- 1.2; U <- 1.5
  p <- integrate(dinvgamma, L, U, "shape" = shape, "rate" = rate)$value

  expect_gt(
    prop.test( "x" = sum(L < draws & draws <= U), "n" = n, "p" = p )$p.value,
    al
  )

})



test_that("`rinvgamma()` and `pinvgamma()` are consistent", {

  expect_gt(
    ks.test(draws, pinvgamma, "shape" = shape, "rate" = rate)$p.value,
    al
  )

})



test_that("`rinvgamma()` and `qinvgamma()` are consistent", {

  p <- .95
  q <- qinvgamma(p, "shape" = shape, "rate" = rate)
  expect_gt(
    prop.test(sum(draws <= q), n, p = p)$p.value,
    al
  )

})



test_that("`pinvgamma(log.p = TRUE)` == `log(pinvgamma())`", {

  expect_equal(
    pinvgamma(2, "shape" = shape, "rate" = rate, log.p =  TRUE),
    pinvgamma(2, "shape" = shape, "rate" = rate, log.p = FALSE) |> log()
  )

  expect_equal(
    pinvgamma(2, "shape" = shape, "rate" = rate, log.p =  TRUE, lower.tail = FALSE),
    pinvgamma(2, "shape" = shape, "rate" = rate, log.p = FALSE, lower.tail = FALSE) |> log()
  )

})



test_that("`qinvgamma(log.p = TRUE)` works properly", {

  # note the following behavior,
  # which shows that setting log.p = TRUE with q___() functions
  # does not *output* log(q___()) like d___() or p___() but assumes that the
  # *input* value is on the log scale
  # qnorm(.975)
  # qnorm(log(.975), log.p = TRUE)

  expect_equal(
    qinvgamma(    .25 , "shape" = shape, "rate" = rate),
    qinvgamma(log(.25), "shape" = shape, "rate" = rate, log.p = TRUE)
  )

  expect_equal(
    qinvgamma(    .25 , "shape" = shape, "rate" = rate, lower.tail = FALSE),
    qinvgamma(log(.25), "shape" = shape, "rate" = rate, lower.tail = FALSE, log.p = TRUE)
  )

})



df <- 5; ncp <- 0

set.seed(1234)
n <- 1e4
al <- .01
draws <- rinvchisq(n, df = df, ncp = ncp)


test_that("`rinvchisq()` produces the correct mean", {

  expect_gt(
    t.test(draws, mu = 1 / (df - 2))$p.value,
    al
  )

})


test_that("`rinvchisq()` and `dinvchisq()` are consistent", {

  L <- 0.2; U <- 0.4
  p <- integrate(dinvchisq, L, U, "df" = df, "ncp" = ncp)$value

  expect_gt(
    prop.test( "x" = sum(L < draws & draws <= U), "n" = n, "p" = p )$p.value,
    al
  )

})


test_that("`rinvchisq()` and `pinvchisq()` are consistent", {

  expect_gt(
    ks.test(draws, pinvchisq, "df" = df, "ncp" = ncp)$p.value,
    al
  )

})


test_that("`rinvchisq()` and `qinvchisq()` are consistent", {

  p <- .95
  q <- qinvchisq(p, "df" = df, "ncp" = ncp)
  expect_gt(
    prop.test(sum(draws <= q), n, p = p)$p.value,
    al
  )

})


test_that("`pinvchisq(log.p = TRUE)` == `log(pinvchisq())`", {

  expect_equal(
    pinvchisq(2, "df" = df, "ncp" = ncp, log.p =  TRUE),
    pinvchisq(2, "df" = df, "ncp" = ncp, log.p = FALSE) |> log()
  )

  expect_equal(
    pinvchisq(2, "df" = df, "ncp" = ncp, log.p =  TRUE, lower.tail = FALSE),
    pinvchisq(2, "df" = df, "ncp" = ncp, log.p = FALSE, lower.tail = FALSE) |> log()
  )

})


test_that("`qinvchisq(log.p = TRUE)` works properly", {

  expect_equal(
    qinvchisq(    .25 , "df" = df, "ncp" = ncp),
    qinvchisq(log(.25), "df" = df, "ncp" = ncp, log.p = TRUE)
  )

  expect_equal(
    qinvchisq(    .25 , "df" = df, "ncp" = ncp, lower.tail = FALSE),
    qinvchisq(log(.25), "df" = df, "ncp" = ncp, lower.tail = FALSE, log.p = TRUE)
  )

})


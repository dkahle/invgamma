context("Internal functions")

test_that("qcontbern works", {
  set.seed(8865323)
  tests <- 10L
  for (t in seq_len(tests)) {
    prob <- runif(1)
    lamb <- runif(1)
    expect_equal(pcontbern(qcontbern(prob, lamb), lamb), prob)
    expect_equal(qcontbern(prob, .5), prob)
  }
})

test_that("pcontbern works", {
  set.seed(8865323)
  tests <- 10L
  for (t in seq_len(tests)) {
    x <- runif(1)
    lamb <- runif(1)
    expect_equal(qcontbern(pcontbern(x, lamb), lamb), x)
  }
})

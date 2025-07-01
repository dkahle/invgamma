test_that("Testing for quantregOR1", {
    set.seed(101)
    data("data25j4")
    y <- data25j4$y
    xMat <- data25j4$x
    k <- dim(xMat)[2]
    J <- dim(as.array(unique(y)))[1]
    b0 <- array(rep(0, k), dim = c(k, 1))
    B0 <- 10*diag(k)
    d0 <- array(0, dim = c(J-2, 1))
    D0 <- 0.25*diag(J - 2)
    output <- quantregOR1(y = y, x = xMat, b0, B0, d0, D0,
    burn = 10, mcmc = 40, p = 0.25, tune = 1, accutoff = 0.5, maxlags = 400, verbose = FALSE)
    expect_equal(round(output$acceptancerate), 50)
    expect_equal(round(output$dicQuant$DIC, 2), 1133.11)
    expect_equal(round(output$logMargLike, 2), -559.73)
})

test_that("Testing for quantregOR1 with J=3", {
  set.seed(101)
  data("data25j3")
  y <- data25j3$y
  xMat <- data25j3$x
  k <- dim(xMat)[2]
  J <- dim(as.array(unique(y)))[1]
  b0 <- array(rep(0, k), dim = c(k, 1))
  B0 <- 10*diag(k)
  d0 <- array(0, dim = c(J-2, 1))
  D0 <- 0.25*diag(J - 2)
  output <- quantregOR1(y = y, x = xMat, b0, B0, d0, D0,
                        burn = 10, mcmc = 40, p = 0.25, tune = 1, accutoff = 0.5, maxlags = 400, verbose = FALSE)
  expect_equal(round(output$acceptancerate), 45)
  expect_equal(round(output$dicQuant$DIC, 1), 818.4)
  expect_equal(round(output$logMargLike), -416)
})

test_that("Testing for qrminfundtheorem", {
  set.seed(101)
  deltaIn <- c(-0.002570995,  1.044481071)
  data("data25j4")
  y <- data25j4$y
  xMat <- data25j4$x
  p <- 0.25
  beta <- c(0.3990094, 0.8168991, 2.8034963)
  cri0     <- 1
  cri1     <- 0.001
  stepsize <- 1
  maxiter  <- 10
  h        <- 0.002
  dh       <- 0.0002
  sw       <- 20
  output <- qrminfundtheorem(deltaIn, y, xMat, beta, cri0, cri1, stepsize, maxiter, h, dh, sw, p)
  expect_equal(round(output$negsum), 607)
})

test_that("Testing for qrnegLogLikensumOR1", {
  set.seed(101)
  data("data25j4")
  y <- data25j4$y
  xMat <- data25j4$x
  p <- 0.25
  betaOne <- c(0.3990094, 0.8168991, 2.8034963)
  deltaOne <- c(-0.002570995, 1.044481071)
  output <- qrnegLogLikensumOR1(y, xMat, betaOne, deltaOne, p)
  expect_equal(round(output$negsumlogl,2), 663.55)
})

test_that("Testing for drawdeltaOR1", {
  set.seed(101)
  data("data25j4")
  y <- data25j4$y
  xMat <- data25j4$x
  p <- 0.25
  beta <- c(0.3990094, 0.8168991, 2.8034963)
  delta0 <- c(-0.9026915, -2.2488833)
  d0 <- matrix(c(0, 0),
                    nrow = 2, ncol = 1, byrow = TRUE)
  D0 <- matrix(c(0.25, 0.00, 0.00, 0.25),
                       nrow = 2, ncol = 2, byrow = TRUE)
  tune <- 0.1
  Dhat <- matrix(c(0.046612180, -0.001954257, -0.001954257, 0.083066204),
              nrow = 2, ncol = 2, byrow = TRUE)
  output <- drawdeltaOR1(y, xMat, beta, delta0, d0, D0, tune, Dhat, p)
  expect_equal(round(output$accept), 1)
})

test_that("Testing for dicORI", {
  set.seed(101)
  data("data25j4")
  y <- data25j4$y
  xMat <- data25j4$x
  k <- dim(xMat)[2]
  J <- dim(as.array(unique(y)))[1]
  b0 <- array(rep(0, k), dim = c(k, 1))
  B0 <- 10*diag(k)
  d0 <- array(0, dim = c(J-2, 1))
  D0 <- 0.25*diag(J - 2)
  output <- quantregOR1(y = y, x = xMat, b0, B0, d0, D0,
  burn = 10, mcmc = 40, p = 0.25, tune = 1, accutoff = 0.5, verbose = FALSE)
  mcmc <- 40
  deltadraws <- output$deltadraws
  betadraws <- output$betadraws
  burn <- 0.25*mcmc
  nsim <- burn + mcmc
  postMeanbeta <- output$postMeanbeta
  postMeandelta <- output$postMeandelta
  dic <- dicOR1(y, xMat, betadraws, deltadraws,
  postMeanbeta, postMeandelta, burn, mcmc, p = 0.25)
  expect_equal(round(dic$DIC,2),1133.11)
  expect_equal(round(dic$pd,2),18.07)
  expect_equal(round(dic$dev,2),1096.98)
})

test_that("Testing for alcdfstd", {
  set.seed(101)
  x <-  -0.5428573
  p <- 0.25
  expect_equal(round(alcdfstd(x, p), 4), 0.1664)
})

test_that("Testing for alcdf", {
  set.seed(101)
  x <- -0.5428573
  mu <- 0.5
  sigma <- 1
  p <- 0.25
  expect_equal(round(alcdf(x, mu, sigma, p),4), 0.1144)
})


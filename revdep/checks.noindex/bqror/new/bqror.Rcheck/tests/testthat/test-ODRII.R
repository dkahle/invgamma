test_that("Testing for quantregORII", {
  set.seed(101)
  data("data25j3")
  y <- data25j3$y
  xMat <- data25j3$x
  k <- dim(xMat)[2]
  b0 <- array(rep(0, k), dim = c(k, 1))
  B0 <- 10*diag(k)
  n0 <- 5
  d0 <- 8
  output <- quantregOR2(y = y, x = xMat, b0, B0, n0, d0, gammacp2 = 3,
  burn = 10, mcmc = 40, p = 0.25, accutoff = 0.5, maxlags = 400, verbose = FALSE)
  expect_equal(round(output$logMargLike), -408)
})

test_that("Testing for drawsigmaOR2", {
  set.seed(101)
  z <- c(21.01744, 33.54702, 33.09195, -3.677646,
    21.06553, 1.490476, 0.9618205, -6.743081, 21.02186, 0.6950479)
  x <- matrix(c(
        1, -0.3010490, 0.8012506,
        1,  1.2764036, 0.4658184,
        1,  0.6595495, 1.7563655,
        1, -1.5024607, -0.8251381,
        1, -0.9733585, 0.2980610,
        1, -0.2869895, -1.0130274,
        1,  0.3101613, -1.6260663,
        1, -0.7736152, -1.4987616,
        1,  0.9961420, 1.2965952,
        1, -1.1372480, 1.7537353),
        nrow = 10, ncol = 3, byrow = TRUE)
  beta <- c(-0.74441, 1.364846, 0.7159231)
  n <- dim(x)[1]
  nu <- array(5 * rep(1,n), dim = c(n, 1))
  tau2 <- 10.6667
  theta <- 2.6667
  n0 <- 5
  d0 <- 8
  output <- drawsigmaOR2(z, x, beta, nu, tau2, theta, n0, d0)
  expect_equal(round(output$sigma), 4)
})

test_that("Testing for dicOR2", {
  set.seed(101)
  data("data25j3")
  y <- data25j3$y
  xMat <- data25j3$x
  k <- dim(xMat)[2]
  b0 <- array(rep(0, k), dim = c(k, 1))
  B0 <- 10*diag(k)
  n0 <- 5
  d0 <- 8
  output <- quantregOR2(y = y, x = xMat, b0, B0, n0, d0, gammacp2 = 3,
  burn = 10, mcmc = 40, p = 0.25, accutoff = 0.5, maxlags = 400, verbose = FALSE)
  gammacp <- c(-Inf, 0, 3, Inf)
  betadraws <- output$betadraws
  sigmadraws <- output$sigmadraws
  postMeanbeta <- output$postMeanbeta
  postMeansigma <- output$postMeansigma
  mcmc = 40
  burn <- 10
  nsim <- burn + mcmc
  dic <- dicOR2(y, xMat, betadraws, sigmadraws, gammacp,
  postMeanbeta, postMeansigma, burn, mcmc, p = 0.25)
  expect_equal(round(dic$DIC), 799)
  expect_equal(round(dic$pd), 3)
  expect_equal(round(dic$dev), 792)
})

test_that("Testing for qrnegLogLikeOR2", {
  set.seed(101)
  data("data25j3")
  y <- data25j3$y
  xMat <- data25j3$x
  p <- 0.25
  gammacp <- c(-Inf, 0, 3, Inf)
  betaOne <- c(1.810504, 1.850332, 6.18116)
  sigmaOne <- 0.9684741
  expect_equal(round(qrnegLogLikeOR2(y, xMat, gammacp, betaOne, sigmaOne, p),2), 902.40)
})

test_that("Testing for rndald", {
  set.seed(101)
  sigma <- 2.503306
  p <- 0.25
  n <- 1
  expect_equal(round(rndald(sigma, p, n),4), 1.0733)
})

test_that("sigma2_update() renders correctly", {
  
  # Create a 3x3 positive semidefinite matrix
  Sigma_s <- matrix(c(1, 0.5, 0.5, 0.5, 2, 0.5, 0.5, 0.5, 3), nrow = 3, byrow = TRUE)
  
  sigma2 <- .sigma2_update(mu = 0.5, lambda_0 = c(2, 3, 5), Sigma_s = Sigma_s, J = 2, a_sigma = 1, b_sigma = 2)
  
  # Runs without error
  expect_true(!is.na(sigma2))
  
  # Class is double
  expect_type(sigma2, "double")
  
  # Output is positive
  expect_true(sigma2 > 0)
  
  # Output is a scalar
  expect_length(sigma2, 1)
  
  # Check if the function produces different samples for different runs
  Sigma_s_2 <- matrix(c(4, 1, 2, 1, 5, 3, 2, 3, 6), nrow = 3, byrow = TRUE)
  sigma2_2 <- .sigma2_update(mu = 0.5, lambda_0 = c(2, 3, 5), Sigma_s = Sigma_s_2, J = 2, a_sigma = 1, b_sigma = 2)
  expect_false(all(sigma2 == sigma2_2))
})

test_that("mu_update() renders correctly", {
  
  # Create a 3x3 positive semidefinite matrix
  Sigma_s <- matrix(c(1, 0.5, 2, 0.5), nrow = 2, byrow = TRUE)
  
  mu <- .mu_update(Sigma_s = Sigma_s, lambda_0 = c(5, 4), sigma2 = 0.5, J = 1)
  
  # Runs without error
  expect_true(!is.na(mu))
  
  # Class is double
  expect_type(mu, "double")
  
  # Output is a scalar
  expect_length(mu, 1)
  
  # Check if the function produces different samples for different runs
  Sigma_s <- matrix(c(3, 1, 1, 2), nrow = 2, byrow = TRUE)
  mu_2 <- .mu_update(Sigma_s, c(5, 4), 0.5, 1)
  expect_false(all(mu == mu_2))
})

test_that("Matrices are computed correctly in ICAR_calc()", {
  # Given a correct input
  J <- 5
  s <- c(0, 10, 15, 20, 25, 30, 35)
  clam = 0.7
  normal_res <- .ICAR_calc(s, J, clam)
  
  # Check if Q is a diagonal matrix
  Q <- normal_res$Q
  nonzero_indices <- which(Q != 0, arr.ind = TRUE)
  expect_equal(Q[nonzero_indices], diag(Q))
  
  # Check if Sigma_s is positive semidefinite
  Sigma_s <- normal_res$Sigma_s
  Sigma_eig <- eigen(Sigma_s)$values
  for (ii in 1:length(Sigma_eig)){
    expect_gte(Sigma_eig[ii], 0)}
  
  # For clam <- 0, W is a matrix of zeros
  clam <- 0
  W <- .ICAR_calc(s, J, clam)$W
  expect_equal(sum(W), 0)

  
  # Check if the dimensions of Q, W, and L are as expected
  expect_equal(dim(Q), c(J+1, J+1))
  expect_equal(dim(W), c(J+1, J+1))
  expect_equal(dim(Sigma_s), c(J+1, J+1))
  
  # For J <- 1
  J <- 1
  s <- c(0, 5, 10)
  J_one_res <- .ICAR_calc(s, J, clam)
  expect_equal(dim(J_one_res$Sigma_s), c(J+1, J+1))
  expect_equal(dim(J_one_res$W), c(J+1, J+1))
  expect_equal(dim(J_one_res$Q), c(J+1, J+1))
  
  # For J <- 0
  J <- 0
  s <- c(0, 10)
  J_zero <- .ICAR_calc(s, J, clam)
  expect_equal(J_zero$Sigma_s, matrix(1))
  expect_equal(J_zero$W, matrix(0))
  expect_equal(J_zero$Q, matrix(0))
  
})

test_that("tau updates correctly", {
  ## mix
  # Works for "normal" input
  lambda_0 <- c(0.5, 0.1, 0.8)
  lambda <- c(0.8, 0.8, 0.8)
  J <- 2
  s <- c(0, 1, 2, 3)
  a_tau <- 0.5
  b_tau <- 0.5
  c_tau <- 0.1
  d_tau  <- 0.1
  p_0 <- 0.5 
  type <- 'mix'

  expect_no_error(.tau_update(lambda_0, lambda, J, s, a_tau, b_tau, c_tau, d_tau,
                             p_0, type))
  res <- .tau_update(lambda_0, lambda, J, s, a_tau, b_tau, c_tau, d_tau,
                    p_0, type)
  
  expect_equal(sum(colSums(res$p_new)), length(lambda))
  expect_equal(length(res$tau), length(lambda))
  
  # Throws warning for negative hyperparameters
  a_neg <- -10
  c_neg <- -10
  expect_error(.tau_update(lambda_0, lambda, J, s, a_neg, b_tau, c_neg, d_tau,
                             p_0, type))
  
  ## uni
  # Works for "normal" input
  lambda_0 <- c(0.5, 0.1, 0.8)
  lambda <- c(0.8, 0.8, 0.8)
  J <- 2
  s <- c(0, 1, 2, 3)
  a_tau <- 0.5
  b_tau <- 0.5
  p_0 <- 0.5 
  type <- 'uni'
  
  expect_no_error(.tau_update(lambda_0, lambda, J, s, a_tau, b_tau, NULL, NULL,
                             p_0, type))
  res <- .tau_update(lambda_0, lambda, J, s, a_tau, b_tau, NULL, NULL,
                    p_0, type)
  
  expect_equal(res$p_new, 1)
  expect_equal(length(res$tau), length(lambda))
  
  # Throws warning for negative hyperparameters
  a_neg <- -1
  c_neg <- -10
  expect_warning(.tau_update(lambda_0, lambda, J, s, a_neg, b_tau, NULL, NULL,
                            p_0, type))
  
  
  ##  
  lambda_0 <- c(0.5, 0.1, 0.8)
  lambda <- c(0.8, 0.8, 0.8)
  J <- 2
  s <- c(0, 1, 2, 3)
  a_tau <- 0.5
  b_tau <- 0.5
  c_tau <- 0.1
  d_tau  <- 0.1
  p_0 <- 0.5 
  type <- 'abcdef'
  
  expect_no_error(.tau_update(lambda_0, lambda, J, s, a_tau, b_tau, c_tau, d_tau,
                             p_0, type))
  res <- .tau_update(lambda_0, lambda, J, s, a_tau, b_tau, c_tau, d_tau,
                    p_0, type)
  
  expect_equal(sum(colSums(res$p_new)), 1)
  
})

test_that("Split point shuffling works", {
  set.seed(2023)
  # For a dataset which should increase it's second split point location
  Y <-  c(9, 13, 13, 18, 23, 25, 35, 36, 38, 45, 70)
  Y_0 <- c(5, 9, 20, 59, 2, 1, 25, 30, 35, 40)
  I <- c(1 ,1 ,0, 1, 1, 1, 0, 1, 0, 1, 1)
  I_0 <- c(1, 1, 0, 0, 1, 1, 1, 1, 0, 0)
  X <- NULL
  X_0 <- matrix(c(stats::rbinom(length(Y_0), 5, 0.5), stats::rbinom(length(Y_0), 5, 0.5), 
                 stats::rbinom(length(Y_0), 5, 0.5)), ncol = 3)
  beta <- NULL
  beta_0 <- c(-1.2, 0.5, 2.0)
  J <- 2
  s_r <- c(10, 15)
  s <- c(0, sort(s_r), max(Y, Y_0))
  lambda <- c(0.1, 0.9, 1.0)
  lambda_0 <- c(0.2, 0.8, 1.0)
  df_curr <- .dataframe_fun(Y, I, X, s, lambda, bp = 0, J)
  df_hist <- .dataframe_fun(Y_0, I_0, X_0, s, lambda, bp = ncol(X_0), J)
  sigma2 <- 1.5
  mu <- 0.5
  maxSj <- min(max(Y), max(Y_0))

  # For current data ran longer than historical
  swap <- .shuffle_split_point_location(df_hist = df_hist, df_curr = df_curr, Y_0 = Y_0,
                          I_0 = I_0, X_0 = X_0, lambda_0 = lambda_0, 
                          beta_0 = beta_0, Y = Y, I = I, X = X, lambda = lambda,
                          beta = beta,  s = s, J = J, bp_0 = ncol(X_0), bp = 0, 
                          clam = 0.5, maxSj = min(max(Y), max(Y_0)))
  
  expect_gt(swap$s[3], s[3])
  expect_equal(swap$s[4], max(Y))
  
  # For historical data ran longer than current
  Y <-  c(9, 12, 12, 18, 23, 25, 35, 36, 38, 45, 70)
  Y_0 <- c(5, 9, 20, 59, 2, 1, 9, 30, 55, 80)
  s <- c(0, sort(s_r), max(Y, Y_0))
  df_curr <- .dataframe_fun(Y, I, X, s, lambda, bp = 0, J)
  df_hist <- .dataframe_fun(Y_0, I_0, X_0, s, lambda, bp = ncol(X_0), J)
  swap <- .shuffle_split_point_location(df_hist = df_hist, df_curr = df_curr, Y_0 = Y_0,
                          I_0 = I_0, X_0 = X_0, lambda_0 = lambda_0, 
                          beta_0 = beta_0, Y = Y, I = I, X = X, lambda = lambda,
                          beta = beta, s = s, J = J, bp_0 = ncol(X_0), 
                          bp = 0, clam = 0.5, maxSj = min(max(Y), max(Y_0)))
  
  expect_gt(swap$s[3], s[3])
  expect_equal(swap$s[4], max(Y_0))
  
  # Dataset where the shuffle should not occur
  Y <-  c(9, 5, 6, 13, 13, 12, 20, 18, 23, 19)
  I <- c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0)
  Y_0 <- c(5, 9, 20, 15, 16, 23, 9)
  I_0 <- c(1, 1, 0, 1, 0, 1, 0)
  X_0 <- matrix(c(stats::rbinom(length(Y_0), 5, 0.5), stats::rbinom(length(Y_0), 5, 0.5), 
                  stats::rbinom(length(Y_0), 5, 0.5)), ncol = 3)
  s_r <- c(12, 16)
  s <- c(0, sort(s_r), max(Y, Y_0))
  df_curr <- .dataframe_fun(Y, I, X, s, lambda, bp = 0, J)
  df_hist <- .dataframe_fun(Y_0, I_0, X_0, s, lambda, bp = ncol(X_0), J)
  Sigma_s <- .ICAR_calc(s, J, clam = 0.5)$Sigma_s
  swap <- .shuffle_split_point_location(df_hist = df_hist, df_curr = df_curr, Y_0 = Y_0,
                                  I_0 = I_0, X_0 = X_0, lambda_0 = lambda_0, 
                                  beta_0 = beta_0, Y = Y, I = I, X = X, lambda = lambda,
                                  beta = beta, s = s, J = J, bp_0 = ncol(X_0), 
                                  bp = 0, clam = 0.5, maxSj = min(max(Y), max(Y_0)))
  
  expect_equal(swap$s, s)
  expect_equal(swap$df_hist, df_hist)
  expect_equal(swap$df_curr, df_curr)
  expect_equal(swap$Sigma_s, Sigma_s)
  
  # Should produce error/warning for s_r > maxSj
  maxSj <- min(max(Y), max(Y_0))
  s_r <- c(10, maxSj + 5)
  s <- c(0, sort(s_r), max(Y, Y_0))
  expect_warning(expect_error(.shuffle_split_point_location(df_hist = df_hist, df_curr = df_curr, Y_0 = Y_0,
                                  I_0 = I_0, X_0 = X_0, lambda_0 = lambda_0, 
                                  beta_0 = beta_0, Y = Y, I = I, X = X, lambda = lambda,
                                  beta = beta, s = s, J = J, bp_0 = ncol(X_0), 
                                  bp = 0, clam = 0.5, maxSj = min(max(Y), max(Y_0))),
              regexp = 'missing value'),
              regexp = 'NaNs produced')
})
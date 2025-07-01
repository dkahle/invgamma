test_that("birth move works as intended", {
  set.seed(2023)
  
  # For a normal input
  U <- runif(1)
  j = 3
  lambda_pre <- c(0.1, 0.2, 0.5, 0.1)
  s <- c(0, 10, 12.5, 17.5, 30)
  lambda_post <- .birth_move(U, s[j+1], s_star = 15, s[j-1], lambda_pre, j)
  
  # Runs without error
  expect_true(any(!is.na(lambda_post)))
  
  # Length is length(lambda_pre) + 1
  expect_equal(length(lambda_post), length(lambda_pre)+1)
  
  # Element 1, 2, and 4 should not change for a new split point between 3 and 4
  expect_equal(lambda_pre[1], lambda_post[1])
  expect_equal(lambda_pre[2], lambda_post[2])
  expect_equal(lambda_pre[4], lambda_post[4+1])
  
  # J <- 1, add the second split point
  j = 2
  lambda_pre <- c(0.1, 0.2)
  s <- c(0, 10, 15)
  lambda_post <- .birth_move(U, s[j+1], s_star = 7, s[j-1], lambda_pre, j)
  
  # Length is length(lambda_pre) + 1
  expect_equal(length(lambda_post), length(lambda_pre)+1)
  
  # Element 1 hould not change for a new split point between 2 and 3
  expect_equal(lambda_pre[1], lambda_post[1])
  
})

test_that("death move works as intended", {
  set.seed(2023)
  
  # For a normal input
  U <- runif(1)
  j = 3
  lambda_pre <- c(0.1, 0.2, 0.5, 0.1)
  s <- c(0, 10, 12.5, 17.5, 30)
  lambda_post <- .death_move(sjp1 = s[j+1], sj = s[j], sjm1 = s[j-1], lambda_pre, j = j)
  
  # Runs without error
  expect_true(any(!is.na(lambda_post)))
  
  # Length is length(lambda_pre) - 1
  expect_equal(length(lambda_post), length(lambda_pre)-1)
  
  # First and last element should not change for removal of split point 3
  expect_equal(lambda_pre[1], lambda_post[1])
  expect_equal(lambda_pre[4], lambda_post[4-1])
  
  # J <- 1
  j = 2
  lambda_pre <- c(0.1, 0.2)
  s <- c(0, 10, 15)
  lambda_post <- .death_move(sjp1 = s[j+1], sj = s[j], sjm1 = s[j-1], lambda_pre, j = j)
  
  # Length is length(lambda_pre) - 1
  expect_equal(length(lambda_post), length(lambda_pre)-1)
  
  # Element 1 should not be equal
  expect_gt(lambda_post[1], lambda_pre[1])
})

test_that("log tau posterior density updates", {
  
  ## mix
  ltau <- .ltau_dprior(tau = c(5, 0.5, 10), a_tau = 1, b_tau = 1, c_tau = 0.5, d_tau = 0.5, p_0 = 0.1, type = "mix")
  
  # Runs without error
  expect_true(!is.na(ltau))
  
  # Output is negative (log of probability)
  expect_true(ltau < 0)
  
  # Output is a scalar
  expect_length(ltau, 1)
  
  # Check if the function produces different samples for different runs
  expect_true(ltau == .ltau_dprior(tau = c(5, 0.5, 10), a_tau = 1, b_tau = 1, 
                        c_tau = 0.5, d_tau = 0.5, p_0 = 0.1, type = "mix"))
  
  ## uni
  ltau <- .ltau_dprior(tau = 8, a_tau = 1, b_tau = 1, c_tau = 0.5, d_tau = 0.5, p_0 = 0.1, type = "uni")
  
  # Runs without error
  expect_true(!is.na(ltau))
  
  # Output is negative (log of probability)
  expect_true(ltau < 0)
  
  # Output is a scalar
  expect_length(ltau, 1)
  
  # Check if the function produces different samples for different runs
  expect_true(ltau == .ltau_dprior(tau = 8, a_tau = 1, b_tau = 1, 
                                  c_tau = 0.5, d_tau = 0.5, p_0 = 0.1, type = "uni"))
  
})

test_that("RJMCMC algorithm is correct", {
  set.seed(2023)
  # For a normal input get the right dimensions
  
  # "mix" type of borrowing
  Y <-  c(9, 8, 7, 18, 23, 25, 35, 36, 38, 45, 70, 55, 49)
  Y_0 <- c(5, 9, 20, 59, 29, 25, 25, 30, 35, 40)
  I <- c(1 ,1 ,0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1)
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
  a_tau = 1; b_tau = 1; c_tau = 0.5; d_tau = 0.5; p_0 = 0.1; type = "mix"
  tau = c(5, 0.5, 3)
  phi = 3
  birth_acc <- 0
  
  # What are reasonable tests? Maybe not applicable? [Q]
  # Force birth move
  for (ii in 1:1000){
    res <- .J_RJMCMC(df_hist, df_curr, Y, Y_0, I, I_0, X, X_0, lambda, lambda_0, beta, 
             beta_0, mu, sigma2, tau,  s, J, Jmax = 20, bp = length(beta), 
             bp_0 = length(beta_0), clam = 0.5, a_tau, b_tau,
             c_tau, d_tau, type, p_0, phi, pi_b = 0.99, maxSj)
    if(length(res$s) > J + 2){
      birth_acc <- birth_acc + 1
      res_save <- res
    }
  
  }
  
  expect_gt(birth_acc/1000, 0.2)
  expect_equal(length(res_save$s), J + 3)
  expect_equal(length(res_save$lambda), J + 2)
  expect_equal(length(res_save$lambda_0), J + 2)
  expect_equal(length(res_save$tau), J + 2)
  
  # Force death move
  death_acc <- 0
  for (ii in 1:1000){
    res <- .J_RJMCMC(df_hist, df_curr, Y, Y_0, I, I_0, X, X_0, lambda, lambda_0, beta, 
                    beta_0, mu, sigma2, tau,  s, J, Jmax = 20, bp = length(beta), 
                    bp_0 = length(beta_0), clam = 0.5, a_tau, b_tau,
                    c_tau, d_tau, type, p_0, phi, pi_b = 0.01, maxSj)
    if(length(res$s) < J + 2){
      death_acc <- death_acc + 1
      res_save <- res
    }
    
  }
  
  expect_gt(death_acc/1000, 0.2)
  expect_equal(length(res_save$s), J + 1)
  expect_equal(length(res_save$lambda), J)
  expect_equal(length(res_save$lambda_0), J)
  expect_equal(length(res_save$tau), J)
  
  
  # "uni" borrowing
  a_tau = 1; b_tau = 1; c_tau = NA; d_tau = NA; p_0 = NA; type = "uni"
  tau = c(5, 0.5, 3)
  phi = 3
  birth_acc <- 0
  # Force birth move
  for (ii in 1:1000){
    res <- .J_RJMCMC(df_hist, df_curr, Y, Y_0, I, I_0, X, X_0, lambda, lambda_0, beta, 
                    beta_0, mu, sigma2, tau,  s, J, Jmax = 20, bp = length(beta), 
                    bp_0 = length(beta_0), clam = 0.5, a_tau, b_tau,
                    c_tau, d_tau, type, p_0, phi, pi_b = 0.99, maxSj)
    if(length(res$s) > J + 2){
      birth_acc <- birth_acc + 1
      res_save <- res
    }
    
  }
  
  expect_gt(birth_acc/1000, 0.2)
  expect_equal(length(res_save$s), J + 3)
  expect_equal(length(res_save$lambda), J + 2)
  expect_equal(length(res_save$lambda_0), J + 2)
  expect_equal(length(res_save$tau), J + 2)
  
  # Force death move
  death_acc <- 0
  for (ii in 1:1000){
    res <- .J_RJMCMC(df_hist, df_curr, Y, Y_0, I, I_0, X, X_0, lambda, lambda_0, beta, 
                    beta_0, mu, sigma2, tau,  s, J, Jmax = 20, bp = length(beta), 
                    bp_0 = length(beta_0), clam = 0.5, a_tau, b_tau,
                    c_tau, d_tau, type, p_0, phi, pi_b = 0.01, maxSj)
    if(length(res$s) < J + 2){
      death_acc <- death_acc + 1
      res_save <- res
    }
    
  }
  
  expect_gt(death_acc/1000, 0.2)
  expect_equal(length(res_save$s), J + 1)
  expect_equal(length(res_save$lambda), J)
  expect_equal(length(res_save$lambda_0), J)
  expect_equal(length(res_save$tau), J)
  
  #  non-piecewise borrowing
  a_tau = 1; b_tau = 1; c_tau = 0.5; d_tau = 0.5; p_0 = 0.1; type = "all"
  tau = 10
  
  birth_acc <- 0
  # Force birth move
  for (ii in 1:1000){
    res <- .J_RJMCMC(df_hist, df_curr, Y, Y_0, I, I_0, X, X_0, lambda, lambda_0, beta, 
                    beta_0, mu, sigma2, tau,  s, J, Jmax = 20, bp = length(beta), 
                    bp_0 = length(beta_0), clam = 0.5, a_tau, b_tau,
                    c_tau, d_tau, type, p_0, phi, pi_b = 0.99, maxSj)
    if(length(res$s) > J + 2){
      birth_acc <- birth_acc + 1
      res_save <- res
    }
    
  }
  
  expect_gt(birth_acc/1000, 0.2)
  expect_equal(length(res_save$s), J + 3)
  expect_equal(length(res_save$lambda), J + 2)
  expect_equal(length(res_save$lambda_0), J + 2)
  expect_equal(res_save$tau, tau)
  
  # Force death move
  death_acc <- 0
  for (ii in 1:1000){
    res <- .J_RJMCMC(df_hist, df_curr, Y, Y_0, I, I_0, X, X_0, lambda, lambda_0, beta, 
                    beta_0, mu, sigma2, tau,  s, J, Jmax = 20, bp = length(beta), 
                    bp_0 = length(beta_0), clam = 0.5, a_tau, b_tau,
                    c_tau, d_tau, type, p_0, phi, pi_b = 0.01, maxSj)
    if(length(res$s) < J + 2){
      death_acc <- death_acc + 1
      res_save <- res
    }
    
  }
  
  expect_gt(death_acc/1000, 0.2)
  expect_equal(length(res_save$s), J + 1)
  expect_equal(length(res_save$lambda), J)
  expect_equal(length(res_save$lambda_0), J)
  expect_equal(res_save$tau, tau) # tau should not change
  
})

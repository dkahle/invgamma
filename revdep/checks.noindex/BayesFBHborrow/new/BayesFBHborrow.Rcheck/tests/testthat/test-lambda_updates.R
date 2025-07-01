test_that("lam_conj_prop updates as intended", {
  Y_0 <-  c(7, 25, 51, 13, 64, 11, 34, 38, 40)
  I_0 <- c(1 ,1 ,0, 1, 1, 1, 1, 0, 0)
  X_0 <- NULL
  Y <-  c(9, 13, 13, 18, 23)
  I <- c(1 ,1 ,0, 1, 1)
  X <- NULL
  s_r <- c(10, 15, 35)
  s <- c(0, sort(s_r),max(c(Y_0,Y)))
  J <- 3
  alpha <- 0.4 # power parameter
  
  # without covariates, lambda_0
  bp_0 <- 0
  lambda_0 <- c(1.5, 1.4, 1.0, 0.5)
  beta_0 <- NULL
  df_hist <- .dataframe_fun(Y_0, I_0, X_0, s, lambda_0, bp = 0, J)
  lambda_0_new <- lambda_0   
  j <- 1
  lambda_prop <- 0
  for (ii in 1:20){
    j <- ii %% 3 + 1
    lambda_0_prop_all <- .lambda_conj_prop(df_hist, beta = beta_0, j, bp = bp_0, alam = 0.01, blam = 0.01)  
    
    # Runs without error
    expect_true(any(!is.na(lambda_0_prop_all$lambda_prop)))
    
    # Different results for each run
    expect_true(lambda_0_prop_all$lambda_prop !=  lambda_prop)
    lambda_prop <- lambda_0_prop_all$lambda_prop
    
    # Shape, rate and lambda all nonnegative
    expect_true(all(lambda_0_prop_all > 0))
  }
  
  # with covariates, lambda_0
  bp_0 <- 3
  lambda_0 <- c(1.5, 1.4, 1.0, 0.5)
  beta_0 <- c(-3.0, 5.0, 1.2)
  X_0 <- matrix(stats::runif(3 * length(Y_0)), ncol = 3)
  df_hist <- .dataframe_fun(Y_0, I_0, X_0, s, lambda_0, bp = bp_0, J)
  lambda_0_new <- lambda_0   
  j <- 1
  lambda_prop <- 0
  for (ii in 1:20){
    j <- ii %% 3 + 1
    lambda_0_prop_all <- .lambda_conj_prop(df_hist, beta = beta_0, j, bp = bp_0)  
    
    # Runs without error
    expect_true(any(!is.na(lambda_0_prop_all$lambda_prop)))
    
    # Different results for each run
    expect_true(lambda_0_prop_all$lambda_prop !=  lambda_prop)
    lambda_prop <- lambda_0_prop_all$lambda_prop
    
    # Shape, rate and lambda all nonnegative
    expect_true(all(lambda_0_prop_all > 0))
  }
  
  # without covariates, lambda
  lambda <- c(0.1, 0.9, 1.2, 1.5)
  beta <- c(1.5, -8.3, 0.1, 9.0)
  bp <- 4
  X <- matrix(stats::rnorm(4 * length(Y)), ncol = 4)
  df_curr <- .dataframe_fun(Y, I, X, s, lambda, bp = bp, J) 
  j <- 1
  lambda_prop <- 0
  for (ii in 1:20){
    j <- ii %% 3 + 1
    lambda_prop_all <- .lambda_conj_prop(df_curr, beta = beta, j, bp = bp)  
    
    # Runs without error
    expect_true(any(!is.na(lambda_prop_all$lambda_prop)))
    
    # Different results for each run
    expect_true(lambda_prop_all$lambda_prop !=  lambda_prop)
    lambda_prop <- lambda_prop_all$lambda_prop
    
    # Shape, rate and lambda all nonnegative
    expect_true(all(lambda_prop_all > 0))
  }
  
  # with covariates, lambda
  lambda <- c(0.1, 0.9, 1.2, 1.5)
  beta <- NULL
  df_curr <- .dataframe_fun(Y, I, X, s, lambda, bp = 0, J) 
  bp <- 0
  j <- 1
  lambda_prop <- 0
  for (ii in 1:20){
    j <- ii %% 3 + 1
    lambda_prop_all <- .lambda_conj_prop(df_curr, beta = beta, j, bp = bp)  
    
    # Runs without error
    expect_true(any(!is.na(lambda_prop_all$lambda_prop)))
    
    # Different results for each run
    expect_true(lambda_prop_all$lambda_prop !=  lambda_prop)
    lambda_prop <- lambda_prop_all$lambda_prop
    
    # Shape, rate and lambda all nonnegative
    expect_true(all(lambda_prop_all > 0))
  }
  
})


test_that("loglikelihood update for both lambda and lambda_0 works ", {
  Y_0 <-  c(7, 25, 51, 13, 64, 11, 34, 38, 40)
  I_0 <- c(1 ,1 ,0, 1, 1, 1, 1, 0, 0)
  X_0 <- NULL
  Y <-  c(9, 13, 13, 18, 23)
  I <- c(1 ,1 ,0, 1, 1)
  X <- NULL
  #s_r <- c(10, 15, 35) # one split point larger than Y_max, could this occur?
  s_r <- c(10, 15, 20)
  s <- c(0, sort(s_r),max(c(Y_0,Y)))
  J <- 3
  alpha <- 0.4 # power parameter
  a_lam <- 0.01
  b_lam <- 0.01
  
  # without covariates, lambda_0
  bp_0 <- 0
  lambda_0 <- c(1.5, 1.4, 1.0, 0.5)
  beta_0 <- NULL
  df_hist <- .dataframe_fun(Y_0, I_0, X_0, s, lambda_0, bp = 0, J)
  lambda_0_new <- lambda_0   
  
  for (j in 1:(J+1)){
    lambda_0_prop_all <- .lambda_conj_prop(df_hist, beta = beta_0, j, bp = bp_0, a_lam, b_lam)  
    lambda_0_prop <- lambda_0_prop_all$lambda_prop
    
    lambda_0_new[j] <- lambda_0_prop 
    shape_prop <- lambda_0_prop_all$shape_prop
    rate_prop <- lambda_0_prop_all$rate_prop
    
    df_prop <- .dataframe_fun(Y = Y_0, I = I_0, X = X_0, s = s, lambda = lambda_0_new, bp = bp_0, J = J) 
    
    llikelihood_ratio <- .llikelihood_ratio_lambda(df_hist, df_prop, beta_0)
    
    expect_true(!is.na(llikelihood_ratio))
    
    comp <- sum((log(df_prop$lambda) - log(df_hist$lambda)) * df_hist$I  - 
                  (df_hist$Y - df_hist$tstart) * (df_prop$lambda - df_hist$lambda)) 
    
    expect_equal(llikelihood_ratio, comp)
  }
  
  # without covariates, lambda
  lambda <- c(0.1, 0.9, 1.2, 1.5)
  beta <- NULL
  df_curr <- .dataframe_fun(Y, I, X, s, lambda, bp = 0, J)
  
  # sample new lambda
  for (j in 1:(J+1)){
    lambda_new <- lambda
    lambda_prop_cc <- .lambda_conj_prop(df_curr, beta, j, bp = 0, a_lam, b_lam) 

    cc_shape_prop <- lambda_prop_cc$shape_prop
    cc_rate_prop <- lambda_prop_cc$rate_prop
    
    lambda_prop_hist <- .lambda_conj_prop(df_hist, beta_0, j, bp = 0, a_lam, b_lam) 
    hist_shape_prop <- lambda_prop_hist$shape_prop
    hist_rate_prop <- lambda_prop_hist$rate_prop
    
    shape_prop <- a_lam + cc_shape_prop + alpha * hist_shape_prop
    rate_prop <- b_lam + cc_rate_prop + alpha * hist_rate_prop
    
    lambda_prop <- stats:: rgamma(1, shape = shape_prop, rate = rate_prop)
    
    lambda_new[j] <- lambda_prop
    
    df_prop <- .dataframe_fun(Y = Y, I = I, X = X, s = s, lambda = lambda_new, bp = 0, J = J)        
    
    llikelihood_ratio <- .llikelihood_ratio_lambda(df_curr, df_prop, beta)
    
    expect_true(!is.na(llikelihood_ratio))
    
    comp <- sum((log(df_prop$lambda) - log(df_curr$lambda)) * df_curr$I  - 
                  (df_curr$Y - df_curr$tstart) * (df_prop$lambda - df_curr$lambda)) 
    
    expect_equal(llikelihood_ratio, comp)
  }
  
  # with covariates, lambda
  beta <- c(0.01, -0.2, 0.005)
  bp = 3
  X <- matrix(rbinom(3 * length(Y), 100, 0.2), ncol = 3)
  df_curr <- .dataframe_fun(Y, I, X, s, lambda, bp = bp, J)
  
  # sample new lambda
  for (j in 1:(J+1)){
    lambda_new <- lambda
    lambda_prop_cc <- .lambda_conj_prop(df_curr, beta, j, a_lam, b_lam, bp = bp) 
    
    cc_shape_prop <- lambda_prop_cc$shape_prop
    cc_rate_prop <- lambda_prop_cc$rate_prop
    
    lambda_prop_hist <- .lambda_conj_prop(df_hist, beta_0, j, a_lam, b_lam, bp = bp) 
    hist_shape_prop <- lambda_prop_hist$shape_prop
    hist_rate_prop <- lambda_prop_hist$rate_prop
    
    shape_prop <- a_lam + cc_shape_prop + alpha * hist_shape_prop
    rate_prop <- b_lam + cc_rate_prop + alpha * hist_rate_prop
    
    expect_gt(shape_prop, 0)
    expect_gt(rate_prop, 0)
    
    lambda_prop <- stats:: rgamma(1, shape = shape_prop, rate = rate_prop)
    
    lambda_new[j] <- lambda_prop
    
    df_prop <- .dataframe_fun(Y = Y, I = I, X = X, s = s, lambda = lambda_new, bp = bp, J = J)        
    
    llikelihood_ratio <- .llikelihood_ratio_lambda(df_curr, df_prop, beta)
    
    expect_true(!is.na(llikelihood_ratio))
    
    X_curr <- as.matrix(df_curr[, substr(colnames(df_curr), 1, 1) == "X"])
    xdpb <- X_curr %*% beta
    comp <- sum((log(df_prop$lambda) - log(df_curr$lambda)) * df_curr$I  - 
                  (df_curr$Y - df_curr$tstart) * (df_prop$lambda - df_curr$lambda) * exp(xdpb))  
    
    expect_equal(llikelihood_ratio, comp)
  }
  
  
})

test_that("nu_sigma_update updates correctly", {
  J <- 3
  s_r <- c(10, 15, 35)
  s <- c(0, sort(s_r), 50)
  lambda_0 <- c(0.1, 0.9, 1.2, 1.5)
  mu <- 2.0
  sigma2 <- 0.5
  ICAR <- .ICAR_calc(s, J, clam = 0.75) 
  Q <- ICAR$Q
  W <- ICAR$W
  for (ii in 1:20){
    j <- ii %% 4 + 1
    nu_sigma <- .nu_sigma_update(j, lambda_0, mu, sigma2, W, Q, J)
    expect_true(all(!is.na(nu_sigma)))
    expect_gt(nu_sigma$sigma2j, 0)
  }
  
})

test_that(".lgamma_ratio is calculated correctly", {
  lambda <- c(1.3, 1.5, 0.5)
  lambda_prop <- 1.0
  
  # works for a normal input
  lgamma <- .lgamma_ratio(x1 = lambda[2], x2 = lambda_prop, shape = 0.05, rate = 0.01)
  expect_true(!is.na(lgamma))
  
  # Throws warning for negative lambdas (produces NaNs)
  lambda_prop <- -1.0
  expect_warning(.lgamma_ratio(x1 = lambda[2], x2 = lambda_prop, shape = 0.05, rate = 0.01))
  lambda_prop <- 1.0
  lambda <- c(1.3, -1.5, 0.5)
  expect_warning(.lgamma_ratio(x1 = lambda[2], x2 = lambda_prop, shape = 0.05, rate = 0.01))
})

test_that("lambda_0_MH_cp works as it should with borrowing", {
  set.seed(2023)
  Y_0 <-  c(7, 25, 51, 13, 64, 11, 34, 38, 40)
  I_0 <- c(1 ,1 ,0, 1, 1, 1, 1, 0, 0)
  X_0 <- NULL
  s_r <- c(10, 15, 20)
  s <- c(0, sort(s_r),max(Y_0))
  J <- 3
  
  # without covariates, lambda_0
  bp_0 <- 0
  lambda_0 <- c(0.01 , 0.01, 0.01, 0.01)
  lambda <- c(1.5, 1.4, 1.0, 0.5)*runif(4,0,1)
  beta_0 <- NULL
  df_hist <- .dataframe_fun(Y_0, I_0, X_0, s, lambda_0, bp = 0, J)
  # piecewise borrowing
  tau <- c(0.9, 3.2, 2.5, 7.0)
  mu = 5.0
  sigma2 = 8.5
  
  # Force a move
  lambda_0_prop <- .lambda_0_MH_cp(df_hist = df_hist, Y_0 = Y_0, I_0 = I_0, X_0 = NULL, s = s,
                             beta_0 = NULL, mu = mu, sigma2 = sigma2,  
                             lambda = lambda, lambda_0 = lambda_0, tau = tau, 
                             bp_0 = 0, J = J, clam = .75, a_lam = 10, 
                             b_lam = 0.05, lambda_0_count = 0, lambda_0_move = 0)
  
  # runs without error
  expect_true(all(!is.na(lambda_0_prop)))
  
  # produces different values
  expect_gt(lambda_0_prop$lambda_0_move, 0)
  
  
  # Check acceptance ratio
  lambda_0 <- c(0.01 , 0.01, 0.01, 0.01)
  count_tot <- 0
  acc_tot <- 0
  for (ii in 1:50) {
    lambda_0_prop <- .lambda_0_MH_cp(df_hist = df_hist, Y_0 = Y_0, I_0 = I_0, X_0 = NULL, s = s,
                                    beta_0 = NULL, mu = mu, sigma2 = sigma2,  
                                    lambda = lambda, lambda_0 = lambda_0, tau = tau, 
                                    bp_0 = 0, J = J, clam = .75, a_lam = 0.01, 
                                    b_lam = 0.01, lambda_0_count = 0, lambda_0_move = 0)
    count_tot <- count_tot + lambda_0_prop$lambda_0_count
    acc_tot <- acc_tot + lambda_0_prop$lambda_0_move
    #lambda_0 <- lambda_0_prop$lambda_0
  }
  
  expect_gt(acc_tot/count_tot, 0.2)
  
  # Same seed produces same output
  set.seed(2023)
  lambda_0_prop_1 <- .lambda_0_MH_cp(df_hist = df_hist, Y_0 = Y_0, I_0 = I_0, X_0 = NULL, s = s,
                                  beta_0 = NULL, mu = mu, sigma2 = sigma2,  
                                  lambda = lambda, lambda_0 = lambda_0, tau = tau, 
                                  bp_0 = 0, J = J, clam = .75, a_lam = 0.01, 
                                  b_lam = 0.01, lambda_0_count = 0, lambda_0_move = 0)
  
  set.seed(2023)
  lambda_0_prop_2 <- .lambda_0_MH_cp(df_hist = df_hist, Y_0 = Y_0, I_0 = I_0, X_0 = NULL, s = s,
                                    beta_0 = NULL, mu = mu, sigma2 = sigma2,  
                                    lambda = lambda, lambda_0 = lambda_0, tau = tau, 
                                    bp_0 = 0, J = J, clam = .75, a_lam = 0.01, 
                                    b_lam = 0.01, lambda_0_count = 0, lambda_0_move = 0)
  
  expect_true(all(lambda_0_prop_1$lambda_0 == lambda_0_prop_2$lambda_0))
  expect_true(all(lambda_0_prop_1$df_hist == lambda_0_prop_2$df_hist))
  expect_true(all(lambda_0_prop_1$lambda_0_count == lambda_0_prop_2$lambda_0_count))
  expect_true(all(lambda_0_prop_1$lambda_0_move == lambda_0_prop_2$lambda_0_move))
  
  # with covariates, on historical
  beta_0 <- c(0.5, -1.2, 1.25)
  bp_0 = 3
  X_0 <- matrix(stats::rbinom(3 * length(Y_0), 100, 0.2), ncol = 3)
  
  df_hist <- .dataframe_fun(Y_0, I_0, X_0, s, lambda_0, bp = bp_0, J)
  
  lambda_0_prop <- .lambda_0_MH_cp(df_hist = df_hist, Y_0 = Y_0, I_0 = I_0, X_0 = X_0, s = s,
                                  beta_0 = beta_0, mu = mu, sigma2 = sigma2,  
                                  lambda = lambda, lambda_0 = lambda_0, tau = tau, 
                                  bp_0 = bp_0, J = J, clam = .75, a_lam = 10, 
                                  b_lam = 0.05, lambda_0_count = 0, lambda_0_move = 0)
  
  # runs without error
  expect_true(all(!is.na(lambda_0_prop)))
  
  
  # expect error for "Inf" values of lambda
  lambda_0 <- c(Inf, 2, 0.5, 1)
  df_hist <- .dataframe_fun(Y_0, I_0, X_0, s, lambda_0, bp = bp_0, J)
  
  expect_error(.lambda_0_MH_cp(df_hist = df_hist, Y_0 = Y_0, I_0 = I_0, X_0 = X_0, s = s,
                                  beta_0 = beta_0, mu = mu, sigma2 = sigma2,  
                                  lambda = lambda, lambda_0 = lambda_0, tau = tau, 
                                  bp_0 = bp_0, J = J, clam = .75, a_lam = 10, 
                                  b_lam = 0.05, lambda_0_count = 0, lambda_0_move = 0),
                regex = 'missing value')
  
  # expect error for "NA" values of lambda
  lambda_0 <- c(NA, 2, 0.5, 1)
  df_hist <- .dataframe_fun(Y_0, I_0, X_0, s, lambda_0, bp = bp_0, J)
  
  expect_error(.lambda_0_MH_cp(df_hist = df_hist, Y_0 = Y_0, I_0 = I_0, X_0 = X_0, s = s,
                              beta_0 = beta_0, mu = mu, sigma2 = sigma2,  
                              lambda = lambda, lambda_0 = lambda_0, tau = tau, 
                              bp_0 = bp_0, J = J, clam = .75, a_lam = 10, 
                              b_lam = 0.05, lambda_0_count = 0, lambda_0_move = 0),
               regex = 'missing value')
})

test_that("lambda_0_MH_cp works as it should without borrowing", {
  set.seed(2023)
  Y_0 <-  c(7, 25, 51, 13, 64, 11, 34, 38, 40)
  I_0 <- c(1 ,1 ,0, 1, 1, 1, 1, 0, 0)
  X_0 <- NULL
  s_r <- c(10, 15, 20)
  s <- c(0, sort(s_r),max(Y_0))
  J <- 3
  
  # without covariates, lambda_0
  bp_0 <- 0
  lambda_0 <- c(0.1 , 50, 0.1, 10)
  lambda <- NULL
  beta_0 <- NULL
  df_hist <- .dataframe_fun(Y_0, I_0, X_0, s, lambda_0, bp = 0, J)

  tau <- NULL
  mu = 5.0
  sigma2 = 100
  
  # Force a move
  lambda_0_prop <- .lambda_0_MH_cp_NoBorrow(df_hist = df_hist, Y_0 = Y_0, I_0 = I_0, 
                                  X_0 = NULL, s = s, beta_0 = NULL, mu = mu,
                                  sigma2 = sigma2,  
                                  lambda_0 = lambda_0, bp_0 = 0,
                                  J = J, clam = .75, a_lam = 10, 
                                  b_lam = 0.05, lambda_0_count = 0,
                                  lambda_0_move = 0)
  
  # runs without error
  expect_true(all(!is.na(lambda_0_prop)))
  
  # tries all combinations
  expect_equal(lambda_0_prop$lambda_0_count, J + 1)
  
  
  # Check acceptance ratio
  lambda_0 <- c(0.01 , 0.01, 0.01, 0.01)
  count_tot <- 0
  acc_tot <- 0
  for (ii in 1:50) {
    lambda_0_prop <- .lambda_0_MH_cp_NoBorrow(df_hist = df_hist, Y_0 = Y_0, I_0 = I_0, 
                                     X_0 = NULL, s = s, beta_0 = NULL, mu = mu,
                                     sigma2 = sigma2,  
                                     lambda_0 = lambda_0,bp_0 = 0,
                                     J = J, clam = 0.8, a_lam = 0.05, 
                                     b_lam = 0.05, lambda_0_count = 0,
                                     lambda_0_move = 0)
    count_tot <- count_tot + lambda_0_prop$lambda_0_count
    acc_tot <- acc_tot + lambda_0_prop$lambda_0_move
    #lambda_0 <- lambda_0_prop$lambda_0
  }
  
  expect_gt(acc_tot/count_tot, 0.2)
  
  # Same seed produces same output
  set.seed(2023)
  lambda_0_prop_1 <- .lambda_0_MH_cp_NoBorrow(df_hist = df_hist, Y_0 = Y_0, I_0 = I_0, 
                                    X_0 = NULL, s = s, beta_0 = NULL, mu = mu,
                                    sigma2 = sigma2,
                                    lambda_0 = lambda_0,  bp_0 = 0,
                                    J = J, clam = 0.8, a_lam = 0.05, 
                                    b_lam = 0.05, lambda_0_count = 0,
                                    lambda_0_move = 0)
  
  set.seed(2023)
  lambda_0_prop_2 <- .lambda_0_MH_cp_NoBorrow(df_hist = df_hist, Y_0 = Y_0, I_0 = I_0, 
                                    X_0 = NULL, s = s, beta_0 = NULL, mu = mu,
                                    sigma2 = sigma2, 
                                    lambda_0 = lambda_0, bp_0 = 0,
                                    J = J, clam = 0.8, a_lam = 0.05, 
                                    b_lam = 0.05, lambda_0_count = 0,
                                    lambda_0_move = 0)
  
  expect_true(all(lambda_0_prop_1$lambda_0 == lambda_0_prop_2$lambda_0))
  expect_true(all(lambda_0_prop_1$df_hist == lambda_0_prop_2$df_hist))
  expect_true(all(lambda_0_prop_1$lambda_0_count == lambda_0_prop_2$lambda_0_count))
  expect_true(all(lambda_0_prop_1$lambda_0_move == lambda_0_prop_2$lambda_0_move))
  
  # with covariates, on historical
  beta_0 <- c(0.5, -1.2, 1.25)
  bp_0 = 3
  X_0 <- matrix(stats::rbinom(3 * length(Y_0), 100, 0.2), ncol = 3)
  
  df_hist <- .dataframe_fun(Y_0, I_0, X_0, s, lambda_0, bp = bp_0, J)
  
  lambda_0_prop <- .lambda_0_MH_cp_NoBorrow(df_hist = df_hist, Y_0 = Y_0, I_0 = I_0, 
                                  X_0 = NULL, s = s, beta_0 = NULL, mu = mu,
                                  sigma2 = sigma2,
                                  lambda_0 = lambda_0, bp_0 = 0,
                                  J = J, clam = 0.8, a_lam = 0.05, 
                                  b_lam = 0.05, lambda_0_count = 0,
                                  lambda_0_move = 0)
  
  # runs without error
  expect_true(all(!is.na(lambda_0_prop)))
  
  
  # expect error for "Inf" values of lambda
  lambda_0 <- c(Inf, 2, 0.5, 1)
  df_hist <- .dataframe_fun(Y_0, I_0, X_0, s, lambda_0, bp = bp_0, J)
  
  expect_error(.lambda_0_MH_cp_NoBorrow(df_hist = df_hist, Y_0 = Y_0, I_0 = I_0, 
                 X_0 = NULL, s = s, beta_0 = NULL, mu = mu,
                 sigma2 = sigma2, 
                 lambda_0 = lambda_0, bp_0 = 0,
                 J = J, clam = 0.8, a_lam = 0.05, b_lam = 0.05, lambda_0_count = 0,
                 lambda_0_move = 0),
               regex = 'missing value')
  
  # expect error for "NA" values of lambda
  lambda_0 <- c(NA, 2, 0.5, 1)
  df_hist <- .dataframe_fun(Y_0, I_0, X_0, s, lambda_0, bp = bp_0, J)
  
  expect_error(.lambda_0_MH_cp_NoBorrow(df_hist = df_hist, Y_0 = Y_0, I_0 = I_0, 
                              X_0 = NULL, s = s, beta_0 = NULL, mu = mu,
                              sigma2 = sigma2, 
                              lambda_0 = lambda_0,bp_0 = 0,
                              J = J, clam = 0.8, a_lam = 0.05, 
                              b_lam = 0.05, lambda_0_count = 0,
                              lambda_0_move = 0),
               regex = 'missing value')
  })

test_that(".lambda_MH_cp works as it should", {
  set.seed(2023)
  Y_0 <-  c(7, 25, 51, 13, 64, 11, 34, 38, 40)
  I_0 <- c(1 ,1 ,0, 1, 1, 1, 1, 0, 0)
  X_0 <- NULL
  Y <- stats::rweibull(10, 0.2, 0.4)
  I <- stats::rbinom(10, 1, 0.4)
  X <- NULL
  s_r <- c(10, 15, 20)
  s <- c(0, sort(s_r),max(c(Y,Y_0)))
  J <- 3
  
  # without covariates, lambda_0
  bp_0 <- 0
  lambda_0 <- c(1.5, 1.4, 1.0, 0.5)
  lambda <- c(0.1, 0.1, 0.1, 0.1)*runif(4,0,1)
  beta_0 <- NULL
  beta <- NULL
  df_hist <- .dataframe_fun(Y_0, I_0, X_0, s, lambda_0, bp = 0, J)
  df_curr <- .dataframe_fun(Y, I, X, s, lambda, bp = 0, J)
  # piecewise borrowing
  tau <- c(9.9, 3.2, 10.5, 7.0)
  mu = 5.0
  sigma2 = 8.5
  
  # Force a move
  lambda_prop <- .lambda_MH_cp(df_hist, df_curr, Y, I, X, s,
                           beta, beta_0, mu, sigma2, lambda, lambda_0, tau, 
                           bp = 0, bp_0 = 0, J, a_lam = 0.01, b_lam = 0.01, lambda_move = 0,
                           lambda_count = 0, alpha = 0.3)
  
  # runs without error
  expect_true(all(!is.na(lambda_prop)))
  
  # produces different values
  expect_gt(lambda_prop$lambda_move, 0)
  
  
  # Check acceptance ratio
  lambda <- c(0.01 , 0.01, 0.01, 0.01)
  count_tot <- 0
  acc_tot <- 0
  for (ii in 1:50) {
    lambda_prop <- .lambda_MH_cp(df_hist, df_curr, Y, I, X, s,
                                  beta, beta_0, mu, sigma2, lambda, lambda_0, tau, 
                                  bp = 0, bp_0 = 0, J, a_lam = 0.01, b_lam = 0.01, lambda_move = 0,
                                lambda_count = 0, alpha = 0.3)
    count_tot <- count_tot + lambda_prop$lambda_count
    acc_tot <- acc_tot + lambda_prop$lambda_move
    #lambda_0 <- lambda_0_prop$lambda_0
  }
  # Error occurs sometimes when there are no events (sum(df$I) == 0)), this might happen very rarely but could trigger a not-so informative error message
  # Should we fix?
  
  expect_gt(acc_tot/count_tot, 0.2)
  
  # Same seed produces same output
  set.seed(2023)
  lambda_prop_1 <- .lambda_MH_cp(df_hist, df_curr, Y, I, X, s,
                                beta, beta_0, mu, sigma2, lambda, lambda_0, tau, 
                                bp = 0, bp_0 = 0, J, a_lam = 0.01, b_lam = 0.01, lambda_move = 0,
                                lambda_count = 0, alpha = 0.3)
  
  set.seed(2023)
  lambda_prop_2 <- .lambda_MH_cp(df_hist, df_curr, Y, I, X, s,
                                beta, beta_0, mu, sigma2, lambda, lambda_0, tau, 
                                bp = 0, bp_0 = 0, J, a_lam = 0.01, b_lam = 0.01, lambda_move = 0,
                                lambda_count = 0, alpha = 0.3)
  
  expect_true(all(lambda_prop_1$lambda == lambda_prop_2$lambda))
  expect_true(all(lambda_prop_1$df_curr == lambda_prop_2$df_curr))
  expect_true(all(lambda_prop_1$lambda_count == lambda_prop_2$lambda_count))
  expect_true(all(lambda_prop_1$lambda_move == lambda_prop_2$lambda_move))
  
  # with covariates, on historical
  beta <- c(0.5, -1.2, 1.25)
  bp = 3
  X <- matrix(stats::rbinom(3 * length(Y), 100, 0.2), ncol = 3)
  
  df_curr <- .dataframe_fun(Y, I, X, s, lambda, bp = bp, J)
  
  lambda_prop <- .lambda_MH_cp(df_hist, df_curr, Y, I, X, s,
                                beta, beta_0, mu, sigma2, lambda, lambda_0, tau, 
                                bp = bp, bp_0 = 0, J, a_lam = 0.01, b_lam = 0.01, lambda_move = 0,
                                lambda_count = 0, alpha = 0.3)
  
  # runs without error
  expect_true(all(!is.na(lambda_prop)))
  
  # expect error for "Inf" values of lambda
  lambda <- c(Inf, 2, 0.5, 1)
  df_curr <- .dataframe_fun(Y, I, X, s, lambda, bp = bp, J)
  
  expect_error(.lambda_MH_cp(df_hist, df_curr, Y, I, X, s,
                            beta, beta_0, mu, sigma2, lambda, lambda_0, tau, 
                            bp = bp, bp_0 = 0, J, a_lam = 0.01, b_lam = 0.01, lambda_move = 0,
                            lambda_count = 0, alpha = 0.3),
               regex = 'missing value')
  
  # expect error for "NA" values of lambda
  lambda <- c(NA, 2, 0.5, 1)
  df_curr <- .dataframe_fun(Y, I, X, s, lambda, bp = bp, J)
  
  expect_error(.lambda_MH_cp(df_hist, df_curr, Y, I, X, s,
                            beta, beta_0, mu, sigma2, lambda, lambda_0, tau, 
                            bp = bp, bp_0 = 0, J, a_lam = 0.01, b_lam = 0.01, lambda_move = 0,
                            lambda_count = 0, alpha = 0.3),
               regex = 'missing value')
  
})
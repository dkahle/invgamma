# Issues:x

set.seed(2023)

n <- 100
s_r <- c(1.0)
lambda <- c(1.0, 3.0)

# Simulate survival times for each segment
id <- stats::runif(n)
beta <- 2
X <- matrix(stats::rbinom(n, size = 1, prob = 0.5)) 
segment1_times <- stats::rexp(sum(id <= 0.5), rate = lambda[1]*exp(beta*X))
segment2_times <- stats::rexp(sum(id > 0.5), rate = lambda[2]*exp(beta*X))

Y <- c(segment1_times, segment2_times)
censoring <- stats::rbinom(n, size = 1, prob = 0.5) 
I <- ifelse(censoring == 1, 0, 1)

# Simulate larger, historic data with similar lambdas
n_0 <- 250
lambda_0 <- c(1.0, 3.0)
id <- stats::runif(n_0)
segment1_times <- stats::rexp(sum(id <= 0.5), rate = lambda_0[1])
segment2_times <- stats::rexp(sum(id > 0.5), rate = lambda_0[2])

Y_0 <- c(segment1_times, segment2_times)
censoring <- stats::rbinom(n_0, size = 1, prob = 0.3)
I_0 <- ifelse(censoring == 1, 0, 1)
X_0 <- NULL
initial_values <- list("J" = 1, 
                     "s_r" = c(1.0),
                     "mu" = 1, 
                     "sigma2" = 2,
                     "tau" = c(2, 3),
                     "lambda_0" = c(1.0, 3.0),
                     "lambda" = c(1.0, 3.0),
                     "beta_0" = NULL,
                     "beta" = c(1.0))

tuning_parameters <- list("Jmax" = 10,
                          "cprop_beta" = 2.4,
                          "alpha" = 0.4,
                          "pi_b" = 0.5)

test_that("Gibbs_WBorrow: output parameters are correct", {
  hyper_parameters <- list("a_tau" = 1, 
                           "b_tau" = 1,
                           "c_tau" = 1,
                           "d_tau" = 0.001, 
                           "type" = "mix",
                           "p_0" = 0.5, 
                           "a_sigma" = 2,
                           "b_sigma" = 2,
                           "clam_smooth" = 0.8, 
                           "phi" = 3)
  iter <- 100

  expect_no_error(.input_check(Y, Y_0, X, X_0, tuning_parameters,  initial_values, hyper_parameters))
  
  out <- GibbsMH(Y, I, X, Y_0, I_0, X_0,
                        tuning_parameters,
                        
                        hyper_parameters,
                        iter = iter)
  # 9 outputs
  expect_equal(length(out), 10)
  
  # 4 "fixed" parameters: J, mu, sigma, beta
  expect_length(out$out_fixed, 4)
  expect_named(out$out_fixed, c("J", "mu", "sigma2", "beta_1"))
  
  # Dimensions are correct for split-point dependent parameters
  expect_equal(dim(out$s), c(iter, tuning_parameters$Jmax + 2))
  expect_equal(dim(out$lambda), c(iter, tuning_parameters$Jmax + 1))
  expect_equal(dim(out$lambda_0), c(iter, tuning_parameters$Jmax + 1))
  expect_equal(dim(out$tau), c(iter, tuning_parameters$Jmax + 1))
  
  # Sampling parameters are scalars
  expect_type(out$lambda_0_move, "double")
  expect_type(out$lambda_move, "double")
})

## Borrowing
test_that("Gibbs_MH_WBorrow runs for mixture borrowing", {
  
  #Specify hyperparameters  on tau
  hyper_parameters <- list("a_tau" = 1, 
                           "b_tau" = 1,
                           "c_tau" = 1,
                           "d_tau" = 0.001, 
                           "type" = "mix",
                           "p_0" = 0.5, 
                           "a_sigma" = 2,
                           "b_sigma" = 2,
                           "clam_smooth" = 0.8, 
                           "phi" = 3)
  
  expect_true(all(!is.na(GibbsMH(Y, I, X, 
                                     Y_0, I_0, X_0,
                                     tuning_parameters,
                                     
                                     hyper_parameters,
                                     iter = 10)$out_fixed)))
  
  out <- GibbsMH(Y, I, X, 
                  Y_0, I_0, X_0,
                  tuning_parameters,
                  
                  hyper_parameters,
                  iter = 100)
  
  expect_gt(out$lambda_0_move, 0)
  expect_gt(out$lambda_move, 0)
  
  # Dimensions are correct for split-point dependent parameters
  expect_equal(dim(out$s), c(100, tuning_parameters$Jmax + 2))
  expect_equal(dim(out$lambda), c(100, tuning_parameters$Jmax + 1))
  expect_equal(dim(out$lambda_0), c(100, tuning_parameters$Jmax + 1))
  expect_equal(dim(out$tau), c(100, tuning_parameters$Jmax + 1))
  
  # Sampling parameters are scalars
  expect_type(out$lambda_0_move, "double")
  expect_type(out$lambda_move, "double")
})

test_that("Gibbs_MH_WBorrow runs for uni borrowing", {
  #Specify hyperparameters  on tau
  hyper_parameters <- list("a_tau" = 1, 
                           "b_tau" = 1,
                           "c_tau" = NA,
                           "d_tau" = NA, 
                           "type" = "uni",
                           "p_0" = NA, 
                           "a_sigma" = 2,
                           "b_sigma" = 2,
                           "clam_smooth" = 0.8, 
                           "phi" = 3)
  
  expect_true(all(!is.na(GibbsMH(Y, I, X, 
                                Y_0, I_0, X_0,
                                 tuning_parameters,
                                         
                                         hyper_parameters,
                                         iter = 10)$out_fixed)))
  
  out <- GibbsMH(Y, I, X, 
                Y_0, I_0, X_0,
                tuning_parameters,
                
                hyper_parameters,
                iter = 100)
  
  expect_gt(out$lambda_0_move, 0)
  expect_gt(out$lambda_move, 0)
  
  # Dimensions are correct for split-point dependent parameters
  expect_equal(dim(out$s), c(100, tuning_parameters$Jmax + 2))
  expect_equal(dim(out$lambda), c(100, tuning_parameters$Jmax + 1))
  expect_equal(dim(out$lambda_0), c(100, tuning_parameters$Jmax + 1))
  expect_equal(dim(out$tau), c(100, tuning_parameters$Jmax + 1))
  
  # Sampling parameters are scalars
  expect_type(out$lambda_0_move, "double")
  expect_type(out$lambda_move, "double")
  
})

test_that("Gibbs_MH_WBorrow runs for non-piecewise borrowing", {
  initial_values <- list("J" = 1, 
                             "s_r" = c(1.0),
                             "mu" = 1, 
                             "sigma2" = 2,
                             "tau" = c(3, 2),
                             "lambda_0" = c(1.0, 3.0),
                             "lambda" = c(1.0, 3.0),
                             "beta_0" = NULL,
                             "beta" = c(1.0) )
  
  hyper_parameters <- list("a_tau" = 1, 
                           "b_tau" = 1,
                           "c_tau" = 0.1,
                           "d_tau" = 0.1, 
                           "type" = "aaa",
                           "p_0" = 0.5, 
                           "a_sigma" = 2,
                           "b_sigma" = 2,
                           "clam_smooth" = 0.8,
                           "phi" = 3)
  
  expect_true(all(!is.na(GibbsMH(Y, I, X, 
                                         Y_0, I_0, X_0,
                                 tuning_parameters,
                                         
                                         hyper_parameters,
                                         iter = 10)$out_fixed)))
  
  out <- GibbsMH(Y, I, X, 
                         Y_0, I_0, X_0,
                 tuning_parameters,
                         
                         hyper_parameters,
                         iter = 100)
  
  expect_gt(out$lambda_0_move, 0)
  expect_gt(out$lambda_move, 0)
  
  # Check that the output tau is now in out_fixed
  expect_length(out$out_fixed, 5)
})

test_that("Gibbs_MH_WBorrow runs for default parameter values", {
  expect_true(all(!is.na(GibbsMH(Y, I, X, 
                                         Y_0, I_0, X_0,
                                         tuning_parameters)$out_fixed)))
  
})

## split points
test_that("Gibbs_MH_WBorrow runs for zero split points", {
  initial_values <- list("J" = 0, 
                             "s_r" = NULL,
                             "mu" = 1, 
                             "sigma2" = 2,
                             "tau" = c(3),
                             "lambda_0" = c(1.0),
                             "lambda" = c(1.0),
                             "beta_0" = NULL,
                             "beta" = c(1.0))
  
  hyper_parameters <- list("a_tau" = 1, 
                           "b_tau" = 1,
                           "c_tau" = 1,
                           "d_tau" = 0.001, 
                           "type" = "aaa",
                           "p_0" = 0.5, 
                           "a_sigma" = 2,
                           "b_sigma" = 2,
                           "clam_smooth" = 0.8, 
                           "phi" = 3)
  
  expect_true(all(!is.na(GibbsMH(Y, I, X, 
                                         Y_0, I_0, X_0,
                                 tuning_parameters,
                                         
                                         hyper_parameters,
                                         iter = 10)$out_fixed)))
  
  out <- GibbsMH(Y, I, X, 
                         Y_0, I_0, X_0,
                 tuning_parameters,
                         
                         hyper_parameters,
                         iter = 100)
  
  expect_gt(out$lambda_0_move, 0)
  expect_gt(out$lambda_move, 0)

})


test_that("Gibbs_MH_WBorrow runs for covariates on historical data", {
  X_0 <- matrix(c(stats::rbinom(length(Y_0),1,0.5), stats::rnorm(length(Y_0),0.1,2),
                        stats::rnorm(length(Y_0),1,2)), ncol = 3)
 
  tuning_parameters$cprop_beta_0 <- 2.4
  initial_values <- list("J" = 1, 
                         "s_r" = c(0.5),
                         "mu" = 1, 
                         "sigma2" = 2,
                         "tau" = c(3, 2),
                         "lambda_0" = c(1.0, 3.0),
                         "lambda" = c(5.0, 2.0),
                         "beta_0" = c(0.1, 0.1, 3.0),
                         "beta" = c(1.0))
  
  hyperparameters <- list("a_tau" = 1, 
                           "b_tau" = 1,
                           "c_tau" = 1,
                           "d_tau" = 0.001, 
                           "type" = "mix",
                           "p_0" = 0.5, 
                           "a_sigma" = 2,
                           "b_sigma" = 2,
                           "clam_smooth" = 0.8,
                           "phi" = 3)
  
  expect_true(all(!is.na(GibbsMH(Y, I, X, 
                                         Y_0, I_0, X_0,
                                 tuning_parameters,
                                         
                                         hyperparameters,
                                         iter = 10)$out_fixed)))
  
  out <- GibbsMH(Y, I, X, 
                         Y_0, I_0, X_0,
                 tuning_parameters,
                         
                         hyperparameters,
                         iter = 100)

  # three covariates in -> three out
  expect_gt(out$lambda_0_move, 0)
  expect_gt(out$lambda_move, 0)

  expect_length(out$out_fixed[paste0("beta_0_",1:3)],3)
  
})


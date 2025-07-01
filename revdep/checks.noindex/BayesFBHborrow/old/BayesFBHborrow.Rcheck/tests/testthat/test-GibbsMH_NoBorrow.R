set.seed(2023)

# Simulate larger, historic data
n_0 <- 250
lambda <- c(1.0, 3.0)
id <- stats::runif(n_0)
segment1_times <- stats::rexp(sum(id <= 0.5), rate = lambda[1])
segment2_times <- stats::rexp(sum(id > 0.5), rate = lambda[2])

Y <- c(segment1_times, segment2_times)
censoring <- stats::rbinom(n_0, size = 1, prob = 0.3)
I <- ifelse(censoring == 1, 0, 1)
X <- NULL

initial_parameters <- list("J" = 1, 
                           "s_r" = c(1.0),
                           "mu" = 1, 
                           "sigma2" = 2,
                           "lambda" = lambda,
                           "beta" = NULL)

tuning_parameters <- list("Jmax" = 10,
                          "cprop_beta" = NULL,
                          "pi_b" = 0.5)

test_that("Gibbs_NoBorrow: output parameters are correct", {
  hyperparameters <- list("a_sigma" = 2,
                           "b_sigma" = 2,
                           "clam_smooth" = 0.8, 
                           "phi" = 3)
  iter <- 100
  
  .input_check(Y, NULL, X, NULL, tuning_parameters, initial_parameters, hyperparameters)
  
  expect_no_error(GibbsMH(Y, I, X, NULL, NULL, NULL, 
                          tuning_parameters,
                          hyperparameters,
                          iter = iter))
  
  out <- GibbsMH(Y, I, X, NULL, NULL, NULL, 
                 tuning_parameters,
                 
                 hyperparameters,
                 iter = iter)
  # 6 outputs
  expect_equal(length(out), 7)
  
  # 3 "fixed" parameters: J, mu, sigma
  expect_length(out$out_fixed, 3)
  expect_named(out$out_fixed, c("J", "mu", "sigma2"))
  
  # Dimensions are correct for split-point dependent parameters
  expect_equal(dim(out$s), c(iter, tuning_parameters$Jmax + 2))
  expect_equal(dim(out$lambda), c(iter, tuning_parameters$Jmax + 1))
  
  # Sampling parameters are scalars and above zero
  expect_type(out$lambda_move, "double")
  expect_gt(out$lambda_move, 0)
})


test_that("Gibbs_MH_NoBorrow runs for default parameter values", {
  expect_true(all(!is.na(GibbsMH(Y, I, X, NULL, NULL, NULL, 
                                 tuning_parameters)$out_fixed)))
  
})

## split points
test_that("Gibbs_MH_NoBorrow runs for zero split points", {
  initial_parameters <- list("J" = 0, 
                             "s_r" = NULL,
                             "mu" = 1, 
                             "sigma2" = 2,
                             "lambda" = c(1.0),
                             "beta" = NULL)
  
  hyperparameters <- list("a_sigma" = 2,
                          "b_sigma" = 2,
                          "clam_smooth" = 0.8, 
                          "phi" = 3)
  
  expect_true(all(!is.na(GibbsMH(Y, I, X, NULL, NULL, NULL, 
                                 tuning_parameters,
                                          
                                         hyperparameters, 
                                         iter = 10)$out_fixed)))
  
  out <- GibbsMH(Y, I, X, NULL, NULL, NULL, 
                 tuning_parameters,
                         
                         hyperparameters, 
                         iter = 100)
  
  expect_gt(out$lambda_move, 0)
})


test_that("Gibbs_MH_NoBorrow runs for covariates on historical data", {
  # rate proposal blows up for medium -> large values of beta
  X <- matrix(c(stats::rbinom(length(Y),1,0.5),
                  stats::rnorm(length(Y),0,2),
                  stats::rnorm(length(Y),0,2)), ncol = 3)
  
  initial_parameters <- list("J" = 1, 
                             "s_r" = c(0.5),
                             "mu" = 1, 
                             "sigma2" = 2,
                             "lambda" = c(1.0, 3.0),
                             "beta" = c(0.1, 0.1, 0.1))
  
  hyperparameters <- list("a_sigma" = 2,
                          "b_sigma" = 2,
                          "clam_smooth" = 0.8, 
                          "phi" = 3)
  
  tuning_parameters$cprop_beta = 2.4 #controls proposal sd for all beta 

  
  expect_true(all(!is.na(GibbsMH(Y, I, X, NULL, NULL, NULL, 
                                 tuning_parameters,
                                         
                                         hyperparameters,
                                         iter = 10)$out_fixed)))
  
  expect_no_error(out <- GibbsMH(Y, I, X, NULL, NULL, NULL,
                                 tuning_parameters,
                                 
                                 hyperparameters, 
                                 iter = 100))
  
  # three covariates in -> three out
  expect_gt(out$lambda_move, 0)
  expect_length(out$out_fixed[paste0("beta_",1:3)],3)
  
})

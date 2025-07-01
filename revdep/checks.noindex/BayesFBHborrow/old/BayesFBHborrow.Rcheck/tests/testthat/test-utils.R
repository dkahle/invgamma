test_that(".dataframe_fun.R has correct formatting", {
  Y <-  c(9, 13, 13, 18, 23)
  I <- c(1 ,1 ,0, 1, 1)
  X <- NULL
  s_r <- 10
  s <- c(0, sort(s_r), max(Y))
  lambda <- c(0.1, 0.9)
  J <- 1

  order <- c("tstart","id","Y", "I", "lambda")
  
  # Order, no covariates
  df <- .dataframe_fun(Y, I, X, s, lambda, bp = 0, J)
  expect_equal(names(df), order)
  
  # Order, with covariates
  X <- matrix(c(stats::rbinom(length(Y),5,0.5), stats::rbinom(length(Y),5,0.5), 
         stats::rbinom(length(Y),5,0.5)), ncol = 3)
  df <- .dataframe_fun(Y, I, X, s, lambda, bp = 3, J)
  order <- c("tstart","id","X1", "X2", "X3","Y", "I", "lambda")
  expect_equal(names(df), order)
  
  # Data Consistency, number of events is equal before and after split
  expect_equal(1, sum(df[df$tstart == 0,]$I))
  expect_equal(3, sum(df[df$tstart == 10,]$I))
  
  # Events are the same even when data is not ordered
  Y_wrong <-  c(13, 18, 23, 13, 9)
  I_wrong <- c(1 ,1 ,1, 0, 1)
  expect_equal(1, sum(df[df$tstart == 0,]$I))
  expect_equal(3, sum(df[df$tstart == 10,]$I))
  
  # Expect error when s is defined wrong (needs to be >= 2)
  # Written specifically in code, should not occur
  
  # Missing Data Handling
  Y_missing <-  c(9, NA, 13, 18, NA)
  I_missing <- c(1 , NA, 0, 1, NA)
  df_missing <- .dataframe_fun(Y_missing, I_missing, X, s, lambda, bp = 0, J)
  expect_equal(sum(is.na(df_missing$Y)), sum(is.na(Y_missing)))
  
  # there is some weird behavior in survSplit where it will give different values of I
  # (NA or 0) every time you run it, not sure if this interferes with anything [Q]
  
  # Empty dataframe, should not be able to split data
  expect_error(data.frame.fun(c(), c(), c(), s, lambda, bp = 0, J))
  
  # Performance Test
  X_large <- matrix(nrow = 1000, ncol = 1000)
  Y_large <- 1:1000
  I_large <- rbinom(n=1000, size=1, prob=0.5)
  for (ii in 1:1000) {
    X_large[ii,] <- 1:1000
  }
  system.time(df_large <- .dataframe_fun(Y_large, I_large, X_large, s, lambda, bp = ncol(X_large), J))
  expect_true(ncol(df_large) == 5+1000)
})

test_that(".logsumexp operates as it should", {
  
  # Postive values
  input_pos <- c(0.5, 0.4, 0.1, 0.01)
  expected_result <- log(sum(exp(input_pos)))
  expect_equal(.logsumexp(input_pos), expected_result)
  
  # Negative values
  input_neg <- c(-0.5, -0.4, -0.1, -0.01)
  expected_result <- log(sum(exp(input_neg)))
  expect_equal(.logsumexp(input_neg), expected_result)
  
  # All zero
  input_zero <- c(0, 0, 0, 0, 0)
  expected_result <- log(sum(exp(input_zero)))
  expect_equal(.logsumexp(input_zero), expected_result)
  
  # NAs present, should they be ignored? Or checked before? [Q]
  input_NA <- c(0.5, 0.4, NA, 0.01)
  expected_result <- log(sum(exp(input_NA)))
  expect_equal(.logsumexp(input_NA), expected_result)
  
})

test_that(".normalize_prob operates as it should", {
  
  # Postive values
  input_pos <- c(0.5, 0.4, 0.1, 0.01)
  expected_result <- exp(input_pos - .logsumexp(input_pos))
  expect_equal(.normalize_prob(input_pos), expected_result)
  
  # Negative values
  input_neg <- c(-0.5, -0.4, -0.1, -0.01)
  expected_result <- exp(input_neg - .logsumexp(input_neg))
  expect_equal(.normalize_prob(input_neg), expected_result)
  
  # All zero
  input_zero <- c(0, 0, 0, 0, 0)
  expected_result <- exp(input_zero - .logsumexp(input_zero))
  expect_equal(.normalize_prob(input_zero), expected_result)
  
  # NAs present, should they be ignored? Or checked before? [Q]
  input_NA <- c(0.5, 0.4, NA, 0.01)
  expected_result <- exp(input_NA - .logsumexp(input_NA))
  expect_equal(.normalize_prob(input_NA), expected_result)
  
})

test_that(".log_likelihood operates as it should", {
  Y <-  c(9, 13, 13, 18, 23)
  I <- c(1 ,1 ,0, 1, 1)
  X <- NULL
  s_r <- 10
  s <- c(0, sort(s_r), max(Y))
  lambda <- c(0.1, 0.9)
  J <- 1
  df <- .dataframe_fun(Y, I, X, s, lambda, bp = 0, J)
  
  ## Without covariates
  # Empty data.frames
  expect_error(.log_likelihood(df = c(), beta = NULL))
  
  # NAs present, should it return NA? How should we handle NAs? [Q]
  Y_NA <-  c(NA, 13, 13, 18, 23)
  I_NA <- c(NA , 1, 0, 1, 1)
  df <- .dataframe_fun(Y_NA, I_NA, X, s, lambda, bp = 0, J)
  expect_equal(sum(is.na(.log_likelihood(df, beta = NULL))), 1)
  
  ## With covariates
  X <- matrix(c(stats::rbinom(length(Y),5,0.5), stats::rbinom(length(Y),5,0.5), 
                stats::rbinom(length(Y),5,0.5)), ncol = 3)
  beta = c(-2, 0.5, 1)
  df <- .dataframe_fun(Y, I, X, s, lambda, bp = 3, J)
  
  # NAs present, should it return NA? How should we handle NAs? [Q]
  Y_NA <-  c(NA, 13, 13, 18, 23)
  I_NA <- c(NA , 1, 0, 1, 1)
  df <- .dataframe_fun(Y_NA, I_NA, X, s, lambda, bp = 3, J)
  expect_equal(sum(is.na(.log_likelihood(df, beta = beta))), 1)
  
  # Message for zero/high/low?
  
})

Y <- c(1, 20, 30)
Y_0 <- c(1, 20, 31)
X <- matrix(1, nrow = 3, ncol = 1)
X_0 <- NULL
tuning_parameters <- list("cprop_beta" = 1,
                          "Jmax" = 10,
                          "pi_b" = 0.5,
                          "alpha" = 0.4)
hyper <- list("a_tau" = 1, 
              "b_tau" = 1, 
              "c_tau" = 1, 
              "d_tau" = 1, 
              "p_0" = 0.1, 
              "clam_smooth" = 0.7, 
              "type" = "mix", 
              "a_sigma" = 1, 
              "b_sigma" = 2, 
              "phi" = 2)

initial_values <- list("J" = 2, 
                       "s_r" = c(5, 10), 
                       "mu" = 5, 
                       "sigma2" = 0.5, 
                       "tau" = c(0.1, 0.2, 0.3), 
                       "lambda_0" = c(0.1, 0.2, 0.3), 
                       "lambda" = c(0.1, 0.2, 0.3), 
                       "beta_0" = NULL, 
                       "beta" = -0.5) 

test_that("Input check: valid inputs do not produce errors/warnings", {
 expect_no_error(.input_check(Y, Y_0, X, X_0, tuning_parameters, NULL, hyper))
 expect_no_error(.input_check(Y, Y_0, X, X_0, tuning_parameters, initial_values, hyper))
})

test_that("Input check: Negative hyperparameter values produce errors", {
  hyper$a_sigma <- -1
  
  expect_error(.input_check(Y, Y_0, X, X_0,  tuning_parameters, NULL, hyper), regexp = "negative")
  expect_error(.input_check(Y, Y_0, X, X_0,  tuning_parameters, initial_values, hyper), regexp = "negative")
})

test_that("Input check: Hyperparameters outside [0, 1] range produce errors", {
  tuning_parameters$pi_b <- 1.3 #error

  expect_error(.input_check(Y, Y_0, X, X_0, tuning_parameters, NULL, hyper), regexp = "range")
  expect_error(.input_check(Y, Y_0, X, X_0, tuning_parameters, initial_values, hyper), regexp = "range")
})

test_that("Input check: borrowing type 'uni' with given c_tau and d_tau produces a warning", {
  hyper$type <- "uni"
    suppressMessages( expect_message(.input_check(Y, Y_0, X, X_0, tuning_parameters, initial_values, hyper), "borrow is 'uni'"))
})

test_that("Input check: s_r values greater than maxSj produce errors", {
  initial_values$s_r = c(5, 50)
  expect_error(.input_check(Y, Y_0, X, X_0, tuning_parameters, initial_values, 
                            hyper), regexp = "s_r must be <")
})

test_that("Input check: negative sigma2 value produces an error", {
  initial_values$sigma2 <- -0.5
  expect_error(.input_check(Y, Y_0, X, X_0, tuning_parameters, initial_values, hyper), regexp = "sigma2 must be > 0")
})

test_that("Input check: incorrect dimensions for tau produce error", {
 initial_values$tau = c(0.1, 0.2, 0.3, 0.1)
 expect_error(.input_check(Y, Y_0, X, X_0, tuning_parameters, initial_values, hyper), regexp = "dimension")

 initial_values$lambda = c(0.1, 0.2, 0.3, 0.1)
 expect_error(.input_check(Y, Y_0, X, X_0, tuning_parameters, initial_values, hyper), regexp = "dimension")
})

test_that("Input check: incorrect dimensions for lambda produce error", {
  initial_values$lambda_0 = c(0.1, 0.2, 0.3, 0.1)
  expect_error(.input_check(Y, Y_0, X, X_0, tuning_parameters, initial_values, hyper), regexp = "dimension")
})

test_that("Input check: Negative lambdas renders error", {
  initial_values$lambda <- c(-0.1, 0.2, 0.3)
  expect_error(.input_check(Y, Y_0, X, X_0, tuning_parameters, initial_values, hyper), regexp = "baseline hazard")
})

test_that("Input check: throws error for wrong dimension of beta/beta_0 when adding covariates", {
  X_0 <- matrix(c(1,0, 0), nrow = 3, ncol = 1)
  tuning_parameters$cprop_beta_0 <- 0.5
  initial_values$beta_0 <- NULL
  expect_error(.input_check(Y, Y_0, X, X_0, tuning_parameters, initial_values, hyper), regexp = "dimension")
  
  initial_values$beta = c(1, 2)
  expect_error(.input_check(Y, Y_0, X, X_0, tuning_parameters, initial_values, hyper), regexp = "beta")
  
})


test_that("Input check for cprop_beta dimensions", {
  tuning_parameters$cprop_beta = c(1, 1)
  expect_error(.input_check(Y, Y_0, X, X_0, tuning_parameters, initial_values, hyper),
               regexp = "dimension mismatch")
})

test_that("group_summary() is working as it should", {
  Y <- stats::rweibull(20, 10, 0.4)
  I <- stats::rbinom(20, 1, 0.2)
  
  #W Without covariates
  X <- NULL
  
  s <- c(0, quantile(Y, probs = c(0.25, 0.75)), max(Y))
  
  # Throws no error for normal input
  expect_no_error(group_no_covariates <- group_summary(Y, I, X, s))
  expect_true(any(!is.na(group_no_covariates)))
  
  # Has correct output
  expect_equal(sum(group_no_covariates$events), sum(I))
  expect_equal(sum(group_no_covariates$num_cnsr), sum(I == 0))
  
  #W With covariates
  X <- stats::rbinom(20, 1, 0.5)
  
  # Throws no error for normal input
  expect_no_error(group <- group_summary(Y, I, X, s))
  expect_true(any(!is.na(group)))
  
  # Has correct output
  expect_equal(sum(group$events_c), sum(I[X == 0])) # control
  expect_equal(sum(group$events_t), sum(I[X == 1])) # treatment
  expect_true(all(group$num_at_risk_c + group$num_at_risk_t == group_no_covariates$num_at_risk))
  
  
})

test_that("init_lambda_hyperparameters() is working as it should", {
  Y <- stats::rweibull(20, 10, 0.4)
  I <- stats::rbinom(20, 1, 0.2)
  
  # Without covariates
  X <- NULL
  
  s <- c(0, quantile(Y, probs = c(0.25, 0.75)), max(Y))
  
  # Throws no error for normal input
  group_data <- group_summary(Y, I, X, s)
  expect_no_error(lambdas <- init_lambda_hyperparameters(group_data, s, w = 0.5))
  
  # Shape is correct:
  expect_length(lambdas$shape, length(s) - 1)
  expect_length(lambdas$rate, length(s) - 1)

})

test_that("Input check runs for only historical/current data (NoBorrow)" ,{
  Y <- c(1, 20, 30)
  Y_0 <- NULL
  X <- matrix(1, nrow = 3, ncol = 1)
  X_0 <- NULL
  tuning_parameters <- list("cprop_beta" = 1,
                            "Jmax" = 10,
                            "pi_b" = 0.5)
  hyper <- list("a_sigma" = 1, 
                "b_sigma" = 2, 
                "phi" = 2)
  
  initial_values <- list("J" = 2, 
                         "s_r" = c(5, 10), 
                         "mu" = 5, 
                         "sigma2" = 0.5, 
                         "lambda" = c(0.1, 0.2, 0.3), 
                         "beta" = -0.5) 
  
  expect_no_error(.input_check(Y, Y_0, X, X_0, tuning_parameters, initial_values, hyper))
  
  s <- .input_check(Y, Y_0, X, X_0, tuning_parameters, initial_values, hyper)
  expect_equal(s, "No borrow")
})

test_that("Setting tuning_parameters works" ,{
  # With borrowing, without covariates on historical, everything given
  X <- matrix(1, nrow = 3, ncol = 1)
  X_0 <- NULL
  tuning_parameters <- list("cprop_beta" = 1,
                            "Jmax" = 10,
                            "pi_b" = 0.5,
                            "alpha" = 1)
  
  new_tuning_param <- .set_tuning_parameters(tuning_parameters = tuning_parameters, 
                                             borrow = TRUE, X, X_0)
  
 expect_equal(tuning_parameters, new_tuning_param)
 
 # With borrowing, withcovariates on historical, missing historical cprop
 X <- matrix(1, nrow = 3, ncol = 1)
 X_0 <- matrix(1, nrow = 3, ncol = 1)
 tuning_parameters <- list("cprop_beta" = 1,
                           "Jmax" = 10,
                           "pi_b" = 0.5,
                           "alpha" = 1)
 
  suppressMessages(expect_message( new_tuning_param <- .set_tuning_parameters(tuning_parameters = tuning_parameters, 
                                            borrow = TRUE, X, X_0),
 "The following tuning_parameters were set to default: cprop_beta_0"))
 
 expect_equal(new_tuning_param$cprop_beta_0, 0.5)
 expect_equal(new_tuning_param$cprop_beta, tuning_parameters$cprop_beta)
 
 
 # Without borrowing, without covariates on historical, everything given
 X <- matrix(1, nrow = 3, ncol = 1)
 X_0 <- NULL
 tuning_parameters <- list("cprop_beta" = 1,
                           "Jmax" = 10,
                           "pi_b" = 0.5)
 
 new_tuning_param <- .set_tuning_parameters(tuning_parameters = tuning_parameters, 
                                            borrow = FALSE, X, X_0)
 
 expect_equal(tuning_parameters, new_tuning_param)
 
 # Without borrowing, without covariates on historical, none given
 X <- matrix(1, nrow = 3, ncol = 1)
 X_0 <- NULL
 tuning_parameters_default <- list("Jmax" = 5,
                           "pi_b" = 0.5,
                           "cprop_beta" = 0.5)
 
  suppressMessages(expect_message(new_tuning_param <- .set_tuning_parameters(tuning_parameters = NULL, 
                                            borrow = FALSE, X, X_0),
                                    "The following tuning_parameters were set to default: Jmax, pi_b, cprop_beta"))
 
 expect_equal(new_tuning_param, tuning_parameters_default)
})

test_that("Setting hyperparameters works" ,{
  # mix, everything given
  hyperparameters_mix_default <- list(
    "a_tau" = 1,
    "b_tau" = 0.001,
    "c_tau" = 1,
    "d_tau" = 1,
    "type" = "mix",
    "p_0" = 0.8,
    "a_sigma" = 1,
    "b_sigma" = 1,
    "phi" = 3, 
    "clam_smooth" = 0.8)
  
  suppressMessages(
  new_hyperparameters <- .set_hyperparameters(hyperparameters = hyperparameters_mix_default, model_choice = "mix"))
  
  expect_equal(new_hyperparameters, hyperparameters_mix_default)
  
  # all, everything given
  hyperparameters <- list(
    "a_tau" = 1,
    "b_tau" = 0.001,
    "c_tau" = 1,
    "d_tau" = 1,
    "type" = "all",
    "p_0" = 0.8,
    "a_sigma" = 1,
    "b_sigma" = 1,
    "phi" = 3, 
    "clam_smooth" = 0.8)
  
  suppressMessages(
  new_hyperparameters <- .set_hyperparameters(hyperparameters = hyperparameters, model_choice = "all"))
  
  expect_equal(new_hyperparameters, hyperparameters)
  
  
  # uni, everything given
  hyperparameters <- list(
    "a_tau" = 1,
    "b_tau" = 0.001,
    "type" = "uni",
    "a_sigma" = 1,
    "b_sigma" = 1,
    "phi" = 3, 
    "clam_smooth" = 0.8)
  
  suppressMessages(
  new_hyperparameters <- .set_hyperparameters(hyperparameters = hyperparameters, model_choice = "uni"))
  
  expect_equal(new_hyperparameters, hyperparameters)
  
  
  # no_borrow, everything given
  hyperparameters_default <- list(
    "a_sigma" = 1,
    "b_sigma" = 1,
    "phi" = 3, 
    "clam_smooth" = 0.8)
  
  suppressMessages(
  new_hyperparameters <- .set_hyperparameters(hyperparameters = hyperparameters_default, model_choice = "no_borrow"))
  
  expect_equal(new_hyperparameters, hyperparameters_default)
  # mix, bits missing
  hyperparameters <- list(
    "a_tau" = 1,
    "b_tau" = 0.001,
    "type" = "mix",
    "p_0" = 0.8,
    "a_sigma" = 1,
    "b_sigma" = 1,
    "phi" = 3)
  
  suppressMessages(expect_message(  new_hyperparameters <- .set_hyperparameters(hyperparameters = hyperparameters, model_choice = "mix"),
  "The following hyperparameters were set to default: c_tau, d_tau, clam_smooth"))
  
  expect_equal(new_hyperparameters[order(names(new_hyperparameters))], 
               hyperparameters_mix_default[order(names(hyperparameters_mix_default))])
  
  hyperparameters <- list(
    "a_tau" = 1,
    "b_tau" = 0.001,
    "type" = "mix",
    "p_0" = 0.8,
    "a_sigma" = 10,
    "b_sigma" = 1,
    "phi" = 3)
  
  suppressMessages(
    expect_message(new_hyperparameters <- .set_hyperparameters(hyperparameters = hyperparameters, model_choice = "mix"),
                   "The following hyperparameters were set to default: c_tau, d_tau, clam_smooth"))
  
  suppressMessages(
  expect_equal(hyperparameters$a_sigma, new_hyperparameters$a_sigma))
  
  # no_borow, bits missing
  suppressMessages(
    expect_message(new_hyperparameters <- .set_hyperparameters(hyperparameters = NULL, model_choice = "no_borrow"),
  "The following hyperparameters were set to default: a_sigma, b_sigma, phi, clam_smooth"))
  
  expect_equal(new_hyperparameters[order(names(new_hyperparameters))], 
               hyperparameters_default[order(names(hyperparameters_default))])
})

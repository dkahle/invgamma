set.seed(2024)
n <- 10
Y <- stats::rweibull(n, 0.4, 5)
I <- stats::rbinom(n, 1, 0.3) 
X <- stats::rbinom(n, 1, 0.5) #50/50 placebo
data <- data.frame("tte" = Y, "event" = I, "X1" = X)
Y_0 <- stats::rweibull(n*5, 0.4, 5)
I_0 <- stats::rbinom(n*5, 1, 0.3) 
X_0 <- stats::rbinom(n*5, 1, 0.5) #50/50 placebo
data_hist <- data.frame("tte" = Y_0, "event" = I_0, "X1" = X_0)


tuning_parameters <- list("Jmax" = 20,
                          "cprop_beta" = 0.3,
                          "cprop_beta_0" = 0.3,
                          "alpha" = 0.4,
                          "pi_b" = 0.5)

hyper <- list("a_tau" = 1, 
              "b_tau" = 1,
              "c_tau" = 1,
              "d_tau" = 0.001, 
              "p_0" = 0.5, 
              "a_sigma" = 2,
              "b_sigma" = 2,
              "clam_smooth" = 0.8, 
              "phi" = 3)

borrow = TRUE
model_choice = "mix"

test_that("BayesFBHborrow works correctly for a normal input (with historical covariates)", {
  
  suppressMessages(
    expect_message(
      expect_message(BayesFBHborrow(data, data_hist, borrow, model_choice, tuning_parameters, hyper, iter = 10,
                                    warmup_iter = 1, verbose = TRUE)),
      "MCMC sampler complete")
  )
  
  expect_no_message(BayesFBHborrow(data, data_hist, borrow, model_choice, tuning_parameters,  hyper, 
                                        iter = 10,  warmup_iter = 1, verbose = FALSE))
  
  # Outputs are correct
  out <- BayesFBHborrow(data, data_hist, borrow, model_choice, tuning_parameters,  hyper, 
                        iter = 100,  warmup_iter = 5, refresh = 0, verbose = FALSE)
  expect_length(out$out, 13)
})

test_that("BayesFBHborrow fails for NA values", {
  data$tte[[1]] <- NA
  expect_error(BayesFBHborrow(data, data_hist, borrow, model_choice, tuning_parameters, hyper, 
                             iter = 10, warmup_iter = 1, verbose = FALSE))
  
})

test_that("BayesFBHborrow fails for wrong type of input", {
  # should work for tibble
  data$tte[[1]] <- 1
  expect_no_error(BayesFBHborrow(tibble::tibble(data), data_hist,  borrow, model_choice, 
                                 tuning_parameters,hyper, 
                            iter = 10, warmup_iter = 1, verbose = FALSE))
  
  # fail for list
  expect_error(BayesFBHborrow(list(data), data_hist, tuning_parameters,  borrow,
                              model_choice, hyper, 
                            iter = 10, warmup_iter = 1, verbose = FALSE),
               "data.frame")
})

#test_that("BayesFBHborrow fails for wrong length of input", {
#  
#  expect_error(BayesFBHborrow(data, data_hist, borrow, model_choice, tuning_parameters,  hyper[-1],
#                            iter = 10, warmup_iter = 1, verbose = FALSE),
#               "missing elements")
#  
#  expect_error(BayesFBHborrow(data, data_hist, borrow, model_choice, tuning_parameters, initial_values[-1], hyper, 
#                            iter = 10, warmup_iter = 1, verbose = FALSE),
#               "initial_values")
#  
#  expect_error(BayesFBHborrow(data, data_hist, borrow, model_choice, tuning_parameters[-1], initial_values, hyper, 
#                              iter = 10, warmup_iter = 1, verbose = FALSE),
#               "tuning")
#})

test_that("default values still hold (WBorrow)", {
  suppressMessages(expect_no_error(BayesFBHborrow(data, data_hist, iter = 50, warmup_iter = 10)))
})

initial_values <- list("J" = 1, 
                           "s_r" = c(mean(Y)),
                           "mu" = 1, 
                           "sigma2" = 2,
                           "lambda" = c(1.0, 3.0),
                           "beta" = c(1.0))

tuning_parameters <- list("Jmax" = 20,
                          "cprop_beta" = 0.3,
                          "pi_b" = 0.5)

hyper <- list("a_sigma" = 2,
              "b_sigma" = 2,
              "Jmax" = 20, 
              "clam_smooth" = 0.8, 
              "phi" = 3)

borrow = FALSE

test_that("BayesFBHborrow.Noborrow works correctly for a normal input", {
  
  suppressMessages(
    expect_message(BayesFBHborrow(data, NULL, borrow, model_choice, tuning_parameters, hyper, 
                                    iter = 10,  warmup_iter = 1, verbose = TRUE)))
  
  expect_no_message(BayesFBHborrow(data, NULL, borrow, model_choice, 
                                     iter = 10,  warmup_iter = 1, verbose = FALSE))
  
  # Outputs are correct
  out <- BayesFBHborrow(data, NULL, iter = 100,  warmup_iter = 5, refresh = 0, verbose = FALSE)
  expect_length(out$out, 9)
})

test_that("BayesFBHborrow fails for NA values", {
  data$tte[[1]] <- NA
  expect_error(BayesFBHborrow(data, NULL, borrow, model_choice, tuning_parameters, hyper, 
                                iter = 10, warmup_iter = 1, verbose = FALSE))
  
})

test_that("BayesFBHborrow fails for wrong type of input", {
  # should work for tibble
  data$tte[[1]] <- 1
  expect_no_error(BayesFBHborrow(tibble::tibble(data), NULL, borrow, model_choice,tuning_parameters, hyper, 
                                   iter = 10, warmup_iter = 1, verbose = FALSE))
  
  # fail for list
  expect_error(BayesFBHborrow(list(data), NULL, borrow, model_choice,tuning_parameters, hyper, 
                                iter = 10, warmup_iter = 1, verbose = FALSE),
               "data.frame")
})

#test_that("BayesFBHborrow fails for wrong length of input", {
#  
#  expect_error(BayesFBHborrowte(data, NULL, borrow, model_choice, tuning_parameters, initial_values, hyper[-1],
#                                iter = 10, warmup_iter = 1, verbose = FALSE),
#               "missing elements")
#  
#  expect_error(BayesFBHborrow(data, NULL, borrow, model_choice,tuning_parameters, initial_values[-1], hyper, 
#                                iter = 10, warmup_iter = 1, verbose = FALSE),
#               "initial_values")
#})

test_that("default values still hold (NoBorrow)", {
  expect_no_error(BayesFBHborrow(data, NULL, iter = 50, warmup_iter = 10))
})


test_that("Output class is correct", {
  out <- BayesFBHborrow(data, NULL, borrow = FALSE, model_choice, tuning_parameters, hyper, 
           iter = 10, warmup_iter = 1, verbose = FALSE)
  expect_s3_class(out$out, c("list", "NoBorrow"))
  
  initial_values <- list("J" = 1, 
                         "s_r" = c(mean(Y)),
                         "mu" = 1, 
                         "sigma2" = 2,
                         "tau" = c(2, 3),
                         "lambda_0" = c(1.0, 3.0),
                         "lambda" = c(1.0, 3.0),
                         "beta_0" = c(2.0),
                         "beta" = c(1.0))
  
  tuning_parameters <- list("Jmax" = 20,
                            "cprop_beta" = 0.3,
                            "alpha" = 0.4,
                            "pi_b" = 0.5)
  
  hyper <- list("a_tau" = 1, 
                "b_tau" = 1,
                "c_tau" = 1,
                "d_tau" = 0.001, 
                "type" = "mix",
                "p_0" = 0.5, 
                "a_sigma" = 2,
                "b_sigma" = 2,
                "clam_smooth" = 0.8, 
                "phi" = 3)
  
  out <- BayesFBHborrow(data, data_hist, borrow = TRUE, model_choice, tuning_parameters, hyper, 
                  iter = 10, warmup_iter = 1, verbose = FALSE)
  expect_s3_class(out$out, c("WBorrow", "list"))
})

test_that("No errors for combination of missing inputs of 'uni'-type borrowing", {
  # missing p_0
  initial_values <- list("J" = 1, 
                             "s_r" = c(mean(Y)),
                             "mu" = 1, 
                             "sigma2" = 2,
                             "tau" = c(2, 3),
                             "lambda_0" = c(0.1, 0.1),
                             "lambda" = c(0.1, 0.1),
                             "beta_0" = c(2.0),
                             "beta" = c(1.0))
  
  hyper <- list("a_tau" = 1, 
                "b_tau" = 5,
                "c_tau" = 1,
                "d_tau" = 0.001, 
                "type" = "uni",
                "p_0" = NULL, #
                "a_sigma" = 2,
                "b_sigma" = 2,
                "clam_smooth" = 0.8, 
                "phi" = 3)
  
  tuning_parameters <- list("Jmax" = 20,
                            "cprop_beta" = 0.3,
                            "alpha" = 0.4,
                            "pi_b" = 0.5)
  
  expect_no_error(out <- BayesFBHborrow(data, data_hist, borrow = TRUE, 
                                        model_choice, tuning_parameters, hyper, 
                           iter = 100, warmup_iter = 100, verbose = FALSE))
  expect_gt(out$out$lambda_move, 0)
  expect_gt(out$out$lambda_0_move, 0)
  
  hyper <- list("a_tau" = 1, 
                "b_tau" = 5,
                "c_tau" = 1,
                "d_tau" = NULL, #
                "type" = "uni",
                "p_0" = 0.5, 
                "a_sigma" = 2,
                "b_sigma" = 2,
                "clam_smooth" = 0.8, 
                "phi" = 3)
  
  expect_no_error(out <- BayesFBHborrow(data, data_hist, borrow = TRUE, model_choice, tuning_parameters, hyper, 
                                  iter = 100, warmup_iter = 100, verbose = FALSE))
  expect_gt(out$out$lambda_move, 0)
  expect_gt(out$out$lambda_0_move, 0)
  
  hyper <- list("a_tau" = 1, 
                "b_tau" = 5,
                "c_tau" = NULL,#
                "d_tau" = 10, 
                "type" = "uni",
                "p_0" = 0.5,
                "a_sigma" = 2,
                "b_sigma" = 2,
                "clam_smooth" = 0.8, 
                "phi" = 3)
  
  expect_no_error(out <- BayesFBHborrow(data, data_hist, borrow = TRUE , model_choice, tuning_parameters, hyper, 
                                  iter = 100, warmup_iter = 100, verbose = FALSE))
  expect_gt(out$out$lambda_move, 0)
  expect_gt(out$out$lambda_0_move, 0)
})


test_that("produces the correct amount of plots", {
  out <- BayesFBHborrow(data, NULL, borrow = FALSE, model_choice, tuning_parameters, hyper, 
                        iter = 10, warmup_iter = 1, verbose = FALSE)
  expect_equal(length(out$plots), 7)
  
  out <- BayesFBHborrow(data, data_hist, borrow = TRUE, model_choice, tuning_parameters, hyper, 
                        iter = 10, warmup_iter = 1, verbose = FALSE)
  expect_equal(length(out$plots), 9)
})

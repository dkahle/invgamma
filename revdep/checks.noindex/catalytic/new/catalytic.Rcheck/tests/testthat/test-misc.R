set.seed(1)

glm_gaussian_data <- mock_glm_gaussian_data()
cat_init_glm_gaussian <- mock_cat_glm_gaussian_initialization(glm_gaussian_data)

glm_binomial_data <- mock_glm_binomial_data()
cat_init_glm_binomial <- mock_cat_glm_binomial_initialization(glm_binomial_data)

cox_data <- mock_cox_data()
cat_init_cox <- mock_cat_cox_initialization(cox_data)

lmm_data <- mock_lmm_data()
cat_init_lmm <- mock_cat_lmm_initialization(lmm_data)


# ----- GLMs -----
test_that("get_glm_sample_data works correctly", {
  # Test data generation for binomial family
  binomial_data <- get_glm_sample_data(
    family_string = "binomial",
    n = 20,
    mean = 0.3
  )
  expect_equal(length(binomial_data), 20)
  expect_true(all(binomial_data %in% c(0, 1)))

  # Test data generation for Gaussian family
  gaussian_data <- get_glm_sample_data(
    family_string = "gaussian",
    n = 20,
    mean = 5,
    sd = 2
  )
  expect_equal(length(gaussian_data), 20)
  expect_true(all(is.numeric(gaussian_data)))

  # Check for error with an invalid family
  expect_null(get_glm_sample_data(
    family_string = "invalid_family",
    n = 20,
    mean = 5,
    sd = 2
  ))
})

test_that("get_hmc_mcmc_result works correctly", {
  # Define a simple quadratic potential function and its gradient
  neg_log_den_func <- function(pos) {
    0.5 * sum(pos^2)
  }
  neg_log_den_grad_func <- function(pos) {
    pos
  }

  # Set initial position, iteration count, scale, and steps
  coefs_0 <- c(1, 1)
  iter <- 10
  hmc_scale <- 0.01
  hmc_steps <- 5

  # Run the function
  result <- get_hmc_mcmc_result(
    neg_log_den_func,
    neg_log_den_grad_func,
    coefs_0,
    iter = iter,
    hmc_scale = hmc_scale,
    hmc_steps = hmc_steps
  )

  # Check the output type and length
  expect_type(result, "double")
  expect_equal(length(result), length(coefs_0))

  result_2 <- get_hmc_mcmc_result(
    neg_log_den_func,
    neg_log_den_grad_func,
    coefs_0,
    iter = iter,
    hmc_scale = hmc_scale,
    hmc_steps = hmc_steps
  )

  # The results should not match exactly
  expect_false(all(result == result_2))
})

test_that("hmc_neal_2010 works correctly", {
  # Define a simple quadratic potential function and its gradient
  neg_log_den_func <- function(pos) {
    0.5 * sum(pos^2)
  }
  neg_log_den_grad_func <- function(pos) {
    pos
  }

  # Set parameters for the HMC function
  leapfrog_stepsize <- 0.1
  leapfrog_step <- 10
  current_pos <- c(1, 1)

  # Call the HMC function
  result <- hmc_neal_2010(
    neg_log_den_func, neg_log_den_grad_func,
    leapfrog_stepsize, leapfrog_step, current_pos
  )

  # Test that the output is a list
  expect_type(result, "list")

  # Test that the list has named elements position, potential_energy, and accepted
  expect_named(result, c("position", "potential_energy", "accepted"))

  # Test that position is a numeric vector of the same length as current_pos
  expect_type(result$position, "double")
  expect_equal(length(result$position), length(current_pos))

  # Test that potential_energy is a numeric scalar
  expect_type(result$potential_energy, "double")
  expect_length(result$potential_energy, 1)

  # Test that accepted is a logical value
  expect_type(result$accepted, "logical")
  expect_length(result$accepted, 1)

  # Run multiple iterations to see if the function accepts/rejects states correctly
  accept_count <- 0
  total_runs <- 100

  for (i in seq_len(total_runs)) {
    result <- hmc_neal_2010(neg_log_den_func, neg_log_den_grad_func,
      leapfrog_stepsize = 0.1, leapfrog_step = 10,
      current_pos = c(1, 1)
    )
    if (result$accepted) accept_count <- accept_count + 1
  }

  # The function should accept some of the proposals
  expect_gt(accept_count, 0)

  # The function should also reject some proposals in the set
  expect_lt(accept_count, total_runs)

  # Test for a small step size where energy should be conserved well
  leapfrog_stepsize <- 0.01
  leapfrog_step <- 20
  current_pos <- c(1, 1)
  result_small_stepsize <- hmc_neal_2010(
    neg_log_den_func, neg_log_den_grad_func,
    leapfrog_stepsize, leapfrog_step, current_pos
  )
})

test_that("get_glm_log_density_grad works correctly", {
  X <- matrix(rnorm(20), ncol = 5)
  Y_binomial <- c(1, 0, 1, 0)
  Y_gaussian <- rnorm(4)
  coefs <- rnorm(6)

  # Retrieve GLM models from glm_data
  model_binomial <- stats::glm(Y_binomial ~ X, binomial)
  model_gaussian <- stats::glm(Y_gaussian ~ X, gaussian)

  # Compute gradients
  grad_binomial <- get_glm_log_density_grad("binomial", X, Y_binomial, coefs)
  grad_gaussian <- get_glm_log_density_grad("gaussian", X, Y_gaussian, coefs)

  # Manually compute expected gradients for comparison
  mean_binomial <- get_glm_mean("binomial", X, coefs)
  grad_binomial_expected <- c(t(model_binomial$prior.weights * (Y_binomial - mean_binomial)) %*% cbind(1, X))

  mean_gaussian <- get_glm_mean("gaussian", X, coefs)
  grad_gaussian_expected <- c(t(model_gaussian$prior.weights * (Y_gaussian - mean_gaussian)) %*% cbind(1, X))

  # Test that computed values match expected values
  expect_equal(grad_binomial, grad_binomial_expected)
  expect_equal(grad_gaussian, grad_gaussian_expected)
})

test_that("get_glm_diag_approx_cov works correctly", {
  X <- matrix(rnorm(20), ncol = 5)
  y <- c(1, 0, 1, 0)

  # Fit a GLM model
  model <- stats::glm(y ~ X, family = binomial)

  # Compute the diagonal covariance matrix
  diag_cov <- get_glm_diag_approx_cov(X, model)

  # Manually compute the expected diagonal covariance for comparison
  hessian <- (t(cbind(1, X))) %*%
    diag(model$fitted.values * (1 - model$fitted.values) * model$prior.weights) %*%
    cbind(1, X)
  expected_diag_cov <- tryCatch(
    diag(solve(hessian)),
    error = function(.) {
      # Regularization for dealing with computationally singular issue
      diag(solve(hessian + diag(ncol(hessian)) * 1e-5))
    }
  )

  # Test that the computed and expected values match
  expect_equal(diag_cov, expected_diag_cov)

  # Check for correct error handling with mismatched dimensions (if applicable)
  expect_error(get_glm_diag_approx_cov(matrix(1:4, ncol = 2), model))
})

test_that("get_glm_custom_var works correctly", {
  # Call the function and check the result
  custom_var <- get_glm_custom_var(Y ~ ., cat_init_glm_gaussian, 1)

  # Expected custom variance calculation manually
  tau_model <- do.call(
    stats::glm,
    list(
      formula = Y ~ .,
      family = cat_init_glm_gaussian$family,
      data = cat_init_glm_gaussian$data,
      weights = c(
        rep(1, cat_init_glm_gaussian$obs_size),
        rep(1 / cat_init_glm_gaussian$syn_size, cat_init_glm_gaussian$syn_size)
      )
    )
  )
  expected_custom_var <- sum(stats::residuals(tau_model)^2) / stats::df.residual(tau_model) +
    sum(stats::residuals(tau_model)[1:cat_init_glm_gaussian$obs_size]^2) / cat_init_glm_gaussian$obs_size


  expect_equal(custom_var, expected_custom_var)
})

test_that("get_glm_lambda works correctly", {
  X <- matrix(rnorm(20), ncol = 5)
  coefs <- rnorm(6)

  # Expected outputs based on the given inputs
  expected_square_error <- 2 * get_glm_mean("gaussian", X, coefs)
  expected_classification_error <- 2 * get_glm_mean("binomial", X, coefs)
  expected_logistic_deviance <- get_linear_predictor(X, coefs)

  # Test for "square_error"
  expect_equal(get_glm_lambda("mean_square_error", X, coefs), expected_square_error)

  # Test for "classification_error"
  expect_equal(get_glm_lambda("mean_classification_error", X, coefs), expected_classification_error)

  # Test for "logistic_deviance"
  expect_equal(get_glm_lambda("logistic_deviance", X, coefs), expected_logistic_deviance)

  # Test that an error is thrown for invalid discrepancy method
  expect_error(get_glm_lambda("invalid_method", X, coefs))
})

test_that("get_glm_log_density works correctly", {
  X <- matrix(rnorm(20), ncol = 5)
  coefs <- rnorm(6)
  Y_binomial <- c(0, 1, 0, 1)
  Y_gaussian <- rnorm(4)
  weights <- 0.5

  # Test for binomial family
  log_density_binomial <- get_glm_log_density("binomial", X, Y_binomial, coefs, weights)
  lp_binomial <- get_linear_predictor(X, coefs)
  expected_log_density_binomial <- weights * sum((Y_binomial * lp_binomial - log(1 + exp(lp_binomial))))
  expect_equal(log_density_binomial, expected_log_density_binomial, tolerance = 1e-6)

  # Test for gaussian family
  log_density_gaussian <- get_glm_log_density("gaussian", X, Y_gaussian, coefs, weights)
  lp_gaussian <- get_linear_predictor(X, coefs)
  expected_log_density_gaussian <- weights * -0.5 * sum((log(2 * pi * max(mean((Y_gaussian - lp_gaussian)^2), 1e-5)) + 1))
  expect_equal(log_density_gaussian, expected_log_density_gaussian, tolerance = 1e-6)

  # Test for invalid family
  expect_null(get_glm_log_density("invalid_family", X, Y_binomial, coefs))
})

test_that("get_glm_mean works correctly", {
  X <- matrix(rnorm(20), ncol = 5)
  coefs <- rnorm(6)

  # Test for binomial family
  mean_binomial <- get_glm_mean("binomial", X, coefs)
  expected_binomial <- 1 / (1 + exp(-get_linear_predictor(X, coefs)))
  expect_equal(mean_binomial, expected_binomial, tolerance = 1e-6)

  # Test for gaussian family
  mean_gaussian <- get_glm_mean("gaussian", X, coefs)
  expected_gaussian <- get_linear_predictor(X, coefs)
  expect_equal(mean_gaussian, expected_gaussian, tolerance = 1e-6)

  # Test for invalid family
  expect_null(get_glm_mean("invalid_family", X, coefs))
})

test_that("get_glm_family_string works correctly", {
  # Test retrieving the family name
  gaussian_family_name <- get_glm_family_string("gaussian")
  expect_equal(gaussian_family_name, "gaussian")

  # Test retrieving the family name with the link function
  poisson_family_name_with_link <- get_glm_family_string("poisson", with_link = TRUE)
  expect_equal(poisson_family_name_with_link, "poisson [log]")

  # Test handling invalid family input
  expect_error(get_glm_family_string("invalid_family"), "'family' must be a valid GLM family.")
  expect_error(get_glm_family_string(function() "invalid"), "'family' must be a valid GLM family.")
})

test_that("validate_glm_input works correctly", {
  # Mock the menu function to simulate user input
  mock_menu <- function(...) {
    return(2) # Simulating user input "NO" to terminate
  }

  # Assign the mock_menu to replace the original menu function
  assign("menu", mock_menu, envir = globalenv())


  # Test for valid inputs
  expect_silent(
    validate_glm_input(
      formula = Y ~ X1 + X2,
      cat_init = cat_init_glm_binomial,
      tau = 0.1,
      tau_seq = seq(0.1, 1, by = 0.1),
      tau_0 = 0.1,
      parametric_bootstrap_iteration_times = 100,
      cross_validation_fold_num = 5,
      risk_estimate_method = "parametric_bootstrap",
      discrepancy_method = "logistic_deviance",
      binomial_tau_lower = 0.01,
      tau_alpha = 0.5,
      tau_gamma = 0.5,
      gibbs_iter = 1000,
      gibbs_warmup = 500,
      coefs_iter = 5,
      gaussian_variance_alpha = NULL,
      gaussian_variance_beta = NULL
    )
  )

  expect_silent(validate_glm_input(
    formula = Y ~ X1 + X2,
    cat_init = cat_init_glm_gaussian,
    tau = 0.1,
    tau_seq = seq(0.1, 1, by = 0.1),
    tau_0 = 0.1,
    parametric_bootstrap_iteration_times = 100,
    cross_validation_fold_num = 5,
    risk_estimate_method = "parametric_bootstrap",
    discrepancy_method = "mean_square_error",
    binomial_tau_lower = 0.01,
    tau_alpha = 0.5,
    tau_gamma = 0.5
  ))

  # Test for invalid formula with wrong response variance
  expect_error(validate_glm_input(
    formula = Y_wrong ~ .,
    cat_init = cat_init_glm_gaussian
  ))

  # Test for invalid cat_init object
  expect_error(validate_glm_input(
    formula = Y ~ .,
    cat_init = cat_init_cox
  ))

  # Test for invalid tau (negative value)
  expect_error(validate_glm_input(
    formula = Y ~ .,
    cat_init = cat_init_glm_gaussian,
    tau = -0.1
  ))

  # Test for invalid tau (vectors)
  expect_error(validate_glm_input(
    formula = Y ~ .,
    cat_init = cat_init_glm_gaussian,
    tau = c(1, 2)
  ))

  # Test for invalid tau_seq (negative value)
  expect_error(validate_glm_input(
    formula = Y ~ .,
    cat_init = cat_init_glm_gaussian,
    tau_seq = c(-1, 0, 1)
  ))

  # Test for invalid tau_seq (single value)
  expect_error(validate_glm_input(
    formula = Y ~ .,
    cat_init = cat_init_glm_gaussian,
    tau_seq = 1
  ))

  # Test for invalid tau_0 (zero value)
  expect_error(validate_glm_input(
    formula = Y ~ .,
    cat_init = cat_init_glm_gaussian,
    tau_0 = 0
  ))

  # Test for invalid tau_0 (vector)
  expect_error(validate_glm_input(
    formula = Y ~ .,
    cat_init = cat_init_glm_gaussian,
    tau_0 = c(1, 2)
  ))

  # Test for invalid parametric_bootstrap_iteration_times (zero value)
  expect_error(validate_glm_input(
    formula = Y ~ .,
    cat_init = cat_init_glm_gaussian,
    parametric_bootstrap_iteration_times = 0
  ))

  # Test for invalid cross_validation_fold_num (zero value)
  expect_error(validate_glm_input(
    formula = Y ~ .,
    cat_init = cat_init_glm_gaussian,
    cross_validation_fold_num = 0
  ))

  # Test for invalid cross_validation_fold_num (greater than number of rows)
  expect_error(validate_glm_input(
    formula = Y ~ .,
    cat_init = cat_init_glm_gaussian,
    risk_estimate_method = "cross_validation",
    discrepancy_method = "mean_square_error",
    cross_validation_fold_num = 100
  ))

  # Test for family == "gaussian" & risk_estimate_method == "steinian_estimate"
  expect_error(validate_glm_input(
    formula = Y ~ .,
    cat_init = cat_init_glm_gaussian,
    risk_estimate_method = "steinian_estimate",
    discrepancy_method = "mean_square_error"
  ))

  # Test for family == "binomial" & risk_estimate_method == "mallowian_estimate"
  expect_error(validate_glm_input(
    formula = Y ~ .,
    cat_init = cat_init_glm_binomial,
    risk_estimate_method = "mallowian_estimate",
    discrepancy_method = "logistic_deviance"
  ))

  # Test for family == "gaussian" & discrepancy_method != "mean_square_error"
  expect_error(validate_glm_input(
    formula = Y ~ .,
    cat_init = cat_init_glm_gaussian,
    risk_estimate_method = "mallowian_estimate",
    discrepancy_method = "logistic_deviance"
  ))

  cat_init_glm_gaussian_large_size <- cat_init_glm_gaussian
  cat_init_glm_gaussian_large_size$obs_size <- 2000
  # Test for large data size when using parametric_bootstrap
  expect_error(validate_glm_input(
    formula = Y ~ .,
    cat_init = cat_init_glm_gaussian_large_size,
    risk_estimate_method = "parametric_bootstrap",
    discrepancy_method = "mean_square_error"
  ))

  cat_init_glm_binomial_large_size <- cat_init_glm_binomial
  cat_init_glm_binomial_large_size$obs_size <- 2000
  # Test for large data size when using steinian_estimate
  expect_error(validate_glm_input(
    formula = Y ~ .,
    cat_init = cat_init_glm_binomial_large_size,
    risk_estimate_method = "steinian_estimate",
    discrepancy_method = "logistic_deviance"
  ))

  # Warn user when family == "binomial" & risk_estimate_method == "steinian_estimate" & discrepancy_method != "logistic_deviance"
  expect_warning(validate_glm_input(
    formula = Y ~ .,
    cat_init = cat_init_glm_binomial,
    risk_estimate_method = "steinian_estimate",
    discrepancy_method = "mean_classification_error"
  ))

  # Test for family == "binomial" & discrepancy_method == "mean_square_error"
  expect_error(validate_glm_input(
    formula = Y ~ .,
    cat_init = cat_init_glm_binomial,
    risk_estimate_method = "steinian_estimate",
    discrepancy_method = "mean_square_error"
  ))

  # Test for binomial using gibbs function
  expect_error(validate_glm_input(
    formula = Y ~ .,
    cat_init = cat_init_glm_gaussian,
    gibbs_iter = 1000,
    gibbs_warmup = 200,
    coefs_iter = 5
  ))

  # Test for gibbs_warmup > gibbs_iter
  expect_error(validate_glm_input(
    formula = Y ~ .,
    cat_init = cat_init_glm_binomial,
    gibbs_iter = 100,
    gibbs_warmup = 200,
    coefs_iter = 5
  ))

  # Test for zero gibbs_iter
  expect_error(validate_glm_input(
    formula = Y ~ .,
    cat_init = cat_init_glm_binomial,
    gibbs_iter = 0,
    gibbs_warmup = 200,
    coefs_iter = 5
  ))

  # Test for zero gibbs_warmup
  expect_error(validate_glm_input(
    formula = Y ~ .,
    cat_init = cat_init_glm_binomial,
    gibbs_iter = 100,
    gibbs_warmup = 0,
    coefs_iter = 5
  ))

  # Test for zero coefs_iter
  expect_error(validate_glm_input(
    formula = Y ~ .,
    cat_init = cat_init_glm_binomial,
    gibbs_iter = 1000,
    gibbs_warmup = 200,
    coefs_iter = 0
  ))

  # Test for large coefs_iter
  expect_error(validate_glm_input(
    formula = Y ~ .,
    cat_init = cat_init_glm_binomial,
    gibbs_iter = 1000,
    gibbs_warmup = 200,
    coefs_iter = 50
  ))

  # Test for zero gaussian_variance_alpha
  expect_error(validate_glm_input(
    formula = Y ~ .,
    cat_init = cat_init_glm_gaussian,
    gaussian_variance_alpha = 0
  ))

  # Test for zero gaussian_variance_beta
  expect_error(validate_glm_input(
    formula = Y ~ .,
    cat_init = cat_init_glm_gaussian,
    gaussian_variance_beta = 0
  ))

  # Test for ignore gaussian_variance_alpha when gaussian_known_variance = TRUE
  cat_init_glm_gaussian_known <- mock_cat_glm_gaussian_initialization(glm_gaussian_data, gaussian_known_variance = TRUE)
  expect_warning(validate_glm_input(
    formula = Y ~ .,
    cat_init = cat_init_glm_gaussian_known,
    gaussian_variance_alpha = 1
  ))

  # Test for ignore gaussian_variance_alpha when family is binomial
  expect_warning(validate_glm_input(
    formula = Y ~ .,
    cat_init = cat_init_glm_binomial,
    gaussian_variance_alpha = 1
  ))

  # Test for ignore gaussian_variance_beta when gaussian_known_variance = TRUE
  expect_warning(validate_glm_input(
    formula = Y ~ .,
    cat_init = cat_init_glm_gaussian_known,
    gaussian_variance_beta = 1
  ))

  # Test for ignore gaussian_variance_beta when family is binomial
  expect_warning(validate_glm_input(
    formula = Y ~ .,
    cat_init = cat_init_glm_binomial,
    gaussian_variance_beta = 1
  ))

  # Test for binomial features when family is gaussian
  expect_warning(validate_glm_input(
    formula = Y ~ .,
    cat_init = cat_init_glm_gaussian,
    binomial_joint_theta = TRUE
  ))

  # Test for binomial features when family is gaussian
  expect_warning(validate_glm_input(
    formula = Y ~ .,
    cat_init = cat_init_glm_gaussian,
    binomial_joint_alpha = TRUE
  ))

  # Test for binomial features when theta is not activated with alpha
  expect_warning(validate_glm_input(
    formula = Y ~ .,
    cat_init = cat_init_glm_binomial,
    binomial_joint_theta = FALSE,
    binomial_joint_alpha = TRUE
  ))
})

test_that("validate_glm_initialization_input checks parameters correctly", {
  # Test for valid inputs
  expect_silent(validate_glm_initialization_input(
    formula = Y ~ 1,
    family = gaussian,
    data = glm_gaussian_data,
    syn_size = NULL,
    custom_variance = NULL,
    gaussian_known_variance = FALSE,
    x_degree = NULL
  ))


  # Test for complex formula
  expect_error(validate_glm_initialization_input(
    formula = Y ~ X1 + X2 + X3,
    family = gaussian,
    data = glm_gaussian_data,
    syn_size = NULL,
    custom_variance = NULL,
    gaussian_known_variance = FALSE,
    x_degree = NULL
  ))

  # Test for wrong data format
  expect_error(validate_glm_initialization_input(
    formula = Y ~ 1,
    family = gaussian,
    data = as.matrix(glm_gaussian_data),
    syn_size = NULL,
    custom_variance = NULL,
    gaussian_known_variance = FALSE,
    x_degree = NULL
  ))

  # Test for empty data
  expect_error(validate_glm_initialization_input(
    formula = Y ~ 1,
    family = gaussian,
    data = data.frame(),
    syn_size = NULL,
    custom_variance = NULL,
    gaussian_known_variance = FALSE,
    x_degree = NULL
  ))

  # Test for lmm formula
  expect_error(validate_glm_initialization_input(
    formula = Y ~ 1 + (1 | X1),
    family = gaussian,
    data = glm_gaussian_data,
    syn_size = NULL,
    custom_variance = NULL,
    gaussian_known_variance = FALSE,
    x_degree = NULL
  ))

  # Test for cox formula
  expect_error(validate_glm_initialization_input(
    formula = survival::Surv(Y, X1) ~ 1,
    family = gaussian,
    data = glm_gaussian_data,
    syn_size = NULL,
    custom_variance = NULL,
    gaussian_known_variance = FALSE,
    x_degree = NULL
  ))

  # Test for zero synthetic data size
  expect_error(validate_glm_initialization_input(
    formula = Y ~ 1,
    family = gaussian,
    data = glm_gaussian_data,
    syn_size = 0,
    custom_variance = NULL,
    gaussian_known_variance = FALSE,
    x_degree = NULL
  ))

  # Test for positive synthetic data size
  expect_silent(validate_glm_initialization_input(
    formula = Y ~ 1,
    family = gaussian,
    data = glm_gaussian_data,
    syn_size = 100,
    custom_variance = NULL,
    gaussian_known_variance = FALSE,
    x_degree = NULL
  ))

  # Test for custom_variance equal to zero
  expect_error(validate_glm_initialization_input(
    formula = Y ~ 1,
    family = gaussian,
    data = glm_gaussian_data,
    syn_size = NULL,
    custom_variance = 0,
    gaussian_known_variance = TRUE,
    x_degree = NULL
  ))

  # Test for positive custom_variance
  expect_silent(validate_glm_initialization_input(
    formula = Y ~ 1,
    family = gaussian,
    data = glm_gaussian_data,
    syn_size = NULL,
    custom_variance = 1,
    gaussian_known_variance = TRUE,
    x_degree = NULL
  ))

  # Test for positive custom_variance but FALSE gaussian_known_variance
  expect_warning(validate_glm_initialization_input(
    formula = Y ~ 1,
    family = gaussian,
    data = glm_gaussian_data,
    syn_size = NULL,
    custom_variance = 1,
    gaussian_known_variance = FALSE,
    x_degree = NULL
  ))

  # Test for positive custom_variance, TRUE gaussian_known_variance but binomial distribution
  expect_warning(validate_glm_initialization_input(
    formula = Y ~ 1,
    family = binomial,
    data = glm_binomial_data,
    syn_size = NULL,
    custom_variance = 1,
    gaussian_known_variance = TRUE,
    x_degree = NULL
  ))

  # Test for x_degree being zero
  expect_error(validate_glm_initialization_input(
    formula = Y ~ 1,
    family = gaussian,
    data = glm_gaussian_data,
    syn_size = NULL,
    custom_variance = 1,
    gaussian_known_variance = TRUE,
    x_degree = rep(0, 3)
  ))

  # Test for x_degree being positive
  expect_silent(validate_glm_initialization_input(
    formula = Y ~ 1,
    family = gaussian,
    data = glm_gaussian_data,
    syn_size = NULL,
    custom_variance = 1,
    gaussian_known_variance = TRUE,
    x_degree = rep(1, 3)
  ))

  # Test for x_degree being a single value
  expect_error(validate_glm_initialization_input(
    formula = Y ~ 1,
    family = gaussian,
    data = glm_gaussian_data,
    syn_size = NULL,
    custom_variance = 1,
    gaussian_known_variance = TRUE,
    x_degree = 1
  ))
})

# ----- COX -----
test_that("get_cox_risk_and_failure_sets works correctly", {
  time_vector <- cox_data$time
  status_vector <- cox_data$status
  indicator_vector <- sort(unique(c(time_vector, time_vector + rexp(length(time_vector)))))

  # Calculate risk and failure sets
  sets <- get_cox_risk_and_failure_sets(time_vector, status_vector, indicator_vector)

  # Test that the result is a list with two elements
  expect_true(is.list(sets))
  expect_equal(length(sets), 2)

  # Test that the risk_set and failure_set are matrices
  expect_true(is.matrix(sets$risk_set))
  expect_true(is.matrix(sets$failure_set))

  # Test that the dimensions of risk_set and failure_set match the input
  expect_equal(dim(sets$risk_set), c(length(time_vector), length(indicator_vector)))
  expect_equal(dim(sets$failure_set), c(length(time_vector), length(indicator_vector)))

  # Test that failure_set has only one 1 per row where status = 1
  expect_true(all(rowSums(sets$failure_set[status_vector == 1, ]) == 1))

  # Test that failure_set is all zeros where status = 0
  expect_true(all(sets$failure_set[status_vector == 0, ] == 0))

  # Test that risk_set is all zeros or ones
  expect_true(all(sets$risk_set %in% c(0, 1)))
})

test_that("get_cox_kappa works correctly", {
  kappa <- get_cox_kappa(
    X = matrix(rnorm(100), ncol = 5),
    time = rexp(20),
    status = rbinom(20, 1, 0.5),
    hazard_constant = 0.02
  )

  # Test that kappa is a numeric value
  expect_true(is.numeric(kappa))

  # Test that kappa is a scalar
  expect_equal(length(kappa), 1)
})

test_that("get_cox_partial_likelihood works correctly", {
  partial_likelihood <- get_cox_partial_likelihood(
    X = matrix(rnorm(100), ncol = 5),
    time = rexp(20),
    status = rbinom(20, 1, 0.5),
    coefs = rnorm(5),
    entry_points = rep(0, 20)
  )

  # Test that the partial likelihood is a numeric scalar
  expect_true(is.numeric(partial_likelihood))
  expect_length(partial_likelihood, 1)
})

test_that("get_cox_qr_solve works correctly", {
  # Define valid input
  hessian_matrix <- matrix(c(2, 1, 1, 2), ncol = 2)
  gradient_vector <- c(5, 6)

  # Test that the computed solution matches the expected solution
  expect_equal(
    get_cox_qr_solve(
      hessian_matrix = hessian_matrix,
      gradient_vector = gradient_vector
    ),
    solve(hessian_matrix, gradient_vector)
  )

  # Test with matrix that might produce NA values
  expect_equal(
    get_cox_qr_solve(
      hessian_matrix = matrix(c(1, 1, 1, 1), ncol = 2),
      gradient_vector = c(1, 1)
    ),
    c(0.0001, 0.0001)
  )

  # Test with another case that produces errors
  expect_equal(
    get_cox_qr_solve(
      hessian_matrix = matrix(c(0, 0, 0, 0), ncol = 2),
      gradient_vector = c(1, 1)
    ),
    c(0.0001, 0.0001)
  )
})

test_that("get_cox_gradient works correctly", {
  X <- matrix(rnorm(15), ncol = 3)
  time <- rexp(5, rate = 0.1)
  status <- rbinom(5, 1, 0.5)
  coefs <- rnorm(3)
  entry_points <- rep(0, 5)

  gradient <- get_cox_gradient(
    X = X,
    time = time,
    status = status,
    coefs = coefs,
    entry_points = entry_points
  )

  expect_true(is.numeric(gradient))
  expect_equal(length(gradient), ncol(X))

  # Test with no events
  expect_equal(get_cox_gradient(
    X = X,
    time = time,
    status = rep(0, length(status)),
    coefs = coefs,
    entry_points = entry_points
  ), c(0, 0, 0))
})

test_that("get_cox_syn_gradient works correctly", {
  X <- matrix(rnorm(15), ncol = 3)
  time <- rexp(5, rate = 0.1)
  coefs <- rnorm(3)

  gradient <- get_cox_syn_gradient(
    X = X,
    time = time,
    coefs = coefs,
    hazard_constant = 0.5
  )

  # Test that the gradient is a numeric vector
  expect_true(is.numeric(gradient))

  # Test that the length of the gradient matches the number of columns in X
  expect_equal(length(gradient), ncol(X))
})

test_that("get_cox_risk_set_idx works correctly", {
  time_of_interest <- 5
  entry_vector <- c(0, 0, 0, 0, 0)
  time_vector <- c(3, 5, 8, 9, 10)
  status_vector <- c(1, 1, 0, 1, 0)

  risk_set_idx <- get_cox_risk_set_idx(
    time_of_interest = time_of_interest,
    entry_vector = entry_vector,
    time_vector = time_vector,
    status_vector = status_vector
  )

  # Test that the output is a vector
  expect_true(is.vector(risk_set_idx))

  # Test that all indices are within the range of the time_vector length
  expect_true(all(risk_set_idx >= 1 & risk_set_idx <= length(time_vector)))

  # Test that all subjects in the risk set entered before or at the time of interest
  expect_true(all(time_of_interest >= entry_vector[risk_set_idx]))

  # Test with a time_of_interest that is not in time_vector
  risk_set_idx_new <- get_cox_risk_set_idx(
    time_of_interest = max(time_vector) + 1,
    entry_vector = entry_vector,
    time_vector = time_vector,
    status_vector = status_vector
  )

  expect_true(length(risk_set_idx_new) == 0)
})

test_that("get_cox_hessian works correctly", {
  X <- matrix(rnorm(15), ncol = 3)
  time <- rexp(5, rate = 0.1)
  status <- rbinom(5, 1, 0.5)
  coefs <- rnorm(3)
  entry_points <- rep(0, 5)

  hessian_matrix <- get_cox_hessian(
    X = X,
    time = time,
    status = status,
    coefs = coefs,
    entry_points = entry_points
  )

  # Test if the computed Hessian is a matrix
  expect_true(is.matrix(hessian_matrix))

  # Test the dimensions of the Hessian matrix
  expect_equal(dim(hessian_matrix), c(ncol(X), ncol(X)))

  # Test that the Hessian is symmetric (should be if correctly calculated)
  expect_true(all.equal(hessian_matrix, t(hessian_matrix), tolerance = 1e-8))

  # Test with no events
  hessian_matrix_no_event <- get_cox_hessian(
    X = X,
    time = time,
    status = c(0, 0, 0, 0, 0),
    coefs = coefs,
    entry_points = entry_points
  )
  expect_equal(hessian_matrix_no_event, matrix(0, ncol = ncol(X), nrow = ncol(X)))
})

test_that("get_cox_syn_hessian works correctly", {
  X <- matrix(rnorm(15), ncol = 3)
  time <- rexp(5, rate = 0.1)
  coefs <- rnorm(3)
  hazard_constant <- 0.7

  hessian_matrix <- get_cox_syn_hessian(
    X = X,
    time = time,
    coefs = coefs,
    hazard_constant = hazard_constant
  )

  # Test if the computed Hessian is a matrix
  expect_true(is.matrix(hessian_matrix))

  # Test the dimensions of the Hessian matrix
  expect_equal(dim(hessian_matrix), c(ncol(X), ncol(X)))

  # Test that the Hessian is symmetric (should be if correctly calculated)
  expect_true(all.equal(hessian_matrix, t(hessian_matrix), tolerance = 1e-8))

  # Test with no variation in time (all times the same)
  hessian_matrix_no_var <- get_cox_syn_hessian(
    X = X,
    time = rep(1, nrow(X)),
    coefs = coefs,
    hazard_constant = hazard_constant
  )

  # Test that the Hessian is still a valid numeric matrix
  expect_true(is.matrix(hessian_matrix_no_var))
  expect_true(is.numeric(hessian_matrix_no_var))
  expect_equal(dim(hessian_matrix_no_var), c(ncol(X), ncol(X)))
})

test_that("validate_cox_input checks parameters correctly", {
  # Test for valid inputs
  cat_init <- get_adjusted_cat_init(
    cat_init = cat_init_cox,
    formula_rhs = ~ X1 + X2
  )

  expect_silent(validate_cox_input(
    formula = ~ X1 + X2,
    cat_init = cat_init,
    tau = 1,
    tau_seq = c(1, 2),
    init_coefficients = rep(0, ncol(cat_init$adj_x)),
    tol = 1e-3,
    max_iter = 30,
    cross_validation_fold_num = 2,
    hazard_beta = 2,
    tau_alpha = 2,
    tau_gamma = 1
  ))

  # Test for cat_init not generated from `cat_cox_initialization`
  cat_init_invalid <- cat_init_lmm
  cat_init_invalid$function_name <- "wrong_initialization"
  expect_error(validate_cox_input(
    formula = ~ X1 + X2,
    cat_init = cat_init_invalid
  ))

  # Test for fomrula contain wrong time/status column name
  expect_error(validate_cox_input(
    formula = survival::Surv(time_wrong, status_wrong) ~ X1 + X2,
    cat_init = cat_init
  ))

  # Test for tau being negative
  expect_error(validate_cox_input(
    formula = ~ X1 + X2,
    cat_init = cat_init,
    tau = -1
  ))

  # Test for tau being a vector
  expect_error(validate_cox_input(
    formula = ~ X1 + X2,
    cat_init = cat_init,
    tau = c(1, 2)
  ))

  # Test for tau_seq being negative
  expect_error(validate_cox_input(
    formula = ~ X1 + X2,
    cat_init = cat_init,
    tau_seq = c(-1, 1)
  ))

  # Test for tau_seq being a single value
  expect_error(validate_cox_input(
    formula = ~ X1 + X2,
    cat_init = cat_init,
    tau_seq = 1
  ))

  # Test for init_coefficients being different length
  expect_error(validate_cox_input(
    formula = ~ X1 + X2,
    cat_init = cat_init,
    init_coefficients = rep(0, ncol(cat_init$adj_x) + 1)
  ))

  # Test for tol being 0
  expect_error(validate_cox_input(
    formula = ~ X1 + X2,
    cat_init = cat_init,
    tol = 0
  ))

  # Test for max_iter being 0
  expect_error(validate_cox_input(
    formula = ~ X1 + X2,
    cat_init = cat_init,
    max_iter = 0
  ))

  # Test for cross_validation_fold_num being 0
  expect_error(validate_cox_input(
    formula = ~ X1 + X2,
    cat_init = cat_init,
    cross_validation_fold_num = 0
  ))

  # Test for cross_validation_fold_num being large
  expect_warning(validate_cox_input(
    formula = ~ X1 + X2,
    cat_init = cat_init,
    cross_validation_fold_num = 3
  ))

  # Test for cross_validation_fold_num being too large
  expect_error(validate_cox_input(
    formula = ~ X1 + X2,
    cat_init = cat_init,
    cross_validation_fold_num = 100
  ))

  # Test for hazard_beta being 0
  expect_error(validate_cox_input(
    formula = ~ X1 + X2,
    cat_init = cat_init,
    hazard_beta = 0
  ))

  # Test for tau_alpha being 0
  expect_error(validate_cox_input(
    formula = ~ X1 + X2,
    cat_init = cat_init,
    tau_alpha = 0
  ))

  # Test for tau_gamma being 0
  expect_error(validate_cox_input(
    formula = ~ X1 + X2,
    cat_init = cat_init,
    tau_gamma = 0
  ))
})

test_that("validate_cox_initialization_input checks parameters correctly", {
  # Test for valid inputs
  expect_silent(validate_cox_initialization_input(
    formula = survival::Surv(time, status) ~ 1,
    data = cox_data,
    syn_size = NULL,
    hazard_constant = NULL,
    entry_points = NULL,
    x_degree = NULL
  ))


  # Test for wrong data format
  expect_error(validate_cox_initialization_input(
    formula = survival::Surv(time, status) ~ 1,
    data = as.matrix(cox_data),
    syn_size = NULL,
    hazard_constant = NULL,
    entry_points = NULL,
    x_degree = NULL
  ))

  # Test for complex formula
  expect_error(validate_cox_initialization_input(
    formula = survival::Surv(time, status) ~ X1 + X2 + X3,
    data = cox_data,
    syn_size = NULL,
    hazard_constant = NULL,
    entry_points = NULL,
    x_degree = NULL
  ))

  # Test for empty data
  expect_error(validate_cox_initialization_input(
    formula = survival::Surv(time, status) ~ 1,
    data = data.frame(),
    syn_size = NULL,
    hazard_constant = NULL,
    entry_points = NULL,
    x_degree = NULL
  ))

  # Test for lmm formula
  expect_error(validate_cox_initialization_input(
    formula = time ~ 1 + (1 | status),
    data = cox_data,
    syn_size = NULL,
    hazard_constant = NULL,
    entry_points = NULL,
    x_degree = NULL
  ))

  # Test for glm formula
  expect_error(validate_cox_initialization_input(
    formula = time ~ 1,
    data = cox_data,
    syn_size = NULL,
    hazard_constant = NULL,
    entry_points = NULL,
    x_degree = NULL
  ))

  # Test for zero synthetic data size
  expect_error(validate_cox_initialization_input(
    formula = survival::Surv(time, status) ~ 1,
    data = cox_data,
    syn_size = 0,
    hazard_constant = NULL,
    entry_points = NULL,
    x_degree = NULL
  ))

  # Test for positive synthetic data size
  expect_silent(validate_cox_initialization_input(
    formula = survival::Surv(time, status) ~ 1,
    data = cox_data,
    syn_size = 100,
    hazard_constant = NULL,
    entry_points = NULL,
    x_degree = NULL
  ))

  # Test for hazard constant equal to zero
  expect_error(validate_cox_initialization_input(
    formula = survival::Surv(time, status) ~ 1,
    data = cox_data,
    syn_size = NULL,
    hazard_constant = 0,
    entry_points = NULL,
    x_degree = NULL
  ))

  # Test for positive hazard constant
  expect_silent(validate_cox_initialization_input(
    formula = survival::Surv(time, status) ~ 1,
    data = cox_data,
    syn_size = NULL,
    hazard_constant = 5,
    entry_points = NULL,
    x_degree = NULL
  ))

  # Test for not aligned entry point
  expect_error(validate_cox_initialization_input(
    formula = survival::Surv(time, status) ~ 1,
    data = cox_data,
    syn_size = NULL,
    hazard_constant = NULL,
    entry_points = c(0, 0, 0, 0, 0),
    x_degree = NULL
  ))

  # Test for right length of entry point
  expect_silent(validate_cox_initialization_input(
    formula = survival::Surv(time, status) ~ 1,
    data = cox_data,
    syn_size = NULL,
    hazard_constant = NULL,
    entry_points = rep(0, nrow(cox_data)),
    x_degree = NULL
  ))

  # Test for negative entry point
  expect_error(validate_cox_initialization_input(
    formula = survival::Surv(time, status) ~ 1,
    data = cox_data,
    syn_size = NULL,
    hazard_constant = NULL,
    entry_points = rep(-1, nrow(cox_data)),
    x_degree = NULL
  ))

  # Test for x_degree being zero
  expect_error(validate_cox_initialization_input(
    formula = survival::Surv(time, status) ~ 1,
    data = cox_data,
    syn_size = NULL,
    hazard_constant = NULL,
    entry_points = NULL,
    x_degree = rep(0, 3)
  ))

  # Test for x_degree being positive
  expect_silent(validate_cox_initialization_input(
    formula = survival::Surv(time, status) ~ 1,
    data = cox_data,
    syn_size = NULL,
    hazard_constant = NULL,
    entry_points = NULL,
    x_degree = rep(1, 3)
  ))

  # Test for x_degree being a single value
  expect_error(validate_cox_initialization_input(
    formula = survival::Surv(time, status) ~ 1,
    data = cox_data,
    syn_size = NULL,
    hazard_constant = NULL,
    entry_points = NULL,
    x_degree = 1
  ))
})

# ----- LMM -----
test_that("update_lmm_variance produces expected results for edge cases", {
  # Edge case with very small variances
  result_small_variance <- update_lmm_variance(
    residual_variance = 1e-10,
    random_effect_variance = 1e-10,
    obs_z_eigenvalues = c(1, 2, 3),
    syn_z_eigenvalues = c(1, 2, 3),
    obs_adjusted_residuals = c(1, 2, 3),
    syn_adjusted_residuals = c(1, 2, 3),
    tau = 1
  )
  expect_true(is.finite(result_small_variance))

  # Check monotonicity with varying tau
  result_low_tau <- update_lmm_variance(1, 1, c(1, 1.5, 2), c(1, 1.5, 2), c(1, 2, 3), c(1, 2, 3), 1)
  result_high_tau <- update_lmm_variance(1, 1, c(1, 1.5, 2), c(1, 1.5, 2), c(1, 2, 3), c(1, 2, 3), 10)
  expect_true(result_low_tau < result_high_tau) # Higher tau should increase likelihood

  # Symmetry check with swapped eigenvalues with tau equal to length of the eigenvalues
  result_obs_syn_swapped <- update_lmm_variance(0.5, 1.2, c(1, 2, 2.5), c(1, 2, 3), c(1, 2, 3), c(1, 2, 3), 3)
  result_syn_obs_swapped <- update_lmm_variance(0.5, 1.2, c(1, 2, 3), c(1, 2, 2.5), c(1, 2, 3), c(1, 2, 3), 3)
  expect_equal(result_obs_syn_swapped, result_syn_obs_swapped)
})

test_that("validate_lmm_input checks parameters correctly", {
  # Test for valid inputs
  expect_silent(validate_lmm_input(
    cat_init = cat_init_lmm,
    tau = 1,
    residual_variance_0 = 1,
    random_effect_variance_0 = 1,
    coefs_0 = rep(1, ncol(cat_init_lmm$obs_x)),
    optimize_domain = c(0, 10),
    max_iter = 100,
    tol = 0.001,
    tau_seq = c(0, 1),
    cross_validation_fold_num = 5
  ))

  # Test for cat_init not generated from `cat_lmm_initialization`
  cat_init_invalid <- cat_init_lmm
  cat_init_invalid$function_name <- "wrong_initialization"
  expect_error(validate_lmm_input(
    cat_init = cat_init_invalid
  ))

  # Test for tau being negative
  expect_error(validate_lmm_input(
    cat_init = cat_init_lmm,
    tau = -1
  ))

  # Test for tau being a vector
  expect_error(validate_lmm_input(
    cat_init = cat_init_lmm,
    tau = c(1, 2)
  ))

  # Test for residual_variance_0 being zero
  expect_error(validate_lmm_input(
    cat_init = cat_init_lmm,
    residual_variance_0 = 0
  ))

  # Test for random_effect_variance_0 being zero
  expect_error(validate_lmm_input(
    cat_init = cat_init_lmm,
    random_effect_variance_0 = 0
  ))

  # Test for coefs_0 being too short for observation x
  expect_error(validate_lmm_input(
    cat_init = cat_init_lmm,
    coefs_0 = c(1)
  ))

  # Test for coefs_0 being too long for observation x
  expect_error(validate_lmm_input(
    cat_init = cat_init_lmm,
    coefs_0 = rep(1, 5)
  ))

  # Test for optimize_domain being negative
  expect_error(validate_lmm_input(
    cat_init = cat_init_lmm,
    optimize_domain = c(-1, 0)
  ))

  # Test for optimize_domain being a single value
  expect_error(validate_lmm_input(
    cat_init = cat_init_lmm,
    optimize_domain = 1
  ))

  # Test for optimize_domain being too long
  expect_error(validate_lmm_input(
    cat_init = cat_init_lmm,
    optimize_domain = c(1, 2, 3)
  ))

  # Test for max_iter being too zero
  expect_error(validate_lmm_input(
    cat_init = cat_init_lmm,
    max_iter = 0
  ))

  # Test for max_iter being a vector
  expect_error(validate_lmm_input(
    cat_init = cat_init_lmm,
    max_iter = c(1, 2)
  ))

  # Test for tol being zero
  expect_error(validate_lmm_input(
    cat_init = cat_init_lmm,
    tol = 0
  ))

  # Test for tol being a vector
  expect_error(validate_lmm_input(
    cat_init = cat_init_lmm,
    tol = c(1, 2)
  ))


  # Test for tau_seq being negative
  expect_error(validate_lmm_input(
    cat_init = cat_init_lmm,
    tau_seq = c(-1, 1)
  ))

  # Test for tau_seq being a single value
  expect_error(validate_lmm_input(
    cat_init = cat_init_lmm,
    tau_seq = 1
  ))

  # Test for cross_validation_fold_num being less than 2
  expect_error(validate_lmm_input(
    cat_init = cat_init_lmm,
    cross_validation_fold_num = 1
  ))

  # Test for cross_validation_fold_num being larger than observation data size
  expect_error(validate_lmm_input(
    cat_init = cat_init_lmm,
    cross_validation_fold_num = 100
  ))

  # Test for cross_validation_fold_num being a vector
  expect_error(validate_lmm_input(
    cat_init = cat_init_lmm,
    cross_validation_fold_num = c(1, 2)
  ))
})

test_that("validate_lmm_initialization_input checks parameters correctly", {
  # Test for valid inputs
  expect_silent(validate_lmm_initialization_input(
    formula = Y ~ X1,
    data = lmm_data,
    x_cols = c("X1", "X2", "X3"),
    y_col = "Y",
    z_cols = c("Z1", "Z2", "Z3"),
    group_col = "group",
    syn_size = NULL
  ))

  # Test for wrong data format
  expect_error(validate_lmm_initialization_input(
    formula = Y ~ X1,
    data = as.matrix(lmm_data),
    x_cols = c("X1", "X2", "X3"),
    y_col = "Y",
    z_cols = c("Z1", "Z2", "Z3"),
    group_col = "group",
    syn_size = NULL
  ))

  # Test for empty data
  expect_error(validate_lmm_initialization_input(
    formula = Y ~ X1,
    data = data.frame(),
    x_cols = c("X1", "X2", "X3"),
    y_col = "Y",
    z_cols = c("Z1", "Z2", "Z3"),
    group_col = "group",
    syn_size = NULL
  ))

  # Test for lmm formula
  expect_silent(validate_lmm_initialization_input(
    formula = Y ~ X1 + (1 | group),
    data = lmm_data,
    x_cols = c("X1", "X2", "X3"),
    y_col = "Y",
    z_cols = c("Z1", "Z2", "Z3"),
    group_col = "group",
    syn_size = NULL
  ))

  # Test for surv formula
  expect_error(validate_lmm_initialization_input(
    formula = survival::Surv(time, status) ~ 1,
    data = lmm_data,
    x_cols = c("X1", "X2", "X3"),
    y_col = "Y",
    z_cols = c("Z1", "Z2", "Z3"),
    group_col = "group",
    syn_size = NULL
  ))

  # Test for lack of x_cols
  expect_error(validate_lmm_initialization_input(
    formula = Y ~ X1,
    data = lmm_data,
    x_cols = c("X1", "X2"),
    y_col = "Y",
    z_cols = c("Z1", "Z2", "Z3"),
    group_col = "group",
    syn_size = NULL
  ))

  # Test for lack of z_cols
  expect_error(validate_lmm_initialization_input(
    formula = Y ~ X1,
    data = lmm_data,
    x_cols = c("X1", "X2", "X3"),
    y_col = "Y",
    z_cols = c("Z1", "Z2"),
    group_col = "group",
    syn_size = NULL
  ))

  # Test for wrong x_cols
  expect_error(validate_lmm_initialization_input(
    formula = Y ~ X1,
    data = lmm_data,
    x_cols = c("X1", "X2", "X3", "X4"),
    y_col = "Y",
    z_cols = c("Z1", "Z2", "Z3"),
    group_col = "group",
    syn_size = NULL
  ))

  # Test for wrong z_cols
  expect_error(validate_lmm_initialization_input(
    formula = Y ~ X1,
    data = lmm_data,
    x_cols = c("X1", "X2", "X3"),
    y_col = "Y",
    z_cols = c("Z1", "Z2", "Z3", "Z4"),
    group_col = "group",
    syn_size = NULL
  ))

  # Test for y_col being vector
  expect_error(validate_lmm_initialization_input(
    formula = Y ~ X1,
    data = lmm_data,
    x_cols = c("X1", "X2"),
    y_col = c("Y", "X3"),
    z_cols = c("Z1", "Z2", "Z3"),
    group_col = "group",
    syn_size = NULL
  ))

  # Test for y_col being wrong
  expect_error(validate_lmm_initialization_input(
    formula = Y ~ X1,
    data = lmm_data,
    x_cols = c("X1", "X2", "X3"),
    y_col = "Y1",
    z_cols = c("Z1", "Z2", "Z3"),
    group_col = "group",
    syn_size = NULL
  ))

  # Test for group_col being a vector
  expect_error(validate_lmm_initialization_input(
    formula = Y ~ X1,
    data = lmm_data,
    x_cols = c("X1", "X2", "X3"),
    y_col = "Y",
    z_cols = c("Z1", "Z2"),
    group_col = c("group", "Z3"),
    syn_size = NULL
  ))

  # Test for group_col being wrong
  expect_error(validate_lmm_initialization_input(
    formula = Y ~ X1,
    data = lmm_data,
    x_cols = c("X1", "X2", "X3"),
    y_col = "Y",
    z_cols = c("Z1", "Z2", "Z3"),
    group_col = c("group1"),
    syn_size = NULL
  ))

  # Test for syn_size is 0
  expect_error(validate_lmm_initialization_input(
    formula = Y ~ X1,
    data = lmm_data,
    x_cols = c("X1", "X2", "X3"),
    y_col = "Y",
    z_cols = c("Z1", "Z2", "Z3"),
    group_col = c("group"),
    syn_size = 0
  ))

  # Test for syn_size is positive and large enough
  expect_silent(validate_lmm_initialization_input(
    formula = Y ~ X1,
    data = lmm_data,
    x_cols = c("X1", "X2", "X3"),
    y_col = "Y",
    z_cols = c("Z1", "Z2", "Z3"),
    group_col = c("group"),
    syn_size = 100
  ))

  # Test for syn_size is zero
  expect_error(validate_lmm_initialization_input(
    formula = Y ~ X1,
    data = lmm_data,
    x_cols = c("X1", "X2", "X3"),
    y_col = "Y",
    z_cols = c("Z1", "Z2", "Z3"),
    group_col = c("group"),
    syn_size = 0
  ))

  # Test for syn_size is negative
  expect_error(validate_lmm_initialization_input(
    formula = Y ~ X1,
    data = lmm_data,
    x_cols = c("X1", "X2", "X3"),
    y_col = "Y",
    z_cols = c("Z1", "Z2", "Z3"),
    group_col = c("group"),
    syn_size = -1
  ))
})

# ----- General -----
test_that("validate_positive function works as expected", {
  # Test for a positive scalar input, should not raise an error
  expect_silent(validate_positive("param1", 5))

  # Test for a zero scalar input, incl_0 = TRUE, should not raise an error
  expect_silent(validate_positive("param1", 0, incl_0 = TRUE))

  # Test for a positive vector input, is_vector = TRUE, should not raise an error
  expect_silent(validate_positive("param1", c(2, 3, 4), is_vector = TRUE))

  # Test for a vector with zero values, incl_0 = TRUE, should not raise an error
  expect_silent(validate_positive("param1", c(0, 2, 3), incl_0 = TRUE, is_vector = TRUE))

  # Test for a negative scalar input, expect an error
  expect_error(
    validate_positive("param1", -5),
    "The provided `param1` should be a positive number."
  )

  # Test for a zero scalar input without incl_0 = TRUE, expect an error
  expect_error(
    validate_positive("param1", 0),
    "The provided `param1` should be a positive number."
  )

  # Test for a negative value in vector input, is_vector = TRUE, expect an error
  expect_error(
    validate_positive("param1", c(-1, 2, 3), is_vector = TRUE),
    "The provided `param1` should be a vector of positive numbers."
  )

  # Test for a non-vector input when is_vector = TRUE, expect an error
  expect_error(
    validate_positive("param1", 5, is_vector = TRUE),
    "The provided `param1` should be a vector of positive numbers."
  )

  # Test for a single element when is_vector = TRUE and incl_0 = TRUE, expect an error if length is 1
  expect_error(
    validate_positive("param1", 0, incl_0 = TRUE, is_vector = TRUE),
    "The provided `param1` should be a vector of non-negative numbers."
  )
})

test_that("get_stan_model loads the correct Stan model file", {
  # Test for default glm type with gaussian family with no additional options
  stan_model <- get_stan_model(type = "glm", glm_family_string = "gaussian")
  expect_s4_class(stan_model, "stanmodel") # Check that it returns a Stan model
  expect_equal(attr(stan_model@model_code, "model_name2"), "glm_gaussian") # Check filename pattern

  # Test for Cox model type
  stan_model <- get_stan_model(type = "cox")
  expect_s4_class(stan_model, "stanmodel")
  expect_equal(attr(stan_model@model_code, "model_name2"), "cox")

  # Test fwith `joint_tau = TRUE` or Cox model type
  stan_model <- get_stan_model(type = "cox", joint_tau = TRUE)
  expect_s4_class(stan_model, "stanmodel")
  expect_equal(attr(stan_model@model_code, "model_name2"), "cox_joint")

  # Test with `glm_family_string` = "binomial"
  stan_model <- get_stan_model(type = "glm", glm_family_string = "binomial")
  expect_s4_class(stan_model, "stanmodel")
  expect_equal(attr(stan_model@model_code, "model_name2"), "glm_binomial")

  # Test with `joint_tau = TRUE` (and binomial family to test combined settings)
  stan_model <- get_stan_model(type = "glm", glm_family_string = "binomial", joint_tau = TRUE)
  expect_s4_class(stan_model, "stanmodel")
  expect_equal(attr(stan_model@model_code, "model_name2"), "glm_binomial_joint")

  # Test for `glm_gaussian_known_variance = TRUE` with gaussian family
  stan_model <- get_stan_model(type = "glm", glm_family_string = "gaussian", glm_gaussian_known_variance = TRUE)
  expect_s4_class(stan_model, "stanmodel")
  expect_equal(attr(stan_model@model_code, "model_name2"), "glm_gaussian_known_variance")

  # Test for `glm_gaussian_known_variance = TRUE` and `joint_tau = TRUE` with gaussian family
  stan_model <- get_stan_model(
    type = "glm", glm_family_string = "gaussian", joint_tau = TRUE,
    glm_gaussian_known_variance = TRUE
  )
  expect_s4_class(stan_model, "stanmodel")
  expect_equal(attr(stan_model@model_code, "model_name2"), "glm_gaussian_joint_known_variance")

  # Test for joint_tau = TRUE` with gaussian family
  stan_model <- get_stan_model(
    type = "glm", glm_family_string = "gaussian", joint_tau = TRUE
  )
  expect_s4_class(stan_model, "stanmodel")
  expect_equal(attr(stan_model@model_code, "model_name2"), "glm_gaussian_joint")

  # Test with multiple options set: `glm_binomial_joint_theta` and `glm_binomial_joint_alpha`
  stan_model <- get_stan_model(
    type = "glm",
    glm_family_string = "binomial",
    glm_binomial_joint_theta = TRUE,
    glm_binomial_joint_alpha = TRUE
  )
  expect_s4_class(stan_model, "stanmodel")
  expect_equal(attr(stan_model@model_code, "model_name2"), "glm_binomial_joint_theta_alpha")

  # Test with `glm_binomial_joint_theta`
  stan_model <- get_stan_model(
    type = "glm",
    glm_family_string = "binomial",
    glm_binomial_joint_theta = TRUE
  )
  expect_s4_class(stan_model, "stanmodel")
  expect_equal(attr(stan_model@model_code, "model_name2"), "glm_binomial_joint_theta")

  # Test with `glm_binomial_joint_alpha`
  stan_model <- get_stan_model(
    type = "glm",
    glm_family_string = "binomial",
    glm_binomial_joint_alpha = TRUE
  )
  expect_s4_class(stan_model, "stanmodel")
  expect_equal(attr(stan_model@model_code, "model_name2"), "glm_binomial_joint")

  # Edge case: invalid `glm_family_string`
  expect_error(get_stan_model(type = "glm", glm_family_string = "invalid_family"))
})

test_that("get_linear_predictor works correctly", {
  # Test with no intercept
  X <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
  coefs <- c(0.5, 1.5)

  expect_equal(
    get_linear_predictor(X, coefs),
    X %*% coefs
  )

  # Test with intercept
  coefs_with_intercept <- c(1, 0.5, 1.5)

  expect_equal(
    get_linear_predictor(X, coefs_with_intercept),
    coefs_with_intercept[1] + X %*% coefs_with_intercept[-1]
  )

  # Test with dimension mismatch
  expect_error(get_linear_predictor(X, c(0.5)))

  # Test with minimal input
  X_small <- matrix(1, ncol = 1)
  coefs_small <- c(1)
  expect_equal(
    get_linear_predictor(X_small, coefs_small),
    X_small %*% coefs_small
  )

  # Test with intercept and minimal input
  coefs_small_with_intercept <- c(1, 2)
  expect_equal(
    get_linear_predictor(X_small, coefs_small_with_intercept),
    coefs_small_with_intercept[1] + X_small %*% coefs_small_with_intercept[2]
  )
})

test_that("get_discrepancy computes in consistence", {
  X <- matrix(rnorm(15), ncol = 3)
  coefs <- rnorm(4)

  binomial_Y <- rbinom(5, 1, 0.5)
  gaussian_Y <- rnorm(5)

  lp <- coefs[1] + c(X %*% coefs[-1])
  binomial_est_Y <- 1 / (1 + exp(-lp))
  gaussian_est_Y <- lp

  # Test for logarithmic error
  expect_equal(
    get_discrepancy(
      discrepancy_method = "mean_logarithmic_error",
      family_string = "binomial",
      X = X,
      Y = binomial_Y,
      coefs = coefs
    ),
    get_discrepancy(
      discrepancy_method = "mean_logarithmic_error",
      family_string = "binomial",
      X = X,
      Y = binomial_Y,
      est_Y = binomial_est_Y
    )
  )

  # Test for mean square error
  expect_equal(
    get_discrepancy(
      discrepancy_method = "mean_square_error",
      family_string = "gaussian",
      X = X,
      Y = gaussian_Y,
      coefs = coefs
    ),
    get_discrepancy(
      discrepancy_method = "mean_square_error",
      family_string = "gaussian",
      X = X,
      Y = gaussian_Y,
      est_Y = gaussian_est_Y
    )
  )

  # Test for mean classification error
  expect_equal(
    get_discrepancy(
      discrepancy_method = "mean_classification_error",
      family_string = "binomial",
      X = X,
      Y = binomial_Y,
      coefs = coefs
    ),
    get_discrepancy(
      discrepancy_method = "mean_classification_error",
      family_string = "binomial",
      X = X,
      Y = binomial_Y,
      est_Y = binomial_est_Y
    )
  )

  # Test for logistic deviance with no coefs input
  expect_error(
    get_discrepancy(
      discrepancy_method = "logistic_deviance",
      family_string = "binomial",
      X = X,
      Y = binomial_Y,
      est_Y = binomial_est_Y
    )
  )

  # Test for logistic deviance with coefs input
  expect_silent(
    get_discrepancy(
      discrepancy_method = "logistic_deviance",
      family_string = "binomial",
      X = X,
      Y = binomial_Y,
      coefs = coefs
    )
  )

  # Test for logistic deviance with different coefs input
  expect_false(
    get_discrepancy(
      discrepancy_method = "logistic_deviance",
      family_string = "binomial",
      X = X,
      Y = binomial_Y,
      coefs = coefs
    ) == get_discrepancy(
      discrepancy_method = "logistic_deviance",
      family_string = "binomial",
      X = X,
      Y = binomial_Y,
      coefs = rnorm(4)
    )
  )
})


test_that("get_adjusted_cat_init adjusts data correctly", {
  formula_rhs_0 <- ~1
  formula_rhs_1 <- ~X1
  formula_rhs_2 <- ~ X1 + X2
  formula_rhs_3 <- ~.

  # Adjust the data
  adjusted_cat_init_0 <- get_adjusted_cat_init(cat_init_glm_gaussian, formula_rhs_0)
  adjusted_cat_init_1 <- get_adjusted_cat_init(cat_init_glm_gaussian, formula_rhs_1)
  adjusted_cat_init_2 <- get_adjusted_cat_init(cat_init_glm_binomial, formula_rhs_2)
  adjusted_cat_init_3 <- get_adjusted_cat_init(cat_init_cox, formula_rhs_3)

  # Check that the adjusted data frame has the expected structure
  expect_s3_class(adjusted_cat_init_0$adj_x, "data.frame")
  expect_s3_class(adjusted_cat_init_1$adj_x, "data.frame")
  expect_s3_class(adjusted_cat_init_2$adj_x, "data.frame")
  expect_s3_class(adjusted_cat_init_3$adj_x, "data.frame")

  # Validate sizes and contents of adjusted observed and synthetic parts
  expect_equal(nrow(adjusted_cat_init_0$adj_obs_x), adjusted_cat_init_0$obs_size)
  expect_equal(nrow(adjusted_cat_init_0$adj_syn_x), adjusted_cat_init_0$size - adjusted_cat_init_0$obs_size)

  expect_equal(nrow(adjusted_cat_init_1$adj_obs_x), adjusted_cat_init_1$obs_size)
  expect_equal(nrow(adjusted_cat_init_1$adj_syn_x), adjusted_cat_init_1$size - adjusted_cat_init_1$obs_size)

  expect_equal(nrow(adjusted_cat_init_2$adj_obs_x), adjusted_cat_init_2$obs_size)
  expect_equal(nrow(adjusted_cat_init_2$adj_syn_x), adjusted_cat_init_2$size - adjusted_cat_init_2$obs_size)

  expect_equal(nrow(adjusted_cat_init_3$adj_obs_x), adjusted_cat_init_3$obs_size)
  expect_equal(nrow(adjusted_cat_init_3$adj_syn_x), adjusted_cat_init_3$size - adjusted_cat_init_3$obs_size)

  # Verify that the adjusted data contains the expected columns based on the formulas
  expect_equal(ncol(adjusted_cat_init_0$adj_x), 0)
  expect_true(all(c("X1") %in% colnames(adjusted_cat_init_1$adj_x)))
  expect_true(all(c("X1", "X2") %in% colnames(adjusted_cat_init_2$adj_x)))
  expect_true(all(c("X1", "X2", "X3") %in% colnames(adjusted_cat_init_3$adj_x)))

  expect_equal(ncol(adjusted_cat_init_0$adj_obs_x), 0)
  expect_true(all(c("X1") %in% colnames(adjusted_cat_init_1$adj_obs_x)))
  expect_true(all(c("X1", "X2") %in% colnames(adjusted_cat_init_2$adj_obs_x)))
  expect_true(all(c("X1", "X2", "X3") %in% colnames(adjusted_cat_init_3$adj_obs_x)))

  expect_equal(ncol(adjusted_cat_init_0$adj_syn_x), 0)
  expect_true(all(c("X1") %in% colnames(adjusted_cat_init_1$adj_syn_x)))
  expect_true(all(c("X1", "X2") %in% colnames(adjusted_cat_init_2$adj_syn_x)))
  expect_true(all(c("X1", "X2", "X3") %in% colnames(adjusted_cat_init_3$adj_syn_x)))
})

test_that("get_resampled_df works correctly for various cases", {
  mock_data <- data.frame(
    X1 = c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1000), # Continuous variable (Test for Deskewing)
    X2 = c(0, 1, 1, 1, 1, 1, 1, 1, 1, 1), # Categorical variable (Test for Flattening)
    X3 = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4), # Another continuous variable (Test for Smoothing)
    X4 = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5) # Another continuous variable (Test for no action)
  )
  data_degree <- c(1, 1, 3, 1)

  # Test basic resampling functionality
  resample_size <- 50
  result <- get_resampled_df(
    data = mock_data,
    resample_size = resample_size,
    data_degree = data_degree
  )

  expect_false(setequal(unique(result$resampled_df[, 1]), unique(mock_data[, 1])))
  expect_true(setequal(unique(result$resampled_df[, 2]), unique(mock_data[, 2])))
  expect_false(setequal(unique(result$resampled_df[, 3]), unique(mock_data[, 3])))
  expect_true(setequal(unique(result$resampled_df[, 4]), unique(mock_data[, 4])))

  # Validate that each column are identified with correct type
  expect_equal(result$resampled_df_log$Type[1], "Continuous")
  expect_equal(result$resampled_df_log$Type[2], "Category")
  expect_equal(result$resampled_df_log$Type[3], "Continuous")
  expect_equal(result$resampled_df_log$Type[4], "Continuous")

  # Validate that each column is processed according to its type
  expect_true(grepl("Deskewing", result$resampled_df_log$Process[1]))
  expect_false(grepl("Flattening", result$resampled_df_log$Process[1]))
  expect_false(grepl("Smoothing", result$resampled_df_log$Process[1]))

  expect_false(grepl("Deskewing", result$resampled_df_log$Process[2]))
  expect_true(grepl("Flattening", result$resampled_df_log$Process[2]))
  expect_false(grepl("Smoothing", result$resampled_df_log$Process[2]))

  expect_false(grepl("Deskewing", result$resampled_df_log$Process[3]))
  expect_false(grepl("Flattening", result$resampled_df_log$Process[3]))
  expect_true(grepl("Smoothing", result$resampled_df_log$Process[3]))

  expect_false(grepl("Deskewing", result$resampled_df_log$Process[4]))
  expect_false(grepl("Flattening", result$resampled_df_log$Process[4]))
  expect_false(grepl("Smoothing", result$resampled_df_log$Process[4]))

  # Check that the resampled data frame has the correct dimensions
  expect_equal(nrow(result$resampled_df), resample_size)
  expect_equal(ncol(result$resampled_df), ncol(mock_data))

  # Check that the resampled log is created correctly
  expect_true(!is.null(result$resampled_df_log))
  expect_equal(nrow(result$resampled_df_log), ncol(mock_data))

  # Validate that resampling processes are logged correctly
  expect_true(all(c("Covariate", "Type", "Process") %in% colnames(result$resampled_df_log)))

  # Test with resample_only = TRUE
  result_resample_only <- get_resampled_df(
    data = mock_data,
    resample_size = resample_size,
    resample_only = TRUE
  )
  expect_true(setequal(unique(result_resample_only$resampled_df[, 1]), unique(mock_data[, 1])))
  expect_true(setequal(unique(result_resample_only$resampled_df[, 2]), unique(mock_data[, 2])))
  expect_true(setequal(unique(result_resample_only$resampled_df[, 3]), unique(mock_data[, 3])))
  expect_true(setequal(unique(result_resample_only$resampled_df[, 4]), unique(mock_data[, 4])))
  expect_null(result_resample_only$resampled_df_log)

  # Test without data_degree parameter
  result_without_degree <- get_resampled_df(
    data = mock_data,
    resample_size = resample_size,
    data_degree = NULL
  )

  # Check if processes are logged with degree
  expect_false(is.null(result_without_degree$resampled_df_log))

  # Validate that each column is processed according to its type
  expect_true(grepl("Deskewing", result_without_degree$resampled_df_log$Process[1]))
  expect_false(grepl("Flattening", result_without_degree$resampled_df_log$Process[1]))
  expect_false(grepl("Smoothing", result_without_degree$resampled_df_log$Process[1]))

  expect_false(grepl("Deskewing", result_without_degree$resampled_df_log$Process[2]))
  expect_true(grepl("Flattening", result_without_degree$resampled_df_log$Process[2]))
  expect_false(grepl("Smoothing", result_without_degree$resampled_df_log$Process[2]))

  expect_false(grepl("Deskewing", result_without_degree$resampled_df_log$Process[3]))
  expect_false(grepl("Flattening", result_without_degree$resampled_df_log$Process[3]))
  expect_false(grepl("Smoothing", result_without_degree$resampled_df_log$Process[3]))

  expect_false(grepl("Deskewing", result_without_degree$resampled_df_log$Process[4]))
  expect_false(grepl("Flattening", result_without_degree$resampled_df_log$Process[4]))
  expect_false(grepl("Smoothing", result_without_degree$resampled_df_log$Process[4]))
})

test_that("get_formula_string works correctly", {
  # Test with a simple formula
  formula <- y ~ x1 + x2
  expected_string <- "y ~ x1 + x2"
  expect_equal(get_formula_string(formula), expected_string)

  # Test with an interaction term
  formula_interaction <- y ~ x1 * x2
  expected_string_interaction <- "y ~ x1 * x2"
  expect_equal(get_formula_string(formula_interaction), expected_string_interaction)

  # Test with a more complex formula
  formula_complex <- y ~ x1 + x2 + I(x1^2) + log(x2)
  expected_string_complex <- "y ~ x1 + x2 + I(x1^2) + log(x2)"
  expect_equal(get_formula_string(formula_complex), expected_string_complex)
})

test_that("get_formula_lhs works correctly", {
  # Test with a simple formula
  formula <- y ~ x1 + x2
  expected_lhs <- "y"
  expect_equal(get_formula_lhs(formula), expected_lhs)

  # Test with an interaction term
  formula_interaction <- y ~ x1 * x2
  expected_lhs_interaction <- "y"
  expect_equal(get_formula_lhs(formula_interaction), expected_lhs_interaction)

  # Test with a more complex formula
  formula_complex <- y ~ x1 + x2 + I(x1^2) + log(x2)
  expected_lhs_complex <- "y"
  expect_equal(get_formula_lhs(formula_complex), expected_lhs_complex)
})

test_that("get_formula_rhs works correctly", {
  # Test with a simple formula
  formula <- y ~ x1 + x2
  expected_rhs <- "x1 + x2"
  expect_equal(get_formula_rhs(formula), expected_rhs)

  # Test with tilde included
  expected_rhs_with_tilde <- "~ x1 + x2"
  expect_equal(get_formula_rhs(formula, with_tilde = TRUE), expected_rhs_with_tilde)

  # Test with interaction term
  formula_interaction <- y ~ x1 * x2
  expected_rhs_interaction <- "x1 * x2"
  expect_equal(get_formula_rhs(formula_interaction), expected_rhs_interaction)

  # Test with a more complex formula
  formula_complex <- y ~ x1 + x2 + I(x1^2) + log(x2)
  expected_rhs_complex <- "x1 + x2 + I(x1^2) + log(x2)"
  expect_equal(get_formula_rhs(formula_complex), expected_rhs_complex)

  # Test with tilde for complex formula
  expected_rhs_complex_with_tilde <- "~ x1 + x2 + I(x1^2) + log(x2)"
  expect_equal(get_formula_rhs(formula_complex, with_tilde = TRUE), expected_rhs_complex_with_tilde)
})


test_that("is.continuous works correctly", {
  # Test with continuous numeric vector
  continuous_vector <- c(1, 2, 3, 4, 5)
  expect_true(is.continuous(continuous_vector))

  # Test with categorical vector
  categorical_vector <- c("A", "B", "A", "B", "C")
  expect_false(is.continuous(categorical_vector))

  # Test with binary numeric vector
  binary_vector <- c(0, 1, 1, 0, 1)
  expect_false(is.continuous(binary_vector))

  # Test with a numeric vector with only two unique values
  two_unique_vector <- c(1, 1, 2, 2, 1)
  expect_false(is.continuous(two_unique_vector))

  # Test with numeric vector with more than two unique values
  mixed_numeric_vector <- c(1, 2, 3, 4, 5, 6)
  expect_true(is.continuous(mixed_numeric_vector))
})

test_that("get_standardized_data works correctly", {
  # Test with numeric and factor columns
  data <- data.frame(
    numeric_col = c(2, 3, NA, 4),
    factor_col = factor(c("A", "B", "A", NA)),
    stringsAsFactors = FALSE
  )

  # Test with numeric NA replacement
  standardized_data <- get_standardized_data(data, na_replace = 0)
  expect_equal(standardized_data$numeric_col, c(2, 3, 0, 4))
  expect_equal(standardized_data$factor_col, factor(c("A", "B", "A", "A")))

  # Test with mode replacement for factors
  standardized_data <- get_standardized_data(data, na_replace = mean)
  expect_equal(standardized_data$numeric_col, c(2, 3, 3, 4))

  # Test with NA omission
  data_with_na_omit <- get_standardized_data(data, na_replace = stats::na.omit)
  expect_equal(nrow(data_with_na_omit), 2)
  expect_equal(ncol(data_with_na_omit), 2)

  # Test with no NA values
  data_no_na <- data.frame(
    numeric_col = c(1, 2, 4),
    factor_col = factor(c("A", "B", "A")),
    stringsAsFactors = FALSE
  )
  standardized_data_no_na <- get_standardized_data(data_no_na)
  expect_equal(standardized_data_no_na, data_no_na)
})

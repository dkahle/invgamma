glm_gaussian_data <- mock_glm_gaussian_data()
glm_binomial_data <- mock_glm_binomial_data()
cat_init_gaussian <- mock_cat_glm_gaussian_initialization(glm_gaussian_data)
cat_init_binomial <- mock_cat_glm_binomial_initialization(glm_binomial_data)
cat_model_gaussian <- cat_glm_tune(formula = ~., cat_init = cat_init_gaussian)
cat_model_binomial <- cat_glm_tune(formula = ~., cat_init = cat_init_binomial)

test_that("cat_glm_tune runs without errors for valid input", {
  expected_components <- c(
    "function_name", "formula", "cat_init",
    "risk_estimate_method", "discrepancy_method", "tau_seq", "tau_0",
    "parametric_bootstrap_iteration_times", "cross_validation_fold_num",
    "tau", "model", "coefficients", "risk_estimate_list"
  )

  cat_model_gaussian$cat_init$adj_obs_x <- cat_model_gaussian$cat_init$adj_syn_x <- cat_model_gaussian$cat_init$adj_x <- NULL
  cat_model_binomial$cat_init$adj_obs_x <- cat_model_binomial$cat_init$adj_syn_x <- cat_model_binomial$cat_init$adj_x <- NULL

  expect_type(cat_model_gaussian, "list")
  expect_type(cat_model_binomial, "list")
  expect_equal(cat_model_gaussian$function_name, "cat_glm_tune")
  expect_equal(cat_model_binomial$function_name, "cat_glm_tune")
  expect_equal(cat_model_gaussian$cat_init, cat_init_gaussian)
  expect_equal(cat_model_binomial$cat_init, cat_init_binomial)
  expect_true(all(expected_components %in% names(cat_model_gaussian)))
  expect_true(all(expected_components %in% names(cat_model_binomial)))

  expect_true(inherits(cat_model_gaussian$model, "glm"))
  expect_true(inherits(cat_model_binomial$model, "glm"))
  expect_equal(length(cat_model_gaussian$risk_estimate_list), length(cat_model_gaussian$tau_seq))
  expect_equal(length(cat_model_binomial$risk_estimate_list), length(cat_model_binomial$tau_seq))
})

test_that("cat_glm_tune returns similar result with same observation data", {
  cat_init_gaussian_2 <- mock_cat_glm_gaussian_initialization(glm_gaussian_data)
  cat_init_binomial_2 <- mock_cat_glm_binomial_initialization(glm_binomial_data)
  cat_model_gaussian_2 <- cat_glm_tune(formula = ~., cat_init = cat_init_gaussian_2)
  cat_model_binomial_2 <- cat_glm_tune(formula = ~., cat_init = cat_init_binomial_2)

  expect_true(all(cat_init_gaussian$obs_data == cat_init_gaussian_2$obs_data)) # Same observation data
  expect_true(all(cat_init_binomial$obs_data == cat_init_binomial_2$obs_data)) # Same observation data
  expect_false(all(cat_init_gaussian$syn_data == cat_init_gaussian_2$syn_data)) # Different synthetic data
  expect_false(all(cat_init_binomial$syn_data == cat_init_binomial_2$syn_data)) # Different synthetic data
  expect_equal(cat_model_gaussian$tau, cat_model_gaussian_2$tau)
  expect_equal(cat_model_binomial$tau, cat_model_binomial_2$tau)
})

test_that("cat_glm_tune returns different result with different tau values", {
  cat_model_gaussian_low_tau <- cat_glm_tune(formula = ~., cat_init = cat_init_gaussian, tau_seq = seq(1, 3))
  cat_model_gaussian_high_tau <- cat_glm_tune(formula = ~., cat_init = cat_init_gaussian, tau_seq = seq(10, 13))
  cat_model_binomial_low_tau <- cat_glm_tune(formula = ~., cat_init = cat_init_binomial, tau_seq = seq(1, 3))
  cat_model_binomial_high_tau <- cat_glm_tune(formula = ~., cat_init = cat_init_binomial, tau_seq = seq(10, 13))

  expect_true(all(coef(cat_model_gaussian_low_tau) != coef(cat_model_gaussian_high_tau)))
  expect_true(all(coef(cat_model_binomial_low_tau) != coef(cat_model_binomial_high_tau)))
})

test_that("cat_glm_tune can select appropriate input according to obervation data", {
  # Small data size
  expect_equal(cat_model_gaussian$risk_estimate_method, "parametric_bootstrap")
  expect_equal(cat_model_gaussian$discrepancy_method, "mean_square_error")
  expect_equal(cat_model_binomial$risk_estimate_method, "parametric_bootstrap")
  expect_equal(cat_model_binomial$discrepancy_method, "logistic_deviance")

  # Large data size
  cat_init_gaussian_300_size <- mock_cat_glm_gaussian_initialization(get_resampled_df(glm_gaussian_data, resample_size = 1001, resample_only = TRUE)$resampled_df)
  cat_init_binomial_300_size <- mock_cat_glm_binomial_initialization(get_resampled_df(glm_binomial_data, resample_size = 1001, resample_only = TRUE)$resampled_df)
  cat_model_gaussian_300_size <- cat_glm_tune(formula = ~., cat_init = cat_init_gaussian_300_size)
  cat_model_binomial_300_size <- cat_glm_tune(formula = ~., cat_init = cat_init_binomial_300_size)
  expect_equal(cat_model_gaussian_300_size$risk_estimate_method, "cross_validation")
  expect_equal(cat_model_gaussian_300_size$discrepancy_method, "mean_square_error")
  expect_equal(cat_model_binomial_300_size$risk_estimate_method, "cross_validation")
  expect_equal(cat_model_binomial_300_size$discrepancy_method, "logistic_deviance")
})

test_that("cat_glm_tune handles invalid inputs gracefully", {
  # Mock the menu function to simulate user input
  mock_menu <- function(...) {
    return(2) # Simulating user input "NO" to terminate
  }

  # Assign the mock_menu to replace the original menu function
  assign("menu", mock_menu, envir = globalenv())

  # Test for invalid pair
  expect_error(cat_glm_tune(formula = ~., risk_estimate_method = "steinian_estimate", cat_init = cat_init_gaussian))
  expect_warning(cat_glm_tune(formula = ~., risk_estimate_method = "steinian_estimate", discrepancy_method = "mean_classification_error", cat_init = cat_init_binomial))
  expect_error(cat_glm_tune(formula = ~., risk_estimate_method = "mallowian_estimate", cat_init = cat_init_binomial))
  expect_error(cat_glm_tune(formula = ~., risk_estimate_method = "parametric_bootstrap", discrepancy_method = "logistic_deviance", cat_init = cat_init_gaussian))
  expect_error(cat_glm_tune(formula = ~., risk_estimate_method = "parametric_bootstrap", discrepancy_method = "mean_classification", cat_init = cat_init_gaussian))
  expect_error(cat_glm_tune(formula = ~., risk_estimate_method = "parametric_bootstrap", discrepancy_method = "mean_square_error", cat_init = cat_init_binomial))
  expect_warning(cat_glm_tune(formula = ~., risk_estimate_method = "cross_validation", cat_init = cat_init_gaussian))

  # Test for large size data for certain method
  cat_init_gaussian_1001_size <- mock_cat_glm_gaussian_initialization(get_resampled_df(glm_gaussian_data, resample_size = 1001, resample_only = TRUE)$resampled_df)
  expect_error(cat_glm_tune(formula = ~., risk_estimate_method = "parametric_bootstrap", cat_init = cat_init_gaussian_1001_size))
  cat_init_binomial_801_size <- mock_cat_glm_binomial_initialization(get_resampled_df(glm_binomial_data, resample_size = 8001, resample_only = TRUE)$resampled_df)
  expect_error(cat_glm_tune(formula = ~., risk_estimate_method = "steinian_estimate", cat_init = cat_init_binomial_801_size))

  # Test for large cross_validation_fold_num
  expect_warning(cat_glm_tune(formula = ~., risk_estimate_method = "cross_validation", cat_init = cat_init_gaussian, cross_validation_fold_num = 5))

  # Test for extreme large cross_validation_fold_num
  expect_error(cat_glm_tune(formula = ~., risk_estimate_method = "cross_validation", cat_init = cat_init_gaussian, cross_validation_fold_num = 100))
})

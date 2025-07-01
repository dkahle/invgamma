lmm_data <- mock_lmm_data()
cat_init <- mock_cat_lmm_initialization(lmm_data)
cat_model <- cat_lmm(cat_init = cat_init)

test_that("cat_lmm runs without errors for valid input", {
  expected_components <- c(
    "function_name", "cat_init", "tau", "residual_variance_0",
    "random_effect_variance_0", "coefs_0", "optimize_domain",
    "max_iter", "tol", "coefficients", "iteration_log"
  )

  expect_type(cat_model, "list")
  expect_equal(cat_model$function_name, "cat_lmm")
  expect_true(all(expected_components %in% names(cat_model)))
})

test_that("cat_lmm returns similar result with same observation data", {
  cat_init_2 <- mock_cat_lmm_initialization(lmm_data)
  cat_model_2 <- cat_lmm(cat_init = cat_init)

  expect_true(all(cat_init$obs_data == cat_init_2$obs_data)) # Same observation data
  expect_false(all(cat_init$syn_data == cat_init_2$syn_data)) # Different synthetic data
  expect_equal(coef(cat_model), coef(cat_model_2), tolerance = 1e-05)
})

test_that("cat_lmm returns expected convergence with different tau values", {
  cat_model_low_tau <- cat_lmm(cat_init = cat_init, tau = 1)
  cat_model_high_tau <- cat_lmm(cat_init = cat_init, tau = 10)

  expect_true(all(coef(cat_model_low_tau) != coef(cat_model_high_tau)))
})

test_that("cat_lmm iteration log records all iterations", {
  cat_model <- cat_lmm(cat_init = cat_init, max_iter = 1)
  expect_equal(nrow(cat_model$iteration_log), 2)
})

test_that("cat_lmm handles edge cases for residual and random effect variance", {
  cat_model_low_variance <- cat_lmm(
    cat_init = cat_init,
    residual_variance_0 = 0.1,
    random_effect_variance_0 = 0.1
  )

  expect_true(cat_model_low_variance$residual_variance_0 == 0.1)
  expect_true(cat_model_low_variance$random_effect_variance_0 == 0.1)
})

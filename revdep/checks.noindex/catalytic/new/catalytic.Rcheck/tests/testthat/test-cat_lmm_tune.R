lmm_data <- mock_lmm_data()
cat_init <- mock_cat_lmm_initialization(lmm_data)
cat_model <- cat_lmm_tune(cat_init = cat_init)

test_that("cat_lmm_tune runs without errors for valid input", {
  expected_components <- c(
    "function_name", "cat_init",
    "tau_seq", "cross_validation_fold_num",
    "tau", "model",
    "coefficients", "risk_estimate_list"
  )

  expect_type(cat_model, "list")
  expect_equal(cat_model$function_name, "cat_lmm_tune")
  expect_equal(cat_model$cat_init, cat_init)
  expect_true(all(expected_components %in% names(cat_model)))

  expect_true(inherits(cat_model$model, "lmerMod"))
  expect_equal(length(cat_model$risk_estimate_list), length(cat_model$tau_seq))
})

test_that("cat_lmm_tune returns similar result with same observation data", {
  cat_init_2 <- mock_cat_lmm_initialization(lmm_data)
  cat_model_2 <- cat_lmm_tune(cat_init = cat_init)

  expect_true(all(cat_init$obs_data == cat_init_2$obs_data)) # Same observation data
  expect_false(all(cat_init$syn_data == cat_init_2$syn_data)) # Different synthetic data
  expect_equal(coef(cat_model), coef(cat_model_2), tolerance = 1e-05)
})

test_that("cat_lmm_tune returns different result with different tau values", {
  cat_model_low_tau <- cat_lmm_tune(cat_init = cat_init, tau_seq = seq(1, 3))
  cat_model_high_tau <- cat_lmm_tune(cat_init = cat_init, tau_seq = seq(10, 13))

  expect_true(all(coef(cat_model_low_tau) != coef(cat_model_high_tau)))
})

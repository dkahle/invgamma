cox_data <- mock_cox_data()
cat_init <- mock_cat_cox_initialization(cox_data)
cat_model_cre <- cat_cox(formula = ~., cat_init = cat_init, method = "CRE")
cat_model_wme <- cat_cox(formula = ~., cat_init = cat_init, method = "WME")

test_that("cat_cox runs without errors for valid input", {
  expected_components <- c(
    "function_name", "formula", "cat_init", "tau", "method",
    "init_coefficients", "tol", "max_iter", "iter",
    "iteration_log", "model", "coefficients"
  )

  cat_model_cre$cat_init$adj_obs_x <- cat_model_cre$cat_init$adj_syn_x <- cat_model_cre$cat_init$adj_x <- NULL
  cat_model_wme$cat_init$adj_obs_x <- cat_model_wme$cat_init$adj_syn_x <- cat_model_wme$cat_init$adj_x <- NULL

  expect_type(cat_model_cre, "list")
  expect_type(cat_model_wme, "list")
  expect_equal(cat_model_cre$function_name, "cat_cox")
  expect_equal(cat_model_wme$function_name, "cat_cox")
  expect_equal(cat_model_cre$cat_init, cat_init)
  expect_equal(cat_model_wme$cat_init, cat_init)
  expect_true(all(expected_components %in% names(cat_model_cre)))
  expect_true(all(expected_components %in% names(cat_model_wme)))

  expect_false(is.null(cat_model_cre$iter))
  expect_false(is.null(cat_model_cre$iteration_log))
  expect_true(is.null(cat_model_wme$iter))
  expect_true(is.null(cat_model_wme$iteration_log))
})

test_that("cat_cox returns similar result with same observation data", {
  cat_init_2 <- mock_cat_cox_initialization(cox_data)
  cat_model_cre_2 <- cat_cox(formula = ~., cat_init = cat_init, method = "CRE")
  cat_model_wme_2 <- cat_cox(formula = ~., cat_init = cat_init, method = "WME")

  expect_true(all(cat_init$obs_data == cat_init_2$obs_data)) # Same observation data
  expect_false(all(cat_init$syn_data == cat_init_2$syn_data)) # Different synthetic data
  expect_equal(coef(cat_model_cre), coef(cat_model_cre_2), tolerance = 1e-05)
  expect_equal(coef(cat_model_wme), coef(cat_model_wme_2), tolerance = 1e-05)
})

test_that("cat_cox returns expected convergence with different tau values", {
  cat_model_cre_low_tau <- cat_cox(formula = ~., cat_init = cat_init, method = "CRE", tau = 0.1)
  cat_model_cre_high_tau <- cat_cox(formula = ~., cat_init = cat_init, method = "CRE", tau = 100)
  cat_model_wme_low_tau <- cat_cox(formula = ~., cat_init = cat_init, method = "WME", tau = 0.1)
  cat_model_wme_high_tau <- cat_cox(formula = ~., cat_init = cat_init, method = "WME", tau = 100)

  expect_true(all(coef(cat_model_cre_low_tau) != coef(cat_model_cre_high_tau)))
  expect_true(all(coef(cat_model_wme_low_tau) != coef(cat_model_wme_high_tau)))
})

test_that("cat_cox iteration log records all iterations for CRE", {
  cat_model_cre_low_max_iter <- cat_cox(
    formula = ~.,
    cat_init = cat_init,
    method = "CRE",
    max_iter = 1
  )
  expect_equal(nrow(cat_model_cre_low_max_iter$iteration_log), 2)
})

test_that("cat_cox with large tols has low iteration for CRE", {
  cat_model_cre_high_tol <- cat_cox(
    formula = ~.,
    cat_init = cat_init,
    method = "CRE",
    tol = 1
  )

  expect_true(cat_model_cre_high_tol$iter < cat_model_cre$iter)
})

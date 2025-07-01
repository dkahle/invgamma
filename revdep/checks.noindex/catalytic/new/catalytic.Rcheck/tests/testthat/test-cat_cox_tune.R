cox_data <- mock_cox_data()
cat_init <- mock_cat_cox_initialization(cox_data)
cat_model_cre <- cat_cox_tune(
  formula = ~.,
  cat_init = cat_init,
  method = "CRE",
  cross_validation_fold_num = 2
)

suppressWarnings(
  cat_model_wme <- cat_cox_tune(
    formula = ~.,
    cat_init = cat_init,
    method = "WME",
    cross_validation_fold_num = 2
  )
)
test_that("cat_cox_tune runs without errors for valid input", {
  expected_components <- c(
    "function_name", "formula", "cat_init", "tau_seq",
    "method", "cross_validation_fold_num", "tau", "model",
    "coefficients", "likelihood_list"
  )

  cat_model_cre$cat_init$adj_obs_x <- cat_model_cre$cat_init$adj_syn_x <- cat_model_cre$cat_init$adj_x <- NULL
  cat_model_wme$cat_init$adj_obs_x <- cat_model_wme$cat_init$adj_syn_x <- cat_model_wme$cat_init$adj_x <- NULL

  expect_type(cat_model_cre, "list")
  expect_type(cat_model_wme, "list")
  expect_equal(cat_model_cre$function_name, "cat_cox_tune")
  expect_equal(cat_model_wme$function_name, "cat_cox_tune")
  expect_equal(cat_model_cre$cat_init, cat_init)
  expect_equal(cat_model_wme$cat_init, cat_init)
  expect_true(all(expected_components %in% names(cat_model_cre)))
  expect_true(all(expected_components %in% names(cat_model_wme)))

  expect_true(inherits(cat_model_cre$model, "cat_cox"))
  expect_true(inherits(cat_model_wme$model, "cat_cox"))
  expect_equal(length(cat_model_cre$likelihood_list), length(cat_model_cre$tau_seq))
  expect_equal(length(cat_model_wme$likelihood_list), length(cat_model_wme$tau_seq))

  expect_false(is.null(cat_model_cre$model$iter))
  expect_false(is.null(cat_model_cre$model$iteration_log))
  expect_true(is.null(cat_model_wme$model$iter))
  expect_true(is.null(cat_model_wme$model$iteration_log))
})

test_that("cat_cox_tune returns similar result with same observation data", {
  cat_init_2 <- mock_cat_cox_initialization(cox_data)
  cat_model_cre_2 <- cat_cox_tune(
    formula = ~.,
    cat_init = cat_init,
    method = "CRE",
    cross_validation_fold_num = 2
  )
  suppressWarnings(
    cat_model_wme_2 <- cat_cox_tune(
      formula = ~.,
      cat_init = cat_init,
      method = "WME",
      cross_validation_fold_num = 2
    )
  )

  expect_true(all(cat_init$obs_data == cat_init_2$obs_data)) # Same observation data
  expect_false(all(cat_init$syn_data == cat_init_2$syn_data)) # Different synthetic data
  expect_equal(cat_model_cre$tau, cat_model_cre_2$tau)
  expect_equal(cat_model_wme$tau, cat_model_wme_2$tau)
})

test_that("cat_cox_tune returns different result with different tau values", {
  cat_model_cre_low_tau <- cat_cox_tune(
    formula = ~.,
    cat_init = cat_init,
    method = "CRE",
    tau_seq = seq(1, 3),
    cross_validation_fold_num = 2
  )
  suppressWarnings(
    cat_model_wme_low_tau <- cat_cox_tune(
      formula = ~.,
      cat_init = cat_init,
      method = "WME",
      tau_seq = seq(1, 3),
      cross_validation_fold_num = 2
    )
  )
  cat_model_cre_high_tau <- cat_cox_tune(
    formula = ~.,
    cat_init = cat_init,
    method = "CRE",
    tau_seq = seq(10, 13),
    cross_validation_fold_num = 2
  )
  suppressWarnings(
    cat_model_wme_high_tau <- cat_cox_tune(
      formula = ~.,
      cat_init = cat_init,
      method = "WME",
      tau_seq = seq(10, 13),
      cross_validation_fold_num = 2
    )
  )

  expect_true(all(coef(cat_model_cre_low_tau) != coef(cat_model_cre_high_tau)))
  expect_true(all(coef(cat_model_wme_low_tau) != coef(cat_model_wme_high_tau)))
})

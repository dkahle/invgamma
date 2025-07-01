cox_data <- mock_cox_data()
cat_init <- mock_cat_cox_initialization(cox_data)

chains <- 1
iter <- 100
warmup <- 50

suppressWarnings(
  cat_model <- cat_cox_bayes_joint(
    formula = ~.,
    cat_init = cat_init,
    chains = chains,
    iter = iter,
    warmup = warmup
  )
)
test_that("cat_cox_bayes_joint runs without errors for valid input", {
  expected_components <- c(
    "function_name", "cat_init", "tau", "hazard_beta", "tau_alpha", "tau_gamma",
    "chains", "iter", "warmup", "stan_data", "stan_model",
    "stan_sample_model", "coefficients", "increment_cumulative_baseline_hazard"
  )

  cat_model$cat_init$adj_obs_x <- cat_model$cat_init$adj_syn_x <- cat_model$cat_init$adj_x <- NULL

  expect_type(cat_model, "list")
  expect_equal(cat_model$function_name, "cat_cox_bayes_joint")
  expect_equal(cat_model$cat_init, cat_init)
  expect_true(all(expected_components %in% names(cat_model)))

  expect_equal(cat_model$chains, chains)
  expect_equal(cat_model$iter, iter)
  expect_equal(cat_model$warmup, warmup)
  expect_true(inherits(cat_model$stan_model, "stanmodel"))
  expect_true(inherits(cat_model$stan_sample_model, "stanfit"))
})

glm_gaussian_data <- mock_glm_gaussian_data()
glm_binomial_data <- mock_glm_binomial_data()
cat_init_gaussian <- mock_cat_glm_gaussian_initialization(glm_gaussian_data)
cat_init_binomial <- mock_cat_glm_binomial_initialization(glm_binomial_data)

cat_model_binomial <- cat_glm_bayes_joint_gibbs(
  formula = ~.,
  cat_init = cat_init_binomial
)

test_that("cat_glm_bayes_joint_gibbs runs without errors for valid input", {
  expected_components <- c(
    "function_name", "formula", "cat_init", "iter", "warmup", "coefs_iter",
    "tau_0", "tau_alpha", "tau_gamma", "refresh", "sys_time",
    "gibbs_iteration_log", "inform_df", "coefficients", "tau"
  )

  cat_model_binomial$cat_init$adj_obs_x <- cat_model_binomial$cat_init$adj_syn_x <- cat_model_binomial$cat_init$adj_x <- NULL

  expect_type(cat_model_binomial, "list")
  expect_equal(cat_model_binomial$function_name, "cat_glm_bayes_joint_gibbs")
  expect_equal(cat_model_binomial$cat_init, cat_init_binomial)
  expect_true(all(expected_components %in% names(cat_model_binomial)))

  expect_equal(cat_model_binomial$iter, 1000)
  expect_equal(cat_model_binomial$warmup, 500)
  # Check dimensions of gibbs_iteration_log
  expect_equal(dim(cat_model_binomial$gibbs_iteration_log), c(1000, ncol(cat_init_binomial$x) + 2))
  # Check structure of inform_df summary
  expect_equal(nrow(cat_model_binomial$inform_df), ncol(cat_model_binomial$gibbs_iteration_log))
  expect_equal(ncol(cat_model_binomial$inform_df), 9) # mean, se_mean, sd, 5 quantiles, n_eff
})

test_that("cat_glm_bayes_joint_gibbs only applicable to Binomial family", {
  expect_error(cat_model_binomial_high_tau_0 <- cat_glm_bayes_joint_gibbs(
    formula = ~.,
    cat_init = cat_init_gaussian,
    tau_0 = 100
  ))
})

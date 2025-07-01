glm_gaussian_data <- mock_glm_gaussian_data()
glm_binomial_data <- mock_glm_binomial_data()
cat_init_gaussian <- mock_cat_glm_gaussian_initialization(glm_gaussian_data)
cat_init_binomial <- mock_cat_glm_binomial_initialization(glm_binomial_data)
cat_model_gaussian <- cat_glm(formula = ~., cat_init = cat_init_gaussian)
cat_model_binomial <- cat_glm(formula = ~., cat_init = cat_init_binomial)

test_that("cat_glm runs without errors for valid input", {
  expected_components <- c(
    "function_name", "formula", "cat_init", "tau", "model", "coefficients"
  )

  cat_model_gaussian$cat_init$adj_obs_x <- cat_model_gaussian$cat_init$adj_syn_x <- cat_model_gaussian$cat_init$adj_x <- NULL
  cat_model_binomial$cat_init$adj_obs_x <- cat_model_binomial$cat_init$adj_syn_x <- cat_model_binomial$cat_init$adj_x <- NULL

  expect_type(cat_model_gaussian, "list")
  expect_type(cat_model_binomial, "list")
  expect_equal(cat_model_gaussian$function_name, "cat_glm")
  expect_equal(cat_model_binomial$function_name, "cat_glm")
  expect_equal(cat_model_gaussian$cat_init, cat_init_gaussian)
  expect_equal(cat_model_binomial$cat_init, cat_init_binomial)
  expect_true(all(expected_components %in% names(cat_model_gaussian)))
  expect_true(all(expected_components %in% names(cat_model_binomial)))

  expect_equal(cat_model_gaussian$tau, ncol(cat_init_gaussian$obs_x) / 4)
  expect_equal(cat_model_binomial$tau, ncol(cat_init_binomial$obs_x))
})

test_that("cat_glm returns similar result with same observation data", {
  cat_init_gaussian_2 <- mock_cat_glm_gaussian_initialization(glm_gaussian_data)
  cat_init_binomial_2 <- mock_cat_glm_binomial_initialization(glm_binomial_data)
  cat_model_gaussian_2 <- cat_glm(formula = ~., cat_init = cat_init_gaussian_2)
  cat_model_binomial_2 <- cat_glm(formula = ~., cat_init = cat_init_binomial_2)

  expect_true(all(cat_init_gaussian$obs_data == cat_init_gaussian_2$obs_data)) # Same observation data
  expect_true(all(cat_init_binomial$obs_data == cat_init_binomial_2$obs_data)) # Same observation data
  expect_false(all(cat_init_gaussian$syn_data == cat_init_gaussian_2$syn_data)) # Different synthetic data
  expect_false(all(cat_init_binomial$syn_data == cat_init_binomial_2$syn_data)) # Different synthetic data
  expect_equal(coef(cat_model_gaussian), coef(cat_model_gaussian_2), tolerance = 1e-1)
  expect_equal(coef(cat_model_binomial), coef(cat_model_binomial_2), tolerance = 1) # Larger tolerance for binomial
})

test_that("cat_glm returns expected convergence with different tau values", {
  cat_model_gaussian_low_tau <- cat_glm(formula = ~., cat_init = cat_init_gaussian, tau = 0.1)
  cat_model_gaussian_high_tau <- cat_glm(formula = ~., cat_init = cat_init_gaussian, tau = 100)
  cat_model_binomial_low_tau <- cat_glm(formula = ~., cat_init = cat_init_binomial, tau = 0.1)
  cat_model_binomial_high_tau <- cat_glm(formula = ~., cat_init = cat_init_binomial, tau = 100)

  expect_true(all(coef(cat_model_gaussian_low_tau) != coef(cat_model_gaussian_high_tau)))
  expect_true(all(coef(cat_model_binomial_low_tau) != coef(cat_model_binomial_high_tau)))
})

test_that("cat_glm replace tau = 0 to tau = 0.01 when data dimention is larger than size", {
  cat_init_gaussian_small <- mock_cat_glm_gaussian_initialization(glm_gaussian_data[1:2, ])
  cat_init_binomial_small <- mock_cat_glm_binomial_initialization(glm_binomial_data[1:2, ])
  expect_warning(cat_model_gaussian_small <- cat_glm(formula = ~., cat_init = cat_init_gaussian_small, tau = 0))
  expect_warning(cat_model_binomial_small <- cat_glm(formula = ~., cat_init = cat_init_binomial_small, tau = 0))

  expect_equal(cat_model_gaussian_small$tau, 0.01)
  expect_equal(cat_model_binomial_small$tau, 0.01)
})

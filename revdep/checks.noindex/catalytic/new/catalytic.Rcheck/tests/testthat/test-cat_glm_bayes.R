glm_gaussian_data <- mock_glm_gaussian_data()
glm_binomial_data <- mock_glm_binomial_data()
cat_init_gaussian_unknown_variance <- mock_cat_glm_gaussian_initialization(glm_gaussian_data)
cat_init_binomial <- mock_cat_glm_binomial_initialization(glm_binomial_data)

chains <- 1
iter <- 100
warmup <- 50

suppressWarnings(
  cat_model_gaussian_unknown <- cat_glm_bayes(
    formula = ~.,
    cat_init = cat_init_gaussian_unknown_variance,
    chains = chains,
    iter = iter,
    warmup = warmup
  )
)
suppressWarnings(
  cat_model_binomial <- cat_glm_bayes(
    formula = ~.,
    cat_init = cat_init_binomial,
    chains = chains,
    iter = iter,
    warmup = warmup
  )
)

test_that("cat_glm_bayes runs without errors for valid input", {
  expected_components <- c(
    "function_name", "cat_init", "tau", "algorithm",
    "chains", "iter", "warmup", "stan_data", "stan_model",
    "stan_sample_model", "coefficients"
  )

  cat_model_gaussian_unknown$cat_init$adj_obs_x <- cat_model_gaussian_unknown$cat_init$adj_syn_x <- cat_model_gaussian_unknown$cat_init$adj_x <- NULL
  cat_model_binomial$cat_init$adj_obs_x <- cat_model_binomial$cat_init$adj_syn_x <- cat_model_binomial$cat_init$adj_x <- NULL

  expect_type(cat_model_gaussian_unknown, "list")
  expect_type(cat_model_binomial, "list")
  expect_equal(cat_model_gaussian_unknown$function_name, "cat_glm_bayes")
  expect_equal(cat_model_binomial$function_name, "cat_glm_bayes")
  expect_equal(cat_model_gaussian_unknown$cat_init, cat_init_gaussian_unknown_variance)
  expect_equal(cat_model_binomial$cat_init, cat_init_binomial)
  expect_true(all(expected_components %in% names(cat_model_gaussian_unknown)))
  expect_true(all(expected_components %in% names(cat_model_binomial)))

  expect_equal(cat_model_gaussian_unknown$chains, chains)
  expect_equal(cat_model_binomial$chains, chains)
  expect_equal(cat_model_gaussian_unknown$iter, iter)
  expect_equal(cat_model_binomial$iter, iter)
  expect_equal(cat_model_gaussian_unknown$warmup, warmup)
  expect_equal(cat_model_binomial$warmup, warmup)
  expect_true(inherits(cat_model_gaussian_unknown$stan_model, "stanmodel"))
  expect_true(inherits(cat_model_binomial$stan_model, "stanmodel"))
  expect_true(inherits(cat_model_gaussian_unknown$stan_sample_model, "stanfit"))
  expect_true(inherits(cat_model_binomial$stan_sample_model, "stanfit"))
})

test_that("cat_glm_bayes returns different results with different tau values", {
  suppressWarnings(
    cat_model_gaussian_unknown_low_tau <- cat_glm_bayes(
      formula = ~.,
      cat_init = cat_init_gaussian_unknown_variance,
      tau = 0.1,
      chains = chains,
      iter = iter,
      warmup = warmup
    )
  )
  suppressWarnings(
    cat_model_binomial_low_tau <- cat_glm_bayes(
      formula = ~.,
      cat_init = cat_init_binomial,
      tau = 0.1,
      chains = chains,
      iter = iter,
      warmup = warmup
    )
  )
  suppressWarnings(
    cat_model_gaussian_unknown_high_tau <- cat_glm_bayes(
      formula = ~.,
      cat_init = cat_init_gaussian_unknown_variance,
      tau = 100,
      chains = chains,
      iter = iter,
      warmup = warmup
    )
  )
  suppressWarnings(
    cat_model_binomial_high_tau <- cat_glm_bayes(
      formula = ~.,
      cat_init = cat_init_binomial,
      tau = 100,
      chains = chains,
      iter = iter,
      warmup = warmup
    )
  )
  expect_true(all(coef(cat_model_gaussian_unknown_low_tau) != coef(cat_model_gaussian_unknown_high_tau)))
  expect_true(all(coef(cat_model_binomial_low_tau) != coef(cat_model_binomial_high_tau)))
})

test_that("cat_glm_bayes deal with known or unknown variance for Gaussian famuly properly", {
  # Unkown variance
  expect_contains(rownames(rstan::summary(cat_model_gaussian_unknown$stan_sample_model)$summary), "sigma")
  expect_equal(cat_model_gaussian_unknown$gaussian_variance_alpha, cat_model_gaussian_unknown$stan_data$variance_alpha)
  expect_equal(cat_model_gaussian_unknown$gaussian_variance_alpha, ncol(cat_init_gaussian_unknown_variance$x))
  expect_equal(cat_model_gaussian_unknown$gaussian_variance_beta, cat_model_gaussian_unknown$stan_data$variance_beta)
  expect_equal(cat_model_gaussian_unknown$gaussian_variance_beta, ncol(cat_init_gaussian_unknown_variance$x) * stats::var(cat_init_gaussian_unknown_variance$obs_y))
  expect_null(cat_model_gaussian_unknown$stan_data$sigma)

  # Known variance
  cat_init_gaussian_known_variance <- mock_cat_glm_gaussian_initialization(
    glm_gaussian_data,
    gaussian_known_variance = TRUE
  )
  suppressWarnings(
    cat_model_gaussian_known <- cat_glm_bayes(
      formula = ~.,
      cat_init = cat_init_gaussian_known_variance,
      chains = chains,
      iter = iter,
      warmup = warmup
    )
  )

  expect_false("sigma" %in% rownames(rstan::summary(cat_model_gaussian_known$stan_sample_model)$summary))
  expect_null(cat_model_gaussian_known$gaussian_variance_alpha)
  expect_null(cat_model_gaussian_known$gaussian_variance_beta)
  expect_null(cat_model_gaussian_known$stan_data$variance_alpha)
  expect_null(cat_model_gaussian_known$stan_data$variance_beta)
  expect_false(is.null(cat_model_gaussian_known$stan_data$sigma))

  # Known variance + give custom_variance
  cat_init_gaussian_known_give_variance <- mock_cat_glm_gaussian_initialization(
    glm_gaussian_data,
    gaussian_known_variance = TRUE,
    custom_variance = 1
  )
  suppressWarnings(
    cat_model_gaussian_known_give <- cat_glm_bayes(
      formula = ~.,
      cat_init = cat_init_gaussian_known_give_variance,
      chains = chains,
      iter = iter,
      warmup = warmup
    )
  )

  expect_false("sigma" %in% rownames(rstan::summary(cat_model_gaussian_known_give$stan_sample_model)$summary))
  expect_null(cat_model_gaussian_known_give$gaussian_variance_alpha)
  expect_null(cat_model_gaussian_known_give$gaussian_variance_beta)
  expect_null(cat_model_gaussian_known_give$stan_data$variance_alpha)
  expect_null(cat_model_gaussian_known_give$stan_data$variance_beta)
  expect_equal(cat_model_gaussian_known_give$stan_data$sigma, 1)

  # Binomial family with known variance
  suppressWarnings(
    expect_warning(
      cat_init_binomial_known_give_variance <- mock_cat_glm_binomial_initialization(
        glm_binomial_data,
        gaussian_known_variance = TRUE,
        custom_variance = 1
      )
    )
  )
  suppressWarnings(
    cat_model_binomial_known_give <- cat_glm_bayes(
      formula = ~.,
      cat_init = cat_init_binomial_known_give_variance,
      chains = chains,
      iter = iter,
      warmup = warmup
    )
  )
  expect_false("sigma" %in% rownames(rstan::summary(cat_model_binomial_known_give$stan_sample_model)$summary))
  expect_null(cat_model_binomial_known_give$gaussian_variance_alpha)
  expect_null(cat_model_binomial_known_give$gaussian_variance_beta)
  expect_null(cat_model_binomial_known_give$stan_data$variance_alpha)
  expect_null(cat_model_binomial_known_give$stan_data$variance_beta)
  expect_null(cat_model_binomial_known_give$stan_data$sigma)

  # Binomial family with unknown variance
  cat_init_binomial_unknown_variance <- mock_cat_glm_binomial_initialization(
    glm_binomial_data,
    gaussian_known_variance = FALSE
  )
  suppressWarnings(
    expect_warning(
      cat_model_binomial_unknown <- cat_glm_bayes(
        formula = ~.,
        cat_init = cat_init_binomial_unknown_variance,
        chains = chains,
        iter = iter,
        warmup = warmup,
        gaussian_variance_alpha = 1,
        gaussian_variance_beta = 1
      )
    )
  )

  expect_false("sigma" %in% rownames(rstan::summary(cat_model_binomial_unknown$stan_sample_model)$summary))
  expect_null(cat_model_binomial_unknown$gaussian_variance_alpha)
  expect_null(cat_model_binomial_unknown$gaussian_variance_beta)
  expect_null(cat_model_binomial_unknown$stan_data$variance_alpha)
  expect_null(cat_model_binomial_unknown$stan_data$variance_beta)
  expect_null(cat_model_binomial_unknown$stan_data$sigma)
})

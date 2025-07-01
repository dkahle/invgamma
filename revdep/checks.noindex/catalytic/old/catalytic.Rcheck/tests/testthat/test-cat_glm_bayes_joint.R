glm_gaussian_data <- mock_glm_gaussian_data()
glm_binomial_data <- mock_glm_binomial_data()
cat_init_gaussian_unknown_variance <- mock_cat_glm_gaussian_initialization(glm_gaussian_data)
cat_init_binomial <- mock_cat_glm_binomial_initialization(glm_binomial_data)

chains <- 1
iter <- 100
warmup <- 50

suppressWarnings(
  cat_model_gaussian_unknown <- cat_glm_bayes_joint(
    formula = ~.,
    cat_init = cat_init_gaussian_unknown_variance,
    chains = chains,
    iter = iter,
    warmup = warmup
  )
)
suppressWarnings(
  cat_model_binomial <- cat_glm_bayes_joint(
    formula = ~.,
    cat_init = cat_init_binomial,
    chains = chains,
    iter = iter,
    warmup = warmup
  )
)
test_that("cat_glm_bayes_joint runs without errors for valid input", {
  expected_components <- c(
    "function_name", "formula", "cat_init", "tau_alpha", "tau_gamma",
    "binomial_tau_lower", "binomial_joint_theta", "binomial_joint_alpha",
    "chains", "iter", "warmup", "algorithm",
    "gaussian_variance_alpha", "gaussian_variance_beta",
    "stan_data", "stan_model", "stan_sample_model", "coefficients", "tau"
  )

  cat_model_gaussian_unknown$cat_init$adj_obs_x <- cat_model_gaussian_unknown$cat_init$adj_syn_x <- cat_model_gaussian_unknown$cat_init$adj_x <- NULL
  cat_model_binomial$cat_init$adj_obs_x <- cat_model_binomial$cat_init$adj_syn_x <- cat_model_binomial$cat_init$adj_x <- NULL

  expect_type(cat_model_gaussian_unknown, "list")
  expect_type(cat_model_binomial, "list")
  expect_equal(cat_model_gaussian_unknown$function_name, "cat_glm_bayes_joint")
  expect_equal(cat_model_binomial$function_name, "cat_glm_bayes_joint")
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

test_that("cat_glm_bayes_joint deal with known or unknown variance for Gaussian famuly properly", {
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
    cat_model_gaussian_known <- cat_glm_bayes_joint(
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
    cat_model_gaussian_known_give <- cat_glm_bayes_joint(
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
  expect_warning(
    cat_init_binomial_known_give_variance <- mock_cat_glm_binomial_initialization(
      glm_binomial_data,
      gaussian_known_variance = TRUE,
      custom_variance = 1
    )
  )
  suppressWarnings(
    cat_model_binomial_known_give <- cat_glm_bayes_joint(
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
      cat_model_binomial_unknown <- cat_glm_bayes_joint(
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

test_that("cat_glm_bayes_joint deal with binomial related augments for Binomial famuly properly", {
  # Test for binomial_tau_lower when family is binomial
  expect_equal(cat_model_binomial$binomial_tau_lower, 0.05)
  expect_equal(cat_model_binomial$binomial_tau_lower, cat_model_binomial$stan_data$tau_lower)
  suppressWarnings(
    cat_model_binomial_2 <- cat_glm_bayes_joint(
      formula = ~.,
      cat_init = cat_init_binomial,
      chains = chains,
      iter = iter,
      warmup = warmup,
      binomial_tau_lower = 1
    )
  )
  expect_equal(cat_model_binomial_2$binomial_tau_lower, 1)
  expect_equal(cat_model_binomial_2$binomial_tau_lower, cat_model_binomial_2$stan_data$tau_lower)

  # Test for binomial_tau_lower when family is gaussian
  suppressWarnings(
    cat_model_gau <- cat_glm_bayes_joint(
      formula = ~.,
      cat_init = cat_init_gaussian_unknown_variance,
      chains = chains,
      iter = iter,
      warmup = warmup,
      binomial_tau_lower = 1
    )
  )
  expect_null(cat_model_gau$stan_data$tau_lower)

  # Test for binomial_joint_theta when family is binomial
  suppressWarnings(
    cat_model_binomial_theta <- cat_glm_bayes_joint(
      formula = ~.,
      cat_init = cat_init_binomial,
      chains = chains,
      iter = iter,
      warmup = warmup,
      binomial_joint_theta = TRUE
    )
  )
  expect_contains(rownames(rstan::summary(cat_model_binomial_theta$stan_sample_model)$summary), "theta")
  expect_false("tau" %in% rownames(rstan::summary(cat_model_binomial_theta$stan_sample_model)$summary))
  expect_equal(rstan::summary(cat_model_binomial_theta$stan_sample_model)$summary["theta", "mean"], 1 / (cat_model_binomial_theta$tau))
  expect_true(cat_model_binomial_theta$binomial_joint_theta)

  # Test for binomial_joint_theta when family is gaussian
  expect_warning(
    cat_model_gaussian_theta <- cat_glm_bayes_joint(
      formula = ~.,
      cat_init = cat_init_gaussian_unknown_variance,
      chains = chains,
      iter = iter,
      warmup = warmup,
      binomial_joint_theta = TRUE
    )
  )

  expect_contains(rownames(rstan::summary(cat_model_gaussian_theta$stan_sample_model)$summary), "tau")
  expect_false("theta" %in% rownames(rstan::summary(cat_model_gaussian_theta$stan_sample_model)$summary))
  expect_false(cat_model_gaussian_theta$binomial_joint_theta)

  # Test for binomial_joint_alpha when family is binomial
  suppressWarnings(
    cat_model_binomial_theta_alpha <- cat_glm_bayes_joint(
      formula = ~.,
      cat_init = cat_init_binomial,
      chains = chains,
      iter = iter,
      warmup = warmup,
      binomial_joint_theta = TRUE,
      binomial_joint_alpha = TRUE
    )
  )
  expect_contains(rownames(rstan::summary(cat_model_binomial_theta_alpha$stan_sample_model)$summary), "theta")
  expect_contains(rownames(rstan::summary(cat_model_binomial_theta_alpha$stan_sample_model)$summary), "tau_alpha")
  expect_equal(rstan::summary(cat_model_binomial_theta_alpha$stan_sample_model)$summary["theta", "mean"], 1 / (cat_model_binomial_theta_alpha$tau))
  expect_false("tau" %in% rownames(rstan::summary(cat_model_binomial_theta_alpha$stan_sample_model)$summary))
  expect_true(cat_model_binomial_theta_alpha$binomial_joint_theta)
  expect_true(cat_model_binomial_theta_alpha$binomial_joint_alpha)

  # Test for binomial_joint_alpha when family is binomial with binomial_joint_theta = FALSE
  suppressWarnings(
    expect_warning(
      cat_model_binomial_alpha <- cat_glm_bayes_joint(
        formula = ~.,
        cat_init = cat_init_binomial,
        chains = chains,
        iter = iter,
        warmup = warmup,
        binomial_joint_theta = FALSE,
        binomial_joint_alpha = TRUE
      )
    )
  )

  expect_contains(rownames(rstan::summary(cat_model_binomial_alpha$stan_sample_model)$summary), "tau")
  expect_false("tau_alpha" %in% rownames(rstan::summary(cat_model_binomial_alpha$stan_sample_model)$summary))
  expect_false("theta" %in% rownames(rstan::summary(cat_model_binomial_alpha$stan_sample_model)$summary))
  expect_false(cat_model_binomial_alpha$binomial_joint_theta)
  expect_true(cat_model_binomial_alpha$binomial_joint_alpha)

  # Test for binomial_joint_alpha when family is gaussian
  expect_warning(
    cat_model_gaussian_theta_alpha <- cat_glm_bayes_joint(
      formula = ~.,
      cat_init = cat_init_gaussian_unknown_variance,
      chains = chains,
      iter = iter,
      warmup = warmup,
      binomial_joint_theta = TRUE,
      binomial_joint_alpha = TRUE
    )
  )

  expect_contains(rownames(rstan::summary(cat_model_gaussian_theta_alpha$stan_sample_model)$summary), "tau")
  expect_false("tau_alpha" %in% rownames(rstan::summary(cat_model_gaussian_theta_alpha$stan_sample_model)$summary))
  expect_false("theta" %in% rownames(rstan::summary(cat_model_gaussian_theta_alpha$stan_sample_model)$summary))
  expect_false(cat_model_gaussian_theta_alpha$binomial_joint_theta)
  expect_false(cat_model_gaussian_theta_alpha$binomial_joint_alpha)
})

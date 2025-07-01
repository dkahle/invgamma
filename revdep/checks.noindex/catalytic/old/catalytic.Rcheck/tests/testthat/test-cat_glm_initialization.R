glm_gaussian_data <- mock_glm_gaussian_data(seed = 1)
glm_binomial_data <- mock_glm_binomial_data(seed = 1)
cat_init_gaussian <- mock_cat_glm_gaussian_initialization(glm_gaussian_data)
cat_init_binomial <- mock_cat_glm_binomial_initialization(glm_binomial_data)

test_that("cat_glm_initialization returns a list with expected structure and contents", {
  expected_components <- c(
    "function_name", "formula", "custom_variance", "gaussian_known_variance",
    "x_degree", "resample_only", "na_replace", "y_col_name",
    "simple_model", "obs_size", "obs_data", "obs_x", "obs_y",
    "syn_size", "syn_data", "syn_x", "syn_y", "syn_x_resample_inform",
    "size", "data", "x", "y"
  )

  expect_type(cat_init_gaussian, "list")
  expect_type(cat_init_binomial, "list")
  expect_equal(cat_init_gaussian$function_name, "cat_glm_initialization")
  expect_equal(cat_init_binomial$function_name, "cat_glm_initialization")
  expect_true(all(expected_components %in% names(cat_init_gaussian)))
  expect_true(all(expected_components %in% names(cat_init_binomial)))
})


test_that("Observation data has same size and structuer", {
  expect_equal(cat_init_gaussian$obs_size, nrow(glm_gaussian_data))
  expect_equal(cat_init_gaussian$family, "gaussian")
  expect_equal(nrow(cat_init_gaussian$obs_data), cat_init_gaussian$obs_size)
  expect_equal(nrow(cat_init_gaussian$obs_x), cat_init_gaussian$obs_size)
  expect_equal(length(cat_init_gaussian$obs_y), cat_init_gaussian$obs_size)
  expect_true(all(colnames(cat_init_gaussian$obs_x) %in% c("X1", "X2", "X3")))
  expect_true(all(cat_init_gaussian$obs_x == glm_gaussian_data[, c("X1", "X2", "X3")]))
  expect_true(all(cat_init_gaussian$obs_y == glm_gaussian_data$Y))

  expect_equal(cat_init_binomial$obs_size, nrow(glm_binomial_data))
  expect_equal(cat_init_binomial$family, "binomial")
  expect_equal(nrow(cat_init_binomial$obs_data), cat_init_binomial$obs_size)
  expect_equal(nrow(cat_init_binomial$obs_x), cat_init_binomial$obs_size)
  expect_equal(length(cat_init_binomial$obs_y), cat_init_binomial$obs_size)
  expect_true(all(colnames(cat_init_binomial$obs_x) %in% c("X1", "X2", "X3")))
  expect_true(all(cat_init_binomial$obs_x == glm_binomial_data[, c("X1", "X2", "X3")]))
  expect_true(all(cat_init_binomial$obs_y == glm_binomial_data$Y))
})

test_that("Synthetic data has correct size and structure", {
  expect_equal(cat_init_gaussian$syn_size, 100)
  expect_equal(nrow(cat_init_gaussian$syn_data), cat_init_gaussian$syn_size)
  expect_equal(nrow(cat_init_gaussian$syn_x), cat_init_gaussian$syn_size)
  expect_equal(length(cat_init_gaussian$syn_y), cat_init_gaussian$syn_size)

  expect_equal(cat_init_binomial$syn_size, 100)
  expect_equal(nrow(cat_init_binomial$syn_data), cat_init_binomial$syn_size)
  expect_equal(nrow(cat_init_binomial$syn_x), cat_init_binomial$syn_size)
  expect_equal(length(cat_init_binomial$syn_y), cat_init_binomial$syn_size)
})

test_that("Whole data has correct size and structure", {
  expect_equal(cat_init_gaussian$size, cat_init_gaussian$obs_size + cat_init_gaussian$syn_size)
  expect_equal(nrow(cat_init_gaussian$data), cat_init_gaussian$size)
  expect_equal(nrow(cat_init_gaussian$x), cat_init_gaussian$size)
  expect_equal(length(cat_init_gaussian$y), cat_init_gaussian$size)
  expect_true(all(colnames(cat_init_gaussian$x) %in% c("X1", "X2", "X3")))
  expect_equal(cat_init_gaussian$y_col_name, "Y")

  expect_equal(cat_init_binomial$size, cat_init_binomial$obs_size + cat_init_binomial$syn_size)
  expect_equal(nrow(cat_init_binomial$data), cat_init_binomial$size)
  expect_equal(nrow(cat_init_binomial$x), cat_init_binomial$size)
  expect_equal(length(cat_init_binomial$y), cat_init_binomial$size)
  expect_true(all(colnames(cat_init_binomial$x) %in% c("X1", "X2", "X3")))
  expect_equal(cat_init_binomial$y_col_name, "Y")
})

test_that("gaussian_known_variance and custom_variance will be handled properly", {
  # Test for binomial family
  expect_warning(
    cat_init_bin <- mock_cat_glm_binomial_initialization(
      glm_binomial_data,
      gaussian_known_variance = TRUE,
      custom_variance = 1
    )
  )
  expect_equal(cat_init_bin$custom_variance, NULL)
  expect_false(cat_init_bin$gaussian_known_variance)

  # Test for gaussian_known_variance=FALSE and give value to custom_variance
  expect_warning(
    cat_init_gau <- mock_cat_glm_gaussian_initialization(
      glm_gaussian_data,
      gaussian_known_variance = FALSE,
      custom_variance = 1
    )
  )
  expect_null(cat_init_gau$custom_variance)
  expect_false(cat_init_gau$gaussian_known_variance)

  # Test for gaussian_known_variance=TRUE and give value to custom_variance
  cat_init_gau_2 <- mock_cat_glm_gaussian_initialization(glm_gaussian_data,
    gaussian_known_variance = TRUE,
    custom_variance = 1
  )
  expect_equal(cat_init_gau_2$custom_variance, 1)
  expect_true(cat_init_gau_2$gaussian_known_variance)

  # Test for gaussian_known_variance=TRUE and not give value to custom_variance
  cat_init_gau_3 <- mock_cat_glm_gaussian_initialization(glm_gaussian_data,
    gaussian_known_variance = TRUE,
    custom_variance = NULL
  )
  expect_null(cat_init_gau_3$custom_variance)
  expect_true(cat_init_gau_3$gaussian_known_variance)
})

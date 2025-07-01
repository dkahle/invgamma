cox_data <- mock_cox_data(seed = 1)
cat_init <- mock_cat_cox_initialization(cox_data)

test_that("cat_cox_initialization returns a list with expected structure and contents", {
  expected_components <- c(
    "function_name", "formula", "hazard_constant", "entry_points",
    "x_degree", "resample_only", "na_replace", "time_col_name", "status_col_name",
    "simple_model", "obs_size", "obs_data", "obs_x", "obs_time",
    "obs_status", "syn_size", "syn_data", "syn_x", "syn_time", "syn_status",
    "syn_x_resample_inform", "size", "data", "x", "time", "status"
  )

  expect_type(cat_init, "list")
  expect_equal(cat_init$function_name, "cat_cox_initialization")
  expect_true(all(expected_components %in% names(cat_init)))
})


test_that("Observation data has same size and structuer", {
  expect_equal(cat_init$obs_size, nrow(cox_data))
  expect_equal(nrow(cat_init$obs_data), cat_init$obs_size)
  expect_equal(nrow(cat_init$obs_x), cat_init$obs_size)
  expect_equal(length(cat_init$obs_time), cat_init$obs_size)
  expect_equal(length(cat_init$obs_status), cat_init$obs_size)
  expect_true(all(colnames(cat_init$obs_x) %in% c("X1", "X2", "X3")))
  expect_true(all(cat_init$obs_x == cox_data[, c("X1", "X2", "X3")]))
  expect_true(all(cat_init$obs_time == cox_data$time))
  expect_true(all(cat_init$obs_status == cox_data$status))
})

test_that("Synthetic data has correct size and structure", {
  expect_equal(cat_init$syn_size, 100)
  expect_equal(nrow(cat_init$syn_data), cat_init$syn_size)
  expect_equal(nrow(cat_init$syn_x), cat_init$syn_size)
  expect_equal(length(cat_init$syn_time), cat_init$syn_size)
  expect_equal(length(cat_init$syn_status), cat_init$syn_size)
})

test_that("Whole data has correct size and structure", {
  expect_equal(cat_init$size, cat_init$obs_size + cat_init$syn_size)
  expect_equal(nrow(cat_init$data), cat_init$size)
  expect_equal(nrow(cat_init$x), cat_init$size)
  expect_equal(length(cat_init$time), cat_init$size)
  expect_equal(length(cat_init$status), cat_init$size)
  expect_true(all(colnames(cat_init$x) %in% c("X1", "X2", "X3")))
  expect_equal(cat_init$time_col_name, "time")
  expect_equal(cat_init$status_col_name, "status")
})

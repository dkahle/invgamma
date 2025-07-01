test_that("dimensions-rmvss", {
  expect_equal(dim(rmvss(10, 1.2, Q=diag(2))), dim(matrix(NA,10,2)))
})

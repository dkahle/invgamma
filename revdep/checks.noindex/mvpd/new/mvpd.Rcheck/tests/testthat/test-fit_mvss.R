test_that("alpha_lt_0-fit_mvss", {
  ## create a 4x4 shape matrix symMat
  S <- matrix(rnorm(4*4, mean=2, sd=4),4); 
  symMat <- as.matrix(Matrix::nearPD(0.5 * (S + t(S)))$mat)
  symMat
  ## generate 100 r.v.'s from 4-dimensional mvss
  X <- mvpd::rmvss(1e2, alpha=1.5, Q=symMat, delta=c(1,2,3,4))
  ## use fit_mvss to recover the parameters, compare to symMat
  fmv <- mvpd::fit_mvss(X)
  fmv
  
  expect_identical(any(fmv$univ_alphas < 0), FALSE)
})

test_that("alpha_ge_2-fit_mvss", {
  ## create a 4x4 shape matrix symMat
  S <- matrix(rnorm(4*4, mean=2, sd=4),4); 
  symMat <- as.matrix(Matrix::nearPD(0.5 * (S + t(S)))$mat)
  symMat
  ## generate 100 r.v.'s from 4-dimensional mvss
  X <- mvpd::rmvss(1e2, alpha=1.5, Q=symMat, delta=c(1,2,3,4))
  ## use fit_mvss to recover the parameters, compare to symMat
  fmv <- mvpd::fit_mvss(X)
  fmv
  
  expect_identical(any(fmv$univ_alphas >= 2), FALSE)
})

cprop_beta = 2.4

Y_0 <-  c(7, 25, 51, 13, 64, 11)
I_0 <- c(1 ,1 ,0, 1, 1, 1)
X_0 <- matrix(stats::rnorm(length(Y_0)*2, 1, 0.5), ncol = 2)
Y <-  c(9, 13, 13, 18, 23)
I <- c(1 ,1 ,0, 1, 1)
X <- matrix(stats::rbinom(length(Y), 1, 0.5))
#s_r <- c(10, 15, 35) # one split point larger than Y_max, could this occur?
s_r <- c(10, 15)
s <- c(0, s_r, max(c(Y_0,Y)))
J <- 2

# without covariates, lambda_0
bp_0 <- ncol(X_0)
lambda_0 <- c(1.5, 1.4, 1.0)
beta_0 <- c(2, 5)

bp <- ncol(X)
lambda <- c(1.5, 1.4, 1.0)
beta <- 4

beta_new <- beta
beta_0_new <- beta_0

beta_new <- stats::rnorm(1, beta[1], cprop_beta)
beta_0_new[1] <- stats::rnorm(1, beta_0[1], cprop_beta)
beta_0_new[2] <- stats::rnorm(1, beta_0[2], cprop_beta)

df_hist <- .dataframe_fun(Y_0, I_0, X_0, s, lambda_0, bp = bp_0, J)
df <- .dataframe_fun(Y, I, X, s, lambda, bp = bp, J) 

test_that("loglikelihood ratio for both beta/beta_0 works ", {

  expect_no_error(llikeli <- .llikelihood_ratio_beta(df, beta, beta_new))
  expect_no_error(llikeli_0 <- .llikelihood_ratio_beta(df_hist, beta_0, beta_0_new))
  expect_true(!is.na(llikeli))
  expect_true(!is.na(llikeli_0))
  
  X <- as.matrix(df[, substr(colnames(df), 1, 1) == "X"])
  xdpb <- X %*% beta
  xdpb_new <- X %*% beta_new
  
  llikelihood_ratio <- sum((xdpb_new  - xdpb) * df$I - ((df$Y - df$tstart) * df$lambda) * (exp(xdpb_new) - exp(xdpb)))
  
  expect_true(!is.na(llikelihood_ratio))
  expect_equal(llikelihood_ratio, llikeli)
  })


test_that("First and second derivative for proposal correct", {
  out_mom <- .beta_mom.NR.fun(df, 1, beta, bp, cprop_beta)
  expect_lt(out_mom$D1, 0)
  expect_lt(out_mom$D2, 0)
})


test_that("Beta Newton Raphson updates are correct", {
 
  set.seed(56)
  out_NR <- .beta_MH_NR(df, beta, bp, cprop_beta, beta_count = 0)
  new_beta <- out_NR$beta
  expect_true(all(!is.na(new_beta)))
  
  beta_count <- beta * 0
  
  set.seed(56)
  for(k in 1:bp){
    
    beta_new <- beta
    all_mom <- .beta_mom.NR.fun(df, k, beta, bp, cprop_beta)
    beta_prop <- stats::rnorm(n = 1, mean =all_mom$mu, sd = sqrt(all_mom$var))
    beta_new[k] <- beta_prop
      
    logacc <- .llikelihood_ratio_beta(df, beta, beta_new)
      
    if(logacc > log(stats::runif(1))){
      beta[k] <- beta_prop
      beta_count[k] <- beta_count[k] + 1
    }
      
  }
  
  expect_equal(beta, new_beta)
  
  # check acceptance ratio
  beta_count <- 0
  for (ii in 1:100) {
    new_beta <- .beta_MH_NR(df, beta, bp, cprop_beta, beta_count)
    beta_count <- new_beta$beta_count
  }
  expect_gt(beta_count, 20)
  
})


test_that(".lprop.dens.beta.NR are correct", {
  mu_prop <- 1
  set.seed(117327)
  beta_prop <- stats::rnorm(n = 1, mean = mu_prop, sd = cprop_beta)
  
  # Negative (log density)
  expect_true(!is.na(.lprop.dens.beta.NR(beta_prop, mu_prop, cprop_beta)))
  expect_lt(.lprop.dens.beta.NR(beta_prop, mu_prop, cprop_beta), 0)
  l1 <- (-1 / (2 * cprop_beta**2)) * (beta_prop - mu_prop)**2
  set.seed(117327)
  expect_equal(l1, .lprop.dens.beta.NR(beta_prop, mu_prop, cprop_beta))
})











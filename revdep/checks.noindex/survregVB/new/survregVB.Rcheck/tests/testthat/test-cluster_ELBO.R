cluster <- rep(1:15, each = 5)

set.seed(1)
x1 <- rnorm(75, 1, 0.2)
x2 <- rbinom(75, 1, 0.5)
epsilon <- rlogis(75)
cen.time.15 <- runif(75, 0, 48)

beta0 <- 0.5
beta1 <- 0.2
beta2 <- 0.8
b <- 0.8

Y <- numeric(50)
random.int <- rnorm(15, 0, 1)
for (l in 1:75) {
  Y[l] <- beta0 + beta1 * x1[l] + beta2 * x2[l] +
    random.int[(l - 1) %/% 5 + 1] + b * epsilon[l]
}
Time <- exp(Y)

# obtain observed time
Time.15 <- pmin(Time, cen.time.15)

# obtain censoring indicator
delta <- rep(1, 75)
delta.15 <- ifelse(Time == Time.15, 1, 0)

# prepare for the data structure
X <- matrix(0, nrow = 75, ncol = 3)
X[, 1] <- 1
X[, 2] <- x1
X[, 3] <- x2

# priors, informative priors
mu_0 <- c(0, 0, 0)
v_0 <- 0.1
alpha_0 <- 3
omega_0 <- 2
lambda_0 <- 3
eta_0 <- 2
tau_0 <- rep(0, 15)

expectation_b <- 0.0322580645
expectation_b.15 <- 0.035971223

alpha <- 311
alpha.15 <- 279

omega <- 450.90591
omega.15 <- 413.84352

# prior values
lambda <- 10.5

Sigma <- matrix(
  c(
    0.03803940, -0.02931614, -0.00766775,
    -0.02931614, 0.02263954, 0.00586149,
    -0.00766775, 0.00586149, 0.00167289
  ),
  nrow = 3, ncol = 3
)
Sigma.15 <- matrix(
  c(
    0.00551067, -0.00437538, -0.00097776,
    -0.00437538, 0.00352800, 0.00072036,
    -0.00097776, 0.00072036, 0.00032793
  ),
  nrow = 3, ncol = 3
)

mu <- matrix(c(0.073256987, -0.056103547, -0.01404328),
  nrow = 1, ncol = 3
)
mu.15 <- matrix(c(0.0090210506, -0.0067687018, -0.00068550822),
  nrow = 1, ncol = 3
)

sigma_squared <- c(
  0.666666667, 0.666666667, 0.666666667, 0.666666667, 0.666666667,
  0.666666667, 0.582328837, 0.666666667, 0.666666667, 0.666666667,
  0.651007817, 0.666666667, 0.666666667, 0.570345677, 0.650926922
)
sigma_squared.15 <- c(
  0.666666667, 0.666666667, 0.666666667, 0.666666667, 0.666666667,
  0.666666667, 0.585606614, 0.666666667, 0.666666667, 0.666666667,
  0.651685066, 0.666666667, 0.666666667, 0.574015078, 0.651607589
)

tau <- c(
  1.3794452158, 2.2990753596, 2.2990753596, 2.2990753596, 2.2990753596,
  2.2990753596, 1.6104826150, 2.2990753596, -0.4598150719, -0.4598150719,
  -0.2986342161, -0.4598150719, -2.2990753596, -0.6511100601, 2.0959758050
)
tau.15 <- c(
  1.3483357195, 2.2472261991, 2.2472261991, 2.2472261991, 2.2472261991,
  2.2472261991, 1.5831199533, 2.2472261991, -0.4494452398, -0.4494452398,
  -0.2922021787, -0.4494452398, -2.2472261991, -0.6406946852, 2.0512668712
)

eta <- 30.4126147
eta.15 <- 29.3901377

test_that("test diff_gamma", {
  expect_equal(
    diff_gamma(tau, sigma_squared, lambda, eta, cluster),
    -14.93795024
  )
  expect_equal(
    diff_gamma(tau.15, sigma_squared.15, lambda, eta.15, cluster),
    -14.6644959
  )
})

test_that("test diff_sigma_gamma", {
  expect_equal(diff_sigma_gamma(lambda_0, eta_0, lambda, eta), -17.707583)
  expect_equal(diff_sigma_gamma(lambda_0, eta_0, lambda, eta.15), -17.62901066)
})

test_that("test elbo_cluster", {
  expect_equal(
    elbo_cluster(
      Y, X, delta, alpha_0, omega_0, mu_0, v_0, lambda_0,
      eta_0, alpha, omega, mu, Sigma, tau, sigma_squared, lambda,
      eta, expectation_b, cluster
    ),
    -1607.13831
  )
  expect_equal(
    elbo_cluster(
      log(Time.15), X, delta, alpha_0, omega_0, mu_0, v_0, lambda_0,
      eta_0, alpha.15, omega.15, mu.15, Sigma.15,
      tau.15, sigma_squared.15, lambda, eta.15,
      expectation_b.15, cluster
    ),
    -1420.022322
  )
})

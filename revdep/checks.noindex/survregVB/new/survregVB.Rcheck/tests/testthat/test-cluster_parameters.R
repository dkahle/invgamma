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

# expected values

Sigma <- matrix(
  c(
    0.0380393981, -0.0293161433, -0.0076677526,
    -0.0293161434, 0.0226395403, 0.0058614865,
    -0.0076677526, 0.0058614865, 0.0016728902
  ),
  nrow = 3, byrow = TRUE
)
Sigma.15 <- matrix(
  c(
    0.0055106702, -0.0043753797, -0.0009777582,
    -0.0043753797, 0.0035279998, 0.0007203575,
    -0.0009777582, 0.0007203575, 0.0003279276
  ),
  nrow = 3, ncol = 3
)

mu <- matrix(c(0.073256833, -0.056103662, -0.014043412),
  nrow = 1, ncol = 3
)
mu.15 <- matrix(c(0.009021086, -0.006768734, -0.0006855568),
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

test_that("test Sigma_star_cluster", {
  expect_equal(
    Sigma_star_cluster(
      Y, X, delta, v_0, alpha, omega_0, mu_0, tau_0,
      expectation_b, cluster
    ),
    Sigma
  )
  expect_equal(
    Sigma_star_cluster(
      log(Time.15), X, delta, v_0, alpha.15, omega_0, mu_0, tau_0,
      expectation_b.15, cluster
    ),
    Sigma.15
  )
})

test_that("test mu_star_cluster", {
  expect_equal(
    mu_star_cluster(
      Y, X, delta, mu_0, v_0, alpha, omega, mu_0, Sigma,
      tau_0, expectation_b, cluster
    ),
    mu
  )
  expect_equal(
    mu_star_cluster(
      log(Time.15), X, delta, mu_0, v_0, alpha.15, omega.15,
      mu_0, Sigma.15, tau_0, expectation_b.15, cluster
    ),
    mu.15
  )
})

test_that("test sigma_star_cluster", {
  expect_equal(
    sigma_squared_star(
      Y, X, delta, alpha, omega, mu, tau_0,
      lambda_0, eta_0, expectation_b, cluster
    ),
    sigma_squared
  )
  expect_equal(
    sigma_squared_star(
      log(Time.15), X, delta, alpha.15, omega.15,
      mu.15, tau_0, lambda_0, eta_0, expectation_b.15,
      cluster
    ),
    sigma_squared.15
  )
})

test_that("test tau_star_cluster", {
  expect_equal(
    tau_star(
      Y, X, delta, alpha, omega, mu, tau_0, sigma_squared,
      expectation_b, cluster
    ),
    tau
  )
  expect_equal(
    tau_star(
      log(Time.15), X, delta, alpha.15, omega.15, mu.15, tau_0,
      sigma_squared.15, expectation_b.15, cluster
    ),
    tau.15
  )
})

test_that("test omega_star_cluster", {
  expect_equal(
    omega_star_cluster(
      Y, X, delta, omega_0, mu, tau,
      expectation_b, cluster
    ),
    54.3301043
  )
  expect_equal(
    omega_star_cluster(
      log(Time.15), X, delta, omega_0, mu.15, tau.15,
      expectation_b.15, cluster
    ),
    37.8544658
  )
})

test_that("test eta_star_cluster", {
  expect_equal(eta_star(eta_0, tau, sigma_squared), 30.4126147)
  expect_equal(eta_star(eta_0, tau.15, sigma_squared.15), 29.3901377)
})

test_that("test lambda_star", {
  expect_equal(lambda_star(20, 3), 21.5)
  expect_equal(lambda_star(103, 7), 106.5)
})

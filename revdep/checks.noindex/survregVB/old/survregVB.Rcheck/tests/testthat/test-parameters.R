set.seed(1)
x1 <- rnorm(300, 1, 0.2)
x2 <- rbinom(300, 1, 0.5)
z <- rlogis(300)
beta0 <- 0.5
beta1 <- 0.2
beta2 <- 0.8
b <- 0.8
y <- beta0 + beta1 * x1 + beta2 * x2 + b * z
Time <- exp(y)

# generate censoring times
set.seed(1)
cen.time.10 <- runif(300, 0, 48)
cen.time.30 <- runif(300, 0, 17)

# obtain observed time
Time.10 <- pmin(Time, cen.time.10)
Time.30 <- pmin(Time, cen.time.30)

# obtain censoring indicator
delta <- rep(1, 300)
delta.10 <- ifelse(Time == Time.10, 1, 0)
delta.30 <- ifelse(Time == Time.30, 1, 0)

# create X matrix
X <- matrix(c(rep(1, 300), x1, x2), nrow = 300)

# priors, use non-informative priors
mu_0 <- c(0, 0, 0)
v_0 <- 0.1
alpha_0 <- 11
omega_0 <- 10

# expected values
expectation_b <- 0.0322580645
expectation_b.10 <- 0.035971223
expectation_b.30 <- 0.0456621

alpha <- 311
alpha.10 <- 279
alpha.30 <- 220

omega <- 450.90591
omega.10 <- 413.84352
omega.30 <- 353.663491

mu <- matrix(c(-20.62301047, 44.3987726, 95.92464991),
  nrow = 1, ncol = 3, byrow = TRUE
)
mu.10 <- matrix(c(-21.7328379, 42.5781894, 93.9974693),
  nrow = 1, ncol = 3, byrow = TRUE
)
mu.30 <- matrix(c(-18.3334498, 33.0593043, 77.6065367),
  nrow = 1, ncol = 3, byrow = TRUE
)

Sigma <- matrix(
  c(
    4.27891521, -3.68461398, -1.01811326,
    -3.68461398, 3.66446482, 0.52146029,
    -1.01811326, 0.52146029, 1.89703518
  ),
  nrow = 3, ncol = 3, byrow = TRUE
)
Sigma.10 <- matrix(
  c(
    4.1924680, -3.6686962, -0.9334673,
    -3.6686962, 3.6716640, 0.4800017,
    -0.9334673, 0.4800017, 1.8374859
  ),
  nrow = 3, ncol = 3, byrow = TRUE
)
Sigma.30 <- matrix(
  c(
    3.8369890, -3.46719200, -0.7512178,
    -3.4671920, 3.50225856, 0.40271576,
    -0.7512178, 0.40271576, 1.6385013
  ),
  nrow = 3, ncol = 3, byrow = TRUE
)

test_that("alpha_star", {
  expect_equal(alpha_star(alpha_0, delta), alpha)
  expect_equal(alpha_star(alpha_0, delta.10), alpha.10)
  expect_equal(alpha_star(alpha_0, delta.30), alpha.30)
})

test_that("omega_star", {
  expect_equal(
    omega_star(log(Time), X, delta, omega_0, mu_0, expectation_b),
    omega
  )
  expect_equal(
    omega_star(log(Time.10), X, delta.10, omega_0, mu_0, expectation_b.10),
    omega.10
  )
  expect_equal(
    omega_star(log(Time.30), X, delta.30, omega_0, mu_0, expectation_b.30),
    omega.30
  )
})

test_that("mu_star", {
  expect_equal(
    mu_star(
      log(Time), X, delta, mu_0, v_0, alpha, omega, mu_0, Sigma, expectation_b
    ),
    mu
  )
  expect_equal(
    mu_star(
      log(Time.10), X, delta.10, mu_0, v_0, alpha.10, omega.10, mu_0, Sigma.10,
      expectation_b.10
    ),
    mu.10
  )
  expect_equal(
    mu_star(
      log(Time.30), X, delta.30, mu_0, v_0, alpha.30, omega.30, mu_0, Sigma.30,
      expectation_b.30
    ),
    mu.30
  )
})

test_that("sigma_star", {
  expect_equal(
    Sigma_star(
      log(Time), X, delta, v_0, alpha, omega, mu_0, expectation_b
    ),
    Sigma
  )
  expect_equal(
    Sigma_star(
      log(Time.10), X, delta.10, v_0, alpha.10, omega.10, mu_0, expectation_b.10
    ),
    Sigma.10
  )
  expect_equal(
    Sigma_star(
      log(Time.30), X, delta.30, v_0, alpha.30, omega.30, mu_0, expectation_b.30
    ),
    Sigma.30
  )
})

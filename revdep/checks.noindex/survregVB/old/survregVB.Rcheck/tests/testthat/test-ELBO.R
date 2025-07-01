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

# expected values
expectation_log_likelihood <- -14671.7188
expectation_log_likelihood.10 <- -12002.9505
expectation_log_likelihood.30 <- -6508.3185

diff_beta <- -579.85756
diff_beta.10 <- -556.03005
diff_beta.30 <- -372.73973

diff_b <- -1484.57637
diff_b.10 <- -1302.70623
diff_b.30 <- -977.5652

elbo <- -16736.1527
elbo.10 <- -13861.6868
elbo.30 <- -7858.6235

test_that("expectation_log_likelihood", {
  expect_equal(
    expectation_log_likelihood(
      log(Time), X, delta, alpha, omega, mu, expectation_b
    ),
    expectation_log_likelihood
  )
  expect_equal(
    expectation_log_likelihood(
      log(Time.10), X, delta.10, alpha.10, omega.10, mu.10, expectation_b.10
    ),
    expectation_log_likelihood.10
  )
  expect_equal(
    expectation_log_likelihood(
      log(Time.30), X, delta.30, alpha.30, omega.30, mu.30, expectation_b.30
    ),
    expectation_log_likelihood.30
  )
})

test_that("diff_beta", {
  expect_equal(diff_beta(mu_0, v_0, mu, Sigma), diff_beta)
  expect_equal(diff_beta(mu_0, v_0, mu.10, Sigma.10), diff_beta.10)
  expect_equal(diff_beta(mu_0, v_0, mu.30, Sigma.30), diff_beta.30)
})

test_that("diff_b", {
  expect_equal(diff_b(alpha_0, omega_0, alpha, omega), diff_b)
  expect_equal(diff_b(alpha_0, omega_0, alpha.10, omega.10), diff_b.10)
  expect_equal(diff_b(alpha_0, omega_0, alpha.30, omega.30), diff_b.30)
})

test_that("elbo", {
  expected <- elbo(
    log(Time), X, delta, alpha_0, omega_0, mu_0, v_0, alpha,
    omega, mu, Sigma, expectation_b
  )
  result <- -16736.1527
  expect_equal(expected, result)

  expect_equal(
    elbo(
      log(Time.10), X, delta.10, alpha_0, omega_0, mu_0, v_0, alpha.10,
      omega.10, mu.10, Sigma.10, expectation_b.10
    ),
    elbo.10
  )
  expect_equal(
    elbo(
      log(Time.30), X, delta.30, alpha_0, omega_0, mu_0, v_0, alpha.30,
      omega.30, mu.30, Sigma.30, expectation_b.30
    ),
    elbo.30
  )
})

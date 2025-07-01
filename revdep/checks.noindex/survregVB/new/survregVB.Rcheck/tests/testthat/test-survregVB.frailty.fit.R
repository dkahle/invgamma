library(survival)

# prepare for the data structure
X <- matrix(c(rep(1, 75), simulation_frailty$x1,
              simulation_frailty$x2), nrow = 75)

# create Surv objects
Y <- Surv(simulation_frailty$Time, simulation_frailty$delta)
Y.15 <- Surv(simulation_frailty$Time.15, simulation_frailty$delta.15)

# get clusters
cluster <- simulation_frailty$cluster

# priors, informative priors
mu_0 <- c(0, 0, 0)
v_0 <- 0.1
alpha_0 <- 3
omega_0 <- 2
lambda_0 <- 3
eta_0 <- 2

test_that("survregVB.frailty.fit", {
  result <- survregVB.frailty.fit(
    Y = Y, X = X, alpha_0 = alpha_0,
    omega_0 = omega_0, mu_0 = mu_0, v_0 = v_0,
    lambda_0 = lambda_0, eta_0 = eta_0,
    cluster = cluster, max_iteration = 100,
    threshold = 0.01
  )
  expected <- list(
    clustered = TRUE,
    ELBO = -312.4124906,
    alpha = 78,
    omega = 52.37919667,
    mu = c(-0.29243491, 0.80833985, 0.55749383),
    Sigma = matrix(
      c(
        0.50246837, -0.456473457, -0.038855380,
        -0.45647346, 0.445258447, 0.004346971,
        -0.03885538, 0.004346971, 0.062897163
      ),
      nrow = 3, byrow = TRUE,
      dimnames = list(NULL, NULL)
    ),
    tau = setNames(
      c(
        -0.2077060470, 0.7037876292, 0.4342901316, 0.1274945678, 1.6122896798,
        0.2853256227, 0.7071727654, 1.8326163036, -1.1214338058, -0.7639845143,
        -0.5222271141, -0.6751576177, -1.5857748947, -0.7926863618, 0.4216802745
      ),
      as.character(1:15)
    ),
    sigma = setNames(
      c(
        0.2246220256, 0.1624513933, 0.1624513933, 0.1885279119, 0.1885279119,
        0.1885279119, 0.2246220256, 0.1885279119, 0.1885598395, 0.1624513933,
        0.1624513933, 0.2778092651, 0.1624513933, 0.1885279119, 0.2335261631
      ),
      as.character(1:15)
    ),
    lambda = 10.5,
    eta = 10.0387575,
    iterations = 13,
    n = 75
  )

  expect_equal(result, expected)

  result <- survregVB.frailty.fit(
    Y = Y.15, X = X, alpha_0 = alpha_0,
    omega_0 = omega_0, mu_0 = mu_0, v_0 = v_0,
    lambda_0 = lambda_0, eta_0 = eta_0,
    cluster = cluster, max_iteration = 100,
    threshold = 0.01
  )
  expected <- list(
    clustered = TRUE,
    ELBO = -260.0001044,
    alpha = 68,
    omega = 44.521887,
    mu = c(-0.392152559, 0.897264002, 0.546942030),
    Sigma = matrix(
      c(
        0.47649862, -0.432523126, -0.039795273,
        -0.43252313, 0.422329210, 0.006203171,
        -0.03979527, 0.006203171, 0.061215951
      ),
      nrow = 3, byrow = TRUE,
      dimnames = list(NULL, NULL)
    ),
    tau = setNames(
      c(
        -0.201920682, 0.668452249, 0.453402893, 0.129862127, 1.658932288,
        0.307320078, 0.549540849, 1.973203993, -1.120620378, -0.762475012,
        -0.519035957, -0.662437695, -1.595921748, -0.811751450, 0.393137035
      ),
      as.character(1:15)
    ),
    sigma = setNames(
      c(
        0.216755495, 0.170345280, 0.156031029, 0.181431876, 0.204781971,
        0.181431876, 0.201117721, 0.208623408, 0.181463037, 0.156031029,
        0.156031029, 0.275834727, 0.156031029, 0.184440787, 0.221017760
      ),
      as.character(1:15)
    ),
    lambda = 10.5,
    eta = 10.25464073,
    iterations = 12,
    n = 75
  )
  expect_equal(result, expected)
})

test_that("unconverged", {
  expect_warning(
    survregVB.frailty.fit(
      Y = Y, X = X, alpha_0 = alpha_0,
      omega_0 = omega_0, mu_0 = mu_0, v_0 = v_0,
      lambda_0 = lambda_0, eta_0 = eta_0,
      cluster = cluster, max_iteration = 11,
      threshold = 0.01
    ),
    "The max iteration has been achieved and the algorithm has not converged"
  )
})

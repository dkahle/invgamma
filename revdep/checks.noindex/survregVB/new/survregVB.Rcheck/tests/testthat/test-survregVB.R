library(survival)
test_that("survregVB", {
  result <- survregVB(Surv(time, infect) ~ trt + fev, dnase,
    501, 500, c(4.4, 0.25, 0.04), 1,
    max_iteration = 100, threshold = 0.0005
  )
  expected <- list(
    ELBO = -4857.09446,
    alpha = 744,
    omega = 674.6480035,
    mu = c(
      "(Intercept)" = 4.112387227, "trt" = 0.415469071,
      "fev" = 0.02129090543
    ),
    Sigma = matrix(
      c(
        0.0362042587960, -1.027351580e-02, -4.732594311e-04,
        -0.0102735158000, 1.986778912e-02, 2.209508562e-05,
        -0.0004732594311, 2.209508562e-05, 8.265796487e-06
      ),
      nrow = 3, byrow = TRUE,
      dimnames = list(
        c("(Intercept)", "trt", "fev"),
        c("(Intercept)", "trt", "fev")
      )
    ),
    iterations = 9,
    n = 645,
    call = quote(survregVB(
      formula = Surv(time, infect) ~ trt + fev,
      data = dnase, alpha_0 = 501,
      omega_0 = 500, mu_0 = c(4.4, 0.25, 0.04),
      v_0 = 1, max_iteration = 100,
      threshold = 5e-04
    ))
  )
  class(expected) <- "survregVB"
  expect_equal(result, expected)
})

test_that("survregVB frailty", {
  # to test if null values are omitted
  simulation_frailty <- rbind(simulation_frailty, NA)
  cluster <- simulation_frailty$cluster
  x1 <- simulation_frailty$x1
  x2 <- simulation_frailty$x2
  Y <- Surv(simulation_frailty$Time, simulation_frailty$delta)
  result <- survregVB(
    formula = Y ~ x1 + x2, alpha_0 = 3, omega_0 = 2,
    mu_0 = c(0, 0, 0), v_0 = 0.1,
    lambda_0 = 3, eta_0 = 2, cluster = cluster,
    max_iteration = 100, threshold = 0.01, na.action = na.omit
  )
  expected <- list(
    clustered = TRUE,
    ELBO = -312.4124906,
    alpha = 78,
    omega = 52.37919667,
    mu = c(
      "(Intercept)" = -0.2924349057, "x1" = 0.8083398502,
      "x2" = 0.5574938250
    ),
    Sigma = matrix(
      c(
        0.5024683696, -0.456473456917, -0.038855380297,
        -0.4564734569, 0.445258447195, 0.0043469710645,
        -0.0388553803, 0.004346971064, 0.062897162502
      ),
      nrow = 3, byrow = TRUE,
      dimnames = list(
        c("(Intercept)", "x1", "x2"),
        c("(Intercept)", "x1", "x2")
      )
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
    n = 75,
    na.action = structure(76, names = "76", class = "omit"),
    call = quote(survregVB(
      formula = Y ~ x1 + x2, alpha_0 = 3, omega_0 = 2,
      mu_0 = c(0, 0, 0), v_0 = 0.1, lambda_0 = 3,
      eta_0 = 2, na.action = na.omit, cluster = cluster,
      max_iteration = 100, threshold = 0.01
    ))
  )
  class(expected) <- "survregVB"
  expect_equal(result, expected)
})

test_that("survregVB errors", {
  expect_error(
    survregVB(
      data = dnase, alpha_0 = 501, omega_0 = 500,
      mu_0 = c(4.4, 0.25, 0.04), v_0 = 1,
      max_iteration = 100, threshold = 0.0005
    ),
    "a formula argument is required"
  )
  expect_error(
    survregVB(
      formula = c(
        Surv(time, infect) ~ trt + fev,
        Surv(time, infect) ~ trt
      ),
      data = dnase, alpha_0 = 501, omega_0 = 500,
      mu_0 = c(4.4, 0.25, 0.04), v_0 = 1,
      max_iteration = 100, threshold = 0.0005
    ),
    "formula argument cannot be a list"
  )
  expect_error(
    survregVB(
      formula = Surv(time, infect) ~ trt + fev,
      data = dnase,
      mu_0 = c(4.4, 0.25, 0.04), v_0 = 1,
      max_iteration = 100, threshold = 0.0005
    ),
    "missing value\\(s\\) for alpha_0, omega_0"
  )
  expect_error(
    survregVB(
      formula = time ~ trt + fev, data = dnase,
      alpha_0 = 501, omega_0 = 500,
      mu_0 = c(4.4, 0.25, 0.04), v_0 = 1,
      max_iteration = 100, threshold = 0.0005
    ),
    "response must be a survival object"
  )
  expect_error(
    survregVB(
      formula = time ~ trt + fev, data = dnase,
      alpha_0 = 501, omega_0 = 500,
      mu_0 = c(4.4, 0.25, 0.04), v_0 = 1,
      max_iteration = 100, threshold = 0.0005
    ),
    "response must be a survival object"
  )
  expect_error(
    survregVB(
      formula = Surv(time, infect, type = "left") ~ trt + fev, data = dnase,
      alpha_0 = 501, omega_0 = 500, mu_0 = c(4.4, 0.25, 0.04), v_0 = 1,
      max_iteration = 100, threshold = 0.0005
    ),
    "only Survival objects of type right are supported"
  )
  expect_error(
    survregVB(
      formula = Surv(time, infect) ~ trt + fev,
      data = dnase, alpha_0 = 501, omega_0 = 500,
      mu_0 = c(4.4, 0.25, 0.04, 1.0), v_0 = 1,
      max_iteration = 100, threshold = 0.0005
    ),
    "the length of mu_0 must match the number of covariates"
  )
})

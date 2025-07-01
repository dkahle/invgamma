library(survival)
result <- survregVB(Surv(time, infect) ~ trt + fev, dnase,
  501, 500, c(4.4, 0.25, 0.04), 1,
  max_iteration = 100, threshold = 0.0005
)
result2 <- suppressWarnings(
  survregVB(Surv(time, infect) ~ trt + fev, dnase,
                    501, 500, c(4.4, 0.25, 0.04), 1,
                    max_iteration = 8, threshold = 0.0005
  )
)

# to test if null values are omitted
simulation_frailty <- rbind(simulation_frailty, NA)
cluster <- simulation_frailty$cluster
x1 <- simulation_frailty$x1
x2 <- simulation_frailty$x2
Y <- Surv(simulation_frailty$Time, simulation_frailty$delta)
result_frailty <- survregVB(
  formula = Y ~ x1 + x2, alpha_0 = 3, omega_0 = 2,
  mu_0 = c(0, 0, 0), v_0 = 0.1,
  lambda_0 = 3, eta_0 = 2, cluster = cluster,
  max_iteration = 100, threshold = 0.01, na.action = na.omit
)

test_that("print", {
  expected_output <- "Call:\nsurvregVB(formula = Surv(time, infect) ~ trt + fev, data = dnase, \n    alpha_0 = 501, omega_0 = 500, mu_0 = c(4.4, 0.25, 0.04), \n    v_0 = 1, max_iteration = 100, threshold = 5e-04)\n\nPosterior distributions of the regression coefficients (Beta):\nmu=\n(Intercept)         trt         fev \n     4.1124      0.4155      0.0213 \n\nSigma=\n            (Intercept)   trt     fev\n(Intercept)    0.036204 -0.01 < 2e-16\ntrt           -0.010274  0.02 2.2e-05\nfev           -0.000473  0.00 8.3e-06\n\nPosterior distribution of the scale parameter (b):\nalpha=  744   omega=  674.648 \n\nELBO=  -4857.094 \n\nNumber of iterations=  9 \n\nn= 645 "
  expect_equal(capture_output(print(result)), expected_output)

  expected_output <- "Call:\nsurvregVB(formula = Surv(time, infect) ~ trt + fev, data = dnase, \n    alpha_0 = 501, omega_0 = 500, mu_0 = c(4.4, 0.25, 0.04), \n    v_0 = 1, max_iteration = 8, threshold = 5e-04)\n\nThe VB algorithm did not converge.\n\nPosterior distributions of the regression coefficients (Beta):\nmu=\n(Intercept)         trt         fev \n     4.1123      0.4156      0.0213 \n\nSigma=\n            (Intercept)   trt     fev\n(Intercept)    0.036254 -0.01 < 2e-16\ntrt           -0.010287  0.02 2.2e-05\nfev           -0.000474  0.00 8.3e-06\n\nPosterior distribution of the scale parameter (b):\nalpha=  744   omega=  674.6411 \n\nELBO=  -4857.18 \n\nNumber of iterations=  8 \n\nn= 645 "
  expect_equal(capture_output(print(result2)), expected_output)

  expected_output <- "Call:\nsurvregVB(formula = Y ~ x1 + x2, alpha_0 = 3, omega_0 = 2, mu_0 = c(0, \n    0, 0), v_0 = 0.1, lambda_0 = 3, eta_0 = 2, na.action = na.omit, \n    cluster = cluster, max_iteration = 100, threshold = 0.01)\n\nPosterior distributions of the regression coefficients (Beta):\nmu=\n(Intercept)          x1          x2 \n     -0.292       0.808       0.557 \n\nSigma=\n            (Intercept)    x1     x2\n(Intercept)      0.5025 -0.46 <2e-16\nx1              -0.4565  0.45 0.0043\nx2              -0.0389  0.00 0.0629\n\nPosterior distribution of the scale parameter (b):\nalpha=  78   omega=  52.3792 \n\nPosterior distribution of the random intercept (sigma_gamma squared):\nlambda=  10.5   eta=  10.03876 \n\nPosterior distributions of the random effects for each cluster (gamma):\ntau=\n     1      2      3      4      5      6      7      8      9     10     11 \n-0.208  0.704  0.434  0.127  1.612  0.285  0.707  1.833 -1.121 -0.764 -0.522 \n    12     13     14     15 \n-0.675 -1.586 -0.793  0.422 \n\nsigma=\n    1     2     3     4     5     6     7     8     9    10    11    12    13 \n0.225 0.162 0.162 0.189 0.189 0.189 0.225 0.189 0.189 0.162 0.162 0.278 0.162 \n   14    15 \n0.189 0.234 \n\nELBO=  -312.412 \n\nNumber of iterations=  13 \n\nn=75 (1 observation deleted due to missingness)"
  expect_equal(capture_output(print(result_frailty)), expected_output)
})

test_that("summary", {
  expected <- list(
    call = quote(survregVB(
      formula = Surv(time, infect) ~ trt + fev,
      data = dnase, alpha_0 = 501,
      omega_0 = 500, mu_0 = c(4.4, 0.25, 0.04),
      v_0 = 1, max_iteration = 100,
      threshold = 5e-04
    )),
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
    estimates = matrix(
      c(
        4.11238723, 0.19027417,   3.73945671,   4.48531774,
        0.41546907, 0.14095315,   0.13920598,   0.69173216,
        0.02129091, 0.00287503,   0.01565595,   0.02692586,
        0.90800539, 0.03333393,   0.84358709,   0.97365570
      ),
      nrow = 4, byrow = TRUE,
      dimnames = list(
        c("(Intercept)", "trt", "fev", "scale"),
        c("Value", "SD", "95% CI Lower", "95% CI Upper")
      )
    )
  )
  class(expected) <- "summary.survregVB"
  expect_equal(summary(result), expected)

  expected <- list(
    clustered = TRUE,
    call = quote(survregVB(
      formula = Y ~ x1 + x2, alpha_0 = 3, omega_0 = 2,
      mu_0 = c(0, 0, 0), v_0 = 0.1, lambda_0 = 3,
      eta_0 = 2, na.action = na.omit, cluster = cluster,
      max_iteration = 100, threshold = 0.01
    )),
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
    na.action = structure(76, names = "76", class = "omit"),
    iterations = 13,
    n = 75,
    estimates = matrix(
      c(
        -0.2924349,  0.70885003, -1.68175544,  1.0968856,
        0.8083399,  0.66727689, -0.49949882,  2.1161785,
        0.5574938,  0.25079307,  0.06594845,  1.0490392,
        0.6802493,  0.07802995,  0.53685649,  0.8374610,
        1.0567113,  0.37270215,  0.49069380,  1.7664480
      ),
      nrow = 5, byrow = TRUE,
      dimnames = list(
        c("(Intercept)", "x1", "x2", "scale", "frailty"),
        c("Value", "SD", "95% CI Lower", "95% CI Upper")
      )
    )
  )
  class(expected) <- "summary.survregVB"
  expect_equal(summary(result_frailty), expected)
})

test_that("print.summary", {
  expected_output <- "Call:\nsurvregVB(formula = Surv(time, infect) ~ trt + fev, data = dnase, \n    alpha_0 = 501, omega_0 = 500, mu_0 = c(4.4, 0.25, 0.04), \n    v_0 = 1, max_iteration = 100, threshold = 5e-04)\n            Value    SD 95% CI Lower 95% CI Upper\n(Intercept) 4.112 0.190        3.739        4.485\ntrt         0.415 0.141        0.139        0.692\nfev         0.021 0.003        0.016        0.027\nscale       0.908 0.033        0.844        0.974\n\nELBO=  -4857.094 \n\nNumber of iterations=  9 \n\nn= 645 "
  expect_equal(capture_output(print(summary(result, digits = 4))), expected_output)

  expected_output <- "Call:\nsurvregVB(formula = Surv(time, infect) ~ trt + fev, data = dnase, \n    alpha_0 = 501, omega_0 = 500, mu_0 = c(4.4, 0.25, 0.04), \n    v_0 = 1, max_iteration = 8, threshold = 5e-04)\n            Value    SD 95% CI Lower 95% CI Upper\n(Intercept) 4.112 0.190        3.739        4.485\ntrt         0.416 0.141        0.139        0.692\nfev         0.021 0.003        0.016        0.027\nscale       0.908 0.033        0.844        0.974\n\nELBO=  -4857.18 \n\nNumber of iterations=  8 \n\nn= 645 "
  expect_equal(capture_output(print(suppressWarnings(summary(result2)))), expected_output)

  expected_output <- "Call:\nsurvregVB(formula = Y ~ x1 + x2, alpha_0 = 3, omega_0 = 2, mu_0 = c(0, \n    0, 0), v_0 = 0.1, lambda_0 = 3, eta_0 = 2, na.action = na.omit, \n    cluster = cluster, max_iteration = 100, threshold = 0.01)\n             Value     SD 95% CI Lower 95% CI Upper\n(Intercept) -0.292  0.709       -1.682        1.097\nx1           0.808  0.667       -0.499        2.116\nx2           0.557  0.251        0.066        1.049\nscale        0.680  0.078        0.537        0.837\nfrailty      1.057  0.373        0.491        1.766\n\nELBO=  -312.412 \n\nNumber of iterations=  13 \n\nn=75 (1 observation deleted due to missingness)"
  expect_equal(capture_output(print(summary(result_frailty))), expected_output)
})

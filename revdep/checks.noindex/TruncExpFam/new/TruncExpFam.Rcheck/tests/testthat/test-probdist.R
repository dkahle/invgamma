context("probdist class")

genparm <- function(type = "int") {
  # ad-hoc function to randomly generate probability distribution parameters
  parm <- switch(type,
    "int"  = rpois(1L, lambda = 10L),
    "prob" = runif(1L),
    "norm" = rnorm(1L, mean = 0L, sd = 10L),
    "pos"  = rexp(1L, rate = .2),
    "neg"  = -genparm("pos"),
    stop("invalid type")
  )
  return(parm)
}

test_that("Beta parameteres are properly converted", {
  fam <- "beta"
  s1 <- genparm()
  s2 <- genparm()

  prbdst <- probdist(shape1 = s1, shape2 = s2, family = fam)
  expect_equal(prbdst$family, fam)
  expect_equal(prbdst$parms, c(shape1 = s1, shape2 = s2))
  expect_equal(prbdst$nat_parms, c(eta1 = s1, eta2 = s2))

  prbdst_nat <- probdist(eta1 = s1, eta2 = s2, family = fam)
  expect_equal(prbdst_nat$parms, c(shape1 = s1, shape2 = s2))
  expect_equal(prbdst_nat$nat_parms, c(eta1 = s1, eta2 = s2))
})

test_that("Binomial parameteres are properly converted", {
  fam <- "binomial"
  sz <- genparm()
  pb <- genparm("prob")
  et <- genparm("norm")

  prbdst <- probdist(size = sz, prob = pb, family = fam)
  expect_equal(prbdst$family, fam)
  expect_equal(prbdst$parms, c(size = sz, prob = pb))
  expect_equal(c(prbdst$nat_parms), c(eta = log(pb / (1 - pb))))

  prbdst_nat <- probdist(eta = et, family = fam)
  expect_equal(prbdst_nat$parms, c(prob = exp(et) / (1 + exp(et))))
  expect_equal(prbdst_nat$nat_parms, c(eta = et))
})

test_that("Chisq parameteres are properly converted", {
  fam <- "chisq"
  nu <- genparm()
  et <- genparm()

  prbdst <- probdist(df = nu, family = fam)
  expect_equal(prbdst$family, fam)
  expect_equal(prbdst$parms, c(df = nu))
  expect_equal(prbdst$nat_parms, c(eta = nu / 2 - 1))

  prbdst_nat <- probdist(eta = et, family = fam)
  expect_equal(prbdst_nat$parms, c(df = 2 * (et + 1)))
  expect_equal(prbdst_nat$nat_parms, c(eta = et))
})

test_that("Contbern parameteres are properly converted", {
  fam <- "contbern"
  lb <- genparm("prob")
  et <- genparm("norm")

  prbdst <- probdist(lambda = lb, family = fam)
  expect_equal(prbdst$family, fam)
  expect_equal(prbdst$parms, c(lambda = lb))
  expect_equal(prbdst$nat_parms, c(eta = log(lb / (1 - lb))))

  prbdst_nat <- probdist(eta = et, family = fam)
  expect_equal(prbdst_nat$parms, c(lambda = exp(et) / (1 + exp(et))))
  expect_equal(prbdst_nat$nat_parms, c(eta = et))
})

test_that("Exponential parameteres are properly converted", {
  fam <- "exp"
  rt <- genparm("pos")
  et <- genparm("neg")

  prbdst <- probdist(rate = rt, family = fam)
  expect_equal(prbdst$family, fam)
  expect_equal(prbdst$parms, c(rate = rt))
  expect_equal(prbdst$nat_parms, c(eta = -rt))

  prbdst_nat <- probdist(eta = et, family = fam)
  expect_equal(prbdst_nat$parms, c(rate = -et))
  expect_equal(prbdst_nat$nat_parms, c(eta = et))
})

test_that("Gamma parameteres are properly converted", {
  fam <- "gamma"
  alpha <- genparm("pos")
  beta <- genparm("pos")
  k <- genparm("pos")
  theta <- genparm("pos")
  et1 <- genparm("norm")
  et2 <- genparm("norm")

  prbdst_1 <- probdist(shape = k, scale = theta, family = fam)
  expect_equal(prbdst_1$family, fam)
  expect_equal(prbdst_1$parms, c(shape = k, scale = theta))
  expect_equal(prbdst_1$nat_parms, c(eta1 = k - 1, eta2 = -1 / theta))

  prbdst_2 <- probdist(shape = alpha, rate = beta, family = fam)
  expect_equal(prbdst_2$family, fam)
  expect_equal(prbdst_2$parms, c(shape = alpha, rate = beta))
  expect_equal(prbdst_2$nat_parms, c(eta1 = alpha - 1, eta2 = -beta))

  prbdst_nat <- probdist(eta1 = et1, eta2 = et2, family = fam)
  expect_equal(prbdst_nat$parms, c(shape = et1 + 1, rate = -et2))
  expect_equal(prbdst_nat$nat_parms, c(eta1 = et1, eta2 = et2))
})

test_that("Inverse gamma parameteres are properly converted", {
  fam <- "invgamma"
  alpha <- genparm("pos")
  beta <- genparm("pos")
  et1 <- genparm("norm")
  et2 <- genparm("norm")

  prbdst_1 <- probdist(shape = alpha, scale = beta, family = fam)
  expect_equal(prbdst_1$family, fam)
  expect_equal(prbdst_1$parms, c(shape = alpha, scale = beta))
  expect_equal(prbdst_1$nat_parms, c(eta1 = -alpha - 1, eta2 = -beta))

  prbdst_nat <- probdist(eta1 = et1, eta2 = et2, family = fam)
  expect_equal(prbdst_nat$parms, c(shape = - et1 - 1, rate = -et2))
  expect_equal(prbdst_nat$nat_parms, c(eta1 = et1, eta2 = et2))
})

test_that("Inverse gaussian parameteres are properly converted", {
  fam <- "invgauss"
  mu <- genparm("pos")
  disp <- genparm("pos")
  lb <- 1 / disp
  et1 <- genparm("neg")
  et2 <- genparm("neg")

  prbdst_1 <- probdist(m = mu, s = disp, family = fam)
  expect_equal(prbdst_1$family, fam)
  expect_equal(prbdst_1$parms, c(m = mu, s = disp))
  expect_equal(prbdst_1$nat_parms, c(eta1 = -lb / 2 / mu ^ 2, eta2 = -lb / 2))

  prbdst_nat <- probdist(eta1 = et1, eta2 = et2, family = fam)
  expect_equal(prbdst_nat$parms, c(m = sqrt(et2 / et1), s = 1 / -2 / et2))
  expect_equal(prbdst_nat$nat_parms, c(eta1 = et1, eta2 = et2))
})

test_that("Log-normal parameteres are properly converted", {
  fam <- "lognormal"
  mu <- genparm("norm")
  sg <- genparm("pos")
  et1 <- genparm("pos")
  et2 <- genparm("neg")

  prbdst_1 <- probdist(meanlog = mu, sdlog = sg, family = fam)
  expect_equal(prbdst_1$family, fam)
  expect_equal(prbdst_1$parms, c(meanlog = mu, sdlog = sg))
  expect_equal(
    prbdst_1$nat_parms, c(eta1 = mu / sg ^ 2, eta2 = -1 / 2 / sg ^ 2)
  )

  prbdst_nat <- probdist(eta1 = et1, eta2 = et2, family = fam)
  expect_equal(
    prbdst_nat$parms, c(meanlog = - et1 / 2 / et2, sdlog = sqrt(-1 / 2 / et2))
  )
  expect_equal(prbdst_nat$nat_parms, c(eta1 = et1, eta2 = et2))
})

test_that("Normal parameteres are properly converted", {
  fam <- "normal"
  mu <- genparm("norm")
  sg <- genparm("pos")
  et1 <- genparm("pos")
  et2 <- genparm("neg")

  prbdst_1 <- probdist(mean = mu, sd = sg, family = fam)
  expect_equal(prbdst_1$family, fam)
  expect_equal(prbdst_1$parms, c(mean = mu, sd = sg))
  expect_equal(
    prbdst_1$nat_parms, c(eta1 = mu / sg ^ 2, eta2 = -1 / 2 / sg ^ 2)
  )

  prbdst_nat <- probdist(eta1 = et1, eta2 = et2, family = fam)
  expect_equal(
    prbdst_nat$parms, c(mean = - et1 / 2 / et2, sd = sqrt(-1 / 2 / et2))
  )
  expect_equal(prbdst_nat$nat_parms, c(eta1 = et1, eta2 = et2))
})

test_that("Poisson parameteres are properly converted", {
  fam <- "poisson"
  lb <- genparm("int")
  et <- genparm("pos")

  prbdst_1 <- probdist(lambda = lb, family = fam)
  expect_equal(prbdst_1$family, fam)
  expect_equal(prbdst_1$parms, c(lambda = lb))
  expect_equal(prbdst_1$nat_parms, c(eta = log(lb)))

  prbdst_nat <- probdist(eta = et, family = fam)
  expect_equal(prbdst_nat$parms, c(lambda = exp(et)))
  expect_equal(prbdst_nat$nat_parms, c(eta = et))
})

test_that("Errors are properly caught", {
  expect_error(
    probdist(shape1 = -1, shape2 = 1, family = "beta"),
    "Invalid parameter domain"
  )
  expect_error(
    probdist(m = -1, s = 1, family = "binomial"),
    "The \\{m, s\\} parameter set does not match the binomial family"
  )

  eta_err_1 <- "Eta must be one single number"
  eta_err_2 <- "Eta must be a vector of two elements"
  expect_error(probdist(eta = -5, family = "beta"), eta_err_2)
  expect_error(probdist(eta1 = 5, eta2 = -2, family = "binomial"), eta_err_1)
  expect_error(probdist(eta1 = 5, eta2 = -2, family = "chisq"), eta_err_1)
  expect_error(probdist(eta = -5, eta2 = -2, family = "contbern"), eta_err_1)
  expect_error(probdist(eta1 = 5, eta2 = -2, family = "exp"), eta_err_1)
  expect_error(probdist(eta = -5, family = "gamma"), eta_err_2)
  expect_error(probdist(eta = -5, family = "invgamma"), eta_err_2)
  expect_error(probdist(eta = -5, family = "invgauss"), eta_err_2)
  expect_error(probdist(eta = -5, family = "lognormal"), eta_err_2)
  expect_error(probdist(eta = -5, family = "normal"), eta_err_2)
  expect_error(probdist(eta1 = 5, eta2 = -2, family = "poisson"), eta_err_1)
})

test_that("Print method works", {
  expect_output(
    print(probdist(mean = 100, sd = 4, family = "gaussian")),
    "Family:\\s+Normal\\nParameters:\\s+mean = 100\\s+sd   = 4"
  )
})

pkgname <- "survregVB"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('survregVB')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("survregVB")
### * survregVB

flush(stderr()); flush(stdout())

### Name: survregVB
### Title: Variational Bayesian Analysis of Survival Data Using a
###   Log-Logistic Accelerated Failure Time Model
### Aliases: survregVB

### ** Examples

# Data frame containing survival data
fit <- survregVB(
  formula = survival::Surv(time, infect) ~ trt + fev,
  data = dnase,
  alpha_0 = 501,
  omega_0 = 500,
  mu_0 = c(4.4, 0.25, 0.04),
  v_0 = 1,
  max_iteration = 100,
  threshold = 0.0005
)
summary(fit)

# Call the survregVB function with shared frailty
fit2 <- survregVB(
  formula = survival::Surv(Time.15, delta.15) ~ x1 + x2,
  data = simulation_frailty,
  alpha_0 = 3,
  omega_0 = 2,
  mu_0 = c(0, 0, 0),
  v_0 = 0.1,
  lambda_0 = 3,
  eta_0 = 2,
  cluster = cluster,
  max_iteration = 100,
  threshold = 0.01
)
summary(fit2)



cleanEx()
nameEx("survregVB.fit")
### * survregVB.fit

flush(stderr()); flush(stdout())

### Name: survregVB.fit
### Title: Variational Bayesian Analysis of Survival Data Using a
###   Log-Logistic Accelerated Failure Time Model
### Aliases: survregVB.fit

### ** Examples

fit <- survregVB.fit(
  Y = survival::Surv(simulation_nofrailty$Time, simulation_nofrailty$delta),
  X = matrix(c(rep(1, 300), simulation_nofrailty$x1, simulation_nofrailty$x2), nrow = 300),
  alpha_0 = 11,
  omega_0 = 10,
  mu_0 = c(0, 0, 0),
  v_0 = 1
)




cleanEx()
nameEx("survregVB.frailty.fit")
### * survregVB.frailty.fit

flush(stderr()); flush(stdout())

### Name: survregVB.frailty.fit
### Title: Variational Bayesian Analysis of Correlated Survival Data Using
###   a Log-Logistic Accelerated Failure Time Model
### Aliases: survregVB.frailty.fit

### ** Examples

fit <- survregVB.frailty.fit(
  X = matrix(c(rep(1, 75), simulation_frailty$x1, simulation_frailty$x2), nrow = 75),
  Y = survival::Surv(simulation_frailty$Time, simulation_frailty$delta),
  alpha_0 = 3,
  omega_0 = 2,
  mu_0 = c(0, 0, 0),
  v_0 = 0.1,
  lambda_0 = 3,
  eta_0 = 2,
  cluster = simulation_frailty$cluster
)




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')

pkgname <- "carfima"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('carfima')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("carfima-package")
### * carfima-package

flush(stderr()); flush(stdout())

### Name: carfima-package
### Title: Continuous-Time Fractionally Integrated ARMA Process for
###   Irregularly Spaced Long-Memory Time Series Data
### Aliases: carfima-package
### Keywords: package

### ** Examples

  ##### Irregularly spaced observation time generation.

  length.time <- 30
  time.temp <- rexp(length.time, rate = 2)
  time <- rep(NA, length.time + 1)
  time[1] <- 0
  for (i in 2 : (length.time + 1)) {
    time[i] <- time[i - 1] + time.temp[i - 1]
  }
  time <- time[-1]

  ##### Data genration for CARFIMA(1, H, 0) based on the observation times. 

  parameter <- c(-0.4, 0.8, 0.2) 
  # AR parameter alpha = -0.4
  # Hurst parameter = 0.8
  # Process uncertainty (standard deviation) sigma = 0.2

  me.sd <- rep(0.05, length.time)
  # Known measurement error standard deviations 0.05 for all observations
  # If not known, remove the argument "measure.error = me.sd" in the following codes,
  # so that the default values (zero) are automatically assigned.

  y <- carfima.sim(parameter = parameter, time = time, 
                   measure.error = me.sd, ar.p = 1, ma.q = 0)  

  ##### Fitting the CARFIMA(1, H, 0) model on the simulated data for MLEs.
  # It takes a long time due to the differential evolution algorithm (global optimizer).
  # res$mle; res$se; res$AIC; res$fitted.values

  ##### Fitting the CARFIMA(1, H, 0) model on the simulated data for Bayesian inference.
  # It takes a long time because the likelihood evaluation is computationally heavy.
  # The last number of bayes.param.scale is to update sigma2 (not sigma) on a log scale.
  # hist(res$param[, 1]); res$accept; res$AIC; res$fitted.values

  ##### Computing the log likelihood of the CARFIMA(1, H, 0) model given the parameters.
  loglik <- carfima.loglik(Y = y, time = time, ar.p = 1, ma.q = 0,
                           measure.error = me.sd,
                           parameter = parameter, fitted = FALSE)



cleanEx()
nameEx("carfima")
### * carfima

flush(stderr()); flush(stdout())

### Name: carfima
### Title: Fitting a CARFIMA(p, H, q) model via frequentist or Bayesian
###   machinery
### Aliases: carfima
### Keywords: methods

### ** Examples

  ##### Irregularly spaced observation time generation.

  length.time <- 30
  time.temp <- rexp(length.time, rate = 2)
  time <- rep(NA, length.time + 1)
  time[1] <- 0
  for (i in 2 : (length.time + 1)) {
    time[i] <- time[i - 1] + time.temp[i - 1]
  }
  time <- time[-1]

  ##### Data genration for CARFIMA(1, H, 0) based on the observation times. 

  parameter <- c(-0.4, 0.8, 0.2) 
  # AR parameter alpha = -0.4
  # Hurst parameter = 0.8
  # Process uncertainty (standard deviation) sigma = 0.2

  me.sd <- rep(0.05, length.time)
  # Known measurement error standard deviations 0.05 for all observations
  # If not known, remove the argument "measure.error = me.sd" in the following codes,
  # so that the default values (zero) are automatically assigned.

  y <- carfima.sim(parameter = parameter, time = time, 
                   measure.error = me.sd, ar.p = 1, ma.q = 0)  

  ##### Fitting the CARFIMA(1, H, 0) model on the simulated data for MLEs.
  # It takes a long time due to the differential evolution algorithm (global optimizer).
  # res$mle; res$se; res$AIC; res$fitted.values

  ##### Fitting the CARFIMA(1, H, 0) model on the simulated data for Bayesian inference.
  # It takes a long time because the likelihood evaluation is computationally heavy.
  # The last number of bayes.param.scale is to update sigma2 (not sigma) on a log scale.
  # hist(res$param[, 1]); res$accept; res$AIC; res$fitted.values

  ##### Computing the log likelihood of the CARFIMA(1, H, 0) model given the parameters.
  loglik <- carfima.loglik(Y = y, time = time, ar.p = 1, ma.q = 0,
                           measure.error = me.sd,
                           parameter = parameter, fitted = FALSE)



cleanEx()
nameEx("carfima.loglik")
### * carfima.loglik

flush(stderr()); flush(stdout())

### Name: carfima.loglik
### Title: Computing the log likelihood function of a CARFIMA(p, H, q)
###   model
### Aliases: carfima.loglik
### Keywords: methods

### ** Examples

  ##### Irregularly spaced observation time generation.


  length.time <- 30
  time.temp <- rexp(length.time, rate = 2)
  time <- rep(NA, length.time + 1)
  time[1] <- 0
  for (i in 2 : (length.time + 1)) {
    time[i] <- time[i - 1] + time.temp[i - 1]
  }
  time <- time[-1]

  ##### Data genration for CARFIMA(1, H, 0) based on the observation times. 

  parameter <- c(-0.4, 0.8, 0.2) 
  # AR parameter alpha = -0.4
  # Hurst parameter = 0.8
  # Process uncertainty (standard deviation) sigma = 0.2

  me.sd <- rep(0.05, length.time)
  # Known measurement error standard deviations 0.05 for all observations
  # If not known, remove the argument "measure.error = me.sd" in the following codes,
  # so that the default values (zero) are automatically assigned.

  y <- carfima.sim(parameter = parameter, time = time, 
                   measure.error = me.sd, ar.p = 1, ma.q = 0)  

  ##### Computing the log likelihood of the CARFIMA(1, H, 0) model given the parameters.
  loglik <- carfima.loglik(Y = y, time = time, ar.p = 1, ma.q = 0,
                           measure.error = me.sd,
                           parameter = parameter, fitted = FALSE)



cleanEx()
nameEx("carfima.sim")
### * carfima.sim

flush(stderr()); flush(stdout())

### Name: carfima.sim
### Title: Simulating a CARFIMA(p, H, q) time series
### Aliases: carfima.sim
### Keywords: methods

### ** Examples

  ##### Irregularly spaced observation time generation.

  length.time <- 30
  time.temp <- rexp(length.time, rate = 2)
  time <- rep(NA, length.time + 1)
  time[1] <- 0
  for (i in 2 : (length.time + 1)) {
    time[i] <- time[i - 1] + time.temp[i - 1]
  }
  time <- time[-1]

  ##### Data genration for CARFIMA(1, H, 0) based on the observation times. 

  parameter <- c(-0.4, 0.8, 0.2) 
  # AR parameter alpha = -0.4
  # Hurst parameter = 0.8
  # Process uncertainty (standard deviation) sigma = 0.2

  me.sd <- rep(0.05, length.time)
  # Known measurement error standard deviations 0.05 for all observations
  # If not known, remove the argument "measure.error = me.sd" in the following codes,
  # so that the default values (zero) are automatically assigned.

  y <- carfima.sim(parameter = parameter, time = time, 
                   measure.error = me.sd, ar.p = 1, ma.q = 0)  



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

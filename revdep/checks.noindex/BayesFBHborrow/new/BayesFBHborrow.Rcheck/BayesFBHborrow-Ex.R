pkgname <- "BayesFBHborrow"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('BayesFBHborrow')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("BayesFBHborrow.NoBorrow")
### * BayesFBHborrow.NoBorrow

flush(stderr()); flush(stdout())

### Name: BayesFBHborrow.NoBorrow
### Title: Run the MCMC sampler without Bayesian Borrowing
### Aliases: BayesFBHborrow.NoBorrow

### ** Examples

set.seed(123)
# Load the example data
data(piecewise_exp_cc, package = "BayesFBHborrow")

# Set your tuning parameters
tuning_parameters <- list("Jmax" = 5,
                          "cprop_beta" = 3.25)
                          
# Set initial values to default
out <- BayesFBHborrow(piecewise_exp_cc, NULL, borrow = FALSE, 
                      tuning_parameters = tuning_parameters,
                      iter = 2, warmup_iter = 0)



cleanEx()
nameEx("BayesFBHborrow")
### * BayesFBHborrow

flush(stderr()); flush(stdout())

### Name: BayesFBHborrow
### Title: BayesFBHborrow: Run MCMC for a piecewise exponential model
### Aliases: BayesFBHborrow

### ** Examples

set.seed(123)
# Load the example data
data(piecewise_exp_cc, package = "BayesFBHborrow")
data(piecewise_exp_hist, package = "BayesFBHborrow")

# Set your tuning parameters
tuning_parameters <- list("Jmax" = 5,
                          "pi_b" = 0.5,
                          "cprop_beta" = 3.25,
                          "alpha" = 0.4)
                          
# Set hyperparameters to default, with the borrowing model "mix"
out <- BayesFBHborrow(data = piecewise_exp_cc, data_hist = piecewise_exp_hist,
                      model_choice = 'mix', tuning_parameters = tuning_parameters,
                      iter = 2, warmup_iter = 0)

# Create a summary of the output
summary(out$out, estimator = "out_fixed")

# Plot the predictive curves for the treatment group
plots <- plot(out$out, out$out$time_grid, x_pred = c(1))



cleanEx()
nameEx("BayesFBHborrow.WBorrow")
### * BayesFBHborrow.WBorrow

flush(stderr()); flush(stdout())

### Name: BayesFBHborrow.WBorrow
### Title: Run the MCMC sampler with Bayesian Borrowing
### Aliases: BayesFBHborrow.WBorrow

### ** Examples

set.seed(123)
# Load the example data
data(piecewise_exp_cc, package = "BayesFBHborrow")
data(piecewise_exp_hist, package = "BayesFBHborrow")

# Set your tuning parameters
tuning_parameters <- list("Jmax" = 5,
                          "pi_b" = 0.5,
                          "cprop_beta" = 3.25,
                          "alpha" = 0.4)
                          
# Set hyperparameters to default, with the borrowing model "mix"
out <- BayesFBHborrow(data = piecewise_exp_cc, data_hist = piecewise_exp_hist,
                      model_choice = 'mix', tuning_parameters = tuning_parameters,
                      iter = 2, warmup_iter = 0)

# Create a summary of the output
summary(out$out, estimator = "out_fixed")

# Plot the predictive curves for the treatment group
plots <- plot(out$out, out$out$time_grid, x_pred = c(1))



cleanEx()
nameEx("GibbsMH.NoBorrow")
### * GibbsMH.NoBorrow

flush(stderr()); flush(stdout())

### Name: GibbsMH.NoBorrow
### Title: GibbsMH sampler, without Bayesian Borrowing
### Aliases: GibbsMH.NoBorrow

### ** Examples

set.seed(123)
# Load example data and set your hyper parameters
data(weibull_cc, package = "BayesFBHborrow")
data(weibull_hist, package = "BayesFBHborrow")

# The datasets consists of 3 (2) columns named "tte", "event" and "X".
# To explicitly run the sampler, extract the samples as following
Y <- weibull_cc$tte
I <- weibull_cc$event
X <- matrix(weibull_cc$X_trt)

# Specify hyperparameters and tuning parameters
hyper <-  list("a_sigma" = 2,
               "b_sigma" = 2,
               "clam_smooth" = 0.5,
               "phi" = 3)

tuning_parameters <- list("Jmax" = 5,
                          "pi_b" = 0.5,
                          "cprop_beta" = 0.5)
                          
# Set initial values to 'NULL' for default settings
output <- GibbsMH(Y, I, X, NULL, NULL, NULL,
                  tuning_parameters = tuning_parameters, hyperparameters = hyper, 
                  iter = 5, warmup_iter = 1)



cleanEx()
nameEx("GibbsMH")
### * GibbsMH

flush(stderr()); flush(stdout())

### Name: GibbsMH
### Title: S3 generic, calls the correct GibbsMH sampler
### Aliases: GibbsMH

### ** Examples

set.seed(123)
# Load example data and set your initial values and hyper parameters
data(weibull_cc, package = "BayesFBHborrow")
data(weibull_hist, package = "BayesFBHborrow")

# The datasets consists of 3 (2) columns named "tte", "event" and "X" 
# (only for concurrent). To explicitly run the sampler, extract the samples as
# following
Y <- weibull_cc$tte
I <- weibull_cc$event
X <- matrix(weibull_cc$X_trt)

Y_0 <- weibull_hist$tte
I_0 <- weibull_hist$event
X_0 <- NULL

# Specify hyperparameters and tuning parameters
hyper <-  list("a_tau" = 1, 
               "b_tau" = 0.001,
               "c_tau" = 1,
               "d_tau" = 1, 
               "type" = 'all',
               "p_0" = 0.5, 
               "a_sigma" = 2,
               "b_sigma" = 2,
               "clam_smooth" = 0.5,
               "phi" = 3)

tuning_parameters <- list("Jmax" = 5,
                          "pi_b" = 0.5,
                          "cprop_beta" = 0.5,
                          "alpha" = 0.4)
                          
output <- GibbsMH(Y, I, X, Y_0, I_0, X_0,
                  tuning_parameters, hyper, 
                  iter = 5, warmup_iter = 1)



cleanEx()
nameEx("GibbsMH.WBorrow")
### * GibbsMH.WBorrow

flush(stderr()); flush(stdout())

### Name: GibbsMH.WBorrow
### Title: GibbsMH sampler, with Bayesian Borrowing
### Aliases: GibbsMH.WBorrow

### ** Examples

set.seed(123)
# Load example data and set your initial values and hyper parameters
data(weibull_cc, package = "BayesFBHborrow")
data(weibull_hist, package = "BayesFBHborrow")

# The datasets consists of 3 (2) columns named "tte", "event" and "X" 
# (only for concurrent). To explicitly run the sampler, extract the samples as
# following
Y <- weibull_cc$tte
I <- weibull_cc$event
X <- matrix(weibull_cc$X_trt)

Y_0 <- weibull_hist$tte
I_0 <- weibull_hist$event
X_0 <- NULL

# Specify hyperparameters and tuning parameters
hyper <-  list("a_tau" = 1, 
               "b_tau" = 0.001,
               "c_tau" = 1,
               "d_tau" = 1, 
               "type" = "all",
               "p_0" = 0.5, 
               "a_sigma" = 2,
               "b_sigma" = 2,
               "clam_smooth" = 0.5,
               "phi" = 3)

tuning_parameters <- list("Jmax" = 5,
                          "pi_b" = 0.5,
                          "cprop_beta" = 0.5,
                          "alpha" = 0.4)
          
output <- GibbsMH(Y, I, X, Y_0, I_0, X_0, tuning_parameters = tuning_parameters,
                  hyperparameters = hyper, iter = 5, warmup_iter = 1)



cleanEx()
nameEx("coef.BayesFBHborrow")
### * coef.BayesFBHborrow

flush(stderr()); flush(stdout())

### Name: coef.BayesFBHborrow
### Title: Extract mean posterior values
### Aliases: coef.BayesFBHborrow

### ** Examples

data(weibull_cc, package = "BayesFBHborrow")

# Set your tuning parameters
tuning_parameters <- list("Jmax" = 5,
                          "pi_b" = 0.5,
                          "cprop_beta" = 0.5)
                          
# run the MCMC sampler
out <- BayesFBHborrow(weibull_cc, NULL, tuning_parameters = tuning_parameters,
                      iter = 3, warmup_iter = 1)

# Plot the posterior mean values of the fixed parameters
coef(out$out)



cleanEx()
nameEx("group_summary")
### * group_summary

flush(stderr()); flush(stdout())

### Name: group_summary
### Title: Create group level data
### Aliases: group_summary

### ** Examples

set.seed(111)
# Load example data and set your initial values and hyper parameters
data(weibull_cc, package = "BayesFBHborrow")
data(weibull_hist, package = "BayesFBHborrow")

Y <- weibull_cc$tte
I <- weibull_cc$event
X <- weibull_cc$X_trt

# Say we want to know the group level data for the following split points
s <- quantile(Y, c(0, 0.45, 0.65, 1), names = FALSE)

group_summary(Y, I, X, s)



cleanEx()
nameEx("init_lambda_hyperparameters")
### * init_lambda_hyperparameters

flush(stderr()); flush(stdout())

### Name: init_lambda_hyperparameters
### Title: Initialize lambda hyperparameters
### Aliases: init_lambda_hyperparameters

### ** Examples

set.seed(111)
# Load example data and set your initial values and hyper parameters
data(weibull_cc, package = "BayesFBHborrow")
data(weibull_hist, package = "BayesFBHborrow")

Y <- weibull_cc$tte
I <- weibull_cc$event
X <- weibull_cc$X_trt

# Say we want to know the group level data for the following split points
s <- quantile(Y, c(0, 0.45, 0.65, 1), names = FALSE)

group_data <- group_summary(Y, I, NULL, s)
init_lambda_hyperparameters(group_data, s)



cleanEx()
nameEx("piecewise_exp_cc")
### * piecewise_exp_cc

flush(stderr()); flush(stdout())

### Name: piecewise_exp_cc
### Title: Example data, simulated from a piecewise exponential model.
### Aliases: piecewise_exp_cc
### Keywords: datasets

### ** Examples

data(piecewise_exp_cc)
survival_model <- survival::survfit(survival::Surv(tte, event) ~ X_trt, data = piecewise_exp_cc)
line_colors <- c("blue", "red")  # Adjust colors as needed
line_types <- 1:length(unique(piecewise_exp_cc$X_trt))
plot(survival_model, col = line_colors, lty = line_types, 
     xlab = "Time (tte)", ylab = "Survival Probability", 
     main = "Kaplan-Meier Survival Curves by Treatment")



cleanEx()
nameEx("piecewise_exp_hist")
### * piecewise_exp_hist

flush(stderr()); flush(stdout())

### Name: piecewise_exp_hist
### Title: Example data, simulated from a piecewise exponential model.
### Aliases: piecewise_exp_hist
### Keywords: datasets

### ** Examples

data(piecewise_exp_cc)
data(piecewise_exp_hist)
piecewise_exp_hist$X_trt <- 0
survival_model <- survival::survfit(survival::Surv(tte, event) ~ X_trt, 
                                    data = rbind(piecewise_exp_cc, 
                                    piecewise_exp_hist))
line_colors <- c("blue", "red", "green")  # Adjust colors as needed
line_types <- 1:length(unique(piecewise_exp_cc$X_trt))
plot(survival_model, col = line_colors, lty = line_types, 
     xlab = "Time (tte)", ylab = "Survival Probability", 
     main = "Kaplan-Meier Survival Curves by Treatment")



cleanEx()
nameEx("plot.BayesFBHborrow")
### * plot.BayesFBHborrow

flush(stderr()); flush(stdout())

### Name: plot.BayesFBHborrow
### Title: Plot the MCMC results
### Aliases: plot.BayesFBHborrow

### ** Examples

data(weibull_cc, package = "BayesFBHborrow")

# Set your tuning parameters
tuning_parameters <- list("Jmax" = 5,
                          "pi_b" = 0.5,
                          "cprop_beta" = 0.5)
                          
# run the MCMC sampler
out <- BayesFBHborrow(weibull_cc, NULL, tuning_parameters = tuning_parameters,
                      iter = 3, warmup_iter = 1)

# for the treatment group
plots <- plot(out$out, out$out$time_grid, x_pred = c(1))



cleanEx()
nameEx("summary.BayesFBHborrow")
### * summary.BayesFBHborrow

flush(stderr()); flush(stdout())

### Name: summary.BayesFBHborrow
### Title: Summarize fixed MCMC results
### Aliases: summary.BayesFBHborrow

### ** Examples

data(piecewise_exp_cc, package = "BayesFBHborrow")

# Set your tuning parameters
tuning_parameters <- list("Jmax" = 5,
                          "pi_b" = 0.5,
                          "cprop_beta" = 0.5)
                          
# run the MCMC sampler
out <- BayesFBHborrow(piecewise_exp_cc, NULL, tuning_parameters = tuning_parameters,
                      iter = 3, warmup_iter = 1)

# Create a summary of the output
summary(out$out, estimator = "out_fixed")



cleanEx()
nameEx("weibull_cc")
### * weibull_cc

flush(stderr()); flush(stdout())

### Name: weibull_cc
### Title: Example data, simulated from a Weibull distribution.
### Aliases: weibull_cc
### Keywords: datasets

### ** Examples

data(weibull_cc)
survival_model <- survival::survfit(survival::Surv(tte, event) ~ X_trt, data = weibull_cc)
line_colors <- c("blue", "red")  # Adjust colors as needed
line_types <- 1:length(unique(weibull_cc$X_trt))
plot(survival_model, col = line_colors, lty = line_types, 
     xlab = "Time (tte)", ylab = "Survival Probability", 
     main = "Kaplan-Meier Survival Curves by Treatment")



cleanEx()
nameEx("weibull_hist")
### * weibull_hist

flush(stderr()); flush(stdout())

### Name: weibull_hist
### Title: Example data, simulated from a Weibull distribution
### Aliases: weibull_hist
### Keywords: datasets

### ** Examples

data(weibull_cc)
data(weibull_hist)
weibull_hist$X_trt <- 0
survival_model <- survival::survfit(survival::Surv(tte, event) ~ X_trt, 
                                    data = rbind(weibull_cc, 
                                    weibull_hist))
line_colors <- c("blue", "red", "green")  # Adjust colors as needed
line_types <- 1:length(unique(weibull_cc$X_trt))
plot(survival_model, col = line_colors, lty = line_types, 
     xlab = "Time (tte)", ylab = "Survival Probability", 
     main = "Kaplan-Meier Survival Curves by Treatment")



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

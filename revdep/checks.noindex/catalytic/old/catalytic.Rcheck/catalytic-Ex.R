pkgname <- "catalytic"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('catalytic')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("cat_cox")
### * cat_cox

flush(stderr()); flush(stdout())

### Name: cat_cox
### Title: Catalytic Cox Proportional Hazards Model (COX) Fitting Function
###   with Fixed Tau
### Aliases: cat_cox

### ** Examples

library(survival)
data("cancer")
cancer$status[cancer$status == 1] <- 0
cancer$status[cancer$status == 2] <- 1

cat_init <- cat_cox_initialization(
  formula = Surv(time, status) ~ 1, # formula for simple model
  data = cancer,
  syn_size = 100, # Synthetic data size
  hazard_constant = 0.1, # Hazard rate value
  entry_points = rep(0, nrow(cancer)), # Entry points of each observation
  x_degree = rep(1, ncol(cancer) - 2), # Degrees for polynomial expansion of predictors
  resample_only = FALSE, # Whether to perform resampling only
  na_replace = stats::na.omit # How to handle NA values in data
)

cat_model_cre <- cat_cox(
  formula = ~.,
  cat_init = cat_init, # Only accept object generated from `cat_cox_initialization`
  tau = 1, # Weight for synthetic data
  method = "CRE", # Choose from `"CRE"` or `"WME"`
  init_coefficients = rep(0, ncol(cat_init$x)), # Initial coefficient values (Only for `CRE`)
  tol = 1e-1, # Tolerance for convergence criterion  (Only for `CRE`)
  max_iter = 3 # Maximum number of iterations for convergence  (Only for `CRE`)
)
cat_model_cre

cat_model_wme <- cat_cox(
  formula = ~.,
  cat_init = cat_init, # Only accept object generated from `cat_cox_initialization`
  tau = 1, # Weight for synthetic data
  method = "WME"
)
cat_model_wme



cleanEx()
nameEx("cat_cox_bayes")
### * cat_cox_bayes

flush(stderr()); flush(stdout())

### Name: cat_cox_bayes
### Title: Bayesian Estimation for Catalytic Cox Proportional-Hazards Model
###   (COX) with Fixed tau
### Aliases: cat_cox_bayes

### ** Examples




cleanEx()
nameEx("cat_cox_bayes_joint")
### * cat_cox_bayes_joint

flush(stderr()); flush(stdout())

### Name: cat_cox_bayes_joint
### Title: Bayesian Estimation for Catalytic Cox Proportional-Hazards Model
###   (COX) with adaptive tau
### Aliases: cat_cox_bayes_joint

### ** Examples




cleanEx()
nameEx("cat_cox_initialization")
### * cat_cox_initialization

flush(stderr()); flush(stdout())

### Name: cat_cox_initialization
### Title: Initialization for Catalytic Cox proportional hazards model
###   (COX)
### Aliases: cat_cox_initialization

### ** Examples

library(survival)
data("cancer")
cancer$status[cancer$status == 1] <- 0
cancer$status[cancer$status == 2] <- 1

cat_init <- cat_cox_initialization(
  formula = Surv(time, status) ~ 1, # formula for simple model
  data = cancer,
  syn_size = 100, # Synthetic data size
  hazard_constant = NULL, # Hazard rate value
  entry_points = rep(0, nrow(cancer)), # Entry points of each observation
  x_degree = rep(1, ncol(cancer) - 2), # Degrees for polynomial expansion of predictors
  resample_only = FALSE, # Whether to perform resampling only
  na_replace = stats::na.omit # How to handle NA values in data
)
cat_init



cleanEx()
nameEx("cat_cox_tune")
### * cat_cox_tune

flush(stderr()); flush(stdout())

### Name: cat_cox_tune
### Title: Catalytic Cox Proportional-Hazards Model (COX) Fitting Function
###   by Tuning tau from a Sequence of tau Values
### Aliases: cat_cox_tune

### ** Examples

library(survival)
data("cancer")
cancer$status[cancer$status == 1] <- 0
cancer$status[cancer$status == 2] <- 1

cat_init <- cat_cox_initialization(
  formula = Surv(time, status) ~ 1, # formula for simple model
  data = cancer,
  syn_size = 100, # Synthetic data size
  hazard_constant = 0.1, # Hazard rate value
  entry_points = rep(0, nrow(cancer)), # Entry points of each observation
  x_degree = rep(1, ncol(cancer) - 2), # Degrees for polynomial expansion of predictors
  resample_only = FALSE, # Whether to perform resampling only
  na_replace = stats::na.omit # How to handle NA values in data
)

cat_model <- cat_cox_tune(
  formula = ~., # Should at least include response variables
  cat_init = cat_init, # Only accept object generated from `cat_cox_initialization`
  tau_seq = c(1, 2), # Vector of weights for synthetic data
  cross_validation_fold_num = 5 # number of folds for cross-validation
)
cat_model



cleanEx()
nameEx("cat_glm")
### * cat_glm

flush(stderr()); flush(stdout())

### Name: cat_glm
### Title: Catalytic Generalized Linear Models (GLMs) Fitting Function with
###   Fixed Tau
### Aliases: cat_glm

### ** Examples

gaussian_data <- data.frame(
  X1 = stats::rnorm(10),
  X2 = stats::rnorm(10),
  Y = stats::rnorm(10)
)

cat_init <- cat_glm_initialization(
  formula = Y ~ 1, # formula for simple model
  data = gaussian_data,
  syn_size = 100, # Synthetic data size
  custom_variance = NULL, # User customized variance value
  gaussian_known_variance = TRUE, # Indicating whether the data variance is known
  x_degree = c(1, 1), # Degrees for polynomial expansion of predictors
  resample_only = FALSE, # Whether to perform resampling only
  na_replace = stats::na.omit # How to handle NA values in data
)

cat_model <- cat_glm(
  formula = ~.,
  cat_init = cat_init, # Only accept object generated from `cat_glm_initialization`
  tau = 1 # Weight for synthetic data
)
cat_model



cleanEx()
nameEx("cat_glm_bayes")
### * cat_glm_bayes

flush(stderr()); flush(stdout())

### Name: cat_glm_bayes
### Title: Bayesian Estimation for Catalytic Generalized Linear Models
###   (GLMs) with Fixed tau
### Aliases: cat_glm_bayes

### ** Examples




cleanEx()
nameEx("cat_glm_bayes_joint")
### * cat_glm_bayes_joint

flush(stderr()); flush(stdout())

### Name: cat_glm_bayes_joint
### Title: Bayesian Estimation for Catalytic Generalized Linear Models
###   (GLMs) with adaptive tau
### Aliases: cat_glm_bayes_joint

### ** Examples




cleanEx()
nameEx("cat_glm_bayes_joint_gibbs")
### * cat_glm_bayes_joint_gibbs

flush(stderr()); flush(stdout())

### Name: cat_glm_bayes_joint_gibbs
### Title: Bayesian Estimation with Gibbs Sampling for Catalytic
###   Generalized Linear Models (GLMs) Binomial Family for Coefficients and
###   tau
### Aliases: cat_glm_bayes_joint_gibbs

### ** Examples




cleanEx()
nameEx("cat_glm_initialization")
### * cat_glm_initialization

flush(stderr()); flush(stdout())

### Name: cat_glm_initialization
### Title: Initialization for Catalytic Generalized Linear Models (GLMs)
### Aliases: cat_glm_initialization

### ** Examples

gaussian_data <- data.frame(
  X1 = stats::rnorm(10),
  X2 = stats::rnorm(10),
  Y = stats::rnorm(10)
)

cat_init <- cat_glm_initialization(
  formula = Y ~ 1, # formula for simple model
  data = gaussian_data,
  syn_size = 100, # Synthetic data size
  custom_variance = NULL, # User customized variance value
  gaussian_known_variance = TRUE, # Indicating whether the data variance is known
  x_degree = c(1, 1), # Degrees for polynomial expansion of predictors
  resample_only = FALSE, # Whether to perform resampling only
  na_replace = stats::na.omit # How to handle NA values in data
)
cat_init



cleanEx()
nameEx("cat_glm_tune")
### * cat_glm_tune

flush(stderr()); flush(stdout())

### Name: cat_glm_tune
### Title: Catalytic Generalized Linear Models (GLMs) Fitting Function by
###   Tuning tau from a Sequence of tau Values
### Aliases: cat_glm_tune

### ** Examples

gaussian_data <- data.frame(
  X1 = stats::rnorm(10),
  X2 = stats::rnorm(10),
  Y = stats::rnorm(10)
)

cat_init <- cat_glm_initialization(
  formula = Y ~ 1, # formula for simple model
  data = gaussian_data,
  syn_size = 100, # Synthetic data size
  custom_variance = NULL, # User customized variance value
  gaussian_known_variance = TRUE, # Indicating whether the data variance is known
  x_degree = c(1, 1), # Degrees for polynomial expansion of predictors
  resample_only = FALSE, # Whether to perform resampling only
  na_replace = stats::na.omit # How to handle NA values in data
)

cat_model <- cat_glm_tune(
  formula = ~.,
  cat_init = cat_init, # Only accept object generated from `cat_glm_initialization`
  risk_estimate_method = "parametric_bootstrap",
  discrepancy_method = "mean_square_error",
  tau_seq = c(1, 2), # Weight for synthetic data
  tau_0 = 2,
  parametric_bootstrap_iteration_times = 20, # Number of bootstrap iterations
  cross_validation_fold_num = 5 # Number of folds
)
cat_model



cleanEx()
nameEx("cat_lmm")
### * cat_lmm

flush(stderr()); flush(stdout())

### Name: cat_lmm
### Title: Catalytic Linear Mixed Model (LMM) Fitting Function with fixed
###   tau
### Aliases: cat_lmm

### ** Examples

data(mtcars)
cat_init <- cat_lmm_initialization(
  formula = mpg ~ wt + (1 | cyl), # formula for simple model
  data = mtcars,
  x_cols = c("wt"), # Fixed effects
  y_col = "mpg", # Response variable
  z_cols = c("disp", "hp", "drat", "qsec", "vs", "am", "gear", "carb"), # Random effects
  group_col = "cyl", # Grouping column
  syn_size = 100, # Synthetic data size
  resample_by_group = FALSE, # Resampling option
  resample_only = FALSE, # Resampling method
  na_replace = mean # NA replacement method
)

cat_model <- cat_lmm(
  cat_init = cat_init, # Only accept object generated from cat_lmm_initialization
  tau = 1, # Weight for synthetic data
  residual_variance_0 = 1, # Initial value for residual variance
  random_effect_variance_0 = 1, # Initial value for random effect variance
  coefs_0 = c(1), # Initial coefficient vector
  optimize_domain = c(0, 10), # Optimization range for residual and random effect variance
  max_iter = 2, # Maximum number of iterations for convergence
  tol = 1e-01 # Tolerance for convergence criterion
)
cat_model



cleanEx()
nameEx("cat_lmm_initialization")
### * cat_lmm_initialization

flush(stderr()); flush(stdout())

### Name: cat_lmm_initialization
### Title: Initialization for Catalytic Linear Mixed Model (LMM)
### Aliases: cat_lmm_initialization

### ** Examples

data(mtcars)
cat_init <- cat_lmm_initialization(
  formula = mpg ~ wt + (1 | cyl), # formula for simple model
  data = mtcars,
  x_cols = c("wt"), # Fixed effects
  y_col = "mpg", # Response variable
  z_cols = c("disp", "hp", "drat", "qsec", "vs", "am", "gear", "carb"), # Random effects
  group_col = "cyl", # Grouping column
  syn_size = 100, # Synthetic data size
  resample_by_group = FALSE, # Resampling option
  resample_only = FALSE, # Resampling method
  na_replace = mean # NA replacement method
)
cat_init



cleanEx()
nameEx("cat_lmm_tune")
### * cat_lmm_tune

flush(stderr()); flush(stdout())

### Name: cat_lmm_tune
### Title: Catalytic Linear Mixed Model (LMM) Fitting Function by Tuning
###   tau from a Sequence of tau Values
### Aliases: cat_lmm_tune

### ** Examples

data(mtcars)
cat_init <- cat_lmm_initialization(
  formula = mpg ~ wt + (1 | cyl), # formula for simple model
  data = mtcars,
  x_cols = c("wt"), # Fixed effects
  y_col = "mpg", # Response variable
  z_cols = c("disp", "hp", "drat", "qsec", "vs", "am", "gear", "carb"), # Random effects
  group_col = "cyl", # Grouping column
  syn_size = 100, # Synthetic data size
  resample_by_group = FALSE, # Resampling option
  resample_only = FALSE, # Resampling method
  na_replace = mean # NA replacement method
)

cat_model <- cat_lmm_tune(
  cat_init = cat_init, # Only accept object generated from cat_lmm_initialization
  tau_seq = c(1, 2), # Vector of weights for synthetic data
  cross_validation_fold_num = 3 # number of folds for cross-validation
)
cat_model




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

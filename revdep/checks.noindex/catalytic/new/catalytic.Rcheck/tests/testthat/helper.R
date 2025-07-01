# ----- GLM -----
#' Generate Mock Data for Generalized Linear Model (GLM) in Guassian Family
#'
#' This function creates a mock dataset suitable for testing Generalized Linear Model (GLM) in Guassian Family.
#' It generates covariates (`X1`, `X2`, `X3`) and a response vector (`Y`).
#'
#' @param seed An integer value to set the random seed for reproducibility. Default is 1.
#'
#' @return A data frame containing columns `X1`, `X2`, `X3` (covariates), `Y` (response).
mock_glm_gaussian_data <- function(seed = 1) {
  set.seed(seed)
  n <- 10
  return(data.frame(
    X1 = rnorm(n),
    X2 = rnorm(n),
    X3 = rnorm(n),
    Y = rnorm(n)
  ))
}

#' Mock Initialization Object for Catalytic Generalized Linear Model (GLM) in Gaussian Family
#'
#' This function sets up a mock a initialization object for catalytic Generalized Linear Model (GLM)
#' in Guassian family using `mocked_glm_data` as input. It prepares the required data and parameters to initialize the
#' `cat_glm_initialization` function with a predefined model structure, specifying the
#' response and predictor variables.
#'
#' @param mocked_glm_gaussian_data A data frame with mock data specifically formatted for catalytic GLM initialization in Gaussian family.
#'   It should contain covariates (`X1`, `X2`, `X3`) and a response vector (`Y`). This data serves as the input for initializing the catalytic GLM.
#' @param ... Additional arguments passed to the `cat_glm_initialization` function.
#'
#' @return A structured list or object from `cat_glm_initialization`, set up with the provided mock data and model parameters.
#'   The model includes all obervation data and is configured to generate synthetic data
#'   with a sample size specified by `syn_size`. More details on `?cat_glm_initialization`.
mock_cat_glm_gaussian_initialization <- function(mocked_glm_gaussian_data, ...) {
  return(
    cat_glm_initialization(
      formula = Y ~ 1,
      family = gaussian,
      data = mocked_glm_gaussian_data,
      syn_size = 100,
      ...
    )
  )
}

#' Generate Mock Data for Generalized Linear Model (GLM) in Binomial Family
#'
#' This function creates a mock dataset suitable for testing Generalized Linear Model (GLM) in Binomial Family.
#' It generates covariates (`X1`, `X2`, `X3`) and a response vector (`Y`).
#'
#' @param seed An integer value to set the random seed for reproducibility. Default is 1.
#'
#' @return A data frame containing columns `X1`, `X2`, `X3` (covariates), `Y` (response).
mock_glm_binomial_data <- function(seed = 1) {
  set.seed(seed)
  n <- 10
  return(data.frame(
    X1 = rnorm(n),
    X2 = rnorm(n),
    X3 = rnorm(n),
    Y = as.integer(rbinom(n, 1, 0.5))
  ))
}

#' Mock Initialization Object for Catalytic Generalized Linear Model (GLM) in Binomial Family
#'
#' This function sets up a mock a initialization object for catalytic Generalized Linear Model (GLM)
#'  in Binomial family using `mocked_glm_data` as input. It prepares the required data and parameters to initialize the
#' `cat_glm_initialization` function with a predefined model structure, specifying the
#' response and predictor variables.
#'
#' @param mocked_glm_binomial_data A data frame with mock data specifically formatted for catalytic GLM initialization in Binomial family.
#'   It should contain covariates (`X1`, `X2`, `X3`) and a response vector (`Y`). This data serves as the input for initializing the catalytic GLM.
#' @param ... Additional arguments passed to the `cat_glm_initialization` function.
#'
#' @return A structured list or object from `cat_glm_initialization`, set up with the provided mock data and model parameters.
#'   The model includes all obervation data and is configured to generate synthetic data
#'   with a sample size specified by `syn_size`. More details on `?cat_glm_initialization`.
mock_cat_glm_binomial_initialization <- function(mocked_glm_binomial_data, ...) {
  return(
    cat_glm_initialization(
      formula = Y ~ 1,
      family = binomial,
      data = mocked_glm_binomial_data,
      syn_size = 100,
      ...
    )
  )
}

# ----- COX -----
#' Generate Mock Data for Cox Proportional Hazards Model
#'
#' This function creates a mock dataset suitable for testing Cox proportional hazards models.
#' It generates covariates (`X1`, `X2`, `X3`), a survival time (`time`), and a censoring indicator (`status`).
#'
#' @param seed An integer value to set the random seed for reproducibility. Default is 1.
#'
#' @return A data frame containing columns `X1`, `X2`, `X3` (covariates), `time` (survival time),
#'   and `status` (censoring indicator, where 1 indicates an event and 0 indicates censoring).
mock_cox_data <- function(seed = 1) {
  set.seed(seed)
  n <- 10
  return(data.frame(
    X1 = rnorm(n),
    X2 = rnorm(n),
    X3 = rnorm(n),
    time = runif(n),
    status = rbinom(n, 1, 0.5)
  ))
}

#' Mock Initialization Object for Catalytic Cox Proportional Hazards Model (COX)
#'
#' This function sets up a mock a initialization object for catalytic Cox Proportional Hazards Model (COX) using
#' `mocked_cox_data` as input. It prepares the required data and parameters to initialize the
#' `cat_cox_initialization` function with a predefined model structure, specifying the
#' response and predictor variables.
#'
#' @param mocked_cox_data A data frame with mock data specifically formatted for catalytic COX initialization.
#'   It should contain columns for fixed effects (`X1`, `X2`, `X3`), a survival time (`time`),
#'   and a censoring indicator (`status`). This data serves as the input for initializing the catalytic COX.
#' @param ... Additional arguments passed to the `cat_cox_initialization` function.
#'
#' @return A structured list or object from `cat_cox_initialization`, set up with the provided mock data and model parameters.
#'   The model includes all obervation data and is configured to generate synthetic data
#'   with a sample size specified by `syn_size`. More details on `?cat_cox_initialization`.
mock_cat_cox_initialization <- function(mocked_cox_data, ...) {
  return(
    cat_cox_initialization(
      formula = survival::Surv(time, status) ~ 1,
      data = mocked_cox_data,
      syn_size = 100,
      ...
    )
  )
}

# ----- LMM -----
#' Generate Mock Data for Linear Mixed Model
#'
#' This function creates a mock dataset for testing a linear mixed model, with both fixed
#' and random effect variables, as well as a grouping factor.
#'
#' @param seed An integer value to set the random seed for reproducibility. Default is 1.
#'
#' @return A data frame containing columns `X1`, `X2`, `X3` (fixed effects), `Y` (response variable),
#'   `Z1`, `Z2`, `Z3` (random effects), and `group` (grouping factor).
mock_lmm_data <- function(seed = 1) {
  set.seed(seed)
  n <- 10
  return(data.frame(
    X1 = rnorm(n),
    X2 = rnorm(n),
    X3 = rnorm(n),
    Y = rnorm(n),
    Z1 = rnorm(n),
    Z2 = rnorm(n),
    Z3 = rnorm(n),
    group = sample(1:3, n, replace = TRUE)
  ))
}

#' Mock Initialization Object for Catalytic Linear Mixed Model (LMM)
#'
#' This function sets up a mock a initialization object for catalytic linear mixed models (LMM) using
#' `mocked_lmm_data` as input. It prepares the required data and parameters to initialize the
#' `cat_lmm_initialization` function with a predefined model structure, specifying the
#' response and predictor variables.
#'
#' @param mocked_lmm_data A data frame with mock data specifically formatted for catalytic LMM initialization.
#'   It should contain columns for fixed effects (`X1`, `X2`, `X3`), random effects (`Z1`, `Z2`, `Z3`),
#'   the response variable (`Y`), and a grouping factor (`group`). This data serves as the input
#'   for initializing the catalytic LMM.
#' @param ... Additional arguments passed to the `cat_lmm_initialization` function.
#'
#' @return A structured list or object from `cat_lmm_initialization`, set up with the provided mock data and model parameters.
#'   The model includes both fixed and random effects and is configured to generate synthetic data
#'   with a sample size specified by `syn_size`. More details on `?cat_lmm_initialization`.
mock_cat_lmm_initialization <- function(mocked_lmm_data, ...) {
  return(cat_lmm_initialization(
    formula = Y ~ X1 + X2 + X3,
    data = mocked_lmm_data,
    x_cols = c("X1", "X2", "X3"),
    z_cols = c("Z1", "Z2", "Z3"),
    y_col = c("Y"),
    group_col = c("group"),
    syn_size = 100,
    ...
  ))
}

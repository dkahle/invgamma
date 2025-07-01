pkgname <- "DTEBOP2"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('DTEBOP2')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("Experimental")
### * Experimental

flush(stderr()); flush(stdout())

### Name: Experimental
### Title: Nivolumab KM data used for reconstructing PFS KM curve
### Aliases: Experimental
### Keywords: datasets

### ** Examples

data(Experimental)
head(Experimental)



cleanEx()
nameEx("SOC")
### * SOC

flush(stderr()); flush(stdout())

### Name: SOC
### Title: Docetaxel KM data used for reconstructing PFS KM curve
### Aliases: SOC
### Keywords: datasets

### ** Examples

data(SOC)
head(SOC)



cleanEx()
nameEx("TTOT_fun")
### * TTOT_fun

flush(stderr()); flush(stdout())

### Name: TTOT_fun
### Title: Compute the Total Time-on-Test (TTOT) Statistic
### Aliases: TTOT_fun

### ** Examples

# Generate sample survival data
set.seed(123)
sim_time <- generate_pe(n = 20, t = 1, lambda1 = 1, lambda2 = 0.5)
sim_status <- sample(c(0, 1), 20, replace = TRUE)
Data <- data.frame(Time = sim_time, Status = sim_status)
# Compute TTOT when time = 1
TTOT_fun(Data, t = 1)



cleanEx()
nameEx("Two_stage_sample_size")
### * Two_stage_sample_size

flush(stderr()); flush(stdout())

### Name: Two_stage_sample_size
### Title: Calculate Sample Sizes for a Two-Stage Trial Design.
### Aliases: Two_stage_sample_size

### ** Examples

# Define design parameters
median.1 <- 4       # Median survival for standard-of-care
median.2 <- 6       # Median survival for experimental arm
L <- 2              # Lower bound of separation time
U <- 2.5            # Upper bound of separation time
S_likely <- 2.28    # Most likely separation time
rate <- 6           # Accrual rate (patients/month)
FUP <- 6            # Follow-up duration (months)
err1 <- 0.1         # Type I error
err2 <- 0.15        # Type II error




cleanEx()
nameEx("conduct")
### * conduct

flush(stderr()); flush(stdout())

### Name: conduct
### Title: Computes the hazard ratio between post- and pre-separation
###   periods, provides the 95% Bayesian credible interval, and makes a
###   Go/No-Go decision based on the trial data. This function is intended
###   for interim and final analyses in two-arm survival trials with
###   delayed treatment effects.
### Aliases: conduct

### ** Examples




cleanEx()
nameEx("event_fun")
### * event_fun

flush(stderr()); flush(stdout())

### Name: event_fun
### Title: Return expected event number at interim analysis and final
###   analysis.
### Aliases: event_fun

### ** Examples




cleanEx()
nameEx("generate_pe")
### * generate_pe

flush(stderr()); flush(stdout())

### Name: generate_pe
### Title: Generate event times from a piecewise exponential distribution.
### Aliases: generate_pe

### ** Examples

set.seed(42)
generate_pe(1000, 1, 1, 0.5)



cleanEx()
nameEx("get.optimal_2arm_piecewise")
### * get.optimal_2arm_piecewise

flush(stderr()); flush(stdout())

### Name: get.optimal_2arm_piecewise
### Title: Obtain Optimal Design Parameters for DTE-BOP2 Design
### Aliases: get.optimal_2arm_piecewise

### ** Examples

# Define design and simulation parameters
median.1 <- 6
median.2 <- 10
L <- 2.6
U <- 3.4
S_likely <- 2.7
trunc.para <- c(1, 1)
rate <- 3
FUP <- 9
err1 <- 0.15
n.interim <- c(30, 50)  # Each arm has 30 patients at interim and 50 at final



cleanEx()
nameEx("getoc_2arm_piecewise")
### * getoc_2arm_piecewise

flush(stderr()); flush(stdout())

### Name: getoc_2arm_piecewise
### Title: Compute Operating Characteristics for Two-Arm Piecewise
###   Exponential Designs
### Aliases: getoc_2arm_piecewise

### ** Examples

# Define trial parameters
median.1 <- 6
median.2 <- 9
trunc.para <- c(1, 1)
rate <- 3
FUP <- 9
n.interim <- c(30, 50)  # Each arm: 30 pts at interim, 50 pts at final
# Run operating characteristics computation
getoc_2arm_piecewise(median.true = c(median.1, median.2),
Uniform = FALSE,lambda = 0.9,gamma = 1,n.interim = n.interim,
  L = 3,U = 3,S_likely = 3,FUP = FUP,trunc.para = trunc.para,
  rate = rate,nsim = 1)



cleanEx()
nameEx("rtrunc_gamma")
### * rtrunc_gamma

flush(stderr()); flush(stdout())

### Name: rtrunc_gamma
### Title: Generate Random Variables from a Truncated Gamma Distribution.
### Aliases: rtrunc_gamma

### ** Examples

rtrunc_gamma(10, 1, 1, 1, 1)



cleanEx()
nameEx("trunc_gamma_para")
### * trunc_gamma_para

flush(stderr()); flush(stdout())

### Name: trunc_gamma_para
### Title: Estimate Shape and Scale Parameters for Truncated Gamma
###   Distribution
### Aliases: trunc_gamma_para

### ** Examples

# Define expert-provided summary data
expert_data_correct <- list(
  list(mean = 2.2, median = 2.27, sd = NULL, q25 = NULL, q975 = NULL),  # Expert A
  list(mean = 2.1, median = 2.3,  sd = NULL, q25 = NULL, q975 = NULL),  # Expert B
  list(mean = NULL, median = 2.31, sd = NULL, q25 = NULL, q975 = NULL)  # Expert C
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

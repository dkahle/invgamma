pkgname <- "bartcs"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('bartcs')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("bart")
### * bart

flush(stderr()); flush(stdout())

### Name: bart
### Title: Fit BART models to select confounders and estimate treatment
###   effect
### Aliases: bart separate_bart single_bart

### ** Examples

data(ihdp, package = "bartcs")
single_bart(
  Y               = ihdp$y_factual,
  trt             = ihdp$treatment,
  X               = ihdp[, 6:30],
  num_tree        = 10,
  num_chain       = 2,
  num_post_sample = 20,
  num_burn_in     = 10,
  verbose         = FALSE
)
separate_bart(
  Y               = ihdp$y_factual,
  trt             = ihdp$treatment,
  X               = ihdp[, 6:30],
  num_tree        = 10,
  num_chain       = 2,
  num_post_sample = 20,
  num_burn_in     = 10,
  verbose         = FALSE
)



cleanEx()
nameEx("count_omp_thread")
### * count_omp_thread

flush(stderr()); flush(stdout())

### Name: count_omp_thread
### Title: Count the number of OpenMP threads for parallel computation
### Aliases: count_omp_thread

### ** Examples

count_omp_thread()




cleanEx()
nameEx("plot.bartcs")
### * plot.bartcs

flush(stderr()); flush(stdout())

### Name: plot.bartcs
### Title: Draw plot for 'bartcs' object
### Aliases: plot.bartcs

### ** Examples

data(ihdp, package = "bartcs")
x <- single_bart(
  Y               = ihdp$y_factual,
  trt             = ihdp$treatment,
  X               = ihdp[, 6:30],
  num_tree        = 10,
  num_chain       = 2,
  num_post_sample = 20,
  num_burn_in     = 10,
  verbose         = FALSE
)

# PIP plot
plot(x, method = "pip")
plot(x, method = "pip", top_n = 10)
plot(x, method = "pip", threshold = 0.5)
# Check `?ggcharts::bar_chart` for other possible arguments.

# trace plot
plot(x, method = "trace")
plot(x, method = "trace", "Y1")
plot(x, method = "trace", "dir_alpha")




cleanEx()
nameEx("summary.bartcs")
### * summary.bartcs

flush(stderr()); flush(stdout())

### Name: summary.bartcs
### Title: Summary for 'bartcs' object
### Aliases: summary.bartcs

### ** Examples

data(ihdp, package = "bartcs")
x <- single_bart(
  Y               = ihdp$y_factual,
  trt             = ihdp$treatment,
  X               = ihdp[, 6:30],
  num_tree        = 10,
  num_chain       = 2,
  num_post_sample = 20,
  num_burn_in     = 10,
  verbose         = FALSE
)
summary(x)




cleanEx()
nameEx("synthetic_data")
### * synthetic_data

flush(stderr()); flush(stdout())

### Name: synthetic_data
### Title: Synthetic dataset for simulation
### Aliases: synthetic_data

### ** Examples

synthetic_data()




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

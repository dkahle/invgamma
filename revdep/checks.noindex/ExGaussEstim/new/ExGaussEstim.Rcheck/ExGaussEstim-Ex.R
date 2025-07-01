pkgname <- "ExGaussEstim"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('ExGaussEstim')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("BayesianExgaussian")
### * BayesianExgaussian

flush(stderr()); flush(stdout())

### Name: BayesianExgaussian
### Title: Bayesian Ex-gaussian Estimate
### Aliases: BayesianExgaussian

### ** Examples





cleanEx()
nameEx("QMLEEstim")
### * QMLEEstim

flush(stderr()); flush(stdout())

### Name: QMLEEstim
### Title: Ex-Gaussian Quantile Maximum Likelihood Estimate
### Aliases: QMLEEstim

### ** Examples

library(gamlss.dist)
set.seed(2703)
data<-rexGAUS(n=100, mu = 500, sigma = 150, nu = 100)
QMLEEstim(data, 'NEMD')





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

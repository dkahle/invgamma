pkgname <- "TruncExpFam"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('TruncExpFam')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("dtrunc")
### * dtrunc

flush(stderr()); flush(stdout())

### Name: dtruncbeta
### Title: Probability Density Function
### Aliases: dtruncbeta dtruncbinom dtruncchisq dtrunccontbern dtrunc
###   dtruncexp dtruncgamma dtruncinvgamma dtruncinvgauss dtrunclnorm
###   dtrunc.trunc_nbinom dtruncnbinom dtruncnorm dtruncpois

### ** Examples

# Using the output of rtrunc
y <- rtrunc(50, mean = 5, sd = 2)
dtrunc(y, eta = c(0, -1))

# Directly-inputting values
dtruncnorm(y = c(5, 0, -10), eta = c(0, -0.05))



cleanEx()
nameEx("empiricalParameters")
### * empiricalParameters

flush(stderr()); flush(stdout())

### Name: empiricalParameters
### Title: Calculate empirical parameters
### Aliases: empiricalParameters

### ** Examples

# Normal distribution
sampNorm <- rtrunc(50, mean = 5, sd = 2)
empiricalParameters(sampNorm)

# Poisson distribution
sampPois <- rtrunc(10, lambda = 100, family = "Poisson")
empiricalParameters(sampPois)



cleanEx()
nameEx("empiricalParameters.numeric")
### * empiricalParameters.numeric

flush(stderr()); flush(stdout())

### Name: empiricalParameters.numeric
### Title: Extract parameters
### Aliases: empiricalParameters.numeric

### ** Examples

# Some random data
x <- c(
  4, 3, 6, 3, 3, 3, 3, 4, 3, 2, 3, 0, 4, 2, 0, 1, 4, 3, 0, 0, 2, 3, 0, 3, 7,
  2, 1, 1, 2, 3, 2, 3, 3, 3, 2, 2, 2, 0, 2, 0, 2, 1, 0, 2, 3, 1, 0, 4, 2, 2,
  0, 1, 1, 1, 2, 2, 3, 1, 3, 1, 1, 0, 3, 3, 2, 0, 2, 2, 3, 0, 2, 1, 0, 0, 1,
  0, 2, 4, 2, 3, 3, 0, 1, 0, 5, 2, 4, 2, 7, 4, 4, 1, 2, 4, 3, 2, 4, 3, 1, 3
)

# Extracting parameters under different distribution assumptions
empiricalParameters(x, family = "normal")
empiricalParameters(x, family = "normal", natural = TRUE)
empiricalParameters(x, family = "binomial", nsize = max(x))
empiricalParameters(x, family = "poisson", natural = FALSE)
empiricalParameters(x, family = "poisson", natural = TRUE)



cleanEx()
nameEx("mlEstimationTruncDist")
### * mlEstimationTruncDist

flush(stderr()); flush(stdout())

### Name: mlEstimationTruncDist
### Title: ML Estimation of Distribution Parameters
### Aliases: mlEstimationTruncDist

### ** Examples

sample_size <- 1000
# Normal
sample.norm <- rtrunc(n = sample_size, mean = 2, sd = 1.5, a = -1)
mlEstimationTruncDist(
  sample.norm,
  y.min = -1, max.it = 500, delta = 0.33,
  print.iter = TRUE
)

# Log-Normal
sample.lognorm <- rtrunc(
  n = sample_size, family = "lognormal", meanlog = 2.5, sdlog = 0.5, a = 7
)
ml_lognormal <- mlEstimationTruncDist(
  sample.lognorm,
  y.min = 7, max.it = 500, tol = 1e-10, delta = 0.3,
  print.iter = FALSE
)
ml_lognormal

# Poisson
sample.pois <- rtrunc(
 n = sample_size, lambda = 10, a = 4, family = "Poisson"
)
mlEstimationTruncDist(
  sample.pois,
  y.min = 4, max.it = 500, delta = 0.33,
  print.iter = 5
)

# Gamma
sample.gamma <- rtrunc(
 n = sample_size, shape = 6, rate = 2, a = 2, family = "Gamma"
)
mlEstimationTruncDist(
  sample.gamma,
  y.min = 2, max.it = 1500, delta = 0.3,
  print.iter = 10
)

# Negative binomial
sample.nbinom <- rtruncnbinom(
 sample_size, size = 50, prob = .3, a = 100, b = 120
)
mlEstimationTruncDist(sample.nbinom, r=10)



cleanEx()
nameEx("natural2parameters")
### * natural2parameters

flush(stderr()); flush(stdout())

### Name: natural2parameters
### Title: Convert natural parameters to distribution parameters
### Aliases: natural2parameters

### ** Examples

samp <- rtrunc(n = 100, lambda = 2, family = "Poisson")
lambda_hat <- empiricalParameters(samp)
eta_hat <- parameters2natural(lambda_hat)
natural2parameters(eta_hat)  # yields back lambda



cleanEx()
nameEx("parameters2natural")
### * parameters2natural

flush(stderr()); flush(stdout())

### Name: parameters2natural
### Title: Convert distribution parameters to natural parameters
### Aliases: parameters2natural

### ** Examples

# Poisson distribution
samp <- rtrunc(n = 100, lambda = 2, family = "Poisson")
parameters2natural(empiricalParameters(samp))



cleanEx()
nameEx("probdist-class")
### * probdist-class

flush(stderr()); flush(stdout())

### Name: probdist-class
### Title: Probability distribution class
### Aliases: probdist-class probdist

### ** Examples

probdist(shape = 2, scale = .25, family = "gamma")
probdist(mean = 2, sd = 10, family = "normal")
probdist(eta1 = 2, eta2 = -1, family = "normal")



cleanEx()
nameEx("ptrunc")
### * ptrunc

flush(stderr()); flush(stdout())

### Name: ptrunc
### Title: Cumulative Distribution Function
### Aliases: ptrunc ptruncnorm ptruncbeta ptruncbinom ptruncpois
###   ptruncchisq ptrunccontbern ptruncexp ptruncgamma ptruncinvgamma
###   ptruncinvgauss ptrunclnorm ptruncnbinom

### ** Examples

ptrunc(0)
ptrunc(6, family = "gaussian", mean = 5, sd = 10, b = 7)
pnorm(6, mean = 5, sd = 10) # for comparison



cleanEx()
nameEx("qtrunc")
### * qtrunc

flush(stderr()); flush(stdout())

### Name: qtrunc
### Title: Quantile Function
### Aliases: qtrunc qtruncbeta qtruncbinom qtruncchisq qtrunccontbern
###   qtruncexp qtruncgamma qtruncinvgamma qtruncinvgauss qtrunclnorm
###   qtruncnbinom qtruncnorm qtruncpois

### ** Examples

qtrunc(0.75)
qtrunc(.2, family = "gaussian", mean = 5, sd = 10, b = 7)
qnorm(.2, mean = 5, sd = 10) # for comparison



cleanEx()
nameEx("rtrunc")
### * rtrunc

flush(stderr()); flush(stdout())

### Name: rtruncbeta
### Title: The Truncated Exponential Family
### Aliases: rtruncbeta rtruncbinom rtruncchisq rtrunccontbern rtruncexp
###   rtruncgamma rtruncinvgamma rtruncinvgauss rtrunclnorm rtruncnbinom
###   rtruncnorm rtruncpois rtrunc rtrunc_direct

### ** Examples

# Truncated binomial distribution
sample.binom <- rtrunc(
  100, family = "binomial", prob = 0.6, size = 20, a = 4, b = 10
)
sample.binom
plot(
  table(sample.binom), ylab = "Frequency", main = "Freq. of sampled values"
)

# Truncated Log-Normal distribution
sample.lognorm <- rtrunc(
  n = 100, family = "lognormal", meanlog = 2.5, sdlog = 0.5, a = 7
)
summary(sample.lognorm)

hist(
  sample.lognorm,
  nclass = 35, xlim = c(0, 60), freq = FALSE,
  ylim = c(0, 0.15)
)

# Normal distribution
sample.norm <- rtrunc(n = 100, mean = 2, sd = 1.5, a = -1)
head(sample.norm)
hist(sample.norm, nclass = 25)

# Gamma distribution
sample.gamma <- rtrunc(n = 100, family = "gamma", shape = 6, rate = 2, a = 2)
hist(sample.gamma, nclass = 15)

# Poisson distribution
sample.pois <- rtrunc(n = 10, family = "poisson", lambda = 10, a = 4)
sample.pois
plot(table(sample.pois))



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

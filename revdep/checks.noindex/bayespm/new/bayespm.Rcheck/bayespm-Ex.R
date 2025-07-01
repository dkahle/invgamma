pkgname <- "bayespm"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('bayespm')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("ECE")
### * ECE

flush(stderr()); flush(stdout())

### Name: ECE
### Title: ECE dataset for the PCC process for Poisson with rate parameter
###   unknown
### Aliases: ECE

### ** Examples

# Loading data
attach(ECE)

# Plotting data
graphpar <- par( oma = c(1,3,2,3) )
plot( 1:length(defect_counts), defect_counts/inspected_units, type = "b", lty = 1,
      xlab = "Observations", ylab = "", xlim = c(0, 25), ylim = c(1.5, 10.5),
      lwd = 1, pch = 16, axes = FALSE, yaxs = "i", main = "ECE dataset" )

# Adding points
points( 1:length(defect_counts), inspected_units, type = "b",
        lty = 2, lwd = 1, pch = 21, col = "gray55" )
# Adding legend
legend( "topleft", legend=c(expression(paste(s[i])), expression(paste(x[i]/s[i])) ), bty = "n",
        cex = 0.8, lty = c(2, 1), lwd = 1, col = c ("gray55", "black") , pch = c(21, 16) )
# Adding axis with names
axis(1) ; axis(2) ; axis(4, col.axis = "gray55", col = "gray55")
mtext( "Number of Defects \n per unit", side = 2,  line = 2.2, cex = 0.9 )
mtext( "Inspected units", side = 4,  line = 2, cex = 0.9, col = "gray55" )
# Resetting graphical paramaters
par(graphpar)




graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("aPTT")
### * aPTT

flush(stderr()); flush(stdout())

### Name: aPTT
### Title: Dataset for PCC process for Normal with both parameters unknown
### Aliases: aPTT

### ** Examples

# Loading data
attach(aPTT)

# Plotting data
plot( 1:length(aPTT_current), aPTT_current, type = "b", lty = 1, xlab = "", ylab = "",
      ylim = c(27.3, 33.4), xlim = c(-30, 30), lwd = 1, pch = 16, axes = FALSE, yaxs = "i",
      main = "aPTT dataset" )

# x - axis for historical and current data
pastx <- c( -30, -20, -10, 0 ) ; currentx <- c( 0, 10, 20, 30 )
# Adding points
points( -length(aPTT_historical):(-1), aPTT_historical,
        type = "b", lty = 2, xlab = "", ylab = "", lwd = 1, pch = 21, col = "gray55" )
# Adding axis with names
axis(2)
mtext( "Current Data", side = 1, at = 15, line = 2.2, cex = 1.1 )
mtext( "Historical Data", side = 1, at = -15, line = 2.2, cex = 1.1, col = "gray55" )
mtext( "aPTT [sec]", side = 2, line = 2.2, cex = 1.1 )
axis( 1, at = currentx, labels = currentx )
axis( 1, at = pastx, labels = pastx, col.axis = "gray55", col = "gray55", lty = 2 )
segments( 0, 27.5, 0, 33.5, lwd = 1 )




cleanEx()
nameEx("betabinom_HM")
### * betabinom_HM

flush(stderr()); flush(stdout())

### Name: betabinom_HM
### Title: The Highest Mass (HM) interval of Beta-Binomial distribution.
### Aliases: betabinom_HM

### ** Examples

betabinom_HM(0.95, 10, 20, 180, plot = TRUE)




cleanEx()
nameEx("betanbinom_HM")
### * betanbinom_HM

flush(stderr()); flush(stdout())

### Name: betanbinom_HM
### Title: The Highest Mass (HM) interval of Beta-Negative Binomial
###   distribution.
### Aliases: betanbinom_HM

### ** Examples

betanbinom_HM(0.95, 5, 20, 80, plot = TRUE)




cleanEx()
nameEx("binom_PCC")
### * binom_PCC

flush(stderr()); flush(stdout())

### Name: binom_PCC
### Title: PCC for Binomial data with probability parameter unknown
### Aliases: binom_PCC

### ** Examples

# 30 Binomial observations introducing an outlier at the 15th observation
set.seed(10)
SimData <- rbinom( n = 30, size = 20, prob = 0.6 )
SimData[15] <- round( SimData[15] + 3*sqrt(20*0.6*0.4) )
binom_PCC( SimData, n = rep(20, 30) )



cleanEx()
nameEx("binom_PRC")
### * binom_PRC

flush(stderr()); flush(stdout())

### Name: binom_PRC
### Title: PRC for Binomial data with probability parameter unknown
### Aliases: binom_PRC

### ** Examples


# the PRC process for the first 30 data points in the third application in
# "Design and properties of the Predictive Ratio Cusum (PRC) control charts"


### HISTORICAL DATA (FIRST BATCH)
HD <- c( 3, 3, 1, 5, 2, 1, 3, 3, 3, 0, 2, 1, 2, 1, 4, 1, 1, 0, 3, 2, 4, 6, 0, 1, 3, 2, 2, 4, 2, 1 )

### Bernoulli trials
nn <- 50

N0 <- length(HD)
NN0 <- rep(50, N0)

binom_PRC(data = HD, n = NN0)


# the PRC process for the last 10 data points in the third application in
# "Design and properties of the Predictive Ratio Cusum (PRC) control charts"

### prior parameters before the first batch

a0 <- 1/2
b0 <- 1/2

### posterior parameters after the first batch

ap <- sum(HD) + a0
bp <- sum(NN0) - sum(HD) + b0

hl = 4.332 # the decision limit is derived by the function binom_PRC_h

### CURRENT DATA (SECOND BATCH)
CD <- c(2, 4, 5, 2, 4, 8, 4, 4, 8, 5)

N <- length(CD)
NN <- rep(50, N)

binom_PRC(data = CD, n = NN, a0 = ap, b0 = bp, h = hl)




cleanEx()
nameEx("binom_PRC_h")
### * binom_PRC_h

flush(stderr()); flush(stdout())

### Name: binom_PRC_h
### Title: Derivation of the decision limit for the PRC for Binomial data
###   with probability parameter unknown
### Aliases: binom_PRC_h

### ** Examples


binom_PRC_h(ARL_0 = NULL, FAP = 0.05, N = 20, n = 10, a0 = 20, b0 = 180)

# derivation of the decision limit of the third application in
# "Design and properties of the Predictive Ratio Cusum (PRC) control charts"

arl0 <- 400
ap <- 66.5
bp <- 1434.5
kl <- 2


# To replicate results from application set 'ARL0tol = .001' and 'it = 1e4'
binom_PRC_h(ARL_0 = arl0, ARL0tol = .01, k = kl, n = 50, a0 = ap, b0 = bp, it = 1e3)






cleanEx()
nameEx("compgamma_HD")
### * compgamma_HD

flush(stderr()); flush(stdout())

### Name: compgamma_HD
### Title: The Highest Density (HD) interval of Compound Gamma
###   distribution.
### Aliases: compgamma_HD

### ** Examples

compgamma_HD(0.95, 2, 10, 10, plot = TRUE)




cleanEx()
nameEx("gamma_PCC")
### * gamma_PCC

flush(stderr()); flush(stdout())

### Name: gamma_PCC
### Title: PCC for Gamma data with rate parameter unknown
### Aliases: gamma_PCC

### ** Examples

# 30 Gamma observations introducing an outlier of 3*sd at the 15th observation
set.seed(100)
out <- rgamma( n = 30, shape = 4, rate = 4 )
out[15] <- out[15] + 1.5
gamma_PCC( out, al = 4 )




cleanEx()
nameEx("gb2_HD")
### * gb2_HD

flush(stderr()); flush(stdout())

### Name: gb2_HD
### Title: The Highest Density (HD) interval of Generalized Beta of the
###   second kind distribution.
### Aliases: gb2_HD

### ** Examples

gb2_HD(0.95, 4, 6, 6, plot = TRUE)




cleanEx()
nameEx("invgamma_PCC")
### * invgamma_PCC

flush(stderr()); flush(stdout())

### Name: invgamma_PCC
### Title: PCC for Inverse-Gamma data with scale parameter unknown
### Aliases: invgamma_PCC

### ** Examples

# 30 Inverse-Gamma observations introducing an outlier at the 15th observation
set.seed(100)
SimData <- 1/rgamma(n = 30, shape = 3, rate = 2)
SimData[15] <- SimData[15] + 3
invgamma_PCC(SimData, al = 3)



cleanEx()
nameEx("lnorm1_PCC")
### * lnorm1_PCC

flush(stderr()); flush(stdout())

### Name: lnorm1_PCC
### Title: PCC for LogNormal data with scale parameter unknown
### Aliases: lnorm1_PCC

### ** Examples

set.seed(9)
SimData <- rlnorm(n = 30, meanlog = 0, sdlog = 1/2)
SimData[15] <- SimData[15] + 3*sqrt( exp(1/4)*( exp(1/4)-1 ) )
plot(SimData)
lnorm1_PCC(SimData, sdl = 1/2)



cleanEx()
nameEx("lnorm2_PCC")
### * lnorm2_PCC

flush(stderr()); flush(stdout())

### Name: lnorm2_PCC
### Title: PCC for LogNormal data with shape parameter unknown
### Aliases: lnorm2_PCC

### ** Examples

# 30 LogNormal observations introducing an outlier at the 15th observation
set.seed(1)
SimData <- rlnorm(n = 30, meanlog = 0, sdlog = 1/2)
SimData[15] <- SimData[15] + 3*sqrt( exp(1/4)*( exp(1/4)-1 ) )
plot(SimData)
lnorm2_PCC(SimData, ml = 0)



cleanEx()
nameEx("lnorm3_PCC")
### * lnorm3_PCC

flush(stderr()); flush(stdout())

### Name: lnorm3_PCC
### Title: PCC for LogNormal data with both parameters unknown
### Aliases: lnorm3_PCC

### ** Examples

# 30 LogNormal observations introducing an outlier at the 15th observation
ssddll <- 1/2
set.seed(9)
SimData <- rlnorm( n = 30, meanlog = 0, sdlog = ssddll)
SimData[15] <- SimData[15] + 3*sqrt( exp(ssddll^2)*( exp(ssddll^2)-1 ) )
plot(SimData)
lnorm3_PCC(SimData)



cleanEx()
nameEx("lnorm_HD")
### * lnorm_HD

flush(stderr()); flush(stdout())

### Name: lnorm_HD
### Title: The Highest Density (HD) interval of Lognormal distribution.
### Aliases: lnorm_HD

### ** Examples

lnorm_HD(0.95, 0, 1/2, plot = TRUE)




cleanEx()
nameEx("lt_HD")
### * lt_HD

flush(stderr()); flush(stdout())

### Name: lt_HD
### Title: The Highest Density (HD) interval of Logt distribution.
### Aliases: lt_HD

### ** Examples

lt_HD(0.95, 10, 0, 1/2, plot = TRUE)




cleanEx()
nameEx("nbinom_HM")
### * nbinom_HM

flush(stderr()); flush(stdout())

### Name: nbinom_HM
### Title: The Highest Mass (HM) interval of Beta-Negative Binomial
###   distribution.
### Aliases: nbinom_HM

### ** Examples

nbinom_HM(0.95, 4, 0.2, plot = TRUE)




cleanEx()
nameEx("nbinom_PCC")
### * nbinom_PCC

flush(stderr()); flush(stdout())

### Name: nbinom_PCC
### Title: PCC for Negative Binomial data with probability parameter
###   unknown
### Aliases: nbinom_PCC

### ** Examples

# 30 Negative Binomial observations introducing an outlier at the 15th observation
set.seed(5)
SimData <- rnbinom(n = 30, size = 10, prob = 0.9)
SimData[15] <- round( SimData[15] + 3*sqrt(10*0.1/(0.9^2)) )
nbinom_PCC(SimData, rl = 10)



cleanEx()
nameEx("norm1_PCC")
### * norm1_PCC

flush(stderr()); flush(stdout())

### Name: norm1_PCC
### Title: PCC for Normal data with mean unknown
### Aliases: norm1_PCC

### ** Examples

# 30 Normal observations introducing an outlier of 3*sd at the 15th observation
set.seed(1234)
out <- rnorm(30)
out[15] <- out[15] + 3
norm1_PCC(out, sdl = 1)

# Real data application
attach(aPTT)
norm1_PCC(data = aPTT_current, historical_data = aPTT_historical, sdl = 0.57)



cleanEx()
nameEx("norm2_PCC")
### * norm2_PCC

flush(stderr()); flush(stdout())

### Name: norm2_PCC
### Title: PCC for Normal data with variance unknown
### Aliases: norm2_PCC

### ** Examples

# 30 Normal observations introducing an outlier of 3*sd at the 15th observation
set.seed(1234)
out <- rnorm(30)
out[15] <- out[15] + 3
norm2_PCC(out, ml = 0)

# Real data application
attach(aPTT)
norm2_PCC(data = aPTT_current, historical_data = aPTT_historical, ml = 30)



cleanEx()
nameEx("norm3_PCC")
### * norm3_PCC

flush(stderr()); flush(stdout())

### Name: norm3_PCC
### Title: PCC for Normal data with both parameters unknown
### Aliases: norm3_PCC

### ** Examples

# 30 Normal observations introducing an outlier of 3*sd at the 15th observation
set.seed(1234)
out <- rnorm(30)
out[15] <- out[15] + 3
norm3_PCC(out)

# Real data application
attach(aPTT)
norm3_PCC(data = aPTT_current, historical_data = aPTT_historical)



cleanEx()
nameEx("norm_HD")
### * norm_HD

flush(stderr()); flush(stdout())

### Name: norm_HD
### Title: The Highest Density (HD) interval of Normal distribution.
### Aliases: norm_HD

### ** Examples

norm_HD(0.95, mu = 10, sdv = 1/2, plot = TRUE)




cleanEx()
nameEx("norm_mean2_PRC")
### * norm_mean2_PRC

flush(stderr()); flush(stdout())

### Name: norm_mean2_PRC
### Title: PRC for Normal data with unknown parameters (mean)
### Aliases: norm_mean2_PRC

### ** Examples



# the PRC process for the first application in
# "Design and properties of the Predictive Ratio Cusum (PRC) control charts"


### CD: Current data (New reagent)
### HD: Historical data (Previous reagent)

CD <- c( 31.0, 30.0, 32.0, 28.0, 33.2, 33.2, 35.1, 35.1, 33.9, 37.9,
         33.2, 36.5, 33.2, 35.1, 34.5, 36.5, 33.2, 35.1, 37.2, 32.6, 36.5 )
HD <- c( 31, 30, 33, 30, 33, 30, 31, 32, 32, 30, 33, 31, 34, 31, 34, 34, 36, 30,
         33, 29, 34, 32, 32, 28, 34, 32, 32, 30, 31, 29, 31, 29, 31, 32,34,34,32 )

N <- length(CD)
n0 <- length(HD)

### initial prior parameters

M0F <- 31.8
LF <- 1/2
AF <- 2
BF <- 2.1^2

norm_mean2_PRC( data = CD, historical_data = HD, alpha_0 = 1/n0, mu0 = M0F,
                l0 = LF, a0 = AF, b0 = BF, h = 3.749, two.sided = TRUE )



### a real data application to aPTT values

### CURRENT DATA aPTT
CD <- c( 29.0, 29.1, 28.7, 28.2, 28.0, 29.1, 28.6, 28.7, 28.6, 29.0, 28.4,
         28.1, 28.8, 29.7, 28.8, 29.8, 28.8, 29.4, 28.4, 28.7, 28.7, 29.5,
         28.5, 28.4, 28.1, 28.6, 28.2, 29.6, 28.9, 29.1, 29.0, 29.9, 28.6,
         29.3, 28.2, 28.6, 27.6, 27.3, 28.7, 27.2, 28.4, 28.0, 28.4, 27.8,
         28.4, 28.4, 27.7, 29.2, 27.5, 27.7)

### HISTORICAL DATA aPTT
HD <- c( 28.0, 28.9, 27.7, 29.3, 28.9, 29.5, 28.2, 27.5, 28.8, 28.9, 28.7,
         27.4, 28.6, 28.5, 29.6, 28.7, 21.3, 29.4, 28.1, 28.9, 28.3, 27.6,
         29.0, 29.2, 27.8, 29.1, 28.9, 29.4, 29.4, 28.9, 28.9, 29.2, 29.4,
         29.4, 28.1, 28.5, 29.7, 29.3, 28.6, 29.2, 29.3, 29.3, 29.3, 30.0,
         29.1, 29.1, 26.8, 29.0, 29.3, 28.3)


norm_mean2_PRC( data = CD, historical_data = HD, mu0 = 28.9,
                l0 = 1/4, a0 = 2, b0 = 0.49, two.sided = TRUE )





cleanEx()
nameEx("norm_mean2_PRC_h")
### * norm_mean2_PRC_h

flush(stderr()); flush(stdout())

### Name: norm_mean2_PRC_h
### Title: Derivation of the decision limit for the PRC for Normal data
###   with unknown parameters (mean)
### Aliases: norm_mean2_PRC_h

### ** Examples


# Derivation of the decision limit of the first application in
# "Design and properties of the Predictive Ratio Cusum (PRC) control charts"

CD <- c( 31.0, 30.0, 32.0, 28.0, 33.2, 33.2, 35.1, 35.1, 33.9, 37.9,
         33.2, 36.5, 33.2, 35.1, 34.5, 36.5, 33.2, 35.1, 37.2, 32.6, 36.5 )
HD <- c( 31, 30, 33, 30, 33, 30, 31, 32, 32, 30, 33, 31, 34, 31, 34, 34, 36, 30,
         33, 29, 34, 32, 32, 28, 34, 32, 32, 30, 31, 29, 31, 29, 31, 32, 34, 34, 32 )

N <- length(CD)
n0 <- length(HD)
Pa0 <- 1/n0
M0F <- 31.8
LF <- 1/2
AF <- 2
BF <- 2.1^2
M0F ; LF ; AF ; BF

# To replicate results from application set 'it = 1e5'
norm_mean2_PRC_h( ARL_0 = NULL, FAP = 0.05, N = N, l0 = LF, a0 = AF,
                  historical_data = HD, alpha_0 = Pa0, it = 1e4 )








cleanEx()
nameEx("pois_PCC")
### * pois_PCC

flush(stderr()); flush(stdout())

### Name: pois_PCC
### Title: PCC for Poisson data with rate parameter unknown
### Aliases: pois_PCC

### ** Examples

# 30 Poisson observations introducing an outlier at the 15th observation
set.seed(1111)
out <- rpois(n = 30, lambda = 4)
out[15] <- out[15] + 6
pois_PCC(out)

# Real data application
attach(ECE)
pois_PCC(data = defect_counts, s = inspected_units)



cleanEx()
nameEx("pois_PRC")
### * pois_PRC

flush(stderr()); flush(stdout())

### Name: pois_PRC
### Title: PRC for Poisson data with rate parameter unknown
### Aliases: pois_PRC

### ** Examples

# the PRC process for the second application in
# "Design and properties of the Predictive Ratio Cusum (PRC) control charts"

### CURRENT DATA
CD <- c(1, 0, 0, 0, 1, 0, 3, 3, 3, 2, 5, 5, 2, 4, 4, 3, 4, 3, 8, 3, 2, 2)

### product exposures per million

sn <- c( 0.206, 0.313, 0.368, 0.678, 0.974, 0.927, 0.814, 0.696, 0.659, 0.775, 0.731,
         0.710, 0.705, 0.754, 0.682, 0.686, 0.763, 0.833, 0.738, 0.741, 0.843, 0.792 )

# regular process
pois_PRC(data = CD, s = sn)

# FIR process
pois_PRC(data = CD, s = sn, FIR = TRUE)




cleanEx()
nameEx("pois_PRC_h")
### * pois_PRC_h

flush(stderr()); flush(stdout())

### Name: pois_PRC_h
### Title: Derivation of the decision limit for the PRC for Poisson data
###   with probability parameter unknown
### Aliases: pois_PRC_h

### ** Examples




pois_PRC_h(ARL_0 = 150, c0 = 40, d0 = 10, it = 1e3)





cleanEx()
nameEx("t_HD")
### * t_HD

flush(stderr()); flush(stdout())

### Name: t_HD
### Title: The Highest Density (HD) interval of Student's t distribution.
### Aliases: t_HD

### ** Examples

t_HD( 0.95, df = 2, mu = 2, sdv = 3, plot = TRUE )



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

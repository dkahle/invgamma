pkgname <- "bqror"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('bqror')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("alcdf")
### * alcdf

flush(stderr()); flush(stdout())

### Name: alcdf
### Title: cdf of an asymmetric Laplace distribution
### Aliases: alcdf

### ** Examples

set.seed(101)
x <- -0.5428573
mu <- 0.5
sigma <- 1
p <- 0.25
output <- alcdf(x, mu, sigma, p)

# output
#   0.1143562




cleanEx()
nameEx("alcdfstd")
### * alcdfstd

flush(stderr()); flush(stdout())

### Name: alcdfstd
### Title: cdf of a standard asymmetric Laplace distribution
### Aliases: alcdfstd

### ** Examples

set.seed(101)
x <-  -0.5428573
p <- 0.25
output <- alcdfstd(x, p)

# output
#   0.1663873




cleanEx()
nameEx("covEffectOR1")
### * covEffectOR1

flush(stderr()); flush(stdout())

### Name: covEffectOR1
### Title: Covariate effect in the OR1 model
### Aliases: covEffectOR1

### ** Examples

set.seed(101)
data("data25j4")
y <- data25j4$y
xMat1 <- data25j4$x
k <- dim(xMat1)[2]
J <- dim(as.array(unique(y)))[1]
b0 <- array(rep(0, k), dim = c(k, 1))
B0 <- 10*diag(k)
d0 <- array(0, dim = c(J-2, 1))
D0 <- 0.25*diag(J - 2)
modelOR1 <- quantregOR1(y = y, x = xMat1, b0, B0, d0, D0,
burn = 10, mcmc = 40, p = 0.25, tune = 1, accutoff = 0.5, maxlags = 400, verbose = FALSE)
xMat2 <- xMat1
xMat2[,3] <- xMat2[,3] + 0.02
res <- covEffectOR1(modelOR1, y, xMat1, xMat2, p = 0.25, verbose = TRUE)

# Summary of Covariate Effect:

#               Covariate Effect
# Category_1          -0.0072
# Category_2          -0.0012
# Category_3          -0.0009
# Category_4           0.0093




cleanEx()
nameEx("covEffectOR2")
### * covEffectOR2

flush(stderr()); flush(stdout())

### Name: covEffectOR2
### Title: Covariate effect in the OR2 model
### Aliases: covEffectOR2

### ** Examples

set.seed(101)
data("data25j3")
y <- data25j3$y
xMat1 <- data25j3$x
k <- dim(xMat1)[2]
b0 <- array(rep(0, k), dim = c(k, 1))
B0 <- 10*diag(k)
n0 <- 5
d0 <- 8
output <- quantregOR2(y, xMat1, b0, B0, n0, d0, gammacp2 = 3,
burn = 10, mcmc = 40, p = 0.25, accutoff = 0.5, maxlags = 400, verbose = FALSE)
xMat2 <- xMat1
xMat2[,3] <- xMat2[,3] + 0.02
res <- covEffectOR2(output, y, xMat1, xMat2, gammacp2 = 3, p = 0.25, verbose = TRUE)

# Summary of Covariate Effect:

#               Covariate Effect
# Category_1          -0.0073
# Category_2          -0.0030
# Category_3           0.0103




cleanEx()
nameEx("dicOR1")
### * dicOR1

flush(stderr()); flush(stdout())

### Name: dicOR1
### Title: Deviance Information Criterion in the OR1 model
### Aliases: dicOR1

### ** Examples

set.seed(101)
data("data25j4")
y <- data25j4$y
xMat <- data25j4$x
k <- dim(xMat)[2]
J <- dim(as.array(unique(y)))[1]
b0 <- array(rep(0, k), dim = c(k, 1))
B0 <- 10*diag(k)
d0 <- array(0, dim = c(J-2, 1))
D0 <- 0.25*diag(J - 2)
output <- quantregOR1(y = y, x = xMat, b0, B0, d0, D0,
burn = 10, mcmc = 40, p = 0.25, tune = 1, accutoff = 0.5, maxlags = 400, verbose = FALSE)
mcmc <- 40
deltadraws <- output$deltadraws
betadraws <- output$betadraws
burn <- 0.25*mcmc
nsim <- burn + mcmc
postMeanbeta <- output$postMeanbeta
postMeandelta <- output$postMeandelta
dic <- dicOR1(y, xMat, betadraws, deltadraws,
postMeanbeta, postMeandelta, burn, mcmc, p = 0.25)

# DIC
#   1375.329
# pd
#   139.1751
# dev
#   1096.979




cleanEx()
nameEx("dicOR2")
### * dicOR2

flush(stderr()); flush(stdout())

### Name: dicOR2
### Title: Deviance Information Criterion in the OR2 model
### Aliases: dicOR2

### ** Examples

set.seed(101)
data("data25j3")
y <- data25j3$y
xMat <- data25j3$x
k <- dim(xMat)[2]
b0 <- array(rep(0, k), dim = c(k, 1))
B0 <- 10*diag(k)
n0 <- 5
d0 <- 8
output <- quantregOR2(y = y, x = xMat, b0, B0, n0, d0, gammacp2 = 3,
burn = 10, mcmc = 40, p = 0.25, accutoff = 0.5, maxlags = 400, verbose = FALSE)
betadraws <- output$betadraws
sigmadraws <- output$sigmadraws
gammacp <- c(-Inf, 0, 3, Inf)
postMeanbeta <- output$postMeanbeta
postMeansigma <- output$postMeansigma
mcmc = 40
burn <- 10
nsim <- burn + mcmc
dic <- dicOR2(y, xMat, betadraws, sigmadraws, gammacp,
postMeanbeta, postMeansigma, burn, mcmc, p = 0.25)

# DIC
#   801.8191
# pd
#   6.608594
# dev
#   788.6019




cleanEx()
nameEx("drawbetaOR1")
### * drawbetaOR1

flush(stderr()); flush(stdout())

### Name: drawbetaOR1
### Title: Samples beta in the OR1 model
### Aliases: drawbetaOR1

### ** Examples

set.seed(101)
data("data25j4")
xMat <- data25j4$x
p <- 0.25
n <- dim(xMat)[1]
k <- dim(xMat)[2]
w <- array( (abs(rnorm(n, mean = 2, sd = 1))), dim = c (n, 1))
theta <- 2.666667
tau2 <- 10.66667
z <- array( (rnorm(n, mean = 0, sd = 1)), dim = c(n, 1))
b0 <- array(0, dim = c(k, 1))
B0 <- diag(k)
invB0 <- matrix(c(
     1, 0, 0,
     0, 1, 0,
     0, 0, 1),
     nrow = 3, ncol = 3, byrow = TRUE)
invB0b0 <- invB0 %*% b0
output <- drawbetaOR1(z, xMat, w, tau2, theta, invB0, invB0b0)

# output$beta
#   -0.2481837 0.7837995 -3.4680418



cleanEx()
nameEx("drawbetaOR2")
### * drawbetaOR2

flush(stderr()); flush(stdout())

### Name: drawbetaOR2
### Title: Samples beta in the OR2 model
### Aliases: drawbetaOR2

### ** Examples

set.seed(101)
z <- c(21.01744, 33.54702, 33.09195, -3.677646,
 21.06553, 1.490476, 0.9618205, -6.743081, 21.02186, 0.6950479)
x <- matrix(c(
     1, -0.3010490, 0.8012506,
     1,  1.2764036, 0.4658184,
     1,  0.6595495, 1.7563655,
     1, -1.5024607, -0.8251381,
     1, -0.9733585, 0.2980610,
     1, -0.2869895, -1.0130274,
     1,  0.3101613, -1.6260663,
     1, -0.7736152, -1.4987616,
     1,  0.9961420, 1.2965952,
     1, -1.1372480, 1.7537353),
     nrow = 10, ncol = 3, byrow = TRUE)
sigma <- 1.809417
n <- dim(x)[1]
nu <- array(5 * rep(1,n), dim = c(n, 1))
tau2 <- 10.6667
theta <- 2.6667
invB0 <- matrix(c(
     1, 0, 0,
     0, 1, 0,
     0, 0, 1),
     nrow = 3, ncol = 3, byrow = TRUE)
invB0b0 <- c(0, 0, 0)

output <- drawbetaOR2(z, x, sigma, nu, tau2, theta, invB0, invB0b0)

# output$beta
#   -0.74441 1.364846 0.7159231




cleanEx()
nameEx("drawdeltaOR1")
### * drawdeltaOR1

flush(stderr()); flush(stdout())

### Name: drawdeltaOR1
### Title: Samples delta in the OR1 model
### Aliases: drawdeltaOR1

### ** Examples

set.seed(101)
data("data25j4")
y <- data25j4$y
xMat <- data25j4$x
p <- 0.25
beta <- c(0.3990094, 0.8168991, 2.8034963)
delta0 <- c(-0.9026915, -2.2488833)
d0 <- matrix(c(0, 0),
                 nrow = 2, ncol = 1, byrow = TRUE)
D0 <- matrix(c(0.25, 0.00, 0.00, 0.25),
                    nrow = 2, ncol = 2, byrow = TRUE)
tune <- 0.1
Dhat <- matrix(c(0.046612180, -0.001954257, -0.001954257, 0.083066204),
             nrow = 2, ncol = 2, byrow = TRUE)
p <- 0.25
output <- drawdeltaOR1(y, xMat, beta, delta0, d0, D0, tune, Dhat, p)

# deltareturn
#   -0.9025802 -2.229514
# accept
#   1




cleanEx()
nameEx("drawlatentOR1")
### * drawlatentOR1

flush(stderr()); flush(stdout())

### Name: drawlatentOR1
### Title: Samples latent variable z in the OR1 model
### Aliases: drawlatentOR1

### ** Examples

set.seed(101)
data("data25j4")
y <- data25j4$y
xMat <- data25j4$x
p <- 0.25
beta <- c(0.3990094, 0.8168991, 2.8034963)
w <- 1.114347
theta <- 2.666667
tau2 <- 10.66667
delta <- c(-0.002570995,  1.044481071)
output <- drawlatentOR1(y, xMat, beta, w, theta, tau2, delta)

# output
#   0.6261896 3.129285 2.659578 8.680291
#   13.22584 2.545938 1.507739 2.167358
#   15.03059 -3.963201 9.237466 -1.813652
#   2.718623 -3.515609 8.352259 -0.3880043
#   -0.8917078 12.81702 -0.2009296 1.069133 ... soon




cleanEx()
nameEx("drawlatentOR2")
### * drawlatentOR2

flush(stderr()); flush(stdout())

### Name: drawlatentOR2
### Title: Samples latent variable z in the OR2 model
### Aliases: drawlatentOR2

### ** Examples

set.seed(101)
data("data25j3")
y <- data25j3$y
xMat <- data25j3$x
beta <- c(1.810504, 1.850332, 6.181163)
sigma <- 0.9684741
n <- dim(xMat)[1]
nu <- array(5 * rep(1,n), dim = c(n, 1))
theta <- 2.6667
tau2 <- 10.6667
gammacp <- c(-Inf, 0, 3, Inf)
output <- drawlatentOR2(y, xMat, beta, sigma, nu,
theta, tau2, gammacp)

# output
#   1.257096 10.46297 4.138694
#   28.06432 4.179275 19.21582
#   11.17549 13.79059 28.3650 .. soon




cleanEx()
nameEx("drawnuOR2")
### * drawnuOR2

flush(stderr()); flush(stdout())

### Name: drawnuOR2
### Title: Samples scale factor nu in the OR2 model
### Aliases: drawnuOR2

### ** Examples

set.seed(101)
z <- c(21.01744, 33.54702, 33.09195, -3.677646,
 21.06553, 1.490476, 0.9618205, -6.743081, 21.02186, 0.6950479)
x <- matrix(c(
     1, -0.3010490, 0.8012506,
     1,  1.2764036, 0.4658184,
     1,  0.6595495, 1.7563655,
     1, -1.5024607, -0.8251381,
     1, -0.9733585, 0.2980610,
     1, -0.2869895, -1.0130274,
     1,  0.3101613, -1.6260663,
     1, -0.7736152, -1.4987616,
     1, 0.9961420, 1.2965952,
     1, -1.1372480, 1.7537353),
     nrow = 10, ncol = 3, byrow = TRUE)
beta <- c(-0.74441, 1.364846, 0.7159231)
sigma <- 3.749524
tau2 <- 10.6667
theta <- 2.6667
indexp <- 0.5
output <- drawnuOR2(z, x, beta, sigma, tau2, theta, indexp)

# output
#   5.177456 4.042261 8.950365
#   1.578122 6.968687 1.031987
#   4.13306 0.4681557 5.109653
#   0.1725333




cleanEx()
nameEx("drawsigmaOR2")
### * drawsigmaOR2

flush(stderr()); flush(stdout())

### Name: drawsigmaOR2
### Title: Samples sigma in the OR2 model
### Aliases: drawsigmaOR2

### ** Examples

set.seed(101)
z <- c(21.01744, 33.54702, 33.09195, -3.677646,
 21.06553, 1.490476, 0.9618205, -6.743081, 21.02186, 0.6950479)
x <- matrix(c(
     1, -0.3010490, 0.8012506,
     1,  1.2764036, 0.4658184,
     1,  0.6595495, 1.7563655,
     1, -1.5024607, -0.8251381,
     1, -0.9733585, 0.2980610,
     1, -0.2869895, -1.0130274,
     1,  0.3101613, -1.6260663,
     1, -0.7736152, -1.4987616,
     1,  0.9961420, 1.2965952,
     1, -1.1372480, 1.7537353),
     nrow = 10, ncol = 3, byrow = TRUE)
beta <- c(-0.74441, 1.364846, 0.7159231)
n <- dim(x)[1]
nu <- array(5 * rep(1,n), dim = c(n, 1))
tau2 <- 10.6667
theta <- 2.6667
n0 <- 5
d0 <- 8
output <- drawsigmaOR2(z, x, beta, nu, tau2, theta, n0, d0)

# output$sigma
#   3.749524




cleanEx()
nameEx("drawwOR1")
### * drawwOR1

flush(stderr()); flush(stdout())

### Name: drawwOR1
### Title: Samples latent weight w in the OR1 model
### Aliases: drawwOR1

### ** Examples

set.seed(101)
z <- c(0.9812363, -1.09788, -0.9650175, 8.396556,
 1.39465, -0.8711435, -0.5836833, -2.792464,
 0.1540086, -2.590724, 0.06169976, -1.823058,
 0.06559151, 0.1612763, 0.161311, 4.908488,
 0.6512113, 0.1560708, -0.883636, -0.5531435)
x <- matrix(c(
     1, 1.4747905363, 0.167095186,
     1, -0.3817326861, 0.041879526,
     1, -0.1723095575, -1.414863777,
     1, 0.8266428137, 0.399722073,
     1, 0.0514888733, -0.105132425,
     1, -0.3159992662, -0.902003846,
     1, -0.4490888878, -0.070475600,
     1, -0.3671705251, -0.633396477,
     1, 1.7655601639, -0.702621934,
     1, -2.4543678120, -0.524068780,
     1,  0.3625025618,  0.698377504,
     1, -1.0339179063,  0.155746376,
     1,  1.2927374692, -0.155186911,
     1, -0.9125108094, -0.030513775,
     1,  0.8761233001,  0.988171587,
     1,  1.7379728231,  1.180760114,
     1,  0.7820635770, -0.338141095,
     1, -1.0212853209, -0.113765067,
     1,  0.6311364051, -0.061883874,
     1,  0.6756039688,  0.664490143),
     nrow = 20, ncol = 3, byrow = TRUE)
beta <- c(-1.583533, 1.407158, 2.259338)
tau2 <- 10.66667
theta <- 2.666667
indexp <- 0.5
output <- drawwOR1(z, x, beta, tau2, theta, indexp)

# output
#   0.16135732
#   0.39333080
#   0.80187227
#   2.27442898
#   0.90358310
#   0.99886987
#   0.41515947 ... soon




cleanEx()
nameEx("ineffactorOR1")
### * ineffactorOR1

flush(stderr()); flush(stdout())

### Name: ineffactorOR1
### Title: Inefficiency factor in the OR1 model
### Aliases: ineffactorOR1

### ** Examples

set.seed(101)
data("data25j4")
y <- data25j4$y
xMat <- data25j4$x
k <- dim(xMat)[2]
J <- dim(as.array(unique(y)))[1]
b0 <- array(rep(0, k), dim = c(k, 1))
B0 <- 10*diag(k)
d0 <- array(0, dim = c(J-2, 1))
D0 <- 0.25*diag(J - 2)
output <- quantregOR1(y = y, x = xMat, b0, B0, d0, D0,
burn = 10, mcmc = 40, p = 0.25, tune = 1, accutoff = 0.5, maxlags = 400, verbose = FALSE)
betadraws <- output$betadraws
deltadraws <- output$deltadraws
inefficiency <- ineffactorOR1(xMat, betadraws, deltadraws, 0.5, 400, TRUE)

# Summary of Inefficiency Factor:

#             Inef Factor
# beta_1        1.1008
# beta_2        3.0024
# beta_3        2.8543
# delta_1       3.6507
# delta_2       3.1784




cleanEx()
nameEx("ineffactorOR2")
### * ineffactorOR2

flush(stderr()); flush(stdout())

### Name: ineffactorOR2
### Title: Inefficiency factor in the OR2 model
### Aliases: ineffactorOR2

### ** Examples

set.seed(101)
data("data25j3")
y <- data25j3$y
xMat <- data25j3$x
k <- dim(xMat)[2]
b0 <- array(rep(0, k), dim = c(k, 1))
B0 <- 10*diag(k)
n0 <- 5
d0 <- 8
output <- quantregOR2(y = y, x = xMat, b0, B0, n0, d0, gammacp2 = 3,
burn = 10, mcmc = 40, p = 0.25, accutoff = 0.5, maxlags = 400, verbose = FALSE)
betadraws <- output$betadraws
sigmadraws <- output$sigmadraws

inefficiency <- ineffactorOR2(xMat, betadraws, sigmadraws, 0.5, 400, TRUE)

# Summary of Inefficiency Factor:
#            Inef Factor
# beta_1       1.5686
# beta_2       1.5240
# beta_3       1.4807
# sigma        2.4228




cleanEx()
nameEx("logMargLikeOR1")
### * logMargLikeOR1

flush(stderr()); flush(stdout())

### Name: logMargLikeOR1
### Title: Marginal likelihood in the OR1 model
### Aliases: logMargLikeOR1

### ** Examples

set.seed(101)
data("data25j4")
y <- data25j4$y
xMat <- data25j4$x
k <- dim(xMat)[2]
J <- dim(as.array(unique(y)))[1]
b0 <- array(rep(0, k), dim = c(k, 1))
B0 <- 10*diag(k)
d0 <- array(0, dim = c(J-2, 1))
D0 <- 0.25*diag(J - 2)
output <- quantregOR1(y = y, x = xMat, b0, B0, d0, D0,
burn = 10, mcmc = 40, p = 0.25, tune = 1, accutoff = 0.5, maxlags = 400, verbose = FALSE)
# output$logMargLike
#   -559.73




cleanEx()
nameEx("logMargLikeOR2")
### * logMargLikeOR2

flush(stderr()); flush(stdout())

### Name: logMargLikeOR2
### Title: Marginal likelihood in the OR2 model
### Aliases: logMargLikeOR2

### ** Examples

set.seed(101)
data("data25j3")
y <- data25j3$y
xMat <- data25j3$x
k <- dim(xMat)[2]
b0 <- array(rep(0, k), dim = c(k, 1))
B0 <- 10*diag(k)
n0 <- 5
d0 <- 8
output <- quantregOR2(y = y, x = xMat, b0, B0, n0, d0, gammacp2 = 3,
burn = 10, mcmc = 40, p = 0.25, accutoff = 0.5, maxlags = 400, verbose = FALSE)
# output$logMargLike
#   -404.57




cleanEx()
nameEx("qrminfundtheorem")
### * qrminfundtheorem

flush(stderr()); flush(stdout())

### Name: qrminfundtheorem
### Title: Minimizes the negative of log-likelihood in the OR1 model
### Aliases: qrminfundtheorem

### ** Examples

set.seed(101)
deltaIn <- c(-0.002570995,  1.044481071)
data("data25j4")
y <- data25j4$y
xMat <- data25j4$x
p <- 0.25
beta <- c(0.3990094, 0.8168991, 2.8034963)
cri0     <- 1
cri1     <- 0.001
stepsize <- 1
maxiter  <- 10
h        <- 0.002
dh       <- 0.0002
sw       <- 20
output <- qrminfundtheorem(deltaIn, y, xMat, beta, cri0, cri1, stepsize, maxiter, h, dh, sw, p)

# deltamin
#   0.8266967 0.3635708
# negsum
#   645.4911
# logl
#    -0.7136999
#    -1.5340787
#    -1.1072447
#    -1.4423124
#    -1.3944677
#    -0.7941271
#    -1.6544072
#    -0.3246632
#    -1.8582422
#    -0.9220822
#    -2.1117739 .. soon
# G
#    0.803892784  0.00000000
#   -0.420190546  0.72908381
#   -0.421776117  0.72908341
#   -0.421776117 -0.60184063
#   -0.421776117 -0.60184063
#    0.151489598  0.86175120
#    0.296995920  0.96329114
#   -0.421776117  0.72908341
#   -0.340103190 -0.48530164
#    0.000000000  0.00000000
#   -0.421776117 -0.60184063.. soon
# H
#   -338.21243  -41.10775
#   -41.10775 -106.32758




cleanEx()
nameEx("qrnegLogLikeOR2")
### * qrnegLogLikeOR2

flush(stderr()); flush(stdout())

### Name: qrnegLogLikeOR2
### Title: Negative sum of log-likelihood in the OR2 model
### Aliases: qrnegLogLikeOR2

### ** Examples

set.seed(101)
data("data25j3")
y <- data25j3$y
xMat <- data25j3$x
p <- 0.25
gammacp <- c(-Inf, 0, 3, Inf)
betaOne <- c(1.810504, 1.850332, 6.18116)
sigmaOne <- 0.9684741
output <- qrnegLogLikeOR2(y, xMat, gammacp, betaOne, sigmaOne, p)

# output
#   902.4045




cleanEx()
nameEx("qrnegLogLikensumOR1")
### * qrnegLogLikensumOR1

flush(stderr()); flush(stdout())

### Name: qrnegLogLikensumOR1
### Title: Negative log-likelihood in the OR1 model
### Aliases: qrnegLogLikensumOR1

### ** Examples

set.seed(101)
deltaOne <- c(-0.002570995, 1.044481071)
data("data25j4")
y <- data25j4$y
xMat <- data25j4$x
p <- 0.25
betaOne <- c(0.3990094, 0.8168991, 2.8034963)
output <- qrnegLogLikensumOR1(y, xMat, betaOne, deltaOne, p)

# nlogl
#   0.7424858
#   1.1649645
#   2.1344390
#   0.9881085
#   2.7677386
#   0.8229129
#   0.8854911
#   0.3534490
#   1.8582422
#   0.9508680 .. soon

# negsumlogl
#   663.5475




cleanEx()
nameEx("quantregOR1")
### * quantregOR1

flush(stderr()); flush(stdout())

### Name: quantregOR1
### Title: Bayesian quantile regression in the OR1 model
### Aliases: quantregOR1

### ** Examples

 set.seed(101)
 data("data25j4")
 y <- data25j4$y
 xMat <- data25j4$x
 k <- dim(xMat)[2]
 J <- dim(as.array(unique(y)))[1]
 b0 <- array(rep(0, k), dim = c(k, 1))
 B0 <- 10*diag(k)
 d0 <- array(0, dim = c(J-2, 1))
 D0 <- 0.25*diag(J - 2)
 output <- quantregOR1(y = y, x = xMat, b0 ,B0, d0, D0,
 burn = 10, mcmc = 40, p = 0.25, tune = 1, accutoff = 0.5, maxlags = 400, verbose = TRUE)

 # Summary of MCMC draws:


 #             Post Mean  Post Std   Upper Credible Lower Credible Inef Factor
 # beta_1       -2.6202   0.3588        -2.0560        -3.3243       1.1008
 # beta_2        3.1670   0.5894         4.1713         2.1423       3.0024
 # beta_3        4.2800   0.9141         5.7142         2.8625       2.8534
 # delta_1       0.2188   0.4043         0.6541        -0.4384       3.6507
 # delta_2       0.4567   0.3055         0.7518        -0.2234       3.1784

 # MH acceptance rate: 50%
 # Log of Marginal Likelihood: -559.73
 # DIC: 1133.11




cleanEx()
nameEx("quantregOR2")
### * quantregOR2

flush(stderr()); flush(stdout())

### Name: quantregOR2
### Title: Bayesian quantile regression in the OR2 model
### Aliases: quantregOR2

### ** Examples

set.seed(101)
data("data25j3")
y <- data25j3$y
xMat <- data25j3$x
k <- dim(xMat)[2]
b0 <- array(rep(0, k), dim = c(k, 1))
B0 <- 10*diag(k)
n0 <- 5
d0 <- 8
output <- quantregOR2(y = y, x = xMat, b0, B0, n0, d0, gammacp2 = 3,
burn = 10, mcmc = 40, p = 0.25, accutoff = 0.5, maxlags = 400, verbose = TRUE)

# Summary of MCMC draws :

#            Post Mean Post Std Upper Credible Lower Credible Inef Factor
#    beta_1   -4.5185   0.9837        -3.1726        -6.2000     1.5686
#    beta_2    6.1825   0.9166         7.6179         4.8619     1.5240
#    beta_3    5.2984   0.9653         6.9954         4.1619     1.4807
#    sigma     1.0879   0.2073         1.5670         0.8436     2.4228

# Log of Marginal Likelihood: -404.57
# DIC: 801.82




cleanEx()
nameEx("rndald")
### * rndald

flush(stderr()); flush(stdout())

### Name: rndald
### Title: Generates random numbers from an AL distribution
### Aliases: rndald

### ** Examples

set.seed(101)
sigma <- 2.503306
p <- 0.25
n <- 1
output <- rndald(sigma, p, n)

# output
#   1.07328




cleanEx()
nameEx("summary.bqrorOR1")
### * summary.bqrorOR1

flush(stderr()); flush(stdout())

### Name: summary.bqrorOR1
### Title: Extractor function for summary
### Aliases: summary.bqrorOR1

### ** Examples

set.seed(101)
data("data25j4")
y <- data25j4$y
xMat <- data25j4$x
k <- dim(xMat)[2]
J <- dim(as.array(unique(y)))[1]
b0 <- array(rep(0, k), dim = c(k, 1))
B0 <- 10*diag(k)
d0 <- array(0, dim = c(J-2, 1))
D0 <- 0.25*diag(J - 2)
output <- quantregOR1(y = y, x = xMat, b0, B0, d0, D0,
burn = 10, mcmc = 40, p = 0.25, tune = 1, accutoff = 0.5, maxlags = 400, verbose = FALSE)
summary(output, 4)

 #            Post Mean  Post Std   Upper Credible Lower Credible Inef Factor
 # beta_1       -2.6202   0.3588        -2.0560        -3.3243       1.1008
 # beta_2        3.1670   0.5894         4.1713         2.1423       3.0024
 # beta_3        4.2800   0.9141         5.7142         2.8625       2.8534
 # delta_1       0.2188   0.4043         0.6541        -0.4384       3.6507
 # delta_2       0.4567   0.3055         0.7518        -0.2234       3.1784




cleanEx()
nameEx("summary.bqrorOR2")
### * summary.bqrorOR2

flush(stderr()); flush(stdout())

### Name: summary.bqrorOR2
### Title: Extractor function for summary
### Aliases: summary.bqrorOR2

### ** Examples

set.seed(101)
data("data25j3")
y <- data25j3$y
xMat <- data25j3$x
k <- dim(xMat)[2]
b0 <- array(rep(0, k), dim = c(k, 1))
B0 <- 10*diag(k)
n0 <- 5
d0 <- 8
output <- quantregOR2(y = y, x = xMat, b0, B0, n0, d0, gammacp2 = 3,
burn = 10, mcmc = 40, p = 0.25, accutoff = 0.5, maxlags = 400, FALSE)
summary(output, 4)

#            Post Mean Post Std Upper Credible Lower Credible Inef Factor
#    beta_1   -4.5185   0.9837        -3.1726        -6.2000     1.5686
#    beta_2    6.1825   0.9166         7.6179         4.8619     1.5240
#    beta_3    5.2984   0.9653         6.9954         4.1619     1.4807
#    sigma     1.0879   0.2073         1.5670         0.8436     2.4228




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

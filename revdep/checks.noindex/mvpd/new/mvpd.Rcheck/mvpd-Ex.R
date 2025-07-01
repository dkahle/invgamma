pkgname <- "mvpd"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('mvpd')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("adaptIntegrate_inf_limPD")
### * adaptIntegrate_inf_limPD

flush(stderr()); flush(stdout())

### Name: adaptIntegrate_inf_limPD
### Title: Adaptive multivariate integration over hypercubes (admitting
###   infinite limits)
### Aliases: adaptIntegrate_inf_limPD
### Keywords: integration multivariate numerical

### ** Examples

## integrate Cauchy Density from -Inf to Inf
adaptIntegrate_inf_limPD(function(x) 1/pi * 1/(1+x^2), -Inf, Inf)
adaptIntegrate_inf_limPD(function(x, scale) 1/(pi*scale) * 1/(1+(x/scale)^2), -Inf, Inf, scale=4)
## integrate Cauchy Density from -Inf to -3
adaptIntegrate_inf_limPD(function(x) 1/pi * 1/(1+x^2), -Inf, -3)$int
stats::pcauchy(-3)
adaptIntegrate_inf_limPD(function(x, scale) 1/(pi*scale) * 1/(1+(x/scale)^2), -Inf, -3, scale=4)$int
stats::pcauchy(-3, scale=4)




cleanEx()
nameEx("dkolm")
### * dkolm

flush(stderr()); flush(stdout())

### Name: dkolm
### Title: Density for the Kolmogorov Distribution
### Aliases: dkolm

### ** Examples

## see https://swihart.github.io/mvpd/articles/deep_dive_kolm.html
dkolm(1)



cleanEx()
nameEx("dmvss")
### * dmvss

flush(stderr()); flush(stdout())

### Name: dmvss
### Title: Multivariate Subgaussian Stable Density
### Aliases: dmvss
### Keywords: distribution

### ** Examples


## print("mvsubgaussPD (d=2, alpha=1.71):")
Q <- matrix(c(10,7.5,7.5,10),2)
mvpd::dmvss(x=c(0,1), alpha=1.71, Q=Q)

## more accuracy = longer runtime
mvpd::dmvss(x=c(0,1),alpha=1.71, Q=Q, abs.tol=1e-8)

Q <- matrix(c(10,7.5,7.5,7.5,10,7.5,7.5,7.5,10),3)
## print("mvsubgausPD (d=3, alpha=1.71):")
mvpd::dmvss(x=c(0,1,2), alpha=1.71, Q=Q)
mvpd::dmvss(x=c(0,1,2), alpha=1.71, Q=Q, spherical=TRUE)

## How `delta` works: same as centering
X <- c(1,1,1)
Q <- matrix(c(10,7.5,7.5,7.5,10,7.5,7.5,7.5,10),3)
D <- c(0.75, 0.65, -0.35)
mvpd::dmvss(X-D, alpha=1.71, Q=Q)
mvpd::dmvss(X  , alpha=1.71, Q=Q, delta=D)





cleanEx()
nameEx("dmvss_mat")
### * dmvss_mat

flush(stderr()); flush(stdout())

### Name: dmvss_mat
### Title: Multivariate Subgaussian Stable Density for matrix inputs
### Aliases: dmvss_mat
### Keywords: distribution

### ** Examples


## print("mvsubgaussPD (d=2, alpha=1.71):")
Q <- matrix(c(10,7.5,7.5,10),2)
mvpd::dmvss(x=c(0,1), alpha=1.71, Q=Q)

## more accuracy = longer runtime
mvpd::dmvss(x=c(0,1),alpha=1.71, Q=Q, abs.tol=1e-8)

Q <- matrix(c(10,7.5,7.5,7.5,10,7.5,7.5,7.5,10),3)
## print("mvsubgausPD (d=3, alpha=1.71):")
mvpd::dmvss(x=c(0,1,2), alpha=1.71, Q=Q)
mvpd::dmvss(x=c(0,1,2), alpha=1.71, Q=Q, spherical=TRUE)

## How `delta` works: same as centering
X <- c(1,1,1)
Q <- matrix(c(10,7.5,7.5,7.5,10,7.5,7.5,7.5,10),3)
D <- c(0.75, 0.65, -0.35)
mvpd::dmvss(X-D, alpha=1.71, Q=Q)
mvpd::dmvss(X  , alpha=1.71, Q=Q, delta=D)





cleanEx()
nameEx("dmvt_mat")
### * dmvt_mat

flush(stderr()); flush(stdout())

### Name: dmvt_mat
### Title: Multivariate t-Distribution Density for matrix inputs
### Aliases: dmvt_mat
### Keywords: distribution

### ** Examples


x <- c(1.23, 4.56)
mu <- 1:2
Sigma <- matrix(c(4, 2, 2, 3), ncol=2)
df01 <- mvtnorm::dmvt(x, delta = mu, sigma = Sigma, df =  1, log=FALSE) # default log = TRUE!
df10 <- mvtnorm::dmvt(x, delta = mu, sigma = Sigma, df = 10, log=FALSE) # default log = TRUE!
df30 <- mvtnorm::dmvt(x, delta = mu, sigma = Sigma, df = 30, log=FALSE) # default log = TRUE!
df01
df10
df30


dmvt_mat(
  matrix(x,ncol=2),
  df = 1,
  Q = Sigma,
  delta=mu)$int


dmvt_mat(
  matrix(x,ncol=2),
  df = 10,
  Q = Sigma,
  delta=mu)$int


dmvt_mat(
  matrix(x,ncol=2),
  df = 30,
  Q = Sigma,
  delta=mu)$int

## Q: can we do non-integer degrees of freedom?
## A: yes for both mvpd::dmvt_mat and mvtnorm::dmvt

df1.5 <- mvtnorm::dmvt(x, delta = mu, sigma = Sigma, df =  1.5, log=FALSE) # default log = TRUE!
df1.5

dmvt_mat(
  matrix(x,ncol=2),
  df = 1.5,
  Q = Sigma,
  delta=mu)$int


## Q: can we do <1 degrees of freedom but >0?
## A: yes for both mvpd::dmvt_mat and mvtnorm::dmvt

df0.5 <- mvtnorm::dmvt(x, delta = mu, sigma = Sigma, df =  0.5, log=FALSE) # default log = TRUE!
df0.5

dmvt_mat(
  matrix(x,ncol=2),
  df = 0.5,
  Q = Sigma,
  delta=mu)$int

df0.0001 <- mvtnorm::dmvt(x, delta = mu, sigma = Sigma, df =  0.0001, 
                          log=FALSE) # default log = TRUE!
df0.0001

dmvt_mat(
  matrix(x,ncol=2),
  df = 0.0001,
  Q = Sigma,
  delta=mu)$int



## Q: can we do ==0 degrees of freedom?
## A: No for both mvpd::dmvt_mat and mvtnorm::dmvt

## this just becomes normal, as per the manual for mvtnorm::dmvt
df0.0 <- mvtnorm::dmvt(x, delta = mu, sigma = Sigma, df =  0, log=FALSE) # default log = TRUE!
df0.0

## Not run: 
##D dmvt_mat(
##D   matrix(x,ncol=2),
##D   df = 0,
##D   Q = Sigma,
##D   delta=mu)$int 
## End(Not run)



cleanEx()
nameEx("fit_mvss")
### * fit_mvss

flush(stderr()); flush(stdout())

### Name: fit_mvss
### Title: Fit a Multivariate Subgaussian Distribution
### Aliases: fit_mvss
### Keywords: distribution

### ** Examples





cleanEx()
nameEx("pmvlogis")
### * pmvlogis

flush(stderr()); flush(stdout())

### Name: pmvlogis
### Title: Multivariate Elliptically Contoured Logistic Distribution
### Aliases: pmvlogis
### Keywords: distribution

### ** Examples


## bivariate
U <- c(1,1)
L <- -U
Q <- matrix(c(10,7.5,7.5,10),2)
mvpd::pmvlogis(L, U, nterms=1000, Q=Q)

## trivariate
U <- c(1,1,1)
L <- -U
Q <- matrix(c(10,7.5,7.5,7.5,10,7.5,7.5,7.5,10),3)
mvpd::pmvlogis(L, U, nterms=1000, Q=Q)

## How `delta` works: same as centering
U <- c(1,1,1)
L <- -U
Q <- matrix(c(10,7.5,7.5,7.5,10,7.5,7.5,7.5,10),3)
D <- c(0.75, 0.65, -0.35)
mvpd::pmvlogis(L-D, U-D, nterms=100, Q=Q)
mvpd::pmvlogis(L  , U  , nterms=100, Q=Q, delta=D)

## recover univariate from trivariate
crit_val <- -1.3
Q <- matrix(c(10,7.5,7.5,7.5,20,7.5,7.5,7.5,30),3) / 10
Q
pmvlogis(c(-Inf,-Inf,-Inf), 
         c( Inf, Inf, crit_val),
         Q=Q)
plogis(crit_val, scale=sqrt(Q[3,3]))

pmvlogis(c(-Inf,     -Inf,-Inf), 
         c( Inf, crit_val, Inf ),
         Q=Q)
plogis(crit_val, scale=sqrt(Q[2,2]))

pmvlogis(c(     -Inf, -Inf,-Inf), 
         c( crit_val,  Inf, Inf ),
         Q=Q)
plogis(crit_val, scale=sqrt(Q[1,1]))
 



cleanEx()
nameEx("pmvss")
### * pmvss

flush(stderr()); flush(stdout())

### Name: pmvss
### Title: Multivariate Subgaussian Stable Distribution
### Aliases: pmvss
### Keywords: distribution

### ** Examples


## bivariate
U <- c(1,1)
L <- -U
Q <- matrix(c(10,7.5,7.5,10),2)
mvpd::pmvss(L, U, alpha=1.71, Q=Q)

## trivariate
U <- c(1,1,1)
L <- -U
Q <- matrix(c(10,7.5,7.5,7.5,10,7.5,7.5,7.5,10),3)
mvpd::pmvss(L, U, alpha=1.71, Q=Q)

## How `delta` works: same as centering
U <- c(1,1,1)
L <- -U
Q <- matrix(c(10,7.5,7.5,7.5,10,7.5,7.5,7.5,10),3)
D <- c(0.75, 0.65, -0.35)
mvpd::pmvss(L-D, U-D, alpha=1.71, Q=Q)
mvpd::pmvss(L  , U  , alpha=1.71, Q=Q, delta=D)






cleanEx()
nameEx("pmvss_mc")
### * pmvss_mc

flush(stderr()); flush(stdout())

### Name: pmvss_mc
### Title: Monte Carlo Multivariate Subgaussian Stable Distribution
### Aliases: pmvss_mc

### ** Examples


## print("mvpd (d=2, alpha=1.71):")
U <- c(1,1)
L <- -U
Q <- matrix(c(10,7.5,7.5,10),2)
mvpd::pmvss_mc(L, U, alpha=1.71, Q=Q, n=1e3)
mvpd::pmvss   (L, U, alpha=1.71, Q=Q)

## more accuracy = longer runtime
mvpd::pmvss_mc(L, U, alpha=1.71, Q=Q, n=1e4)

U <- c(1,1,1)
L <- -U
Q <- matrix(c(10,7.5,7.5,7.5,10,7.5,7.5,7.5,10),3)
## print("mvpd: (d=3, alpha=1.71):")
mvpd::pmvss_mc(L, U, alpha=1.71, Q=Q, n=1e3)





cleanEx()
nameEx("rkolm")
### * rkolm

flush(stderr()); flush(stdout())

### Name: rkolm
### Title: Random Variates for the Kolmogorov Distribution
### Aliases: rkolm

### ** Examples

## see https://swihart.github.io/mvpd/articles/deep_dive_kolm.html
rkolm(10)



cleanEx()
nameEx("rmvlogis")
### * rmvlogis

flush(stderr()); flush(stdout())

### Name: rmvlogis
### Title: Multivariate Logistic Random Variables
### Aliases: rmvlogis
### Keywords: distribution

### ** Examples

rmvlogis(10, Q=diag(5))

## Not run: 
##D QMAT <- matrix(c(1,0,0,1),nrow=2)
##D draw_NNMD  <- NonNorMvtDist::rmvlogis(2e3, parm1=rep(0,2), parm2=rep(1,2))
##D draw_mvpd  <-          mvpd::rmvlogis(2e3,     Q=QMAT)
##D 
##D mean(draw_NNMD[,1]   < -1 & draw_NNMD[,2]   < 3)
##D mean(draw_mvpd[,1] < -1 & draw_mvpd[,2] < 3)
##D 
##D plogis(-1)
##D mean(draw_NNMD[,1] < -1)
##D mean(draw_mvpd[,1] < -1)
##D 
##D plogis(3)
##D mean(draw_NNMD[,2] < 3)
##D mean(draw_mvpd[,2] < 3)
##D  
##D rangex <- range(c(draw_mvpd[,1],draw_NNMD[,1]))
##D rangey <- range(c(draw_mvpd[,2],draw_NNMD[,2]))
##D 
##D par(mfrow=c(3,2), pty="s", mai=c(.5,.1,.1,.1))
##D plot(draw_NNMD, xlim=rangex, ylim=rangey); abline(h=0,v=0)
##D plot(draw_mvpd   , xlim=rangex, ylim=rangey); abline(h=0,v=0)
##D 
##D hist(draw_NNMD[,1]  , breaks = 100,  xlim=rangex, probability=TRUE, ylim=c(0,.40))
##D curve(dlogis(x), add=TRUE, col="blue",lwd=2)
##D hist(draw_mvpd[,1], breaks = 100,  xlim=rangex, probability=TRUE, ylim=c(0,.40))
##D curve(dlogis(x), add=TRUE, col="blue",lwd=2)
##D hist(draw_NNMD[,2]  , breaks = 100,  xlim=rangex, probability=TRUE, ylim=c(0,.40))
##D curve(dlogis(x), add=TRUE, col="blue",lwd=2)
##D hist(draw_mvpd[,2], breaks = 100,  xlim=rangex, probability=TRUE, ylim=c(0,.40))
##D curve(dlogis(x), add=TRUE, col="blue",lwd=2)
## End(Not run)



cleanEx()
nameEx("rmvss")
### * rmvss

flush(stderr()); flush(stdout())

### Name: rmvss
### Title: Multivariate Subgaussian Stable Random Variates
### Aliases: rmvss
### Keywords: distribution

### ** Examples

## generate 10 random variates of a bivariate mvss
rmvss(n=10, alpha=1.71, Q=matrix(c(10,7.5,7.5,10),2))

## generate 10 random variates of a trivariate mvss
Q <- matrix(c(10,7.5,7.5,7.5,10,7.5,7.5,7.5,10),3)
rmvss(n=10, alpha=1.71, Q=Q)





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

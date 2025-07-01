pkgname <- "RepeatedHighDim"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('RepeatedHighDim')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("GA_diagplot")
### * GA_diagplot

flush(stderr()); flush(stdout())

### Name: GA_diagplot
### Title: Diagnostic plot for comparison of two correlation matrices.
### Aliases: GA_diagplot

### ** Examples


## Not run: 
##D 
##D R1 = diag(10)
##D X0 <- start_matrix(p=c(0.4, 0.2, 0.5, 0.15, 0.4, 0.35, 0.2, 0.25, 0.3, 0.4), k = 5000)
##D Xt <- iter_matrix(X0, R = diag(10), T = 10000, e.min = 0.00001)
##D GA_diagplot(R1, Rt = Xt$Rt, col.method = "trafficlight")
##D GA_diagplot(R1, Rt = Xt$Rt, col.method = "updown")
##D 
## End(Not run)



cleanEx()
nameEx("GlobTestMissing")
### * GlobTestMissing

flush(stderr()); flush(stdout())

### Name: GlobTestMissing
### Title: Detection of global group effect
### Aliases: GlobTestMissing

### ** Examples

### Global comparison of a set of 100 proteins between two experimental groups,
### where (tau * 100) percent of expression levels are missing.
n1 = 10
n2 = 10
d = 100
tau = 0.1
X1 = t(matrix(rnorm(n1*d, 0, 1), n1, d))
X2 = t(matrix(rnorm(n2*d, 0.1, 1), n2, d))
X1[sample(1:(n1*d), tau * (n1*d))] = NA
X2[sample(1:(n2*d), tau * (n2*d))] = NA
GlobTestMissing(X1, X2, nperm=100)



cleanEx()
nameEx("RHighDim")
### * RHighDim

flush(stderr()); flush(stdout())

### Name: RHighDim
### Title: Detection of global group effect
### Aliases: RHighDim

### ** Examples

### Global comparison of a set of 100 genes between two experimental groups.
X1 = matrix(rnorm(1000, 0, 1), 10, 100)
X2 = matrix(rnorm(1000, 0.1, 1), 10, 100)
RHD = RHighDim(X1, X2, paired=FALSE)
summary_RHD(RHD)



cleanEx()
nameEx("bag")
### * bag

flush(stderr()); flush(stdout())

### Name: bag
### Title: Calculates the bag
### Aliases: bag

### ** Examples

## Attention: calculation is currently time-consuming.

## Not run: 
##D ## Two 3-dimensional example data sets D1 and D2
##D n <- 200
##D x1 <- rnorm(n, 0, 1)
##D y1 <- rnorm(n, 0, 1)
##D z1 <- rnorm(n, 0, 1)
##D D1 <- data.frame(cbind(x1, y1, z1))
##D x2 <- rnorm(n, 1, 1)
##D y2 <- rnorm(n, 1, 1)
##D z2 <- rnorm(n, 1, 1)
##D D2 <- data.frame(cbind(x2, y2, z2))
##D colnames(D1) <- c("x", "y", "z")
##D colnames(D2) <- c("x", "y", "z")
##D 
##D # Placing outliers in D1 and D2
##D D1[17,] = c(4, 5, 6)
##D D2[99,] = -c(3, 4, 5)
##D 
##D # Grid size and graphic parameters
##D grid.size <- 20
##D red <- rgb(200, 100, 100, alpha = 100, maxColorValue = 255)
##D blue <- rgb(100, 100, 200, alpha = 100, maxColorValue = 255)
##D yel <- rgb(255, 255, 102, alpha = 100, maxColorValue = 255)
##D white <- rgb(255, 255, 255, alpha = 100, maxColorValue = 255)
##D require(rgl)
##D material3d(color=c(red, blue, yel, white),
##D alpha=c(0.5, 0.5, 0.5, 0.5), smooth=FALSE, specular="black")
##D 
##D # Calucation and visualization of gemplot for D1
##D G <- gridfun(D1, grid.size=20)
##D G$H <- hldepth(D1, G, verbose=TRUE)
##D dm <- depmed(G)
##D B <- bag(D1, G)
##D L <- loop(D1, B, dm=dm)
##D bg3d(color = "gray39" )
##D points3d(D1[L$outliers==0,1], D1[L$outliers==0,2], D1[L$outliers==0,3], col="green")
##D text3d(D1[L$outliers==1,1], D1[L$outliers==1,2],D1[L$outliers==1,3],
##D as.character(which(L$outliers==1)), col=yel)
##D spheres3d(dm[1], dm[2], dm[3], col=yel, radius=0.1)
##D material3d(1,alpha=0.4)
##D gem(B$coords, B$hull, red)
##D gem(L$coords.loop, L$hull.loop, red)
##D axes3d(col="white")
##D 
##D # Calucation and visualization of gemplot for D2
##D G <- gridfun(D2, grid.size=20)
##D G$H <- hldepth(D2, G, verbose=TRUE)
##D dm <- depmed(G)
##D B <- bag(D2, G)
##D L <- loop(D2, B, dm=dm)
##D points3d(D2[L$outliers==0,1], D2[L$outliers==0,2], D2[L$outliers==0,3], col="green")
##D text3d(D2[L$outliers==1,1], D2[L$outliers==1,2],D2[L$outliers==1,3],
##D as.character(which(L$outliers==1)), col=yel)
##D spheres3d(dm[1], dm[2], dm[3], col=yel, radius=0.1)
##D gem(B$coords, B$hull, blue)
##D gem(L$coords.loop, L$hull.loop, blue)
## End(Not run)



cleanEx()
nameEx("depmed")
### * depmed

flush(stderr()); flush(stdout())

### Name: depmed
### Title: Calculates the depth median.
### Aliases: depmed

### ** Examples

## Attention: calculation is currently time-consuming.
## Not run: 
##D 
##D # A 3-dimensional example data set D1
##D n <- 200
##D x1 <- rnorm(n, 0, 1)
##D y1 <- rnorm(n, 0, 1)
##D z1 <- rnorm(n, 0, 1)
##D D1 <- data.frame(cbind(x1, y1, z1))
##D colnames(D1) <- c("x", "y", "z")
##D 
##D # Specification of the grid and calculation of the halfspace location depth at each grid location.
##D G <- gridfun(D1, grid.size=20)
##D G$H <- hldepth(D1, G, verbose=TRUE)
##D dm <- depmed(G) ## Calculation of the depth median
## End(Not run)



cleanEx()
nameEx("fc_ci")
### * fc_ci

flush(stderr()); flush(stdout())

### Name: fc_ci
### Title: Calculation of adjusted confidence intervals
### Aliases: fc_ci

### ** Examples

### Artificial microarray data
d = 1000 ### Number of genes
n = 10 ### Sample per group
fc = rlnorm(d, 0, 0.1)
mu1 = rlnorm(d, 0, 1) ### Mean vector group 1
mu2 = mu1 * fc ### Mean vector group 2
sd1 = rnorm(d, 1, 0.2)
sd2 = rnorm(d, 1, 0.2)
X1 = matrix(NA, d, n) ### Expression levels group 1
X2 = matrix(NA, d, n) ### Expression levels group 2
for (i in 1:n) {
  X1[,i] = rnorm(d, mu1, sd=sd1)
  X2[,i] = rnorm(d, mu2, sd=sd2)
}
X = cbind(X1, X2)
heatmap(X)

### Differential expression analysis with limma
if(check_limma()){
group = gl(2, n)
design = model.matrix(~ group)
fit1 = limma::lmFit(X, design)
fit = limma::eBayes(fit1)

### Calculation of confidence intervals
CI = fc_ci(fit=fit, alpha=0.05, method="raw")
head(CI)
CI = fc_ci(fit=fit, alpha=0.05, method="BH")
head(CI)
CI = fc_ci(fit=fit, alpha=0.05, method="BY")
head(CI)

fc_plot(CI, xlim=c(-0.5, 3), ylim=-log10(c(1, 0.0001)), updown="up")
fc_plot(CI, xlim=c(-3, 0.5), ylim=-log10(c(1, 0.0001)), updown="down")
fc_plot(CI, xlim=c(-3, 3), ylim=-log10(c(1, 0.0001)), updown="all")
}



cleanEx()
nameEx("fc_plot")
### * fc_plot

flush(stderr()); flush(stdout())

### Name: fc_plot
### Title: Volcano plot of adjusted confidence intervals
### Aliases: fc_plot

### ** Examples

### Artificial microarray data
d = 1000 ### Number of genes
n = 10 ### Sample per group
fc = rlnorm(d, 0, 0.1)
mu1 = rlnorm(d, 0, 1) ### Mean vector group 1
mu2 = mu1 * fc ### Mean vector group 2
sd1 = rnorm(d, 1, 0.2)
sd2 = rnorm(d, 1, 0.2)
X1 = matrix(NA, d, n) ### Expression levels group 1
X2 = matrix(NA, d, n) ### Expression levels group 2
for (i in 1:n) {
  X1[,i] = rnorm(d, mu1, sd=sd1)
  X2[,i] = rnorm(d, mu2, sd=sd2)
}
X = cbind(X1, X2)
heatmap(X)

### Differential expression analysis with limma
if(check_limma()){
group = gl(2, n)
design = model.matrix(~ group)
fit1 = limma::lmFit(X, design)
fit = limma::eBayes(fit1)

### Calculation of confidence intervals
CI = fc_ci(fit=fit, alpha=0.05, method="raw")
head(CI)
CI = fc_ci(fit=fit, alpha=0.05, method="BH")
head(CI)
CI = fc_ci(fit=fit, alpha=0.05, method="BY")
head(CI)

fc_plot(CI, xlim=c(-0.5, 3), ylim=-log10(c(1, 0.0001)), updown="up")
fc_plot(CI, xlim=c(-3, 0.5), ylim=-log10(c(1, 0.0001)), updown="down")
fc_plot(CI, xlim=c(-3, 3), ylim=-log10(c(1, 0.0001)), updown="all")
}



cleanEx()
nameEx("gem")
### * gem

flush(stderr()); flush(stdout())

### Name: gem
### Title: Plots a gemstone to an interactive graphics device
### Aliases: gem

### ** Examples

## Attention: calculation is currently time-consuming.
## Not run: 
##D 
##D # Two 3-dimensional example data sets D1 and D2
##D n <- 200
##D x1 <- rnorm(n, 0, 1)
##D y1 <- rnorm(n, 0, 1)
##D z1 <- rnorm(n, 0, 1)
##D D1 <- data.frame(cbind(x1, y1, z1))
##D x2 <- rnorm(n, 1, 1)
##D y2 <- rnorm(n, 1, 1)
##D z2 <- rnorm(n, 1, 1)
##D D2 <- data.frame(cbind(x2, y2, z2))
##D colnames(D1) <- c("x", "y", "z")
##D colnames(D2) <- c("x", "y", "z")
##D 
##D # Placing outliers in D1 and D2
##D D1[17,] = c(4, 5, 6)
##D D2[99,] = -c(3, 4, 5)
##D 
##D # Grid size and graphic parameters
##D grid.size <- 20
##D red <- rgb(200, 100, 100, alpha = 100, maxColorValue = 255)
##D blue <- rgb(100, 100, 200, alpha = 100, maxColorValue = 255)
##D yel <- rgb(255, 255, 102, alpha = 100, maxColorValue = 255)
##D white <- rgb(255, 255, 255, alpha = 100, maxColorValue = 255)
##D require(rgl)
##D material3d(color=c(red, blue, yel, white),
##D alpha=c(0.5, 0.5, 0.5, 0.5), smooth=FALSE, specular="black")
##D 
##D # Calucation and visualization of gemplot for D1
##D G <- gridfun(D1, grid.size=20)
##D G$H <- hldepth(D1, G, verbose=TRUE)
##D dm <- depmed(G)
##D B <- bag(D1, G)
##D L <- loop(D1, B, dm=dm)
##D bg3d(color = "gray39" )
##D points3d(D1[L$outliers==0,1], D1[L$outliers==0,2], D1[L$outliers==0,3], col="green")
##D text3d(D1[L$outliers==1,1], D1[L$outliers==1,2], D1[L$outliers==1,3],
##D as.character(which(L$outliers==1)), col=yel)
##D spheres3d(dm[1], dm[2], dm[3], col=yel, radius=0.1)
##D material3d(1,alpha=0.4)
##D gem(B$coords, B$hull, red)
##D gem(L$coords.loop, L$hull.loop, red)
##D axes3d(col="white")
##D 
##D # Calucation and visualization of gemplot for D2
##D G <- gridfun(D2, grid.size=20)
##D G$H <- hldepth(D2, G, verbose=TRUE)
##D dm <- depmed(G)
##D B <- bag(D2, G)
##D L <- loop(D2, B, dm=dm)
##D points3d(D2[L$outliers==0,1], D2[L$outliers==0,2], D2[L$outliers==0,3], col="green")
##D text3d(D2[L$outliers==1,1], D2[L$outliers==1,2], D2[L$outliers==1,3],
##D as.character(which(L$outliers==1)), col=yel)
##D spheres3d(dm[1], dm[2], dm[3], col=yel, radius=0.1)
##D gem(B$coords, B$hull, blue)
##D gem(L$coords.loop, L$hull.loop, blue)
##D 
##D # Example of outlier detection with four principal components.
##D # Attention: calculation is currently time-consuming.
##D 
##D set.seed(123)
##D n <- 200
##D x1 <- rnorm(n, 0, 1)
##D x2 <- rnorm(n, 0, 1)
##D x3 <- rnorm(n, 0, 1)
##D x4 <- rnorm(n, 0, 1)
##D D <- data.frame(cbind(x1, x2, x3, x4))
##D D[67,] = c(7, 0, 0, 0)
##D 
##D date()
##D G = gridfun(D, 20, 4)
##D G$H = hldepth(D, G, verbose=TRUE)
##D dm = depmed(G)
##D B = bag(D, G)
##D L = loop(D, B, dm=dm)
##D which(L$outliers==1)
##D date()
## End(Not run)



cleanEx()
nameEx("hldepth")
### * hldepth

flush(stderr()); flush(stdout())

### Name: hldepth
### Title: Calculates the halfspace location depth
### Aliases: hldepth

### ** Examples

## Attention: calculation is currently time-consuming.
## Not run: 
##D 
##D # A 3-dimensional example data set D1
##D n <- 200
##D x1 <- rnorm(n, 0, 1)
##D y1 <- rnorm(n, 0, 1)
##D z1 <- rnorm(n, 0, 1)
##D D1 <- data.frame(cbind(x1, y1, z1))
##D colnames(D1) <- c("x", "y", "z")
##D 
##D # Specification of the grid and calculation of the halfspace location depth at each grid location.
##D G <- gridfun(D1, grid.size=20)
##D G$H <- hldepth(D1, G, verbose=TRUE)
## End(Not run)



cleanEx()
nameEx("iter_matrix")
### * iter_matrix

flush(stderr()); flush(stdout())

### Name: iter_matrix
### Title: Genetic algorithm for generating correlated binary data
### Aliases: iter_matrix

### ** Examples

### Generation of the representive matrix Xt
X0 <- start_matrix(p = c(0.5, 0.6), k = 1000)
Xt <- iter_matrix(X0, R = diag(2),  T = 10000,e.min = 0.00001)$Xt

### Drawing of a random sample S of size n = 10
S <- Xt[sample(1:1000, 10, replace = TRUE),]



cleanEx()
nameEx("loop")
### * loop

flush(stderr()); flush(stdout())

### Name: loop
### Title: Calculates the fence and the loop
### Aliases: loop

### ** Examples

## Attention: calculation is currently time-consuming.
## Not run: 
##D 
##D # Two 3-dimensional example data sets D1 and D2
##D n <- 200
##D x1 <- rnorm(n, 0, 1)
##D y1 <- rnorm(n, 0, 1)
##D z1 <- rnorm(n, 0, 1)
##D D1 <- data.frame(cbind(x1, y1, z1))
##D x2 <- rnorm(n, 1, 1)
##D y2 <- rnorm(n, 1, 1)
##D z2 <- rnorm(n, 1, 1)
##D D2 <- data.frame(cbind(x2, y2, z2))
##D colnames(D1) <- c("x", "y", "z")
##D colnames(D2) <- c("x", "y", "z")
##D 
##D # Placing outliers in D1 and D2
##D D1[17,] = c(4, 5, 6)
##D D2[99,] = -c(3, 4, 5)
##D 
##D # Grid size and graphic parameters
##D grid.size <- 20
##D red <- rgb(200, 100, 100, alpha = 100, maxColorValue = 255)
##D blue <- rgb(100, 100, 200, alpha = 100, maxColorValue = 255)
##D yel <- rgb(255, 255, 102, alpha = 100, maxColorValue = 255)
##D white <- rgb(255, 255, 255, alpha = 100, maxColorValue = 255)
##D require(rgl)
##D material3d(color=c(red, blue, yel, white),
##D  alpha=c(0.5, 0.5, 0.5, 0.5), smooth=FALSE, specular="black")
##D 
##D # Calucation and visualization of gemplot for D1
##D G <- gridfun(D1, grid.size=20)
##D G$H <- hldepth(D1, G, verbose=TRUE)
##D dm <- depmed(G)
##D B <- bag(D1, G)
##D L <- loop(D1, B, dm=dm)
##D bg3d(color = "gray39" )
##D points3d(D1[L$outliers==0,1], D1[L$outliers==0,2], D1[L$outliers==0,3], col="green")
##D text3d(D1[L$outliers==1,1], D1[L$outliers==1,2], D1[L$outliers==1,3],
##D as.character(which(L$outliers==1)), col=yel)
##D spheres3d(dm[1], dm[2], dm[3], col=yel, radius=0.1)
##D material3d(1,alpha=0.4)
##D gem(B$coords, B$hull, red)
##D gem(L$coords.loop, L$hull.loop, red)
##D axes3d(col="white")
##D 
##D # Calucation and visualization of gemplot for D2
##D G <- gridfun(D2, grid.size=20)
##D G$H <- hldepth(D2, G, verbose=TRUE)
##D dm <- depmed(G)
##D B <- bag(D2, G)
##D L <- loop(D2, B, dm=dm)
##D points3d(D2[L$outliers==0,1], D2[L$outliers==0,2], D2[L$outliers==0,3], col="green")
##D text3d(D2[L$outliers==1,1], D2[L$outliers==1,2], D2[L$outliers==1,3],
##D as.character(which(L$outliers==1)), col=yel)
##D spheres3d(dm[1], dm[2], dm[3], col=yel, radius=0.1)
##D gem(B$coords, B$hull, blue)
##D gem(L$coords.loop, L$hull.loop, blue)
## End(Not run)



cleanEx()
nameEx("netRNA")
### * netRNA

flush(stderr()); flush(stdout())

### Name: netRNA
### Title: netRNA:Network meta-analysis for gene expression data
### Aliases: netRNA

### ** Examples


## Not run: 
##D #'#######################
##D ### Data generation ###
##D #######################
##D n = 100 ### Sample size per group
##D G = 100 ### Number of genes
##D 
##D ### Basic expression, fold change, batch effects and error
##D alpha.1 = rnorm(G, 0, 1)
##D alpha.2 = rnorm(G, 0, 1)
##D beta.1 = rnorm(G, 0, 1)
##D beta.2 = rnorm(G, 0, 1)
##D gamma.1 = rnorm(G, 0, 1)
##D gamma.2 = rnorm(G, 2, 1)
##D delta.1 = sqrt(invgamma::rinvgamma(G, 1, 1))
##D delta.2 = sqrt(invgamma::rinvgamma(G, 1, 2))
##D sigma.g = rep(1, G)
##D 
##D # Generate gene names
##D gene_names <- paste("Gene", 1:G, sep = "")
##D 
##D ### Data matrices of control and treatment (disease) groups
##D C.1 = matrix(NA, G, n)
##D C.2 = matrix(NA, G, n)
##D T.1 = matrix(NA, G, n)
##D T.2 = matrix(NA, G, n)
##D 
##D for (j in 1:n) {
##D  C.1[,j] = alpha.1 + (0 * beta.1) + gamma.1 + (delta.1 * rnorm(1, 0, sigma.g))
##D  C.2[,j] = alpha.1 + (0 * beta.2) + gamma.2 + (delta.2 * rnorm(1, 0, sigma.g))
##D  T.1[,j] = alpha.2 + (1 * beta.1) + gamma.1 + (delta.1 * rnorm(1, 0, sigma.g))
##D  T.2[,j] = alpha.2 + (1 * beta.2) + gamma.2 + (delta.2 * rnorm(1, 0, sigma.g))
##D }
##D 
##D study1 = cbind(C.1, T.1)
##D study2 = cbind(C.2, T.2)
##D 
##D # Assign gene names to row names
##D #rownames(study1) <- gene_names
##D #rownames(study2) <- gene_names
##D #############################
##D ### Differential Analysis ###
##D #############################
##D 
##D if(check_limma()){
##D ### study1: treatment A versus control
##D group = gl(2, n)
##D M = model.matrix(~ group)
##D fit = limma::lmFit(study1, M)
##D fit = limma::eBayes(fit)
##D p.S1 = fit$p.value[,2]
##D fc.S1 = fit$coefficients[,2]
##D fce.S1 = sqrt(fit$s2.post) * sqrt(fit$cov.coefficients[2,2])
##D 
##D ### study2: treatment B versus control
##D group = gl(2, n)
##D M = model.matrix(~ group)
##D fit = limma::lmFit(study2, M)
##D fit = limma::eBayes(fit)
##D p.S2 = fit$p.value[,2]
##D fc.S2 = fit$coefficients[,2]
##D fce.S2 = sqrt(fit$s2.post) * sqrt(fit$cov.coefficients[2,2])
##D 
##D 
##D 
##D #############################
##D ### Network meta-analysis ###
##D #############################
##D p.net = rep(NA, G)
##D fc.net = rep(NA, G)
##D treat1 = c("uninfected", "uninfected")
##D treat2 = c("ZIKA", "HSV1")
##D studlab = c("experiment1", "experiment2")
##D fc.true = beta.2 - beta.1
##D 
##D TEs <- list(fc.S1, fc.S2)
##D seTEs <- list(fce.S1, fce.S2)
##D }
##D 
##D # Example usage:
##D test <- netRNA(TE = TEs, seTE = seTEs, treat1 = treat1, treat2 = treat2, studlab = studlab)
## End(Not run)



cleanEx()
nameEx("rho_bounds")
### * rho_bounds

flush(stderr()); flush(stdout())

### Name: rho_bounds
### Title: Calculate lower and upper the bounds for pairwise correlations
### Aliases: rho_bounds

### ** Examples

### A simple example
R <- diag(4)
p <- c(0.1, 0.2, 0.4, 0.5)

rho_bounds(R, p)



cleanEx()
nameEx("rmvbinary_EP")
### * rmvbinary_EP

flush(stderr()); flush(stdout())

### Name: rmvbinary_EP
### Title: Simulating correlated binary variables using the algorithm by
###   Emrich and Piedmonte (1991)
### Aliases: rmvbinary_EP

### ** Examples

## Generation of a random sample
rmvbinary_EP(n = 10, R = diag(2), p = c(0.5, 0.6))



cleanEx()
nameEx("rmvbinary_QA")
### * rmvbinary_QA

flush(stderr()); flush(stdout())

### Name: rmvbinary_QA
### Title: Simulating correlated binary variables using the algorithm by
###   Qaqish (2003)
### Aliases: rmvbinary_QA

### ** Examples

## Generation of a random sample
rmvbinary_QA(n = 10, R = diag(2), p = c(0.5, 0.6))



cleanEx()
nameEx("scTC_bpplot")
### * scTC_bpplot

flush(stderr()); flush(stdout())

### Name: scTC_bpplot
### Title: scTC_bpplot: Post-trim breakpoint heatmap for scTrimClust
###   results
### Aliases: scTC_bpplot

### ** Examples

## Not run: 
##D scTC_bpplot(
##D   covid_markers = RepeatedHighDim:::covid_markers,
##D   robust_covid_markers = RepeatedHighDim:::robust_covid_markers,
##D   robust_covid_markers_02trim = RepeatedHighDim:::robust_covid_markers_02trim,
##D   robust_covid_markers_03trim = RepeatedHighDim:::robust_covid_markers_03trim,
##D   robust_covid_data = RepeatedHighDim:::robust_covid_data,
##D   trim_percent_vector = c(0, 10, 20, 30, 40),
##D   plot_title = "CLR, nPCs:5, nFeatures:1000",
##D   legend_title = "Percent markers of non-trimmed"
##D )
## End(Not run)




cleanEx()
nameEx("scTC_trim_effect")
### * scTC_trim_effect

flush(stderr()); flush(stdout())

### Name: scTC_trim_effect
### Title: scTC_trim_effect: Compare scTrimClust trimming against default
###   Seurat analysis
### Aliases: scTC_trim_effect

### ** Examples

## Not run: 
##D 
##D method_pairs <- list(
##D   CLR = list(
##D     data1 = RepeatedHighDim:::scTC_eff_clr,
##D     data2 = RepeatedHighDim:::scTC_eff_clr_robust
##D   ),
##D   LogNorm = list(
##D     data1 = RepeatedHighDim:::scTC_eff_log,
##D     data2 = RepeatedHighDim:::scTC_eff_log_robust
##D   )
##D )
##D 
##D method_colors <- setNames(grey.colors(2), c("CLR", "LogNorm"))
##D 
##D scTC_trim_effect(
##D   method_pairs = method_pairs,
##D   method_colors = method_colors,
##D   column_title = "nPCs:5, nFeatures:1000"
##D )
##D 
##D set_colors <- grey.colors(3)
##D names(set_colors) <- c("S1:standard", "S2:intersect", "S3:trimmed")
##D 
##D scTC_trim_effect(
##D   method_pairs = method_pairs,
##D   method_colors = method_colors,
##D   set_colors = setNames(c("blue", "green", "red"), names(set_colors)),
##D   heatmap_color_palette = colorRamp2(c(0, 50, 100), c("white", "pink", "purple")),
##D   column_title = "Custom Color Example"
##D )
## End(Not run)




cleanEx()
nameEx("scTrimClust")
### * scTrimClust

flush(stderr()); flush(stdout())

### Name: scTrimClust
### Title: scTrimClust: Cluster visualization with alpha hull-based outlier
###   detection
### Aliases: scTrimClust

### ** Examples

## Not run: 
##D 
##D scTrimClust(RepeatedHighDim:::seurat_obj,reduction = 'tsne',
##D group.by = 'CellType',
##D hull.alpha = 2,
##D remove.outliers = FALSE,
##D outlier.quantile = 0.2,
##D outlier.alpha = 0.3,
##D outlier.color = "red",
##D pt.size = 5,
##D outline.color = "black",
##D outline.outliers = TRUE)
##D 
##D # second example with custom outlier col per cluster
##D 
##D scTrimClust(RepeatedHighDim:::seurat_obj,reduction = 'tsne',
##D group.by = 'CellType',
##D hull.alpha = 2,
##D remove.outliers = FALSE,
##D outlier.quantile = 0.2,
##D outlier.alpha = 0.3,
##D outlier.colors = c('TypeA'="black",
##D 'TypeB'='violet','TypeC' ='pink'),
##D pt.size = 5,
##D outline.color = "black",
##D outline.outliers = TRUE)$plot
##D 
## End(Not run)




cleanEx()
nameEx("sequence_probs")
### * sequence_probs

flush(stderr()); flush(stdout())

### Name: sequence_probs
### Title: Calculation of probabilities for binary sequences
### Aliases: sequence_probs

### ** Examples

### Generation of the representive matrix Xt
X0 <- start_matrix(p = c(0.5, 0.6), k = 1000)
Xt <- iter_matrix(X0, R = diag(2), T = 10000, e.min = 0.00001)$Xt

### Calculation of probabilities for binary sequences
sequence_probs(Xt = Xt)



cleanEx()
nameEx("start_matrix")
### * start_matrix

flush(stderr()); flush(stdout())

### Name: start_matrix
### Title: Setup of the start matrix
### Aliases: start_matrix

### ** Examples

X0 <- start_matrix(p = c(0.5, 0.6), k = 10000)

## check if p can be restored
apply(X0, 2, mean)



cleanEx()
nameEx("summary_RHD")
### * summary_RHD

flush(stderr()); flush(stdout())

### Name: summary_RHD
### Title: Summary of RHighDim function
### Aliases: summary_RHD

### ** Examples

### Global comparison of a set of 100 genes between two experimental groups.
X1 = matrix(rnorm(1000, 0, 1), 10, 100)
X2 = matrix(rnorm(1000, 0.1, 1), 10, 100)
RHD = RHighDim (X1, X2, paired=FALSE)
summary_RHD(RHD)



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

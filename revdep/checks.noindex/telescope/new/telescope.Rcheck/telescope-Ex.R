pkgname <- "telescope"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('telescope')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("plotBubble")
### * plotBubble

flush(stderr()); flush(stdout())

### Name: plotBubble
### Title: Plot multivariate categorical data.
### Aliases: plotBubble

### ** Examples

with(chickwts, plotBubble(data.frame(cut(weight, 100 * 1:5), feed)))



cleanEx()
nameEx("plotScatter")
### * plotScatter

flush(stderr()); flush(stdout())

### Name: plotScatter
### Title: Pairwise scatter plots of the data.
### Aliases: plotScatter

### ** Examples

plotScatter(iris[, 1:4], iris$Species, label = "dim")



cleanEx()
nameEx("sampleLCA")
### * sampleLCA

flush(stderr()); flush(stdout())

### Name: sampleLCA
### Title: Telescoping sampling of the LCA model where a prior on the
###   number of components K is specified.
### Aliases: sampleLCA

### ** Examples

if (requireNamespace("poLCA", quietly = TRUE)) {
    data("carcinoma", package = "poLCA")
    y <- carcinoma
    N <- nrow(y)
    r <- ncol(y)
    
    M <- 200
    thin <- 1
    burnin <- 100
    Kmax <- 50  
    Kinit <- 10
    
    G <- "MixDynamic"
    priorOnAlpha <- priorOnAlpha_spec("gam_1_2")
    priorOnK <- priorOnK_spec("Pois_1")
    
    cat <- apply(y, 2, max)
    a0 <- rep(1, sum(cat))

    cl_y <- kmeans(y, centers = Kinit, iter.max = 20)
    S_0 <- cl_y$cluster
    eta_0 <- cl_y$size/N

    pi_0 <- do.call("cbind", lapply(1:r, function(j) {
        prop.table(table(S_0, y[, j]), 1)
    }))

    result <- sampleLCA(
        y, S_0, pi_0, eta_0, a0, 
        M, burnin, thin, Kmax, 
        G, priorOnK, priorOnAlpha)

    K <- result$K
    Kplus <- result$Kplus   
    
    plot(K, type = "l", ylim = c(0, max(K)),  
         xlab = "iteration", main = "",
         ylab = expression("K" ~ "/" ~ K["+"]), col = 1)
    lines(Kplus, col = 2)
    legend("topright", legend = c("K", expression(K["+"])),
           col = 1:2, lty = 1, box.lwd = 0)
}




cleanEx()
nameEx("sampleLCAMixture")
### * sampleLCAMixture

flush(stderr()); flush(stdout())

### Name: sampleLCAMixture
### Title: Telescoping sampling of the mixture of LCA models where a prior
###   on the number of components K is specified.
### Aliases: sampleLCAMixture

### ** Examples

data("SimData", package = "telescope")
y <- as.matrix(SimData[, 1:30])
z <- SimData[, 31]
N <- nrow(y)
r <- ncol(y)
    
M <- 5
thin <- 1
burnin <- 0
Kmax <- 50  
Kinit <- 10
    
G <- "MixDynamic"
priorOnAlpha <- priorOnAlpha_spec("gam_1_2")
priorOnK <- priorOnK_spec("Pois_1")
d0 <- 1  

cat <- apply(y, 2, max)
a_mu <- rep(20, sum(cat))
mu_0 <- matrix(rep(rep(1/cat, cat), Kinit),
  byrow = TRUE, nrow = Kinit)

c_phi <- 30; d_phi <- 1; b_phi <- rep(10, r)
a_phi <- rep(1, r)
phi_0 <- matrix(cat, Kinit, r, byrow = TRUE)

a_00 <- 0.05

s_mu <- 2; s_phi <- 2; eps <- 0.01 

set.seed(1234)
cl_y <- kmeans(y, centers = Kinit, nstart = 100, iter.max = 50)
S_0 <- cl_y$cluster
eta_0 <- cl_y$size/N

I_0 <- rep(1L, N)
L <- 2
for (k in 1:Kinit) {
  cl_size <- sum(S_0 == k)
  I_0[S_0 == k] <- rep(1:L, length.out = cl_size)
}

index <- c(0, cumsum(cat))
low <- (index + 1)[-length(index)]
up <- index[-1]

pi_km <- array(NA_real_, dim = c(Kinit, L, sum(cat)))
rownames(pi_km) <- paste0("k_", 1:Kinit)
for (k in 1:Kinit) {
  for (l in 1:L) {
    index <- (S_0 == k) & (I_0 == l)
    for (j in 1:r) {
      pi_km[k, l, low[j]:up[j]] <- tabulate(y[index, j], cat[j]) / sum(index)
    }
  }
}
pi_0 <- pi_km 

result <- sampleLCAMixture(
    y, S_0, L,
    pi_0, eta_0, mu_0, phi_0,
    a_00, a_mu, a_phi, b_phi, c_phi, d_phi,
    M, burnin, thin, Kmax,
    s_mu, s_phi, eps,
    G, priorOnAlpha, d0, priorOnK)



cleanEx()
nameEx("sampleMultNormMixture")
### * sampleMultNormMixture

flush(stderr()); flush(stdout())

### Name: sampleMultNormMixture
### Title: Telescoping sampling of a Bayesian finite multivariate Gaussian
###   mixture where a prior on the number of components is specified.
### Aliases: sampleMultNormMixture

### ** Examples

y <- iris[, 1:4]
z <- iris$Species
r <- ncol(y)

M <- 50
thin <- 1
burnin <- 0
Kmax <- 40  
Kinit <- 10

G <- "MixStatic"      
priorOnE0 <- priorOnE0_spec("G_1_20", 1)
priorOnK <- priorOnK_spec("BNB_143")

R <- apply(y, 2, function(x) diff(range(x)))
b0 <- apply(y, 2, median)
B_0 <- rep(1, r)  
B0 <- diag((R^2) * B_0)
c0 <- 2.5 + (r-1)/2
g0 <- 0.5 + (r-1)/2
G0 <- 100 * g0/c0 * diag((1/R^2), nrow = r)
C0 <- g0 * chol2inv(chol(G0))

cl_y <- kmeans(y, centers = Kinit, nstart = 100)
S_0 <- cl_y$cluster
mu_0 <- t(cl_y$centers)

eta_0 <- rep(1/Kinit, Kinit)
Sigma_0 <- array(0, dim = c(r, r, Kinit))
Sigma_0[, , 1:Kinit] <- 0.5 * C0

result <- sampleMultNormMixture(
  y, S_0, mu_0, Sigma_0, eta_0,
  c0, g0, G0, C0, b0, B0,  
  M, burnin, thin, Kmax, G, priorOnK, priorOnE0)

K <- result$K
Kplus <- result$Kplus   

plot(K, type = "l", ylim = c(0, max(K)),
     xlab = "iteration", main = "",
     ylab = expression("K" ~ "/" ~ K["+"]), col = 1)
lines(Kplus, col = 2)
legend("topright", legend = c("K", expression(K["+"])),
       col = 1:2, lty = 1, box.lwd = 0)




cleanEx()
nameEx("samplePoisMixture")
### * samplePoisMixture

flush(stderr()); flush(stdout())

### Name: samplePoisMixture
### Title: Telescoping sampling of a Bayesian finite Poisson mixture with a
###   prior on the number of components K.
### Aliases: samplePoisMixture

### ** Examples

N <- 200
z <- sample(1:2, N, prob = c(0.5, 0.5), replace = TRUE)
y <- rpois(N, c(1, 6)[z])

M <- 200
thin <- 1
burnin <- 100

Kmax <- 50  
Kinit <- 10

G <- "MixDynamic"
priorOnAlpha <- priorOnAlpha_spec("gam_1_2")
priorOnK <- priorOnK_spec("BNB_143")

a0 <- 0.1 
h0 <- 0.5 
b0 <- a0/mean(y) 
H0 <- h0/b0

cl_y <- kmeans(y, centers = Kinit, nstart = 100)
S_0 <- cl_y$cluster
mu_0 <- t(cl_y$centers)
eta_0 <- rep(1/Kinit, Kinit)

result <- samplePoisMixture(
  y, S_0, mu_0, eta_0, 
  a0, b0, h0, H0,
  M, burnin, thin, Kmax, 
  G, priorOnK, priorOnAlpha)

K <- result$K
Kplus <- result$Kplus

plot(K, type = "l", ylim = c(0, max(K)), 
     xlab = "iteration", main = "",
     ylab = expression("K" ~ "/" ~ K["+"]), col = 1)
lines(Kplus, col = 2)
legend("topright", legend = c("K", expression(K["+"])),
       col = 1:2, lty = 1, box.lwd = 0)




cleanEx()
nameEx("sampleUniNormMixture")
### * sampleUniNormMixture

flush(stderr()); flush(stdout())

### Name: sampleUniNormMixture
### Title: Telescoping sampling of a Bayesian finite univariate Gaussian
###   mixture where a prior on the number of components K is specified.
### Aliases: sampleUniNormMixture

### ** Examples

if (requireNamespace("mclust", quietly = TRUE)) {
    data("acidity", package = "mclust")
    y <- acidity
    
    N <- length(y)
    r <- 1
    
    M <- 200
    thin <- 1
    burnin <- 100
    Kmax <- 50  
    Kinit <- 10
    
    G <- "MixStatic" 
    priorOnE0 <- priorOnE0_spec("e0const", 0.01)
    priorOnK <- priorOnK_spec("Pois_1", 50)
    
    R <- diff(range(y))
    c0 <- 2 + (r-1)/2
    C0 <- diag(c(0.02*(R^2)), nrow = r)
    g0 <- 0.2 + (r-1) / 2
    G0 <- diag(10/(R^2), nrow = r)
    B0 <- diag((R^2), nrow = r)
    b0 <- as.matrix((max(y) + min(y))/2, ncol = 1)  
    
    cl_y <- kmeans(y, centers = Kinit, nstart = 100)
    S_0 <- cl_y$cluster
    mu_0 <- t(cl_y$centers)
    eta_0 <- rep(1/Kinit, Kinit)
    sigma2_0 <- array(0, dim = c(1, 1, Kinit))
    sigma2_0[1, 1, ] <- 0.5 * C0

    result <- sampleUniNormMixture(
        y, S_0, mu_0, sigma2_0, eta_0,
        c0, g0, G0, C0, b0, B0,
        M, burnin, thin, Kmax,
        G, priorOnK, priorOnE0)
    
    K <- result$K
    Kplus <- result$Kplus  
    
    plot(K, type = "l", ylim = c(0, max(K)),
         xlab = "iteration", main = "",
         ylab = expression("K" ~ "/" ~ K["+"]), col = 1)
    lines(Kplus, col = 2)
    legend("topright", legend = c("K", expression(K["+"])),
           col = 1:2, lty = 1, box.lwd = 0)
}




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

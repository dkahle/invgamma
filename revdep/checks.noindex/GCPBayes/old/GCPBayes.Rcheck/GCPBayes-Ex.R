pkgname <- "GCPBayes"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('GCPBayes')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("CS")
### * CS

flush(stderr()); flush(stdout())

### Name: CS
### Title: Continuous Spike
### Aliases: CS

### ** Examples

############################# Gene DNAJC1 ###############################################
data(DNAJC1)
Breast <- DNAJC1$Breast
Thyroid <- DNAJC1$Thyroid
genename <- "DNAJC1"
snpnames <- Breast$snp
Betah <- list(Breast$beta, Thyroid$beta)
Sigmah <- list(diag(Breast$se^2), diag(Thyroid$se^2))
K <- 2
m <- 14

pvalue <- matrix(0, K, m)
for (k in 1:K) {
  pvalue[k, ] <- 2 * pnorm(-abs(Betah[[k]] / sqrt(diag(Sigmah[[k]]))))
}

zinit <- rep(0, K)
for (j in 1:K) {
  index <- 1:m
  PVALUE <- p.adjust(pvalue[j, ])
  SIGNALS <- index[PVALUE < 0.05]
  modelf1 <- rep(0, m)
  modelf1[SIGNALS] <- 1
  if (max(modelf1) == 1) (zinit[j] <- 1)
}


RES <- CS(Betah, Sigmah,
  kappa0 = 0.5, tau20 = 1, zeta0 = zinit,
  m = m, K = K, niter = 2000, burnin = 1000, nthin = 2, nchains = 1, a1 = 0.1, a2 = 0.1,
  c1 = 0.1, c2 = 0.1, sigma2 = 10^-3, snpnames = snpnames, genename = genename
)
## Not run: 
##D   print(RES)
##D 
##D 
##D   RES1 <- CS(Betah, Sigmah,
##D     kappa0 = c(0.2, 0.5), tau20 = c(1, 2), zeta0 = zinit,
##D     m = m, K = K, niter = 2000, burnin = 1000, nthin = 2, nchains = 2,
##D     a1 = 0.1, a2 = 0.1, c1 = 0.1, c2 = 0.1, sigma2 = 10^-3, snpnames, genename
##D   )
##D   print(RES1)
##D   ################### Simulated summary level data with K=5 ###############################
##D   data(Simulated_summary)
##D   genename <- Simulated_summary$genename
##D   snpnames <- Simulated_summary$snpnames
##D   Betah <- Simulated_summary$simBeta
##D   Sigmah <- Simulated_summary$simSIGMA
##D   K <- 5
##D   m <- 10
##D 
##D   pvalue <- matrix(0, K, m)
##D   for (k in 1:K) {
##D     pvalue[k, ] <- 2 * pnorm(-abs(Betah[[k]] / sqrt(diag(Sigmah[[k]]))))
##D   }
##D 
##D   zinit <- rep(0, K)
##D   for (j in 1:K) {
##D     index <- 1:m
##D     PVALUE <- p.adjust(pvalue[j, ])
##D     SIGNALS <- index[PVALUE < 0.05]
##D     modelf1 <- rep(0, m)
##D     modelf1[SIGNALS] <- 1
##D     if (max(modelf1) == 1) (zinit[j] <- 1)
##D   }
##D 
##D 
##D   RES <- CS(Betah, Sigmah,
##D     kappa0 = 0.5, tau20 = 1, zeta0 = zinit,
##D     m = m, K = K, niter = 2000, burnin = 1000, nthin = 2, nchains = 1, a1 = 0.1, a2 = 0.1,
##D     c1 = 0.1, c2 = 0.1, sigma2 = 10^-3, snpnames = snpnames, genename = genename
##D   )
##D   print(RES)
##D 
##D 
##D   RES1 <- CS(Betah, Sigmah,
##D     kappa0 = c(0.2, 0.5), tau20 = c(1, 2), zeta0 = zinit,
##D     m = m, K = K, niter = 2000, burnin = 1000, nthin = 2, nchains = 2,
##D     a1 = 0.1, a2 = 0.1, c1 = 0.1, c2 = 0.1, sigma2 = 10^-3, snpnames, genename
##D   )
##D   print(RES1)
##D   ################################### Gene PARP2 ##########################################
##D   library(BhGLM)
##D   data(PARP2)
##D   Breast <- PARP2$Breast
##D   Thyroid <- PARP2$Thyroid
##D   genename <- "PARP2"
##D   snpnames <- c("rs3093872", "rs3093921", "rs1713411", "rs3093926", "rs3093930", "rs878156")
##D 
##D 
##D   Fit1 <- BhGLM::bglm(y1 ~ ., family = binomial(link = "logit"), data = Breast)
##D   Betah1 <- Fit1$coefficients[-1]
##D   Sigmah1 <- cov(coef(arm::sim(Fit1)))[-1, -1]
##D 
##D   Fit2 <- BhGLM::bglm(y2 ~ ., family = binomial(link = "logit"), data = Thyroid)
##D   Betah2 <- Fit2$coefficients[-1]
##D   Sigmah2 <- cov(coef(arm::sim(Fit2)))[-1, -1]
##D 
##D   Betah <- list(Betah1, Betah2)
##D   Sigmah <- list(Sigmah1, Sigmah2)
##D   K <- 2
##D   m <- 6
##D 
##D   pvalue <- matrix(0, K, m)
##D   for (k in 1:K) {
##D     pvalue[k, ] <- 2 * pnorm(-abs(Betah[[k]] / sqrt(diag(Sigmah[[k]]))))
##D   }
##D 
##D   zinit <- rep(0, K)
##D   for (j in 1:K) {
##D     index <- 1:m
##D     PVALUE <- p.adjust(pvalue[j, ])
##D     SIGNALS <- index[PVALUE < 0.05]
##D     modelf1 <- rep(0, m)
##D     modelf1[SIGNALS] <- 1
##D     if (max(modelf1) == 1) (zinit[j] <- 1)
##D   }
##D 
##D 
##D   RES <- CS(Betah, Sigmah,
##D     kappa0 = 0.5, tau20 = 1, zeta0 = zinit,
##D     m = m, K = K, niter = 2000, burnin = 1000, nthin = 2, nchains = 1, a1 = 0.1, a2 = 0.1,
##D     c1 = 0.1, c2 = 0.1, sigma2 = 10^-3, snpnames = snpnames, genename = genename
##D   )
##D   print(RES)
##D 
##D   RES1 <- CS(Betah, Sigmah,
##D     kappa0 = c(0.2, 0.5), tau20 = c(1, 2), zeta0 = zinit,
##D     m = m, K = K, niter = 2000, burnin = 1000, nthin = 2, nchains = 2,
##D     a1 = 0.1, a2 = 0.1, c1 = 0.1, c2 = 0.1, sigma2 = 10^-3, snpnames, genename
##D   )
##D   print(RES1)
##D   ########### Simulated individual level data with K=3 and continuous phynotype ###########
##D   library(BhGLM)
##D   data(Simulated_individual)
##D   Study1 <- Simulated_individual$Study1
##D   Study2 <- Simulated_individual$Study2
##D   Study3 <- Simulated_individual$Study3
##D   K <- 3
##D   m <- 30
##D   genename <- "Simulated"
##D   snpnames <- sprintf("SNP%s", seq(1:m))
##D 
##D 
##D   Fit1 <- BhGLM::bglm(Y1 ~ ., family = gaussian, data = data.frame(Study1))
##D   Betah1 <- Fit1$coefficients[-1]
##D   Sigmah1 <- cov(coef(arm::sim(Fit1)))[-1, -1]
##D 
##D   Fit2 <- BhGLM::bglm(Y2 ~ ., family = gaussian, data = data.frame(Study2))
##D   Betah2 <- Fit2$coefficients[-1]
##D   Sigmah2 <- cov(coef(arm::sim(Fit2)))[-1, -1]
##D 
##D   Fit3 <- BhGLM::bglm(Y3 ~ ., family = gaussian, data = data.frame(Study3))
##D   Betah3 <- Fit3$coefficients[-1]
##D   Sigmah3 <- cov(coef(arm::sim(Fit3)))[-1, -1]
##D 
##D   Betah <- list(Betah1, Betah2, Betah3)
##D   Sigmah <- list(Sigmah1, Sigmah2, Sigmah3)
##D 
##D 
##D   pvalue <- matrix(0, K, m)
##D   for (k in 1:K) {
##D     pvalue[k, ] <- 2 * pnorm(-abs(Betah[[k]] / sqrt(diag(Sigmah[[k]]))))
##D   }
##D 
##D   zinit <- rep(0, K)
##D   for (j in 1:K) {
##D     index <- 1:m
##D     PVALUE <- p.adjust(pvalue[j, ])
##D     SIGNALS <- index[PVALUE < 0.05]
##D     modelf1 <- rep(0, m)
##D     modelf1[SIGNALS] <- 1
##D     if (max(modelf1) == 1) (zinit[j] <- 1)
##D   }
##D 
##D   RES <- CS(Betah, Sigmah,
##D     kappa0 = 0.5, tau20 = 1, zeta0 = zinit,
##D     m = m, K = K, niter = 2000, burnin = 1000, nthin = 2, nchains = 1, a1 = 0.1, a2 = 0.1,
##D     c1 = 0.1, c2 = 0.1, sigma2 = 10^-3, snpnames = snpnames, genename = genename
##D   )
##D   print(RES)
##D 
##D   RES1 <- CS(Betah, Sigmah,
##D     kappa0 = c(0.2, 0.5), tau20 = c(1, 2), zeta0 = zinit,
##D     m = m, K = K, niter = 2000, burnin = 1000, nthin = 2, nchains = 2,
##D     a1 = 0.1, a2 = 0.1, c1 = 0.1, c2 = 0.1, sigma2 = 10^-3, snpnames, genename
##D   )
##D   print(RES1)
##D 
##D   ########### Simulated individual level data with K=2 and gene expression data ###########
##D   library(BhGLM)
##D   data(Simulated_individual_survival)
##D   Study1 <- Simulated_individual_survival$Study1
##D   Study2 <- Simulated_individual_survival$Study2
##D   K <- 2
##D   m <- 10
##D   genename <- "Simulated"
##D   snpnames <- sprintf("G%s", seq(1:m))
##D 
##D 
##D   Fit1 <- BhGLM::bcoxph(Study1$T ~ Study1$X)
##D   Betah1 <- Fit1$coefficients
##D   Sigmah1 <- Fit1$var
##D 
##D 
##D   Fit2 <- BhGLM::bcoxph(Study2$T ~ Study2$X)
##D   Betah2 <- Fit2$coefficients
##D   Sigmah2 <- Fit2$var
##D 
##D   Betah <- list(Betah1, Betah2)
##D   Sigmah <- list(Sigmah1, Sigmah2)
##D 
##D   pvalue <- matrix(0, K, m)
##D   for (k in 1:K) {
##D     pvalue[k, ] <- 2 * pnorm(-abs(Betah[[k]] / sqrt(diag(Sigmah[[k]]))))
##D   }
##D 
##D   zinit <- rep(0, K)
##D   for (j in 1:K) {
##D     index <- 1:m
##D     PVALUE <- p.adjust(pvalue[j, ])
##D     SIGNALS <- index[PVALUE < 0.05]
##D     modelf1 <- rep(0, m)
##D     modelf1[SIGNALS] <- 1
##D     if (max(modelf1) == 1) (zinit[j] <- 1)
##D   }
##D 
##D 
##D   RES1 <- CS(Betah, Sigmah,
##D     kappa0 = c(0.2, 0.5), tau20 = c(1, 2), zeta0 = zinit,
##D     m = m, K = K, niter = 2000, burnin = 1000, nthin = 2, nchains = 2,
##D     a1 = 0.1, a2 = 0.1, c1 = 0.1, c2 = 0.1, sigma2 = 10^-3, snpnames, genename
##D   )
##D   print(RES1)
## End(Not run)



cleanEx()
nameEx("DS")
### * DS

flush(stderr()); flush(stdout())

### Name: DS
### Title: Dirac Spike
### Aliases: DS

### ** Examples

############################# Gene DNAJC1 ###############################################
data(DNAJC1)
Breast <- DNAJC1$Breast
Thyroid <- DNAJC1$Thyroid
genename <- "DNAJC1"
snpnames <- Breast$snp
Betah <- list(Breast$beta, Thyroid$beta)
Sigmah <- list(diag(Breast$se^2), diag(Thyroid$se^2))
K <- 2
m <- 14

RES <- DS(Betah, Sigmah,
  kappa0 = 0.5, sigma20 = 1,
  m = m, K = K, niter = 2000, burnin = 1000, nthin = 2, nchains = 1,
  a1 = 0.1, a2 = 0.1, d1 = 0.1, d2 = 0.1, snpnames, genename
)
## Not run: 
##D   print(RES)
##D 
##D   RES1 <- DS(Betah, Sigmah,
##D     kappa0 = c(0.2, 0.5), sigma20 = c(1, 2),
##D     m = m, K = K, niter = 2000, burnin = 1000, nthin = 2, nchains = 2,
##D     a1 = 0.1, a2 = 0.1, d1 = 0.1, d2 = 0.1, snpnames, genename
##D   )
##D   print(RES1)
##D   ################### Simulated summary level data with K=5 ###############################
##D   data(Simulated_summary)
##D   genename <- Simulated_summary$genename
##D   snpnames <- Simulated_summary$snpnames
##D   Betah <- Simulated_summary$simBeta
##D   Sigmah <- Simulated_summary$simSIGMA
##D   K <- 5
##D   m <- 10
##D 
##D   RES <- DS(Betah, Sigmah,
##D     kappa0 = 0.5, sigma20 = 1,
##D     m = m, K = K, niter = 2000, burnin = 1000, nthin = 2, nchains = 1,
##D     a1 = 0.1, a2 = 0.1, d1 = 0.1, d2 = 0.1, snpnames, genename
##D   )
##D   print(RES)
##D 
##D   RES1 <- DS(Betah, Sigmah,
##D     kappa0 = c(0.2, 0.5), sigma20 = c(1, 2),
##D     m = m, K = K, niter = 2000, burnin = 1000, nthin = 2, nchains = 2,
##D     a1 = 0.1, a2 = 0.1, d1 = 0.1, d2 = 0.1, snpnames, genename
##D   )
##D   print(RES1)
##D   ################################### Gene PARP2 ##########################################
##D   library(BhGLM)
##D   data(PARP2)
##D   Breast <- PARP2$Breast
##D   Thyroid <- PARP2$Thyroid
##D   genename <- "PARP2"
##D   snpnames <- c("rs3093872", "rs3093921", "rs1713411", "rs3093926", "rs3093930", "rs878156")
##D 
##D 
##D   Fit1 <- BhGLM::bglm(y1 ~ ., family = binomial(link = "logit"), data = Breast)
##D   Betah1 <- Fit1$coefficients[-1]
##D   Sigmah1 <- cov(coef(arm::sim(Fit1)))[-1, -1]
##D 
##D 
##D   Fit2 <- BhGLM::bglm(y2 ~ ., family = binomial(link = "logit"), data = Thyroid)
##D   Betah2 <- Fit2$coefficients[-1]
##D   Sigmah2 <- cov(coef(arm::sim(Fit2)))[-1, -1]
##D 
##D   Betah <- list(Betah1, Betah2)
##D   Sigmah <- list(Sigmah1, Sigmah2)
##D   K <- 2
##D   m <- 6
##D 
##D   RES <- DS(Betah, Sigmah,
##D     kappa0 = 0.5, sigma20 = 1,
##D     m = m, K = K, niter = 2000, burnin = 1000, nthin = 2, nchains = 1,
##D     a1 = 0.1, a2 = 0.1, d1 = 0.1, d2 = 0.1, snpnames, genename
##D   )
##D   print(RES)
##D 
##D   RES1 <- DS(Betah, Sigmah,
##D     kappa0 = c(0.2, 0.5), sigma20 = c(1, 2),
##D     m = m, K = K, niter = 2000, burnin = 1000, nthin = 2, nchains = 2,
##D     a1 = 0.1, a2 = 0.1, d1 = 0.1, d2 = 0.1, snpnames, genename
##D   )
##D   print(RES1)
##D   ########### Simulated individual level data with K=3 and continuous phynotype ###########
##D   library(BhGLM)
##D   data(Simulated_individual)
##D   Study1 <- Simulated_individual$Study1
##D   Study2 <- Simulated_individual$Study2
##D   Study3 <- Simulated_individual$Study3
##D   K <- 3
##D   m <- 30
##D   genename <- "Simulated"
##D   snpnames <- sprintf("SNP%s", seq(1:m))
##D 
##D 
##D   Fit1 <- BhGLM::bglm(Y1 ~ ., family = gaussian, data = data.frame(Study1))
##D   Betah1 <- Fit1$coefficients[-1]
##D   Sigmah1 <- cov(coef(arm::sim(Fit1)))[-1, -1]
##D 
##D 
##D   Fit2 <- BhGLM::bglm(Y2 ~ ., family = gaussian, data = data.frame(Study2))
##D   Betah2 <- Fit2$coefficients[-1]
##D   Sigmah2 <- cov(coef(arm::sim(Fit2)))[-1, -1]
##D 
##D 
##D   Fit3 <- BhGLM::bglm(Y3 ~ ., family = gaussian, data = data.frame(Study3))
##D   Betah3 <- Fit3$coefficients[-1]
##D   Sigmah3 <- cov(coef(arm::sim(Fit3)))[-1, -1]
##D 
##D   Betah <- list(Betah1, Betah2, Betah3)
##D   Sigmah <- list(Sigmah1, Sigmah2, Sigmah3)
##D 
##D 
##D   RES <- DS(Betah, Sigmah,
##D     kappa0 = 0.5, sigma20 = 1,
##D     m = m, K = K, niter = 2000, burnin = 1000, nthin = 2, nchains = 1,
##D     a1 = 0.1, a2 = 0.1, d1 = 0.1, d2 = 0.1, snpnames, genename
##D   )
##D   print(RES)
##D 
##D 
##D   RES1 <- DS(Betah, Sigmah,
##D     kappa0 = c(0.2, 0.5), sigma20 = c(1, 2),
##D     m = m, K = K, niter = 2000, burnin = 1000, nthin = 2, nchains = 2,
##D     a1 = 0.1, a2 = 0.1, d1 = 0.1, d2 = 0.1, snpnames, genename
##D   )
##D   print(RES1)
##D 
##D   ########### Simulated individual level data with K=2 and gene expression data ###########
##D   library(BhGLM)
##D   data(Simulated_individual_survival)
##D   Study1 <- Simulated_individual_survival$Study1
##D   Study2 <- Simulated_individual_survival$Study2
##D   K <- 2
##D   m <- 10
##D   genename <- "Simulated"
##D   snpnames <- sprintf("G%s", seq(1:m))
##D 
##D 
##D   Fit1 <- BhGLM::bcoxph(Study1$T ~ Study1$X)
##D   Betah1 <- Fit1$coefficients
##D   Sigmah1 <- Fit1$var
##D 
##D 
##D   Fit2 <- BhGLM::bcoxph(Study2$T ~ Study2$X)
##D   Betah2 <- Fit2$coefficients
##D   Sigmah2 <- Fit2$var
##D 
##D   Betah <- list(Betah1, Betah2)
##D   Sigmah <- list(Sigmah1, Sigmah2)
##D 
##D 
##D 
##D   RES1 <- DS(Betah, Sigmah,
##D     kappa0 = c(0.2, 0.5), sigma20 = c(1, 2),
##D     m = m, K = K, niter = 2000, burnin = 1000, nthin = 2, nchains = 2,
##D     a1 = 0.1, a2 = 0.1, d1 = 0.1, d2 = 0.1, snpnames, genename
##D   )
##D   print(RES1)
## End(Not run)



cleanEx()
nameEx("HS")
### * HS

flush(stderr()); flush(stdout())

### Name: HS
### Title: Hierarchical Spike
### Aliases: HS

### ** Examples

############################# PARP2_summary ###############################################
data(PARP2_summary)
Breast <- PARP2_summary$Breast
Thyroid <- PARP2_summary$Thyroid
Betah <- list(Breast$beta, Thyroid$beta)
Sigmah <- list(diag(Breast$se), diag(Thyroid$se))
genename <- "PARP2"
snpnames <- Breast$snp
K <- 2
m <- 6

RES <- HS(Betah, Sigmah,
  kappa0 = 0.5, kappastar0 = 0.5, sigma20 = 1, s20 = 1,
  m = m, K = K, niter = 1000, burnin = 500, nthin = 1, nchains = 1,
  a1 = 0.1, a2 = 0.1, d1 = 0.1, d2 = 0.1, c1 = 1, c2 = 1, e2 = 1, snpnames, genename
)
## Not run: 
##D   print(RES)
##D   ############################# Gene DNAJC1 ###############################################
##D   data(DNAJC1)
##D   Breast <- DNAJC1$Breast
##D   Thyroid <- DNAJC1$Thyroid
##D   genename <- "DNAJC1"
##D   snpnames <- Breast$snp
##D   Betah <- list(Breast$beta, Thyroid$beta)
##D   Sigmah <- list(diag(Breast$se^2), diag(Thyroid$se^2))
##D   K <- 2
##D   m <- 14
##D 
##D   RES <- HS(Betah, Sigmah,
##D     kappa0 = 0.5, kappastar0 = 0.5, sigma20 = 1, s20 = 1,
##D     m = m, K = K, niter = 2000, burnin = 1000, nthin = 1, nchains = 1,
##D     a1 = 0.1, a2 = 0.1, d1 = 0.1, d2 = 0.1, c1 = 1, c2 = 1, e2 = 1, snpnames, genename
##D   )
##D   print(RES)
##D 
##D   RES1 <- HS(Betah, Sigmah,
##D     kappa0 = c(0.5, 0.3), kappastar0 = c(0.5, 0.3), sigma20 = c(2, 1), s20 = c(1, 2),
##D     m = m, K = K, niter = 2000, burnin = 1000, nthin = 2, nchains = 2,
##D     a1 = 0.1, a2 = 0.1, d1 = 0.1, d2 = 0.1, c1 = 1, c2 = 1, e2 = 1, snpnames, genename
##D   )
##D   print(RES1)
##D   ################### Simulated summary level data with K=5 ###############################
##D   data(Simulated_summary)
##D   genename <- Simulated_summary$genename
##D   snpnames <- Simulated_summary$snpnames
##D   Betah <- Simulated_summary$simBeta
##D   Sigmah <- Simulated_summary$simSIGMA
##D   K <- 5
##D   m <- 10
##D 
##D   RES <- HS(Betah, Sigmah,
##D     kappa0 = 0.5, kappastar0 = 0.5, sigma20 = 1, s20 = 1,
##D     m = m, K = K, niter = 2000, burnin = 1000, nthin = 2, nchains = 1,
##D     a1 = 0.1, a2 = 0.1, d1 = 0.1, d2 = 0.1, c1 = 1, c2 = 1, e2 = 1, snpnames, genename
##D   )
##D   print(RES)
##D 
##D   RES1 <- HS(Betah, Sigmah,
##D     kappa0 = c(0.5, 0.3), kappastar0 = c(0.5, 0.3), sigma20 = c(2, 1), s20 = c(1, 2),
##D     m = m, K = K, niter = 2000, burnin = 1000, nthin = 2, nchains = 2,
##D     a1 = 0.1, a2 = 0.1, d1 = 0.1, d2 = 0.1, c1 = 1, c2 = 1, e2 = 1, snpnames, genename
##D   )
##D   print(RES1)
##D   ################################### Gene PARP2 ##########################################
##D   library(BhGLM)
##D   data(PARP2)
##D   Breast <- PARP2$Breast
##D   Thyroid <- PARP2$Thyroid
##D   genename <- "PARP2"
##D   snpnames <- c("rs3093872", "rs3093921", "rs1713411", "rs3093926", "rs3093930", "rs878156")
##D 
##D 
##D   Fit1 <- BhGLM::bglm(y1 ~ ., family = binomial(link = "logit"), data = Breast)
##D   Betah1 <- Fit1$coefficients[-1]
##D   Sigmah1 <- cov(coef(arm::sim(Fit1)))[-1, -1]
##D 
##D 
##D   Fit2 <- BhGLM::bglm(y2 ~ ., family = binomial(link = "logit"), data = Thyroid)
##D   Betah2 <- Fit2$coefficients[-1]
##D   Sigmah2 <- cov(coef(arm::sim(Fit2)))[-1, -1]
##D 
##D   Betah <- list(Betah1, Betah2)
##D   Sigmah <- list(Sigmah1, Sigmah2)
##D   K <- 2
##D   m <- 6
##D 
##D 
##D   RES <- HS(Betah, Sigmah,
##D     kappa0 = 0.5, kappastar0 = 0.5, sigma20 = 1, s20 = 1,
##D     m = m, K = K, niter = 2000, burnin = 1000, nthin = 2, nchains = 1,
##D     a1 = 0.1, a2 = 0.1, d1 = 0.1, d2 = 0.1, c1 = 1, c2 = 1, e2 = 1, snpnames, genename
##D   )
##D   print(RES)
##D 
##D   RES1 <- HS(Betah, Sigmah,
##D     kappa0 = c(0.5, 0.3), kappastar0 = c(0.5, 0.3), sigma20 = c(2, 1), s20 = c(1, 2),
##D     m = m, K = K, niter = 2000, burnin = 1000, nthin = 2, nchains = 2,
##D     a1 = 0.1, a2 = 0.1, d1 = 0.1, d2 = 0.1, c1 = 1, c2 = 1, e2 = 1, snpnames, genename
##D   )
##D   print(RES1)
##D   ########### Simulated individual level data with K=3 and continuous phynotype ###########
##D   library(BhGLM)
##D   data(Simulated_individual)
##D   Study1 <- Simulated_individual$Study1
##D   Study2 <- Simulated_individual$Study2
##D   Study3 <- Simulated_individual$Study3
##D   K <- 3
##D   m <- 30
##D   genename <- "Simulated"
##D   snpnames <- sprintf("SNP%s", seq(1:m))
##D 
##D 
##D   Fit1 <- BhGLM::bglm(Y1 ~ ., family = gaussian, data = data.frame(Study1))
##D   Betah1 <- Fit1$coefficients[-1]
##D   Sigmah1 <- cov(coef(arm::sim(Fit1)))[-1, -1]
##D 
##D 
##D   Fit2 <- BhGLM::bglm(Y2 ~ ., family = gaussian, data = data.frame(Study2))
##D   Betah2 <- Fit2$coefficients[-1]
##D   Sigmah2 <- cov(coef(arm::sim(Fit2)))[-1, -1]
##D 
##D 
##D   Fit3 <- BhGLM::bglm(Y3 ~ ., family = gaussian, data = data.frame(Study3))
##D   Betah3 <- Fit3$coefficients[-1]
##D   Sigmah3 <- cov(coef(arm::sim(Fit3)))[-1, -1]
##D 
##D   Betah <- list(Betah1, Betah2, Betah3)
##D   Sigmah <- list(Sigmah1, Sigmah2, Sigmah3)
##D 
##D 
##D   RES <- HS(Betah, Sigmah,
##D     kappa0 = 0.5, kappastar0 = 0.5, sigma20 = 1, s20 = 1,
##D     m = m, K = K, niter = 2000, burnin = 1000, nthin = 2, nchains = 1,
##D     a1 = 0.1, a2 = 0.1, d1 = 0.1, d2 = 0.1, c1 = 1, c2 = 1, e2 = 1, snpnames, genename
##D   )
##D   print(RES)
##D 
##D   RES1 <- HS(Betah, Sigmah,
##D     kappa0 = c(0.5, 0.3), kappastar0 = c(0.5, 0.3), sigma20 = c(2, 1), s20 = c(1, 2),
##D     m = m, K = K, niter = 2000, burnin = 1000, nthin = 2, nchains = 2,
##D     a1 = 0.1, a2 = 0.1, d1 = 0.1, d2 = 0.1, c1 = 1, c2 = 1, e2 = 1, snpnames, genename
##D   )
##D   print(RES1)
##D 
##D   ########### Simulated individual level data with K=2 and gene expression data ###########
##D   library(BhGLM)
##D   data(Simulated_individual_survival)
##D   Study1 <- Simulated_individual_survival$Study1
##D   Study2 <- Simulated_individual_survival$Study2
##D   K <- 2
##D   m <- 10
##D   genename <- "Simulated"
##D   snpnames <- sprintf("G%s", seq(1:m))
##D 
##D 
##D   Fit1 <- BhGLM::bcoxph(Study1$T ~ Study1$X)
##D   Betah1 <- Fit1$coefficients
##D   Sigmah1 <- Fit1$var
##D 
##D 
##D   Fit2 <- BhGLM::bcoxph(Study2$T ~ Study2$X)
##D   Betah2 <- Fit2$coefficients
##D   Sigmah2 <- Fit2$var
##D 
##D   Betah <- list(Betah1, Betah2)
##D   Sigmah <- list(Sigmah1, Sigmah2)
##D 
##D   RES1 <- HS(Betah, Sigmah,
##D     kappa0 = c(0.5, 0.3), kappastar0 = c(0.5, 0.3), sigma20 = c(2, 1), s20 = c(1, 2),
##D     m = m, K = K, niter = 2000, burnin = 1000, nthin = 2, nchains = 2,
##D     a1 = 0.1, a2 = 0.1, d1 = 0.1, d2 = 0.1, c1 = 1, c2 = 1, e2 = 1, snpnames, genename
##D   )
##D   print(RES1)
## End(Not run)



cleanEx()
nameEx("MCMCplot")
### * MCMCplot

flush(stderr()); flush(stdout())

### Name: MCMCplot
### Title: MCMC plot
### Aliases: MCMCplot

### ** Examples

#############################Gene DNAJC1 ###############################################
data(DNAJC1)
Breast <- DNAJC1$Breast
Thyroid <- DNAJC1$Thyroid
genename <- "DNAJC1"
snpnames <- Breast$snp
Betah <- list(Breast$beta, Thyroid$beta)
Sigmah <- list(diag(Breast$se^2), diag(Thyroid$se^2))
K <- 2
m <- 14


RES1 <- DS(Betah, Sigmah,
           kappa0 = 0.5, sigma20 = 1,
           m = m, K = K, niter = 2000, burnin = 1000, nthin = 2, nchains = 1,
           a1 = 0.1, a2 = 0.1, d1 = 0.1, d2 = 0.1, snpnames, genename
)


MCMCplot(Result = RES1, k = 2, nchains = 1, whichsnps = sample(snpnames, 7),
                     betatype = "l",
                     acftype = "correlation",
                     dencol = "white", denlty = 1, denbg = "white")
###################Simulated summary level data with K=5 ###############################
## Not run: 
##D data(Simulated_summary)
##D genename <- Simulated_summary$genename
##D snpnames <- Simulated_summary$snpnames
##D Betah <- Simulated_summary$simBeta
##D Sigmah <- Simulated_summary$simSIGMA
##D K <- 5
##D m <- 10
##D 
##D RES1 <- DS(Betah, Sigmah,
##D  kappa0 = c(0.2, 0.5), sigma20 = c(1, 2),
##D  m = m, K = K, niter = 2000, burnin = 1000, nthin = 2, nchains = 2,
##D  a1 = 0.1, a2 = 0.1, d1 = 0.1, d2 = 0.1, snpnames, genename)
##D 
##D MCMCplot(Result = RES1, k = 3, nchains = 2, whichsnps = sample(snpnames, 3),
##D          betatype = "l",
##D          acftype = "partial",
##D          dencol = "blue", denlty = 1, denbg = "black")
##D 
##D RES1 <- DS(Betah, Sigmah,
##D  kappa0 = c(0.2, 0.5, 0.6), sigma20 = c(1, 2, 1.5),
##D  m = m, K = K, niter = 2000, burnin = 1000, nthin = 2, nchains = 3,
##D  a1 = 0.1, a2 = 0.1, d1 = 0.1, d2 = 0.1, snpnames, genename)
##D 
##D MCMCplot(Result = RES1, k = 3, nchains = 3, whichsnps = sample(snpnames, 5),
##D          betatype = "l",
##D          acftype = "partial",
##D          dencol = "white", denlty = 1, denbg = "white")
##D #############################Gene DNAJC1 ###############################################
##D pvalue <- matrix(0, K, m)
##D for (k in 1:K) {
##D   pvalue[k, ] <- 2 * pnorm(-abs(Betah[[k]] / sqrt(diag(Sigmah[[k]]))))
##D }
##D 
##D zinit <- rep(0, K)
##D for (j in 1:K) {
##D   index <- 1:m
##D   PVALUE <- p.adjust(pvalue[j, ])
##D  SIGNALS <- index[PVALUE < 0.05]
##D  modelf1 <- rep(0, m)
##D  modelf1[SIGNALS] <- 1
##D   if (max(modelf1) == 1) (zinit[j] <- 1)
##D }
##D 
##D RES <- CS(Betah, Sigmah,
##D   kappa0 = 0.5, tau20 = 1, zeta0 = zinit,
##D  m = m, K = K, niter = 2000, burnin = 1000, nthin = 2, nchains = 1, a1 = 0.1, a2 = 0.1,
##D c1 = 0.1, c2 = 0.1, sigma2 = 10^-3, snpnames = snpnames, genename = genename)
##D 
##D MCMCplot(Result = RES1, k = 1, nchains = 1, whichsnps = sample(snpnames, 7),
##D          betatype = "l",
##D          acftype = "correlation",
##D          dencol = "white", denlty = 1, denbg = "white")
##D ###################################Gene PARP2 ##########################################
##D library(BhGLM)
##D data(PARP2)
##D Breast <- PARP2$Breast
##D Thyroid <- PARP2$Thyroid
##D genename <- "PARP2"
##D snpnames <- c("rs3093872", "rs3093921", "rs1713411", "rs3093926", "rs3093930", "rs878156")
##D 
##D 
##D Fit1 <- BhGLM::bglm(y1~ ., family=binomial(link="logit"),data=Breast)
##D Betah1 <-  Fit1$coefficients[-1]
##D Sigmah1 <- cov(coef(arm::sim(Fit1)))[-1,-1]
##D 
##D Fit2 <- BhGLM::bglm(y2~ ., family=binomial(link="logit"),data=Thyroid)
##D Betah2 <-  Fit2$coefficients[-1]
##D Sigmah2 <- cov(coef(arm::sim(Fit2)))[-1,-1]
##D 
##D Betah <- list(Betah1,Betah2)
##D Sigmah <- list(Sigmah1,Sigmah2)
##D K <- 2
##D m <- 6
##D 
##D 
##D RES1 <- DS(Betah, Sigmah, kappa0=c(0.2,0.5), sigma20=c(1,2),
##D           m=m, K=K, niter=1000, burnin=500, nthin=1, nchains=2,
##D           a1=0.1, a2=0.1, d1=0.1, d2=0.1, snpnames, genename)
##D 
##D MCMCplot(Result=RES1, k=1, nchains=2, whichsnps=snpnames,
##D          betatype = "l",
##D          acftype = "correlation",
##D          dencol = "red", denlty = 1, denbg = "white")
##D 
##D 
##D RES1 <- DS(Betah, Sigmah, kappa0=c(0.2,0.5), sigma20=c(1,2),
##D           m=m, K=K, niter=2000, burnin=1000, nthin=2, nchains=2,
##D           a1=0.1, a2=0.1, d1=0.1, d2=0.1, snpnames, genename)
##D 
##D MCMCplot(Result=RES1, k=1, nchains=2, whichsnps=snpnames,
##D          betatype = "l",
##D          acftype = "correlation",
##D          dencol = "white", denlty = 1, denbg = "white")
## End(Not run)




cleanEx()
nameEx("summaryCS")
### * summaryCS

flush(stderr()); flush(stdout())

### Name: summaryCS
### Title: Summary function of Continuous Spike
### Aliases: summaryCS

### ** Examples

############################# Gene DNAJC1 ###############################################
data(DNAJC1)
Breast <- DNAJC1$Breast
Thyroid <- DNAJC1$Thyroid
genename <- "DNAJC1"
snpnames <- Breast$snp
Betah <- list(Breast$beta, Thyroid$beta)
Sigmah <- list(diag(Breast$se^2), diag(Thyroid$se^2))
K <- 2
m <- 14

pvalue <- matrix(0, K, m)
for (k in 1:K) {
  pvalue[k, ] <- 2 * pnorm(-abs(Betah[[k]] / sqrt(diag(Sigmah[[k]]))))
}

zinit <- rep(0, K)
for (j in 1:K) {
  index <- 1:m
  PVALUE <- p.adjust(pvalue[j, ])
  SIGNALS <- index[PVALUE < 0.05]
  modelf1 <- rep(0, m)
  modelf1[SIGNALS] <- 1
  if (max(modelf1) == 1) (zinit[j] <- 1)
}


RES <- CS(Betah, Sigmah,
  kappa0 = 0.5, tau20 = 1, zeta0 = zinit,
  m = m, K = K, niter = 2000, burnin = 1000, nthin = 2, nchains = 1, a1 = 0.1, a2 = 0.1,
  c1 = 0.1, c2 = 0.1, sigma2 = 10^-3, snpnames = snpnames, genename = genename
)
summaryCS(RES)



cleanEx()
nameEx("summaryDS")
### * summaryDS

flush(stderr()); flush(stdout())

### Name: summaryDS
### Title: Summary function of Dirac Spike
### Aliases: summaryDS

### ** Examples

############################# Gene DNAJC1 ###############################################
data(DNAJC1)
Breast <- DNAJC1$Breast
Thyroid <- DNAJC1$Thyroid
genename <- "DNAJC1"
snpnames <- Breast$snp
Betah <- list(Breast$beta, Thyroid$beta)
Sigmah <- list(diag(Breast$se^2), diag(Thyroid$se^2))
K <- 2
m <- 14


RES <- DS(Betah, Sigmah,
  kappa0 = 0.5, sigma20 = 1,
  m = m, K = K, niter = 2000, burnin = 1000, nthin = 2, nchains = 1,
  a1 = 0.1, a2 = 0.1, d1 = 0.1, d2 = 0.1, snpnames, genename
)
summaryDS(RES)



cleanEx()
nameEx("summaryHS")
### * summaryHS

flush(stderr()); flush(stdout())

### Name: summaryHS
### Title: Summary function of Hierarchical Spike
### Aliases: summaryHS

### ** Examples

############################# PARP2_summary ###############################################
data(PARP2_summary)
Breast <- PARP2_summary$Breast
Thyroid <- PARP2_summary$Thyroid
Betah <- list(Breast$beta, Thyroid$beta)
Sigmah <- list(diag(Breast$se), diag(Thyroid$se))
genename <- "PARP2"
snpnames <- Breast$snp
K <- 2
m <- 6


RES <- HS(Betah, Sigmah,
  kappa0 = 0.5, kappastar0 = 0.5, sigma20 = 1, s20 = 1,
  m = m, K = K, niter = 1000, burnin = 500, nthin = 1, nchains = 1,
  a1 = 0.1, a2 = 0.1, d1 = 0.1, d2 = 0.1, c1 = 1, c2 = 1, e2 = 1, snpnames, genename
)
summaryHS(RES)



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

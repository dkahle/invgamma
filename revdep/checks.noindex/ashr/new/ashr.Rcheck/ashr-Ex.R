pkgname <- "ashr"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('ashr')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("ash")
### * ash

flush(stderr()); flush(stdout())

### Name: ash
### Title: Adaptive Shrinkage
### Aliases: ash ash.workhorse

### ** Examples


beta = c(rep(0,100),rnorm(100))
sebetahat = abs(rnorm(200,0,1))
betahat = rnorm(200,beta,sebetahat)
beta.ash = ash(betahat, sebetahat)
names(beta.ash)
head(beta.ash$result) # the main dataframe of results
head(get_pm(beta.ash)) # get_pm returns posterior mean
head(get_lfsr(beta.ash)) # get_lfsr returns the local false sign rate
graphics::plot(betahat,get_pm(beta.ash),xlim=c(-4,4),ylim=c(-4,4))

## Not run: 
##D # Why is this example included here? -Peter
##D CIMatrix=ashci(beta.ash,level=0.95)
##D print(CIMatrix)
## End(Not run)

# Illustrating the non-zero mode feature.
betahat=betahat+5
beta.ash = ash(betahat, sebetahat)
graphics::plot(betahat,get_pm(beta.ash))
betan.ash=ash(betahat, sebetahat,mode=5)
graphics::plot(betahat,get_pm(betan.ash))
summary(betan.ash)

# Running ash with different error models
beta.ash1 = ash(betahat, sebetahat, lik = lik_normal())
beta.ash2 = ash(betahat, sebetahat, lik = lik_t(df=4))

e = rnorm(100)+log(rf(100,df1=10,df2=10)) # simulated data with log(F) error
e.ash = ash(e,1,lik=lik_logF(df1=10,df2=10))

# Specifying the output
beta.ash = ash(betahat, sebetahat, output = c("fitted_g","logLR","lfsr"))

#Running ash with a pre-specified g, rather than estimating it
beta = c(rep(0,100),rnorm(100))
sebetahat = abs(rnorm(200,0,1))
betahat = rnorm(200,beta,sebetahat)
true_g = normalmix(c(0.5,0.5),c(0,0),c(0,1)) # define true g
## Passing this g into ash causes it to i) take the sd and the means
## for each component from this g, and ii) initialize pi to the value
## from this g.
beta.ash = ash(betahat, sebetahat,g=true_g,fixg=TRUE)

# running with weights
beta.ash = ash(betahat, sebetahat, optmethod="w_mixEM",
               weights = c(rep(0.5,100),rep(1,100)))

# Different algorithms can be used to compute maximum-likelihood
# estimates of the mixture weights. Here, we illustrate use of the
# EM algorithm and the (default) SQP algorithm.
set.seed(1)
betahat  <- c(8.115,9.027,9.289,10.097,9.463)
sebeta   <- c(0.6157,0.4129,0.3197,0.3920,0.5496)
fit.em   <- ash(betahat,sebeta,mixcompdist = "normal",optmethod = "mixEM")
fit.sqp  <- ash(betahat,sebeta,mixcompdist = "normal",optmethod = "mixSQP")
range(fit.em$fitted$pi - fit.sqp$fitted$pi)



cleanEx()
nameEx("ash_pois")
### * ash_pois

flush(stderr()); flush(stdout())

### Name: ash_pois
### Title: Performs adaptive shrinkage on Poisson data
### Aliases: ash_pois

### ** Examples

   beta = c(rep(0,50),rexp(50))
   y = rpois(100,beta) # simulate Poisson observations
   y.ash = ash_pois(y,scale=1)



cleanEx()
nameEx("ashci")
### * ashci

flush(stderr()); flush(stdout())

### Name: ashci
### Title: Credible Interval Computation for the ash object
### Aliases: ashci

### ** Examples

beta = c(rep(0,20),rnorm(20))
sebetahat = abs(rnorm(40,0,1))
betahat = rnorm(40,beta,sebetahat)
beta.ash = ash(betahat, sebetahat)

CImatrix=ashci(beta.ash,level=0.95)

CImatrix1=ashci(beta.ash,level=0.95,betaindex=c(1,2,5))
CImatrix2=ashci(beta.ash,level=0.95,lfsr_threshold=0.1)



cleanEx()
nameEx("cdf_post")
### * cdf_post

flush(stderr()); flush(stdout())

### Name: cdf_post
### Title: cdf_post
### Aliases: cdf_post

### ** Examples

beta = rnorm(100,0,1)
betahat= beta+rnorm(100,0,1)
sebetahat=rep(1,100)
ash.beta = ash(betahat,1,mixcompdist="normal")
cdf0 = cdf_post(ash.beta$fitted_g,0,set_data(betahat,sebetahat))
graphics::plot(cdf0,1-get_pp(ash.beta))



cleanEx()
nameEx("comp_cdf_post")
### * comp_cdf_post

flush(stderr()); flush(stdout())

### Name: comp_cdf_post
### Title: comp_cdf_post
### Aliases: comp_cdf_post

### ** Examples

beta = rnorm(100,0,1)
betahat= beta+rnorm(100,0,1)
sebetahat=rep(1,100)
ash.beta = ash(betahat,1,mixcompdist="normal")
comp_cdf_post(get_fitted_g(ash.beta),0,data=set_data(beta,sebetahat))



cleanEx()
nameEx("comp_postsd")
### * comp_postsd

flush(stderr()); flush(stdout())

### Name: comp_postsd
### Title: comp_postsd
### Aliases: comp_postsd

### ** Examples

beta = rnorm(100,0,1)
betahat= beta+rnorm(100,0,1)
ash.beta = ash(betahat,1,mixcompdist="normal")
data= set_data(betahat,rep(1,100))
comp_postmean(get_fitted_g(ash.beta),data)
comp_postsd(get_fitted_g(ash.beta),data)
comp_postprob(get_fitted_g(ash.beta),data)



cleanEx()
nameEx("get_post_sample")
### * get_post_sample

flush(stderr()); flush(stdout())

### Name: get_post_sample
### Title: Sample from posterior
### Aliases: get_post_sample

### ** Examples

beta = rnorm(100,0,1)
betahat= beta+rnorm(100,0,1)
ash.beta = ash(betahat,1,mixcompdist="normal")
post.beta = get_post_sample(ash.beta,1000)



cleanEx()
nameEx("igmix")
### * igmix

flush(stderr()); flush(stdout())

### Name: igmix
### Title: Constructor for igmix class
### Aliases: igmix

### ** Examples

igmix(c(0.5,0.5),c(1,1),c(1,2))




cleanEx()
nameEx("lik_binom")
### * lik_binom

flush(stderr()); flush(stdout())

### Name: lik_binom
### Title: Likelihood object for Binomial error distribution
### Aliases: lik_binom

### ** Examples

   p = rbeta(100,2,2) # prior mode: 0.5
   n = rpois(100,10)
   y = rbinom(100,n,p) # simulate Binomial observations
   ash(rep(0,length(y)),1,lik=lik_binom(y,n))



cleanEx()
nameEx("lik_logF")
### * lik_logF

flush(stderr()); flush(stdout())

### Name: lik_logF
### Title: Likelihood object for logF error distribution
### Aliases: lik_logF

### ** Examples

   e = rnorm(100) + log(rf(100,df1=10,df2=10)) # simulate some data with log(F) error
   ash(e,1,lik=lik_logF(df1=10,df2=10))



cleanEx()
nameEx("lik_normal")
### * lik_normal

flush(stderr()); flush(stdout())

### Name: lik_normal
### Title: Likelihood object for normal error distribution
### Aliases: lik_normal

### ** Examples

   z = rnorm(100) + rnorm(100) # simulate some data with normal error
   ash(z,1,lik=lik_normal())



cleanEx()
nameEx("lik_normalmix")
### * lik_normalmix

flush(stderr()); flush(stdout())

### Name: lik_normalmix
### Title: Likelihood object for normal mixture error distribution
### Aliases: lik_normalmix

### ** Examples

   e = rnorm(100,0,0.8) 
   e[seq(1,100,by=2)] = rnorm(50,0,1.5) # generate e~0.5*N(0,0.8^2)+0.5*N(0,1.5^2)
   betahat = rnorm(100)+e
   ash(betahat, 1, lik=lik_normalmix(c(0.5,0.5),c(0.8,1.5)))



cleanEx()
nameEx("lik_pois")
### * lik_pois

flush(stderr()); flush(stdout())

### Name: lik_pois
### Title: Likelihood object for Poisson error distribution
### Aliases: lik_pois

### ** Examples

   beta = c(rnorm(100,50,5)) # prior mode: 50
   y = rpois(100,beta) # simulate Poisson observations
   ash(rep(0,length(y)),1,lik=lik_pois(y))




cleanEx()
nameEx("lik_t")
### * lik_t

flush(stderr()); flush(stdout())

### Name: lik_t
### Title: Likelihood object for t error distribution
### Aliases: lik_t

### ** Examples

   z = rnorm(100) + rt(100,df=4) # simulate some data with t error
   ash(z,1,lik=lik_t(df=4))



cleanEx()
nameEx("mixcdf")
### * mixcdf

flush(stderr()); flush(stdout())

### Name: mixcdf
### Title: mixcdf
### Aliases: mixcdf

### ** Examples

mixcdf(normalmix(c(0.5,0.5),c(0,0),c(1,2)),seq(-4,4,length=100))




cleanEx()
nameEx("normalmix")
### * normalmix

flush(stderr()); flush(stdout())

### Name: normalmix
### Title: Constructor for normalmix class
### Aliases: normalmix

### ** Examples

normalmix(c(0.5,0.5),c(0,0),c(1,2))




cleanEx()
nameEx("pcdf_post")
### * pcdf_post

flush(stderr()); flush(stdout())

### Name: pcdf_post
### Title: pcdf_post
### Aliases: pcdf_post

### ** Examples

beta = rnorm(100,0,1)
betahat= beta+rnorm(100,0,1)
sebetahat=rep(1,100)
ash.beta = ash(betahat,1,mixcompdist="normal")
c = pcdf_post(get_fitted_g(ash.beta),beta,set_data(betahat,sebetahat))



cleanEx()
nameEx("tnormalmix")
### * tnormalmix

flush(stderr()); flush(stdout())

### Name: tnormalmix
### Title: Constructor for tnormalmix class
### Aliases: tnormalmix

### ** Examples

tnormalmix(c(0.5,0.5),c(0,0),c(1,2),c(-10,0),c(0,10))




cleanEx()
nameEx("unimix")
### * unimix

flush(stderr()); flush(stdout())

### Name: unimix
### Title: Constructor for unimix class
### Aliases: unimix

### ** Examples

unimix(c(0.5,0.5),c(0,0),c(1,2))



cleanEx()
nameEx("vcdf_post")
### * vcdf_post

flush(stderr()); flush(stdout())

### Name: vcdf_post
### Title: vcdf_post
### Aliases: vcdf_post

### ** Examples

beta = rnorm(100,0,1)
betahat= beta+rnorm(100,0,1)
sebetahat=rep(1,100)
ash.beta = ash(betahat,1,mixcompdist="normal")
c = vcdf_post(get_fitted_g(ash.beta),seq(-5,5,length=1000),data = set_data(betahat,sebetahat))



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

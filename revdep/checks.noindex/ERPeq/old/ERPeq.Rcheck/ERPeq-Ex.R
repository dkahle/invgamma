pkgname <- "ERPeq"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('ERPeq')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("cdfbsgdp")
### * cdfbsgdp

flush(stderr()); flush(stdout())

### Name: cdfbsgdp
### Title: Cumulative distribution function of the
###   Birnbaum-Saunders-Generalized Pareto distribution
### Aliases: cdfbsgdp

### ** Examples

cdfbsgdp(c(0.5,2,0.5),3)



cleanEx()
nameEx("cdfeexp")
### * cdfeexp

flush(stderr()); flush(stdout())

### Name: cdfeexp
### Title: Cumulative distribution function of the exponentiated
###   exponential distribution
### Aliases: cdfeexp

### ** Examples

cdfeexp(c(0.5,0.3),2)



cleanEx()
nameEx("cdfer")
### * cdfer

flush(stderr()); flush(stdout())

### Name: cdfer
### Title: Cumulative distribution function of the exponentiated Rayleigh
###   distribution
### Aliases: cdfer

### ** Examples

cdfer(c(0.5,0.3),2)



cleanEx()
nameEx("cdfew")
### * cdfew

flush(stderr()); flush(stdout())

### Name: cdfew
### Title: Cumulative distribution function of the exponentiated Weibull
###   distribution
### Aliases: cdfew

### ** Examples

cdfew(c(0.5,0.3,0.6),2)



cleanEx()
nameEx("cdfgamma")
### * cdfgamma

flush(stderr()); flush(stdout())

### Name: cdfgamma
### Title: Cumulative distribution function of the Gamma distribution
### Aliases: cdfgamma

### ** Examples

cdfgamma(c(2,3),5)



cleanEx()
nameEx("cdfggamma")
### * cdfggamma

flush(stderr()); flush(stdout())

### Name: cdfggamma
### Title: Cumulative distribution function of the generalized gamma
###   distribution
### Aliases: cdfggamma

### ** Examples

pdfggamma(c(2,5,3),3)




cleanEx()
nameEx("cdfgumbel")
### * cdfgumbel

flush(stderr()); flush(stdout())

### Name: cdfgumbel
### Title: Cumulative distribution function of the gumbel distribution
### Aliases: cdfgumbel

### ** Examples

pdfgumbel(c(0.5,0.3),2)



cleanEx()
nameEx("cdfinvgamma")
### * cdfinvgamma

flush(stderr()); flush(stdout())

### Name: cdfinvgamma
### Title: Cumulative distribution function of the inverse gamma
###   distribution
### Aliases: cdfinvgamma

### ** Examples

cdfinvgamma(c(2,5,3),3)



cleanEx()
nameEx("cdfiwweibull")
### * cdfiwweibull

flush(stderr()); flush(stdout())

### Name: cdfiwweibull
### Title: Cumulative distribution function of the inverse Weibull
###   distribution
### Aliases: cdfiwweibull

### ** Examples

cdfiwweibull(c(2,3),5)



cleanEx()
nameEx("cdflevy")
### * cdflevy

flush(stderr()); flush(stdout())

### Name: cdflevy
### Title: Cumulative distribution function of the Levy distribution
### Aliases: cdflevy

### ** Examples

cdflevy(c(0.5,0.3),2)



cleanEx()
nameEx("cdflnormal")
### * cdflnormal

flush(stderr()); flush(stdout())

### Name: cdflnormal
### Title: Cumulative distribution function of the log-normal distribution
### Aliases: cdflnormal

### ** Examples

cdflnormal(c(2,3),5)



cleanEx()
nameEx("cdfpareto")
### * cdfpareto

flush(stderr()); flush(stdout())

### Name: cdfpareto
### Title: Cumulative distribution function of the Pareto distribution
### Aliases: cdfpareto

### ** Examples

cdfpareto(c(2,5),2)



cleanEx()
nameEx("cdfrayleigh")
### * cdfrayleigh

flush(stderr()); flush(stdout())

### Name: cdfrayleigh
### Title: Cumulative distribution function of the Rayleigh distribution
### Aliases: cdfrayleigh

### ** Examples

cdfrayleigh(c(2),5)



cleanEx()
nameEx("cdfweibull")
### * cdfweibull

flush(stderr()); flush(stdout())

### Name: cdfweibull
### Title: Cumulative distribution function of the Weibull distribution
### Aliases: cdfweibull

### ** Examples

cdfweibull(c(2,3),5)



cleanEx()
nameEx("expexpcp")
### * expexpcp

flush(stderr()); flush(stdout())

### Name: expexpcp
### Title: Probabilistic estimation of earthquake recurrence interval using
###   exponentiated exponential distribution
### Aliases: expexpcp

### ** Examples

fit=fitexpexp(c(1,1),data=data_earthquake_7)
expexpcp(fit,r=2,te=5)



cleanEx()
nameEx("expraycp")
### * expraycp

flush(stderr()); flush(stdout())

### Name: expraycp
### Title: Probabilistic estimation of earthquake recurrence interval using
###   exponentiated Rayleigh distribution
### Aliases: expraycp

### ** Examples

fit=fitexprayleigh(c(0.5,0.5),data=data_earthquake_7)
expraycp(fit,r=2,te=5)



cleanEx()
nameEx("expweicp")
### * expweicp

flush(stderr()); flush(stdout())

### Name: expweicp
### Title: Probabilistic estimation of earthquake recurrence interval using
###   exponentiated Weibull distribution
### Aliases: expweicp

### ** Examples

fit=fitexpweibull(c(1,1,1),data=data_earthquake_7)
expweicp(fit,r=2,te=5)



cleanEx()
nameEx("fitbsgpd")
### * fitbsgpd

flush(stderr()); flush(stdout())

### Name: fitbsgpd
### Title: Fitting the Birnbaum-Saunders-Generalized Pareto distribution
### Aliases: fitbsgpd

### ** Examples

library(VGAM)
data=ERPeq::rbsgpd(500,5,0.7,0.2)
fitbsgpd(starts =c(1,1),data=data)



cleanEx()
nameEx("fitexpexp")
### * fitexpexp

flush(stderr()); flush(stdout())

### Name: fitexpexp
### Title: Fitting the exponentiated exponential distribution
### Aliases: fitexpexp

### ** Examples

data=rexpexp(500,2,3)
fitexpexp(starts =c(2,2),data=data)



cleanEx()
nameEx("fitexprayleigh")
### * fitexprayleigh

flush(stderr()); flush(stdout())

### Name: fitexprayleigh
### Title: Fitting the exponentiated exponentiated Rayleigh distribution
### Aliases: fitexprayleigh

### ** Examples

data=rexprayleigh(500,2,3)
fitexprayleigh(starts =c(2,2),data=data)



cleanEx()
nameEx("fitexpweibull")
### * fitexpweibull

flush(stderr()); flush(stdout())

### Name: fitexpweibull
### Title: Fitting the exponentiated Weibull distribution
### Aliases: fitexpweibull

### ** Examples

data=rexpweibull(500,2,3,5)
fitexpweibull(starts =c(2,2,2),data=data)



cleanEx()
nameEx("fitgamma")
### * fitgamma

flush(stderr()); flush(stdout())

### Name: fitgamma
### Title: Fitting the gamma distribution
### Aliases: fitgamma

### ** Examples

datagamma=rgamma(500,2,2)
fitgamma(starts =c(2,2),data=datagamma)



cleanEx()
nameEx("fitggamma")
### * fitggamma

flush(stderr()); flush(stdout())

### Name: fitggamma
### Title: Fitting the generalized gamma distribution
### Aliases: fitggamma

### ** Examples

library(rmutil)
data=rggamma(500,2,2,2)
fitggamma(starts =c(1,1,1),data=data)



cleanEx()
nameEx("fitgumbel")
### * fitgumbel

flush(stderr()); flush(stdout())

### Name: fitgumbel
### Title: Fitting the Gumbel distribution
### Aliases: fitgumbel

### ** Examples

library(VGAM)
data=rgumbel(500,2,0.5)
fitgumbel(starts =c(2,2),data=data)



cleanEx()
nameEx("fitinvgamma")
### * fitinvgamma

flush(stderr()); flush(stdout())

### Name: fitinvgamma
### Title: Fitting the inverse gamma distribution
### Aliases: fitinvgamma

### ** Examples

library(invgamma)
data=rinvgamma(500,2,0.5)
fitinvgamma(starts =c(2,2),data=data)



cleanEx()
nameEx("fitiweibull")
### * fitiweibull

flush(stderr()); flush(stdout())

### Name: fitiweibull
### Title: Fitting the gamma distribution
### Aliases: fitiweibull

### ** Examples

set.seed(7)
data=rgamma(500,shape=1,scale=1)
fitiweibull(starts =c(0.5,0.5),data=data)



cleanEx()
nameEx("fitlevy")
### * fitlevy

flush(stderr()); flush(stdout())

### Name: fitlevy
### Title: Fitting the Levy distribution
### Aliases: fitlevy

### ** Examples

library(VGAM)
data=ERPeq::rlevy(100,2,0.1)
fitlevy(starts =c(0.1),data=data)



cleanEx()
nameEx("fitlnormal")
### * fitlnormal

flush(stderr()); flush(stdout())

### Name: fitlnormal
### Title: Fitting the log-normal distribution
### Aliases: fitlnormal

### ** Examples

data=rlnorm(500,2,0.5)
fitlnormal(starts =c(2,2),data=data)



cleanEx()
nameEx("fitpareto")
### * fitpareto

flush(stderr()); flush(stdout())

### Name: fitpareto
### Title: Fitting the Pareto distribution
### Aliases: fitpareto

### ** Examples

library(VGAM)
data=VGAM::rpareto(500,5,2)
fitpareto(starts =c(2),data=data)



cleanEx()
nameEx("fitrayleigh")
### * fitrayleigh

flush(stderr()); flush(stdout())

### Name: fitrayleigh
### Title: Fitting the Rayleigh distribution
### Aliases: fitrayleigh

### ** Examples

library(VGAM)
data=rrayleigh(500,2)
fitrayleigh(starts =c(2),data=data)



cleanEx()
nameEx("fitweibull")
### * fitweibull

flush(stderr()); flush(stdout())

### Name: fitweibull
### Title: Fitting the Weibull distribution
### Aliases: fitweibull

### ** Examples

dataweibull=rweibull(500,2,2)
fitweibull(starts =c(2,2),data=dataweibull)



cleanEx()
nameEx("gammacp")
### * gammacp

flush(stderr()); flush(stdout())

### Name: gammacp
### Title: Probabilistic estimation of earthquake recurrence interval using
###   gamma distribution
### Aliases: gammacp

### ** Examples

fit=fitgamma(c(1,1),data=data_earthquake_6_6.5)
gammacp(fit,r=2,te=5)



cleanEx()
nameEx("ggammacp")
### * ggammacp

flush(stderr()); flush(stdout())

### Name: ggammacp
### Title: Probabilistic estimation of earthquake recurrence interval using
###   generalized gamma distribution
### Aliases: ggammacp

### ** Examples

fit=fitggamma(c(1,1,1),data=data_earthquake_6_6.5)
ggammacp(fit,r=2,te=5)



cleanEx()
nameEx("gumbelcp")
### * gumbelcp

flush(stderr()); flush(stdout())

### Name: gumbelcp
### Title: Probabilistic estimation of earthquake recurrence interval using
###   Gumbel distribution
### Aliases: gumbelcp

### ** Examples

fit=fitgumbel(c(1,1),data=data_earthquake_7)
gumbelcp(fit,r=2,te=5)



cleanEx()
nameEx("invgammacp")
### * invgammacp

flush(stderr()); flush(stdout())

### Name: invgammacp
### Title: Probabilistic estimation of earthquake recurrence interval using
###   inverse gamma distribution
### Aliases: invgammacp

### ** Examples

fit=fitinvgamma(c(1,1),data=data_earthquake_7)
invgammacp(fit,r=2,te=5)



cleanEx()
nameEx("iweibullcp")
### * iweibullcp

flush(stderr()); flush(stdout())

### Name: iweibullcp
### Title: Probabilistic estimation of earthquake recurrence interval using
###   inverse Weibull distribution
### Aliases: iweibullcp

### ** Examples

fit=fitiweibull(c(1,1),data=data_earthquake_6.5_7)
iweibullcp(fit,r=2,te=5)



cleanEx()
nameEx("levycp")
### * levycp

flush(stderr()); flush(stdout())

### Name: levycp
### Title: Probabilistic estimation of earthquake recurrence interval using
###   Levy distribution
### Aliases: levycp

### ** Examples

fit=fitlevy(c(1),data=data_earthquake_7)
levycp(fit,r=2,te=5)



cleanEx()
nameEx("lnormalcp")
### * lnormalcp

flush(stderr()); flush(stdout())

### Name: lnormalcp
### Title: Probabilistic estimation of earthquake recurrence interval using
###   log-normal distribution
### Aliases: lnormalcp

### ** Examples

fit=fitlnormal(c(1,1),data=data_earthquake_6.5_7)
lnormalcp(fit,r=2,te=5)



cleanEx()
nameEx("paretocp")
### * paretocp

flush(stderr()); flush(stdout())

### Name: paretocp
### Title: Probabilistic estimation of earthquake recurrence interval using
###   Pareto distribution
### Aliases: paretocp

### ** Examples

library(VGAM)
data=VGAM::rpareto(200,2,5)
fit=fitpareto(c(0.5),data=data)
paretocp(fit,r=2,te=5)



cleanEx()
nameEx("pdfbsgdp")
### * pdfbsgdp

flush(stderr()); flush(stdout())

### Name: pdfbsgdp
### Title: Probability density function of the
###   Birnbaum-Saunders-Generalized Pareto distribution
### Aliases: pdfbsgdp

### ** Examples

pdfbsgdp(c(2,0.5,0.5),1)



cleanEx()
nameEx("pdfeexp")
### * pdfeexp

flush(stderr()); flush(stdout())

### Name: pdfeexp
### Title: Probability density function of the exponentiated exponential
###   distribution
### Aliases: pdfeexp

### ** Examples

pdfeexp(c(0.5,0.3),2)



cleanEx()
nameEx("pdfer")
### * pdfer

flush(stderr()); flush(stdout())

### Name: pdfer
### Title: Probability density function of the exponentiated Rayleigh
###   distribution
### Aliases: pdfer

### ** Examples

pdfer(c(0.5,0.3),2)



cleanEx()
nameEx("pdfew")
### * pdfew

flush(stderr()); flush(stdout())

### Name: pdfew
### Title: Probability density function of the exponentiated Weibull
###   distribution
### Aliases: pdfew

### ** Examples

pdfew(c(0.5,0.3,0.6),2)



cleanEx()
nameEx("pdfgamma")
### * pdfgamma

flush(stderr()); flush(stdout())

### Name: pdfgamma
### Title: Probability density function of the Gamma distribution
### Aliases: pdfgamma

### ** Examples

pdfgamma(c(2,3),5)



cleanEx()
nameEx("pdfggamma")
### * pdfggamma

flush(stderr()); flush(stdout())

### Name: pdfggamma
### Title: Probability density function of the generalized gamma
###   distribution
### Aliases: pdfggamma

### ** Examples

pdfggamma(c(2,5,3),3)



cleanEx()
nameEx("pdfgumbel")
### * pdfgumbel

flush(stderr()); flush(stdout())

### Name: pdfgumbel
### Title: Probability density function of the gumbel distribution
### Aliases: pdfgumbel

### ** Examples

pdfgumbel(c(0.5,0.3),2)



cleanEx()
nameEx("pdfinvgamma")
### * pdfinvgamma

flush(stderr()); flush(stdout())

### Name: pdfinvgamma
### Title: Probability density function of the inverse gamma distribution
### Aliases: pdfinvgamma

### ** Examples

pdfinvgamma(c(2,5,3),3)



cleanEx()
nameEx("pdfiweibull")
### * pdfiweibull

flush(stderr()); flush(stdout())

### Name: pdfiweibull
### Title: Probability density function of the inverse Weibull distribution
### Aliases: pdfiweibull

### ** Examples

pdfiweibull(c(2,3),5)



cleanEx()
nameEx("pdflevy")
### * pdflevy

flush(stderr()); flush(stdout())

### Name: pdflevy
### Title: Probability density function of the Levy distribution
### Aliases: pdflevy

### ** Examples

pdflevy(c(0.5,0.3),2)



cleanEx()
nameEx("pdflnormal")
### * pdflnormal

flush(stderr()); flush(stdout())

### Name: pdflnormal
### Title: Probability density function of the log-normal distribution
### Aliases: pdflnormal

### ** Examples

pdflnormal(c(2,3),5)



cleanEx()
nameEx("pdfpareto")
### * pdfpareto

flush(stderr()); flush(stdout())

### Name: pdfpareto
### Title: Probability density function of the Pareto distribution
### Aliases: pdfpareto

### ** Examples

pdfpareto(c(2,5),3)



cleanEx()
nameEx("pdfrayleigh")
### * pdfrayleigh

flush(stderr()); flush(stdout())

### Name: pdfrayleigh
### Title: Probability density function of the Rayleigh distribution
### Aliases: pdfrayleigh

### ** Examples

pdfrayleigh(c(2),5)



cleanEx()
nameEx("pdfweibull")
### * pdfweibull

flush(stderr()); flush(stdout())

### Name: pdfweibull
### Title: Probability density function of the Weibull distribution
### Aliases: pdfweibull

### ** Examples

pdfweibull(c(2,3),5)



cleanEx()
nameEx("rayleighcp")
### * rayleighcp

flush(stderr()); flush(stdout())

### Name: rayleighcp
### Title: Probabilistic estimation of earthquake recurrence interval using
###   Rayleigh distribution
### Aliases: rayleighcp

### ** Examples

fit=fitrayleigh(c(1),data=data_earthquake_7)
rayleighcp(fit,r=2,te=5)



cleanEx()
nameEx("rbsgpd")
### * rbsgpd

flush(stderr()); flush(stdout())

### Name: rbsgpd
### Title: Generate random observations from Birnbaum-Saunders-Generalized
###   Pareto distribution
### Aliases: rbsgpd

### ** Examples

rbsgpd(100,2,3,5)



cleanEx()
nameEx("rexpexp")
### * rexpexp

flush(stderr()); flush(stdout())

### Name: rexpexp
### Title: Generate random observations from exponentiated exponential
###   distribution
### Aliases: rexpexp

### ** Examples

rexpexp(100,2,3)



cleanEx()
nameEx("rexprayleigh")
### * rexprayleigh

flush(stderr()); flush(stdout())

### Name: rexprayleigh
### Title: Generate random observations from exponentiated Rayleigh
###   distribution
### Aliases: rexprayleigh

### ** Examples

rexprayleigh(100,2,3)



cleanEx()
nameEx("rexpweibull")
### * rexpweibull

flush(stderr()); flush(stdout())

### Name: rexpweibull
### Title: Generate random observations from exponentiated Weibull
###   distribution
### Aliases: rexpweibull

### ** Examples

rexpweibull(100,2,3,2)



cleanEx()
nameEx("rlevy")
### * rlevy

flush(stderr()); flush(stdout())

### Name: rlevy
### Title: Generate random observations from Levy distribution
### Aliases: rlevy

### ** Examples

rlevy(500,2,3)



cleanEx()
nameEx("weibullcp")
### * weibullcp

flush(stderr()); flush(stdout())

### Name: weibullcp
### Title: Probabilistic estimation of earthquake recurrence interval using
###   Weibull distribution
### Aliases: weibullcp

### ** Examples

fit=fitweibull(c(1,1),data=data_earthquake_6_6.5)
weibullcp(fit,r=2,te=5)



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

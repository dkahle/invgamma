pkgname <- "OBASpatial"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('OBASpatial')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("dnsrposoba")
### * dnsrposoba

flush(stderr()); flush(stdout())

### Name: dnsrposoba
### Title: Objective posterior density for the NSR model
### Aliases: dnsrposoba

### ** Examples



data(dataelev)

######### Using reference prior ###########
dnsrposoba(x=5,prior="reference",formula=elevation~1,
kappa=1,cov.model="matern",data=dataelev)

######### Using Jeffreys' rule prior ###########
dnsrposoba(x=5,prior="jef.rul",formula=elevation~1,
kappa=1,cov.model="matern",data=dataelev)

######### Using vague independent prior ###########
dnsrposoba(x=5,prior="vague",formula=elevation~1,
kappa=0.3,cov.model="matern",data=dataelev,intphi=c(0.1,10))




cleanEx()
nameEx("dnsrprioroba")
### * dnsrprioroba

flush(stderr()); flush(stdout())

### Name: dnsrprioroba
### Title: Objective prior density for the NSR model
### Aliases: dnsrprioroba

### ** Examples


data(dataelev)## data using by Berger et. al (2001)

######### Using reference prior ###########
dnsrprioroba(x=20,kappa=0.3,cov.model="matern",data=dataelev)


######### Using jef.rule prior###########
dnsrprioroba(x=20,prior="jef.rul",kappa=0.3,cov.model="matern",
data=dataelev)

######### Using  jef.ind prior ###########
dnsrprioroba(x=20,prior="jef.ind",trend=0,
kappa=0.3,cov.model="matern",data=dataelev)



cleanEx()
nameEx("dtsrposoba")
### * dtsrposoba

flush(stderr()); flush(stdout())

### Name: dtsrposoba
### Title: Objective posterior density for the TSR model
### Aliases: dtsrposoba

### ** Examples

data(dataca20)

######### Using reference prior ###########
dtsrposoba(x=c(5,11),prior="reference",formula=calcont~altitude+area,
kappa=0.3,cov.model="matern",data=dataca20)

######### Using Jeffreys' rule prior ###########
dtsrposoba(x=c(5,11),prior="jef.rul",formula=calcont~altitude+area,
kappa=0.3,cov.model="matern",data=dataca20)


######### Using Jeffreys' independent prior ###########
dtsrposoba(x=c(5,11),prior="jef.ind",formula=calcont~altitude+area
,kappa=0.3,cov.model="matern",data=dataca20)

######### Using vague independent prior ###########
dtsrposoba(x=c(5,11,.3),prior="vague",formula=calcont~altitude+area,
kappa=0.3,cov.model="matern",data=dataca20,intphi=c(0.1,10),
intnu=c(4.1,30))



cleanEx()
nameEx("dtsrprioroba")
### * dtsrprioroba

flush(stderr()); flush(stdout())

### Name: dtsrprioroba
### Title: Objective prior density for the TSR model
### Aliases: dtsrprioroba

### ** Examples

data(dataca20)

######### Using reference prior and a constant trend###########
dtsrprioroba(x=c(6,100),kappa=0.3,cov.model="matern",data=dataca20)


######### Using jef.rule prior and 1st trend###########
dtsrprioroba(x=c(6,100),prior="jef.rul",trend=~altitude+area,
kappa=0.3,cov.model="matern",data=dataca20)

######### Using  jef.ind prior ###########
dtsrprioroba(x=c(6,100),prior="jef.ind",trend=0,
kappa=0.3,cov.model="matern",data=dataca20)



cleanEx()
nameEx("intmT")
### * intmT

flush(stderr()); flush(stdout())

### Name: intmT
### Title: Marginal posterior density for a model.
### Aliases: intmT

### ** Examples


## Don't show: 
set.seed(25)
data(dataca20)

######### Using reference prior ###########
m1=intmT(prior="reference",formula=calcont~altitude+area,
kappa=0.3,cov.model="matern",data=dataca20,maxEval=1)

## End(Don't show)




cleanEx()
nameEx("intmnorm")
### * intmnorm

flush(stderr()); flush(stdout())

### Name: intmnorm
### Title: Marginal posterior density for a model.
### Aliases: intmnorm

### ** Examples


## Don't show: 
data(dataca20)

set.seed(25)
data(dataelev)## data using by Berger et. al (2001)

######### Using reference prior ###########
m1=intmnorm(prior="reference",formula=elevation~1,
kappa=0.5,cov.model="matern",data=dataelev,maxEval=1)

log(m1)


######### Using reference prior kappa=1 ###########
m2=intmnorm(prior="reference",formula=elevation~1,
kappa=1,cov.model="matern",data=dataelev,maxEval=1)
log(m2)

######### Using reference prior kappa=1.5 ###########
m3=intmnorm(prior="reference",formula=elevation~1
,kappa=1.5,cov.model="matern",data=dataelev,maxEval=1)
log(m3)

tot=m1+m2+m3

########posterior probabilities: higher probability:
#########prior="reference", kappa=1
p1=m1/tot
p2=m2/tot
p3=m3/tot


## End(Don't show)





cleanEx()
nameEx("nsroba")
### * nsroba

flush(stderr()); flush(stdout())

### Name: nsroba
### Title: Bayesian estimation for the NSR model.
### Aliases: nsroba

### ** Examples


## Don't show: 
set.seed(25)
data(dataelev)


######covariance matern: kappa=0.5
res=nsroba(elevation~1, kappa = 0.5, cov.model = "matern", data=dataelev,
ini.pars=c(10,390),iter=2,burn=0,thin=1)

summary(res)

## End(Don't show)





cleanEx()
nameEx("nsrobapred")
### * nsrobapred

flush(stderr()); flush(stdout())

### Name: nsrobapred1
### Title: Prediction under Normal Objective Bayesian Analysis (OBA).
### Aliases: nsrobapred1

### ** Examples



## Don't show: 
set.seed(25)
data(dataelev)
d1=dataelev[1:42,]

reselev=nsroba(elevation~1, kappa = 0.5, cov.model = "matern", data=d1,
ini.pars=c(10,3),intphi=c(0.8,10),iter=2,burn=0,thin=1)

datapred1=dataelev[43:52,]
coordspred1=datapred1[,1:2]
nsrobapred1(obj=reselev,coordspred=coordspred1,xpred=rep(1,10))
## End(Don't show)





cleanEx()
nameEx("summary.nsroba")
### * summary.nsroba

flush(stderr()); flush(stdout())

### Name: summary.nsroba
### Title: Summary of a nsroba object
### Aliases: summary.nsroba

### ** Examples


## Don't show: 
set.seed(25)
data(dataelev)


######covariance matern: kappa=0.5
resa=nsroba(elevation~1, kappa = 0.5, cov.model = "matern", data=dataelev,
ini.pars=c(10,3),iter=2,burn=0,thin=1)
summary(resa)

## End(Don't show)





cleanEx()
nameEx("summary.tsroba")
### * summary.tsroba

flush(stderr()); flush(stdout())

### Name: summary.tsroba
### Title: Summary of a nsroba object
### Aliases: summary.tsroba

### ** Examples


## Don't show: 

set.seed(25)
data(dataca20)
d1=dataca20[1:158,]

######covariance matern: kappa=0.3 prior:reference
rest=tsroba(calcont~altitude+area, kappa = 0.3, data=d1,
           ini.pars=c(10,3,10),iter=2,burn=0,thin=1)

summary(rest)

## End(Don't show)





cleanEx()
nameEx("tsroba")
### * tsroba

flush(stderr()); flush(stdout())

### Name: tsroba
### Title: Bayesian estimation for the TSR model.
### Aliases: tsroba

### ** Examples


## Don't show: 

set.seed(25)
data(dataca20)
d1=dataca20[1:158,]

xpred=model.matrix(calcont~altitude+area,data=dataca20[159:178,])
xobs=model.matrix(calcont~altitude+area,data=dataca20[1:158,])
coordspred=dataca20[159:178,1:2]

######covariance matern: kappa=0.3 prior:reference
res=tsroba(calcont~altitude+area, kappa = 0.3, data=d1,
           ini.pars=c(10,390,10),iter=2,burn=0,thin=1)

## End(Don't show)





cleanEx()
nameEx("tsrobapred")
### * tsrobapred

flush(stderr()); flush(stdout())

### Name: tsrobapred
### Title: Prediction under Student-t Objective Bayesian Analysis (OBA).
### Aliases: tsrobapred

### ** Examples

## Don't show: 
set.seed(25)
data(dataca20)
d1=dataca20[1:158,]

######covariance matern: kappa=0.3 prior:reference
res=tsroba(calcont~altitude+area, kappa = 0.3, data=d1,
ini.pars=c(10,3,10),iter=2,thin=1,burn=0)

datapred=dataca20[159:178,]
formula=calcont~altitude+area
xpred=model.matrix(formula,data=datapred)

tsrobapred(res,xpred=xpred,coordspred=dataca20[159:178,1:2])
## End(Don't show)




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

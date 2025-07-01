pkgname <- "DTAT"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('DTAT')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("Onoue.Friberg")
### * Onoue.Friberg

flush(stderr()); flush(stdout())

### Name: Onoue.Friberg
### Title: POMP PK/PD model for docetaxel, combining Onoue et al (2016)
###   with Friberg et al (2002)
### Aliases: Onoue.Friberg

### ** Examples

# Reproduce the sim$pkpd model and sim$pop population from reference #3:
library(pomp)
Onoue.Friberg(N=25)
sim$pop # NB: this differs from pop of original paper...

# Whereas the present version of Onoue.Friberg() draws simulated populations
# using pomp::rprior(), to reproduce the original F1000Research paper [3] we
# re-draw sim$pop as originally & prosaically done (see https://osf.io/vwnqz/):
set.seed(2016)
N <- 25
dtx.mm <- 0.808 # molar mass (mg/µM) of docetaxel
sim$pop$Circ0 <- rlnorm(N, meanlog=log(5050), sdlog=0.42) # units=cells/mm^3
sim$pop$MTT <- rlnorm(N, meanlog=log(89.3), sdlog=0.16)  # mean transit time
sim$pop$gamma <- rlnorm(N, meanlog=log(0.163), sdlog=0.039) # feedback factor
sim$pop$Emax <- rlnorm(N, meanlog=log(83.9), sdlog=0.33)
sim$pop$EC50 <- rlnorm(N, meanlog=log(7.17*dtx.mm), sdlog=0.50)
# PK params from 2-compartment docetaxel model of Onoue et al (2016)
sim$pop$CL <- rlnorm(N, meanlog=log(32.6), sdlog=0.295)
sim$pop$Q  <- rlnorm(N, meanlog=log(5.34), sdlog=0.551)
sim$pop$Vc <- rlnorm(N, meanlog=log(5.77), sdlog=0.1) # Onoue gives no CV% for V1
sim$pop$Vp <- rlnorm(N, meanlog=log(11.0), sdlog=0.598) # Called 'V2' in Onoue
sim$pop$kTR=4/sim$pop$MTT

# Now we run the sim$pkpd model, separately for each of N simultated individuals:
allout <- data.frame() # accumulator for N individual ODE solutions
for (id in 1:sim$N) {
  out <- trajectory(sim$pkpd,
                    params=c(sim$pop[sim$pop$id==id, -which(names(sim$pop) %in% c('id','MTT'))]
                             , sigma=0.05, dose=100, duration=1),
                    format="data.frame")
  # drop 'traj' and shift 'time' to first column
  out <- out[,c('time',setdiff(colnames(out),c('time','traj')))]
  out$id <- paste("id",id,sep="")
  allout <- rbind(allout, out)
}

library(Hmisc)
allout <- upData(allout
                 , id = ordered(id, levels=paste("id",1:sim$N,sep=""))
                 , units=c(Prol="cells/mm^3", Tx.1="cells/mm^3",
                           Tx.2="cells/mm^3", Tx.3="cells/mm^3",
                           Circ="cells/mm^3",
                           Cc="ng/mL", Cp="ng/mL",
                           time="hours"), print=FALSE)

library(tidyr)
cout <- gather(allout, key="Series", value="Concentration"
, Cc, Cp
, factor_key = TRUE)

label(cout$Concentration) <- "Drug Concentration"

# Figure 1 from reference [3]:
library(RColorBrewer)
xYplot(Concentration ~ time | id, group=Series
       , data=cout, subset=time<6
       , layout=c(5,NA)
       , type='l', as.table=TRUE
       , label.curves=FALSE
       , par.settings=list(superpose.line=list(lwd=2,col=brewer.pal(4,"PRGn")[c(1,4)]))
       , scales=list(y=list(log=TRUE, lim=c(10^-3,10^1)))
       , auto.key=list(points=FALSE, lines=TRUE, columns=2))

mout <- gather(allout, key="Series", value="ANC"
, Prol, Tx.1, Tx.2, Tx.3, Circ
, factor_key = TRUE)

mout <- upData(mout
               , time = time/24
               , units = c(time="days")
               , print = FALSE)

# Figure 3 from citation [3]:
xYplot(ANC ~ time | id, group=Series
       , data=mout
       , layout=c(5,5)
       , type='l', as.table=TRUE
       , label.curves=FALSE
       , par.settings=list(superpose.line=list(lwd=2,col=brewer.pal(11,"RdYlBu")[c(1,3,4,8,10)]))
       , scales=list(y=list(log=TRUE, lim=c(100,15000), at=c(200, 500, 1000, 2000, 5000, 10000)))
       , auto.key=list(points=FALSE, lines=TRUE, columns=5))




cleanEx()
nameEx("de.bioRxiv.240846")
### * de.bioRxiv.240846

flush(stderr()); flush(stdout())

### Name: de.bioRxiv.240846
### Title: Simulated '3+3/PC' dose-titration study from bioRxiv paper no.
###   240846
### Aliases: de.bioRxiv.240846
### Keywords: datasets

### ** Examples


data(de.bioRxiv.240846)
# Demonstrate that the new S4 3+3/PC implementation reproduces the
# simulated trial from the paper:
set.seed(2017)
CV <- 0.7; mean_mtd <- 1.0
shape <- CV^-2; scale <- mean_mtd/shape
trial <- new("DE", doses=0.25 * 1.4^(0:6),
             MTDi=rgamma(24, shape=shape, scale=scale),
             units="mg")
trial <- titration(trial, periods=10)
stopifnot(all(trial@data == de.bioRxiv.240846[[10]]))
stopifnot(trial@stop_esc == attr(de.bioRxiv.240846[[10]],'stop.esc'))




cleanEx()
nameEx("dose.survfit")
### * dose.survfit

flush(stderr()); flush(stdout())

### Name: dose.survfit
### Title: Calculate a dose-survival curve from a dose titration study,
###   adding a confidence band
### Aliases: dose.survfit
### Keywords: survival

### ** Examples

CV <- 0.7; mean_mtd <- 1.0
shape <- CV^-2; scale <- mean_mtd/shape
trial <- new("DE", doses=0.25 * 1.4^(0:6),
             MTDi=rgamma(24, shape=shape, scale=scale),
             units="mg")
trial <- titration(trial, periods=10)
sf <- dose.survfit(trial@data)
summary(sf)




cleanEx()
nameEx("dose.survival")
### * dose.survival

flush(stderr()); flush(stdout())

### Name: dose.survival
### Title: Extract interval-censored dose tolerance data from a dose
###   titration study
### Aliases: dose.survival
### Keywords: survival

### ** Examples

CV <- 0.7; mean_mtd <- 1.0
shape <- CV^-2; scale <- mean_mtd/shape
trial <- new("DE", doses=0.25 * 1.4^(0:6),
             MTDi=rgamma(24, shape=shape, scale=scale),
             units="mg")
trial <- titration(trial, periods=10)
dose.survival(trial@data)




cleanEx()
nameEx("ds.curve")
### * ds.curve

flush(stderr()); flush(stdout())

### Name: ds.curve
### Title: Extract the dose-survival curve, with its upper and lower
###   confidence band limits
### Aliases: ds.curve
### Keywords: survival

### ** Examples

CV <- 0.7; mean_mtd <- 1.0
shape <- CV^-2; scale <- mean_mtd/shape
trial <- new("DE", doses=0.25 * 1.4^(0:6),
             MTDi=rgamma(24, shape=shape, scale=scale),
             units="mg")
trial <- titration(trial, periods=10)
ds.curve(trial@data)




cleanEx()
nameEx("dtat1000")
### * dtat1000

flush(stderr()); flush(stdout())

### Name: dtat1000
### Title: Precomputed neutrophil-guided chemotherapy dose titration for
###   1000 simulated subjects.
### Aliases: dtat1000
### Keywords: datasets

### ** Examples


data(dtat1000)
# 1. Extract the N final doses, assuming convergence by the tenth course
MTD_i <- with(dtat1000, dose[time==27])
MTD_i <- MTD_i[MTD_i < 5000] # Exclude few outliers
# 2. Do a kernel density plot
library(Hmisc)
library(latticeExtra)
hist <- histogram(~MTD_i, breaks=c(0,100,200,300,400,600,900,1500,2500,4000,5000)
                  , xlab=expression(MTD[i]))
approx <- data.frame(mtd_i=seq(0, 5000, 10))
approx <- upData(approx,
                 gamma = dgamma(mtd_i, shape=1.75, scale=200))
dist <- xyplot(gamma ~ mtd_i, data=approx, type='l', col='black', lwd=2)
library(grid)
hist + dist
grid.text(expression(MTD[i] %~%
                     paste("Gamma(", alpha==1.75, ", ", beta==1/200,")"))
         , x=unit(0.5,"npc")
         , y=unit(0.75,"npc")
         )
## A very long repro, which a user of this package may well wish to verify
## by running the examples interactively, although it takes many minutes
## to compute.  (Enclosed in a dontest block to avoid overburdening CRAN.)




cleanEx()
nameEx("runDTATapp")
### * runDTATapp

flush(stderr()); flush(stdout())

### Name: runDTATapp
### Title: Run Shiny apps included in package DTAT
### Aliases: runDTATapp

### ** Examples

if(interactive()){
runDTATapp("Sim33PC")
runDTATapp("TheCost")
}



cleanEx()
nameEx("seq.function")
### * seq.function

flush(stderr()); flush(stdout())

### Name: seq.function
### Title: A seq method supporting custom-scaled plot axes.
### Aliases: seq.function

### ** Examples


# Provide evenly-spaced length-6 sequence from 100 to 1000,
# evenly spaced on a fourth-root scale:
seq(function(dose, a=4.0) dose^(1/a), from=100, to=1000, length.out=6, digits=0)




cleanEx()
nameEx("sim")
### * sim

flush(stderr()); flush(stdout())

### Name: sim
### Title: Environment for simulation global variables.
### Aliases: sim
### Keywords: datasets

### ** Examples


# Even when nrow(pop) is large, one may easily restrict
# time-consuming simulations to pop[1:N,], as follows:
sim$N <- 25
# Now perform simulation work
## Not run: 
##D titrate(...)
## End(Not run)




cleanEx()
nameEx("titrate")
### * titrate

flush(stderr()); flush(stdout())

### Name: titrate
### Title: Perform neutrophil-guided dose titration of a chemotherapy drug.
### Aliases: titrate

### ** Examples

if(interactive()){
# Reproduce Figure 5 from the F1000Research paper (run time > 10 s).
# 1. Set up sim$pop & sim$pkpd by running the repro for Figures 1 & 3:
example(topic="Onoue.Friberg", package="DTAT", ask=FALSE)
# 2. Do the neutrophil-nadir-guided dose titration:
chemo <- titrate(doserange = c(50, 3000),
                 dta=newton.raphson(dose1 = 50,
                                    omega = 0.75,
                                    slope1 = -2.0,
                                    slopeU = -0.2)
                 )
library(latticeExtra)
newton <- chemo$course
new.ts <- chemo$anc.ts
anc.tics <- c(200,500,1500,4000,10000)
right <- xYplot(ANC ~ time | id, data=new.ts
                , as.table=TRUE, type="l"
                , layout=c(5,5)
                , scales=list(y=list(log=TRUE, lim=c(100,1.5e4)
                                     , at=anc.tics
                                     , lab=as.character(anc.tics)),
                              x=list(at=seq(0,30,3)))
)
newton <- upData(newton
                 , time = 3*(cycle-1)
                 , labels = c(time="Time")
                 , units = c(time="weeks")
                 , print = FALSE)
dose.tics <- c(50, 200, 600, 1500, 3000)
left <- xYplot(scaled.dose ~ time | id, data=newton
               , as.table=TRUE, type='p', pch='+', cex=1.5
               , layout=c(5,5)
               , scales=list(y=list(lim=DTAT:::scaled(c(30,3200))
                                    , at=DTAT:::scaled(dose.tics)
                                    , lab=as.character(dose.tics)),
                             x=list(lim=c(-1,31)
                                    , at=seq(0,30,3)
                                    , lab=c('0','','6','','12','','18','','24','','30')))
)
update(doubleYScale(left, right, add.ylab2=TRUE)
       , par.settings = simpleTheme(col=brewer.pal(4,"PRGn")[c(4,1)])
)
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

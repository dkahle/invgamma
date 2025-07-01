pkgname <- "dwp"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('dwp')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("Acins")
### * Acins

flush(stderr()); flush(stdout())

### Name: Acins
### Title: Calculate Area of Intersection inside Circle and Square with
###   Common Center
### Aliases: Acins

### ** Examples

 # calculate area in annulus intersecting square
s <- 10 # radius or half-width of square
r <- c(11, 12) # inner and outer radii of circle
diff(Acins(r, s)) # intersection of square and annulus
# figure to illustrate the calculated area:
theta <- seq(0, 2 * pi, length = 1500)
plot(0, xlim = max(r) * c(-1, 1), ylim = max(r) * c(-1, 1),
  xlab = "x", ylab = "y", asp = 1, bty = "n", type = "n")
xi <- r[1] * cos(theta)
yi <- r[1] * sin(theta)
xo <- r[2] * cos(theta)
yo <- r[2] * sin(theta)
i1 <- which(abs(xi) <= s & abs(yi) <= s)
i2 <- which(abs(xo) <= s & abs(yo) <= s)
i2 <- sort(i2, decreasing = TRUE)
xi <- xi[i1]
yi <- yi[i1]
xo <- xo[i2]
yo <- yo[i2]
polygon(col = 8, border = NA,
  x = c(xi[xi >= 0 & yi >= 0], xo[xo >= 0 & yo >= 0]), 
  y = c(yi[xi >= 0 & yi >= 0], yo[xo >= 0 & yo >= 0]))
polygon(col = 8, border = NA, 
  x = c(xi[xi <= 0 & yi >= 0], xo[xo <= 0 & yo >= 0]), 
  y = c(yi[xi <= 0 & yi >= 0], yo[xo <= 0 & yo >= 0]))
polygon(col = 8, border = NA,
  x = c(xi[xi <= 0 & yi <= 0], xo[xo <= 0 & yo <= 0]), 
 y = c(yi[xi <= 0 & yi <= 0], yo[xo <= 0 & yo <= 0]))
polygon(col = 8, border = NA,
 x = c(xi[xi >= 0 & yi <= 0], xo[xo >= 0 & yo <= 0]), 
 y = c(yi[xi >= 0 & yi <= 0], yo[xo >= 0 & yo <= 0]))
lines(r[1] * cos(theta), r[1]* sin(theta))
lines(r[2]* cos(theta), r[2] * sin(theta))
rect(-s, -s, s, s)
 # calculate areas in series of 1 m annuli extending to corner of square
 s <- 10.5 # radius of square (center to side)
 diff(Acins(r = 0:ceiling(sqrt(2) * s), s))



cleanEx()
nameEx("addCarcass")
### * addCarcass

flush(stderr()); flush(stdout())

### Name: addCarcass
### Title: Add Carcasses to a Site Layout
### Aliases: addCarcass addCarcass.shapeCarcass addCarcass.data.frame

### ** Examples

 data(layout_simple)
 data(carcass_simple)
 sitedata <- initLayout(layout_simple)
 ringdata <- prepRing(sitedata)
 ringsWithCarcasses <- addCarcass(carcass_simple, data_ring = ringdata)



cleanEx()
nameEx("ddFit")
### * ddFit

flush(stderr()); flush(stdout())

### Name: ddFit
### Title: Fit Distance Distribution Model(s)
### Aliases: ddFit ddFit.data.frame ddFit.rings ddFit.list ddFit.xyLayout
###   ddFit.ringscc

### ** Examples

 data(layout_simple) 
 data(carcass_simple)
 sitedata <- initLayout(layout_simple) # initialize
 ringdata <- prepRing(sitedata) # format site layout data for modeling
 ringsWithCarcasses <- addCarcass(carcass_simple, data_ring = ringdata) # add carcasses to site
 distanceModels <- ddFit(ringsWithCarcasses) # fit distance models



cleanEx()
nameEx("ddd")
### * ddd

flush(stderr()); flush(stdout())

### Name: ddd
### Title: Calculate Probability Functions for Distance Distributions
### Aliases: ddd pdd qdd rdd rcd

### ** Examples

data(layout_simple)
data(carcass_simple)
sitedata <- initLayout(layout_simple)
ringdata <- prepRing(sitedata)
ringsWithCarcasses <- addCarcass(carcass_simple, data_ring = ringdata)
distanceModels <- ddFit(ringsWithCarcasses)
modelEvaluations <- modelFilter(distanceModels)
bestModel <- modelEvaluations$filtered
pdd(100, model = bestModel) # estimated fraction of carcasses within 100m
ddd(1:150, model = bestModel) # estimated PDF of the carcass distances
qdd(0.9, model = bestModel) # estimated 0.9 quantile of carcass distances
rdd(1000, model = bestModel) # 1000 random draws from estimated carcass distribution



cleanEx()
nameEx("initLayout")
### * initLayout

flush(stderr()); flush(stdout())

### Name: initLayout
### Title: Create a Data Structure or Map for the Site Layout
### Aliases: initLayout

### ** Examples

data(layout_simple)
# converts properly formatted dataframe to 'simpleLayout' object
initLayout(layout_simple) 

data(layout_xy)
initLayout(layout_xy, dataType = "xy")

data(layout_polygon)
initLayout(layout_polygon, dataType = "polygon", unitCol = "turbine")




cleanEx()
nameEx("modelFilter")
### * modelFilter

flush(stderr()); flush(stdout())

### Name: modelFilter
### Title: Run Models through a Sieve to Filter out Dubious Fits
### Aliases: modelFilter

### ** Examples

 data(layout_simple)
 data(carcass_simple)
 sitedata <- initLayout(layout_simple)
 ringdata <- prepRing(sitedata)
 ringsWithCarcasses <- addCarcass(carcass_simple, data_ring = ringdata)
 distanceModels <- ddFit(ringsWithCarcasses)
 stats(distanceModels)
 stats(distanceModels[["tnormal"]])
 stats(distanceModels[["lognormal"]])
 



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

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/invgamma.R
\name{invgamma}
\alias{invgamma}
\alias{dinvgamma}
\alias{pinvgamma}
\alias{qinvgamma}
\alias{rinvgamma}
\title{The Inverse Gamma Distribution}
\usage{
dinvgamma(x, shape, rate = 1, scale = 1/rate, log = FALSE)

pinvgamma(q, shape, rate = 1, scale = 1/rate, lower.tail = TRUE, log.p = FALSE)

qinvgamma(p, shape, rate = 1, scale = 1/rate, lower.tail = TRUE, log.p = FALSE)

rinvgamma(n, shape, rate = 1, scale = 1/rate)
}
\arguments{
\item{x, q}{vector of quantiles.}

\item{shape, rate, scale}{\code{shape}, \code{rate}, and \code{scale} parameters of
corresponding gamma distribution. In particular, \code{rate} and \code{scale} are
\strong{not} the rate and scale of the inverse gamma distribution, but of the
\emph{gamma} distribution.}

\item{log, log.p}{logical; if \code{TRUE}, probabilities p are given as log(p).}

\item{lower.tail}{logical; if \code{TRUE} (default), probabilities are \eqn{P[X
\leq x]}; if \code{FALSE} \eqn{P[X > x]}.}

\item{p}{vector of probabilities.}

\item{n}{number of observations. If length(n) > 1, the length is taken to be
the number required.}
}
\description{
Density, distribution function, quantile function and random generation for
the inverse gamma distribution.
}
\details{
The inverse gamma distribution with parameters shape and rate has density
\deqn{f(x) = \frac{rate^{shape}}{\Gamma(shape)} x^{-1-shape} e^{-rate/x}} it
is the inverse of the standard gamma parameterization in R. If \eqn{X \sim
InvGamma(shape, rate)}, \deqn{E[X] = \frac{rate}{shape-1}} when \eqn{shape > 1}
and \deqn{Var(X) = \frac{rate^2}{(shape - 1)^2(shape - 2)}} for \eqn{shape >
2}.

The functions \verb{(d/p/q/r)invgamma()} simply wrap those of the standard
\verb{(d/p/q/r)gamma()} R implementation, so look at, say, \code{\link[stats:GammaDist]{stats::dgamma()}} for
details.
}
\examples{

s <- seq(0, 5, .01)
plot(s, dinvgamma(s, 7, 10), type = 'l')

f <- function(x) dinvgamma(x, 7, 10)
q <- 2
integrate(f, 0, q)
(p <- pinvgamma(q, 7, 10))
qinvgamma(p, 7, 10) # = q
mean(rinvgamma(1e5, 7, 10) <= q)

shape <- 3; rate <- 7
x <- rinvgamma(1e6, shape, rate)
mean(x) # = rate / (shape - 1)
var(x)  # = rate^2 / ( (shape - 1)^2 * (shape - 2) )

shape <- 7; rate <- 2.01
x <- rinvgamma(1e6, shape, rate)
mean(x) # = rate / (shape - 1)
var(x)  # = rate^2 / ( (shape - 1)^2 * (shape - 2) )

qnorm(log(.25), log.p = TRUE)
qnorm(.25)

qinvgamma(log(.25), shape = shape, rate = rate, log.p = TRUE)
qinvgamma(.25, shape = shape, rate = rate)

\dontrun{ `rinvgamma()` warns when shape <= .01

rinvgamma(10, .01, rate) # warns

}

}
\seealso{
\code{\link[stats:GammaDist]{stats::dgamma()}}; these functions just wrap the \verb{(d/p/q/r)gamma()}
functions.
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/invchisq.R
\name{invchisq}
\alias{invchisq}
\alias{dinvchisq}
\alias{pinvchisq}
\alias{qinvchisq}
\alias{rinvchisq}
\title{The Inverse (non-central) Chi-Squared Distribution}
\usage{
dinvchisq(x, df, ncp = 0, log = FALSE)

pinvchisq(q, df, ncp = 0, lower.tail = TRUE, log.p = FALSE)

qinvchisq(p, df, ncp = 0, lower.tail = TRUE, log.p = FALSE)

rinvchisq(n, df, ncp = 0)
}
\arguments{
\item{x, q}{vector of quantiles.}

\item{df}{degrees of freedom (non-negative, but can be non-integer).}

\item{ncp}{non-centrality parameter (non-negative).}

\item{log, log.p}{logical; if \code{TRUE}, probabilities p are given as log(p).}

\item{lower.tail}{logical; if \code{TRUE} (default), probabilities are \eqn{P[X
\leq x]}; if \code{FALSE} \eqn{P[X > x]}.}

\item{p}{vector of probabilities.}

\item{n}{number of observations. If length(n) > 1, the length is taken to be
the number required.}
}
\description{
Density, distribution function, quantile function and random generation for
the inverse chi-squared distribution.
}
\details{
The functions \verb{(d/p/q/r)invchisq()} simply wrap those of the standard
\verb{(d/p/q/r)chisq()} R implementation, so look at, say, \code{\link[stats:Chisquare]{stats::dchisq()}} for
details.
}
\examples{

s <- seq(0, 3, .01)
plot(s, dinvchisq(s, 3), type = 'l')

f <- function(x) dinvchisq(x, 3)
q <- 2
integrate(f, 0, q)
(p <- pinvchisq(q, 3))
qinvchisq(p, 3) # = q
mean(rinvchisq(1e5, 3) <= q)




f <- function(x) dinvchisq(x, 3, ncp = 2)
q <- 1.5
integrate(f, 0, q)
(p <- pinvchisq(q, 3, ncp = 2))
qinvchisq(p, 3, ncp = 2) # = q
mean(rinvchisq(1e7, 3, ncp = 2) <= q)



}
\seealso{
\code{\link[stats:Chisquare]{stats::dchisq()}}; these functions just wrap the \verb{(d/p/q/r)chisq()}
functions.
}

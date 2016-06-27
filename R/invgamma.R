#' The Inverse Gamma Distribution
#'
#' Density, distribution function, quantile function and random
#' generation for the inverse gamma distribution.
#'
#' The inverse gamma distribution with parameters shape and rate has
#' density \emph{f(x) = rate^shape/Gamma(shape) x^(-1-shape)
#' e^(-rate/x)} it is the inverse of the standard gamma
#' parameterzation in R.
#'
#' The functions (d/p/q/r)invgamma simply wrap those of the standard
#' (d/p/q/r)gamma R implementation, so look at, say,
#' \code{\link{dgamma}} for details.
#'
#'
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations. If length(n) > 1, the length is
#'   taken to be the number required.
#' @param shape inverse gamma shape parameter
#' @param rate inverse gamma rate parameter
#' @param scale alternative to rate; scale = 1/rate
#' @param log,log.p logical; if TRUE, probabilities p are given as
#'   log(p).
#' @param lower.tail logical; if TRUE (default), probabilities are
#'   P[X <= x] otherwise, P[X > x].
#' @seealso \code{\link{dgamma}}; these functions just wrap the
#'   (d/p/q/r)gamma functions.
#' @name invgamma
#' @importFrom stats dgamma pgamma qgamma rgamma
#' @examples
#'
#' s <- seq(0, 5, .01)
#' plot(s, dinvgamma(s, 7, 10), type = 'l')
#'
#' f <- function(x) dinvgamma(x, 7, 10)
#' q <- 2
#' integrate(f, 0, q)
#' (p <- pinvgamma(q, 7, 10))
#' qinvgamma(p, 7, 10) # = q
#' mean(rinvgamma(1e5, 7, 10) <= q)
#'
#'
#'
NULL




#' @rdname invgamma
#' @export
dinvgamma <- function(x, shape, rate, scale = 1/rate, log = FALSE) {
  if(missing(rate) && !missing(scale)) rate <- 1/scale
  log_f <- dgamma(1/x, shape, rate, log = TRUE) - 2*log(x)
  if(log) return(log_f)
  exp(log_f)
}




#' @rdname invgamma
#' @export
pinvgamma <- function(q, shape, rate, scale = 1/rate, lower.tail = TRUE, log.p = FALSE) {
  if(missing(rate) && !missing(scale)) rate <- 1/scale
  pgamma(1/q, shape, rate, lower.tail = !lower.tail, log.p = log.p)
}
# pgamma(q) = P(X <= q) = P(1/X >= 1/q) = 1 - P(1/X < 1/q)
# P(1/X < x) = 1 - pgamma(1/x)




#' @rdname invgamma
#' @export
qinvgamma <- function(p, shape, rate, scale = 1/rate, lower.tail = TRUE, log.p = FALSE) {
  if(missing(rate) && !missing(scale)) rate <- 1/scale
  qgamma(1-p, shape, rate, lower.tail = lower.tail, log.p = log.p)^(-1)
}
# P(1/X < x) = 1 - pgamma(1/x)
# x = 1 - pgamma(1/y)
# pgamma(1/y) = 1 - x
# 1/y = qgamma(1 - x)
# y = qgamma(1-x)^(-1)




#' @rdname invgamma
#' @export
rinvgamma <- function(n, shape, rate, scale = 1/rate) {
  if(missing(rate) && !missing(scale)) rate <- 1/scale
  1 / rgamma(n, shape, rate)
}

















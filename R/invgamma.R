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
#'
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations. If length(n) > 1, the length is
#'   taken to be the number required.
#' @param shape inverse gamma parameter shape
#' @param rate inverse gamma parameter rate
#' @param log logical; if TRUE, probabilities p are given as log(p).
#' @param lower.tail logical; if TRUE (default), probabilities are
#'   P[X <= x] otherwise, P[X > x].
#' @name invgamma
#' @examples
#'
#' s <- seq(0, 5, .01)
#' plot(s, dinvgamma(s, .5, .8))
#'
#' f <- function(x) dinvgamma(x, .5, .8)
#' integrate(f, 0, 2)
#' pinvgamma(2, .5, .8)
#' qinvgamma(0.3710934, .5, .8)
#' mean(rinvgamma(1e5, .5, .8) <= 2)
#'
#'
#'
NULL




#' @rdname invgamma
#' @export
dinvgamma <- function(x, shape, rate, log = FALSE) {
  log_f <- shape*log(rate) - lgamma(shape) + (-1-shape)*log(x) - rate/x
  if(log) return(log_f)
  exp(log_f)
}


#' @rdname invgamma
#' @export
pinvgamma <- function(q, shape, rate, lower.tail = TRUE) {
  pgamma(1/q, shape, rate, lower.tail = !lower.tail)
}
# pgamma(q) = P(X <= q) = P(1/X >= 1/q) = 1 - P(1/X < 1/q)
# P(1/X < x) = 1 - pgamma(1/x)


#' @rdname invgamma
#' @export
qinvgamma <- function(p, shape, rate) {
  qgamma(1-p, shape, rate)^(-1)
}
# P(1/X < x) = 1 - pgamma(1/x)
# x = 1 - pgamma(1/y)
# pgamma(1/y) = 1 - x
# 1/y = qgamma(1 - x)
# y = qgamma(1-x)^(-1)


#' @rdname invgamma
#' @export
rinvgamma <- function(n, shape, rate) {
  rgamma(n, shape, rate)^(-1)
}


#' The Inverse Gamma Distribution
#'
#' Density, distribution function, quantile function and random generation for
#' the inverse gamma distribution.
#'
#' The inverse gamma distribution with parameters shape and rate has density
#' \deqn{f(x) = \frac{rate^{shape}}{\Gamma(shape)} x^{-1-shape} e^{-rate/x}} it
#' is the inverse of the standard gamma parameterization in R. If \eqn{X \sim
#' InvGamma(shape, rate)}, \deqn{E[X] = \frac{rate}{shape-1}} when \eqn{shape > 1}
#' and \deqn{Var(X) = \frac{rate^2}{(shape - 1)^2(shape - 2)}} for \eqn{shape > 2}.
#'
#' The functions `(d/p/q/r)invgamma()` simply wrap those of the standard
#' `(d/p/q/r)gamma()` R implementation, so look at, say, [stats::dgamma()] for
#' details.
#'
#'
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations. If length(n) > 1, the length is taken to be
#'   the number required.
#' @param shape inverse gamma shape parameter
#' @param rate inverse gamma rate parameter
#' @param scale alternative to rate; scale = 1/rate
#' @param log,log.p logical; if `TRUE`, probabilities p are given as log(p).
#' @param lower.tail logical; if `TRUE` (default), probabilities are \eqn{P[X
#'   \leq x]}; if `FALSE` \eqn{P[X > x]}.
#' @seealso [stats::dgamma()]; these functions just wrap the `(d/p/q/r)gamma()`
#'   functions.
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
#' shape <- 3; rate <- 7
#' x <- rinvgamma(1e6, shape, rate)
#' mean(x) # = rate / (shape - 1)
#' var(x)  # = rate^2 / ( (shape - 1)^2 * (shape - 2) )
#'
#' shape <- 7; rate <- 2.01
#' x <- rinvgamma(1e6, shape, rate)
#' mean(x) # = rate / (shape - 1)
#' var(x)  # = rate^2 / ( (shape - 1)^2 * (shape - 2) )
#'
#' rinvgamma(10, .001, rate)
#'
NULL




#' @rdname invgamma
#' @export
dinvgamma <- function(x, shape, rate = 1, scale = 1/rate, log = FALSE) {
  if(missing(rate) && !missing(scale)) rate <- 1/scale
  log_f <- dgamma(1/x, shape, rate, log = TRUE) - 2*log(x)
  if(log) return(log_f)
  exp(log_f)
}




#' @rdname invgamma
#' @export
pinvgamma <- function(q, shape, rate = 1, scale = 1/rate, lower.tail = TRUE, log.p = FALSE) {
  if(missing(rate) && !missing(scale)) rate <- 1/scale
  pgamma(1/q, shape, rate, lower.tail = !lower.tail, log.p = log.p)
}
# pgamma(q) = P(X <= q) = P(1/q <= 1/X) = 1 - P(1/X < 1/q)
# so P(1/X <= 1/q) = P(1/X < 1/q) = 1 - pgamma(q).
# if x = 1/q, P(1/X <= x) = 1 - pgamma(1/x)




#' @rdname invgamma
#' @export
qinvgamma <- function(p, shape, rate = 1, scale = 1/rate, lower.tail = TRUE, log.p = FALSE) {
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
rinvgamma <- function(n, shape, rate = 1, scale = 1/rate) {
  if(missing(rate) && !missing(scale)) rate <- 1/scale
  if (shape <= .01) stop("`rinvgamma()` is unreliable for `shape` <= .01.")
  1 / rgamma(n, shape, rate)
}

















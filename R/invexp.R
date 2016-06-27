#' The Inverse Exponential Distribution
#'
#' Density, distribution function, quantile function and random
#' generation for the inverse exponential distribution.
#'
#' The functions (d/p/q/r)invexp simply wrap those of the standard
#' (d/p/q/r)exp R implementation, so look at, say,
#' \code{\link{dexp}} for details.
#'
#'
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations. If length(n) > 1, the length is
#'   taken to be the number required.
#' @param rate degrees of freedom (non-negative, but can be
#'   non-integer).
#' @param log,log.p logical; if TRUE, probabilities p are given as
#'   log(p).
#' @param lower.tail logical; if TRUE (default), probabilities are
#'   P[X <= x] otherwise, P[X > x].
#' @seealso \code{\link{dexp}}; these functions just wrap the
#'   (d/p/q/r)exp functions.
#' @importFrom stats dexp pexp qexp rexp
#' @name invexp
#' @examples
#'
#' s <- seq(0, 10, .01)
#' plot(s, dinvexp(s, 2), type = 'l')
#'
#' f <- function(x) dinvexp(x, 2)
#' q <- 3
#' integrate(f, 0, q)
#' (p <- pinvexp(q, 2))
#' qinvexp(p, 2) # = q
#' mean(rinvexp(1e5, 2) <= q)
#'
#' pinvgamma(q, 1, 2)
#'
#'
#'
NULL




#' @rdname invexp
#' @export
dinvexp <- function(x, rate = 1, log = FALSE) {
  log_f <- dexp(1/x, rate, log = TRUE) - 2*log(x)
  if(log) return(log_f)
  exp(log_f)
}




#' @rdname invexp
#' @export
pinvexp <- function(q, rate = 1, lower.tail = TRUE, log.p = FALSE) {
  pexp(1/q, rate, lower.tail = !lower.tail, log.p = log.p)
}




#' @rdname invexp
#' @export
qinvexp <- function(p, rate = 1, lower.tail = TRUE, log.p = FALSE) {
  qexp(1-p, rate, lower.tail = lower.tail, log.p = log.p)^(-1)
}





#' @rdname invexp
#' @export
rinvexp <- function(n, rate = 1) {
  1 / rexp(n, rate)
}

















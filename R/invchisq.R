#' The Inverse (non-central) Chi-Squared Distribution
#'
#' Density, distribution function, quantile function and random generation for
#' the inverse chi-squared distribution.
#'
#' The functions `(d/p/q/r)invchisq()` simply wrap those of the standard
#' `(d/p/q/r)chisq()` R implementation, so look at, say, [stats::dchisq()] for
#' details.
#'
#'
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations. If length(n) > 1, the length is taken to be
#'   the number required.
#' @param df degrees of freedom (non-negative, but can be non-integer).
#' @param ncp non-centrality parameter (non-negative).
#' @param log,log.p logical; if `TRUE`, probabilities p are given as log(p).
#' @param lower.tail logical; if `TRUE` (default), probabilities are \eqn{P[X
#'   \leq x]}; if `FALSE` \eqn{P[X > x]}.
#' @seealso [stats::dchisq()]; these functions just wrap the `(d/p/q/r)chisq()`
#'   functions.
#' @importFrom stats dchisq pchisq qchisq rchisq
#' @name invchisq
#' @examples
#'
#' s <- seq(0, 3, .01)
#' plot(s, dinvchisq(s, 3), type = 'l')
#'
#' f <- function(x) dinvchisq(x, 3)
#' q <- 2
#' integrate(f, 0, q)
#' (p <- pinvchisq(q, 3))
#' qinvchisq(p, 3) # = q
#' mean(rinvchisq(1e5, 3) <= q)
#'
#'
#'
#'
#' f <- function(x) dinvchisq(x, 3, ncp = 2)
#' q <- 1.5
#' integrate(f, 0, q)
#' (p <- pinvchisq(q, 3, ncp = 2))
#' qinvchisq(p, 3, ncp = 2) # = q
#' mean(rinvchisq(1e7, 3, ncp = 2) <= q)
#'
#'
#'
NULL




#' @rdname invchisq
#' @export
dinvchisq <- function(x, df, ncp = 0, log = FALSE) {
  log_f <- dchisq(1/x, df, ncp, log = TRUE) - 2*log(x)
  log_f[x == 0] <- -Inf
  if(log) return(log_f)
  exp(log_f)
}




#' @rdname invchisq
#' @export
pinvchisq <- function(q, df, ncp = 0, lower.tail = TRUE, log.p = FALSE) {
  pchisq(1/q, df, ncp, lower.tail = !lower.tail, log.p = log.p)
}




#' @rdname invchisq
#' @export
qinvchisq <- function(p, df, ncp = 0, lower.tail = TRUE, log.p = FALSE) {
  if (log.p) {
    qchisq(  p, df, ncp, lower.tail = !lower.tail, log.p = TRUE)^(-1)
  } else {
    qchisq(1-p, df, ncp, lower.tail = lower.tail, log.p = FALSE)^(-1)
  }
}





#' @rdname invchisq
#' @export
rinvchisq <- function(n, df, ncp = 0) {
  if (df <= .01 && ncp <= 10) {
    warning("`rinvchisq()` is unreliable for `df` <= .01 and `ncp` <= 10.",
            call. = FALSE, immediate. = TRUE)
  }
  1 / rchisq(n, df, ncp)
}

















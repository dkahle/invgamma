#' The Inverse (non-central) Chi-Squared Distribution
#'
#' Density, distribution function, quantile function and random
#' generation for the inverse chi-squared distribution.
#'
#' The functions (d/p/q/r)invchisq simply wrap those of the standard
#' (d/p/q/r)chisq R implementation, so look at, say,
#' \code{\link{dchisq}} for details.
#'
#'
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations. If length(n) > 1, the length is
#'   taken to be the number required.
#' @param df degrees of freedom (non-negative, but can be
#'   non-integer).
#' @param ncp non-centrality parameter (non-negative).
#' @param log,log.p logical; if TRUE, probabilities p are given as
#'   log(p).
#' @param lower.tail logical; if TRUE (default), probabilities are
#'   P[X <= x] otherwise, P[X > x].
#' @seealso \code{\link{dchisq}}; these functions just wrap the
#'   (d/p/q/r)chisq functions.
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
  qchisq(1-p, df, ncp, lower.tail = lower.tail, log.p = log.p)^(-1)
}





#' @rdname invchisq
#' @export
rinvchisq <- function(n, df, ncp = 0) {
  1 / rchisq(n, df, ncp)
}

















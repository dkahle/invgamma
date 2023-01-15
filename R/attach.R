.onAttach <- function(...) {
  if(!interactive()) return()
  packageStartupMessage(
    'Parameter names in this package may not be intuitive. Please read\n',
    'https://github.com/dkahle/invgamma/issues/1 before using this package.'
  )
}

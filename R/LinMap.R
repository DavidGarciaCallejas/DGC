#' Remap a vector to a determined range
#'
#' @param x vector to map
#' @param from starting point of new range
#' @param to end point of new range
#'
#' @return vector of length(x)
#' @export
#' 
LinMap <- function(x, from, to) {
  # Shifting the vector so that min(x) == 0
  x <- x - min(x)
  # Scaling to the range of [0, 1]
  x <- x / max(x)
  # Scaling to the needed amplitude
  x <- x * (to - from)
  # Shifting to the needed level
  x + from
}


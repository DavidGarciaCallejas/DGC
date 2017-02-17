#' Function for calculating the mode of a vector
#'
#' @param x vector af any length and type
#'
#' @return Most repeated value in the data. In case of ties, the first value is reported
#' @export
#'
#' @examples Mode(c("a","b","a","b"))
#'
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

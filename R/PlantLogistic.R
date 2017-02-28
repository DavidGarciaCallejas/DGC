#' Logistic function for plant responses
#' 
#' Different parameterizations may be used for plant survival or growth responses
#' to an arbitrary stress factor. Modelled following fig. 1 of Maestre et al. 2009
#'
#' parameter space for survival probability:
#' a = 1
#' b approaching 0
#' 0.1 < c < 0.98
#' 
#' parameter space for growth:
#' a = 1
#' a combination of b approaching 0 and c = 0.9 as a limit; 
#' b > 0.7 and c > 0.95 as the other limit
#' 
#' @param x response
#' @param a first parameter
#' @param b second parameter
#' @param c third parameter
#'
#' @return logistic function
#' @export
#'
PlantLogistic <- function(x,a,b,c){
  a/(1+b*c^-x)
}



#' Interaction Frequency of a pairwise interaction
#' 
#' Interaction frequency calculated from net abundances, as a logistic function
#' with an asymptote at the lowest abundance of the two
#'
#' @param n1 abundance of the first species
#' @param n2 abundance of the second species
#' @param a parameter of the logistic function. A higher a implies faster approach to the asymptote
#' @param x0 parameter of the logistic function
#' @param threshold threshold below which an abundance is considered zero
#'
#' @return value, positive and non-integer, of interaction frequency
#' @export
#'
#' @examples InteractionFrequency(n1 = 10, n2 = 100, a = 0.1, x0 = 1, threshold = 1e-5)
#' InteractionFrequency(n1 = 10, n2 = 100, a = 0.01, x0 = 1, threshold = 1e-5)
InteractionFrequency <- function(n1,n2,a,x0,threshold){
  if(!is.null(n1) & !(is.null(n2)) & !is.na(n1) & !is.na(n2) & n1 > threshold & n2 > threshold & a != 0){
    (min(n1,n2)*(1/(1+exp(-a*(max(n1,n2) - x0)))))
  }else{
    return(0)
  }
  
}

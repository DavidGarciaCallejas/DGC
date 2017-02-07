

#' GeneratePerturbation
#' 
#' Generate a perturbation of a certain length in a time series, given by values in a vector
#'
#' @param perturbation.type values: "white", "pulse", "incremental"
#' @param timesteps length of the time series
#' @param perturbation.mean mean value
#' @param perturbation.sd standard deviation
#' @param perturbation.prob for perturbation type = pulse, probability of observing a perturbation
#' @param perturbation.max for perturbation type = incremental, maximum value for the perturbation. If 0, no maximum
#' @param perturbation.increment for perturbation type = incremental, increment in MEAN at each timestep
#'
#' @return numeric vector of given length
#' @export
#'
#' @examples GeneratePerturbation(perturbation.type = "pulse",timesteps = 100,perturbation.mean = 1,perturbation.sd = 3,perturbation.prob = 0.2)
GeneratePerturbation <- function(perturbation.type = "white",
                                 timesteps = 10,
                                 perturbation.mean = 0,
                                 perturbation.sd = 1,
                                 perturbation.prob = NULL, 
                                 perturbation.max = NULL, 
                                 perturbation.increment = NULL){
  
  if(is.null(perturbation.type) | is.null(timesteps) | is.null(perturbation.mean) | is.null(perturbation.sd)){
    stop("*** function GeneratePerturbation: provide valid arguments")
  }
  
  if(perturbation.type == "white"){
    my.perturbation <- rnorm(n = timesteps,mean = perturbation.mean,sd = perturbation.sd)
  }else if(perturbation.type == "pulse"){
    
    if(is.null(perturbation.prob)){
      stop("*** function GeneratePerturbation: provide valid arguments")
    }
    
    my.perturbation <- numeric(timesteps)
    my.perturbation <- ifelse(runif(n = timesteps,min = 0,max = 1) < perturbation.prob,rnorm(n = timesteps,mean = perturbation.mean,sd = perturbation.sd),0)
  }else if(perturbation.type == "incremental"){
    
    if(is.null(perturbation.max) | is.null(perturbation.increment)){
      stop("*** function GeneratePerturbation: provide valid arguments")
    }
    
    my.perturbation <- sapply(1:timesteps, function(x)rnorm(1,perturbation.mean + x*perturbation.increment,perturbation.sd))
    my.perturbation <- ifelse(my.perturbation > perturbation.max,perturbation.max,my.perturbation)
  }# if-else
  
  return(my.perturbation)
}



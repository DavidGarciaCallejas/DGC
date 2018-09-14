#' Sample positions from a given probability density distribution
#'
#' this function randomly selects "times" positions in a vector according to a given distribution, 
#' either "power-law" or provided in the column "my.data$prob"
#' It accepts a power-law distribution:
#' y = c*x^k
#' this parameterization is consistent with Hatton et al. 2015, 
#' who found a general scaling of k ~ 0.75 por prey and predator biomass across ecosystems globally
#' hence the default k = 0.75.
#' it also accepts a lognormal distribution with logmean and logsd
#' and a gambin distribution with parameters gambin.alpha and gambin.maxoctave
#'
#' @param times number of positions to be selected
#' @param choice.length length of the vector to select positions from
#' @param dist either "uniform", "power-law", "lognormal" or "gambin"
#' @param c for the power law parametrization
#' @param k exponent of the power law
#' @param logmean mean of the lognormal distribution
#' @param logsd standard deviation of the lognormal distribution
#' @param gambin.alpha alpha value of the gambin distribution
#' @param gambin.maxoctave value of the maximum octave of the gambin distribution -TODO-
#' @param cum.sum cumulative sum of the returning values
#' @param my.data dataframe specifying the distribution to sample from, instead of dist. Of the form my.data$prob
#' 
#' @return numeric vector of length times
#'
#' @author David García-Callejas, \email{david.garcia.callejas@@gmail.com}
#'
#' @examples
#' t1 <- GenerateProbNumbers(100,c = 1, k = 0.75,cum.sum = 100)
#' hist(t1)
#' table(t1)
#' t2 <- GenerateProbNumbers(times = 2000, k = 0.25, cum.sum = 10000)
#' t3 <- GenerateProbNumbers(times = 80, choice.length = 100, dist = "gambin", gambin.alpha = 2, gambin.maxoctave = 8, cum.sum = 10000)
#' hist(t2)
#' hist(t3)
#'
#'@importFrom gambin gambin_exp
#
#' @export


GenerateProbNumbers <- function(times,
                                choice.length = 100,
                                dist = "power-law",
                                c = 1,
                                k = 0.75,
                                logmean = 0,
                                logsd = 0.5,
                                gambin.alpha = 2,
                                gambin.maxoctave = 8,
                                cum.sum = 0,
                                my.data = NULL){
  if(is.null(my.data)){
    x <- seq(1,10,length.out = choice.length)
    
    # generate data from a probability density function
    
    if(dist == "power-law"){
      # I am not interested in the relationship between predator biomass (y-axis) and prey biomass (x-axis), 
      # but in the resulting biomass (y-axis) per trophic level or species (x-axis)
      # therefore the negative sign
      prob_vec <- plfunc.2(x,c,-k)
      
      # make the distribution sum to 1 and generate the cumulative distribution
      cum_prob <- cumsum(prob_vec/sum(prob_vec))
      
      # sample positions from the distribution
      choice_vec <- numeric(times)
      for(i.times in 1:times){
        test <- runif(1,0,1)
        i.test <- 1
        while(i.test <= length(cum_prob) & choice_vec[i.times] == 0){
          if(test <= cum_prob[i.test]){
            choice_vec[i.times] <- i.test
          }
          i.test <- i.test + 1
        }
      }# for i.times
      
    }else if(dist == "lognormal"){
      choice_vec <- rlnorm(times,logmean,logsd)
    }else if(dist == "uniform"){
      choice_vec <- runif(times)
    }else if(dist == "gambin"){
      # expected <- gambin::gambin_exp(alpha = gambin.alpha, maxoctave = gambin.maxoctave, total_species = times)
      # expected <- round(expected)
      
      # the behaviour of gambin_exp has changed. Use dgambin:
      expected <- round(gambin::dgambin(0:gambin.maxoctave,alpha = 2,maxoctave = 8) * times)
      
      if(sum(expected) != times){
        difference <- abs(times - sum(expected))
        for(i.diff in 1:difference){
          position <- sample(x = 1:length(expected),size = 1)
          expected[position] <- ifelse(sum(expected) < times, expected[position] + 1, expected[position] - 1)
        }# for
      }# if
      
      choice_vec <- numeric(times)
      
      my.sp <- 1
      # for each octave
      for(i.octave in 1:length(expected)){
        # if there are species in this octave
        if(expected[i.octave] > 0){
          # for each sp in this octave
          for(i.sp in 1:expected[i.octave]){
            choice_vec[my.sp] <- round(runif(1,2^i.octave,(2^(i.octave+1))-1))
            my.sp <- my.sp + 1
          }# for i.sp
        }# if
      }# for i.octave
    }else{
      stop("**** GenerateProbNumbers: specify an underlying distribution (power-law,lognormal,gambin or uniform)")
    }
    
  }else if(!is.null(my.data)){
    cum_prob <- cumsum(my.data$prob/sum(my.data$prob))
    
    # sample positions from the distribution
    choice_vec <- numeric(times)
    for(i.times in 1:times){
      test <- runif(1,0,1)
      i.test <- 1
      while(i.test <= length(cum_prob) & choice_vec[i.times] == 0){
        if(test <= cum_prob[i.test]){
          choice_vec[i.times] <- i.test
        }
        i.test <- i.test + 1
      }
    }# for i.times
  }
  
  if(cum.sum != 0){
    choice_vec <- (choice_vec/sum(choice_vec))*cum.sum + rnorm(length(choice_vec),mean = 0,sd = choice_vec/10)
  }else{
    choice_vec
  }
}

#' Power law sampling
#'
#' @param x base
#' @param c parameter
#' @param k exponent
#'
#' @return value of (x*c)^k
#' @export
#'
#' @examples plfunc.2(10,1,0.75)
plfunc.2 <- function(x,c,k){
  (c*x)^k
}


#' Assign trophic levels and abundances to a set of species
#'
#' Assigns a trophic level and abundance to a given number of species, constrained by ecologicaly plausible assumptions
#'
#' @param num.sp number of species
#' @param trophic.levels number of integer trophic levels in the community
#' @param abundance.distribution S.A.D. of the community, either "uniform", "power-law", "lognormal" or "gambin"
#' @param scaling.law.tl whether abundances scale with trophic level following a power law
#' @param scaling.exponent.tl exponent of the power law trophic level abundance scaling
#' @param basal.abundance abundance of the basal trophic level
#' @param ... additional parameters to specify the S.A.D. (see \code{\link{GenerateProbNumbers}})
#' @return dataframe containing species integer ID, abundances and trophic levels
#'
#' @author David García-Callejas, \email{david.garcia.callejas@@gmail.com}
#'
#' @examples
#' abundance.list <- AssignTrophicLevel(num.sp = 20, 
#' trophic.levels = 4, abundance.distribution = "gambin", scaling.law.tl = TRUE, scaling.exponent.tl = 0.75, 
#' basal.abundance = 1000, gambin.alpha = 2, gambin.maxoctave = 8)
#'
#' @export

AssignTrophicLevel <- function(num.sp, 
                               trophic.levels,
                               abundance.distribution = "power-law",
                               # scaling.exponent.abund = 0.75,
                               scaling.law.tl = F, 
                               scaling.exponent.tl = 0,
                               basal.abundance = 1000,...){
  
  if(scaling.law.tl){
    
    trophic.level.abundance <- numeric(trophic.levels)
    trophic.level.abundance[1] <- basal.abundance
    
    # calculate abundance for each trophic level
    # including a white noise term with mean = 0 and sd = abundance/10
    if(length(trophic.level.abundance) > 1){
      for(i.trophic.level in 2:trophic.levels){
        trophic.level.abundance[i.trophic.level] <- trophic.level.abundance[i.trophic.level - 1]^scaling.exponent.tl #abundance(biomass = trophic.level.biomass[i.trophic.level],c = scaling.constant.biomass,k = scaling.exponent.biomass)
        trophic.level.abundance[i.trophic.level] <- trophic.level.abundance[i.trophic.level] + rnorm(n = 1,mean = 0,sd = trophic.level.abundance[i.trophic.level]/10)
      }
    }
    # generate abundances; be aware of passing the right arguments to GenerateProbNumbers
    abundance.list <- GenerateProbNumbers(times = num.sp,dist = abundance.distribution,cum.sum = sum(trophic.level.abundance),...)
    # abundance.list <- GenerateProbNumbers(times = num.sp,dist = abundance.distribution,cum.sum = sum(trophic.level.abundance),gambin.alpha, gambin.maxoctave)
    
    # create the dataframe
    tl.results <- data.frame(species = c(1:length(abundance.list)), abundance = sort(abundance.list,decreasing = T), trophic.level = 0)
    
    # the most abundant species will always be at the basal level
    tl.results$trophic.level[1] <- 1
    
    for(i.tl in trophic.levels:1){
      
      # iterative process: add species until a trophic level is "filled"
      # for convenience I start in the upper trophic level, since it will be the most difficult to fill (few species will have such low abundances)
      
      if(i.tl == 1){
        my.abund <- tl.results$abundance[1]
      }else{
        my.abund <- 0
      }
      
      # certain tolerance, it does not need to be exact
      tl.tolerance <- trophic.level.abundance[i.tl]*0.1
      
      my.sp <- 0
      
      # while the abundance of the i.tl trophic level is not within the tolerance levels, keep adding species
      while(length(my.sp) != 0 & findInterval(my.abund,c(trophic.level.abundance[i.tl] - tl.tolerance,trophic.level.abundance[i.tl] + tl.tolerance)) != 1){
        
        # potential species
        my.sp <- which(tl.results$trophic.level == 0 & tl.results$abundance + my.abund < (trophic.level.abundance[i.tl] + tl.tolerance))
        
        # sample from the potential species pool
        if(length(my.sp)>0){
          my.sp <- ifelse(length(my.sp) == 1,my.sp,sample(my.sp,size = 1))
          my.abund <- my.abund + tl.results$abundance[tl.results$species == my.sp]
          tl.results$trophic.level[tl.results$species == my.sp] <- i.tl
        }# if
      }# while
      
    }# for i.tl
    
    # if any species remains unassigned, randomly assign it to the first or second trophic levels
    if(sum(tl.results$trophic.level == 0) > 0){
      tl.results$trophic.level[tl.results$trophic.level == 0] <- sample(1:2,size = sum(tl.results$trophic.level == 0),replace = T)
    }
    
  }else{
    # generate abundances; be aware of passing the right arguments to GenerateProbNumbers
    
    # here there is no abundance scaling: we assume that overall community abundance of every trophic level is more or less similar
    trophic.level.abundance <- basal.abundance * trophic.levels
    
    abundance.list <- GenerateProbNumbers(times = num.sp,dist = abundance.distribution,cum.sum = trophic.level.abundance,...)
    tl.results <- data.frame(species = c(1:length(abundance.list)), abundance = sort(abundance.list,decreasing = T), trophic.level = 0)
    
    # most abundant species is always a primary producer
    tl.results$trophic.level[1] <- 1
    # random assignment of trophic level
    tl.results$trophic.level[2:num.sp] <- sample(x = 1:trophic.levels,size = length(abundance.list)-1,replace = T)
    
  }# if-else scaling law
  
  # return the dataframe
  tl.results
}
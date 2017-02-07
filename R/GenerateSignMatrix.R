#' Generate sign matrix
#'
#' @param tl.results community dataframe from \code{\link{AssignTrophicLevel}}
#' @param connectance C = L/Sp, where Sp is the potential connectance considering the assumptions.
#' it also accepts a vector of specific connectances, 
#' in which case no interaction probabilities are used
#' @param interaction.probs dataframe with 5 rows: competition, amensalism, antagonism, mutualism, commensalism
#' and a column "prob" with the percentage for each interaction
#' @param WithinTypeProb data frame with columns 'interaction type', 'tl.category' and 'prob'
#' where interaction type is 'amensalism','antagonism', 'commensalism, 'competition' or 'mutualism';
#' tl.category is 'same', 'adjacent', or 'other'. An example is included in the package.
#' and prob is the probability of occurrence of each combination of interaction and trophic levels of the species involved.
#' @param verbose detailed output
#'
#' @return Integer matrix of size NxN 
#' @export
#'
GenerateSignMatrix <- function(tl.results, connectance = NULL, interaction.probs = NULL, WithinTypeProb, verbose = F){
  
  thesaurus <- data.frame(index = 1:5, type = c("competition","amensalism","antagonism","mutualism","commensalism"))
  
  num.sp <- nrow(tl.results)
  
  sign.matrix <- mat.or.vec(nr = num.sp,nc = num.sp)
  
  # how many links?
  # differentiate potential links by interaction type
  # refer to the notebook for some notes about the overall connectance of a network,
  # the connectance of each subnetwork, etc.
  
  # pool of potentially feasible links from which to sample
  if(missing(WithinTypeProb)){
    stop("*** function GenerateSignMatrix: please specify WithinTypeProb argument")
  }else{
    potential.links <- PotentialLinks(tl.results, WithinTypeProb = WithinTypeProb, return.type = "positions")
  }
  # maximum number of realized links per interaction type
  max.links <- MaxLinks(tl.results = tl.results,potential.links = potential.links)
  
  # there is the option that interaction.probs gives a fixed probability of 1
  # for a certain type, i.e. forces a single-interaction network
  # while, on the other hand, there are no potential links of that type in the community.
  # In that case, return a zero matrix
  
  unique.type <- which(interaction.probs$prob == 1)
  if(length(unique.type) == 1){
    if(!(thesaurus$type[thesaurus$index == unique.type] %in% potential.links$type)){
      return(sign.matrix)
    }
  }
  
  # the net number of links per interaction type can be computed
  # either from type-specific connectances or interaction type probabilities + overall connectance
  
  if(length(connectance) == 1){
    
    # previously:
    # C = L/Sp * Sp/(S(S-1)/2)
    # L = Sp * C * (S(S-1)/2)/Sp
    
    # now:
    # C = L/Sp -> L = Sp * C
    
    # in this case, if a single value of connectance was provided,
    # I assume the potential link space is a fully connected network
    # hence I keep the S(S-1)/2
    
    num.links <- round(num.sp*(num.sp - 1)/2 * connectance)
    
    if(num.links > 0){
      # for each link
      for(i.link in 1:num.links){
        
        #######
        #######
        # randomly select an interaction type according to the probabilities given
        
        my.interaction.type <- GenerateProbNumbers(times = 1,choice.length = 5, my.data = interaction.probs) 
        if(my.interaction.type == 1){
          my.interaction.type <- "competition"
        }else if(my.interaction.type == 2){
          my.interaction.type <- "amensalism"
        }else if(my.interaction.type == 3){
          my.interaction.type <- "antagonism"
        }else if(my.interaction.type == 4){
          my.interaction.type <- "mutualism"
        }else if(my.interaction.type == 5){
          my.interaction.type <- "commensalism"
        }
        
        # it might be the case that I run out of interactions of a certain type
        # in that case, keep sampling.
        # It will affect the realized interaction type ratios, but the other option is
        # to stop creating links
        
        while(sum(potential.links$type == my.interaction.type) == 0){
          my.interaction.type <- GenerateProbNumbers(times = 1,choice.length = 5, my.data = interaction.probs) 
          if(my.interaction.type == 1){
            my.interaction.type <- "competition"
          }else if(my.interaction.type == 2){
            my.interaction.type <- "amensalism"
          }else if(my.interaction.type == 3){
            my.interaction.type <- "antagonism"
          }else if(my.interaction.type == 4){
            my.interaction.type <- "mutualism"
          }else if(my.interaction.type == 5){
            my.interaction.type <- "commensalism"
          }
        }
        
        #######
        #######
        # select a specific link
        
        # the selection of a specific link of the desired type is not random,
        # rather it is selected according to the probabilities defined in the WithinTypeProb dataframe
        # that specify the probability that a certain interaction will involve the same, adjacent or other 
        # trophic levels
        
        # hence, first, select the trophic levels involved
        
        my.tl.involved <- GenerateProbNumbers(times = 1,my.data = WithinTypeProb[WithinTypeProb$interaction.type == my.interaction.type,])
        my.tl.involved <- switch(my.tl.involved, "1"="same", "2"="adjacent","3"="other")
        
        # make sure that there are links available of that trophic level arrangement. Otherwise, choose another
        while(sum(potential.links$tl.category == my.tl.involved & potential.links$type == my.interaction.type) == 0){
          my.tl.involved <- GenerateProbNumbers(times = 1,my.data = WithinTypeProb[WithinTypeProb$interaction.type == my.interaction.type,])
          my.tl.involved <- switch(my.tl.involved, "1"="same", "2"="adjacent","3"="other")
        }
        
        # afterwards, random sampling for the specific link
        my.index <- which(potential.links$type == my.interaction.type & potential.links$tl.category == my.tl.involved)
        
        my.link <- potential.links[resample(which(potential.links$type == my.interaction.type & potential.links$tl.category == my.tl.involved),size = 1),]
        
        # this is equivalent to the following two lines, because in "potential.links" the column "WithinTypeProb"
        # already yields the probability of that interaction to be selected, given a certain type
        
        # names(potential.links)[5] <- "prob"
        # my.link <- GenerateProbNumbers(times = 1, my.data = potential.links[potential.links$type == my.interaction.type,])
        
        #######
        #######
        # add the link to the interaction matrix. Also,
        # as selecting this specific interaction discards another potential ones, update the potential.links dataframe
        
        if(my.interaction.type == "competition"){
          sign.matrix[my.link$sp.a,my.link$sp.b] <- -1
          sign.matrix[my.link$sp.b,my.link$sp.a] <- -1
        }else if(my.interaction.type == "amensalism"){
          sign.matrix[my.link$sp.a,my.link$sp.b] <- -1
          sign.matrix[my.link$sp.b,my.link$sp.a] <- 0
        }else if(my.interaction.type == "antagonism"){
          sign.matrix[my.link$sp.a,my.link$sp.b] <- 1
          sign.matrix[my.link$sp.b,my.link$sp.a] <- -1
        }else if(my.interaction.type == "mutualism"){
          sign.matrix[my.link$sp.a,my.link$sp.b] <- 1
          sign.matrix[my.link$sp.b,my.link$sp.a] <- 1
        }else if(my.interaction.type == "commensalism"){
          sign.matrix[my.link$sp.a,my.link$sp.b] <- 1
          sign.matrix[my.link$sp.b,my.link$sp.a] <- 0
        }
        
        to.remove <- which((potential.links$sp.a == my.link$sp.a & potential.links$sp.b == my.link$sp.b) | 
                             (potential.links$sp.a == my.link$sp.b & potential.links$sp.b == my.link$sp.a))
        potential.links <- potential.links[-to.remove,]
        
      }# for i.link
    }# if num.links > 0
  }else{
    
    remaining.links <- round(connectance * max.links)
    
    # is it possible to allocate one more link?
    possible.links <- rep(T,5)
    
    if(verbose){
      print(paste("**** number of links to be assigned - potential links available:",sep=""))
      for(i.type in 1:nrow(thesaurus)){
        print(paste(thesaurus$type[i.type],": ",remaining.links[i.type], " - ", sum(potential.links$type == thesaurus$type[i.type]),sep=""))
      }
      print("****")
    }
    
    while(sum(possible.links & remaining.links > 0) > 0){
      
      potential.types <- which(remaining.links > 0 & possible.links)
      
      # if potential.types has length 1, sample
      # note that here no probabilities are used, 
      # since only connectance values were specified
      
      # sample function does not work if the sampling space is of length 1
      if(length(potential.types)>1){
        my.interaction.type.index <- sample(potential.types,size = 1) #GenerateProbNumbers(times = 1,choice.length = 5, my.data = interaction.probs) 
      }else{
        my.interaction.type.index <- potential.types
      }
      
      my.interaction.type <- thesaurus$type[thesaurus$index == my.interaction.type.index]
      
      # for selecting the specific link, I do use the probabilities associated for the different trophic levels
      
      # the selection of a specific link of the desired type is not random,
      # rather it is selected according to the probabilities defined in the WithinTypeProb dataframe
      # that specify the probability that a certain interaction will involve the same, adjacent or other 
      # trophic levels
      
      # hence, first, select the trophic levels involved
      
      my.tl.involved <- GenerateProbNumbers(times = 1,my.data = WithinTypeProb[WithinTypeProb$interaction.type == my.interaction.type,])
      my.tl.involved <- switch(my.tl.involved, "1"="same", "2"="adjacent","3"="other")
      
      # make sure that there are links available of that trophic level arrangement. Otherwise, choose another
      while(sum(potential.links$tl.category == my.tl.involved & potential.links$type == my.interaction.type) == 0){
        my.tl.involved <- GenerateProbNumbers(times = 1,my.data = WithinTypeProb[WithinTypeProb$interaction.type == my.interaction.type,])
        my.tl.involved <- switch(my.tl.involved, "1"="same", "2"="adjacent","3"="other")
      }
      
      # afterwards, random sampling for the specific link
      
      my.link <- potential.links[sample(which(potential.links$type == my.interaction.type & potential.links$tl.category == my.tl.involved),size = 1),]
      
      # add the link to the interaction matrix. Also,
      # as selecting this specific interaction discards another potential ones, update the potential.links dataframe
      
      if(my.interaction.type == "competition"){
        sign.matrix[my.link$sp.a,my.link$sp.b] <- -1
        sign.matrix[my.link$sp.b,my.link$sp.a] <- -1
      }else if(my.interaction.type == "amensalism"){
        sign.matrix[my.link$sp.a,my.link$sp.b] <- -1
        sign.matrix[my.link$sp.b,my.link$sp.a] <- 0
      }else if(my.interaction.type == "antagonism"){
        sign.matrix[my.link$sp.a,my.link$sp.b] <- 1
        sign.matrix[my.link$sp.b,my.link$sp.a] <- -1
      }else if(my.interaction.type == "mutualism"){
        sign.matrix[my.link$sp.a,my.link$sp.b] <- 1
        sign.matrix[my.link$sp.b,my.link$sp.a] <- 1
      }else if(my.interaction.type == "commensalism"){
        sign.matrix[my.link$sp.a,my.link$sp.b] <- 1
        sign.matrix[my.link$sp.b,my.link$sp.a] <- 0
      }
      
      to.remove <- which((potential.links$sp.a == my.link$sp.a & potential.links$sp.b == my.link$sp.b) | 
                           (potential.links$sp.a == my.link$sp.b & potential.links$sp.b == my.link$sp.a))
      potential.links <- potential.links[-to.remove,]
      
      for(i.type in 1:nrow(thesaurus)){
        if(sum(potential.links$type == thesaurus$type[i.type]) == 0){
          possible.links[thesaurus$index[i.type]] <- F
        }
      }
      
      remaining.links[my.interaction.type.index] <- remaining.links[my.interaction.type.index] - 1
      
      if(verbose){
        print(paste("**** link succesfully assigned of type: ",my.interaction.type,sep=""))
        print(paste("**** number of links removed: ",length(to.remove),sep=""))
        print(paste("**** remaining links to be assigned - potential links still available:",sep=""))
        for(i.type in 1:nrow(thesaurus)){
          print(paste(thesaurus$type[i.type],": ",remaining.links[i.type], " - ", sum(potential.links$type == thesaurus$type[i.type]),sep=""))
        }
        print("****")
      }
      
      #       print(paste("my.interaction.type:", my.interaction.type,sep=""))
      #       print(paste("remaining.links-possible.links... competition: ",remaining.links[1],"-",possible.links[1],
      #                   ", amensalism: ",remaining.links[2],"-",possible.links[2],
      #                   ", antagonism: ",remaining.links[3],"-",possible.links[3],
      #                   ", mutualism: ",remaining.links[4],"-",possible.links[4],
      #                   ", commensalism: ",remaining.links[5],"-",possible.links[5],sep=""))
    }# while
    
  }# if-else length(connectance) == 1
  
  sign.matrix
  
}
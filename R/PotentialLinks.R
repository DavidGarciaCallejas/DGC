#' Assign trophic level category
#'
#' Assign trophic level categorization of a pairwise interaction
#'
#' @param tl.a trophic level of first species
#' @param tl.b trophic level of second species
#'
#' @return character with potential values "same", "adjacent", or "other". Other represents species separated by >= 1 trophic level
#' @export
#'
#' @examples assign.tl.category(1,3)
#' 
assign.tl.category <- function(tl.a,tl.b){
  my.value <- NULL
  ifelse(abs(tl.a - tl.b) == 0, my.value <- "same", ifelse(abs(tl.a - tl.b) == 1, my.value <- "adjacent", my.value <- "other"))
  return(my.value)
}

#' Calculate the number of potential links on a given community
#'
#' Potential links are calculated based on a community given by \code{AssignTrophicLevel}
#' it may return a dataframe with the positions of every potential link ("positions)
#' or a vector with the number of potential links for each interaction type ("number")
#' 
#' IMPORTANT NOTE: this is not the same as the potential number of realized links at one time, the potential connectance.
#' For two sp A and B and an amensalist relationship, both A->B and B->A are potentially feasible, but also mutually exclusive:
#' the realization of one of them prevents the another from being realized.
#'
#' @param tl.results a dataframe as given by \code{AssignTrophicLevel}
#' @param within.type.prob a dataframe with columns "interaction.type" (amensalism, antagonism, competition, commensalism, mutualism),
#'  "tl.category" (same, adjacent, other), "prob" (probability of occurrence for each combination of interaction type and trophic level categorization)
#' @param return.type specify the return type of the function. Either a dataframe with the positions of every potential link ("positions)
# or a vector with the number of potential links for each interaction type ("number")
#'
#' @return see return.type
#' @export
#'
#' @examples  tl.results <- AssignTrophicLevel(num.sp = 20,
#'                                  trophic.levels = 4,
#'                                  abundance.distribution = "gambin",
#'                                  scaling.law.tl = T,
#'                                  scaling.exponent.tl = 0.75,
#'                                  basal.abundance = 1000,
#'                                  gambin.alpha = 2,
#'                                  gambin.maxoctave = 8)
#' 
#' within.type.prob <- data.frame(interaction.type = c(rep("competition",3),rep("amensalism",3),rep("antagonism",3),rep("mutualism",3),rep("commensalism",3)),
#'                    tl.category = rep(c("same","adjacent","other"),5),
#'                    prob = rep(0.33,15))
#' within.type.prob$prob[within.type.prob$interaction.type == "antagonism" & within.type.prob$tl.category == "same"] <- 0
#' within.type.prob$prob[within.type.prob$interaction.type == "antagonism" & within.type.prob$tl.category == "adjacent"] <- 0.75
#' within.type.prob$prob[within.type.prob$interaction.type == "antagonism" & within.type.prob$tl.category == "other"] <- 0.25
#' potential.links <- PotentialLinks(tl.results,within.type.prob)
#' 
PotentialLinks <- function(tl.results, within.type.prob, return.type = "positions"){
  
  if(missing(within.type.prob) | missing(tl.results)){
    stop("*** function PotentialLinks: please specify valid arguments")
  }else{ # if "within.type.prob" is present
    
    # the sp.a column refers to the rows of the interaction matrix, sp.b to the columns
    # so that an antagonistic interaction on sp.a = 4 and sp.b = 3 means that
    # it benefits species 4 and is detrimental to species 3.
    num.sp <- nrow(tl.results)
    potential.links <- data.frame(sp.a = sort(rep(tl.results$species,num.sp*5)), 
                                  sp.b = rep(sort(rep(tl.results$species,5)),num.sp),
                                  tl.category = "same",
                                  type = rep(c("competition", "amensalism", "antagonism", "mutualism", "commensalism"),num.sp^2),
                                  within.type.prob = 0,
                                  stringsAsFactors = F)
    
    # 1 - no self-links
    
    potential.links <- subset(potential.links, sp.a != sp.b)
    
    # 2 - for each link and type, assign link category (same, adjacent, other)
    potential.links$tl.sp.a <- tl.results$trophic.level[match(potential.links$sp.a,tl.results$species)]
    potential.links$tl.sp.b <- tl.results$trophic.level[match(potential.links$sp.b,tl.results$species)]
    
    potential.links$tl.category <- mapply(FUN = assign.tl.category,potential.links$tl.sp.a,potential.links$tl.sp.b)
    
    # 3 - assign probability of ocurrence given its interaction type and trophic levels involved
    interaction.type <- c("competition","amensalism","antagonism","mutualism","commensalism")
    potential.links <- merge(potential.links,within.type.prob,by.x = c("tl.category","type"),by.y = c("tl.category","interaction.type"),all = T)
    potential.links <- potential.links[,c("sp.a","sp.b","tl.category","type","prob","tl.sp.a","tl.sp.b")]
    potential.links <- arrange(potential.links,sp.a,sp.b)
    names(potential.links)[5] <- "within.type.prob"
    
    potential.links <- potential.links[complete.cases(potential.links),]
    
    ##### here is the key: keep only those links with probability > 0
    potential.links <- subset(potential.links, within.type.prob > 0)
    
    ##### do not allow antagonisms in which the species at the lower trophic level benefits
    upward.antagonism <- which(potential.links$tl.sp.a < potential.links$tl.sp.b & potential.links$type == "antagonism")
    if(length(upward.antagonism) > 0){
      potential.links <- potential.links[-upward.antagonism,]
    }
    
    for(i.type in 1:length(interaction.type)){
      num.same <- sum(potential.links$type == interaction.type[i.type] & 
                        potential.links$tl.category == "same")
      potential.links$within.type.prob[potential.links$type == interaction.type[i.type] & 
                                         potential.links$tl.category == "same"] <- potential.links$within.type.prob[potential.links$type == interaction.type[i.type] & 
                                                                                                                      potential.links$tl.category == "same"]/num.same
      num.adjacent <- sum(potential.links$type == interaction.type[i.type] & 
                            potential.links$tl.category == "adjacent")
      potential.links$within.type.prob[potential.links$type == interaction.type[i.type] & 
                                         potential.links$tl.category == "adjacent"] <- potential.links$within.type.prob[potential.links$type == interaction.type[i.type] & 
                                                                                                                          potential.links$tl.category == "adjacent"]/num.adjacent
      num.other <- sum(potential.links$type == interaction.type[i.type] & 
                         potential.links$tl.category == "other")
      potential.links$within.type.prob[potential.links$type == interaction.type[i.type] & 
                                         potential.links$tl.category == "other"] <- potential.links$within.type.prob[potential.links$type == interaction.type[i.type] & 
                                                                                                                       potential.links$tl.category == "other"]/num.other
    }
    # clean it up
    # potential.links <- potential.links[c("sp.a","sp.b","type","within.type.prob")]
    
    if(return.type == "positions"){
      potential.links
    }else{
      number.links <- c(competition = sum(potential.links$type == "competition"), 
                        amensalism = sum(potential.links$type == "amensalism"),
                        antagonism = sum(potential.links$type == "antagonism"),
                        mutualism = sum(potential.links$type == "mutualism"),
                        commensalism = sum(potential.links$type == "commensalism"))
      number.links
    }# if-else
    
  }# if all arguments are present
  
}

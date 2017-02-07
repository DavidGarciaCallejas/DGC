
#' Maximum number of links for every interaction type
#'
#' Returns the maximum number of links that can be realized in a given community
#' per interaction type, i.e. if an interaction type was allowed to fill the entire link space
#'
#' @param tl.results a dataframe as given by \code{AssignTrophicLevel}
#' @param potential.links a dataframe as given by \code{PotentialLinks(...,return.type = "positions")}
#'
#' @return numeric vector of length 5
#' @export
#'
#' @examples tl.results <- AssignTrophicLevel(num.sp = 20,
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
#' max.links <- MaxLinks(tl.results,potential.links)
MaxLinks <- function(tl.results,potential.links){
  
  if(nrow(potential.links) > 0){
    
    # in principle, every potential link can be realized
    potential.links$valid <- T
    
    # for every interaction type, discard links from the potential ones
    # basically, if an interaction a->b is valid, it means b->a does not
    # need to be counted as well, because I consider a->b and b->a as part
    # of the same link, not two different links
    
    for(i.link in 1:nrow(potential.links)){
      if(potential.links$valid[i.link]){
        my.index <- which(potential.links$sp.a == potential.links$sp.b[i.link] &
                            potential.links$sp.b == potential.links$sp.a[i.link] & 
                            potential.links$type == potential.links$type[i.link])
        potential.links$valid[my.index] <- F
      }
    }# for i.link
    potential.links <- potential.links[potential.links$valid,]
    max.links <- c(competition = sum(potential.links$type == "competition"),
                   amensalism = sum(potential.links$type == "amensalism"),
                   antagonism = sum(potential.links$type == "antagonism"),
                   mutualism = sum(potential.links$type == "mutualism"),
                   commensalism = sum(potential.links$type == "commensalism"))
  }else{
    max.links <- rep(0,5)
  }
  max.links
}


#' Return probabilities of occurrence of different interaction types across trophic levels,
#' based either on predator-prey topologies (cascade and niche), the literature review
#' by García-Callejas et al. (2017) or random assignment of probabilities
#'
#' @param antagonistic topology of antagonistic interactions, either "cascade","niche" or "empirical"
#' @param other.int topology of other interaction types, either "random" or "empirical"
#'
#' @return data.frame interaction.probs with columns "interaction.type", "tl.category" and "prob"
#' @export
#' 
#' @examples GetInteractionsProb("empirical","empirical")
GetInteractionsProb <- function(antagonistic = "niche", other.int = "random"){
  interaction.probs <- data.frame(interaction.type = c(rep("competition",3),rep("amensalism",3),rep("antagonism",3),rep("mutualism",3),rep("commensalism",3)),
                                  tl.category = rep(c("same","adjacent","other"),5),
                                  prob = rep(0,15))
  if(antagonistic == "empirical"){
    # same trophic level
    interaction.probs$prob[interaction.probs$interaction.type == "antagonism" & interaction.probs$tl.category == "same"] <- 0.015
    # adjacent trophic levels
    interaction.probs$prob[interaction.probs$interaction.type == "antagonism" & interaction.probs$tl.category == "adjacent"] <- 0.918
    # other trophic levels
    interaction.probs$prob[interaction.probs$interaction.type == "antagonism" & interaction.probs$tl.category == "other"] <- 0.067
  }else if(antagonistic == "niche"){
    # same trophic level
    interaction.probs$prob[interaction.probs$interaction.type == "antagonism" & interaction.probs$tl.category == "same"] <- 0.015
    # adjacent trophic levels
    interaction.probs$prob[interaction.probs$interaction.type == "antagonism" & interaction.probs$tl.category == "adjacent"] <- 0.918
    # other trophic levels
    interaction.probs$prob[interaction.probs$interaction.type == "antagonism" & interaction.probs$tl.category == "other"] <- 0.067
  }else if(antagonistic == "cascade"){
    # same trophic level
    interaction.probs$prob[interaction.probs$interaction.type == "antagonism" & interaction.probs$tl.category == "same"] <- 0
    # adjacent trophic levels
    interaction.probs$prob[interaction.probs$interaction.type == "antagonism" & interaction.probs$tl.category == "adjacent"] <- 0.918
    # other trophic levels
    interaction.probs$prob[interaction.probs$interaction.type == "antagonism" & interaction.probs$tl.category == "other"] <- 0.082
  }# if-else antagonistic
  
  if(other.int == "random"){
    ### competition
    # same trophic level
    interaction.probs$prob[interaction.probs$interaction.type == "competition" & interaction.probs$tl.category == "same"] <- 0.333
    # adjacent trophic levels
    interaction.probs$prob[interaction.probs$interaction.type == "competition" & interaction.probs$tl.category == "adjacent"] <- 0.333
    # other trophic levels
    interaction.probs$prob[interaction.probs$interaction.type == "competition" & interaction.probs$tl.category == "other"] <- 0.333
    
    ### amensalism
    # same trophic level
    interaction.probs$prob[interaction.probs$interaction.type == "amensalism" & interaction.probs$tl.category == "same"] <- 0.333
    # adjacent trophic levels
    interaction.probs$prob[interaction.probs$interaction.type == "amensalism" & interaction.probs$tl.category == "adjacent"] <- 0.333
    # other trophic levels
    interaction.probs$prob[interaction.probs$interaction.type == "amensalism" & interaction.probs$tl.category == "other"] <- 0.333
    
    ### mutualism
    # same trophic level
    interaction.probs$prob[interaction.probs$interaction.type == "mutualism" & interaction.probs$tl.category == "same"] <- 0.333
    # adjacent trophic levels
    interaction.probs$prob[interaction.probs$interaction.type == "mutualism" & interaction.probs$tl.category == "adjacent"] <- 0.333
    # other trophic levels
    interaction.probs$prob[interaction.probs$interaction.type == "mutualism" & interaction.probs$tl.category == "other"] <- 0.333
    
    ### commensalism
    # same trophic level
    interaction.probs$prob[interaction.probs$interaction.type == "commensalism" & interaction.probs$tl.category == "same"] <- 0.333
    # adjacent trophic levels
    interaction.probs$prob[interaction.probs$interaction.type == "commensalism" & interaction.probs$tl.category == "adjacent"] <- 0.333
    # other trophic levels
    interaction.probs$prob[interaction.probs$interaction.type == "commensalism" & interaction.probs$tl.category == "other"] <- 0.333
  }else if(other.int == "empirical"){
    ### competition
    # same trophic level
    interaction.probs$prob[interaction.probs$interaction.type == "competition" & interaction.probs$tl.category == "same"] <- 0.333
    # adjacent trophic levels
    interaction.probs$prob[interaction.probs$interaction.type == "competition" & interaction.probs$tl.category == "adjacent"] <- 0.333
    # other trophic levels
    interaction.probs$prob[interaction.probs$interaction.type == "competition" & interaction.probs$tl.category == "other"] <- 0.333
    
    ### amensalism
    # same trophic level
    interaction.probs$prob[interaction.probs$interaction.type == "amensalism" & interaction.probs$tl.category == "same"] <- 0.333
    # adjacent trophic levels
    interaction.probs$prob[interaction.probs$interaction.type == "amensalism" & interaction.probs$tl.category == "adjacent"] <- 0.333
    # other trophic levels
    interaction.probs$prob[interaction.probs$interaction.type == "amensalism" & interaction.probs$tl.category == "other"] <- 0.333
    
    ### mutualism
    # same trophic level
    interaction.probs$prob[interaction.probs$interaction.type == "mutualism" & interaction.probs$tl.category == "same"] <- 0.333
    # adjacent trophic levels
    interaction.probs$prob[interaction.probs$interaction.type == "mutualism" & interaction.probs$tl.category == "adjacent"] <- 0.333
    # other trophic levels
    interaction.probs$prob[interaction.probs$interaction.type == "mutualism" & interaction.probs$tl.category == "other"] <- 0.333
    
    ### commensalism
    # same trophic level
    interaction.probs$prob[interaction.probs$interaction.type == "commensalism" & interaction.probs$tl.category == "same"] <- 0.333
    # adjacent trophic levels
    interaction.probs$prob[interaction.probs$interaction.type == "commensalism" & interaction.probs$tl.category == "adjacent"] <- 0.333
    # other trophic levels
    interaction.probs$prob[interaction.probs$interaction.type == "commensalism" & interaction.probs$tl.category == "other"] <- 0.333
  }# if-else other interactions
  
  interaction.probs
}

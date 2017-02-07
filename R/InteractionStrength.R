


#' Interaction strength of a pairwise interaction
#' 
#' Interaction strength calculated as the sign of the interaction multiplied by a scaling factor
#'
#' @param n1 Abundance of the first species
#' @param n2 Abundance of the second species
#' @param sign.matrix sign matrix of the community
#' @param sp1 index of the first species in the sign matrix
#' @param sp2 index of the second species in the sign matrix
#' @param scale.factor numeric vector with five elements, giving the maximum strength relative to
#' species' intrinsic growth rate. If e.g. the scale factor equals 1, a certain interaction
#' can have a strength of the same order of magnitude that the r parameter
#'
#' @return dimensionless interaction strength value
#' @export
#'
#' @examples InteractionStrength(n1 = 10,n2 = 100,
#' sign.matrix = matrix(data = c(0,-1,1,0),nrow = 2),sp1 = 1,sp2 = 2,scale.factor = rep(0.1,5))
InteractionStrength <- function(n1,n2,sign.matrix,sp1,sp2,scale.factor){
  
  per.capita.strength <- 0
  
  if(!is.null(n1) & !(is.null(n2))){
    if(!is.na(n1) & !is.na(n2)){
      
      if(n1 > 0 & n2 > 0){
        
        if(sign.matrix[sp1,sp2] != 0){
          
          if(sign.matrix[sp1,sp2] > 0){
            
            if(sign.matrix[sp2,sp1] == 0){
              current.interaction <- "commensalism"
            }else if(sign.matrix[sp2,sp1] < 0){
              current.interaction <- "positive.antagonism"
            }else if(sign.matrix[sp2,sp1] > 0){
              current.interaction <- "mutualism"
            }
            
          }else{ 
            
            if(sign.matrix[sp2,sp1] == 0){
              current.interaction <- "amensalism"
            }else if(sign.matrix[sp2,sp1] < 0){
              current.interaction <- "competition"
            }else if(sign.matrix[sp2,sp1] > 0){
              current.interaction <- "negative.antagonism"
            }
          }# if-else
          
          my.factor <- 0
          
          # for now, negative antagonisms are the ones with different scale.factor
          
          if(current.interaction == "competition"){
            my.factor <- scale.factor[1]
          }else if(current.interaction == "amensalism"){
            my.factor <- scale.factor[2]
          }else if(current.interaction == "negative.antagonism"){
            my.factor <- scale.factor[3]
          }else if(current.interaction == "positive.antagonism"){
            my.factor <- scale.factor[5] ########################### will change this ##################
          }else if(current.interaction == "mutualism"){
            my.factor <- scale.factor[4]
          }else if(current.interaction == "commensalism"){
            my.factor <- scale.factor[5]
          }
          
          #           per.capita.strength <- sign(sign.matrix[sp1,sp2]) * 1/n1 * my.factor
          per.capita.strength <- sign(sign.matrix[sp1,sp2]) * my.factor
          
        }# if sign.matrix[sp1,sp2] != 0. I don't consider sign.matrix[sp2,sp1] because this is only the effect of sp2 over sp1
      }# if != 0
    }# if !is.na
  }# if !is.null
  
  return(per.capita.strength)
  
}

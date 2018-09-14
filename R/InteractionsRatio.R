#' returns the ratio of interactions in a given square matrix
#'
#' @param interaction.matrix a square matrix
#' @param relative.to ratio can be relative to all interactions (option "all"), including "no interaction", 
#' or relative to all valid interactions (option "valid")
#'
#' @return a named vector with relative frequencies for all interaction types
#' @export
#'
#' @examples
InteractionsRatio <- function(interaction.matrix,relative.to = "all"){
  
  if(!(is.null(nrow(interaction.matrix)) | is.null(ncol(interaction.matrix)))){
    
    # in the same order as the vector of results
    
    num.competition <- 0
    num.amensalism <- 0
    num.antagonism <- 0
    num.mutualism <- 0
    num.commensalism <- 0
    
    num.no.interaction <- 0
    
    # go check every species pair, by looking at all the 
    # elements of the upper diagonal
    # this is necessary because there can be {0,+} or {0,-} interactions
    # that would pass unnoticed if I used which(upper.tri(..) != 0)
    
    index.upper.tri <- which(upper.tri(interaction.matrix),arr.ind = T)
    
    for(i.elem in 1:nrow(index.upper.tri)){
      if(interaction.matrix[index.upper.tri[i.elem,1],index.upper.tri[i.elem,2]] < 0 & 
         interaction.matrix[index.upper.tri[i.elem,2],index.upper.tri[i.elem,1]] < 0){
        num.competition <- num.competition + 1
      }else if((interaction.matrix[index.upper.tri[i.elem,1],index.upper.tri[i.elem,2]] < 0 & 
                interaction.matrix[index.upper.tri[i.elem,2],index.upper.tri[i.elem,1]] > 0) | 
               (interaction.matrix[index.upper.tri[i.elem,1],index.upper.tri[i.elem,2]] > 0 & 
                interaction.matrix[index.upper.tri[i.elem,2],index.upper.tri[i.elem,1]] < 0)){
        num.antagonism <- num.antagonism + 1
      }else if((interaction.matrix[index.upper.tri[i.elem,1],index.upper.tri[i.elem,2]] < 0 & 
                interaction.matrix[index.upper.tri[i.elem,2],index.upper.tri[i.elem,1]] == 0) | 
               (interaction.matrix[index.upper.tri[i.elem,1],index.upper.tri[i.elem,2]] == 0 & 
                interaction.matrix[index.upper.tri[i.elem,2],index.upper.tri[i.elem,1]] < 0)){
        num.amensalism <- num.amensalism + 1
      }else if((interaction.matrix[index.upper.tri[i.elem,1],index.upper.tri[i.elem,2]] > 0 & 
                interaction.matrix[index.upper.tri[i.elem,2],index.upper.tri[i.elem,1]] == 0) | 
               (interaction.matrix[index.upper.tri[i.elem,1],index.upper.tri[i.elem,2]] == 0 & 
                interaction.matrix[index.upper.tri[i.elem,2],index.upper.tri[i.elem,1]] > 0)){
        num.commensalism <- num.commensalism + 1
      }else if((interaction.matrix[index.upper.tri[i.elem,1],index.upper.tri[i.elem,2]] > 0 & 
                interaction.matrix[index.upper.tri[i.elem,2],index.upper.tri[i.elem,1]] > 0)){
        num.mutualism <- num.mutualism + 1
      }else if((interaction.matrix[index.upper.tri[i.elem,1],index.upper.tri[i.elem,2]] == 0 & 
                interaction.matrix[index.upper.tri[i.elem,2],index.upper.tri[i.elem,1]] == 0)){
        num.no.interaction <- num.no.interaction + 1
      }
    } # for each element in the upper diagonal
    
    # a bit messed up, but this part returns a vector of five or six elements with the relative frequencies of each interaction type
    # including the case when all are zero.
    
    if(relative.to == "all"){
      total <- sum(num.competition,num.amensalism,num.antagonism,num.mutualism,num.commensalism,num.no.interaction)
      return.total <- c(num.competition,num.amensalism,num.antagonism,num.mutualism,num.commensalism,num.no.interaction)/total
      names(return.total) <- c("competition","amensalism","antagonism","mutualism","commensalism","no.interaction")
      
    }else{
      total <- sum(num.competition,num.amensalism,num.antagonism,num.mutualism,num.commensalism)
      
      if(total == 0){
        return.total <- rep(0,5)
      }else{
        return.total <- c(num.competition,num.amensalism,num.antagonism,num.mutualism,num.commensalism)/total
      }
      names(return.total) <- c("competition","amensalism","antagonism","mutualism","commensalism")
      
    }#ifelse
  }else{
    return.total <- rep(0,5)
    names(return.total) <- c("competition","amensalism","antagonism","mutualism","commensalism")
  }
  return.total
}
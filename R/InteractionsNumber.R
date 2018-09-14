#' returns the count of interactions in a given square matrix
#'
#' @param interaction.matrix 
#'
#' @return named vector with count of all interaction types
#' @export
#'
#' @examples
InteractionsNumber <- function(interaction.matrix){
  
  # in the same order as the vector of results
  
  num.competition <- 0
  num.amensalism <- 0
  num.antagonism <- 0
  num.mutualism <- 0
  num.commensalism <- 0
  
  num.no.interaction <- 0
  
  # check whether an actual matrix is provided
  if(!(is.null(nrow(interaction.matrix)) | is.null(ncol(interaction.matrix)))){
    
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
    
  }# check
  
  my.result <- c(num.competition,num.amensalism,num.antagonism,num.mutualism,num.commensalism,num.no.interaction)
  names(my.result) <- c("competition","amensalism","antagonism","mutualism","commensalism","no.interaction")
  my.result
}
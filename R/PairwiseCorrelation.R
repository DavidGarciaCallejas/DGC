
#' Computes the correlation of a pairwise interaction strength matrix or dataframe
#' following the formulation of Tang et al. (2014)
#'
#' @param interaction.matrix NxN matrix or dataframe with rows for each pairwise effect
#' If in dataframe format, columns should refer to the affected species (rows of the matrix)
#' followed by the coupled species (columns of the matrix)
#' and interaction strength value
#'
#' @return correlation value
#' @export
#'
#' @examples 
#' interaction.matrix <- matrix(c(1,2,3,1,1,1,0.4,-2,-5),nrow = 3)
#' interaction.data <- as.data.frame.table(interaction.matrix)
#' names(interaction.data) <- c("coupled.sp","affected.sp","impact")
#' interaction.data <- interaction.data[,c("affected.sp","coupled.sp","impact")]
#' PairwiseCorrelation(interaction.matrix)
#' PairwiseCorrelation(interaction.data)


PairwiseCorrelation <- function(interaction.matrix){
  
  if(is.data.frame(interaction.matrix)){
    if(is.factor(interaction.matrix[,1])){
      if(is.numeric(levels(interaction.matrix[,1]))){
        interaction.matrix[,1] <-as.numeric(interaction.matrix[,1])
      }else if(is.character(levels(interaction.matrix[,1]))){
        interaction.matrix[,1] <-as.character(interaction.matrix[,1])
      }
    }
    if(is.factor(interaction.matrix[,2])){
      if(is.numeric(levels(interaction.matrix[,2]))){
        interaction.matrix[,2] <-as.numeric(interaction.matrix[,2])
      }else if(is.character(levels(interaction.matrix[,2]))){
        interaction.matrix[,2] <-as.character(interaction.matrix[,2])
      }
    }
    interaction.matrix <- as.matrix(tidyr::spread_(interaction.matrix,names(interaction.matrix)[2],names(interaction.matrix)[3])[,-1])
  }
  
  #mean of the products of the pairs c_ij, c_ji 
  mean.product <- 0
  
  for(i.row in 1:nrow(interaction.matrix)){
    for(i.col in 1:ncol(interaction.matrix)){
      if(upper.tri(interaction.matrix)[i.row,i.col]){
        mean.product <- mean.product + interaction.matrix[i.row,i.col]*interaction.matrix[i.col,i.row]
      }# if upper.tri
    }# for i.col
  }# for i.row
  
  mean.product <- mean.product/sum(upper.tri(interaction.matrix) == T)
  
  mean.values <- mean(interaction.matrix[row(interaction.matrix) != col(interaction.matrix)])
  variance.value <- var(interaction.matrix[row(interaction.matrix) != col(interaction.matrix)])
  
  (mean.product * mean.values^2)/variance.value
}

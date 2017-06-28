#' Calculate impact matrices from abundance data
#'
#' @param abundance.data abundances vector
#' @param sign.matrix sign matrix
#' @param a.param vector of 5 parameters adjusting the net interaction frequency for each type: 
#' in the following order: competition, amensalism, antagonism, mutualism, commensalism
#' @param x0 parameter for the logistic function. In principle, 1 works fine for all simulations
#' @param threshold extinction threshold for abundance
#' @param scale.factor factor scaling the strength of each interaction type
#' e.g. a scaling of 1 indicates that the maximum effect
#' of one sp. over another is of the same order of magnitude
#' than the population growth rate. Same order as a.param
#' @param return.type either matrix or dataframe
#' @param my.model dynamic equations model for calculating the jacobian
#' @param param.list list of parameters for the dynamic model
#'
#' @return either a dataframe or a list with two matrices
#' @export
#'
ImpactMatrix <- function(abundance.data, 
                         sign.matrix, 
                         a.param = c(0.01,0.01,0.001,0.01,0.01),
                         x0 = 1,
                         threshold = 1e-03,
                         scale.factor = c(0.1,0.1,0.5,0.1,0.1),
                         metric = "impact",
                         return.type = "dataframe", my.model = NULL, param.list = NULL){
  
  # the function can return impact coefficients, i.e IF(i,j)*IS(i,j) or the coefficients of the community matrix
  # for now, it returns both types
  # it can return them in matrix form or in dataframe form
  
  if(metric == "impact" | metric == "both"){
  
  # check if abundances > 0
  if(sum(abundance.data)>0 & !(is.null(nrow(sign.matrix)) | is.null(ncol(sign.matrix)))){ 
    
    impact.matrix <- matrix(data = 0,nrow = length(abundance.data),ncol = length(abundance.data))
    total.abund <- sum(abundance.data)
    
    for(i.row in 1:length(abundance.data)){
      for(i.col in 1:length(abundance.data)){
        if(abundance.data[i.row] != 0 & abundance.data[i.col] != 0){
          
          int.type <- sign.matrix[i.row,i.col]
          
          if(int.type != 0){
            
            if(int.type == 1){
              if(sign.matrix[i.col,i.row] == 1){
                # positive effect over i.row from a mutualism
                a = a.param[4]
              }else if(sign.matrix[i.col,i.row] == 0){
                # positive effectr over i.row from a commensalism
                a = a.param[5]
              }else{
                # positive effect over i.row from an antagonism
                a = a.param[3]
              }
            }else{
              if(sign.matrix[i.col,i.row] == 1){
                # negative effect over i.row, from antagonism
                a = a.param[3]
              }else if(sign.matrix[i.col,i.row] == 0){
                # negative effect over i.row, from amensalism
                a = a.param[2]
              }else{
                # negative effect over i.row, from competition
                a = a.param[1]
              }
            }# if-else
            
            impact.matrix[i.row,i.col] <- DGC::InteractionFrequency(n1 = abundance.data[i.row], 
                                                                    n2 = abundance.data[i.col],
                                                                    a = a,
                                                                    x0 = x0,
                                                                    threshold = threshold) * DGC::InteractionStrength(n1 = abundance.data[i.row],
                                                                                                                      n2 = abundance.data[i.col],
                                                                                                                      sign.matrix = sign.matrix, 
                                                                                                                      sp1 = i.row, 
                                                                                                                      sp2 = i.col, 
                                                                                                                      scale.factor = scale.factor)
          }# if interaction != 0
        }# if != 0
      }# for i.col
    }# for i.row
    
    }else{ # community matrix coefs.
    
    # for this, I need to run the model and get the jacobian/community matrix
    dynamics <- ode(y = abundance.data, times = c(0:1) ,func = mymodel, parms = param.list, method = "rk4")
    dynamics[is.na(dynamics)] <- 0
    community.matrix <- jacobian.full(y = dynamics[1,!dimnames(dynamics)[[2]] %in% c("time")],func = mymodel,parms = param.list)
    community.matrix[is.na(community.matrix)] <- 0
    
    # I am not concerned with intraspecific terms FOR NOW
    diag(community.matrix) <- 0
    
    }# if-else metric
    
    # prepare the output, if it's a dataframe
    
    if(return.type != "matrix"){
      
      num.interactions <- InteractionsNumber(impact.matrix)
      
      num.competition <- num.interactions[1]
      num.amensalism <- num.interactions[2]
      num.antagonism <- num.interactions[3]
      num.mutualism <- num.interactions[4]
      num.commensalism <- num.interactions[5]
      
      num.interactions <- num.competition*2 + num.amensalism + num.antagonism*2 + num.mutualism*2 + num.commensalism
      
      if(metric == "impact"){
        impact.data <- data.frame(interaction.type = character(num.interactions),
                                  affected.sp = numeric(num.interactions),
                                  coupled.sp = numeric(num.interactions),
                                  impact.matrix.value = numeric(num.interactions), stringsAsFactors = F)      
      }else if(metric == "both"){
        impact.data <- data.frame(interaction.type = character(num.interactions),
                                  affected.sp = numeric(num.interactions),
                                  coupled.sp = numeric(num.interactions),
                                  impact.matrix.value = numeric(num.interactions), 
                                  community.matrix.value = numeric(num.interactions), stringsAsFactors = F)
      }else{
        impact.data <- data.frame(interaction.type = character(num.interactions),
                                  affected.sp = numeric(num.interactions),
                                  coupled.sp = numeric(num.interactions), 
                                  community.matrix.value = numeric(num.interactions), stringsAsFactors = F)
      }# if-else metric
      

      # auxiliary
      int.count <- 0
      
      for(i.row in 1:length(abundance.data)){
        for(i.col in 1:length(abundance.data)){
          
          if(impact.matrix[i.row,i.col] != 0){
            
            if(impact.matrix[i.row,i.col] > 0){
              
              if(impact.matrix[i.col,i.row] == 0){
                current.interaction <- "commensalism"
              }else if(impact.matrix[i.col,i.row] < 0){
                current.interaction <- "positive.antagonism"
              }else if(impact.matrix[i.col,i.row] > 0){
                current.interaction <- "mutualism"
              }
              
            }else{ 
              
              if(impact.matrix[i.col,i.row] == 0){
                current.interaction <- "amensalism"
              }else if(impact.matrix[i.col,i.row] < 0){
                current.interaction <- "competition"
              }else if(impact.matrix[i.col,i.row] > 0){
                current.interaction <- "negative.antagonism"
              }
            }# if-else
            
            int.count <- int.count + 1
            
            affected.sp <- i.row
            coupled.sp <- i.col
            
            impact.data$interaction.type[int.count] <- current.interaction
            impact.data$affected.sp[int.count] <- affected.sp
            impact.data$coupled.sp[int.count] <- coupled.sp
            
            if(metric == "impact" | metric == "both"){
              impact.data$impact.matrix.value[int.count] <- impact.matrix[i.row,i.col]
              if(metric == "both"){
                impact.data$community.matrix.value[int.count] <- community.matrix[i.row,i.col]
              }
            }else{
              impact.data$community.matrix.value[int.count] <- community.matrix[i.row,i.col]
            }# if-else metric
            
          }# if != 0
        }# for i.col
      }# for i.row
      
      return(impact.data)
      
    }else{
      if(metric == "impact"){
        impact.matrix
      }else if(metric == "both"){
        return(list(impact.matrix = impact.matrix, community.matrix = community.matrix))
      }else{
        community.matrix
      }
    }# if-else return.type != matrix
    
  }else{# if-else abundances > 0 and num.sp>1
    return(0)
  }# if-else abundances > 0 and num.sp>1
}
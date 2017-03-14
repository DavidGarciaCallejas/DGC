#' Get the size of a community in each cell of a grid
#'
#' By community size I mean here the net size of all individuals
#' regardless of their species identity. 
#' For example, five individuals of five different species and size "1" each
#' will give a community size of "5"; the same number will be the result
#' of having only one individual of size "5"
#'
#' @param spatial.grid the spatial grid containing the cells. See grid_structure.R
#' @param plant.population nested list with the plant population of each cell. See model.R
#'
#' @return vector of size equal to the number of cells, with the size of the community at each cell
#' @export
#'
GetCommunitySize <- function(spatial.grid,plant.population){
  community.size <- integer(nrow(spatial.grid))
  
  for(i.cell in 1:nrow(spatial.grid)){
    for(i.sp in 1:length(plant.population[[i.cell]])){
      # if there are individuals of i.sp
      if(length(plant.population[[i.cell]][[i.sp]])>0){
        for(i.plant in 1:length(plant.population[[i.cell]][[i.sp]])){
          community.size[i.cell] <- community.size[i.cell] + plant.population[[i.cell]][[i.sp]][[i.plant]]["size"]
        }# for i.plant
      }# if length(plant.population[[i.cell]][[i.sp]])>0
    }# for i.sp
  }# for i.cell
  
  # return
  community.size
}

# spatial.grid <- read.table(file = "/home/david/CREAF/FPU/plant_interactions/Results/grid_20_2factors.csv",header = TRUE,sep = ";",dec = ".")
# species.parameters <- read.table(file = "/home/david/CREAF/FPU/plant_interactions/Results/species_parameters.csv",header = TRUE,sep = ";",dec = ".")
# 
# plant.population <- list()
# # first level: each cell has a list of species
# for(i.cell in 1:nrow(spatial.grid)){
#   plant.population[[i.cell]] <- list(nrow(species.parameters))
#   
#   # second level: each combination of species and cell is itself a list, containing the sizes of
#   # the different individuals of that species in that cell
#   
#   for(i.sp in 1:nrow(species.parameters)){
#     plant.population[[i.cell]][[i.sp]] <- list()
#   }# for i.sp
# }# for i.cell
# 
# ################
# # # test two individuals of all species in all cells without amelioration
# for(i.cell in 1:nrow(spatial.grid)){
#   for(i.sp in 1:nrow(species.parameters)){
#     plant.population[[i.cell]][[i.sp]][[length(plant.population[[i.cell]][[i.sp]])+1]] <- c(ID = spatial.grid$grid.ID[i.cell],species = species.parameters$species[i.sp],size = 1)
#     plant.population[[i.cell]][[i.sp]][[length(plant.population[[i.cell]][[i.sp]])+1]] <- c(ID = spatial.grid$grid.ID[i.cell],species = species.parameters$species[i.sp],size = 2)
#     
#     # plant.population[nrow(plant.population)+1,] <- c(spatial.grid$grid.ID[i.cell],species.parameters$species[i.sp],1)
#   }
# }
# ################

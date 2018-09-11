install.packages('kohonen', dependencies = TRUE)

library("kohonen")
require(tidyverse)
require(magrittr)
library(RColorBrewer)
n <- 60
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

data = diamonds 
map_dimension = 20
n_iterations = 1000
recalculate_map = T
recalculate_no_clusters = T

# prepare data

numerics = summarise_all( data, is.numeric ) %>%
    as.logical()

factors = names(data)%>%
    .[!numerics]

numerics = names(data)%>%
    .[numerics]



data_list = list()
distances = vector()

for (fac in factors){
    
    data_list[[fac]] = kohonen::classvec2classmat( data[[fac]] )
    
    distances = c(distances, 'tanimoto')
    
}

data_list[['numerics']] = scale(data[,numerics])
distances = c( distances, 'euclidean')

str(data_list)

names(data_list)

distances

# create a grid onto which the som will be mapped
# we might want to change map dimension in future versions

som_grid = kohonen::somgrid(xdim = map_dimension
                            , ydim=map_dimension
                            , topo="hexagonal")



if(recalculate_map == F & file.exists('som.Rdata') == T){
    
    load('som.Rdata')
    
} else{
    
    m = kohonen::supersom( data_list
                           , grid=som_grid
                           , rlen= n_iterations
                           , alpha = 0.05
                           , whatmap = c(factors, 'numerics')
                           , dist.fcts = distances
                           #, user.weights = weight_layers
                           #, maxNA.fraction = .5
    )
    
    save(m, file = 'som.Rdata')
    
}

plot(m, type="changes")
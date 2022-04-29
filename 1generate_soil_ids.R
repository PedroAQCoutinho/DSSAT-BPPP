library(stringr)
library(sf)
#remove all
rm(list = ls())
#set seed for unique ids
set.seed(1)
#setwd in project folder
#setwd('project')

#number of ids
row.number <- nrow(st_read('data/grid/BR_grid.shp'))
#function to create unique id
unique_id_gen <- function(a, outfile) {
  v <- sapply(1:a, function(u)sample(LETTERS,4) %>% str_flatten)
  while (length(v[duplicated(v)])!=0) {
    v2 <- sapply(1:(length(v[duplicated(v)])), function(u){
      sample(LETTERS,4) %>% str_flatten})
    v <- v[-which(duplicated(v))]
    v <- c(v, v2)
  } 
  saveRDS(data.frame(id = v), outfile)
}


#create folder in data folder
if(!dir.exists('data/soil_ids')) dir.create('data/soil_ids')

#create file
unique_id_gen(row.number, 'data/soil_ids/grid_id.rds')



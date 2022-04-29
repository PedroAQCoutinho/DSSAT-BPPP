library(raster)
library(foreach)
library(parallel)
library(dplyr)

rm(list=ls())
#set
options(scipen = 999)
options(stringsAsFactors = T)
#set folder to project folder
setwd('project')
#load
pasture <- raster('data/pasture/pa_br_pastagens_30m_2020_LAPIG.tif')
grid <- raster('data/grid/pa_br_gridXavier_30m.tif')
grid.values <- st_read('data/grid/pa_br_gridXavier.shp') %>% st_drop_geometry()
grid.values$count <-  0

#blocksize
bs <- blockSize(pasture) ; bs


#test prototype
i <- 701
dt <- data.frame(pasture = getValues(pasture, row = bs$row[i], nrows = bs$nrows[i]),
                 grid = getValues(grid, row = bs$row[i], nrows = bs$nrows[i]))

dt <- dt[!is.na(dt$pasture),]
dt.agg <- aggregate(dt$pasture~dt$grid, FUN = 'sum')
colnames(dt.agg) <- c('grid', 'pasture') ; dt.agg

#create dir
#create folder in data folder
if(!dir.exists('data/simulation_points')) dir.create('data/simulation_points')

#parallel processing
cl <- makeSOCKcluster(16)
registerDoSNOW(cl)
clusterExport(cl, ls())
snow::clusterEvalQ(cl, {library(dplyr)})
snow::clusterEvalQ(cl, {library(raster)})


#for(i in 1:bs$n) {
out.bind <- foreach(i=(1:bs$n), .combine = rbind) %dopar% {
  print(i)
  dt <- data.frame(count = getValues(pastagens, row = bs$row[i], nrows = bs$nrows[i]),
                   grid = getValues(grid, row = bs$row[i], nrows = bs$nrows[i]))
  
  dt <- dt %>%
    filter(!(is.na(count) | is.na(grid)))
  if(nrow(dt)>0) {
    dt.agg <- aggregate(count~grid, data = dt, FUN = 'sum')
  } else { 
    dt.agg <- NA
  }
  
  out[[i]] <<- dt.agg
  
  
  
} ; out.bind

#bind and filter
out.bind <- out.bind %>% filter(!is.na(grid) | !is.na(count))
#save
saveRDS(out.bind, 'data/simulation_points/out.rds')
#aggregate
out.agg <- aggregate(count~grid,data = out.bind,  FUN = 'sum')
#calc area
out.agg$area_ha <- out.agg$count*0.09
#save
saveRDS(out.agg, 'data/simulation_points/simulation_points.rds')


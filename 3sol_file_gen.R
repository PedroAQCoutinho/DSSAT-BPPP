library(dplyr)
library(DSSAT)
library(rcropmod)
library(sf)


rm(list=ls())
#set
options(scipen = 999)
options(stringsAsFactors = F)
setwd('project')
#load
modelo.sol <- read_sol(paste0('data/SOL/TEST.SOL'))
grid.xavier <- st_read('data/grid/BR_grid.shp') #sirgas
coords <- data.frame(sp::coordinates(as_Spatial(grid.xavier)))
colnames(coords) <- c('x', 'y')
grid.xavier <- st_as_sf(cbind(grid.xavier, coords))
###load functions
source('codes/soil_hyd.R')
####load files names
zonals <- list.files('data/SoilGrids_data/');zonals
###JOINING UP SOIL DATA WITH XAVIER GRID
#Var names
vrs <- c('Clay', 'Sand', 'MOS', 'pH', 'SRTM', 'SLOPE', 'SWC')
for (a in vrs) {
  
  if (a  == 'SLOPE') {
    
    r <- read.csv(paste0('data/SoilGrids_data/', zonals[grep('SLOPE', zonals)]))[,c('value', 'mean')]
    
    colnames(r) <- c('value', 'slope')
    
    grid.xavier <- left_join(grid.xavier, r , by = c("value"="value"))
    
  } else if  ( a == 'SRTM') {
    
    r <- read.csv(paste0('data/SoilGrids_data/', zonals[grep('SRTM', zonals)]))[,c('value', 'mean')]
    
    colnames(r) <- c('value', 'altitude')
    
    grid.xavier <- left_join(grid.xavier, r , by = c("value"="value"))
    
    
  }  else {
    
    r <- read.csv(paste0('data/SoilGrids_data/', zonals[grep(a, zonals)]))[,c('value', 'b0', 'b10', 'b30', 
                                                                              'b60','b100','b200')]
    colnames(r) <- c('value', paste0(a, '_', c('b0', 'b10', 'b30', 
                                               'b60','b100','b200')))  
    
    grid.xavier  <- left_join(grid.xavier, r , by = c("value"="value"))
  }
  
  rm(r)
}



for (i in 1:nrow(grid.xavier)) {
  
  
  modelo.sol$PEDON[[1]] <- paste0('TX', (100000+grid.xavier$value[i])) %>% as.character
  modelo.sol$TEXTURE[[1]] <- -99
  modelo.sol$DEPTH[[1]] <- 200
  modelo.sol$DESCRIPTION[[1]] <- 'PASTURE SIMULATION BR'
  modelo.sol$LAT[[1]] <- grid.xavier$y[i]
  modelo.sol$LONG[[1]] <- grid.xavier$x[i]
  modelo.sol$`SCS FAMILY`[[1]] <- -99
  #COLOR
  modelo.sol$SCOM[[1]] <- -99
  #ALBEDO GEN?RICO 0.2
  modelo.sol$SALB[[1]] <- 0.2
  #EVAPOTRANSPIRATION LIMIT
  modelo.sol$SLU1[[1]] <- 6
  #DRAINAGE RATE
  modelo.sol$SLDR[[1]] <- sldr(grid.xavier$Clay_b0[i])
  #RUNOFF CURVE
  modelo.sol$SLRO[[1]] <- slro( slo  = grid.xavier$slope[i] ,
                                depth = 5 ,
                                texture = soil_class(grid.xavier$Clay_b0[i],grid.xavier$Sand_b0[i] )   , 
                                drainage = sldr(grid.xavier$Clay_b0[i]))  %>% as.numeric
  #MINERALIZATION FACTOR
  modelo.sol$SLNF <- 1
  #PHOTOSINTESIS FACTOR
  modelo.sol$SLPF <- 1
  #PH DETERMINATION METHOD NA
  modelo.sol$SMHB <- -99
  modelo.sol$SMPX <- -99
  modelo.sol$SMKE <- -99
  
  
  
  modelo.sol$SLLL[[1]] <- c(soil_hydraulics(grid.xavier$Sand_b0[i]/100,grid.xavier$Clay_b0[i]/100,1)[2],
                            soil_hydraulics(grid.xavier$Sand_b10[i]/100,grid.xavier$Clay_b10[i]/100,1)[2],
                            soil_hydraulics(grid.xavier$Sand_b30[i]/100,grid.xavier$Clay_b30[i]/100,1)[2],
                            soil_hydraulics(grid.xavier$Sand_b60[i]/100,grid.xavier$Clay_b60[i]/100,1)[2],
                            soil_hydraulics(grid.xavier$Sand_b100[i]/100,grid.xavier$Clay_b100[i]/100,1)[2],
                            soil_hydraulics(grid.xavier$Sand_b200[i]/100,grid.xavier$Clay_b200[i]/100,1)[2]) %>% as.numeric 
  
  modelo.sol$SDUL[[1]] <-   c(soil_hydraulics(grid.xavier$Sand_b0[i]/100,grid.xavier$Clay_b0[i]/100,1)[1],
                              soil_hydraulics(grid.xavier$Sand_b10[i]/100,grid.xavier$Clay_b10[i]/100,1)[1],
                              soil_hydraulics(grid.xavier$Sand_b30[i]/100,grid.xavier$Clay_b30[i]/100,1)[1],
                              soil_hydraulics(grid.xavier$Sand_b60[i]/100,grid.xavier$Clay_b60[i]/100,1)[1],
                              soil_hydraulics(grid.xavier$Sand_b100[i]/100,grid.xavier$Clay_b100[i]/100,1)[1],
                              soil_hydraulics(grid.xavier$Sand_b200[i]/100,grid.xavier$Clay_b200[i]/100,1)[1]) %>% as.numeric
  
  modelo.sol$SSAT[[1]] <-  c(soil_hydraulics(grid.xavier$Sand_b0[i]/100,grid.xavier$Clay_b0[i]/100,1)[3],
                             soil_hydraulics(grid.xavier$Sand_b10[i]/100,grid.xavier$Clay_b10[i]/100,1)[3],
                             soil_hydraulics(grid.xavier$Sand_b30[i]/100,grid.xavier$Clay_b30[i]/100,1)[3],
                             soil_hydraulics(grid.xavier$Sand_b60[i]/100,grid.xavier$Clay_b60[i]/100,1)[3],
                             soil_hydraulics(grid.xavier$Sand_b100[i]/100,grid.xavier$Clay_b100[i]/100,1)[3],
                             soil_hydraulics(grid.xavier$Sand_b200[i]/100,grid.xavier$Clay_b200[i]/100,1)[3]) %>% as.numeric
  
  
  modelo.sol$SSKS[[1]] <-   c(soil_hydraulics(grid.xavier$Sand_b0[i]/100,grid.xavier$Clay_b0[i]/100,1)[5],
                              soil_hydraulics(grid.xavier$Sand_b10[i]/100,grid.xavier$Clay_b10[i]/100,1)[5],
                              soil_hydraulics(grid.xavier$Sand_b30[i]/100,grid.xavier$Clay_b30[i]/100,1)[5],
                              soil_hydraulics(grid.xavier$Sand_b60[i]/100,grid.xavier$Clay_b60[i]/100,1)[5],
                              soil_hydraulics(grid.xavier$Sand_b100[i]/100,grid.xavier$Clay_b100[i]/100,1)[5],
                              soil_hydraulics(grid.xavier$Sand_b200[i]/100,grid.xavier$Clay_b200[i]/100,1)[5]) %>% as.numeric
  
  
  modelo.sol$SBDM[[1]] <-   c(soil_hydraulics(grid.xavier$Sand_b0[i]/100,grid.xavier$Clay_b0[i]/100,1)[4],
                              soil_hydraulics(grid.xavier$Sand_b10[i]/100,grid.xavier$Clay_b10[i]/100,1)[4],
                              soil_hydraulics(grid.xavier$Sand_b30[i]/100,grid.xavier$Clay_b30[i]/100,1)[4],
                              soil_hydraulics(grid.xavier$Sand_b60[i]/100,grid.xavier$Clay_b60[i]/100,1)[4],
                              soil_hydraulics(grid.xavier$Sand_b100[i]/100,grid.xavier$Clay_b100[i]/100,1)[4],
                              soil_hydraulics(grid.xavier$Sand_b200[i]/100,grid.xavier$Clay_b200[i]/100,1)[4]) %>% as.numeric
  
  modelo.sol$SLOC <- -99
  modelo.sol$SLCL[[1]]  <- grid.xavier[i,grep('Clay', colnames(grid.xavier), value = T)] %>% st_drop_geometry() %>% as.numeric 
  modelo.sol$SLSI[[1]] <- grid.xavier[1, grep('Clay', colnames(grid.xavier), value = T)] %>% st_drop_geometry() %>% as.numeric 
  modelo.sol$SLHW[[1]] <- (grid.xavier[i, grep('pH', colnames(grid.xavier), value = T)] %>% st_drop_geometry() %>% as.numeric)/10  #no gee o ph é dado em pH*10 
  modelo.sol$SCEC[[1]] <- c(-99,-99,-99,-99,-99,-99)
  modelo.sol$SLCF <- -99
  #replace TX for a non existent soil file
  write_sol(modelo.sol, paste0('C:/DSSAT47/Soil/','TX' , '.SOL'), append = T)
  print(i) 
  
  
  
}


library(dplyr)
library(sf)
library(DSSAT)
library(rcropmod)
library(parallel)
library(doSNOW)
library(foreach)
library(doParallel)


rm(list=ls())
#set
options(scipen = 999)
options(stringsAsFactors = F)
setwd('project')
#unzip files if you havent yet
unzip('data/climate/climate.rar', exdir = 'data/climate')
#load required files
grid.xavier <- st_read('data/grid/BR_grid.shp') #sirgas
coords <- data.frame(sp::coordinates(as_Spatial(grid.xavier)))
colnames(coords) <- c('x', 'y')
grid.xavier <- st_as_sf(cbind(grid.xavier, coords))
altitude <- read.csv(paste0('data/SoilGrids_data/BR_xavier_zonal_SRTM.csv'))[,c('value', 'mean')]
colnames(altitude) <- c('value', 'altitude')
grid.xavier <- left_join(grid.xavier, altitude , by = c("value"="value"))
uniqid <- readRDS("inputs/soil_ids/grid_id.rds")$id

#dates
#dates to fill the WTH files
anos <- seq(1980,2016, by = 1) %>%
  as.character
mes <- c("01", "12")
dia <- c("01","31")
inicio <- paste0('1980', '01', '01')
fim <- paste0('2016', '12', '31')

#parallel processing
cl <- makeSOCKcluster(16)
registerDoSNOW(cl)
clusterExport(cl, ls())
snow::clusterEvalQ(cl, {library(rcropmod)})
#run
out <- foreach(u=grid.xavier$value) %dopar% {
  
  rec <- readRDS(paste0('data/climate/climate_', u, '.rds'))
  u <- which(grid.xavier$value==u)
  
  
  #check if all observations of Tmax if bigger than Tmin
  if(all(rec$TMAX>rec$TMIN)) {
    
    
    weather(
      xy = c(grid.xavier$x[u],grid.xavier$y[u]),
      elev = grid.xavier$altitude[u],
      srad = rec$SRAD,
      tmax = rec$TMAX,
      tmin = rec$TMIN,
      prec = rec$RAIN,
      sdate = inicio,
      edate = fim,
      name = uniqid[u],
      ea = NULL,
      wind = rec$WIND,
      rh = rec$RHUM,
      tmu = NULL,
      sh=NULL,
      pres = NULL,
      tdew = NULL,
      outdir = "C:/DSSAT47/Weather/")
    
    return(NULL) } else {
      #if theres a problema                    
      #increases 0.1  celsis tmin value and inputs
      rec[which(rec$TMAX<rec$TMIN),'TMAX'] <- rec[which(rec$TMAX<rec$TMIN),'TMIN']+0.1
      
      
      
      weather(
        xy = c(grid.xavier$x[u],grid.xavier$y[u]),
        elev = grid.xavier$altitude[u],
        srad = rec$SRAD,
        tmax = rec$TMAX,
        tmin = rec$TMIN,
        prec = rec$RAIN,
        sdate = inicio,
        edate = fim,
        name = uniqid[u],
        ea = NULL,
        wind = rec$WIND,
        rh = rec$RHUM,
        tmu = NULL,
        sh=NULL,
        pres = NULL,
        tdew = NULL,
        outdir = "C:/DSSAT47/Weather/")
      
      return(NULL) 
      
      
    }
  
  
}

#BUG::::weather() function misnaming files
#rename files
files <- list.files('C:/DSSAT47/Weather/', pattern = '.WTH')
for (i in files) {
  
  file.rename(from = paste0('C:/DSSAT47/Weather/', i), to = paste0('C:/DSSAT47/Weather/', gsub('[0-9][0-9][0-9][0-9]', '8001' ,i)))
  
}





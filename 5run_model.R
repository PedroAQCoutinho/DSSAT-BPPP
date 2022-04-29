#run DSSAT
library(DSSAT)
library(dplyr)
library(sf)
library(lubridate)
library(foreach)
library(rcropmod)
library(RSQLite)
library(raster)
library(sf)
rm(list=ls())

#setwd
setwd('project/')
#set folder to output dssat

#set options
options(DSSAT.CSM = 'C:/DSSAT47/DSCSM047.EXE')
options(stringsAsFactors = F)
options(scipen = 999)
####load files names
zonals <- list.files('data/SoilGrids_data/')
####load functions
source('soil_hyd.R')
####load required data
#pasture
pasture <- raster("data/pasture/pa_br_pasture_30m_2020_LAPIG.tif")
#nacional limits
br.limits <- st_read('data/br_limits/pa_br_limiteNacional_250_2021_ibge.shp')
#federative limits
br.ufs <- st_read('data/br_limits/pa_br_limiteEstadual_250_2015_ibge.shp')
#DSSAT X FILE
file.x <- read_filex('data/xfile/SPPI7901.BRX')
#simulation points
simulation_points <- readRDS('data/simulation_points/simulation_points.rds')$grid
#grid xavier
grid.xavier <- st_read('data/grid/BR_grid.shp')
#grids letter id - defined once
ids <- readRDS("data/soil_ids/grid_id.rds")$id
#load MOW example
mow.example <- DSSAT::read_filet('C:/DSSAT47/Brachiaria/SPPI1101.MOW')
#load weather example
weather.file <- DSSAT::read_wth('data/WTH/ZXUJ8001.WTH')
#harvest dates and n aplications
mow.dates <- read.csv2('data/harvest_dates_n_aplication/harvest_dates_n_application.csv')
mow.dates$data <- as.Date(mow.dates$data, '%d/%m/%Y')



#plot simulation points
p.xavier <- ggplot()+
  geom_sf(data = grid.xavier %>%
            dplyr::filter(value %in% simulation_points)
          , fill = NA)+
  geom_sf(data = br.ufs, fill = NA, cex = 0.6, col = 'darkblue')+
  ggtitle('Simulation points')+
  theme_bw(); p.xavier
#save
ggsave('images/simulation_points.png',
       plot = p.xavier, units = 'in', dpi = 200, height = 20, width = 20, scale = 0.5)



###JOINING UP SOIL DATA WITH XAVIER GRID
#Var names
vrs <- c('Clay', 'Sand')

for (a in vrs) {
  
  r <- read.csv(paste0('G:/Meu Drive/MARCELA/soilgrids_xavier_zonal/', grep(a, zonals, value = T)))[,c('value', 'b0', 'b10', 'b30', 
                                                                                                       'b60','b100','b200')]
  colnames(r) <- c('value', paste0(a, '_', c('b0', 'b10', 'b30', 
                                             'b60','b100','b200')))  
  
  grid.xavier <- left_join(grid.xavier, r , by = c("value"="value"))
  
  
  rm(r)
}


#fertilizing days (filter from harvest file)
length.days.to.fertilize <- 1:nrow(mow.dates %>% filter(qte_n != 0))
days.to.fertilize <- which(mow.dates$qte_n!=0) 


#SETTING UP FERTILIZATION
file.x$`FERTILIZERS (INORGANIC)`[length.days.to.fertilize,'F'] <- 1
file.x$`FERTILIZERS (INORGANIC)`[length.days.to.fertilize,'FDATE'] <- as.POSIXct(mow.dates$data[days.to.fertilize])
file.x$`FERTILIZERS (INORGANIC)`[length.days.to.fertilize,'FMCD'] <- file.x$`FERTILIZERS (INORGANIC)`[1,'FMCD']
file.x$`FERTILIZERS (INORGANIC)`[length.days.to.fertilize,'FACD'] <- file.x$`FERTILIZERS (INORGANIC)`[1,'FACD']
file.x$`FERTILIZERS (INORGANIC)`[length.days.to.fertilize,'FDEP'] <- file.x$`FERTILIZERS (INORGANIC)`[1,'FDEP']
file.x$`FERTILIZERS (INORGANIC)`[length.days.to.fertilize,'FAMN'] <- mow.dates$qte_n[days.to.fertilize]
file.x$`FERTILIZERS (INORGANIC)`[length.days.to.fertilize,'FERNAME'] <- file.x$`FERTILIZERS (INORGANIC)`[1,'FERNAME']

#turn Nitro and Water effect on = YES 
file.x$`SIMULATION CONTROLS`[,'WATER'] <- 'Y'
file.x$`SIMULATION CONTROLS`[,'NITRO'] <- 'Y'

##BUILDING FINAL MOW FILE
mow.example <- mow.example[1:nrow(mow.dates),]
mow.example[1:nrow(mow.dates),'TRNO'] <- 1
mow.example[1:nrow(mow.dates),'DATE'] <- as.POSIXct(mow.dates$data)
mow.example[1:nrow(mow.dates),'MOW'] <- 3000
mow.example[1:nrow(mow.dates),'RSPLF'] <- 27
mow.example[1:nrow(mow.dates),'MVS'] <- 3
mow.example[1:nrow(mow.dates),'RSHT'] <- 20

#write MOW FILE
write_filet(mow.example, 'C:/DSSAT47/Brachiaria/SPPI7902.MOW', drop_duplicate_rows = T)

#SQLITE NEW DATABASE
#create folder to tempo files
if(!dir.exists('outputs')) dir.create('outputs')
conn <- dbConnect(SQLite(), 'outputs/outputs.db')

#create folder to temp files
if(!dir.exists('dssat_temp')) dir.create('dssat_temp')
setwd('dssat_temp')

#run takes aroung 100hours for 9 thousand points
for (k in simulation_points) {
  
  k <- which(grid.xavier$value==k)
  #setting up xfile with local info
  file.x$`PLANTING DETAILS`[,'PDATE'] <- as.POSIXct(as.Date('15/10/1980', '%d/%m/%Y'))
  file.x$`IRRIGATION AND WATER MANAGEMENT`[,'IDATE'] <- as.POSIXct(as.Date('15/10/1980', '%d/%m/%Y'))
  
  file.x$`TREATMENTS                        -------------FACTOR LEVELS------------`[1,] <- file.x$`TREATMENTS                        -------------FACTOR LEVELS------------`[1,]
  file.x$`TREATMENTS                        -------------FACTOR LEVELS------------`[1, 'TNAME'] <- paste0('MARANDU', grid.xavier$value[k])    
  file.x$FIELDS[1,'L'] <- 1
  file.x$FIELDS[1, 'ID_FIELD'] <- 'AAAA0001'
  file.x$FIELDS[1,'WSTA'] <- ids[k]
  file.x$FIELDS[1,'ID_SOIL'] <- paste0('TX', (100000+grid.xavier$value[k])) %>% as.character
  file.x$FIELDS[1,'FLNAME'] <- 'TX'
  file.x$`INITIAL CONDITIONS`$SH2O[[1]] <- c(soil_hydraulics(grid.xavier$Sand_b0[k]/100,grid.xavier$Clay_b0[k]/100,1)[1],
                                             soil_hydraulics(grid.xavier$Sand_b10[k]/100,grid.xavier$Clay_b10[k]/100,1)[1],
                                             soil_hydraulics(grid.xavier$Sand_b30[k]/100,grid.xavier$Clay_b30[k]/100,1)[1],
                                             soil_hydraulics(grid.xavier$Sand_b60[k]/100,grid.xavier$Clay_b60[k]/100,1)[1],
                                             soil_hydraulics(grid.xavier$Sand_b100[k]/100,grid.xavier$Clay_b100[k]/100,1)[1],
                                             soil_hydraulics(grid.xavier$Sand_b200[k]/100,grid.xavier$Clay_b200[k]/100,1)[1]) %>% as.numeric
  
  
  
  
  write_filex(file.x, 'C:/DSSAT47/Brachiaria/SPPI7902.BRX')
  # Write batch file
  write_dssbatch(x='C:/DSSAT47/Brachiaria/SPPI7902.BRX', trtno=1, rp = 1,sq = 0, op = 0, co = 0)
  cat(paste0('run : ', k))
  #run
  run_dssat(run_mode = 'B')
  
  
  #plant growth
  pgro <- data.frame(read_output('PlantGro.OUT'))
  #assign value column
  pgro[, 'value'] <- grid.xavier$value[k]
  pgro$DATE <- as.character(pgro$DATE)
  
  #evapotranspiration
  et <- data.frame(read_output('ET.OUT'))
  #assign value column
  et[, 'value'] <- grid.xavier$value[k]
  et$DATE <- as.character(et$DATE)
  
  #forage out
  forage <- data.frame(read_output('FORAGE.OUT'))
  #assign value column
  forage[, 'value'] <- grid.xavier$value[k]
  forage$DATE <- as.character(forage$DATE)
  
  #write in sqlite
  dbWriteTable(conn, 'ET', et, append = T)
  dbWriteTable(conn, 'PlantGro', pgro,  append = T)
  dbWriteTable(conn, 'Forage', forage, append = T)
  
  
  
  cat('\n')
  cat('\n')
  
  
  
  
  
}
#
dbDisconnect(conn)






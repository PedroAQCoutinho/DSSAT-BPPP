#run DSSAT
library(DSSAT)
library(dplyr)
library(sf)
library(foreign)
library(lubridate)
library(foreach)
library(rcropmod)
library(ggplot2)
library(RSQLite)
library(gridExtra)
library(raster)
library(sf)
library(tidyr)
library(rgeos)
rm(list=ls())



#set options
options(DSSAT.CSM = 'C:/DSSAT47/DSCSM047.EXE')
options(stringsAsFactors = F)
options(scipen = 999)


conn <- dbConnect(SQLite(), 'D:/pedro/PRODUTIVIDADE_POTENCIAL_PASTAGENS/outputs/outputs.db')
dbListTables(conn)


#exploracao
dbSendQuery(conn, 'DROP TABLE prototipo;')
dbSendQuery(conn, 'CREATE TABLE prototipo as
                   select * from PlantGro limit 13227')

#load
#dias de colheita e aplicacoes
mow.dates <- data.frame(DATE = read.csv2('D:/pedro/PRODUTIVIDADE_POTENCIAL_PASTAGENS/inputs/harvest_dates_n_aplication/harvest_dates_n_application.csv')[,1])
mow.dates$DATE <- as.POSIXct(as.Date(mow.dates$DATE, '%d/%m/%Y'))
mow.dates$colhe <- 1:nrow(mow.dates)
#load weather example
weather.file <- data.frame(DSSAT::read_wth('C:/DSSAT47/Weather/ZXUJ8001.WTH')[,'DATE'])

#entrecortes label
entrecortes.label <- weather.file %>% left_join(mow.dates, by = c('DATE')) %>%
  mutate(ano = format(DATE, '%Y'),
         colhe = ifelse(DATE == as.POSIXct(as.Date('01/01/1980', '%d/%m/%Y')), 0, colhe),
         DATE = as.character(DATE)) %>%
  fill(colhe)

#entrecortes contagem
entrecortes.count <- entrecortes.label %>%
  group_by(colhe) %>%
  tally()

#write aux tables
dbWriteTable(conn, 'entrecortes_label', entrecortes.label)
dbWriteTable(conn, 'entrecortes_count', entrecortes.count)

#perform
#Query completa
# dbSendQuery(conn, 'CREATE TABLE PlantGro_mensal as
# 
#             WITH soma as
#             (select b.colhe, SUM(HERB) HERB_total, value
#             from prototipo a
#             left join entrecortes_label b on a.DATE = b.DATE and substr(a.DATE, 0, 5) = b.ano
#             group by b.colhe, value)
# 
#             select a.value ,a.DATE, substr(a.DATE, 0, 5) ANO, substr(a.DATE, 6,2) MES, b.colhe numero_colheita, c.n dias_entrecorte, HERB biomassa_seca_colhida,
#             HERB_total biomassa_seca_colhida_total_periodo,
#             sum(HERB_total/c.n) soma_biomassa_seca_colhida_taxa_diaria
#             from prototipo a
#             left join entrecortes_label b on a.DATE = b.DATE and substr(a.DATE, 0, 5) = b.ano
#             left join entrecortes_count c on b.colhe = c.colhe
#             left join soma d on b.colhe = d.colhe and a.value = d.value
#                  group by a.value, ANO, MES')

#query limpa
if (!'PlantGro_mensal' %in% dbListTables(conn)) dbSendQuery(conn, 'CREATE TABLE PlantGro_mensal as
            WITH soma as
            (select b.colhe, SUM(HERB) HERB_total, value 
            from PlantGro a
            left join entrecortes_label b on a.DATE = b.DATE and substr(a.DATE, 0, 5) = b.ano
            group by b.colhe, value)
            
            select a.value ,a.DATE, substr(a.DATE, 0, 5) ANO, substr(a.DATE, 6,2) MES,
            sum(HERB_total/c.n) soma_biomassa_seca_colhida_taxa_diaria
            from PlantGro a
            left join entrecortes_label b on a.DATE = b.DATE and substr(a.DATE, 0, 5) = b.ano
            left join entrecortes_count c on b.colhe = c.colhe
            left join soma d on b.colhe = d.colhe and a.value = d.value
                 group by a.value, ANO, MES')


#not cumulativa variables
if (!'CWAD_LWAD_SWAD_mensal_diario' %in% dbListTables(conn)) dbSendQuery(conn, 'CREATE TABLE CWAD_LWAD_SWAD_mensal_diario as
select value, CAST(substr(DATE, 0, 5) AS NUMERIC)ANO, substr(DATE, 6, 2) MES, 
		substr(DATE, 9, 2) DIA,  AVG(CWAD) CWAD, 
		AVG(SWAD) SWAD, AVG(LWAD) LWAD
from PlantGro pg
group by value, MES, DIA')

#explore
dbGetQuery(conn, 'select distinct value from  PlantGro_mensal')


#test
out <- dbGetQuery(conn,'select * from PlantGro_mensal')

#last year of simulation for each point
last.year.per.point <- out %>%
  group_by(value) %>% 
  mutate(ANO = as.numeric(ANO),
         MES = as.numeric(MES)) %>%
  filter(ANO == max(ANO)) %>%
  dplyr::select(value, ANO)

#number of points per last year of year o dying
number.points.per.year.of.dying <- last.year.per.point %>%
  group_by(ANO) %>%
  tally()

#summarise
monthly.herb <- out %>%
  mutate(ANO = as.numeric(ANO),
         MES = as.numeric(MES)) %>%
  group_by(MES) %>%
  summarise(HERB = mean(soma_biomassa_seca_colhida_taxa_diaria))


sum(monthly.herb$HERB)



out2 <- dbGetQuery(conn,'select * from CWAD_LWAD_SWAD_mensal_diario')


monthly.others <- out2 %>%
  mutate(ANO = as.numeric(ANO),
         MES = as.numeric(MES),
         DIA = as.numeric(DIA)) %>%
  group_by(MES) %>%
  summarise(CWAD = mean(CWAD),
            LWAD = mean(LWAD),
            SWAD = mean(SWAD),)


to.plot <- monthly.herb %>% left_join(monthly.others , by = 'MES')


line.colors <- c('Haste matéria seca (SWAD) (kg/ha)' = 'green',
                 'Folha matéria seca (LWAD) (kg/ha)'='darkblue', 
                 'Parte aérea matéria seca (CWAD) (kg/ha)' = 'red'
)
fill.colors <- c('Biomassa seca (kg/ha)' = 'gray')

#plot bar graph for per UF
p2 <-  
  ggplot(data = to.plot)+
  geom_bar(aes(x = MES, y = HERB, fill = 'Biomassa seca (kg/ha)') , stat = 'identity')+
  geom_line(aes(x = MES, y = SWAD, group= 1, color = 'Haste matéria seca (SWAD) (kg/ha)'), linetype = 'twodash' ,cex = 1)+
  geom_line(aes(x = MES, y = LWAD, group= 1, color = 'Folha matéria seca (LWAD) (kg/ha)'), linetype = 'dashed' ,cex = 1 )+
  geom_line(aes(x = MES, y = CWAD, group= 1, color = 'Parte aérea matéria seca (CWAD) (kg/ha)'), linetype = 'dotted' ,cex = 1)+
  scale_y_continuous(sec.axis=sec_axis(trans = ~./50, breaks = seq(0, 100, by = 10), labels = paste0(seq(0, 1, by = 0.1)*100, '%') )) +
  scale_fill_manual(name = '', values = fill.colors, )+
  scale_color_manual(name = '', values = line.colors)+
  
  labs(x = 'Mês', y='BS (kg/ha)') +
  
  scale_x_discrete(labels = c('Jan', 'Fev', 'Mar', 'Abr', 'Mai', 'Jun', 'Jul', 'Ago', 'Set', 'Out', 'Nov', 'Dez'))+
  theme(axis.text.x = element_text(angle = 315))+
  theme_minimal()

p2








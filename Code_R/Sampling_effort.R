library(sf)
library(ggplot2)
library(ggpubr)
library(mapview)
library(rgdal)
library(rgeos)
library(dplyr)
library(tidyr)
library(tmap)
library(geosphere)
library(vctrs)
library(viridis)


Sard <- st_read("R/Data/Atlas/limiteAmministrRegionale.shp") %>% st_transform(32632)


# create a grid on the map of the Sardinian borders
g <- Sard %>%
  st_make_grid(cellsize = 10000) %>%
  st_intersection(Sard) %>%
  st_cast("MULTIPOLYGON") %>%
  st_sf() %>%
  mutate(id = row_number())


# Plants

Plants <- read.csv('Gap_analysis_data/GBIF_data/GBIF_Vascular_Plants_dupl.csv')

Plants <- Plants[,c('datasetKey','species', 'decimalLongitude', 'decimalLatitude', 'year', 'coordinateUncertaintyInMeters' )]


names(Plants)[3] <- "x"
names(Plants)[4] <- "y"
names(Plants)[6] <- "spatialUncertainty"


list_dataset <- c('Wikiplant', 'PlantNet_aut', 'iNaturalist', 'PlantNet')


Plants$datasetKey <- ifelse(Plants$datasetKey %in% list_dataset, Plants$datasetKey, Plants$datasetKey <- "Others")


Plants$datasetKey <- gsub('PlantNet_aut', 'PlantNet', Plants$datasetKey)


Dataset_freq1 <- vec_count(Plants$datasetKey)
Dataset_freq1$count_perc <- (Dataset_freq1$count/nrow(Plants))*100


Plants$Wikiplant <- ifelse(Plants$datasetKey == 'Wikiplant', 1, 0)
Plants$PlantNet <- ifelse(Plants$datasetKey == 'PlantNet', 1, 0)
Plants$Others <- ifelse(Plants$datasetKey == 'Others', 1, 0)
Plants$iNaturalist <- ifelse(Plants$datasetKey == 'iNaturalist', 1, 0)

# transform dataframe into shp data 

p_shp  <- st_as_sf(Plants , coords = c('x', 'y'))

p_shp <- st_set_crs(p_shp, 4326)

p_shp <- p_shp %>% st_transform(32632)


# join plants shp data with our grid 
g_plants <- st_join(g, p_shp)

#select rows with NA values in the points column
na_rows <- g_plants[is.na(g_plants$species), ]

id_NA <- na_rows$id


group_g_plants1 <- g_plants %>%
  group_by(id, datasetKey) %>% 
  summarise(Total_SE = n(),
            Wiki_SE = sum(Wikiplant),
            PlantN_SE = sum(PlantNet),
            Others_SE = sum(Others),
            iNatur_SE = sum(iNaturalist))


group_g_plants1$Total_SE <- ifelse(group_g_plants1$id %in% id_NA, group_g_plants1$Total_SE == 0, group_g_plants1$Total_SE)


group_g_plants2 <- g_plants %>%
  group_by(id) %>% 
  summarise(Total_SE = n(),
            Wiki_SE = sum(Wikiplant),
            PlantN_SE = sum(PlantNet),
            Others_SE = sum(Others),
            iNatur_SE = sum(iNaturalist))


group_g_plants2$Total_SE <- ifelse(group_g_plants2$id %in% id_NA, group_g_plants2$Total_SE == 0, group_g_plants2$Total_SE)

# fill na with 0
group_g_plants2[is.na(group_g_plants2)] <- 0


# data analysis

### Total sampling effort

sum(group_g_plants2$Total_SE)

# 104329 total observations

nrow(group_g_plants2[group_g_plants2$Total_SE > 0,])

# 300

### Wikiplant

sum(group_g_plants2$Wiki_SE)

# 44870 total observations

nrow(group_g_plants2[group_g_plants2$Wiki_SE > 0,])

# 279 cells sampled by wikiplant

### PlantNet

sum(group_g_plants2$PlantN_SE)

# 40227 total observations

nrow(group_g_plants2[group_g_plants2$PlantN_SE > 0,])

# 298 cells sampled by PlantNet (19 more respect to wikiplant)


### Others dataset (unknown)

sum(group_g_plants2$Others_SE)

# 11042 total observations

nrow(group_g_plants2[group_g_plants2$Others_SE > 0,])

# 263 cells sampled by Others

### iNaturalist

sum(group_g_plants2$iNatur_SE)

# 8190 total observations

nrow(group_g_plants2[group_g_plants2$iNatur_SE > 0,])

# 274 cells sampled by iNaturalist


p1  <- 
  ggplot(group_g_plants2[group_g_plants2$Total_SE > 0,])+
  geom_sf(aes(fill = Total_SE))+
  scale_fill_distiller('SE',palette= "Spectral", 
                       breaks = c(200, 400, 600, 800, 1000, 1200, 1400, 
                                  1600, 1800, 2000, 2200, 2400), limits = c(0,2500)) +
  labs(title = "Total sampling effort", x="Longitude", y="Latitude", fill = "Total_SE") +theme_light()+
  theme(legend.background=element_blank(),
        panel.grid = element_blank(),
        legend.position = 'right',
        plot.title = element_text(size=15,face = 'bold',hjust = 0.5),
        legend.title=element_text(size=12,face = 'bold'),
        axis.title.x = element_text(size=14,face = 'bold'),
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.title.y = element_text(size=14,face = 'bold'),
        axis.text.y = element_text(size = 10, face = 'bold'),
        axis.ticks.y=element_blank(),
        text = element_text(size=12), 
        strip.text = element_text(size=12),
        legend.text = element_text(size=8.5,angle = 0), 
        legend.key.size = unit(1, 'cm'))

p2  <- 
  ggplot(group_g_plants1[group_g_plants1$Total_SE > 0,])+
  geom_sf(aes(fill = Total_SE))+
  facet_grid(~datasetKey)+
  scale_fill_distiller('SE',palette= 'Spectral', 
                       breaks = c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400), limits = c(0,1500)) +
  labs(title = "Sampling effort by data source", x="Longitude", y="Latitude", fill = "Total_SE") +theme_light()+
  theme(legend.background=element_blank(),
        panel.grid = element_blank(),
        legend.position = 'right',
        plot.title = element_text(size=15,face = 'bold',hjust = 0.5),
        legend.title=element_text(size=12,face = 'bold'),
        axis.title.x = element_text(size=14,face = 'bold'),
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.title.y = element_text(size=14,face = 'bold'),
        axis.text.y = element_text(size = 10, face = 'bold'),
        axis.ticks.y=element_blank(),
        text = element_text(size=12), 
        strip.text = element_text(color = "black",size=12),
        strip.background = element_blank(),
        legend.text = element_text(size=8.5,angle = 0), 
        legend.key.size = unit(1, 'cm'))



Wiki <-  g_plants[g_plants$datasetKey == "Wikiplant",]

summary(Wiki$year)

#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   1950    1980    1988    1990    2001    2020       7 

PlantNet <-  g_plants[g_plants$datasetKey == "PlantNet",]

summary(PlantNet$year)

#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 #  2009    2019    2021    2020    2022    2022       7 

Others <-  g_plants[g_plants$datasetKey == "Others",]

summary(Others$year)

#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   1952    2003    2014    2011    2019    2023       7 

iNaturalist <-  g_plants[g_plants$datasetKey == "iNaturalist",]

summary(iNaturalist$year)

#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   1994    2016    2021    2019    2022    2023       7

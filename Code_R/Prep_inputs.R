library(sf)
library(raster)
library(MazamaSpatialUtils)
library(fasterize)
library(tmap)
library(dplyr)
library(tidyverse)
library(vctrs)


# read sardinian border polygons SHP

Sard <- st_read("R/Data/Atlas/limiteAmministrRegionale.shp") %>% st_transform(32632)


# create a grid of the map of the Sardinian borders
g <- Sard %>%
  st_make_grid(cellsize = 10000) %>%
  st_intersection(Sard) %>%
  st_cast("MULTIPOLYGON") %>%
  st_sf() %>%
  mutate(id = row_number())


# calculate area in m2 
g$area <- st_area(g)

# convert column to numeric
g$area <- as.numeric(g$area)


# read sardinian roads SHP

roads <-  st_read("Gap_analysis_data/SHP/Spatial_determinants/DBGT_10K_22_V02_01_ELEMENTO_STRADALE.shp") %>% st_transform(32632)


roads <- roads[,c('SegmentID', 'geometry')]


# join roads with grids
roads_g <- st_intersection(roads,g)

# select the id of interest
id_list <- sort(unique(roads_g$id))


# calculate road length in m
roads_g$length <- st_length(roads_g)

# convert to numeric
roads_g$length <- as.numeric(roads_g$length)

# calculate total sum of lengths by id
total_lenghtBy_id <- setNames(aggregate(roads_g$length, by=list(id= roads_g$id), FUN=sum), c("id","tot_road_length"))


# merge g with total_lenghtBy_id by id
g <- merge(g, total_lenghtBy_id, by = "id", all.x = TRUE)


# for the column area set 0 where the grid did not intersect the roads
g$area <- ifelse(g$id %in% id_list, g$area, 0)

# fill na with 0
g$tot_road_length[is.na(g$tot_road_length)] <- 0

# calculate density
g$road_density <- g$tot_road_length/g$area

# fill na with 0
g$road_density[is.na(g$road_density)] <- 0


# define function to scale values between 0 and 1
scale_values <- function(x){(x-min(x))/(max(x)-min(x))}

#scale values in 'road_density' column to be between 0 and 1
g$road_density <- scale_values(g$road_density )


### sdNDVI

# read sdNDVI by grid
sdNDVI <- read_csv("Gap_analysis_data/SHP/Spatial_determinants/sdNDVI.csv", show_col_types = FALSE)

# add sdNDVI to the grid
g$sdNDVI <- sdNDVI$sdNDVI

# fill na with 0
g$sdNDVI[is.na(g$sdNDVI)] <- 0


#scale values in 'sdNDVI' column to be between 0 and 1
g$sdNDVI <- scale_values(g$sdNDVI)


### Topographic Roughness

# read Sardinian Dem
Dem <- raster('C:/Users/Rai_8/Documents/R/Data/Clima_Morfologia/TIF/Dem_agg.tif')

# remove comments to observe Dem plot
#Dem_spat <- as(Dem, "SpatRaster")

#plot(Dem_spat)


rough <- terrain(Dem, # raster DEM
                 opt="roughness", # roughness function
                 unit='degrees') # unit degree

# remove comments to observe Dem plot

#plot(rough,
 #    main= "Roughness",
  #   col=rainbow(50))

# convert raster to point data
rough_points <- rasterToPoints(rough)

str(rough_points)

rough_points <- as.data.frame(rough_points)

# transform dataframe into shp data 

rough_points_shp  <- st_as_sf(rough_points , coords = c('x', 'y'))

rough_points_shp <- st_set_crs(rough_points_shp, 32632)


# join grid with rough points 
g_rough  <- st_join(g, rough_points_shp)

# calculate mean roughness by id
group_g_rough <- g_rough %>%
  group_by(id) %>% 
  summarise(mean_roughness = mean(roughness))

# fill na with 0
group_g_rough$mean_roughness[is.na(group_g_rough$mean_roughness)] <- 0


# scale values in 'mean_roughness' column to be between 0 and 1
group_g_rough$mean_roughness <- scale_values(group_g_rough$mean_roughness)



# add mean_roughness to grid
g$mean_roughness<- group_g_rough$mean_roughness



# sampling effort by dataset source

# Plants

Plants <- read.csv('Gap_analysis_data/GBIF_data/GBIF_Vascular_Plants_dupl.csv')

Plants <- Plants[,c('datasetKey','species', 'decimalLongitude', 'decimalLatitude', 'year', 'coordinateUncertaintyInMeters' )]


names(Plants)[3] <- "x"
names(Plants)[4] <- "y"
names(Plants)[6] <- "spatialUncertainty"


list_dataset <- c('Wikiplant', 'PlantNet_aut', 'iNaturalist', 'PlantNet')


Plants$datasetKey <- ifelse(Plants$datasetKey %in% list_dataset, Plants$datasetKey, Plants$datasetKey <- "Others")


Plants$datasetKey <- gsub('PlantNet_aut', 'PlantNet', Plants$datasetKey)

# dataset frequency 
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


group_g_plants <- g_plants %>%
  group_by(id) %>% 
  summarise(Total_SE = n(),
            Wiki_SE = sum(Wikiplant),
            PlantN_SE = sum(PlantNet),
            Others_SE = sum(Others),
            iNatur_SE = sum(iNaturalist))


group_g_plants$Total_SE <- ifelse(group_g_plants$id %in% id_NA, group_g_plants$Total_SE == 0, group_g_plants$Total_SE)

# fill na with 0
group_g_plants[is.na(group_g_plants)] <- 0

# calculate proportion (weight) of each dataset source

group_g_plants$W_Wiki_SE <- group_g_plants$Wiki_SE / group_g_plants$Total_SE
group_g_plants$W_PlantN_SE <- group_g_plants$PlantN_SE / group_g_plants$Total_SE
group_g_plants$W_Others_SE <- group_g_plants$Others_SE / group_g_plants$Total_SE
group_g_plants$W_iNatur_SE <- group_g_plants$iNatur_SE / group_g_plants$Total_SE

# fill na with 0
group_g_plants[is.na(group_g_plants)] <- 0

g$W_Wiki_SE <- group_g_plants$W_Wiki_SE
g$W_PlantN_SE <- group_g_plants$W_PlantN_SE
g$W_Others_SE <- group_g_plants$W_Others_SE
g$W_iNatur_SE <- group_g_plants$W_iNatur_SE

# Latitude and Longitude

# calculate centroid of each cell
centroid <- st_centroid(g)

# extract Latitude and Longitude 
df <- centroid %>% extract(geometry, c('lat', 'lon'), '\\((.*), (.*)\\)', convert = TRUE) 

# add Latitude and Longitude to grid
g$Lat <- df$lat
g$Lon <- df$lon



# select variables of interest


g <- g[,c('id', 'road_density', 'sdNDVI', 'mean_roughness', 'W_Wiki_SE', 'W_PlantN_SE', 'W_Others_SE', 'W_iNatur_SE', 'Lon', 'Lat')]



# save the results
st_write(g, "g_inputs.shp")



funchir::stale_package_check('Prep_inputs.R')



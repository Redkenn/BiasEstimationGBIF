library(dplyr)
library(tidyr)
library(sf)
library(raster)
library(rgdal)
library(spatstat)
library(purrr)
library(TPD)
library(vegan)
library(rnaturalearth)
library(mapview)

library(rlist)
library(viridis)

# sardinia border polygons

Sard <- st_read("R/Data/Atlas/limiteAmministrRegionale.shp") %>% st_transform(32632)



# GBIF vascular plants Without duplicates (from 1950 to 2023)

Plants <- read.csv('Gap_analysis_data/GBIF_data/GBIF_Vascular_Plants.csv')

Plants <- Plants[,c('species', 'decimalLongitude', 'decimalLatitude', 'year', 'coordinateUncertaintyInMeters' )]


names(Plants)[2] <- "x"
names(Plants)[3] <- "y"
names(Plants)[5] <- "spatialUncertainty"

# transform dataframe into shp data 

p_shp  <- st_as_sf(Plants , coords = c('x', 'y'))

p_shp <- st_set_crs(p_shp, 4326)

p_shp <- p_shp %>% st_transform(32632)



# create a grid on the map of the Sardinian borders
g <- Sard %>%
  st_make_grid(cellsize = 10000) %>%
  st_intersection(Sard) %>%
  st_cast("MULTIPOLYGON") %>%
  st_sf() %>%
  mutate(id = row_number())



# select the id of interest
id_list <- sort(unique(g$id))


# creates the for loop for the st_touches and st_union function, so 
# that the new polygon is represented by the target polygon +
# neighbouring polygons


g_list <- list()

for (i in id_list){
  g_target <- g[g$id == i,]
  g_touches <- st_touches(g_target,g)
  list <- list.append(g_touches[[1]],i)
  g_new <- st_union(g[list, ])
  g_list[[i]] <- g_new
}

# use unsplit function to create sfc_GEOMETRY
g_list_unsp <- unsplit(g_list, f = g$id)

# create the new grid
g2 <- g_list_unsp %>%
  st_sf %>%
  st_cast


# add id_list to the grid

g2$id <- id_list

# join plants shp data with our grid 
p_g <- st_join(p_shp,g2)

# Remove any rows in which there are NAs in id column.
p_g <- p_g  %>%
  filter(!is.na(id))

# select the id of interest
id_list <- sort(unique(p_g$id))

start <- Sys.time()
# group p_g by id and species to calculate n° obs of each species per id
d1 <- p_g %>% dplyr::select(id, species) %>% unique() %>%
  group_by(id,species) %>% 
  summarise(ni = n())
print( Sys.time() - start )


d1 <- data.frame(d1$id, d1$species, d1$ni)

names(d1)[1] <- "id"
names(d1)[2] <- "species"
names(d1)[3] <- "ni"

# set NA for rows with empty string
d1$species <- na_if(d1$species, "")


# remove rows with NA
d1 <- na.omit(d1)

##########
# Remove comments only if you want to save the results of the
# previous calculation, as it takes a long time. Then read the
# dataset to avoid redoing the calculation

#write.csv(d1, "dd2_ni.csv", row.names=FALSE)

#d1 <- read.csv('Gap_analysis_data/dd2_ni.csv')

##########

# create pivot_wider of d1
d1 %>%  
  unique%>% 
  pivot_wider(names_from = species,values_from = ni) %>% 
  dplyr::select(id, order(colnames(.))) %>% 
  as.data.frame() -> d1Wide


# fill na with 0
d1Wide[is.na(d1Wide)] <- 0


# update g with id_list
g <- g[g$id %in% id_list, ]


# remove id column
d1Wide <- subset(d1Wide, select = -id)


# calcluate Pielou's evenness for the taxonomic bias

df <- data.frame(S=rowSums(d1Wide[,1:2390]),
                 H=diversity(d1Wide[,1:2390]))
df$J <- df$H/log(df$S)
df$id <- d1Wide$id 


# Pielou's evenness 1
g$J_Taxon <- df$J


# check the value scale
summary(g)


# save the results
st_write(g, "Taxonomic_bias.shp")





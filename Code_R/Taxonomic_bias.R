library(iNEXT)
library(dplyr)
library(tidyr)
library(spatstat)
library(sf)
library(ggplot2)
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



# join plants shp data with our grid 
p_g <- st_join(p_shp,g)

# Remove any rows in which there are NAs in id column.
p_g <- p_g  %>%
  filter(!is.na(id))

# select the id of interest
id_list <- sort(unique(p_g$id))

# 18 minutes of run

# group p_g by id and species to calculate n? obs of each species per id
d1 <- p_g %>% dplyr::select(id, species) %>% unique() %>%
  group_by(id,species) %>% 
  summarise(ni = n())


#d1 <- read.csv('Gap_analysis_data/dd_ni.csv')

d1 <- data.frame(d1$id, d1$species, d1$ni)

names(d1)[1] <- "id"
names(d1)[2] <- "species"
names(d1)[3] <- "ni"

# split the d1 dataset by grouping of id using "f" argument

split_d1 <- split(d1$ni, f = d1$id)

# use iNEXT function with 5 knots

result <- iNEXT(split_d1, q=0, datatype="abundance", knots = 5)

# extract the sample coverage estimate for the reference sample

SC <- result[["DataInfo"]]$SC

# update g with id_list
g <- g[g$id %in% id_list, ]

# completeness
g$Comp <- SC


# save the results
st_write(g, "Taxonomic_bias.shp")





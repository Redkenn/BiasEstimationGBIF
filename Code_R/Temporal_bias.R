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


# sardinia border polygons

Sard <- st_read("R/Data/Atlas/limiteAmministrRegionale.shp") %>% st_transform(32632)


# create a grid on the map of the Sardinian borders
g <- Sard %>%
  st_make_grid(cellsize = 10000) %>%
  st_intersection(Sard) %>%
  st_cast("MULTIPOLYGON") %>%
  st_sf() %>%
  mutate(id = row_number())


# GBIF vascular plants with duplicates (from 1950 to 2023)

Plants <- read.csv('Gap_analysis_data/GBIF_data/GBIF_Vascular_Plants_dupl.csv')

Plants <- Plants[,c('species', 'decimalLongitude', 'decimalLatitude', 'year', 'coordinateUncertaintyInMeters' )]


names(Plants)[2] <- "x"
names(Plants)[3] <- "y"
names(Plants)[5] <- "spatialUncertainty"

# transform dataframe into shp data 

p_shp  <- st_as_sf(Plants , coords = c('x', 'y'))

p_shp <- st_set_crs(p_shp, 4326)

p_shp <- p_shp %>% st_transform(32632)

# join plants shp data with our grid 
p_g <- st_join(p_shp,g)

# Remove any rows in which there are NAs in id column.
p_g <- p_g  %>%
  filter(!is.na(id))


# group p_g by id and year to calculate n° cells of each year per id
d1 <- p_g %>% dplyr::select(id, year) %>% unique() %>%
  group_by(id,year) %>% 
  summarise(N_cells= n())


d1 <- data.frame(d1$id, d1$year, d1$N_cells)

names(d1)[1] <- "id"
names(d1)[2] <- "year"
names(d1)[3] <- "N_cells"

# create pivot_wider of d1
d1 %>%  
  unique%>% 
  pivot_wider(names_from = year,values_from = N_cells) %>% 
  dplyr::select(id, order(colnames(.))) %>% 
  as.data.frame() -> d1Wide

# fill na with 0
d1Wide[is.na(d1Wide)] <- 0


# calcluate Pielou's evenness for the temporal bias

df1 <- data.frame(S=rowSums(d1Wide[,2:75]),
                  H=diversity(d1Wide[,2:75]))
df1$J <- df1$H/log(df1$S)
df1$id <- d1Wide$id 

# fill na with mean of the column
df1$J[is.na(df1$J)] <- mean(df1$J, na.rm = TRUE)


# select the id of interest
id_list <- sort(unique(df1$id))

# update g with id_list
g <- g[g$id %in% id_list, ]

# add Pielou's evenness to the grid
g$J_Temp <- df1$J


# save the results
st_write(g, "Temporal_bias.shp")

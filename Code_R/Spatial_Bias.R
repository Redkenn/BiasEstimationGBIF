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


# join plants shp data with our grid 
p_g <- st_join(p_shp,g)

# Remove any rows in which there are NAs in id column.
p_g <- p_g  %>%
  filter(!is.na(id))


# select the id of interest
id_list <- sort(unique(p_g$id))

#sort p_g in ascending order of id
p_g_ord  <- p_g[order(p_g$id),]

#Select columns id and geometry (the latter is selected automatically)
p_g_ord <- p_g_ord[,c('id')]

#extract coordinates from p_g_ord
coord <- as.data.frame(st_coordinates(p_g_ord))

#create dataframe with id and coordinates of points
p_g_coord <- data.frame(id = p_g_ord$id,coord)

# split the p_g_coord by grouping of id using "f" argument
split_data <- split(p_g_coord, f = p_g_coord$id)

# prepare the list of ppp objects
ppp_list <- list()

for (i in id_list){
  id <- as.character(i)
  W <- as.owin(g[g$id == i,])
  ppp_obj <- ppp(split_data[[id]][["X"]], split_data[[id]][["Y"]], window = W)
  ppp_list[[i]] <- ppp_obj
  ppp_list <- ppp_list[!sapply(ppp_list,is.null)]
  
}


# about 5 hours of simulation for the Clarkevans test

start <- Sys.time()
# clarkevans.test 
test <- map(ppp_list, ~ clarkevans.test(.x, alternative = 'clustered', correction="cdf"))
print( Sys.time() - start )

extracted_values.nni <- list()


for (i in 1:length(test)) {
  extracted_values.nni[[i]] <- test[[i]][["statistic"]][["R"]]
}

NNI <- unlist(extracted_values.nni)


### Update the grid with calculated indexes

# update g with id_list
g <- g[g$id %in% id_list, ]


# add NNI to the grid
g$NNI <- NNI


# fill na with mean of the column
g$NNI[is.na(g$NNI)] <- mean(g$NNI, na.rm = TRUE)

# save the results
st_write(g, "Spatial_bias.shp")

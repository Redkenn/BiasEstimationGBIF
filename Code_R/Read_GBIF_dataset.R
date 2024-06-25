library(sf)
library(dplyr)



# GBIF vascular plants 

Plants <- read.csv("Gap_analysis_data/GBIF_data/GBIF_full.csv", sep='\t')


# filter by phylum Tracheophyta
Plants <- Plants[Plants$phylum == 'Tracheophyta',]


# filter species since 1950
Plants = Plants[Plants$year >= 1950,]


# select target columns
Plants <- Plants[,c('datasetKey', 'species', 'decimalLongitude', 'decimalLatitude', 'year', 'coordinateUncertaintyInMeters' )]


Plants$datasetKey <- gsub('7a3679ef-5582-4aaa-81f0-8c2545cafc81', 'PlantNet', Plants$datasetKey)
Plants$datasetKey <- gsub('14d5676a-2c54-4f94-9023-1e8dcd822aa0', 'PlantNet_aut', Plants$datasetKey)
Plants$datasetKey <- gsub('ea04653c-58f5-4f14-9ed0-39f85871f1db', 'Wikiplant', Plants$datasetKey)
Plants$datasetKey <- gsub('50c9509d-22c7-4a22-a47d-8c48425ef4a7', 'iNaturalist', Plants$datasetKey)

# Select rows without NA
Plants <- Plants[is.na(Plants$species) == FALSE,]



# Check number of decimals longitude and latitude 
str_lat <- as.character(Plants$decimalLatitude)
str_lon <- as.character(Plants$decimalLongitude)
dec_lat <- numeric()
dec_lon <- numeric()


for (i in 1:length(str_lat)) {
  if (grepl("e", str_lat[i])) {
    str_lat[i] <- "0.00"
    decla <- unlist(strsplit(str_lat[i], "\\."))[2]
    dec_lat <- c(dec_lat, nchar(decla))
  } else {
    decla <- unlist(strsplit(str_lat[i], "\\."))[2]
    dec_lat <- c(dec_lat, nchar(decla))
  }
}

for (i in 1:length(str_lon)) {
  declo <- unlist(strsplit(str_lon[i], "\\."))[2]
  dec_lon <- c(dec_lon, nchar(declo))
}

Plants$dec_lat <- dec_lat
Plants$dec_lon <- dec_lon

# filter only include those with min. 2 points
Plants <- Plants %>% filter(dec_lat >= 2, dec_lon >= 2)
cat("length only including lon-lat 2 decimals", nrow(Plants), "\n")


# 105169 obs

# check the number of duplicated coordinates and species
sum(duplicated(Plants[,c('species', 'decimalLongitude', 'decimalLatitude' )], keep = "first"))


# drop duplicates
Plants_2 <- Plants[!duplicated(Plants[,c('species', 'decimalLongitude', 'decimalLatitude' )], keep = "first"), ]


# 92342 obs

# select target columns for Plants and Plants_2
Plants <- Plants[,c('datasetKey','species', 'decimalLongitude', 'decimalLatitude', 'year', 'coordinateUncertaintyInMeters' )]

Plants_2 <- Plants_2[,c('datasetKey','species', 'decimalLongitude', 'decimalLatitude', 'year', 'coordinateUncertaintyInMeters' )]



# save files in csv format, one with duplicates and one without
write.csv(Plants, "GBIF_Vascular_Plants_dupl.csv", row.names=FALSE)

write.csv(Plants_2, "GBIF_Vascular_Plants.csv", row.names=FALSE)






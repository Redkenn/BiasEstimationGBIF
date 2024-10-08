library(sf)
library(tricolore)
library(ggplot2)
library(ggpubr)
library(viridis)
library(dplyr)
library(tidyr)

# Read the shp file with the three types of bias
Tax_bias <- st_read("Gap_analysis_data/SHP/Outputs/Taxonomic_bias.shp") %>% st_transform(32632)

Temp_bias <- st_read("Gap_analysis_data/SHP/Outputs/Temporal_bias.shp") %>% st_transform(32632)

Spat_bias <- st_read("Gap_analysis_data/SHP/Outputs/Spatial_bias.shp") %>% st_transform(32632)



# select the id of interest 
id_list <- sort(unique(Temp_bias$id))

# update Tax_bias grid with id_list
Tax_bias <- Tax_bias[Tax_bias$id %in% id_list, ]


# rename the dataframe
df_bias <- Tax_bias

# add bias columns to df_bias
df_bias$J_Temp <- Temp_bias$J_Temp
df_bias$NNI <- Spat_bias$NNI

# create a repetition of rows (900 rows)
df_bias <- df_bias[rep(seq_len(nrow(df_bias)), each = 3), ]

# create a repetition of bias names 
bias <- rep(c('Taxonomic bias (Completeness)', 'Temporal bias (J Index)', 'Spatial bias (NNI)'), times = 300)

# add bias column to df_bias
df_bias$bias <- bias

# assign bias values to the labels in the bias column
df_bias$Comp <- ifelse(df_bias$bias == "Taxonomic bias (Completeness)", df_bias$Comp, 0)
df_bias$J_Temp <- ifelse(df_bias$bias == "Temporal bias (J Index)", df_bias$J_Temp, 0)
df_bias$NNI <- ifelse(df_bias$bias == "Spatial bias (NNI)", df_bias$NNI, 0)

# add column Total_bias
df_bias$Total_bias <- 0

# assign bias values in the Total_bias column
df_bias$Total_bias <- ifelse(df_bias$bias == "Taxonomic bias (Completeness)", df_bias$Total_bias <- df_bias$Comp, 0)
df_bias$Total_bias <- ifelse(df_bias$bias == "Temporal bias (J Index)", df_bias$Total_bias <- df_bias$J_Temp, df_bias$Comp)
df_bias$Total_bias <- ifelse(df_bias$Total_bias == 0, df_bias$NNI, df_bias$Total_bias)

  
p1  <- 
  ggplot(df_bias)+
  geom_sf(aes(fill = Total_bias))+
  facet_grid(~factor(bias,levels = c('Taxonomic bias (Completeness)','Temporal bias (J Index)','Spatial bias (NNI)')))+
  #scale_fill_distiller("",palette= 'viridis', direction = 1) +
  scale_fill_viridis("",option= 'viridis', direction = 1) +
  labs(title = "Bias map", x="Longitude", y="Latitude", fill = "Total_bias") + theme_light()+
  theme(legend.background = element_blank(),
        panel.grid = element_blank(),
        legend.position = 'right',
        plot.title = element_text(size=15,face = 'bold',hjust = 0.5),
        legend.title = element_text(size=12,face = 'bold'),
        axis.title.x = element_text(size=14,face = 'bold'),
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.title.y = element_text(size=14,face = 'bold'),
        axis.text.y = element_text(size = 10, face = 'bold'),
        axis.ticks.y = element_blank(),
        text = element_text(size=12), 
        strip.text = element_text(color = "black",size=10),
        strip.background = element_blank(),
        legend.text = element_text(size=9,angle = 0), 
        legend.key.size = unit(1, 'cm')) 
      



# create trivariate map
outputs <- Tax_bias
outputs$J_Temp <- Temp_bias$J_Temp
outputs$NNI <- Spat_bias$NNI

tric <- Tricolore(outputs, p1 = 'J_Temp', p2 = 'Comp', p3 = 'NNI',
                  contrast = 0.5, hue= 1, breaks = 2) 

outputs$rgb <- tric$rgb 

# change RGB values in viridis RGB 
outputs$rgb <- ifelse(outputs$rgb == "#CA798B", "#5ec962", outputs$rgb)
outputs$rgb <- ifelse(outputs$rgb == "#5F5E5E", "#21918c", outputs$rgb)
outputs$rgb <- ifelse(outputs$rgb == "#6B9E54", "#fde725", outputs$rgb)
outputs$rgb <- ifelse(outputs$rgb == "#5A96C4", "#3b528b", outputs$rgb)

tric$rgb <- outputs$rgb

tric[["key"]][["data"]][["rgb"]] <-  c("#5ec962", "#21918c", "#3b528b", "#fde725",
                                       "#5ec962", "#21918c", "#3b528b", "#fde725",
                                       "#5ec962", "#21918c", "#3b528b", "#fde725")

p2  <- 
  ggplot(outputs)+
  geom_sf(aes(fill = factor(rgb)))+
  scale_fill_identity()+
  labs(title = "Trivariate map of bias metrics", x="Longitude", y="Latitude", fill = "Accuracy") +
  theme_light()+
  theme(legend.background=element_blank(),
        panel.grid = element_blank(),
        legend.position = 'bottom',
        plot.title = element_text(size=15,face = 'bold',hjust = 0.5),
        legend.title=element_text(size=12,face = 'bold'),
        axis.title.x = element_text(size=14,face = 'bold'),
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.title.y = element_text(size=14,face = 'bold'),
        axis.text.y = element_text(size = 10, face = 'bold'),
        axis.ticks.y=element_blank(),
        text = element_text(size=12), 
        strip.text = element_text(size=12),
        legend.text = element_text(size=12,angle = 0), 
        legend.key.size = unit(0.8, 'cm')) 


legend <- tric$key + labs(L = 'J Index', T = 'Completeness', R = 'NNI')



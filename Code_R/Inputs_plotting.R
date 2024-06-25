library(sf)
library(ggplot2)
library(ggpubr)
library(viridis)
library(dplyr)
library(tidyr)


# read input variables
inputs <- st_read("Gap_analysis_data/SHP/Inputs/g_inputs.shp") %>% st_transform(32632)


names(inputs)[1] <- "id"
names(inputs)[2] <- "RD"
names(inputs)[3] <- "sdNDVI"
names(inputs)[4] <- "MR"
names(inputs)[5] <- "WikiW"
names(inputs)[6] <- "PlantW"
names(inputs)[7] <- "OthersW"
names(inputs)[8] <- "iNaturW"




p1  <- 
  ggplot(inputs)+
  geom_sf(aes(fill = RD))+
  scale_fill_viridis('RD',option='viridis',direction = 1)+
  labs(title = "RD", x="Longitude", y="Latitude", fill = "RD") +theme_light()+
  theme(legend.background=element_blank(),
        panel.grid = element_blank(),
        legend.position = 'bottom',
        plot.title = element_text(size=17,face = 'bold',hjust = 0.5),
        legend.title=element_text(size=12,face = 'bold'),
        axis.title.x = element_text(size=14,face = 'bold'),
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.title.y = element_text(size=14,face = 'bold'),
        axis.text.y = element_text(size = 10, face = 'bold'),
        axis.ticks.y=element_blank(),
        text = element_text(size=12), 
        strip.text = element_text(size=12),
        legend.text = element_text(size=10,angle = 0), 
        legend.key.size = unit(0.7, 'cm'))  

p2  <- 
  ggplot(inputs)+
  geom_sf(aes(fill = sdNDVI))+
  scale_fill_viridis('sdNDVI',option='viridis',direction = 1)+
  labs(title = "sdNDVI", x="Longitude", y="Latitude", fill = "sdNDVI") +theme_light()+
  theme(legend.background=element_blank(),
        panel.grid = element_blank(),
        legend.position = 'bottom',
        plot.title = element_text(size=17,face = 'bold',hjust = 0.5),
        legend.title=element_text(size=12,face = 'bold'),
        axis.title.x = element_text(size=14,face = 'bold'),
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.title.y = element_text(size=14,face = 'bold'),
        axis.text.y = element_text(size = 10, face = 'bold'),
        axis.ticks.y=element_blank(),
        text = element_text(size=12), 
        strip.text = element_text(size=12),
        legend.text = element_text(size=10,angle = 0), 
        legend.key.size = unit(0.7, 'cm')) 


p3  <- 
  ggplot(inputs)+
  geom_sf(aes(fill = MR))+
  scale_fill_viridis('MR',option='viridis',direction = 1)+
  labs(title = "MR", x="Longitude", y="Latitude", fill = "MR") +theme_light()+
  theme(legend.background=element_blank(),
        panel.grid = element_blank(),
        legend.position = 'bottom',
        plot.title = element_text(size=17,face = 'bold',hjust = 0.5),
        legend.title=element_text(size=12,face = 'bold'),
        axis.title.x = element_text(size=14,face = 'bold'),
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.title.y = element_text(size=14,face = 'bold'),
        axis.text.y = element_text(size = 10, face = 'bold'),
        axis.ticks.y=element_blank(),
        text = element_text(size=12), 
        strip.text = element_text(size=12),
        legend.text = element_text(size=10,angle = 0), 
        legend.key.size = unit(0.7, 'cm')) 

p4  <- 
  ggplot(inputs)+
  geom_sf(aes(fill = WikiW))+
  scale_fill_viridis('WikiW',option='viridis',direction = 1)+
  labs(title = "WikiW", x="Longitude", y="Latitude", fill = "WikiW") +theme_light()+
  theme(legend.background=element_blank(),
        panel.grid = element_blank(),
        legend.position = 'bottom',
        plot.title = element_text(size=17,face = 'bold',hjust = 0.5),
        legend.title=element_text(size=12,face = 'bold'),
        axis.title.x = element_text(size=14,face = 'bold'),
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.title.y = element_text(size=14,face = 'bold'),
        axis.text.y = element_text(size = 10, face = 'bold'),
        axis.ticks.y=element_blank(),
        text = element_text(size=12), 
        strip.text = element_text(size=12),
        legend.text = element_text(size=10,angle = 0), 
        legend.key.size = unit(0.7, 'cm'))

p5  <- 
  ggplot(inputs)+
  geom_sf(aes(fill = PlantW))+
  scale_fill_viridis('PlantW',option='viridis',direction = 1)+
  labs(title = "PlantW", x="Longitude", y="Latitude", fill = "PlantW") +theme_light()+
  theme(legend.background=element_blank(),
        panel.grid = element_blank(),
        legend.position = 'bottom',
        plot.title = element_text(size=17,face = 'bold',hjust = 0.5),
        legend.title=element_text(size=12,face = 'bold'),
        axis.title.x = element_text(size=14,face = 'bold'),
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.title.y = element_text(size=14,face = 'bold'),
        axis.text.y = element_text(size = 10, face = 'bold'),
        axis.ticks.y=element_blank(),
        text = element_text(size=12), 
        strip.text = element_text(size=12),
        legend.text = element_text(size=10,angle = 0), 
        legend.key.size = unit(0.7, 'cm')) 


p6  <- 
  ggplot(inputs)+
  geom_sf(aes(fill = iNaturW))+
  scale_fill_viridis('iNaturW',option='viridis',direction = 1)+
  labs(title = "iNaturW", x="Longitude", y="Latitude", fill = "iNaturW") +theme_light()+
  theme(legend.background=element_blank(),
        panel.grid = element_blank(),
        legend.position = 'bottom',
        plot.title = element_text(size=17,face = 'bold',hjust = 0.5),
        legend.title=element_text(size=12,face = 'bold'),
        axis.title.x = element_text(size=14,face = 'bold'),
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.title.y = element_text(size=14,face = 'bold'),
        axis.text.y = element_text(size = 10, face = 'bold'),
        axis.ticks.y=element_blank(),
        text = element_text(size=12), 
        strip.text = element_text(size=12),
        legend.text = element_text(size=10,angle = 0), 
        legend.key.size = unit(0.7, 'cm')) 


p7  <- 
  ggplot(inputs)+
  geom_sf(aes(fill = OthersW))+
  scale_fill_viridis('OthersW',option='viridis',direction = 1)+
  labs(title = "OthersW", x="Longitude", y="Latitude", fill = "OthersW") +theme_light()+
  theme(legend.background=element_blank(),
        panel.grid = element_blank(),
        legend.position = 'bottom',
        plot.title = element_text(size=17,face = 'bold',hjust = 0.5),
        legend.title=element_text(size=12,face = 'bold'),
        axis.title.x = element_text(size=14,face = 'bold'),
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.title.y = element_text(size=14,face = 'bold'),
        axis.text.y = element_text(size = 10, face = 'bold'),
        axis.ticks.y=element_blank(),
        text = element_text(size=12), 
        strip.text = element_text(size=12),
        legend.text = element_text(size=10,angle = 0), 
        legend.key.size = unit(0.7, 'cm')) 


ptot1 <- p1 + p2 + p3 + p4 + plot_annotation(tag_levels = 'A') + plot_layout(nrow = 1, ncol = 4)
ptot2 <- p5 + p6 + p7 + plot_annotation(tag_levels = list(c('E','F','G'))) + plot_layout(nrow = 1, ncol = 4)



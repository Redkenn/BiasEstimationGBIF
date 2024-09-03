library(sf)
library(mgcv)
library(pdp)
library(ggplot2)
library(gratia)
library(patchwork)
library(spdep)
library(usdm)
library(marginaleffects)

# read input variables
inputs <- st_read("Gap_analysis_data/SHP/Inputs/g_inputs.shp") %>% st_transform(32632)

# read output variables
Tax_bias <- st_read("Gap_analysis_data/SHP/Outputs/New output/Taxonomic_bias.shp") %>% st_transform(32632)

Temp_bias <- st_read("Gap_analysis_data/SHP/Outputs/Temporal_bias.shp") %>% st_transform(32632)

Spat_bias <- st_read("Gap_analysis_data/SHP/Outputs/Spatial_bias.shp") %>% st_transform(32632)


# select the id of interest 
id_list <- sort(unique(Temp_bias$id))


# update inputs grid with id_list
inputs <- inputs[inputs$id %in% id_list, ]


# update Taxonomic bias data with id_list
Tax_bias <- Tax_bias[Tax_bias$id %in% id_list, ]


# create dataframe for analysis

df <- data.frame(inputs$id, inputs$rd_dnst, inputs$sdNDVI,inputs$mn_rghn, inputs$W_Wk_SE,
                 inputs$W_PN_SE, inputs$W_Ot_SE, inputs$W_Nt_SE, inputs$Lon, inputs$Lat, 
                 Tax_bias$Comp, Temp_bias$J_Temp, Spat_bias$NNI)

names(df)[1] <- "id"
names(df)[2] <- "RD"
names(df)[3] <- "sdNDVI"
names(df)[4] <- "MR"
names(df)[5] <- "WikiW"
names(df)[6] <- "PlantW"
names(df)[7] <- "OthersW"
names(df)[8] <- "iNaturW"
names(df)[9] <- "Lon"
names(df)[10] <- "Lat"
names(df)[11] <- "Comp"
names(df)[12] <- "Jtemp"
names(df)[13] <- "NNI"


summary(df)

# Fit a GAM model for the Jtaxo
gam_Comp1 <- gam(Comp ~ s(RD) + s(sdNDVI)  + s(MR) + s(Lon, Lat), data = df, method = 'REML', select = TRUE)
gam_Comp2 <- gam(Comp ~ s(RD) + s(sdNDVI)  + s(MR) + s(WikiW) + s(Lon, Lat), data = df, method = 'REML', select = TRUE)
gam_Comp3 <- gam(Comp ~ s(RD) + s(sdNDVI)  + s(MR) + s(PlantW) + s(Lon, Lat), data = df, method = 'REML', select = TRUE)
gam_Comp4 <- gam(Comp ~ s(RD) + s(sdNDVI)  + s(MR) + s(OthersW) + s(Lon, Lat), data = df, method = 'REML', select = TRUE)
gam_Comp5 <- gam(Comp ~ s(RD) + s(sdNDVI)  + s(MR) + s(iNaturW) + s(Lon, Lat), data = df, method = 'REML', select = TRUE)



# model comparison
AIC(gam_Comp1,gam_Comp2,gam_Comp3,gam_Comp4,gam_Comp5)

# Summary of the GAM
summary(gam_Comp5)


# prepare plot for partial effects
p1 <- plot_predictions(gam_Comp5, condition = 'RD', 
                 type = 'response', points = 0.5,
                 rug = TRUE)  + theme_minimal() + theme(element_blank()) + theme(aspect.ratio = 1) + labs(title = "Taxonomic bias", x = 'RD', y = "Completeness") + theme(title = element_text(size = 15), text = element_text(size = 15))
p2 <- plot_predictions(gam_Comp5, condition = 'sdNDVI', 
                       type = 'response', points = 0.5,
                       rug = TRUE)  + theme_minimal() + theme(element_blank()) + theme(aspect.ratio = 1) + labs(title = "Taxonomic bias", x = 'sdNDVI', y = "Completeness") + theme(title = element_text(size = 15), text = element_text(size = 15))
p3 <- plot_predictions(gam_Comp5, condition = 'MR', 
                       type = 'response', points = 0.5,
                       rug = TRUE)  + theme_minimal() + theme(element_blank()) + theme(aspect.ratio = 1) + labs(title = "Taxonomic bias", x = 'MR', y = "Completeness") + theme(title = element_text(size = 15), text = element_text(size = 15))
p4 <- plot_predictions(gam_Comp5, condition = 'iNaturW', 
                       type = 'response', points = 0.5,
                       rug = TRUE)  + theme_minimal() + theme(element_blank()) + theme(aspect.ratio = 1) + labs(title = "Taxonomic bias", x = 'iNaturW', y = "Completeness") + theme(title = element_text(size = 15), text = element_text(size = 15))


# Fit a GAM model for the Jtemp
gam_Jtemp1 <-  gam(Jtemp ~ s(RD) + s(sdNDVI) + s(MR) + s(Lon, Lat), data = df, method = 'REML', select = TRUE)
gam_Jtemp2 <-  gam(Jtemp ~ s(RD) + s(sdNDVI) + s(MR) + s(WikiW) + s(Lon, Lat), data = df, method = 'REML', select = TRUE)
gam_Jtemp3 <-  gam(Jtemp ~ s(RD) + s(sdNDVI) + s(MR) + s(PlantW) + s(Lon, Lat), data = df, method = 'REML', select = TRUE)
gam_Jtemp4 <-  gam(Jtemp ~ s(RD) + s(sdNDVI) + s(MR) + s(OthersW) + s(Lon, Lat), data = df, method = 'REML', select = TRUE)
gam_Jtemp5 <-  gam(Jtemp ~ s(RD) + s(sdNDVI) + s(MR) + s(iNaturW) + s(Lon, Lat), data = df, method = 'REML', select = TRUE)



AIC(gam_Jtemp1,gam_Jtemp2,gam_Jtemp3,gam_Jtemp4,gam_Jtemp5)

# Summary of the GAM
summary(gam_Jtemp3)


# prepare plot for partial effects
p5 <- plot_predictions(gam_Jtemp3, condition = 'RD', 
                       type = 'response', points = 0.5,
                       rug = TRUE)  + theme_minimal() + theme(element_blank()) + theme(aspect.ratio = 1) + labs(title = "Temporal bias", x = 'RD', y = "J Index") + theme(title = element_text(size = 15), text = element_text(size = 15))
p6 <- plot_predictions(gam_Jtemp3, condition = 'MR', 
                       type = 'response', points = 0.5,
                       rug = TRUE)  + theme_minimal() + theme(element_blank()) + theme(aspect.ratio = 1) + labs(title = "Temporal bias", x = 'MR', y = "J Index") + theme(title = element_text(size = 15), text = element_text(size = 15))
p7 <- plot_predictions(gam_Jtemp3, condition = 'PlantW', 
                       type = 'response', points = 0.5,
                       rug = TRUE)  + theme_minimal() + theme(element_blank()) + theme(aspect.ratio = 1) + labs(title = "Temporal bias", x = 'PlantW', y = "J Index") + theme(title = element_text(size = 15), text = element_text(size = 15))


# Fit a GAM model for the NNI
gam_NNI1 <- gam(NNI~ s(RD) + s(sdNDVI)  + s(MR) + s(Lon,Lat), data = df, method = 'REML', select = TRUE)
gam_NNI2 <- gam(NNI~ s(RD) + s(sdNDVI)  + s(MR) + s(WikiW) + s(Lon,Lat), data = df, method = 'REML', select = TRUE)
gam_NNI3 <- gam(NNI~ s(RD) + s(sdNDVI)  + s(MR) + s(PlantW) + s(Lon,Lat), data = df, method = 'REML', select = TRUE)
gam_NNI4 <- gam(NNI~ s(RD) + s(sdNDVI)  + s(MR) + s(OthersW) + s(Lon,Lat), data = df, method = 'REML', select = TRUE)
gam_NNI5 <- gam(NNI~ s(RD) + s(sdNDVI)  + s(MR) + s(iNaturW) + s(Lon,Lat), data = df, method = 'REML', select = TRUE)


# Model comparison
AIC(gam_NNI1,gam_NNI2,gam_NNI3,gam_NNI4,gam_NNI5)

# Summary of the GAM
summary(gam_NNI3)

# prepare plot for partial effects
p8 <- plot_predictions(gam_NNI3, condition = 'MR', 
                       type = 'response', points = 0.5,
                       rug = TRUE)  + theme_minimal() + theme(element_blank()) + theme(aspect.ratio = 1) + labs(title = "Spatial bias", x = 'MR', y = "NNI") + theme(title = element_text(size = 15), text = element_text(size = 15))
p9 <- plot_predictions(gam_NNI3, condition = 'PlantW', 
                       type = 'response', points = 0.5,
                       rug = TRUE)  + theme_minimal() + theme(element_blank()) + theme(aspect.ratio = 1) + labs(title = "Spatial bias", x = 'PlantW', y = "NNI") + theme(title = element_text(size = 15), text = element_text(size = 15))



ptot1 <- p1 + p2 + p3 + plot_annotation(tag_levels = 'A') + plot_layout(nrow = 1, ncol = 3)
ptot2 <- p4 + p5 + p6 + plot_annotation(tag_levels = list(c('D','E','F'))) + plot_layout(nrow = 1, ncol = 3)
ptot3 <- p7 + p8 + p9 + plot_annotation(tag_levels = list(c('G','H','I'))) + plot_layout(nrow = 1, ncol = 3)



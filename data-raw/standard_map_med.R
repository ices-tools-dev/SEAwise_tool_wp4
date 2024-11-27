###===================================================
# Standardise maps for WP 4 mediteranean 
# Author: LB 
# Date: 10/11/2023
#R version 4.2.1
###=====================================================


rm(list=ls())

library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(dplyr)
library(sf)
library(ggmap, quietly = T)
library(ggnewscale)
library(ggtext)


land <- read_sf("./data-raw/data/Europe_coastline_shapefile/Europe_coastline_poly.shp")
gsa <- read_sf("./data-raw/data/med/alp_gsa_sett_2014.shp")
gsa_centroids <- st_centroid(gsa)


###set the projection for land!!
land <-sf::st_transform(land, crs =4326)

minlong <- 24
maxlong <- 12
minlat  <- 46
maxlat  <- 35

coordslim <- c(minlong,maxlong,minlat,maxlat)
coordxmap <- round(seq(minlong,maxlong,length.out = 4))
coordymap <- round(seq(minlat,maxlat,length.out = 4))
ext <- st_bbox(c(xmin = minlong, xmax = maxlong,
                                  ymin = minlat, ymax = maxlat),
                                crs =  4326)

sf_use_s2(FALSE)
slim_land <- st_crop(land,ext)
sf_use_s2(TRUE)

rsgsa17 <- raster("./data-raw/data/med/WP4_results_GSA17-18-19/D4.3/RBS_2017_2021_AdriaticSea_GSAs17_18.tif")

plot(rsgsa17)
###RBS
rbs_gsa17_18 <- as.data.frame(raster("./data-raw/data/med/WP4_results_GSA17-18-19/D4.3/RBS_2017_2021_AdriaticSea_GSAs17_18.tif"),xy = TRUE)

rbs_gsa19 <- as.data.frame(raster("./data-raw/data/med/WP4_results_GSA17-18-19/D4.3/RBS_2017_2021_WesternIonianSea_GSA19.tif"),xy = TRUE)

rbs_gsa20 <- as.data.frame(raster("./data-raw/data/med/figures_rasters_GSA20_v2/figures_rasters_GSA20/T4_3/GSA20_RBS.tiff"),xy = TRUE)


rbs <- ggplot()+
  geom_raster(aes(x = x, y = y, fill =RBS_2017_2021_AdriaticSea_GSAs17_18),data = rbs_gsa17_18,na.rm=T)+
  scale_fill_viridis_c(option="viridis",na.value = NA, name = "RBS in GSA 17 & 18",direction = -1)+
  new_scale_fill() +
  geom_raster(aes(x = x, y = y, fill =RBS_2017_2021_WesternIonianSea_GSA19),data = rbs_gsa19,na.rm=T)+
  scale_fill_viridis_c(option="magma",na.value = NA, name = "RBS in GSA 19",direction = -1)+
  new_scale_fill() +
  geom_raster(aes(x = x, y = y, fill =GSA20_RBS),data = rbs_gsa20,na.rm=T)+
  scale_fill_viridis_c(option="plasma",na.value = NA, name = "RBS in GSA 20",direction = -1)+
  geom_sf(data=land,col=NA,fill="grey")+
  geom_sf(data=gsa[gsa$fid %in% c(9:12),],col="orange",alpha=0.2,fill=NA)+
  geom_sf_label(data=gsa_centroids[gsa$fid %in% c(9:12),],aes(label = GSA),col="black")+
  #geom_vline(xintercept = c(15.7,15.75) )+
  theme_classic()+
  theme(plot.background=element_blank(),
        panel.background=element_blank(),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border  = element_rect(colour = "grey", linewidth=.5,fill=NA),
        legend.text   = element_text(size=11),
        legend.title  = element_text(size=11))+
  scale_x_continuous(breaks=coordxmap)+
  scale_y_continuous(breaks=coordymap,expand=c(0,0))+
  coord_sf(xlim=c(coordslim[1], coordslim[2]), ylim=c(coordslim[3],coordslim[4]))+
  ylab("Latitude")+
  xlab("Longitude")
rbs

ggsave("maps/med_rbs.jpeg",dpi = 300, height =7.45,width =  8.92)


###litter 
litter_gsa17_18 <- as.data.frame(raster("./data/med/WP4_results_GSA17-18-19/D4.5/Litter_FR_kg_km2_1.tif"),xy = TRUE)

overlapprob_gsa20 <- as.data.frame(raster("./data/med/figures_rasters_GSA20_v2/figures_rasters_GSA20/T4_5/T4_5_trawlslitter_GSA20.tiff"),xy = TRUE)

nameFillgsa18 <- bquote(
  atop(
  Fishing ~ related ~litter ~ (kg/km^2), 'in' ~ GSA ~ 18
)
)
# 
nameFillgsa20 <- bquote(
  atop(
    Overlap ~ probability ~ of ~ trawls, and ~ litter ~ 'in' ~ GSA ~ 20
  )
)


litmap <- ggplot()+
   geom_raster(aes(x = x, y = y, fill =pred),data = litter_gsa17_18,na.rm=T)+
   scale_fill_viridis_c(option="viridis",na.value = NA, name = nameFillgsa18,direction = -1,trans = "sqrt")+
   new_scale_fill() +
  # geom_raster(aes(x = x, y = y, fill =RBS_2017_2021_WesternIonianSea_GSA19),data = rbs_gsa19,na.rm=T)+
  # scale_fill_viridis_c(option="magma",na.value = NA, name = "RBSin GSA 19",direction = -1)+
  # new_scale_fill() +
  geom_raster(aes(x = x, y = y, fill =T4_5_trawlslitter_GSA20),data = overlapprob_gsa20,na.rm=T)+
  scale_fill_viridis_c(option="plasma",na.value = NA, name = nameFillgsa20,direction = -1,trans = "sqrt",limits=c(0,1))+
  geom_sf(data=land,col=NA,fill="grey")+
  geom_sf(data=gsa[gsa$fid %in% c(9:12),],col="orange",alpha=0.2,fill=NA)+
  geom_sf_label(data=gsa_centroids[gsa$fid %in% c(9:12),],aes(label = GSA),col="black")+
  #geom_vline(xintercept = c(15.7,15.75) )+
  theme_classic()+
  theme(plot.background=element_blank(),
        panel.background=element_blank(),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border  = element_rect(colour = "grey", linewidth=.5,fill=NA),
        legend.text   = element_text(size=11),
        legend.title  = element_text(size=11))+
  guides(colour = guide_legend(nrow = 3))+
  scale_x_continuous(breaks=coordxmap)+
  scale_y_continuous(breaks=coordymap,expand=c(0,0))+
  coord_sf(xlim=c(coordslim[1], coordslim[2]), ylim=c(coordslim[3],coordslim[4]))+
  ylab("Latitude")+
  xlab("Longitude")
  
  litmap

ggsave("maps/med_litmap.jpeg",dpi = 300, height =7.45,width =  8.92)


#risk
###RBS
squabla_gsa17_18 <- read_sf("./data/med/WP4_results_GSA17-18-19/D4.2/OTB_SQUABLA_risk.shp",crs=4326)
squabla_gsa17_18 <- st_transform(squabla_gsa17_18,crs=4326)
table(squabla_gsa17_18$R3.score)
class(squabla_gsa17_18$R3.score)

factor_levels <- c("Low","Medium", "High")

squabla_gsa17_18$R3.score <- factor(squabla_gsa17_18$R3.score, levels = 1:3, labels = factor_levels)
table(squabla_gsa17_18$R3.score)

MPO_gsa20 <- as.data.frame(raster("./data/med/figures_rasters_GSA20_v2/figures_rasters_GSA20/T4_2/T4_2_MPO_LLS_ex_score_GSA20.tiff"),xy = TRUE)
table(MPO_gsa20$T4_2_MPO_LLS_ex_score_GSA20)


# nameFillgsa18 <- bquote(
#   expression(
#     Bycatch ~ mortality ~ risk ~ of ~ longnose ~ spurdog ~ to ~ OTB ~ gear ~ 'in' ~ GSA ~ 18
#   )
# )

nameFillgsa18 <- c("Bycatch mortality risk of longnose spurdog to OTB gear in GSA 18")

nameFillgsa20 <- c("Exposure of bull ray to LLS gear in GSA 20")
# nameFillgsa20 <- bquote(
#   expression(
#     Exposure ~ of ~ bull ~ ray ~ to,LLS ~ gear ~ 'in' ~ GSA ~ 20
#   )
# )

bycat <- ggplot()+
  geom_sf(aes(fill = R3.score),col=NA,data = squabla_gsa17_18,na.rm=T)+
  #scale_fill_viridis_c(option="viridis",na.value = NA, name = "RBS in GSA 17 & 18",direction = -1)+
  scale_fill_viridis_d(name= stringr::str_wrap(nameFillgsa18,25) ,na.value="white",labels=c("Low","Medium","High",""),option ="plasma",drop = FALSE) +
  new_scale_fill() +
  # geom_raster(aes(x = x, y = y, fill =RBS_2017_2021_WesternIonianSea_GSA19),data = rbs_gsa19,na.rm=T)+
  # scale_fill_viridis_c(option="magma",na.value = NA, name = "RBSin GSA 19",direction = -1)+
  # new_scale_fill() +
  geom_raster(aes(x = x, y = y, fill =T4_2_MPO_LLS_ex_score_GSA20),data = MPO_gsa20,na.rm=T)+
  scale_fill_viridis_c(option="plasma",na.value = NA, name = stringr::str_wrap(nameFillgsa20,25),direction=-1)+
  geom_sf(data=land,col=NA,fill="grey")+
  geom_sf(data=gsa[gsa$fid %in% c(9:12),],col="orange",alpha=0.2,fill=NA)+
  geom_sf_label(data=gsa_centroids[gsa$fid %in% c(9:12),],aes(label = GSA),col="black")+
  #geom_vline(xintercept = c(15.7,15.75) )+
  theme_classic()+
  theme(plot.background=element_blank(),
        panel.background=element_blank(),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border  = element_rect(colour = "grey", linewidth=.5,fill=NA),
        legend.text   = element_text(size=11),
        legend.title  = element_text(size=11))+
  scale_x_continuous(breaks=coordxmap)+
  scale_y_continuous(breaks=coordymap,expand=c(0,0))+
  coord_sf(xlim=c(coordslim[1], coordslim[2]), ylim=c(coordslim[3],coordslim[4]))+
  ylab("Latitude")+
  xlab("Longitude")

bycat

ggsave("maps/med_bycat.jpeg",dpi = 300, height =7.45,width =  8.92)

##ecosystem

#see excel spreadsheets
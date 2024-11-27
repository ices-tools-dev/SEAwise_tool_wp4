###===================================================
# Standardise maps for WP 4 north sea
# Author: LB 
# Date: 10/11/2023
#R version 4.2.1
###=====================================================


rm(list=ls())

#library(sp)
#library(rgdal)
library(raster)
#library(rgeos)
library(dplyr)
library(sf)
library(ggmap, quietly = T)
library(ggnewscale)
library(ggtext)
library(stringr)

setwd("C:/Users/lbatts/OneDrive - Marine Institute/WP4/synthesis/4Neil")

land <- read_sf("./data-raw/data/Europe_coastline_shapefile/Europe_coastline_poly.shp")
#ecoreg <- read_sf("./data/ICES_ecoregions_20171207_erase_ESRI.shp")
#ecoreg_centroids <- st_centroid(ecoreg)
#head(ecoreg)

###set the projection for land!!
land <-sf::st_transform(land, crs =4326)

minlong <- -6
maxlong <- 13
minlat  <- 48
maxlat  <- 63

coordslim <- c(minlong,maxlong,minlat,maxlat)
coordxmap <- round(seq(minlong,maxlong,length.out = 4))
coordymap <- round(seq(minlat,maxlat,length.out = 4))
ext <- st_bbox(c(xmin = minlong, xmax = maxlong,
                                  ymin = minlat, ymax = maxlat),
                                crs =  4326)


sf_use_s2(FALSE)
slim_land <- st_crop(land,ext)
sf_use_s2(TRUE)

load("./data-raw/data/NS/Luke_NSstate_VMSdatacall2019.RData")
colnames(NS)[1] <- "csquares"

load("./data-raw/data/NS/Greater North Sea_region_grid_sensitivity.RData")
grid <- st_as_sf(Region)
rbs_grid <- left_join(grid,NS)
names(rbs_grid) <- str_remove(names(rbs_grid), "state_")

usethis::use_data(rbs_grid, overwrite = T)
# summary(NS)
# head(NS)
# head(Region)

# table(grid$state_2018)
# head(grid)

# rbs <- ggplot(data = grid,aes(fill=state_2018,col=state_2018))+
#   geom_sf(na.rm=T)+
#   scale_fill_viridis_c(option="viridis",na.value = NA, name = "RBS",direction = -1)+
#   scale_color_viridis_c(option="viridis",na.value = NA, name = "RBS",direction = -1)+
#    # new_scale_fill() +
#    # geom_raster(aes(x = x, y = y, fill =state),data = rbs_cs,na.rm=T)+
#    # scale_fill_viridis_c(option="plasma",na.value = NA, name = "RBS with the IGFS survey",direction = -1)+
#   # new_scale_fill() +
#   # geom_raster(aes(x = x, y = y, fill =GSA20_RBS),data = rbs_gsa20,na.rm=T)+
#   # scale_fill_viridis_c(option="plasma",na.value = NA, name = "RBS in GSA 20",direction = -1)+
#   geom_sf(data=land,col=NA,fill="grey")+
#   # geom_sf(data=gsa[gsa$fid %in% c(9:12),],col="orange",alpha=0.2,fill=NA)+
#   # geom_sf_label(data=gsa_centroids[gsa$fid %in% c(9:12),],aes(label = GSA),col="black")+
#   # #geom_vline(xintercept = c(15.7,15.75) )+
#   theme_classic()+
#   theme(plot.background=element_blank(),
#         panel.background=element_blank(),
#         axis.text.y   = element_text(size=16),
#         axis.text.x   = element_text(size=16),
#         axis.title.y  = element_text(size=16),
#         axis.title.x  = element_text(size=16),
#         panel.border  = element_rect(colour = "grey", linewidth=.5,fill=NA),
#         legend.text   = element_text(size=11),
#         legend.title  = element_text(size=11))+
#   scale_x_continuous(breaks=coordxmap)+
#   scale_y_continuous(breaks=coordymap,expand=c(0,0))+
#   coord_sf(xlim=c(coordslim[1], coordslim[2]), ylim=c(coordslim[3],coordslim[4]))+
#   ylab("Latitude")+
#   xlab("Longitude")
# rbs
# 
# ggsave("maps/NS_rbs.jpeg",dpi = 300, height =7.45,width =  8.92)
# 

###litter 

load("./data-raw/data/litter_casper_NE_atlantic.RData") #caspers results cover whole NE atlantic
# head(litter)
# table(litter$ICES_SUB)
# dim(litter)
litter$noperkm <- litter$Fishing.related.2021*length(litter$lon) 
# head(litter)
# summary(litter$Fishing.related.2021)
# summary(litter$noperkm)

litter <- litter %>%
  filter(str_detect(as.character(litter$ICES_SUB), paste(c("IV",20),collapse = '|')))

usethis::use_data(litter)

#"\nVb\n"

# 
# littersub <- litter %>%
#   filter(str_detect(as.character(litter$ICES_SUB), paste(as.character(21:32),collapse = '|')))

# dim(littersub)
# table(littersub$ICES_SUB)
# summary(littersub$noperkm)
# summary(littersub$lon)

# litter$grid <- interaction(litter$lon,litter$lat)
# length(unique(litter$grid))


# fish_igfs <- as.data.frame(raster("C:/Users/lbatts/OneDrive - Marine Institute/WP4/synthesis/data/figures_rasters_GSA20_v2/figures_rasters_GSA20/T4_5/T4_5_trawlslitter_GSA20.tiff"),xy = TRUE)


nameFilllit <- bquote(
  atop(
  Predicted ~ fisheries ~ related,
  litter ~ (Numbers/km^2)
)
)
#
# nameFillgsa20 <- bquote(
#   atop(
#     Overlap ~ probability ~ of ~ trawls, and ~ litter ~ 'in' ~ GSA ~ 20
#   )
# )

# #Fishing.related.2021
# litmap <- ggplot()+
#    geom_tile(aes(x = lon, y = lat, fill =noperkm),data = littersub,na.rm=T)+
#    scale_fill_viridis_c(option="viridis",na.value = NA, name = nameFilllit,direction = -1,trans = "sqrt")+
#    #new_scale_fill() +
#   # geom_raster(aes(x = x, y = y, fill =RBS_2017_2021_WesternIonianSea_GSA19),data = rbs_gsa19,na.rm=T)+
#   # scale_fill_viridis_c(option="magma",na.value = NA, name = "RBSin GSA 19",direction = -1)+
#   # new_scale_fill() +
#   # geom_raster(aes(x = x, y = y, fill =T4_5_trawlslitter_GSA20),data = overlapprob_gsa20,na.rm=T)+
#   # scale_fill_viridis_c(option="plasma",na.value = NA, name = nameFillgsa20,direction = -1,trans = "sqrt",limits=c(0,1))+
#   geom_sf(data=land,col=NA,fill="grey")+
#   #geom_sf(data=gsa[gsa$fid %in% c(9:12),],col="orange",alpha=0.2,fill=NA)+
#   #geom_sf_label(data=gsa_centroids[gsa$fid %in% c(9:12),],aes(label = GSA),col="black")+
#   #geom_vline(xintercept = c(15.7,15.75) )+
#   theme_classic()+
#   theme(plot.background=element_blank(),
#         panel.background=element_blank(),
#         axis.text.y   = element_text(size=16),
#         axis.text.x   = element_text(size=16),
#         axis.title.y  = element_text(size=16),
#         axis.title.x  = element_text(size=16),
#         panel.border  = element_rect(colour = "grey", linewidth=.5,fill=NA),
#         legend.text   = element_text(size=11),
#         legend.title  = element_text(size=11))+
#   guides(colour = guide_legend(nrow = 3))+
#   scale_x_continuous(breaks=coordxmap)+
#   scale_y_continuous(breaks=coordymap,expand=c(0,0))+
#   coord_sf(xlim=c(coordslim[1], coordslim[2]), ylim=c(coordslim[3],coordslim[4]))+
#   ylab("Latitude")+
#   xlab("Longitude")
#   
# litmap
# 
# ggsave("maps/NS_litmap.jpeg",dpi = 300, height =7.45,width =  8.92)
# 

#bycatch
###
bycatch_files <- list.files("data-raw/data/NS/Final risk scores_T4.2_Cefas/")
bycatch <- purrr::map(.x = bycatch_files, ~ get(load(paste0("data-raw/data/NS/Final risk scores_T4.2_Cefas/", .x))))
names <- str_remove(bycatch_files, pattern = "_final.risk.RData")
names <- str_replace_all(names, pattern = "_", replacement = " ")
names <- str_to_title(names)
bycatch <- purrr::map2_df(.x = bycatch, .y = names, function(.x,.y) mutate(.x, species = .y))
bycatch <- mutate(bycatch, 
                  whitespace_count = str_count(variable, " "),  # Count number of whitespaces
                  gear = case_when(
                    whitespace_count == 1 ~ word(variable, 1),  # Split at the first whitespace
                    TRUE ~ word(variable, 1, 2)  # Split at the second whitespace
                  ),
                  season = case_when(
                    whitespace_count == 1 ~ word(variable, 2),  # Take the second word
                    TRUE ~ str_split_fixed(variable, " ", 3)[, 3]  # Everything after the second whitespace
                  )
) %>%
  select(-c(whitespace_count, variable)) 

usethis::use_data(bycatch, overwrite = T)


# # load("./data-raw/data/NS/Final risk scores_T4.2_Cefas/common_skate_complex_final_risk.RData")
# # bycatch <- bind_rows(bycatch, risk_dfmelt)
# 
# head(risk_dfmelt)
# class(risk_dfmelt)
# risk_dfmelt[risk_dfmelt$variable=="Otter trawls Q2",]
# tmp <- risk_dfmelt[risk_dfmelt$variable=="Otter trawls Q2",]
# 
# 
# nameFillgsa18 <- c("Bycatch mortality risk of Atlantic wolffish to otter trawls in Q2")
# # nameFillgsa20 <- bquote(
# #   expression(
# #     Exposure ~ of ~ bull ~ ray ~ to,LLS ~ gear ~ 'in' ~ GSA ~ 20
# #   )
# # )
# 
# bycat <- ggplot()+
#   geom_raster(aes(x = x, y = y, fill =value),data = tmp,na.rm=T)+
#   scale_fill_viridis_d(name= stringr::str_wrap(nameFillgsa18,25) ,na.value="white",labels=c("Low","Medium","High",""),option ="viridis",drop = FALSE)+
#     geom_sf(data=land,col=NA,fill="grey")+
#   # geom_sf(data=gsa[gsa$fid %in% c(9:12),],col="orange",alpha=0.2,fill=NA)+
#   # geom_sf_label(data=gsa_centroids[gsa$fid %in% c(9:12),],aes(label = GSA),col="black")+
#   # #geom_vline(xintercept = c(15.7,15.75) )+
#   theme_classic()+
#   theme(plot.background=element_blank(),
#         panel.background=element_blank(),
#         axis.text.y   = element_text(size=16),
#         axis.text.x   = element_text(size=16),
#         axis.title.y  = element_text(size=16),
#         axis.title.x  = element_text(size=16),
#         panel.border  = element_rect(colour = "grey", linewidth=.5,fill=NA),
#         legend.text   = element_text(size=11),
#         legend.title  = element_text(size=11))+
#   scale_x_continuous(breaks=coordxmap)+
#   scale_y_continuous(breaks=coordymap,expand=c(0,0))+
#   coord_sf(xlim=c(coordslim[1], coordslim[2]), ylim=c(coordslim[3],coordslim[4]))+
#   ylab("Latitude")+
#   xlab("Longitude")
# 
# bycat
# 
# ggsave("maps/NS_bycat.jpeg",dpi = 300, height =7.45,width =  8.92)



#####ecosystem

#see "wp4_plot_script_4_4_strathe2e"

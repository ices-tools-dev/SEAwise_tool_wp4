###===================================================
# Standardise maps for WP 4 western waters 
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
library(stringr)

land <- read_sf("./data-raw/data/Europe_coastline_shapefile/Europe_coastline_poly.shp")
ecoreg <- read_sf("./data-raw/data/ICES_ecoregions_20171207_erase_ESRI.shp")
#ecoreg_centroids <- st_centroid(ecoreg)
#head(ecoreg)

###set the projection for land!!
land <-sf::st_transform(land, crs =4326)

minlong <- -15.6
maxlong <- 0
minlat  <- 42
maxlat  <- 60

cutlat  <- 50


coordslim <- c(minlong,maxlong,minlat,maxlat)
coordxmap <- round(seq(minlong,maxlong,length.out = 4))
coordymap <- round(seq(minlat,maxlat,length.out = 4))
ext <- st_bbox(c(xmin = minlong, xmax = maxlong,
                                  ymin = minlat, ymax = maxlat),
                                crs =  4326)

cutext_bob <- st_bbox(c(xmin = minlong, xmax = maxlong,
                 ymin = minlat, ymax = cutlat),
               crs =  4326)
cutext_igfs <- st_bbox(c(xmin = minlong, xmax = maxlong,
                        ymin = cutlat, ymax = cutlat),
                      crs =  4326)


sf_use_s2(FALSE)
slim_land <- st_crop(land,ext)
sf_use_s2(TRUE)


load(file = "./data-raw/data//WW/rbs_bob.RData" ) # object is rbs_bob
load("./data-raw/data/WW/newrelbs.RDA") #object is depsar_dat and is celtic sea region

raster::crs(rbs_bob) <- "EPSG:3035"
rbs_bob <- projectRaster(rbs_bob, crs=4326)

plot(rbs_bob)

#sf_use_s2(FALSE)
#sf_use_s2(TRUE)

## since they overlap, crop them to show on same map
rbs_bob <- crop(rbs_bob,cutext_bob)
rbs_cs <- depsar_dat[depsar_dat$y>50,]
# min(depsar_dat$x)


rbs_bob <- as.data.frame(rbs_bob,xy=TRUE)
# head(rbs_bob)
# dim(rbs_bob)


rbs <- ggplot()+
  geom_raster(aes(x = x, y = y, fill =  RBS_surface_sarmean),data = rbs_bob,na.rm=T)+
  scale_fill_viridis_c(option="viridis",na.value = NA, name = "RBS with EVHOE survey",direction = -1)+
   new_scale_fill() +
   geom_raster(aes(x = x, y = y, fill =state),data = rbs_cs,na.rm=T)+
   scale_fill_viridis_c(option="plasma",na.value = NA, name = "RBS with the IGFS survey",direction = -1)+
  # new_scale_fill() +
  # geom_raster(aes(x = x, y = y, fill =GSA20_RBS),data = rbs_gsa20,na.rm=T)+
  # scale_fill_viridis_c(option="plasma",na.value = NA, name = "RBS in GSA 20",direction = -1)+
  geom_sf(data=land,col=NA,fill="grey")+
  # geom_sf(data=gsa[gsa$fid %in% c(9:12),],col="orange",alpha=0.2,fill=NA)+
  # geom_sf_label(data=gsa_centroids[gsa$fid %in% c(9:12),],aes(label = GSA),col="black")+
  # #geom_vline(xintercept = c(15.7,15.75) )+
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

#ggsave("maps/WW_rbs.jpeg",dpi = 300, height =7.45,width =  8.92)


###litter 

load("./data/litter_casper_NE_atlantic.RData")
head(litter)
table(litter$ICES_SUB)
dim(litter)
litter$noperkm <- litter$Fishing.related.2021*length(litter$lon) 
head(litter)
summary(litter$Fishing.related.2021)
summary(litter$noperkm)



littersub <- litter %>%
  filter(str_detect(as.character(litter$ICES_SUB), paste(c("\nVb\n","VI", "VII"),collapse = '|')))

# 
# littersub <- litter %>% 
#   filter(str_detect(as.character(litter$ICES_SUB), paste(as.character(20:29),collapse = '|')))

dim(littersub)
table(littersub$ICES_SUB)
summary(littersub$noperkm)
summary(littersub$lon)

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

#Fishing.related.2021
litmap <- ggplot()+
   geom_raster(aes(x = lon, y = lat, fill =noperkm),data = littersub,na.rm=T)+
   scale_fill_viridis_c(option="viridis",na.value = NA, name = nameFilllit,direction = -1,trans = "sqrt")+
   #new_scale_fill() +
  # geom_raster(aes(x = x, y = y, fill =RBS_2017_2021_WesternIonianSea_GSA19),data = rbs_gsa19,na.rm=T)+
  # scale_fill_viridis_c(option="magma",na.value = NA, name = "RBSin GSA 19",direction = -1)+
  # new_scale_fill() +
  # geom_raster(aes(x = x, y = y, fill =T4_5_trawlslitter_GSA20),data = overlapprob_gsa20,na.rm=T)+
  # scale_fill_viridis_c(option="plasma",na.value = NA, name = nameFillgsa20,direction = -1,trans = "sqrt",limits=c(0,1))+
  geom_sf(data=land,col=NA,fill="grey")+
  #geom_sf(data=gsa[gsa$fid %in% c(9:12),],col="orange",alpha=0.2,fill=NA)+
  #geom_sf_label(data=gsa_centroids[gsa$fid %in% c(9:12),],aes(label = GSA),col="black")+
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

ggsave("maps/WW_litmap.jpeg",dpi = 300, height =7.45,width =  8.92)


#bycatch risk
###RBS
PUFMAU_bob <- read_sf("./data-raw/data/WW/amaia/PUFMAU.shp/PUFMAU.shp",crs=4326)
table(PUFMAU_bob$z)
class(PUFMAU_bob$z)

factor_levels <- c("Low","Medium", "High")

PUFMAU_bob$z <- factor(PUFMAU_bob$z, levels = 1:3, labels = factor_levels)
table(PUFMAU_bob$z)

load("data-raw/data/WW/bigdat.rda") ## irish waters cetatcean risk
head(longdat)
pph <- longdat[longdat$species=="Pph" & longdat$gear=="nets" & longdat$season=="summer",]
dim(pph)
pph <- pph %>% group_by(grid_id) %>% summarise(mean = mean(R, na.rm=TRUE))
tmp_notsf <- tmp_notsf[,c("grid_id","x")]
tmp_notsf$R <- pph$mean[match(pph$grid_id,tmp_notsf$grid_id)]
#Categorize
tmp_notsf$R1.score<-NA
tmp_notsf$R1.score<-ifelse(tmp_notsf$R>3.18, 3,tmp_notsf$R1.score)
tmp_notsf$R1.score<-ifelse(tmp_notsf$R<=3.18&  tmp_notsf$R>2.64, 2,tmp_notsf$R1.score)
tmp_notsf$R1.score<-factor(ifelse(tmp_notsf$R<2.64,1,tmp_notsf$R1.score))
summary(tmp_notsf$R1.score)
levels(tmp_notsf$R1.score) <- c(1,2,3)
min(st_coordinates(tmp_notsf$x)[,1])

tmp <- sf::st_as_sf(tmp_notsf, crs =4326)


nameFillgsa20 <- c("Bycatch mortality risk of harbour porpoise to netters in Irish waters in summer")

nameFillgsa18 <- c("Bycatch mortality risk of balearic shearwater to longlines in the Bay of Biscay")
# nameFillgsa20 <- bquote(
#   expression(
#     Exposure ~ of ~ bull ~ ray ~ to,LLS ~ gear ~ 'in' ~ GSA ~ 20
#   )
# )

bycat <- ggplot()+
  geom_sf(aes(fill = z),col=NA,data = PUFMAU_bob,na.rm=T)+
  #scale_fill_viridis_c(option="viridis",na.value = NA, name = "RBS in GSA 17 & 18",direction = -1)+
  scale_fill_viridis_d(name= stringr::str_wrap(nameFillgsa18,25) ,na.value="white",labels=c("Low","Medium","High",""),option ="viridis",drop = FALSE) +
  new_scale_fill() +
  # geom_raster(aes(x = x, y = y, fill =RBS_2017_2021_WesternIonianSea_GSA19),data = rbs_gsa19,na.rm=T)+
  # scale_fill_viridis_c(option="magma",na.value = NA, name = "RBSin GSA 19",direction = -1)+
  # new_scale_fill() +
  geom_sf(aes(fill = R1.score),col=NA,data = tmp,na.rm=T)+
  scale_fill_viridis_d(na.value="white",labels=c("Low","Medium","High",""),option ="plasma",drop = FALSE,name = stringr::str_wrap(nameFillgsa20,25)) +
    geom_sf(data=land,col=NA,fill="grey")+
  # geom_sf(data=gsa[gsa$fid %in% c(9:12),],col="orange",alpha=0.2,fill=NA)+
  # geom_sf_label(data=gsa_centroids[gsa$fid %in% c(9:12),],aes(label = GSA),col="black")+
  # #geom_vline(xintercept = c(15.7,15.75) )+
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

ggsave("maps/WW_bycat.jpeg",dpi = 300, height =7.45,width =  8.92)



#####ecosystem

AGCSbiomass_output <- read.csv(file=paste("./data-raw/data/", sh,"/",sh,"_",SCENARIO,"_BIOMASS_RESULTS.csv",sep=""),header=TRUE)
AGCSecosystem_CR_output <- read.csv(file=paste("./data-raw/data/", sh,"/",sh,"_",SCENARIO,"_ECOSYSTEM_RISK_RESULTS.csv",sep=""),header=TRUE)

#Extract fish biomass

extract_af_ratio <- function(data){
  
  NFC<-ncol(data)
  
  subdata<-subset(data,units=="Tonnes_WW_in_the_whole_model_domain")
  
  fsub1 <- subdata[grep("Planktivorous_fish",subdata$variablename),]
  fsub2 <- subdata[grep("Demersal_fish",subdata$variablename),]
  fsub3 <- subdata[grep("Migratory_fish",subdata$variablename),]
  
  if(nrow(fsub1) == 1) PTY<-"nolarvae"
  if(nrow(fsub1) == 2) PTY<-"larvae"
  notlarvrow<-1
  if(nrow(fsub1) == 2){
    larvrow<-grep("larvae",fsub1$variablename)
    if(larvrow==1) notlarvrow<-2
    if(larvrow==2) notlarvrow<-1
  }
  notlarvae_biomass1<-(as.numeric(fsub1[notlarvrow,3:(NFC)]))/1000
  #if(PTY=="larvae") larvae_biomass1<-(as.numeric(fsub1[larvrow,3:(NFC)]))/1000
  
  if(nrow(fsub2) == 1) PTY<-"nolarvae"
  if(nrow(fsub2) == 2) PTY<-"larvae"
  notlarvrow<-1
  if(nrow(fsub2) == 2){
    larvrow<-grep("larvae",fsub2$variablename)
    if(larvrow==1) notlarvrow<-2
    if(larvrow==2) notlarvrow<-1
  }
  notlarvae_biomass2<-(as.numeric(fsub2[notlarvrow,3:(NFC)]))/1000
  #if(PTY=="larvae") larvae_biomass2<-(as.numeric(fsub2[larvrow,3:(NFC)]))/1000
  
  fishbiomass<-notlarvae_biomass1+notlarvae_biomass2 + as.numeric(fsub3[,3:(NFC)])/1000
  
  #Extract predator biomass
  
  psub1 <- subdata[grep("Bird",subdata$variablename),]
  psub2 <- subdata[grep("Pinniped",subdata$variablename),]
  psub3 <- subdata[grep("Cetacean",subdata$variablename),]
  
  apexbiomass <- as.numeric( (psub1[,3:(NFC)] + psub2[,3:(NFC)] + psub3[,3:(NFC)]))/1000
  
  af_ratio<- apexbiomass/fishbiomass
  
  return(af_ratio)
  
}

#.................

AGCSaf_ratio<-extract_af_ratio(data=AGCSbiomass_output)
AGCSrisk<-as.numeric((subset(AGCSecosystem_CR_output,variablename=="whole_ecosystem_CR"))[,3:(AGNF+2)])
ecosystem_data <- data.frame(
  AGCSaf_ratio = AGCSaf_ratio,
  AGCSrisk = AGCSrisk
)

usethis::use_data(ecosystem_data)

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


land <- read_sf("./data-raw/data/Europe_coastline_shapefile/Europe_coastline_poly.shp")
###set the projection for land!!
land <-sf::st_transform(land, crs =4326)
usethis::use_data(land)

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

####SRBS
load("./data-raw/data/NS/Luke_NSstate_VMSdatacall2019.RData")
colnames(NS)[1] <- "csquares"

load("./data-raw/data/NS/Greater North Sea_region_grid_sensitivity.RData")
grid <- st_as_sf(Region)
rbs_grid <- left_join(grid,NS)
names(rbs_grid) <- str_remove(names(rbs_grid), "state_")

# usethis::use_data(rbs_grid, overwrite = T)

###litter 

load("./data-raw/data/litter_casper_NE_atlantic.RData") #caspers results cover whole NE atlantic

litter$noperkm <- litter$Fishing.related.2021*length(litter$lon) 

litter <- litter %>%
  filter(str_detect(as.character(litter$ICES_SUB), paste(c("IV",20),collapse = '|')))

# usethis::use_data(litter)



nameFilllit <- bquote(
  atop(
  Predicted ~ fisheries ~ related,
  litter ~ (Numbers/km^2)
)
)

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

# usethis::use_data(bycatch, overwrite = T)


#####ecosystem

SCENARIO<-"ALLGEARS"
MODEL<-"NORTH_SEA"
sh <- "NS"

AGNSbiomass_output <- read.csv(file=paste("./data-raw/data/", sh,"/",sh,"_",SCENARIO,"_BIOMASS_RESULTS.csv",sep=""),header=TRUE)
AGNSecosystem_CR_output <- read.csv(file=paste("./data-raw/data/", sh,"/",sh,"_",SCENARIO,"_ECOSYSTEM_RISK_RESULTS.csv",sep=""),header=TRUE)

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

AGgmultvec<-c(0,0.2,0.4,0.7,1,1.5,2,2.5,3,4,6,8) # sequences of gear mults to use in the impact phase
AGNF<-length(AGgmultvec)


AGNSaf_ratio<-extract_af_ratio(data=AGNSbiomass_output)
AGNSrisk<-as.numeric((subset(AGNSecosystem_CR_output,variablename=="whole_ecosystem_CR"))[,3:(AGNF+2)])
ecosystem_data <- data.frame(
  ratio = AGNSaf_ratio,
  risk = AGNSrisk
)

# usethis::use_data(ecosystem_data)

WP4_NS <- list(rbs = rbs_grid,
               bycatch = bycatch,
               litter = litter,
               ecosystem = ecosystem_data,
               map_parameters = list(coordslim = coordslim,
                                     coordxmap = coordxmap,
                                     coordymap = coordymap))

usethis::use_data(WP4_NS, overwrite = T)

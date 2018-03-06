#3

#This script conducts spatial joins between parcels and watersheds and between parcels and buildings. 
#Join pair retention is based on largest area of intersection.

library(foreach)
library(doParallel)
library(dplyr)
library(sf)
library(raster)
library(rgeos)
library(sp)
library(spdplyr)
library(stringr)
library(rgdal)
library(tibble)


#Note: If RStudio session on RServer hangs go to home/.rstudio/sessions/active/ and delete the suspended session data folder

wd<- getwd()

##########################################
#Join parcel and subwatersheds
############################################
projection<- "+proj=lcc +lat_1=35.25 +lat_2=36.41666666666666 +lat_0=34.33333333333334 +lon_0=-86 +x_0=600000.0000000001 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs " #set proj info to work with

sub_ws<-shapefile(paste0(wd,"/data/","subwatersheds_smoothed.shp"))
sub_ws<-spTransform(sub_ws, projection) #set projection
sub_ws$wsarea<-gArea(sub_ws, byid=TRUE) #area by watershed
sub_ws@data<-sub_ws@data[ ,c(26:27)]
subws<-st_as_sf(sub_ws)
saveRDS(subws,paste0(wd,"/data/","subws.RDS"))

parcelfiles <- c("parcel2005.RDS", "parcel2006.RDS", "parcel2007.RDS", "parcel2008.RDS", "parcel2009.RDS", "parcel2010.RDS", "parcel2011.RDS",
                 "parcel2012.RDS", "parcel2013.RDS","parcel2014.RDS","parcel2015.RDS")

savefiles<-c("parcel2005ws.RDS", "parcel2006ws.RDS", "parcel2007ws.RDS", "parcel2008ws.RDS", "parcel2009ws.RDS", "parcel2010ws.RDS", "parcel2011ws.RDS",
             "parcel2012ws.RDS", "parcel2013ws.RDS","parcel2014ws.RDS","parcel2015ws.RDS")

#Define a function that associates parcels with the watershed in which they have the largest area of overlap
join_parcel_watershed = function(parcelfiles, savefiles, projection, sub_ws){
  
  n<-length(parcelfiles)
  registerDoParallel(n)
  getDoParWorkers() 
  
  foreach (i=1:n) %dopar% {
    p<-readRDS(paste0(wd,"/data/",parcelfiles[2]))
    p<-spTransform(p, projection) #set projection
    p$parea<-gArea(p, byid=TRUE) #area 
    p$PID<-seq(1:length(p[,1]))
    
    test<-st_as_sf(p) #convert to simpler spatial format (sf)
    test2<-st_as_sf(sub_ws)
    test3<-sf::st_join(test, test2) #conduct spatial join on sf objects
    test4<-sf::st_intersection(st_buffer(test, 0), test2) %>% mutate(int_area = as.numeric(st_area(.)))  #caluclate area of intersection between parcels and watersheds
    test4<-test4[, c(29,30,33)]  #keep only parcel id, ws_id and intersection area
    test5<-left_join(test3, test4, by =c("PID", "WS_ID")) #join intersection area to spatial joined parcels and watersheds
    test5<-arrange(test5, desc(int_area)) #sort from large to small interseciton area
    test6<-test5[duplicated(test5[,29])==FALSE, ] #remove duplicates by parcel id (keeps first (largest) by default)
    test6<-st_as_sf(test6)
    
    test6<-as(test6, "Spatial") #convert back to SPDF
    test6<-spTransform(test6, projection)
    saveRDS(test6, paste0(wd,"/data/", savefiles[i]))
    #plot(test6)
    rm(p, test, test2, test3, test4, test5,  test6)
  }
}

join_parcel_watershed(parcelfiles, savefiles, projection, sub_ws)

##########################################
#Join buildings and parcels by location
############################################
projection<- "+proj=lcc +lat_1=35.25 +lat_2=36.41666666666666 +lat_0=34.33333333333334 +lon_0=-86 +x_0=600000.0000000001 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs" #set proj info to work with

parcels<-c("parcel2005ws", "parcel2014ws", "parcel2015ws")
buildings<-c("bldg2005","bldg2014","bldg2015")
combinednames<-c("pbws2005", "pbws2014","pbws2015")

join_bldgs_parcels = function(parcels, buildings, combinednames, projection){
  
  n<-length(parcels)
  registerDoParallel(n)
  getDoParWorkers() 
  
  foreach (i=1:n) %dopar% {
    p<-readRDS(paste0(wd,"/data/",parcels[i],".RDS"))
    b<-readRDS(paste0(wd,"/data/",buildings[i],".RDS"))
    test<-st_as_sf(b) #convert to simpler spatial format (sf)
    test2<-st_as_sf(p)
    test3<-sf::st_join(test, test2) #conduct spatial join on sf objects
    test4<-sf::st_intersection(test, st_buffer(test2,0)) %>% mutate(int_area2 = as.numeric(st_area(.))) #caluclate area of intersection between parcels and buildings
    test4<-test4[, c(1,3,36)]  #keep only bldg id, stanpar and intersection area2 
    test5<-left_join(test3, test4, by =c("BLDG_ID", "STANPAR")) #join intersection area to spatial joined parcels and buildings
    sums<- as.data.frame (test5 %>% group_by (BLDG_ID) %>% summarise (totLA=sum(LA_SQ_FT), totDU=sum(DU_COUNT), totLAPR=sum(LAND_APPR), totIAPR=sum(IMPR_APPR), 
                                                                      totAPR=sum(TOTAL_APPR), meanIAPR=mean(IMPR_APPR), meanAPR=mean(TOTAL_APPR), countP=length(STANPAR))) #consolidate info for multiple parcels in same building (assume cases where buildings cross parcel boundaries are negligible)
    test5<-left_join(test5, sums, by = "BLDG_ID")
    test5<-arrange(test5, desc(int_area2)) #sort from large to small intersection area
    test6<-test5[duplicated(test5[,1])==FALSE, ] #remove duplicates by bldg id (keeps first (largest IA) by default)
    test6<-st_as_sf(test6)
    pbws<-as(test6, "Spatial")
    pbws<-spTransform(pbws, projection)
    saveRDS(pbws, paste0(wd,"/data/", combinednames[i],".RDS"))
  }
}

join_bldgs_parcels(parcels, buildings, combinednames, projection)

pbws2005<-readRDS("pbws2005.RDS") 
pbws2014<-readRDS("pbws2014.RDS")
pbws2015<-readRDS("pbws2015.RDS")


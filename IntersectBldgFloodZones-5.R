#5

#This script conducts spatial joins of the building files with flooding shapefiles including FEMA flood zones and 
#the 2010 flood inundation boundaries. It also conducts joins with a 100 meter riparian stream buffer and calculates
#the distance from buildings to streams. 

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


############################################################################################################
#Intersect pbws building file with inundation, flood zones, riparian buffer, and streams
#############################################################################################################

#Define a function that joins building files with flood areas

join_floodareas_bldg = function(bldgfiles, savefiles, projection){
  
  #read in all the flood area and buffer planting info
  inundation<- shapefile(paste0(wd,"/data/","FloodInundationDavidsonCo.shp"))
  inun<-st_as_sf(inundation)
  inun<-st_transform(inun, projection)
  inun$inundate<-"Inundated"
  inun<-inun[ c(4,5)]
  rm(inundation)
  
  floodway<-shapefile(paste0(wd,"/data/","floodway.shp"))
  floodway<-st_as_sf(floodway)
  floodway<-st_transform(floodway, projection) 
  floodway<-floodway[,c(4,10)]
  
  fz100yr<-shapefile(paste0(wd,"/data/","OnehunderedyrFloodplain.shp"))
  fz100yr<-st_as_sf(fz100yr)
  fz100yr<-st_transform(fz100yr, projection)  
  fz100yr<-fz100yr[,c(3,8)]
  
  fz500yr<-shapefile(paste0(wd,"/data/","FivehundredyrFloodplain.shp"))
  fz500yr<-st_as_sf(fz500yr)
  fz500yr<-st_transform(fz500yr, projection) 
  fz500yr<-fz500yr[,c(3,8)]
  
  ripzone<-shapefile(paste0(wd,"/data/","full_100m_stream_buffer.shp"))
  ripzone<-st_as_sf(ripzone)
  ripzone<-st_transform(ripzone, projection) 
  ripzone<-ripzone[,c(1,5)]
  ripzone[,1]<-"RipZone"
  
  #loop to join read-in files to bldg files
  n<-length(bldgfiles)
  registerDoParallel(n)
  getDoParWorkers() 
  
  foreach (i=1:n) %dopar% {
    p<-readRDS(paste0(wd,"/data/",bldgfiles[i]))
    
    test<-sf::st_join(p, inun) #conduct spatial join of buildings and inundated area (intersection)
    test2<-arrange(test, desc(inundate)) #sort from large to small 
    test3<-test2[duplicated(test2[,1])==FALSE, ] #remove duplicates by bldg id (keeps first (innundated) by default)
    test3<-st_as_sf(test3)
    
    test4<-sf::st_join(test3, floodway) #now join with floodway
    
    test5<-sf::st_join(test4, fz100yr) #now join with 100 yr
    test6<-arrange(test5, desc(DESCRIPT.y)) #sort from large to small 
    test6<-test6[duplicated(test6[,1])==FALSE, ] #remove duplicates by bldg id 
    test6<-st_as_sf(test6)
    
    test7<-sf::st_join(test6, fz500yr) #now join with 500 yr
    test8<-arrange(test7, desc(DESCRIPT)) #sort from large to small 
    test8<-test8[duplicated(test8[,1])==FALSE, ] #remove duplicates by bldg id 
    test8<-st_as_sf(test8)
    
    test11<-sf::st_join(test8, ripzone) #now join with riparian zone
    test12<-arrange(test11, desc(OBJECTID)) #sort from large to small 
    test12<-test12[duplicated(test12[,1])==FALSE, ] #remove duplicates by bldg id 
    test12<-st_as_sf(test12)
    
    saveRDS(test12, paste0(wd,"/data/", savefiles[i]))
    #plot(test6)
    rm(p, test, test2, test3, test4, test5,  test6, test7, test8, test9, test10)
  }
}

bldgfiles <- c("pbws2005.RDS", "pbws2006.RDS", "pbws2007.RDS", "pbws2008.RDS", "pbws2009.RDS", "pbws2010.RDS", "pbws2011.RDS",
               "pbws2012.RDS", "pbws2013.RDS","pbws2014.RDS","pbws2015.RDS")

savefiles<-c("pbws2005b.RDS", "pbws2006b.RDS", "pbws2007b.RDS", "pbws2008b.RDS", "pbws2009b.RDS", "pbws2010b.RDS", "pbws2011b.RDS",
             "pbws2012b.RDS", "pbws2013b.RDS","pbws2014b.RDS","pbws2015b.RDS")


join_floodareas_bldg(bldgfiles, savefiles, projection)

###Also add the straightline distance from each building to the stream system

streams<-shapefile(paste0(wd,"/data/","NHDstreams.shp"))
streams<-st_as_sf(streams)
streams<-st_transform(streams, projection)
streams<-st_union(streams)

n<-length(bldgfiles)
registerDoParallel(n)
getDoParWorkers()

foreach (i=1:n) %dopar% {
  p<-readRDS(paste0(wd,"/data/",savefiles[i]))
  p<-st_transform(p, projection)
  
  test13<- st_distance(p, streams) #now calculate distance from bldgs to streams, takes ~2 hours
  test13<-as.data.frame(test13)
  colnames(test13)<-c("DistStr")
  test14<-dplyr::bind_cols(p, test13)
  test14$DistStr<-as.numeric(test14$DistStr)
  test14<-st_as_sf(test14)
  
  saveRDS(test14, paste0(wd,"/data/", "stream", savefiles[i]))
} 

rm(savefiles, bldgfiles, streams)


# t<-readRDS(paste0(wd,"/data/","streampbws2009b.RDS"))
# t<-readRDS(paste0(wd,"/data/","streampbws2010b.RDS"))



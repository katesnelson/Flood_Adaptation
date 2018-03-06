#4

#This script creates building footprint shapefiles for each year between 2005 and 2014 
#by interpolating the footprints using 2005 and 2014 measured footprint shapefiles and 
#tax parcel information for each year.

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

############################################################################################
#Build building shapefiles for remaining years
#######################################################################

#st_erase = function(x, y) st_difference(x, st_union(st_combine(y))) #define function st_erase(x,y) that will erase y from x

currentyears<-c("2006","2007","2008","2009","2010", "2011","2012","2013") #"2010"

interpolate_buildings = function(currentyears, projection){
  
  n<-length(currentyears)
  registerDoParallel(n)
  getDoParWorkers() 
  
  foreach (i=1:n) %dopar% {
    #p_current<-readRDS("parcel2010.RDS")
    p_current<-readRDS(paste0(wd,"/data/","parcel",currentyears[i],".RDS"))
    p_pre<-readRDS(paste0(wd,"/data/","parcel2005.RDS"))
    p_post<-readRDS(paste0(wd,"/data/","parcel2014.RDS"))
    
    comp_pre<-p_pre@data$STANPAR
    comp_post<-p_post@data$STANPAR
    comp_current<-p_current@data$STANPAR
    
    new_current<-(comp_current[!(comp_current %in% comp_pre)]) #compare existing parcel identifiers in current and previous year, this reports newly identified parcels in 2010
    incurrentandpost<-as.data.frame(new_current[new_current %in% comp_post]) #these are parcel newly id'd in 2010 that are present in 2014
    incurrentnotinpost<-as.data.frame(new_current[!(new_current %in% comp_post)]) #just a check --> 757 parcels that are not in 05, are in 10, and are not in 14 (out of 24718 new parcels)
    removedcurrent<-as.data.frame(comp_pre[!(comp_pre %in% comp_current)]) #compare existing parcel identifiers in 2010 and 2005, this reports parcels that exist in 2005 but not in 2010
    
    subpostforcurrent<-p_post[(p_post$STANPAR %in% incurrentandpost[,1]), ] #subset of 14 parcels that are new in 10 and present in 14
    subpreforcurrent<-p_pre[!(p_pre$STANPAR %in% removedcurrent[,1]), ] #subset of 05 parcels that exist in 10
    
    subpost<-st_as_sf(subpostforcurrent)
    subpre<-st_as_sf(subpreforcurrent)
    subpre<- st_transform (subpre, projection)
    subpost<- st_transform (subpost, projection)
    
    subpost.y<-st_combine(subpost)
    subpost.y<-st_union(subpost.y)
    new<-st_difference(st_buffer(subpre, 0), st_buffer(subpost.y, 0)) #takes about 2.5 hours on desktop or single thread
    #subpre2<-st_erase(subpre, subpost) #remove the post parcel subset from the pre parcel subset to avoid overlapping polygons when shapefiles merged --> slower than doing in several steps
    
    bldgpre<-readRDS(paste0(wd,"/data/","pbws2005.RDS"))
    bldgpre<-st_as_sf(bldgpre)
    bldgpost<-readRDS(paste0(wd,"/data/","pbws2014.RDS")) 
    bldgpost<-st_as_sf(bldgpost)
    
    bldgpreforcurrent<-bldgpre[new, ] #subset of buildings in pre time to use in current
    bldgpostforcurrent<-bldgpost[subpost, ] #subset of buildings in post time to use in current
    
    bldgcurrent<-rbind(bldgpreforcurrent, bldgpostforcurrent)
    
    #remove buildings bought-out prior to currentyear (could be some homes in the 2005 shapefile used for the 2010 interpolated shapefile that were bought out 2006-2009)
    bldgcurrent2<-bldgcurrent
    bldgcurrent2$Met_Aq_D<- as.Date(bldgcurrent2$Met_Aq_D, format = "%m/%d/%Y")
    bldgcurrent2<-bldgcurrent2[is.na(bldgcurrent2$Met_Aq_D) | bldgcurrent2$Met_Aq_D > as.Date(paste0("1/01/",currentyears[i]), format = "%m/%d/%Y"), ]
    
    #rebuild bldg_id and pid as may have some overlapping numbers from combining 2005 and 2014 data (these unique ids are not consistent idenitifier for the bldgs or parcels, but are used for indexing purposes)
    bldgcurrent2$BLDG_ID<-seq(1:length(bldgcurrent2$BLDG_ID))
    unique_p<-as.data.frame(unique(bldgcurrent2$STANPAR))
    names(unique_p)<-c("STANPAR")
    unique_p$PID<-seq(1:length(unique_p$STANPAR))
    bldgcurrent2b<-left_join(bldgcurrent2, unique_p, by="STANPAR")
    
    saveRDS(bldgcurrent2b, paste0(wd,"/data/","pbws",currentyears[i],".RDS"))
    
    rm(t, bldgpost, bldgpre, bldgpostforcurrent, bldgpreforcurrent, incurrentandpost, incurrentnotinpost, new, removedcurrent, subpost, subpre, comp_current, comp_post,
       comp_pre, new_current, p_current, p_post, p_pre, subpost.y, subpostforcurrent, subpreforcurrent, bldgcurrent, bldgcurrent2, bldgcurrent2b, unique_p)
  }
}

interpolate_buildings(currentyears, projection)

#check the record size on these to make sure we are actually removing and adding buildings properly
# p1<-readRDS(paste0(wd,"/data/","pbws",currentyears[1],".RDS"))
# length(unique(p1$BLDG_ID))
# length(unique(p1$STANPAR))
# p1$Met_Aq_D[!is.na(p1$Met_Aq_D)]
# p2<-readRDS(paste0(wd,"/data/","pbws",currentyears[2],".RDS"))
# length(unique(p2$BLDG_ID))
# length(unique(p2$STANPAR))
# p3<-readRDS(paste0(wd,"/data/","pbws",currentyears[3],".RDS")) 
# length(unique(p3$BLDG_ID))
# length(unique(p3$STANPAR))
# p4<-readRDS(paste0(wd,"/data/","pbws",currentyears[4],".RDS"))  
# length(unique(p4$BLDG_ID))
# length(unique(p4$STANPAR))
# p5<-readRDS(paste0(wd,"/data/","pbws",currentyears[5],".RDS"))
# length(unique(p5$BLDG_ID))
# length(unique(p5$STANPAR))
# p5$Met_Aq_D[!is.na(p5$Met_Aq_D)]
# p6<-readRDS(paste0(wd,"/data/","pbws",currentyears[6],".RDS"))
# length(unique(p6$BLDG_ID))
# length(unique(p6$STANPAR))
# p7<-readRDS(paste0(wd,"/data/","pbws",currentyears[7],".RDS"))
# length(unique(p7$BLDG_ID))
# length(unique(p7$STANPAR))


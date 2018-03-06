#8

#This script computes the shortest distance from any building to a park or buyout-created open greenspace.


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


#############################################################################
#Calculate Proximity to Greenspace
################################################

names<-c( "damagesplusstreampbws2005b.RDS","damagesplusstreampbws2006b.RDS", "damagesplusstreampbws2007b.RDS", "damagesplusstreampbws2008b.RDS", "damagesplusstreampbws2009b.RDS", "damagesplusstreampbws2010b.RDS", "damagesplusstreampbws2011b.RDS",
          "damagesplusstreampbws2012b.RDS", "damagesplusstreampbws2013b.RDS", "damagesplusstreampbws2014b.RDS","damagesplusstreampbws2015b.RDS",
          "damagesplusnewpbws2006b_nb.RDS","damagesplusnewpbws2007b_nb.RDS","damagesplusnewpbws2008b_nb.RDS","damagesplusnewpbws2009b_nb.RDS","damagesplusnewpbws2010b_nb.RDS", "damagesplusnewpbws2011b_nb.RDS","damagesplusnewpbws2012b_nb.RDS", 
          "damagesplusnewpbws2013b_nb.RDS","damagesplusnewpbws2014b_nb.RDS","damagesplusnewpbws2015b_nb.RDS",
          "damagesplusnewpbws2010b_ab.RDS", "damagesplusnewpbws2011b_ab.RDS","damagesplusnewpbws2012b_ab.RDS", "damagesplusnewpbws2013b_ab.RDS","damagesplusnewpbws2014b_ab.RDS","damagesplusnewpbws2015b_ab.RDS",
          "damagesplusnewpbws2010b_w.RDS", "damagesplusnewpbws2011b_w.RDS","damagesplusnewpbws2012b_w.RDS", "damagesplusnewpbws2013b_w.RDS","damagesplusnewpbws2014b_w.RDS","damagesplusnewpbws2015b_w.RDS") 

parcels<-c("parcel2005.RDS", "parcel2006.RDS", "parcel2007.RDS", "parcel2008.RDS", "parcel2009.RDS", "parcel2010.RDS", "parcel2011.RDS",
           "parcel2012.RDS","parcel2013.RDS","parcel2014.RDS","parcel2015.RDS",
           "parcel2006.RDS", "parcel2007.RDS", "parcel2008.RDS", "parcel2009.RDS", "parcel2010.RDS", "parcel2011.RDS",
           "parcel2012.RDS", "parcel2013.RDS","parcel2014.RDS","parcel2015.RDS",
           "parcel2010.RDS", "parcel2011.RDS",
           "parcel2012.RDS", "parcel2013.RDS","parcel2014.RDS","parcel2015.RDS",
           "parcel2010.RDS", "parcel2011.RDS",
           "parcel2012.RDS", "parcel2013.RDS","parcel2014.RDS","parcel2015.RDS")

year<-c(2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,
        2006, 2007, 2008, 2009, 2010,2011,2012,2013,2014,2015,
        2010,2011,2012,2013,2014,2015,
        2010,2011,2012,2013,2014,2015)

scenario<-c("ob","ob","ob","ob","ob","ob","ob","ob","ob","ob","ob",
            "nb","nb","nb","nb","nb","nb","nb","nb","nb","nb",
            "ab","ab","ab","ab","ab","ab",
            "w","w","w","w","w","w")

n<-length(names)
registerDoParallel(n)
getDoParWorkers() 

foreach (i=1:n) %dopar% {
  b<-readRDS(paste0(wd,"/data/new/", names[i]))
  p<-readRDS(paste0(wd, "/data/", parcels[i]))
  p<-st_as_sf(p)
  p<-st_transform(p, projection)
  p$LAND_USE<-as.numeric(p$LAND_USE)
  own<-c("METRO GOV'T  WW", "METRO GOV'T  WW  WATER & SEWER") #owner for bought out homes
  gs<-p[(p$LAND_USE == 1) | ((p$LAND_USE == 10) & p$OWNER %in% own),] #parks and rec areas and vacant residential land from buyouts, approx 4500 parcels
  gs<-gs[!is.na(gs$LAND_USE),] 
  
  test<- st_distance(b, gs) #now calculate distance from bldgs to greenspace
  test2<-test[,1]
  test2[]<-NA
  for (j in 1:length(test[,1])) {
    test2[j]<-min(test[j,])
  }
  test2<-as.numeric(test2)
  test2<-as.data.frame(test2)
  colnames(test2)<-c("DistGS")
  test3<-dplyr::bind_cols(b, test2)
  test3<-st_as_sf(test3)
  saveRDS(test3,paste0(wd,"/data/new/","pbwsGS",year[i], scenario[i], ".RDS"))
}

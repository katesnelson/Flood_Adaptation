#10

#This script consolidates building sclae information to unique parcel locations for later use in modeling, and computes
#aggregate statistics on parcels for each watershed.


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

#####################################################################
#Consolidate Building-Level Info to Unique Parcel Locations
#####################################################################

bldgfiles<-c("newpbwsGS2005ob.RDS", "newpbwsGS2006ob.RDS", "newpbwsGS2007ob.RDS", "newpbwsGS2008ob.RDS", "newpbwsGS2009ob.RDS", 
             "newpbwsGS2010ob.RDS", "newpbwsGS2011ob.RDS","newpbwsGS2012ob.RDS", "newpbwsGS2013ob.RDS","newpbwsGS2014ob.RDS","newpbwsGS2015ob.RDS",
             "newpbwsGS2006nb.RDS","newpbwsGS2007nb.RDS","newpbwsGS2008nb.RDS","newpbwsGS2009nb.RDS","newpbwsGS2010nb.RDS", 
             "newpbwsGS2011nb.RDS","newpbwsGS2012nb.RDS", "newpbwsGS2013nb.RDS","newpbwsGS2014nb.RDS","newpbwsGS2015nb.RDS",
             "newpbwsGS2010ab.RDS", "newpbwsGS2011ab.RDS","newpbwsGS2012ab.RDS", "newpbwsGS2013ab.RDS","newpbwsGS2014ab.RDS","newpbwsGS2015ab.RDS",
             "newpbwsGS2010w.RDS", "newpbwsGS2011w.RDS","newpbwsGS2012w.RDS", "newpbwsGS2013w.RDS","newpbwsGS2014w.RDS","newpbwsGS2015w.RDS")

parcelfiles<-c("parcel2005ws.RDS", "parcel2006ws.RDS", "parcel2007ws.RDS", "parcel2008ws.RDS", "parcel2009ws.RDS", "parcel2010ws.RDS", 
               "parcel2011ws.RDS", "parcel2012ws.RDS", "parcel2013ws.RDS","parcel2014ws.RDS","parcel2015ws.RDS",
               "parcel2006ws.RDS", "parcel2007ws.RDS", "parcel2008ws.RDS", "parcel2009ws.RDS", "parcel2010ws.RDS", "parcel2011ws.RDS",
               "parcel2012ws.RDS", "parcel2013ws.RDS","parcel2014ws.RDS","parcel2015ws.RDS",
               "parcel2010ws.RDS", "parcel2011ws.RDS", "parcel2012ws.RDS", "parcel2013ws.RDS","parcel2014ws.RDS","parcel2015ws.RDS",
               "parcel2010ws.RDS", "parcel2011ws.RDS", "parcel2012ws.RDS", "parcel2013ws.RDS","parcel2014ws.RDS","parcel2015ws.RDS")

scenario<-c("ob","ob","ob","ob","ob","ob","ob","ob","ob","ob","ob",
            "nb","nb","nb","nb","nb","nb","nb","nb","nb","nb",
            "ab","ab","ab","ab","ab","ab",
            "w","w","w","w","w","w")

year<-c(2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,
        2006,2007, 2008, 2009, 2010,2011,2012,2013,2014,2015,
        2010,2011,2012,2013,2014,2015,
        2010,2011,2012,2013,2014,2015)

n<-length(parcelfiles)
registerDoParallel(n)
getDoParWorkers() 

foreach (i=1:n) %dopar% {
  p<-readRDS(paste0(wd,"/data/",parcelfiles[i]))
  b<-readRDS(paste0(wd,"/data/new/",bldgfiles[i]))
  test<-st_as_sf(b) #convert to simpler spatial format (sf)
  test2<-st_as_sf(p)
  
  sums<- as.data.frame(test %>% group_by (STANPAR) %>% summarise (countB=length(unique(BLDG_ID)), totBLDGIA=sum(BLDG_Area, na.rm=T), pInun=max(inundate, na.rm=T), 
                                                                  pfloodway=max(floodway, na.rm=T), pfema100yr = max(fema100yr, na.rm=T), pfema500yr = max(fema500yr, na.rm=T), pdleveldepth =max(DLEVEL_depth, na.rm=T),
                                                                  pdleveldepthfreeb=max(DLEVEL_freeb, na.rm=T),pUSACE100 = max(USACE_100, na.rm=T), pUSACE500 = max(USACE_500yr, na.rm=T), pUSACE1000= max(USACE_1000yr, na.rm=T),
                                                                  pUSACE100freeb= max(USACE_100_freeb, na.rm=T), pUSACE500freeb = max(USACE_500yr_freeb, na.rm=T), pUSACE1000freeb= max(USACE_1000yr_freeb, na.rm=T),
                                                                  pDMGWDFIA = max(DMG_WD_FIA,na.rm=T), pDMGWDSV = sum(DMG_WD_SV, na.rm=T), pDMG100FIA= max(DMG_100_FIA, na.rm=T), pDMG500FIA= max(DMG_500_FIA, na.rm=T), 
                                                                  pDMG1000FIA= max(DMG_1000_FIA, na.rm=T),pDMG100SV= sum(DMG_100_SV, na.rm=T), pDMG500SV= sum(DMG_500_SV, na.rm=T), pDMG1000SV= sum(DMG_1000_SV, na.rm=T),
                                                                  pDMGWDFIAfreeb = max(DMG_WD_FIA_freeb,na.rm=T), pDMGWDSVfreeb = sum(DMG_WD_SV_freeb, na.rm=T), pDMG100FIAfreeb= max(DMG_100_FIA_freeb, na.rm=T), pDMG500FIAfreeb= max(DMG_500yr_FIA_freeb, na.rm=T), 
                                                                  pDMG1000FIAfreeb= max(DMG_1000yr_FIA_freeb, na.rm=T),pDMG100SVfree= sum(DMG_100_SV_freeb, na.rm=T), pDMG500SVfreeb= sum(DMG_500_SV_freeb, na.rm=T), pDMG1000SVfreeb= sum(DMG_1000_SV_freeb, na.rm=T),
                                                                  pDMGCONTWDFIA = max(DMGCONT_WD_FIA,na.rm=T), pDMGCONT100FIA= max(DMGCONT_100_FIA, na.rm=T), pDMGCONT500FIA= max(DMGCONT_500yr_FIA, na.rm=T), pDMGCONT1000FIA= max(DMGCONT_1000yr_FIA, na.rm=T),
                                                                  pDMGCONTWDFIAfreeb = max(DMGCONT_WD_FIA_freeb,na.rm=T), pDMGCONT100FIAfreeb= max(DMGCONT_100_FIA_freeb, na.rm=T), pDMGCONT500FIAfreeb= max(DMGCONT_500yr_FIA_freeb, na.rm=T), pDMGCONT1000FIAfreeb= max(DMGCONT_1000yr_FIA_freeb, na.rm=T),
                                                                  prelocWDHZ = sum(Reloc_WD_HZ,na.rm=T), prelocWDSV = sum(Reloc_WD_SV, na.rm=T), preloc100HZ= sum(Reloc_100_HZ, na.rm=T), preloc500HZ= sum(Reloc_500yr_HZ, na.rm=T), 
                                                                  preloc1000HZ= sum(Reloc_1000yr_HZ, na.rm=T),preloc100SV= sum(Reloc_100_SV, na.rm=T), preloc500SV= sum(Reloc_500yr_SV, na.rm=T), preloc1000SV= sum(Reloc_1000yr_SV, na.rm=T),
                                                                  prelocWDHZfreeb = sum(Reloc_WD_HZ_freeb,na.rm=T), prelocWDSVfreeb = sum(Reloc_WD_SV_freeb, na.rm=T), preloc100HZfreeb= sum(Reloc_100_HZ_freeb, na.rm=T), preloc500HZfreeb= sum(Reloc_500yr_HZ_freeb, na.rm=T), 
                                                                  preloc1000HZfreeb= sum(Reloc_1000yr_HZ_freeb, na.rm=T),preloc100SVfreeb= sum(Reloc_100_SV_freeb, na.rm=T), preloc500SVfreeb= sum(Reloc_500yr_SV_freeb, na.rm=T), preloc1000SVfreeb= sum(Reloc_1000yr_SV_freeb, na.rm=T),
                                                                  plaborWDSV = sum(Labor_WD_SV, na.rm=T), plabor100SV= sum(Labor_100_SV, na.rm=T), plabor500SV= sum(Labor_500yr_SV, na.rm=T), plabor1000SV= sum(Labor_1000yr_SV, na.rm=T),
                                                                  plaborWDSVfreeb = sum(Labor_WD_SV_freeb, na.rm=T), plabor100SVfreeb= sum(Labor_100_SV_freeb, na.rm=T), plabor500SVfreeb= sum(Labor_500yr_SV_freeb, na.rm=T), plabor1000SVfreeb= sum(Labor_1000yr_SV_freeb, na.rm=T),
                                                                  pDistGS = mean(DistGS), pDistStr=mean(DistStr))) #consolidate info for multiple buildings on same parcel 
  
  test3<-left_join(test, sums, by = "STANPAR") #merge back with building info
  test4<-arrange(test3, desc(totDU)) #sort from most DU identified per building on parcel to fewest
  test5<-test4[duplicated(test4[,3])==FALSE, ] #remove duplicates by STANPAR (keeps first (consolidated DU) by default), left with just one building per parcel
  #st_geometry(test5)<-NULL
  
  #merge with parcels
  test2<-test2[,c(1,2,5,10,28,30,31,33)]
  test6<-left_join(test2, test5, by="STANPAR")#join all the build data to parcels
  
  #remove duplicate geometries for parcels in same location 
  test7<-arrange(test6, desc(totDU))
  test7<-st_as_sf(test7)
  test7$PID<-seq(1:length(test7$STANPAR)) #parcel index
  center<-st_centroid(test7)
  test8b<-center[duplicated(center$geometry.x)==FALSE,] #removes ~424 parcels
  #test8<-test7[duplicated(test7$geometry.x)==FALSE,] #removes ~373 parcels
  test9<-test7[test7$PID %in% test8b$PID,] #keep parcels and parcel shapes for unique parcel locations
  
  saveRDS(test9, paste0(wd,"/data/new/", "newparcels", year[i], scenario[i], ".RDS"))
}

#####################################################################
#Consolidate Building-Level Info to Unique Parcel Locations
#####################################################################

bldgfiles<-c("newpbwsGS2005ob.RDS", "newpbwsGS2006ob.RDS", "newpbwsGS2007ob.RDS", "newpbwsGS2008ob.RDS", "newpbwsGS2009ob.RDS", 
             "newpbwsGS2010ob.RDS", "newpbwsGS2011ob.RDS","newpbwsGS2012ob.RDS", "newpbwsGS2013ob.RDS","newpbwsGS2014ob.RDS","newpbwsGS2015ob.RDS",
             "newpbwsGS2006nb.RDS","newpbwsGS2007nb.RDS","newpbwsGS2008nb.RDS","newpbwsGS2009nb.RDS","newpbwsGS2010nb.RDS", 
             "newpbwsGS2011nb.RDS","newpbwsGS2012nb.RDS", "newpbwsGS2013nb.RDS","newpbwsGS2014nb.RDS","newpbwsGS2015nb.RDS",
             "newpbwsGS2010ab.RDS", "newpbwsGS2011ab.RDS","newpbwsGS2012ab.RDS", "newpbwsGS2013ab.RDS","newpbwsGS2014ab.RDS","newpbwsGS2015ab.RDS",
             "newpbwsGS2010w.RDS", "newpbwsGS2011w.RDS","newpbwsGS2012w.RDS", "newpbwsGS2013w.RDS","newpbwsGS2014w.RDS","newpbwsGS2015w.RDS")

parcelfiles<-c("parcel2005ws.RDS", "parcel2006ws.RDS", "parcel2007ws.RDS", "parcel2008ws.RDS", "parcel2009ws.RDS", "parcel2010ws.RDS", 
               "parcel2011ws.RDS", "parcel2012ws.RDS", "parcel2013ws.RDS","parcel2014ws.RDS","parcel2015ws.RDS",
               "parcel2006ws.RDS", "parcel2007ws.RDS", "parcel2008ws.RDS", "parcel2009ws.RDS", "parcel2010ws.RDS", "parcel2011ws.RDS",
               "parcel2012ws.RDS", "parcel2013ws.RDS","parcel2014ws.RDS","parcel2015ws.RDS",
               "parcel2010ws.RDS", "parcel2011ws.RDS", "parcel2012ws.RDS", "parcel2013ws.RDS","parcel2014ws.RDS","parcel2015ws.RDS",
               "parcel2010ws.RDS", "parcel2011ws.RDS", "parcel2012ws.RDS", "parcel2013ws.RDS","parcel2014ws.RDS","parcel2015ws.RDS")

scenario<-c("ob","ob","ob","ob","ob","ob","ob","ob","ob","ob","ob",
            "nb","nb","nb","nb","nb","nb","nb","nb","nb","nb",
            "ab","ab","ab","ab","ab","ab",
            "w","w","w","w","w","w")

year<-c(2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,
        2006,2007, 2008, 2009, 2010,2011,2012,2013,2014,2015,
        2010,2011,2012,2013,2014,2015,
        2010,2011,2012,2013,2014,2015)

n<-length(parcelfiles)
registerDoParallel(n)
getDoParWorkers() 

foreach (i=1:n) %dopar% {
  p<-readRDS(paste0(wd,"/data/",parcelfiles[i]))
  b<-readRDS(paste0(wd,"/data/new/",bldgfiles[i]))
  test<-st_as_sf(b) #convert to simpler spatial format (sf)
  test2<-st_as_sf(p)
  
  sums<- as.data.frame(test %>% group_by (STANPAR) %>% summarise (countB=length(unique(BLDG_ID)), totBLDGIA=sum(BLDG_Area, na.rm=T), pInun=max(inundate, na.rm=T), 
                                                                  pfloodway=max(floodway, na.rm=T), pfema100yr = max(fema100yr, na.rm=T), pfema500yr = max(fema500yr, na.rm=T), pdleveldepth =max(DLEVEL_depth, na.rm=T),
                                                                  pdleveldepthfreeb=max(DLEVEL_freeb, na.rm=T),pUSACE100 = max(USACE_100, na.rm=T), pUSACE500 = max(USACE_500yr, na.rm=T), pUSACE1000= max(USACE_1000yr, na.rm=T),
                                                                  pUSACE100freeb= max(USACE_100_freeb, na.rm=T), pUSACE500freeb = max(USACE_500yr_freeb, na.rm=T), pUSACE1000freeb= max(USACE_1000yr_freeb, na.rm=T),
                                                                  pDMGWDFIA = max(DMG_WD_FIA,na.rm=T), pDMGWDSV = sum(DMG_WD_SV, na.rm=T), pDMG100FIA= max(DMG_100_FIA, na.rm=T), pDMG500FIA= max(DMG_500_FIA, na.rm=T), 
                                                                  pDMG1000FIA= max(DMG_1000_FIA, na.rm=T),pDMG100SV= sum(DMG_100_SV, na.rm=T), pDMG500SV= sum(DMG_500_SV, na.rm=T), pDMG1000SV= sum(DMG_1000_SV, na.rm=T),
                                                                  pDMGWDFIAfreeb = max(DMG_WD_FIA_freeb,na.rm=T), pDMGWDSVfreeb = sum(DMG_WD_SV_freeb, na.rm=T), pDMG100FIAfreeb= max(DMG_100_FIA_freeb, na.rm=T), pDMG500FIAfreeb= max(DMG_500yr_FIA_freeb, na.rm=T), 
                                                                  pDMG1000FIAfreeb= max(DMG_1000yr_FIA_freeb, na.rm=T),pDMG100SVfree= sum(DMG_100_SV_freeb, na.rm=T), pDMG500SVfreeb= sum(DMG_500_SV_freeb, na.rm=T), pDMG1000SVfreeb= sum(DMG_1000_SV_freeb, na.rm=T),
                                                                  pDMGCONTWDFIA = max(DMGCONT_WD_FIA,na.rm=T), pDMGCONT100FIA= max(DMGCONT_100_FIA, na.rm=T), pDMGCONT500FIA= max(DMGCONT_500yr_FIA, na.rm=T), pDMGCONT1000FIA= max(DMGCONT_1000yr_FIA, na.rm=T),
                                                                  pDMGCONTWDFIAfreeb = max(DMGCONT_WD_FIA_freeb,na.rm=T), pDMGCONT100FIAfreeb= max(DMGCONT_100_FIA_freeb, na.rm=T), pDMGCONT500FIAfreeb= max(DMGCONT_500yr_FIA_freeb, na.rm=T), pDMGCONT1000FIAfreeb= max(DMGCONT_1000yr_FIA_freeb, na.rm=T),
                                                                  prelocWDHZ = sum(Reloc_WD_HZ,na.rm=T), prelocWDSV = sum(Reloc_WD_SV, na.rm=T), preloc100HZ= sum(Reloc_100_HZ, na.rm=T), preloc500HZ= sum(Reloc_500yr_HZ, na.rm=T), 
                                                                  preloc1000HZ= sum(Reloc_1000yr_HZ, na.rm=T),preloc100SV= sum(Reloc_100_SV, na.rm=T), preloc500SV= sum(Reloc_500yr_SV, na.rm=T), preloc1000SV= sum(Reloc_1000yr_SV, na.rm=T),
                                                                  prelocWDHZfreeb = sum(Reloc_WD_HZ_freeb,na.rm=T), prelocWDSVfreeb = sum(Reloc_WD_SV_freeb, na.rm=T), preloc100HZfreeb= sum(Reloc_100_HZ_freeb, na.rm=T), preloc500HZfreeb= sum(Reloc_500yr_HZ_freeb, na.rm=T), 
                                                                  preloc1000HZfreeb= sum(Reloc_1000yr_HZ_freeb, na.rm=T),preloc100SVfreeb= sum(Reloc_100_SV_freeb, na.rm=T), preloc500SVfreeb= sum(Reloc_500yr_SV_freeb, na.rm=T), preloc1000SVfreeb= sum(Reloc_1000yr_SV_freeb, na.rm=T),
                                                                  plaborWDSV = sum(Labor_WD_SV, na.rm=T), plabor100SV= sum(Labor_100_SV, na.rm=T), plabor500SV= sum(Labor_500yr_SV, na.rm=T), plabor1000SV= sum(Labor_1000yr_SV, na.rm=T),
                                                                  plaborWDSVfreeb = sum(Labor_WD_SV_freeb, na.rm=T), plabor100SVfreeb= sum(Labor_100_SV_freeb, na.rm=T), plabor500SVfreeb= sum(Labor_500yr_SV_freeb, na.rm=T), plabor1000SVfreeb= sum(Labor_1000yr_SV_freeb, na.rm=T),
                                                                  pDistGS = mean(DistGS), pDistStr=mean(DistStr))) #consolidate info for multiple buildings on same parcel 
  
  test3<-left_join(test, sums, by = "STANPAR") #merge back with building info
  test4<-arrange(test3, desc(totDU)) #sort from most DU identified per building on parcel to fewest
  test5<-test4[duplicated(test4[,3])==FALSE, ] #remove duplicates by STANPAR (keeps first (consolidated DU) by default), left with just one building per parcel
  #st_geometry(test5)<-NULL
  
  #merge with parcels
  test2<-test2[,c(1,2,5,10,28,30,31,33)]
  test6<-left_join(test2, test5, by="STANPAR")#join all the build data to parcels
  
  #remove duplicate geometries for parcels in same location 
  test7<-arrange(test6, desc(totDU))
  test7<-st_as_sf(test7)
  test7$PID<-seq(1:length(test7$STANPAR)) #parcel index
  center<-st_centroid(test7)
  test8b<-center[duplicated(center$geometry.x)==FALSE,] #removes ~424 parcels
  #test8<-test7[duplicated(test7$geometry.x)==FALSE,] #removes ~373 parcels
  test9<-test7[test7$PID %in% test8b$PID,] #keep parcels and parcel shapes for unique parcel locations
  
  saveRDS(test9, paste0(wd,"/data/new/", "newparcels", year[i], scenario[i], ".RDS"))
}


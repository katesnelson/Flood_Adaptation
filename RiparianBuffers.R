#11

#This script delineates built-up areas by watershed and removes them from the riparian buffer zone and calculates 
#riparian buffer widths and un-built riparian buffer areas for each watershed. It also conducts spatial joins with
#buffer planting areas (data provided by MWS) to provide estimates of numbers of trees planted on buyout parcels
#and the contribution of these trees to stormwater runoff absorption in each watershed.


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



########################################################################
#Calculate Riparian Buffer Area and Width and intersect with watersheds
########################################################################

#names of building files for scenarios and years
pbwsnames<-c("pbwsGS2005ob.RDS", "pbwsGS2006ob.RDS", "pbwsGS2007ob.RDS", "pbwsGS2008ob.RDS", "pbwsGS2009ob.RDS", "pbwsGS2010ob.RDS", "pbwsGS2011ob.RDS",
             "pbwsGS2012ob.RDS", "pbwsGS2013ob.RDS","pbwsGS2014ob.RDS","pbwsGS2015ob.RDS",
             "pbwsGS2006nb.RDS","pbwsGS2007nb.RDS","pbwsGS2008nb.RDS","pbwsGS2009nb.RDS","pbwsGS2010nb.RDS", "pbwsGS2011nb.RDS","pbwsGS2012nb.RDS", "pbwsGS2013nb.RDS","pbwsGS2014nb.RDS","pbwsGS2015nb.RDS",
             "pbwsGS2010ab.RDS", "pbwsGS2011ab.RDS","pbwsGS2012ab.RDS", "pbwsGS2013ab.RDS","pbwsGS2014ab.RDS","pbwsGS2015ab.RDS",
             "pbwsGS2010w.RDS", "pbwsGS2011w.RDS","pbwsGS2012w.RDS", "pbwsGS2013w.RDS","pbwsGS2014w.RDS","pbwsGS2015w.RDS") 

subwsnames<-c("subwsnewparcels2005ob.RDS", "subwsnewparcels2006ob.RDS", "subwsnewparcels2007ob.RDS", "subwsnewparcels2008ob.RDS", "subwsnewparcels2009ob.RDS", "subwsnewparcels2010ob.RDS", "subwsnewparcels2011ob.RDS",
              "subwsnewparcels2012ob.RDS", "subwsnewparcels2013ob.RDS","subwsnewparcels2014ob.RDS","subwsnewparcels2015ob.RDS","subwsnewparcels2006nb.RDS", "subwsnewparcels2007nb.RDS", "subwsnewparcels2008nb.RDS", "subwsnewparcels2009nb.RDS", "subwsnewparcels2010nb.RDS", "subwsnewparcels2011nb.RDS",
              "subwsnewparcels2012nb.RDS", "subwsnewparcels2013nb.RDS","subwsnewparcels2014nb.RDS","subwsnewparcels2015nb.RDS",
              "subwsnewparcels2010ab.RDS", "subwsnewparcels2011ab.RDS","subwsnewparcels2012ab.RDS", "subwsnewparcels2013ab.RDS","subwsnewparcels2014ab.RDS","subwsnewparcels2015ab.RDS",
              "subwsnewparcels2010w.RDS", "subwsnewparcels2011w.RDS","subwsnewparcels2012w.RDS", "subwsnewparcels2013w.RDS","subwsnewparcels2014w.RDS","subwsnewparcels2015w.RDS")

#identify built_up areas and erase from 100 meter riparian stream buffer to identify "protectective" riparian buffer area

n<-length(pbwsnames)
registerDoParallel(n)
getDoParWorkers() 

foreach (i=1:n) %dopar% {
  
  p<-readRDS(paste0(wd,"/data/new/","new",pbwsnames[i]))
  p$DistStr<-as.numeric(p$DistStr)
  
  
  #initialize a sfc object with first watershed built area
  riparian_zones<- p[p$Riparian %in% "RipZone",] #as we want to examine overlap with riparian buffer conduct processes only on watersheds that intersect with the buffer
  watershed_list<-unique(riparian_zones$WS_ID) #unique watersheds that intersect with riparian buffer zone
  p3<-riparian_zones[riparian_zones$WS_ID %in% watershed_list[1],] #take the first watershed int he list for initializing
  p4<-st_union(p3)
  t<-st_triangulate(p4, dTolerance=1) #triangulate between buildings
  t<-st_cast(t) #convert to multipoygon
  t<-st_sf(t)
  
  t$areas<-st_area(t) #calculate areas of triangulations
  t$areas<-as.numeric(t$areas)
  ave_area<-mean(t$areas)
  std_area<-sd(t$areas)
  tolerance<-ave_area + 2*std_area
  t2<-t[t$areas <= tolerance,]          #subset to smaller triangulations
  t2<-t2[!is.na(t2$areas), ]
  
  t3<-t2
  t3<-st_cast(t3,"MULTILINESTRING")  #convert to line strings
  t3$length<-st_length(t3)
  t3$length<-as.numeric(t3$length)
  ave_length<-mean(t3$length)
  std_length<-sd(t3$length)
  tolerance<-ave_length +std_length
  t4<-t3[t3$length <= tolerance,]    #subset to shorter lines
  
  tfinal<-st_cast(t4,"MULTIPOLYGON")  #convert back to multipolygon
  tfinal<-st_transform(tfinal, projection)
  # plot(tfinal[,1])
  # plot(p3[,1])
  # plot(tfinal[,3], border="black", col=NULL, add=TRUE)
  #t2<-st_union(st_combine(t2))
  built_area<-tfinal
  #plot(built_area[,1])
  
  # Start the clock!
  ptm <- proc.time()
  #now build the built areas for other watersheds and merge with the initalized object, takes about 1.5 hrs
  for (j in unique(watershed_list[2:length(watershed_list)])) {
    p3<-p[p$WS_ID %in% j,]
    p4<-st_union(p3)
    t<-st_triangulate(p4, dTolerance=1) #triangulate between buildings
    t<-st_cast(t) #convert to multipoygon
    t<-st_sf(t)
    
    t$areas<-st_area(t) #calculate areas of triangulations
    t$areas<-as.numeric(t$areas)
    ave_area<-mean(t$areas)
    std_area<-sd(t$areas)
    tolerance<-ave_area + 2*std_area
    t2<-t[t$areas <= tolerance,]          #subset to smaller triangulations
    t2<-t2[!is.na(t2$areas), ]
    
    t3<-t2
    t3<-st_cast(t3,"MULTILINESTRING")  #convert to line strings
    t3$length<-st_length(t3)
    t3$length<-as.numeric(t3$length)
    ave_length<-mean(t3$length)
    std_length<-sd(t3$length)
    tolerance<-ave_length +std_length
    t4<-t3[t3$length <= tolerance,]    #subset to shorter lines
    
    tfinal<-st_cast(t4,"MULTIPOLYGON")  #convert back to multipolygon
    tfinal<-st_transform(tfinal, projection)
    
    built_area<-rbind(built_area,tfinal) #merge the traingulations for each watershed
    
  }
  proc.time() - ptm
  saveRDS(built_area, paste0(wd,"/data/new/","builtA",pbwsnames[i]))
  rm(built_area)
}

#plot(built_area[,1])
rm(built_area, p,p3,p4,riparian_zones,t,t2,t3,t4,tfinal,tnew)
#Now remove built-up areas from 100meter riparian stream buffer and calculate riparian width

#A 100 meter buffer around all NHD stream and river areas
ripzone<-shapefile(paste0(wd,"/data/","full_100m_stream_buffer.shp"))
ripzone<-st_as_sf(ripzone)
ripzone<-st_transform(ripzone, projection) 
ripzone<-ripzone[,c(1,5)]
ripzone[,1]<-"RipZone"

streams<-shapefile(paste0(wd,"/data/","NHDstreams.shp"))
streams<-st_as_sf(streams)
streams<-st_transform(streams, projection)
streams<-st_union(streams)

n<-length(pbwsnames)
registerDoParallel(n)
getDoParWorkers() 

foreach (i=1:n) %dopar% {
  ba<-readRDS(paste0(wd,"/data/new/","builtA", pbwsnames[i]))
  tolerance<-mean(ba$areas,na.rm=T) + 2*sd(ba$areas,na.rm=T)
  ba2<-ba[ba$areas <= tolerance,]          #subset to smaller triangulations
  ba2<-ba2[!is.na(ba2$areas), ]
  tolerance<-mean(ba2$length) + sd(ba2$length)
  ba2<-ba2[ba2$length <= tolerance,]  
  ba<-ba2
  
  ba<-st_combine(ba) #prep for difference
  ba<-st_union(st_buffer(ba,0))   #prep for difference
  
  ptm <- proc.time()
  riparea<-st_difference(st_buffer(ripzone, 0), st_buffer(ba, 0)) #remove built-up area from full riparian area
  proc.time() - ptm
  
  saveRDS(riparea, paste0(wd,"/data/new/","RA", pbwsnames[i]))
  
  r<-st_cast(riparea, "MULTIPOLYGON") #prep for distance calculation
  r<-st_cast(r,"POINT")
  
  ptm <- proc.time()
  ripwidth<-st_distance(r,streams)#calculate distance from riparian area points to streams
  proc.time() - ptm
  
  ptm <- proc.time()
  test2<-ripwidth[,1]
  test2[]<-NA
  for (j in 1:length(ripwidth[,1])) {
    test2[j]<-min(ripwidth[j,])          #use the shortest distance from each point on the riparian area boundary to the stream
  }
  test2<-as.numeric(test2)
  test2<-as.data.frame(test2)
  colnames(test2)<-c("WidthRip")
  test3<-dplyr::bind_cols(r, test2)
  test3<-st_as_sf(test3)
  test3<-test3[test3$WidthRip > 1,]
  proc.time() - ptm
  
  saveRDS(test3,paste0(wd,"/data/new/", "RWID",pbwsnames[i]))
}

#now intersect riparian areas and riparian width points with watersheds
n<-length(pbwsnames)
registerDoParallel(n)
getDoParWorkers() 

foreach (i=1:n) %dopar% {
  ra<-readRDS(paste0(wd,"/data/new/","RA",pbwsnames[i]))
  sws<-readRDS(paste0(wd,"/data/new/",subwsnames[i]))
  test<-sf::st_intersection(sws, ra) %>% mutate(rip_area = as.numeric(st_area(.)))  #caluclate area of intersection between watersheds and riparian areas
  #test<-test[, c(29,30,33)]  #keep only parcel id, ws_id and intersection area
  test<-st_as_sf(test)
  st_geometry(test)<- NULL #remove geometry as it keeps the riparian area geom
  test<-left_join(sws,test[,c(1,31,32)],by="WS_ID") #merge new riparian data back with watershed geometries
  
  rw<-readRDS(paste0(wd,"/data/new/", "RWID",pbwsnames[i]))
  test2<-st_intersection(rw,sws)
  test3<-test2 %>% group_by (WS_ID) %>% summarise (AveRW = mean(WidthRip, na.rm=T), MinRW = min(WidthRip, na.rm=T), MaxRW = max(WidthRip, na.rm=T))
  test4<-left_join(test,test3, by="WS_ID")
  saveRDS(test4, paste0(wd,"/data/new/", "wsRip", subwsnames[i]))
  
}

#t<-readRDS(paste0(wd,"/data/", "wsRip", subwsnames[1]))

#finally add the new watershed data to the parcel shapefiles, and add tree water uptake estimates by watershed

pnames<-c("newparcels2005ob.RDS", "newparcels2006ob.RDS", "newparcels2007ob.RDS", "newparcels2008ob.RDS", "newparcels2009ob.RDS", "newparcels2010ob.RDS", "newparcels2011ob.RDS",
          "newparcels2012ob.RDS", "newparcels2013ob.RDS","newparcels2014ob.RDS","newparcels2015ob.RDS", "newparcels2006nb.RDS", "newparcels2007nb.RDS", "newparcels2008nb.RDS", "newparcels2009nb.RDS", "newparcels2010nb.RDS", "newparcels2011nb.RDS",
          "newparcels2012nb.RDS", "newparcels2013nb.RDS","newparcels2014nb.RDS","newparcels2015nb.RDS",
          "newparcels2010ab.RDS", "newparcels2011ab.RDS","newparcels2012ab.RDS", "newparcels2013ab.RDS","newparcels2014ab.RDS","newparcels2015ab.RDS",
          "newparcels2010w.RDS", "newparcels2011w.RDS","newparcels2012w.RDS", "newparcels2013w.RDS","newparcels2014w.RDS","newparcels2015w.RDS") 

n<-length(pnames)
registerDoParallel(n)
getDoParWorkers() 

foreach (i=1:n) %dopar% {
  p<-readRDS(paste0(wd,"/data/new/","updated",pnames[i]))
  s<-readRDS(paste0(wd,"/data/new/", "wsRip", subwsnames[i]))
  #clean the parcel file
  keep<-  c("STANPAR","OWNER.x", "parea.x","WS_ID.x" , "BLDG_ID" , "BLDG_Area" , "ACQUIRED_D" , "SALE_PRICE"  , "PROP_ADDR" , "PROP_HOUSE", "PROP_STREE"  ,"PROP_ZIP",
            "ACREAGE_TE" , "LAND_USE.y","LAND_USE_D","LA_SQ_FT" ,"DU_COUNT" ,"ASSD_DTE" ,"LAND_APPR", "IMPR_APPR" , "TOTAL_APPR", "Met_Aq_D" , "Aq_Cost" ,"Future",
            "buyout" ,"DLEVEL","FLOODZONE" , "GENUSE","PID", "totLA","totDU" , "totLAPR" , "totIAPR", "totAPR", "meanIAPR" , "meanAPR" ,"countP" , "inundate", "floodway",
            "fema100yr", "fema500yr", "PlantingSITE" , "TreesPlanted" , "Riparian" , "DistStr" , "Aq_year" ,"BLD_Year" , "countB", "totBLDGIA", "pInun" , "pfloodway" ,"pfema100yr" ,
            "pfema500yr" , "pdleveldepth","pdleveldepthfreeb", "pUSACE100", "pUSACE500" , "pUSACE1000", "pUSACE100freeb", "pUSACE500freeb", "pUSACE1000freeb", "pDMGWDFIA", "pDMGWDSV",
            "pDMG100FIA" , "pDMG500FIA" , "pDMG1000FIA", "pDMG100SV", "pDMG500SV" , "pDMG1000SV","pDMGWDFIAfreeb" , "pDMGWDSVfreeb", "pDMG100FIAfreeb" , "pDMG500FIAfreeb",
            "pDMG1000FIAfreeb" , "pDMG100SVfree", "pDMG500SVfreeb" ,"pDMG1000SVfreeb" , "pDMGCONTWDFIA", "pDMGCONT100FIA" , "pDMGCONT500FIA" , "pDMGCONT1000FIA" , "pDMGCONTWDFIAfreeb",
            "pDMGCONT100FIAfreeb", "pDMGCONT500FIAfreeb" , "pDMGCONT1000FIAfreeb", "prelocWDHZ" , "prelocWDSV"  ,"preloc100HZ" , "preloc500HZ" , "preloc1000HZ", "preloc100SV",
            "preloc500SV" , "preloc1000SV"  , "prelocWDHZfreeb" , "prelocWDSVfreeb" , "preloc100HZfreeb" , "preloc500HZfreeb" , "preloc1000HZfreeb", "preloc100SVfreeb" ,
            "preloc500SVfreeb", "preloc1000SVfreeb" , "plaborWDSV" , "plabor100SV" , "plabor500SV" , "plabor1000SV" ,"plaborWDSVfreeb","plabor100SVfreeb" , "plabor500SVfreeb",
            "plabor1000SVfreeb" , "pDistGS", "pDistStr"  ,"wsarea" , "wsbldgarea.y" , "wsboarea.y" , "wsbowisharea.y" , "bldgcnt.y", "bocnt.y" , "bowishcnt.y" , "indcnt.y" ,
            "boind.y" , "bowishind.y", "percbld.y" , "percbo.y" ,"CN.y" , "S.y" , "Q.y" , "IA.y" , "Vrunoff.y", "Vabsorb.y" ,"pcnt" , "bopcnt" , "bopwishcnt" , "indpcnt", 
            "bopind", "bopwishind", "bldgAppr" , "boAppr" , "indboAppr" , "boAqCst" , "indboAqC" ,"geometry.x"  )               
  # c("STANPAR", "OWNER.x","parea.x",  "WS_ID.x", "BLDG_ID", "BLDG_Area","ACQUIRED_D","SALE_PRICE", "PROP_ADDR", "PROP_HOUSE", "PROP_STREE",     
  #        "PROP_ZIP", "ACREAGE_TE",  "LAND_USE_D","LA_SQ_FT", "DU_COUNT",   "ASSD_DTE",       "LAND_APPR",      "IMPR_APPR",      "TOTAL_APPR",     "Met_Aq_D",
  #        "Aq_Cost", "Future",         "buyout",         "DLEVEL",         "FLOODZONE",      "GENUSE",        "PID",       "totLA",  "totDU",          "totLAPR",
  #        "totIAPR",        "totAPR",         "meanIAPR",       "meanAPR",        "countP",         "inundate",       "floodway", "fema100yr",      "fema500yr",
  #        "PlantingSITE",   "TreesPlanted",   "Riparian",       "DistStr",        "USACE_100",      "USACE_500yr",    "USACE_1000yr", "DMG_WD_FIA",     "DMG_WD_SV",
  #        "DMG_100_FIA",    "DMG_500_FIA",    "DMG_1000_FIA",   "DMG_100_SV",     "DMG_500_SV",     "DMG_1000_SV",    "DistGS",   "wsbldgarea.x",   "wsboarea.x",
  #        "wsbowisharea.x", "bldgcnt.x" ,     "bocnt.x",        "bowishcnt.x",    "indcnt.x",       "boind.x", "bowishind.x",    "percbld.x",      "percbo.x",
  #        "CN.x",           "S.x",            "Q.x",            "IA.x",           "Vrunoff.x" ,     "Vabsorb.x", "countB",         "totBLDGIA",      "pInun",
  #        "pfloodway",      "pfema100yr",     "pfema500yr",     "pUSACE100",      "pUSACE500",      "pUSACE1000", "pDMG100FIA",     "pDMG500FIA",     "pDMG1000FIA",
  #        "pDMG100SV",      "pDMG500SV",      "pDMG1000SV",     "pDistGS",        "pDistStr",       "wsarea",   "pcnt", "bopcnt",         "bopwishcnt",
  #        "indpcnt",        "bopind",         "bopwishind",     "bldgAppr",       "boAppr",         "indboAppr",      "boAqCst", "indboAqC",   "geometry.x" )
  p2<-p[, names(p) %in% keep]
  names(p2)<- c("STANPAR","OWNER", "parea","WS_ID" , "BLDG_ID" , "BLDG_Area" , "ACQUIRED_D" , "SALE_PRICE"  , "PROP_ADDR" , "PROP_HOUSE", "PROP_STREE"  ,"PROP_ZIP",
                "ACREAGE_TE" , "LAND_USE","LAND_USE_D","LA_SQ_FT" ,"DU_COUNT" ,"ASSD_DTE" ,"LAND_APPR", "IMPR_APPR" , "TOTAL_APPR", "Met_Aq_D" , "Aq_Cost" ,"Future",
                "buyout" ,"DLEVEL","FLOODZONE" , "GENUSE","PID", "totLA","totDU" , "totLAPR" , "totIAPR", "totAPR", "meanIAPR" , "meanAPR" ,"countP" , "inundate", "floodway",
                "fema100yr", "fema500yr", "PlantingSITE" , "TreesPlanted" , "Riparian" , "DistStr" , "Aq_year" ,"BLD_Year" , "countB", "totBLDGIA", "pInun" , "pfloodway" ,"pfema100yr" ,
                "pfema500yr" , "pdleveldepth","pdleveldepthfreeb", "pUSACE100", "pUSACE500" , "pUSACE1000", "pUSACE100freeb", "pUSACE500freeb", "pUSACE1000freeb", "pDMGWDFIA", "pDMGWDSV",
                "pDMG100FIA" , "pDMG500FIA" , "pDMG1000FIA", "pDMG100SV", "pDMG500SV" , "pDMG1000SV","pDMGWDFIAfreeb" , "pDMGWDSVfreeb", "pDMG100FIAfreeb" , "pDMG500FIAfreeb",
                "pDMG1000FIAfreeb" , "pDMG100SVfree", "pDMG500SVfreeb" ,"pDMG1000SVfreeb" , "pDMGCONTWDFIA", "pDMGCONT100FIA" , "pDMGCONT500FIA" , "pDMGCONT1000FIA" , "pDMGCONTWDFIAfreeb",
                "pDMGCONT100FIAfreeb", "pDMGCONT500FIAfreeb" , "pDMGCONT1000FIAfreeb", "prelocWDHZ" , "prelocWDSV"  ,"preloc100HZ" , "preloc500HZ" , "preloc1000HZ", "preloc100SV",
                "preloc500SV" , "preloc1000SV"  , "prelocWDHZfreeb" , "prelocWDSVfreeb" , "preloc100HZfreeb" , "preloc500HZfreeb" , "preloc1000HZfreeb", "preloc100SVfreeb" ,
                "preloc500SVfreeb", "preloc1000SVfreeb" , "plaborWDSV" , "plabor100SV" , "plabor500SV" , "plabor1000SV" ,"plaborWDSVfreeb","plabor100SVfreeb" , "plabor500SVfreeb",
                "plabor1000SVfreeb" , "pDistGS", "pDistStr"  ,"wsarea" , "wsbldgarea" , "wsboarea" , "wsbowisharea" , "bldgcnt", "bocnt" , "bowishcnt" , "indcnt" ,
                "boind" , "bowishind", "percbld" , "percbo" ,"CN" , "S" , "Q" , "IA" , "Vrunoff", "Vabsorb" ,"pcnt" , "bopcnt" , "bopwishcnt" , "indpcnt", 
                "bopind", "bopwishind", "bldgAppr" , "boAppr" , "indboAppr" , "boAqCst" , "indboAqC" ,"geometry.x"  )     
  # c("STANPAR", "OWNER","parea",  "WS_ID", "BLDG_ID", "BLDG_Area","ACQUIRED_D","SALE_PRICE", "PROP_ADDR", "PROP_HOUSE", "PROP_STREE",     
  #                     "PROP_ZIP", "ACREAGE_TE",  "LAND_USE_D","LA_SQ_FT", "DU_COUNT",   "ASSD_DTE",       "LAND_APPR",      "IMPR_APPR",      "TOTAL_APPR",     "Met_Aq_D",
  #                     "Aq_Cost", "Future",         "buyout",         "DLEVEL",         "FLOODZONE",      "GENUSE",        "PID",       "totLA",  "totDU",          "totLAPR",
  #                     "totIAPR",        "totAPR",         "meanIAPR",       "meanAPR",        "countP",         "inundate",       "floodway", "fema100yr",      "fema500yr",
  #                     "PlantingSITE",   "TreesPlanted",   "Riparian",       "DistStr",        "USACE_100",      "USACE_500yr",    "USACE_1000yr", "DMG_WD_FIA",     "DMG_WD_SV",
  #                     "DMG_100_FIA",    "DMG_500_FIA",    "DMG_1000_FIA",   "DMG_100_SV",     "DMG_500_SV",     "DMG_1000_SV",    "DistGS",   "wsbldgarea",   "wsboarea",
  #                     "wsbowisharea", "bldgcnt" ,     "bocnt",        "bowishcnt",    "indcnt",       "boind", "bowishind",    "percbld",      "percbo",
  #                     "CN",           "S",            "Q",            "IA",           "Vrunoff" ,     "Vabsorb", "countB",         "totBLDGIA",      "pInun",
  #                     "pfloodway",      "pfema100yr",     "pfema500yr",     "pUSACE100",      "pUSACE500",      "pUSACE1000", "pDMG100FIA",     "pDMG500FIA",     "pDMG1000FIA",
  #                     "pDMG100SV",      "pDMG500SV",      "pDMG1000SV",     "pDistGS",        "pDistStr",       "wsarea",   "pcnt", "bopcnt",         "bopwishcnt",
  #                     "indpcnt",        "bopind",         "bopwishind",     "bldgAppr",       "boAppr",         "indboAppr",      "boAqCst", "indboAqC",   "geometry" )
  
  
  #add tree water uptake by parcel
  if (length(unique(p2$PID))<length(p2$PID)){ #fix PID if neccessary
    p2$PID<-seq(1:length(p2$STANPAR))
  }
  
  plants<-shapefile(paste0(wd,"/data/","Buffer_Plantings.shp"))
  plants<-st_as_sf(plants)
  plants<-st_transform(plants, projection)
  trees<-read.csv(paste0(wd,"/data/","buyouts trees.csv"))
  plants<-left_join(plants, trees, by = c("SITE"= "Row.Labels"))
  plants<-plants[,c(1,7,8)]
  rm(trees)
  
  test<-sf::st_join(p2, plants) #now join with plants
  test2<-arrange(test, desc(Sum.of.Trees.Planted)) #sort from large to small 
  test2<-test2[duplicated(test2[,"PID"])==FALSE, ] #remove duplicates by pid
  test2<-st_as_sf(test2)
  test3<- test2 %>% group_by(SITE) %>% summarise(cntsites = n())
  test2<-left_join(test2,test3, by="SITE")
  test2$Sum.of.Trees.Planted<-test2$Sum.of.Trees.Planted/test2$cntsites #distribute total trees for each aggregate evenly across parcels included in site
  
  test2$TreesPlanted<-NA
  for (i in 1:length(test2$TreesPlanted)){ #only keep trees in parcels where the buyout has been completed and the home removed
    if (!is.na(test2$Sum.of.Trees.Planted[i])){
      if (!is.na(test2$IMPR_APPR[i]) & test2$IMPR_APPR[i] == 0){
        test2$TreesPlanted[i]<-test2$Sum.of.Trees.Planted[i]
      }
    }
  }
  
  p2<-test2
  
  sum10<-as.data.frame(p2 %>% group_by(WS_ID) %>% summarise (treecnt = sum(TreesPlanted, na.rm=T))) #count of trees per watershed
  s<-left_join(s,sum10, by="WS_ID")
  s$TreeV<-NA
  s$TreeV<-s$treecnt*147*0.133681 #in cubic feet https://www.itreetools.org/streets/resources/Streets_CTG/PSW_GTR202_Northeast_CTG.pdf table 9 for med tree 5 years
  s$totVro<-s$Vrunoff - s$TreeV 
  s$totVab<-s$Vabsorb + s$TreeV
  
  s2<-s 
  st_geometry(s2)<-NULL
  p3<-left_join(p2, s2[,c(1,31:38)],by="WS_ID")
  
  saveRDS(p3,paste0(wd,"/data/new/","final", pnames[i]))
  saveRDS(s,paste0(wd,"/data/new/","final", subwsnames[i]))
}

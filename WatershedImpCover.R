#9

#This script conducts a spatial intersection between buildings and watersheds in order to determine the exact amount of
#impervious building cover in each watershed. The imperviousness is used to estimate stormwater runoff quantities produced 
#by the building cover. Additional summary statistics are calucalted for each watershed.


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



#############################################################################################################################
#Intersect pbws building files with subwatersheds & compute bldg impervious area and bldg inundation information by watershed
#############################################################################################################################

#Define function that intersects buildings with watersheds, calculates intersection areas, and calculates summary statistics on bldg inundation and imperviousness by watershed
compute_watershed_bldgstats = function(projection, sub_ws, CN_open, CN_brush, Precip_100yr, pbwsnames, dates){
  
  n<-length(pbwsnames)
  registerDoParallel(n)
  getDoParWorkers() 
  
  foreach (i=1:n) %dopar% {
    pb<-readRDS(paste0(wd,"/data/new/", pbwsnames[i]))
    test<-st_as_sf(pb) #convert to simpler spatial format (sf)
    test<-st_transform(test, projection)
    test2<-st_as_sf(sub_ws)
    
    #clean bldg files
    keep<-c("BLDG_ID","BLDG_Area","STANPAR","OWNER","ACQUIRED_D","SALE_PRICE", "PROP_ADDR.x","PROP_HOUSE.x","PROP_STREE.x","PROP_ZIP","ACREAGE_TE","LAND_USE",            
            "LAND_USE_D","LA_SQ_FT","DU_COUNT","ASSD_DTE", "LAND_APPR", "IMPR_APPR","TOTAL_APPR","Met_Aq_D","Aq_Cost","Future","buyout",               
            "DLEVEL" , "FLOODZONE","GENUSE","parea","PID" ,"PID.x", "WS_ID","totLA","totDU","totLAPR","totIAPR", "totAPR", "meanIAPR","meanAPR","countP", 
            "inundate", "DESCRIPT.x",  "DESCRIPT.y","DESCRIPT","SITE", "Sum.of.Trees.Planted", "OBJECTID", "DistStr","USACE_100","USACE_500yr" ,"USACE_1000yr","Aq_year","BLD_Year","DLEVEL_depth",             
            "DLEVEL_freeb","USACE_100_freeb" ,"USACE_500yr_freeb","USACE_1000yr_freeb","DMG_WD_FIA","DMG_WD_SV", "DMG_100_FIA", "DMG_500yr_FIA", "DMG_1000yr_FIA", "DMG_100_SV", 
            "DMG_500_SV", "DMG_1000_SV",  "DMG_WD_FIA_freeb","DMG_WD_SV_freeb","DMG_100_FIA_freeb","DMG_500yr_FIA_freeb", "DMG_1000yr_FIA_freeb", "DMG_100_SV_freeb", "DMG_500_SV_freeb", 
            "DMG_1000_SV_freeb" ,"DMGCONT_WD_FIA" , "DMGCONT_100_FIA" ,"DMGCONT_500yr_FIA", "DMGCONT_1000yr_FIA","DMGCONT_WD_FIA_freeb","DMGCONT_100_FIA_freeb", "DMGCONT_500yr_FIA_freeb",
            "DMGCONT_1000yr_FIA_freeb", "Reloc_WD_HZ" ,"Reloc_WD_SV" , "Reloc_100_HZ", "Reloc_100_SV", "Reloc_500yr_HZ" , "Reloc_500yr_SV", "Reloc_1000yr_HZ" ,"Reloc_1000yr_SV",
            "Reloc_WD_HZ_freeb", "Reloc_WD_SV_freeb" ,"Reloc_100_HZ_freeb", "Reloc_100_SV_freeb", "Reloc_500yr_HZ_freeb" , "Reloc_500yr_SV_freeb","Reloc_1000yr_HZ_freeb",
            "Reloc_1000yr_SV_freeb" ,"Labor_WD_SV","Labor_100_SV","Labor_500yr_SV", "Labor_1000yr_SV", "Labor_WD_SV_freeb","Labor_100_SV_freeb" ,"Labor_500yr_SV_freeb",
            "Labor_1000yr_SV_freeb", "DistGS","geometry")
    test3<-test[ ,names(test) %in% keep]
    names(test3)<-c("BLDG_ID","BLDG_Area","STANPAR","OWNER","ACQUIRED_D","SALE_PRICE", "PROP_ADDR","PROP_HOUSE","PROP_STREE","PROP_ZIP","ACREAGE_TE","LAND_USE",            
                    "LAND_USE_D","LA_SQ_FT","DU_COUNT","ASSD_DTE", "LAND_APPR", "IMPR_APPR","TOTAL_APPR","Met_Aq_D","Aq_Cost","Future","buyout",               
                    "DLEVEL" , "FLOODZONE","GENUSE","parea","PID" , "WS_ID","totLA","totDU","totLAPR","totIAPR", "totAPR", "meanIAPR","meanAPR","countP", 
                    "inundate", "floodway",  "fema100yr","fema500yr","PlantingSITE", "TreesPlanted", "Riparian", "DistStr","USACE_100","USACE_500yr" ,"USACE_1000yr","Aq_year","BLD_Year","DLEVEL_depth",             
                    "DLEVEL_freeb","USACE_100_freeb" ,"USACE_500yr_freeb","USACE_1000yr_freeb","DMG_WD_FIA","DMG_WD_SV", "DMG_100_FIA", "DMG_500_FIA", "DMG_1000_FIA", "DMG_100_SV", 
                    "DMG_500_SV", "DMG_1000_SV",  "DMG_WD_FIA_freeb","DMG_WD_SV_freeb","DMG_100_FIA_freeb","DMG_500yr_FIA_freeb", "DMG_1000yr_FIA_freeb", "DMG_100_SV_freeb", "DMG_500_SV_freeb", 
                    "DMG_1000_SV_freeb" ,"DMGCONT_WD_FIA" , "DMGCONT_100_FIA" ,"DMGCONT_500yr_FIA", "DMGCONT_1000yr_FIA","DMGCONT_WD_FIA_freeb","DMGCONT_100_FIA_freeb", "DMGCONT_500yr_FIA_freeb",
                    "DMGCONT_1000yr_FIA_freeb", "Reloc_WD_HZ" ,"Reloc_WD_SV" , "Reloc_100_HZ", "Reloc_100_SV", "Reloc_500yr_HZ" , "Reloc_500yr_SV", "Reloc_1000yr_HZ" ,"Reloc_1000yr_SV",
                    "Reloc_WD_HZ_freeb", "Reloc_WD_SV_freeb" ,"Reloc_100_HZ_freeb", "Reloc_100_SV_freeb", "Reloc_500yr_HZ_freeb" , "Reloc_500yr_SV_freeb","Reloc_1000yr_HZ_freeb",
                    "Reloc_1000yr_SV_freeb" ,"Labor_WD_SV","Labor_100_SV","Labor_500yr_SV", "Labor_1000yr_SV", "Labor_WD_SV_freeb","Labor_100_SV_freeb" ,"Labor_500yr_SV_freeb",
                    "Labor_1000yr_SV_freeb", "DistGS","geometry")
    
    #make sure all bldg_id are unique (used as index) and rebuild if necessary (some files (no buyout scenario) may have overlapping numbers from combining multiple years data)
    if (length(test3$BLDG_ID)!= length(unique(test3$BLDG_ID))) {
      test3$BLDG_ID<-seq(1:length(test3$BLDG_ID)) #rebuild bldg_id
    }
    
    test4<-sf::st_intersection(st_buffer(test3,0), test2) %>% mutate(wsinta = as.numeric(st_area(.))) #caluclate area of intersection between buildings and watersheds
    #test5<-test4[is.na(test4$Met_Aq_D) | test4$Future =="TRUE" | as.Date(test4$Met_Aq_D, format = "%m/%d/%Y") > dates[i], ] #drop buildings that were boughtout prior to date that are still in footprint shapefile (assume they have been or will be demolished by end of year)
    test5<-test4
    
    sum1<-as.data.frame(test5 %>% group_by(WS_ID) %>% summarise (wsbldgarea = sum(wsinta))) #calculate impervious area from buildings by watershed
    sum2<-as.data.frame(test5[test5$buyout =='YES' & test5$Future =="FALSE",] %>% group_by(WS_ID) %>% summarise (wsboarea = sum(wsinta))) #same but for completed buyout homes only
    sum3<-as.data.frame(test5[test5$buyout =='YES',] %>% group_by(WS_ID) %>% summarise (wsbowisharea = sum(wsinta))) #same but for all wishlist buyout homes only
    
    sum4<-as.data.frame(test5 %>% group_by(WS_ID) %>% summarise (bldgcnt = length(unique(BLDG_ID)))) #count of buildings
    sum5<-as.data.frame(test5[test5$buyout =='YES' & test5$Future =="FALSE",] %>% group_by(WS_ID) %>% summarise (bocnt = length(unique(BLDG_ID)))) #count of current buyouts bldgs
    sum6<-as.data.frame(test5[test5$buyout =='YES',] %>% group_by(WS_ID) %>% summarise (bowishcnt = length(unique(BLDG_ID)))) #count of wish list buyout bldgs
    
    sum7<-as.data.frame(test5[test5$inundate =="Inundated" | !is.na(test5$DLEVEL),] %>% group_by(WS_ID) %>% summarise (indcnt = length(unique(BLDG_ID)))) #count all inundated or damaged buildings
    sum8<-as.data.frame(test5[test5$buyout =='YES' & test5$Future =="FALSE" & (test5$inundate =="Inundated"| !is.na(test5$DLEVEL)),] %>% group_by(WS_ID) %>% summarise (boind = length(unique(BLDG_ID)))) #same for compelted buyout bldgs
    sum9<-as.data.frame(test5[test5$buyout =='YES'& (test5$inundate =="Inundated"| !is.na(test5$DLEVEL)),] %>% group_by(WS_ID) %>% summarise (bowishind = length(unique(BLDG_ID)))) #same for buyout bldgs on wishlist
    
    test6<-left_join(test2, sum1, by="WS_ID") # join sums back to watershed shapefile
    test6<-left_join(test6, sum2, by="WS_ID")
    test6<-left_join(test6, sum3, by="WS_ID")
    test6<-left_join(test6, sum4, by="WS_ID")
    test6<-left_join(test6, sum5, by="WS_ID")
    test6<-left_join(test6, sum6, by="WS_ID")
    test6<-left_join(test6, sum7, by="WS_ID")
    test6<-left_join(test6, sum8, by="WS_ID")
    test6<-left_join(test6, sum9, by="WS_ID")
    
    test6$percbld<-test6$wsbldgarea/test6$wsarea*100 #add percent impervious watershed area columns
    test6$percbo<-test6$wsboarea/test6$wsarea*100
    test6$CN<- CN_open + (test6$percbld/100)*(98- CN_open)*(1-0.5*1)#in final parentheses second 1 is the ratio of unconnected imp area to total imp area for 
    #conditions where we ignore paved areas, if buffer plantings added use a weighted average instead of just CN-Open
    test6$S<-(1000/test6$CN)-10
    test6$Q<-(Precip_100yr - 0.2*test6$S)^2/(Precip_100yr + 0.8*test6$S)
    test6$IA<-0.2*test6$S
    test6$Vrunoff<-test6$Q*test6$wsarea/12 #volume of water (cuft) associated with runoff in inches for each watershed
    test6$Vabsorb<-test6$IA*test6$wsarea/12 #volume of water initially abstracted for each watershed
    # hist(test6$percbld) #test to make sure data makes sense
    # hist(test6$percbo)
    # hist(test6$Q)
    #plot(test6[,c(6:7,10)])
    #test6<-st_as_sf(test6)
    saveRDS(test6, paste0(wd,"/data/new/","newsubws", pbwsnames[i])) #save the watershed shapefiles
    st_geometry(test6)<-NULL
    test7<-left_join(test3, test6, by="WS_ID") #merge the watershed level impervisouness values with the building shapefiles, test6[ ,c(1,7:10)] undo column selection later
    saveRDS(test7, paste0(wd,"/data/new/","new", pbwsnames[i])) #save the pbws shapefiles
  }
}

#Watershed runoff calculations using curve number method 
#USDA: Urban Hydrology for Small Watersheds TR-55
#https://www.nrcs.usda.gov/Internet/FSE_DOCUMENTS/stelprdb1044171.pdf
CN_open<-61
CN_brush<-48 #only if we add in buffer plantings
Precip_100yr<-6.5 #inches

sub_ws<-shapefile(paste0(wd,"/data/","subwatersheds_smoothed.shp"))
sub_ws<-spTransform(sub_ws, projection) #set projection
sub_ws$wsarea<-gArea(sub_ws, byid=TRUE) #area by watershed
sub_ws<-sub_ws[ ,c(26:27)]

#for observed scenarios

pbwsnames<-c("pbwsGS2005ob.RDS", "pbwsGS2006ob.RDS", "pbwsGS2007ob.RDS", "pbwsGS2008ob.RDS", "pbwsGS2009ob.RDS", "pbwsGS2010ob.RDS", "pbwsGS2011ob.RDS",
             "pbwsGS2012ob.RDS", "pbwsGS2013ob.RDS","pbwsGS2014ob.RDS","pbwsGS2015ob.RDS") 
dates<-c(as.Date("1/01/2005", format = "%m/%d/%Y"),as.Date("1/01/2006", format = "%m/%d/%Y"), as.Date("1/01/2007", format = "%m/%d/%Y"),
         as.Date("1/01/2008", format = "%m/%d/%Y"),as.Date("1/01/2009", format = "%m/%d/%Y"), as.Date("1/01/2010", format = "%m/%d/%Y"), 
         as.Date("1/01/2011", format = "%m/%d/%Y"), as.Date("1/01/2012", format = "%m/%d/%Y"), as.Date("1/01/2013", format = "%m/%d/%Y"),
         as.Date("1/01/2014", format = "%m/%d/%Y"), as.Date("1/01/2015", format = "%m/%d/%Y")) 



compute_watershed_bldgstats (projection, sub_ws, CN_open, CN_brush, Precip_100yr, pbwsnames, dates)


#for simulated scenarios

pbwsnames<-c("pbwsGS2006nb.RDS","pbwsGS2007nb.RDS","pbwsGS2008nb.RDS","pbwsGS2009nb.RDS","pbwsGS2010nb.RDS", "pbwsGS2011nb.RDS","pbwsGS2012nb.RDS", "pbwsGS2013nb.RDS","pbwsGS2014nb.RDS","pbwsGS2015nb.RDS",
             "pbwsGS2010ab.RDS", "pbwsGS2011ab.RDS","pbwsGS2012ab.RDS", "pbwsGS2013ab.RDS","pbwsGS2014ab.RDS","pbwsGS2015ab.RDS",
             "pbwsGS2010w.RDS", "pbwsGS2011w.RDS","pbwsGS2012w.RDS", "pbwsGS2013w.RDS","pbwsGS2014w.RDS","pbwsGS2015w.RDS")

dates<-c(as.Date("1/01/2006", format = "%m/%d/%Y"),as.Date("1/01/2007", format = "%m/%d/%Y"),as.Date("1/01/2008", format = "%m/%d/%Y"),as.Date("1/01/2009", format = "%m/%d/%Y"),
         as.Date("1/01/2010", format = "%m/%d/%Y"),as.Date("1/01/2011", format = "%m/%d/%Y"),as.Date("1/01/2012", format = "%m/%d/%Y"),
         as.Date("1/01/2013", format = "%m/%d/%Y"), as.Date("1/01/2014", format = "%m/%d/%Y"),as.Date("1/01/2015", format = "%m/%d/%Y"),
         as.Date("1/01/2010", format = "%m/%d/%Y"),as.Date("1/01/2011", format = "%m/%d/%Y"),as.Date("1/01/2012", format = "%m/%d/%Y"),
         as.Date("1/01/2013", format = "%m/%d/%Y"), as.Date("1/01/2014", format = "%m/%d/%Y"),as.Date("1/01/2015", format = "%m/%d/%Y"),
         as.Date("1/01/2010", format = "%m/%d/%Y"),as.Date("1/01/2011", format = "%m/%d/%Y"),as.Date("1/01/2012", format = "%m/%d/%Y"),
         as.Date("1/01/2013", format = "%m/%d/%Y"), as.Date("1/01/2014", format = "%m/%d/%Y"),as.Date("1/01/2015", format = "%m/%d/%Y"))

compute_watershed_bldgstats (projection, sub_ws, CN_open, CN_brush, Precip_100yr, pbwsnames, dates)


#for county totals
sum(sum13$indpcnt)
sum(sum7$indcnt)

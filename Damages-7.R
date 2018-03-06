#7

#This script computes structural damages, contents damages, relocation costs, and labor costs for all years 
#and buyout program adoption scenarios as well as for the observed freeboard ordinance conditions and for a no 
#freeboard ordinace scenario.  Calculations are based on depth of inundation for which data on
#observed damages from the Nashville Metro Water Services (MWS) windshield survey and HAZUS modeled 100, 500, and 1000 year 
#storm event depth grids were used. Depth to damage/cost relationships were obtained from the HAZUS technical manual and
#from data from a post-flood survey of Nashville residents conducted by Vanderbilt. For each buyout scenario year columns providing
#the potential damage that would be incured for each building given a repeat 2010 flood event, a theoretical 100yr storm, 
#a theoretical 500yr storm, and a theoretical 1000yr storm is calculated two ways (using HAZUS depth-damage or survey data) to
#provide a low and high end estimate.

#ADD the totBLDGIA per parcel at the top to use in damage calculations.

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

##########################################################################################################################################
#Intersect buildings with Depth Grid and Compute FIA  and Survey-based Damage Estimates using Depth Grid and Windshield Survey Depths
##########################################################################################################################################

#From damage suvey we know that a Damage Rank of 0,1,2,3,4, indicates zero, less than 1 ft, less than 2ft, less than 6ft, more than 6ft of inundation, respectively
#From residential post-flood survey we have average repair costs by damage rating of 0,1,2,3, of 52, 2797, 11237, 72614, respectively (assume damage ratings correspond with DLEVELS of 1,2,3,4)

Depth<-as.numeric(seq(from=0, to=10, by=1)) #estimated depth
DLEVEL<-as.numeric(c(0,1,2,3,3,3,3,4,4,4,4)) #Damage rating from Nashville windshield survey
Res_Damage<-as.numeric(c(18,18,25,28,30,32,40,43,44,45,46)) #as a percent of value from https://www.fema.gov/media-library-data/20130726-1820-25045-8292/hzmh2_1_fl_tm.pdf, figure 5.2
MobH_damage<-as.numeric(c(10,44,63,73,78,79,80,81,81,81, 81)) #as a percent of value from https://www.fema.gov/media-library-data/20130726-1820-25045-8292/hzmh2_1_fl_tm.pdf, figure 5.2
TwoFl_Damage<-as.numeric(c(12,16,20,24,28,32,38,44,49,51,53)) #as a percent of value from https://www.fema.gov/media-library-data/20130726-1820-25045-8292/hzmh2_1_fl_tm.pdf, figure 5.2
OneFl_contents_dmg<-as.numeric(c(12,25,34,36,38,41,45,50,55,60,60)) #as a percent of value from https://www.fema.gov/media-library-data/20130726-1820-25045-8292/hzmh2_1_fl_tm.pdf, figure 5.5
TwoFl_contents_dmg<-as.numeric(c(8,11,19,23,29,33,39,44,50,58,58)) #as a percent of value from https://www.fema.gov/media-library-data/20130726-1820-25045-8292/hzmh2_1_fl_tm.pdf
Rep_cost<-as.numeric(c(0.015184645,1.115334455,2.215484264,9.313724683,16.4119651,25.26323882,
                       34.11451253,42.96578624,51.81705996,69.51960738,87.22215481)) #use moving average of survey estimated repair costs per sqft
Ave_reloc<-as.numeric(c(0.71,6.4935,12.277,43.3685,74.46,102.9825,131.505,160.0275,188.55,245.595,302.64)) #use moving average of survey estimated relocation time
Ave_labor_hrs<-as.numeric(c(0.006604875,0.041720275,0.076835674,0.95566056,1.834485446,2.189075501,2.543665555,
                            2.898255609,3.252845664,3.962025772,4.671205881))#use moving average of survey estimated labor hrs per sqft
Res_restime<-as.numeric(c(90,112,134,156,178,200,222,244,266,365,365)) #approx equal intervals in restoration time (days) using RES1 from HAZUS table 14.10
Nonres_restime<-as.numeric(c(210,255,300,345,390,435,480,525,570,750,750)) #approx equal intervals in restoration time (days) using COM1 from HAZUS table 14.10
Car_Damage<-as.numeric(c(15,15,15,60,60,100,100,100,100,100,100)) #as a percent of value from https://www.fema.gov/media-library-data/20130726-1820-25045-8292/hzmh2_1_fl_tm.pdf, table 8.1
FIA_depth_to_damage<-data.frame(Depth=Depth, DLEVEL=DLEVEL, Res_Damage=Res_Damage, MobH_damage=MobH_damage, 
                                TwoFl_Damage=TwoFl_Damage, OneFl_contents_dmg=OneFl_contents_dmg,TwoFl_contents_dmg=TwoFl_contents_dmg, 
                                Rep_cost=Rep_cost,Ave_reloc = Ave_reloc,Ave_labor_hrs=Ave_labor_hrs,
                                Res_restime=Res_restime,Nonres_restime=Nonres_restime,Car_Damage=Car_Damage)


#Contents value as percent of strucutre value (HAZUS https://www.fema.gov/media-library-data/20130726-1820-25045-8292/hzmh2_1_fl_tm.pdf pg 454)
res_cont<-.5
nonres_cont<-1.25

#Table 14.10 for rental and relocation costs
#Table 14.12 for restoration time
#8-4 FOR VEHICLES
#Table 11-1 for debris generation
#survey for expected time of relocation and labor hours/costs

#inflation adjustment rates to 2010 dollars
annual_inflation_rate<-data.frame(as.numeric(c(1.12, 1.08, 1.05, 1.01, 1.02, 1.00, 0.97, 0.95, 0.94, 0.92, 0.92)), #http://www.usinflationcalculator.com/  for 2005 to 2015 in reference to 2010 dollars
                                  c(2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015))
colnames(annual_inflation_rate)<-c("InfRate","Year")

#building types
MH<-c(18,62,88)
residential<-c(11,12,13, 14, 15, 16, 17, 18, 19, 37,38,39,62, 81, 82, 83, 84, 88, 89, 92, 95, 97)

pbwsnames<-c("streampbws2005b.RDS", "streampbws2006b.RDS", "streampbws2007b.RDS", "streampbws2008b.RDS", "streampbws2009b.RDS", "streampbws2010b.RDS", "streampbws2011b.RDS",
             "streampbws2012b.RDS", "streampbws2013b.RDS","streampbws2014b.RDS","streampbws2015b.RDS",
             "newpbws2006b_nb.RDS","newpbws2007b_nb.RDS","newpbws2008b_nb.RDS","newpbws2009b_nb.RDS","newpbws2010b_nb.RDS", "newpbws2011b_nb.RDS","newpbws2012b_nb.RDS", "newpbws2013b_nb.RDS","newpbws2014b_nb.RDS","newpbws2015b_nb.RDS",
             "newpbws2010b_ab.RDS", "newpbws2011b_ab.RDS","newpbws2012b_ab.RDS", "newpbws2013b_ab.RDS","newpbws2014b_ab.RDS","newpbws2015b_ab.RDS",
             "newpbws2010b_w.RDS", "newpbws2011b_w.RDS","newpbws2012b_w.RDS", "newpbws2013b_w.RDS","newpbws2014b_w.RDS","newpbws2015b_w.RDS") 

years<-c(2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,
         2006, 2007, 2008, 2009, 2010,2011,2012,2013,2014,2015,
         2010,2011,2012,2013,2014,2015,
         2010,2011,2012,2013,2014,2015)

##############################################
#Read in Data
################################################

HAZUS100<-raster(paste0(wd,"/data/","USACE_100.tif")) #assume these depths are in feet
HAZUS500<-raster(paste0(wd,"/data/","USACE_500yr.tif"))
HAZUS1000<-raster(paste0(wd,"/data/","USACE_1000yr.tif"))

HAZUS100<-projectRaster(HAZUS100, crs = projection)
HAZUS100<-rasterToPoints(HAZUS100, spatial=TRUE) #convert raster to points
HAZUS100<-st_as_sf(HAZUS100) #convert spatial points to sf object
HAZUS100<-st_transform(HAZUS100, projection)

HAZUS500<-projectRaster(HAZUS500, crs = projection)
HAZUS500<-rasterToPoints(HAZUS500, spatial=TRUE) #convert raster to points
HAZUS500<-st_as_sf(HAZUS500) #convert spatial points to sf object
HAZUS500<-st_transform(HAZUS500, projection)

HAZUS1000<-projectRaster(HAZUS1000, crs = projection)
HAZUS1000<-rasterToPoints(HAZUS1000, spatial=TRUE) #convert raster to points
HAZUS1000<-st_as_sf(HAZUS1000) #convert spatial points to sf object
HAZUS1000<-st_transform(HAZUS1000, projection)

# DEM<-raster(paste0(wd,"/data/","filled dem.tif")) #filled digital elevation model (USGS NED 1/3arcsecond), with elevation in meters
# DEM<-projectRaster(DEM, crs = projection)
# DEM<-rasterToPoints(DEM, spatial=TRUE) #convert raster to points
# DEM<-st_as_sf(DEM) #convert spatial points to sf object
# DEM<-st_transform(DEM, projection)
# DEM$filled_dem<-DEM$filled_dem*3.28 #convert elevations to feet
#saveRDS(DEM,paste0(wd,"/data/","DEM.rds"))
DEM<-readRDS(paste0(wd,"/data/","DEM.rds"))

# BFE<-raster(paste0(wd,"/data/","bfe_rstr_ft21.tif")) #filled digital elevation model (USGS NED 1/3arcsecond), with elevation in meters
# BFE<-projectRaster(BFE, crs = projection)
# BFE<-rasterToPoints(BFE, spatial=TRUE) #convert raster to points
# BFE<-st_as_sf(BFE) #convert spatial points to sf object
# BFE$BFE<-BFE$bfe_rstr_ft21
# BFE<-BFE[,c(2,3)]
# BFE<-st_transform(BFE, projection)
#saveRDS(BFE,paste0(wd,"/data/","BFE.rds"))
BFE<-readRDS(paste0(wd,"/data/","BFE.rds"))

##################################################
#Loop for spatial joins, depth adjustments
#################################################

n<-length(pbwsnames)
registerDoParallel(n)
getDoParWorkers() 

foreach (i=1:n) %dopar% { 
  p<-readRDS(paste0(wd,"/data/", pbwsnames[i]))
  p$DistStr<-as.numeric(p$DistStr)
  
  test<-sf::st_join(p, HAZUS100) #conduct spatial join of buildings and HAZUS 100 yr flood depth
  test2<-arrange(test, desc(USACE_100)) #sort from large to small 
  test3<-test2[duplicated(test2[,1])==FALSE, ] #remove duplicates by bldg id (keeps first largest depth by default)
  test3<-st_as_sf(test3)
  
  test4<-sf::st_join(test3, HAZUS500) #conduct spatial join of buildings and HAZUS 500 yr flood depth
  test5<-arrange(test4, desc(USACE_500yr)) #sort from large to small 
  test6<-test5[duplicated(test5[,1])==FALSE, ] #remove duplicates by bldg id (keeps first largest depth by default)
  test6<-st_as_sf(test6)
  
  test7<-sf::st_join(test6, HAZUS1000) #conduct spatial join of buildings and HAZUS 1000 yr flood depth
  test8<-arrange(test7, desc(USACE_1000yr)) #sort from large to small 
  test9<-test8[duplicated(test8[,1])==FALSE, ] #remove duplicates by bldg id (keeps first largest depth by default)
  test9<-st_as_sf(test9)
  
  test10<-sf::st_join(test9, DEM) #conduct spatial join of buildings and DEM
  test10<-arrange(test10, filled_dem) #sort from small to large 
  test10<-test10[duplicated(test10[,1])==FALSE, ] #remove duplicates by bldg id (keeps first largest depth by default)
  test9<-st_as_sf(test10)
  
  test10<-sf::st_join(test9, BFE) #conduct spatial join of buildings and DEM
  test10<-arrange(test10, BFE) #sort from small to large 
  test10<-test10[duplicated(test10[,1])==FALSE, ] #remove duplicates by bldg id (keeps first largest depth by default)
  test9<-st_as_sf(test10)
  
  rm(test, test2, test3,test4,test5,test6,test7,test8)
  
  ##################################
  #inflation adjust monetary values 
  ##################################
  test9$IMPR_APPR<-test9$IMPR_APPR*annual_inflation_rate$InfRate[annual_inflation_rate$Year %in% years[i]]
  test9$LAND_APPR <-test9$LAND_APPR*annual_inflation_rate$InfRate[annual_inflation_rate$Year %in% years[i]]
  test9$TOTAL_APPR <-test9$TOTAL_APPR*annual_inflation_rate$InfRate[annual_inflation_rate$Year %in% years[i]]
  test9$totAPR <-test9$totAPR*annual_inflation_rate$InfRate[annual_inflation_rate$Year %in% years[i]]
  test9$totLAPR <-test9$totLAPR*annual_inflation_rate$InfRate[annual_inflation_rate$Year %in% years[i]]
  test9$totIAPR <-test9$totIAPR*annual_inflation_rate$InfRate[annual_inflation_rate$Year %in% years[i]]
  
  test9$Met_Aq_D<-as.Date(test9$Met_Aq_D, format = "%m/%d/%Y")
  test9$Met_Aq_D<-as.character(test9$Met_Aq_D)
  test9$Aq_year<-str_sub(test9$Met_Aq_D,-4,-1)
  for (j in 1: length(test9$Aq_Cost)){ #loop takes about 10 min
    test9$Aq_Cost[j] <-test9$Aq_Cost[j]*annual_inflation_rate$InfRate[annual_inflation_rate$Year >= test9$Aq_year[j]][1] #adjust aquisition cost to 2010 dollars based on year of aquisition
  }
  
  #######################################################################
  #Add build year where available, if not avail, assume post 1979 build 
  ########################################################################
  p<-readRDS(paste0(wd,"/data/","parcels2006.RDS"))
  p<-p@data
  p$BLD_Year<-substr(p$PARCEL_DAT,1,4)
  p$BLD_Year<-as.numeric(p$BLD_Year)
  p<-p[,c("STANPAR","BLD_Year")]
  p<-p[duplicated(p[,1])==FALSE,]
  
  
  test9<-left_join(test9,p,by="STANPAR")
  test9$BLD_Year[is.na(test9$BLD_Year)]<-1980 
  #################################################################################################################################################################
  #Calculate windshield depths assuming that homes built after 1979 DID NOT implement the required BFE+4ft freeboard requirement (no freeboard ordinance scenario)
  ##############################################################################################################################################################
  # We assume that all homes in the 100yr floodplain built after 1979 are built at BFE + 4 ft and that the observed DLEVELs reflect this and should be adjusted up for the no freeboard ordinance scenario
  
  test9$DLEVEL_depth<-as.numeric(test9$DLEVEL)#leave 0,1,2, as depths of 0ft, 1ft, and 2ft
  test9$DLEVEL_depth[test9$DLEVEL_depth ==4]<-7 #estimate depth for DLEVEL 4 as 7 ft (greater than 6ft)
  test9$DLEVEL_depth[test9$DLEVEL_depth == 3]<-4
  
  if (i>11 & i<22){ #for cases where no buyouts estimate what the observed DLEVEL would have been for houses that were not present in flood but would have been if not for buyout program
    test9$DLEVEL_depth[is.na(test9$DLEVEL_depth) & test9$Aq_year < 2010]<-round(mean(test9$DLEVEL_depth[!is.na(test9$buyout)],na.rm=T)) #add estimate of damage for homes not there in 2010 actual
  }
  
  dup<-test9
  
  test9$BFE[test9$BFE==0]<-NA
  
  test9$DLEVEL_freeb<-NA
  for (j in 1: length(test9$DLEVEL)){
    if(!is.na(test9$DLEVEL_depth[j])){ #if the building was damaged
      if (test9$BLD_Year[j] > 1979 & !is.na(test9$BFE[j])){ #if post 1979 and house in floodplain
        test9$DLEVEL_freeb[j]<- test9$DLEVEL_depth[j] + (test9$BFE[j] + 4 - test9$filled_dem[j] + 2) #increase the depth of inundation by the difference in expected home elevation
      } else {
        test9$DLEVEL_freeb[j]<-test9$DLEVEL_depth[j]
      }
    }
    if ( !is.na(test9$DLEVEL_freeb[j]) & test9$DLEVEL_freeb[j] < test9$DLEVEL_depth[j]){ #if the house would have been at a higher elevation using 2ft above dem home elevation assumption use the original depth
      test9$DLEVEL_freeb[j]<-test9$DLEVEL_depth[j]
    }
  }
  
  ####################################################################################################################################
  #Calculate HAZUS depths after accounting for 4ft freeboard on homes built after 1979 and floor elevation 2ft above DEM otherwise
  ###################################################################################################################################  
  test9$USACE_100_freeb <- test9$USACE_100 - 2 #if no 4 ft freeboard req assume home floor elevations are 2 ft above DEM elevation, therefore inundation depth above DEM - 2
  test9$USACE_500yr_freeb <- test9$USACE_500yr - 2 #if no 4 ft freeboard req assume home floor elevations are 2 ft above DEM elevation
  test9$USACE_1000yr_freeb <- test9$USACE_1000yr - 2 #if no 4 ft freeboard req assume home floor elevations are 2 ft above DEM elevation
  
  for (j in 1: length(test9$USACE_100)){
    if (test9$BLD_Year[j] > 1979 & !is.na(test9$BFE[j])){#if post 1979 and house in floodplain
      test9$USACE_100[j] <- test9$USACE_100[j] - (test9$BFE[j] + 4 - test9$filled_dem[j]) #reduce the inundation depth to account for home elevation above DEM
    } else {
      test9$USACE_100[j]<-test9$USACE_100[j] - 2 #assume standard 2 ft above DEM
    }
    if (!is.na(test9$USACE_100[j]) & test9$USACE_100_freeb[j] < test9$USACE_100[j]){ #if inundation using 2ft above dem home elevation assumption is less than using the BFE + 4 ft requirement (ie DEM + 2 is higher than BFE + 4), use the 2ft above DEM inundation depth
      test9$USACE_100[j]<-test9$USACE_100_freeb[j]
    }
  }
  test9$USACE_100[test9$USACE_100<0]<-NA  #no negative home inundation depths
  test9$USACE_100_freeb[test9$USACE_100_freeb<0]<-NA
  
  for (j in 1: length(test9$USACE_500yr)){
    if (test9$BLD_Year[j] > 1979 & !is.na(test9$BFE[j])){#if post 1979 and house in floodplain
      test9$USACE_500yr[j] <- test9$USACE_500yr[j] - (test9$BFE[j] + 4 - test9$filled_dem[j]) #reduce the inundation depth to account for home elevation above DEM
    } else {
      test9$USACE_500yr[j]<-test9$USACE_500yr[j] - 2 #assume standard 2 ft above DEM
    }
    if (!is.na(test9$USACE_500yr[j]) & test9$USACE_500yr_freeb[j] < test9$USACE_500yr[j]){ #if inundation using 2ft above dem home elevation assumption is less than using the BFE + 4 ft requirement (ie DEM + 2 is higher than BFE + 4), use the 2ft above DEM inundation depth
      test9$USACE_500yr[j]<-test9$USACE_500yr_freeb[j]
    }
  }
  test9$USACE_500yr[test9$USACE_500yr<0]<-NA  #no negative home inundation depths
  test9$USACE_500yr_freeb[test9$USACE_500yr_freeb<0]<-NA
  
  
  for (j in 1: length(test9$USACE_1000yr)){
    if (test9$BLD_Year[j] > 1979 & !is.na(test9$BFE[j])){#if post 1979 and house in floodplain
      test9$USACE_1000yr[j] <- test9$USACE_1000yr[j] - (test9$BFE[j] + 4 - test9$filled_dem[j]) #reduce the inundation depth to account for home elevation above DEM
    } else {
      test9$USACE_1000yr[j]<-test9$USACE_1000yr[j] - 2 #assume standard 2 ft above DEM
    }
    if (!is.na(test9$USACE_1000yr[j]) & test9$USACE_1000yr_freeb[j] < test9$USACE_1000yr[j]){ #if inundation using 2ft above dem home elevation assumption is less than using the BFE + 4 ft requirement (ie DEM + 2 is higher than BFE + 4), use the 2ft above DEM inundation depth
      test9$USACE_1000yr[j]<-test9$USACE_1000yr_freeb[j]
    }
  }
  test9$USACE_1000yr[test9$USACE_1000yr<0]<-NA  #no negative home inundation depths
  test9$USACE_1000yr_freeb[test9$USACE_1000yr_freeb<0]<-NA
  
  saveRDS(test9, paste0(wd,"/data/new/", "depths", pbwsnames[i]) )
}
rm(test9,test10,HAZUS500,HAZUS1000,HAZUS100,DEM,BFE,p)
####################################################################################################
#calculate structural damages, assuming 4ft freeboard req in place (observed/actual conditions)
####################################################################################################
n<-length(pbwsnames)
registerDoParallel(n)
getDoParWorkers() 

foreach (i=1:n) %dopar% { 
  test9<-readRDS(paste0(wd,"/data/new/depths", pbwsnames[i]))
  
  test9$DMG_WD_FIA<-NA
  for (j in 1: length(test9$DMG_WD_FIA)){ #takes about 5 min
    if (!is.na(test9$DLEVEL_depth)[j]){ 
      d<- test9$DLEVEL_depth[j]
      if (test9$LAND_USE[j] %in% MH){
        test9$DMG_WD_FIA[j]<-FIA_depth_to_damage$MobH_damage[which(FIA_depth_to_damage$Depth == d)][1]*test9$IMPR_APPR[j]/100 
      }else{
        if(test9$totLA[j] > (1.5*test9$BLDG_Area[j])){
          test9$DMG_WD_FIA[j]<-FIA_depth_to_damage$TwoFl_Damage[which(FIA_depth_to_damage$Depth == d)][1]*test9$totIAPR[j]/100 #for multiple story buildings that may contain multiple tax parcels use the total value of all tax parcels in the building
        } else{
          test9$DMG_WD_FIA[j]<-FIA_depth_to_damage$Res_Damage[which(FIA_depth_to_damage$Depth == d)][1]*test9$IMPR_APPR[j]/100 
        }
      }
    }
  }
  
  test9$DMG_WD_SV<-NA #low end estimate as the repair costs are from survey of single family dwellings only
  for (j in 1: length(test9$DMG_WD_SV)){
    if (!is.na(test9$DLEVEL_depth)[j]){
      d<- test9$DLEVEL_depth[j]
      test9$DMG_WD_SV[j]<-FIA_depth_to_damage$Rep_cost[which(FIA_depth_to_damage$Depth == d)][1]*test9$BLDG_Area[j] #Rep_cost in $/sqft so mutiply by area
    }
  }
  
  test9$DMG_100_FIA<-NA
  for (j in 1: length(test9$DMG_100_FIA)) {
    if (!is.na(test9$USACE_100)[j]){ 
      if (test9$LAND_USE[j] %in% MH){ #mobile homes
        test9$DMG_100_FIA[j]<-FIA_depth_to_damage$MobH_damage[FIA_depth_to_damage$Depth %in% round(test9$USACE_100[j])][1] *test9$IMPR_APPR[j]/100
      }else{
        
        if (!is.na(test9$totLA[j]) & (test9$totLA[j] > (1.5*test9$BLDG_Area[j]))){ #multiple story buildings
          test9$DMG_100_FIA[j]<-FIA_depth_to_damage$TwoFl_Damage[FIA_depth_to_damage$Depth %in% round(p$USACE_100[j])][1] *test9$IMPR_APPR[j]/100
          if (!is.na(test9$USACE_100[j]) & test9$USACE_100[j] > 10) { #control for wildly high depth grid values
            test9$DMG_100_FIA[j]<-max(FIA_depth_to_damage$TwoFl_Damage)*test9$IMPR_APPR[j]/100
          }
        }else{ #single story buildings or buildings with totLA = NA
          test9$DMG_100_FIA[j]<-FIA_depth_to_damage$Res_Damage[FIA_depth_to_damage$Depth %in% round(test9$USACE_100[j])][1] *test9$IMPR_APPR[j]/100 #assuming these depths are in  feet
          if (!is.na(test9$USACE_100[j]) & test9$USACE_100[j] > 10) { #control for wildly high depth grid values
            test9$DMG_100_FIA[j]<-max(FIA_depth_to_damage$Res_Damage)*test9$IMPR_APPR[j]/100
          }
        }
        
      }
    }
  }
  
  
  test9$DMG_500yr_FIA<-NA
  for (j in 1: length(test9$DMG_500yr_FIA)) {
    if (!is.na(test9$USACE_500yr)[j]){ 
      if (test9$LAND_USE[j] %in% MH){ #mobile homes
        test9$DMG_500yr_FIA[j]<-FIA_depth_to_damage$MobH_damage[FIA_depth_to_damage$Depth %in% round(test9$USACE_500yr[j])][1] *test9$IMPR_APPR[j]/100
      }else{
        
        if (!is.na(test9$totLA[j]) & (test9$totLA[j] > (1.5*test9$BLDG_Area[j]))){ #multiple story buildings
          test9$DMG_500yr_FIA[j]<-FIA_depth_to_damage$TwoFl_Damage[FIA_depth_to_damage$Depth %in% round(test9$USACE_500yr[j])][1] *test9$IMPR_APPR[j]/100
          if (!is.na(test9$USACE_500yr[j]) & test9$USACE_500yr[j] > 10) { #control for wildly high depth grid values
            test9$DMG_500yr_FIA[j]<-max(FIA_depth_to_damage$TwoFl_Damage)*test9$IMPR_APPR[j]/100
          }
        }else{ #single story buildings or buildings with totLA = NA
          test9$DMG_500yr_FIA[j]<-FIA_depth_to_damage$Res_Damage[FIA_depth_to_damage$Depth %in% round(test9$USACE_500yr[j])][1] *test9$IMPR_APPR[j]/100 #assuming these depths are in  feet
          if (!is.na(test9$USACE_500yr[j]) & test9$USACE_500yr[j] > 10) { #control for wildly high depth grid values
            test9$DMG_500yr_FIA[j]<-max(FIA_depth_to_damage$Res_Damage)*test9$IMPR_APPR[j]/100
          }
        }
        
      }
    }
  }
  
  test9$DMG_1000yr_FIA<-NA
  for (j in 1: length(test9$DMG_1000yr_FIA)) {
    if (!is.na(test9$USACE_1000yr)[j]){ 
      if (test9$LAND_USE[j] %in% MH){ #mobile homes
        test9$DMG_1000yr_FIA[j]<-FIA_depth_to_damage$MobH_damage[FIA_depth_to_damage$Depth %in% round(test9$USACE_1000yr[j])][1] *test9$IMPR_APPR[j]/100
      }else{
        
        if (!is.na(test9$totLA[j]) & (test9$totLA[j] > (1.5*test9$BLDG_Area[j]))){ #multiple story buildings
          test9$DMG_1000yr_FIA[j]<-FIA_depth_to_damage$TwoFl_Damage[FIA_depth_to_damage$Depth %in% round(test9$USACE_1000yr[j])][1] *test9$IMPR_APPR[j]/100
          if (!is.na(test9$USACE_1000yr[j]) & test9$USACE_1000yr[j] > 10) { #control for wildly high depth grid values
            test9$DMG_1000yr_FIA[j]<-max(FIA_depth_to_damage$TwoFl_Damage)*test9$IMPR_APPR[j]/100
          }
        }else{ #single story buildings or buildings with totLA = NA
          test9$DMG_1000yr_FIA[j]<-FIA_depth_to_damage$Res_Damage[FIA_depth_to_damage$Depth %in% round(test9$USACE_1000yr[j])][1] *test9$IMPR_APPR[j]/100 #assuming these depths are in  feet
          if (!is.na(test9$USACE_1000yr[j]) & test9$USACE_1000yr[j] > 10) { #control for wildly high depth grid values
            test9$DMG_1000yr_FIA[j]<-max(FIA_depth_to_damage$Res_Damage)*test9$IMPR_APPR[j]/100
          }
        }
        
      }
    }
  }
  
  test9$DMG_100_SV<-NA
  for (j in 1: length(test9$DMG_100_SV)){
    test9$DMG_100_SV[j]<-FIA_depth_to_damage$Rep_cost[FIA_depth_to_damage$Depth %in% round(test9$USACE_100[j])][1]*test9$BLDG_Area[j]
    if  (!is.na(test9$USACE_100[j]) & test9$USACE_100[j] > 10) {
      test9$DMG_100_SV[j]<-max(FIA_depth_to_damage$Rep_cost)*test9$BLDG_Area[j]
    }
  }
  
  test9$DMG_500_SV<-NA
  for (j in 1: length(test9$DMG_500_SV)){
    test9$DMG_500_SV[j]<-FIA_depth_to_damage$Rep_cost[FIA_depth_to_damage$Depth %in% round(test9$USACE_500yr[j])][1]*test9$BLDG_Area[j]
    if (!is.na(test9$USACE_500yr[j]) & test9$USACE_500yr[j] > 10) {
      test9$DMG_500_SV[j]<-max(FIA_depth_to_damage$Rep_cost)*test9$BLDG_Area[j]
    }
  }
  
  test9$DMG_1000_SV<-NA
  for (j in 1: length(test9$DMG_1000_SV)){
    test9$DMG_1000_SV[j]<-FIA_depth_to_damage$Rep_cost[FIA_depth_to_damage$Depth %in% round(test9$USACE_1000yr[j])][1]*test9$BLDG_Area[j]
    if  (!is.na(test9$USACE_1000yr[j]) & test9$USACE_1000yr[j] > 10) {
      test9$DMG_1000_SV[j]<-max(FIA_depth_to_damage$Rep_cost)*test9$BLDG_Area[j]
    }
  }
  
  ###################################################################   
  #calculate structural damages assuming no 4ft freeboard reqs
  #######################################################################
  
  test9$DMG_WD_FIA_freeb<-NA
  for (j in 1: length(test9$DMG_WD_FIA_freeb)){ #takes about 5 min
    if (!is.na(test9$DLEVEL_freeb)[j]){ 
      d<- test9$DLEVEL_freeb[j]
      if (test9$LAND_USE[j] %in% MH){
        test9$DMG_WD_FIA_freeb[j]<-FIA_depth_to_damage$MobH_damage[which(FIA_depth_to_damage$Depth == d)][1]*test9$IMPR_APPR[j]/100 
      }else{
        if(test9$totLA[j] > (1.5*test9$BLDG_Area[j])){
          test9$DMG_WD_FIA_freeb[j]<-FIA_depth_to_damage$TwoFl_Damage[which(FIA_depth_to_damage$Depth == d)][1]*test9$totIAPR[j]/100 #for multiple story buildings that may contain multiple tax parcels use the total value of all tax parcels in the building
        } else{
          test9$DMG_WD_FIA_freeb[j]<-FIA_depth_to_damage$Res_Damage[which(FIA_depth_to_damage$Depth == d)][1]*test9$IMPR_APPR[j]/100 
        }
      }
    }
  }
  
  test9$DMG_WD_SV_freeb<-NA #low end estimate as the repair costs are from survey of single family dwellings only
  for (j in 1: length(test9$DMG_WD_SV_freeb)){
    if (!is.na(test9$DLEVEL_freeb)[j]){
      d<- test9$DLEVEL_freeb[j]
      test9$DMG_WD_SV_freeb[j]<-FIA_depth_to_damage$Rep_cost[which(FIA_depth_to_damage$Depth == d)][1]*test9$BLDG_Area[j]
    }
  }
  
  test9$DMG_100_FIA_freeb<-NA
  for (j in 1: length(test9$DMG_100_FIA_freeb)) {
    if (!is.na(test9$USACE_100_freeb)[j]){ 
      if (test9$LAND_USE[j] %in% MH){ #mobile homes
        test9$DMG_100_FIA_freeb[j]<-FIA_depth_to_damage$MobH_damage[FIA_depth_to_damage$Depth %in% round(test9$USACE_100_freeb[j])][1] *test9$IMPR_APPR[j]/100
      }else{
        
        if (!is.na(test9$totLA[j]) & (test9$totLA[j] > (1.5*test9$BLDG_Area[j]))){ #multiple story buildings
          test9$DMG_100_FIA_freeb[j]<-FIA_depth_to_damage$TwoFl_Damage[FIA_depth_to_damage$Depth %in% round(test9$USACE_100_freeb[j])][1] *test9$IMPR_APPR[j]/100
          if (!is.na(test9$USACE_100_freeb[j]) & test9$USACE_100_freeb[j] > 10) { #control for wildly high depth grid values
            test9$DMG_100_FIA_freeb[j]<-max(FIA_depth_to_damage$TwoFl_Damage)*test9$IMPR_APPR[j]/100
          }
        }else{ #single story buildings or buildings with totLA = NA
          test9$DMG_100_FIA_freeb[j]<-FIA_depth_to_damage$Res_Damage[FIA_depth_to_damage$Depth %in% round(test9$USACE_100_freeb[j])][1] *test9$IMPR_APPR[j]/100 #assuming these depths are in  feet
          if (!is.na(test9$USACE_100_freeb[j]) & test9$USACE_100_freeb[j] > 10) { #control for wildly high depth grid values
            test9$DMG_100_FIA_freeb[j]<-max(FIA_depth_to_damage$Res_Damage)*test9$IMPR_APPR[j]/100
          }
        }
        
      }
    }
  }
  
  
  test9$DMG_500yr_FIA_freeb<-NA
  for (j in 1: length(test9$DMG_500yr_FIA_freeb)) {
    if (!is.na(test9$USACE_500yr_freeb)[j]){ 
      if (test9$LAND_USE[j] %in% MH){ #mobile homes
        test9$DMG_500yr_FIA_freeb[j]<-FIA_depth_to_damage$MobH_damage[FIA_depth_to_damage$Depth %in% round(test9$USACE_500yr_freeb[j])][1] *test9$IMPR_APPR[j]/100
      }else{
        
        if (!is.na(test9$totLA[j]) & (test9$totLA[j] > (1.5*test9$BLDG_Area[j]))){ #multiple story buildings
          test9$DMG_500yr_FIA_freeb[j]<-FIA_depth_to_damage$TwoFl_Damage[FIA_depth_to_damage$Depth %in% round(test9$USACE_500yr_freeb[j])][1] *test9$IMPR_APPR[j]/100
          if (!is.na(test9$USACE_500yr_freeb[j]) & test9$USACE_500yr_freeb[j] > 10) { #control for wildly high depth grid values
            test9$DMG_500yr_FIA_freeb[j]<-max(FIA_depth_to_damage$TwoFl_Damage)*test9$IMPR_APPR[j]/100
          }
        }else{ #single story buildings or buildings with totLA = NA
          test9$DMG_500yr_FIA_freeb[j]<-FIA_depth_to_damage$Res_Damage[FIA_depth_to_damage$Depth %in% round(test9$USACE_500yr_freeb[j])][1] *test9$IMPR_APPR[j]/100 #assuming these depths are in  feet
          if (!is.na(test9$USACE_500yr_freeb[j]) & test9$USACE_500yr_freeb[j] > 10) { #control for wildly high depth grid values
            test9$DMG_500yr_FIA_freeb[j]<-max(FIA_depth_to_damage$Res_Damage)*test9$IMPR_APPR[j]/100
          }
        }
        
      }
    }
  }
  
  test9$DMG_1000yr_FIA_freeb<-NA
  for (j in 1: length(test9$DMG_1000yr_FIA_freeb)) {
    if (!is.na(test9$USACE_1000yr_freeb)[j]){ 
      if (test9$LAND_USE[j] %in% MH){ #mobile homes
        test9$DMG_1000yr_FIA_freeb[j]<-FIA_depth_to_damage$MobH_damage[FIA_depth_to_damage$Depth %in% round(test9$USACE_1000yr_freeb[j])][1] *test9$IMPR_APPR[j]/100
      }else{
        
        if (!is.na(test9$totLA[j]) & (test9$totLA[j] > (1.5*test9$BLDG_Area[j]))){ #multiple story buildings
          test9$DMG_1000yr_FIA_freeb[j]<-FIA_depth_to_damage$TwoFl_Damage[FIA_depth_to_damage$Depth %in% round(test9$USACE_1000yr_freeb[j])][1] *test9$IMPR_APPR[j]/100
          if (!is.na(test9$USACE_1000yr_freeb[j]) & test9$USACE_1000yr_freeb[j] > 10) { #control for wildly high depth grid values
            test9$DMG_1000yr_FIA_freeb[j]<-max(FIA_depth_to_damage$TwoFl_Damage)*test9$IMPR_APPR[j]/100
          }
        }else{ #single story buildings or buildings with totLA = NA
          test9$DMG_1000yr_FIA_freeb[j]<-FIA_depth_to_damage$Res_Damage[FIA_depth_to_damage$Depth %in% round(test9$USACE_1000yr_freeb[j])][1] *test9$IMPR_APPR[j]/100 #assuming these depths are in  feet
          if (!is.na(test9$USACE_1000yr_freeb[j]) & test9$USACE_1000yr_freeb[j] > 10) { #control for wildly high depth grid values
            test9$DMG_1000yr_FIA_freeb[j]<-max(FIA_depth_to_damage$Res_Damage)*test9$IMPR_APPR[j]/100
          }
        }
        
      }
    }
  }
  
  test9$DMG_100_SV_freeb<-NA
  for (j in 1: length(test9$DMG_100_SV_freeb)){
    test9$DMG_100_SV_freeb[j]<-FIA_depth_to_damage$Rep_cost[FIA_depth_to_damage$Depth %in% round(test9$USACE_100_freeb[j])][1]*test9$BLDG_Area[j]
    if  (!is.na(test9$USACE_100_freeb[j]) & test9$USACE_100_freeb[j] > 10) {
      test9$DMG_100_SV_freeb[j]<-max(FIA_depth_to_damage$Rep_cost)*test9$BLDG_Area[j]
    }
  }
  
  test9$DMG_500_SV_freeb<-NA
  for (j in 1: length(test9$DMG_500_SV_freeb)){
    test9$DMG_500_SV_freeb[j]<-FIA_depth_to_damage$Rep_cost[FIA_depth_to_damage$Depth %in% round(test9$USACE_500yr_freeb[j])][1]*test9$BLDG_Area[j]
    if (!is.na(test9$USACE_500yr_freeb[j]) & test9$USACE_500yr_freeb[j] > 10) {
      test9$DMG_500_SV_freeb[j]<-max(FIA_depth_to_damage$Rep_cost)*test9$BLDG_Area[j]
    }
  }
  
  test9$DMG_1000_SV_freeb<-NA
  for (j in 1: length(test9$DMG_1000_SV_freeb)){
    test9$DMG_1000_SV_freeb[j]<-FIA_depth_to_damage$Rep_cost[FIA_depth_to_damage$Depth %in% round(test9$USACE_1000yr_freeb[j])][1]*test9$BLDG_Area[j]
    if  (!is.na(test9$USACE_1000yr_freeb[j]) & test9$USACE_1000yr_freeb[j] > 10) {
      test9$DMG_1000_SV_freeb[j]<-max(FIA_depth_to_damage$Rep_cost)*test9$BLDG_Area[j]
    }
  }
  
  #####################################################################################################
  #calculate content damages, assuming 4ft freeboard req in place (observed/actual conditions)
  ####################################################################################################
  test9$DMGCONT_WD_FIA<-NA
  for (j in 1: length(test9$DMGCONT_WD_FIA)){ #takes about 5 min
    if (!is.na(test9$DLEVEL_depth)[j]){ 
      d<- test9$DLEVEL_depth[j]
      if(test9$totLA[j] > (1.5*test9$BLDG_Area[j])){
        test9$DMGCONT_WD_FIA[j]<-FIA_depth_to_damage$TwoFl_contents_dmg[which(FIA_depth_to_damage$Depth == d)][1]*test9$totIAPR[j]/100 #for multiple story buildings that may contain multiple tax parcels use the total value of all tax parcels in the building
      } else {
        test9$DMGCONT_WD_FIA[j]<-FIA_depth_to_damage$OneFl_contents_dmg[which(FIA_depth_to_damage$Depth == d)][1]*test9$IMPR_APPR[j]/100 
      }
      if (test9$LAND_USE[j] %in% residential){
        test9$DMGCONT_WD_FIA[j]<-test9$DMGCONT_WD_FIA[j]*res_cont #if residential building mutiply by contents to value ratio of 0.5
      } else {
        test9$DMGCONT_WD_FIA[j]<-test9$DMGCONT_WD_FIA[j]*nonres_cont #if commercial/industrial/other multiply by contents to value ratio of 1.25 (mid point in HAZUS tables)
      }
    }
  }
  
  
  
  test9$DMGCONT_100_FIA<-NA
  for (j in 1: length(test9$DMGCONT_100_FIA)) {
    if (!is.na(test9$USACE_100)[j]){ 
      if (!is.na(test9$totLA[j]) & (test9$totLA[j] > (1.5*test9$BLDG_Area[j]))){ #multiple story buildings
        test9$DMGCONT_100_FIA[j]<-FIA_depth_to_damage$TwoFl_contents_dmg[FIA_depth_to_damage$Depth %in% round(test9$USACE_100[j])][1] *test9$IMPR_APPR[j]/100
        if (!is.na(test9$USACE_100[j]) & test9$USACE_100[j] > 10) { #control for wildly high depth grid values
          test9$DMGCONT_100_FIA[j]<-max(FIA_depth_to_damage$TwoFl_contents_dmg)*test9$IMPR_APPR[j]/100
        }
      }else{ #single story buildings or buildings with totLA = NA
        test9$DMGCONT_100_FIA[j]<-FIA_depth_to_damage$OneFl_contents_dmg[FIA_depth_to_damage$Depth %in% round(test9$USACE_100[j])][1] *test9$IMPR_APPR[j]/100 #assuming these depths are in  feet
        if (!is.na(test9$USACE_100[j]) & test9$USACE_100[j] > 10) { #control for wildly high depth grid values
          test9$DMGCONT_100_FIA[j]<-max(FIA_depth_to_damage$OneFl_contents_dmg)*test9$IMPR_APPR[j]/100
        }
      }
      if (test9$LAND_USE[j] %in% residential){
        test9$DMGCONT_100_FIA[j]<-test9$DMGCONT_100_FIA[j]*res_cont #if residential building mutiply by contents to value ratio of 0.5
      } else {
        test9$DMGCONT_100_FIA[j]<-test9$DMGCONT_100_FIA[j]*nonres_cont #if commercial/industrial/other multiply by contents to value ratio of 1.25 (mid point in HAZUS tables)
      }
    }
  }
  
  
  
  
  test9$DMGCONT_500yr_FIA<-NA
  for (j in 1: length(test9$DMGCONT_500yr_FIA)) {
    if (!is.na(test9$USACE_500yr)[j]){ 
      if (!is.na(test9$totLA[j]) & (test9$totLA[j] > (1.5*test9$BLDG_Area[j]))){ #multiple story buildings
        test9$DMGCONT_500yr_FIA[j]<-FIA_depth_to_damage$TwoFl_contents_dmg[FIA_depth_to_damage$Depth %in% round(test9$USACE_500yr[j])][1] *test9$IMPR_APPR[j]/100
        if (!is.na(test9$USACE_500yr[j]) & test9$USACE_500yr[j] > 10) { #control for wildly high depth grid values
          test9$DMGCONT_500yr_FIA[j]<-max(FIA_depth_to_damage$TwoFl_contents_dmg)*test9$IMPR_APPR[j]/100
        }
      }else{ #single story buildings or buildings with totLA = NA
        test9$DMGCONT_500yr_FIA[j]<-FIA_depth_to_damage$OneFl_contents_dmg[FIA_depth_to_damage$Depth %in% round(test9$USACE_500yr[j])][1] *test9$IMPR_APPR[j]/100 #assuming these depths are in  feet
        if (!is.na(test9$USACE_500yr[j]) & test9$USACE_500yr[j] > 10) { #control for wildly high depth grid values
          test9$DMGCONT_500yr_FIA[j]<-max(FIA_depth_to_damage$OneFl_contents_dmg)*test9$IMPR_APPR[j]/100
        }
      }
      if (test9$LAND_USE[j] %in% residential){
        test9$DMGCONT_500yr_FIA[j]<-test9$DMGCONT_500yr_FIA[j]*res_cont #if residential building mutiply by contents to value ratio of 0.5
      } else {
        test9$DMGCONT_500yr_FIA[j]<-test9$DMGCONT_500yr_FIA[j]*nonres_cont #if commercial/industrial/other multiply by contents to value ratio of 1.25 (mid point in HAZUS tables)
      }
    }
  }
  
  test9$DMGCONT_1000yr_FIA<-NA
  for (j in 1: length(test9$DMGCONT_1000yr_FIA)) {
    if (!is.na(test9$USACE_1000yr)[j]){ 
      if (!is.na(test9$totLA[j]) & (test9$totLA[j] > (1.5*test9$BLDG_Area[j]))){ #multiple story buildings
        test9$DMGCONT_1000yr_FIA[j]<-FIA_depth_to_damage$TwoFl_contents_dmg[FIA_depth_to_damage$Depth %in% round(test9$USACE_1000yr[j])][1] *test9$IMPR_APPR[j]/100
        if (!is.na(test9$USACE_1000yr[j]) & test9$USACE_1000yr[j] > 10) { #control for wildly high depth grid values
          test9$DMGCONT_1000yr_FIA[j]<-max(FIA_depth_to_damage$TwoFl_contents_dmg)*test9$IMPR_APPR[j]/100
        }
      }else{ #single story buildings or buildings with totLA = NA
        test9$DMGCONT_1000yr_FIA[j]<-FIA_depth_to_damage$OneFl_contents_dmg[FIA_depth_to_damage$Depth %in% round(test9$USACE_1000yr[j])][1] *test9$IMPR_APPR[j]/100 #assuming these depths are in  feet
        if (!is.na(test9$USACE_1000yr[j]) & test9$USACE_1000yr[j] > 10) { #control for wildly high depth grid values
          test9$DMGCONT_1000yr_FIA[j]<-max(FIA_depth_to_damage$OneFl_contents_dmg)*test9$IMPR_APPR[j]/100
        }
      }
      if (test9$LAND_USE[j] %in% residential){
        test9$DMGCONT_1000yr_FIA[j]<-test9$DMGCONT_1000yr_FIA[j]*res_cont #if residential building mutiply by contents to value ratio of 0.5
      } else {
        test9$DMGCONT_1000yr_FIA[j]<-test9$DMGCONT_1000yr_FIA[j]*nonres_cont #if commercial/industrial/other multiply by contents to value ratio of 1.25 (mid point in HAZUS tables)
      }
    }
  }
  
  
  ###################################################################   
  #calculate content damages assuming no 4ft freeboard reqs
  #######################################################################
  
  test9$DMGCONT_WD_FIA_freeb<-NA
  for (j in 1: length(test9$DMGCONT_WD_FIA_freeb)){ #takes about 5 min
    if (!is.na(test9$DLEVEL_freeb)[j]){ 
      if(test9$totLA[j] > (1.5*test9$BLDG_Area[j])){
        test9$DMGCONT_WD_FIA_freeb[j]<-FIA_depth_to_damage$TwoFl_contents_dmg[which(FIA_depth_to_damage$Depth == d)][1]*test9$totIAPR[j]/100 #for multiple story buildings that may contain multiple tax parcels use the total value of all tax parcels in the building
      } else{
        test9$DMGCONT_WD_FIA_freeb[j]<-FIA_depth_to_damage$OneFl_contents_dmg[which(FIA_depth_to_damage$Depth == d)][1]*test9$IMPR_APPR[j]/100 
      }
      if (test9$LAND_USE[j] %in% residential){
        test9$DMGCONT_WD_FIA_freeb[j]<-test9$DMGCONT_WD_FIA_freeb[j]*res_cont #if residential building mutiply by contents to value ratio of 0.5
      } else {
        test9$DMGCONT_WD_FIA_freeb[j]<-test9$DMGCONT_WD_FIA_freeb[j]*nonres_cont #if commercial/industrial/other multiply by contents to value ratio of 1.25 (mid point in HAZUS tables)
      }
    }
  }
  
  
  test9$DMGCONT_100_FIA_freeb<-NA
  for (j in 1: length(test9$DMGCONT_100_FIA_freeb)) {
    if (!is.na(test9$USACE_100_freeb)[j]){ 
      if (!is.na(test9$totLA[j]) & (test9$totLA[j] > (1.5*test9$BLDG_Area[j]))){ #multiple story buildings
        test9$DMGCONT_100_FIA_freeb[j]<-FIA_depth_to_damage$TwoFl_contents_dmg[FIA_depth_to_damage$Depth %in% round(test9$USACE_100_freeb[j])][1] *test9$IMPR_APPR[j]/100
        if (!is.na(test9$USACE_100_freeb[j]) & test9$USACE_100_freeb[j] > 10) { #control for wildly high depth grid values
          test9$DMGCONT_100_FIA_freeb[j]<-max(FIA_depth_to_damage$TwoFl_contents_dmg)*test9$IMPR_APPR[j]/100
        }
      }else{ #single story buildings or buildings with totLA = NA
        test9$DMGCONT_100_FIA_freeb[j]<-FIA_depth_to_damage$OneFl_contents_dmg[FIA_depth_to_damage$Depth %in% round(test9$USACE_100_freeb[j])][1] *test9$IMPR_APPR[j]/100 #assuming these depths are in  feet
        if (!is.na(test9$USACE_100_freeb[j]) & test9$USACE_100_freeb[j] > 10) { #control for wildly high depth grid values
          test9$DMGCONT_100_FIA_freeb[j]<-max(FIA_depth_to_damage$OneFl_contents_dmg)*test9$IMPR_APPR[j]/100
        }
      }
      if (test9$LAND_USE[j] %in% residential){
        test9$DMGCONT_100_FIA_freeb[j]<-test9$DMGCONT_100_FIA_freeb[j]*res_cont #if residential building mutiply by contents to value ratio of 0.5
      } else {
        test9$DMGCONT_100_FIA_freeb[j]<-test9$DMGCONT_100_FIA_freeb[j]*nonres_cont #if commercial/industrial/other multiply by contents to value ratio of 1.25 (mid point in HAZUS tables)
      }
    }
  }
  
  
  test9$DMGCONT_500yr_FIA_freeb<-NA
  for (j in 1: length(test9$DMGCONT_500yr_FIA_freeb)) {
    if (!is.na(test9$USACE_500yr_freeb)[j]){ 
      if (!is.na(test9$totLA[j]) & (test9$totLA[j] > (1.5*test9$BLDG_Area[j]))){ #multiple story buildings
        test9$DMGCONT_500yr_FIA_freeb[j]<-FIA_depth_to_damage$TwoFl_contents_dmg[FIA_depth_to_damage$Depth %in% round(test9$USACE_500yr_freeb[j])][1] *test9$IMPR_APPR[j]/100
        if (!is.na(test9$USACE_500yr_freeb[j]) & test9$USACE_500yr_freeb[j] > 10) { #control for wildly high depth grid values
          test9$DMGCONT_500yr_FIA_freeb[j]<-max(FIA_depth_to_damage$TwoFl_contents_dmg)*test9$IMPR_APPR[j]/100
        }
      }else{ #single story buildings or buildings with totLA = NA
        test9$DMGCONT_500yr_FIA_freeb[j]<-FIA_depth_to_damage$OneFl_contents_dmg[FIA_depth_to_damage$Depth %in% round(test9$USACE_500yr_freeb[j])][1] *test9$IMPR_APPR[j]/100 #assuming these depths are in  feet
        if (!is.na(test9$USACE_500yr_freeb[j]) & test9$USACE_500yr_freeb[j] > 10) { #control for wildly high depth grid values
          test9$DMGCONT_500yr_FIA_freeb[j]<-max(FIA_depth_to_damage$OneFl_contents_dmg)*test9$IMPR_APPR[j]/100
        }
      }
      if (test9$LAND_USE[j] %in% residential){
        test9$DMGCONT_500yr_FIA_freeb[j]<-test9$DMGCONT_500yr_FIA_freeb[j]*res_cont #if residential building mutiply by contents to value ratio of 0.5
      } else {
        test9$DMGCONT_500yr_FIA_freeb[j]<-test9$DMGCONT_500yr_FIA_freeb[j]*nonres_cont #if commercial/industrial/other multiply by contents to value ratio of 1.25 (mid point in HAZUS tables)
      }
    }
  }
  
  
  test9$DMGCONT_1000yr_FIA_freeb<-NA
  for (j in 1: length(test9$DMGCONT_1000yr_FIA_freeb)) {
    if (!is.na(test9$USACE_1000yr_freeb)[j]){ 
      if (!is.na(test9$totLA[j]) & (test9$totLA[j] > (1.5*test9$BLDG_Area[j]))){ #multiple story buildings
        test9$DMGCONT_1000yr_FIA_freeb[j]<-FIA_depth_to_damage$TwoFl_contents_dmg[FIA_depth_to_damage$Depth %in% round(test9$USACE_1000yr_freeb[j])][1] *test9$IMPR_APPR[j]/100
        if (!is.na(test9$USACE_1000yr_freeb[j]) & test9$USACE_1000yr_freeb[j] > 10) { #control for wildly high depth grid values
          test9$DMGCONT_1000yr_FIA_freeb[j]<-max(FIA_depth_to_damage$TwoFl_contents_dmg)*test9$IMPR_APPR[j]/100
        }
      }else{ #single story buildings or buildings with totLA = NA
        test9$DMGCONT_1000yr_FIA_freeb[j]<-FIA_depth_to_damage$OneFl_contents_dmg[FIA_depth_to_damage$Depth %in% round(test9$USACE_1000yr_freeb[j])][1] *test9$IMPR_APPR[j]/100 #assuming these depths are in  feet
        if (!is.na(test9$USACE_1000yr_freeb[j]) & test9$USACE_1000yr_freeb[j] > 10) { #control for wildly high depth grid values
          test9$DMGCONT_1000yr_FIA_freeb[j]<-max(FIA_depth_to_damage$OneFl_contents_dmg)*test9$IMPR_APPR[j]/100
        }
      }
      if (test9$LAND_USE[j] %in% residential){
        test9$DMGCONT_1000yr_FIA_freeb[j]<-test9$DMGCONT_1000yr_FIA_freeb[j]*res_cont #if residential building mutiply by contents to value ratio of 0.5
      } else {
        test9$DMGCONT_1000yr_FIA_freeb[j]<-test9$DMGCONT_1000yr_FIA_freeb[j]*nonres_cont #if commercial/industrial/other multiply by contents to value ratio of 1.25 (mid point in HAZUS tables)
      }
    }
  }
  
  
  saveRDS(test9, paste0(wd,"/data/new/", "damages", pbwsnames[i]) )
}

###########################################################################################################################################
#Calculate Costs Associated with Relocation Time, Labor Costs 
#######################################################################################

pbwsnames<-c("streampbws2005b.RDS", "streampbws2006b.RDS", "streampbws2007b.RDS", "streampbws2008b.RDS", "streampbws2009b.RDS", "streampbws2010b.RDS", "streampbws2011b.RDS",
             "streampbws2012b.RDS", "streampbws2013b.RDS","streampbws2014b.RDS","streampbws2015b.RDS",
             "newpbws2006b_nb.RDS","newpbws2007b_nb.RDS","newpbws2008b_nb.RDS","newpbws2009b_nb.RDS","newpbws2010b_nb.RDS", "newpbws2011b_nb.RDS","newpbws2012b_nb.RDS", "newpbws2013b_nb.RDS","newpbws2014b_nb.RDS","newpbws2015b_nb.RDS",
             "newpbws2010b_ab.RDS", "newpbws2011b_ab.RDS","newpbws2012b_ab.RDS", "newpbws2013b_ab.RDS","newpbws2014b_ab.RDS","newpbws2015b_ab.RDS",
             "newpbws2010b_w.RDS", "newpbws2011b_w.RDS","newpbws2012b_w.RDS", "newpbws2013b_w.RDS","newpbws2014b_w.RDS","newpbws2015b_w.RDS") 

#Ave cost of relocation per sqft per day from survey
reloc_cpd<-.009

#Estimated cost of labor per hour 
labor_cph<-19.21

#Rental cost for residential homes from HAZUS Table 14.10 RES1 in $ per ft2 per day
res_rent<-0.02 

#Rental cost for commerical space from HAZUS Table 14.10 COM1 in $ per ft2 per day
nonres_rent<-0.04 

#Disruption cost for residential homes from HAZUS Table 14.10 RES1 in $ per ft2 
res_disrup<-0.82 

#Disruption cost for commerical space from HAZUS Table 14.10 COM1 in $ per ft2 
nonres_disrup<-1.09 


n<-length(pbwsnames)
registerDoParallel(n)
getDoParWorkers() 

foreach (i=1:n) %dopar% { 
  p<-readRDS(paste0(wd,"/data/new/", "damages", pbwsnames[i]) )
  
  #########################################################################################################################
  #Calculate relocation costs using Hazus and survey estimates for windshield and HAZUS depths, observed freeboard scenario
  ########################################################################################################################
  p$Reloc_WD_HZ<-NA
  for (j in 1: length(p$Reloc_WD_HZ)){ 
    if (!is.na(p$DLEVEL_depth)[j]){ 
      d<- p$DLEVEL_depth[j]
      if (p$LAND_USE[j] %in% residential){ #pull relocation time and calculate reloc costs for residential buildings (function of rent and disruption costs)
        p$Reloc_WD_HZ[j]<-FIA_depth_to_damage$Res_restime[which(FIA_depth_to_damage$Depth == d)][1]*p$BLDG_Area[j]*res_rent + p$BLDG_Area[j]*res_disrup
      }else{ #same for non residential
        p$Reloc_WD_HZ[j]<-FIA_depth_to_damage$Nonres_restime[which(FIA_depth_to_damage$Depth == d)][1]*p$BLDG_Area[j]*nonres_rent + p$BLDG_Area[j]*nonres_disrup
      }
    }
  }
  
  p$Reloc_WD_SV<-NA #Calculate relocation costs
  for (j in 1: length(p$Reloc_WD_SV)){
    if (!is.na(p$DLEVEL_depth)[j]){
      d<- p$DLEVEL_depth[j]
      p$Reloc_WD_SV[j]<-FIA_depth_to_damage$Ave_reloc[which(FIA_depth_to_damage$Depth == d)][1]*p$BLDG_Area[j]*reloc_cpd
    }
  }
  
  p$Reloc_100_HZ<-NA
  for (j in 1: length(p$Reloc_100_HZ)) {
    if (!is.na(p$USACE_100)[j]){ 
      if (p$LAND_USE[j] %in% residential){ #residential relocation costs
        p$Reloc_100_HZ[j]<-FIA_depth_to_damage$Res_restime[FIA_depth_to_damage$Depth %in% round(p$USACE_100[j])][1]*p$BLDG_Area[j]*res_rent + p$BLDG_Area[j]*res_disrup
        if (!is.na(p$USACE_100[j]) & p$USACE_100[j] > 10) { #control for wildly high depth grid values
          p$Reloc_100_HZ[j]<-max(FIA_depth_to_damage$Res_restime)*p$BLDG_Area[j]*res_rent + p$BLDG_Area[j]*res_disrup
        }
      }else{ #nonres reloc costs
        p$Reloc_100_HZ[j]<-FIA_depth_to_damage$Nonres_restime[FIA_depth_to_damage$Depth %in% round(p$USACE_100[j])][1]*p$BLDG_Area[j]*nonres_rent + p$BLDG_Area[j]*nonres_disrup
        if (!is.na(p$USACE_100[j]) & p$USACE_100[j] > 10) { #control for wildly high depth grid values
          p$Reloc_100_HZ[j]<-max(FIA_depth_to_damage$Nonres_restime)*p$BLDG_Area[j]*nonres_rent + p$BLDG_Area[j]*nonres_disrup
        }
      }
    }
  }
  
  p$Reloc_100_SV<-NA
  for (j in 1: length(p$Reloc_100_SV)){
    p$Reloc_100_SV[j]<-FIA_depth_to_damage$Ave_reloc[FIA_depth_to_damage$Depth %in% round(p$USACE_100[j])][1]*p$BLDG_Area[j]*reloc_cpd
    if  (!is.na(p$USACE_100[j]) & p$USACE_100[j] > 10) {
      p$Reloc_100_SV[j]<-max(FIA_depth_to_damage$Ave_reloc)*p$BLDG_Area[j]*reloc_cpd
    }
  }
  
  p$Reloc_500yr_HZ<-NA
  for (j in 1: length(p$Reloc_500yr_HZ)) {
    if (!is.na(p$USACE_500yr)[j]){ 
      if (p$LAND_USE[j] %in% residential){ #residential relocation costs
        p$Reloc_500yr_HZ[j]<-FIA_depth_to_damage$Res_restime[FIA_depth_to_damage$Depth %in% round(p$USACE_500yr[j])][1]*p$BLDG_Area[j]*res_rent + p$BLDG_Area[j]*res_disrup
        if (!is.na(p$USACE_500yr[j]) & p$USACE_500yr[j] > 10) { #control for wildly high depth grid values
          p$Reloc_500yr_HZ[j]<-max(FIA_depth_to_damage$Res_restime)*p$BLDG_Area[j]*res_rent + p$BLDG_Area[j]*res_disrup
        }
      }else{ #nonres reloc costs
        p$Reloc_500yr_HZ[j]<-FIA_depth_to_damage$Nonres_restime[FIA_depth_to_damage$Depth %in% round(p$USACE_500yr[j])][1]*p$BLDG_Area[j]*nonres_rent + p$BLDG_Area[j]*nonres_disrup
        if (!is.na(p$USACE_500yr[j]) & p$USACE_500yr[j] > 10) { #control for wildly high depth grid values
          p$Reloc_500yr_HZ[j]<-max(FIA_depth_to_damage$Nonres_restime)*p$BLDG_Area[j]*nonres_rent + p$BLDG_Area[j]*nonres_disrup
        }
      }
    }
  }
  
  p$Reloc_500yr_SV<-NA
  for (j in 1: length(p$Reloc_500yr_SV)){
    p$Reloc_500yr_SV[j]<-FIA_depth_to_damage$Ave_reloc[FIA_depth_to_damage$Depth %in% round(p$USACE_500yr[j])][1]*p$BLDG_Area[j]*reloc_cpd
    if  (!is.na(p$USACE_500yr[j]) & p$USACE_500yr[j] > 10) {
      p$Reloc_500yr_SV[j]<-max(FIA_depth_to_damage$Ave_reloc)*p$BLDG_Area[j]*reloc_cpd
    }
  }
  
  p$Reloc_1000yr_HZ<-NA
  for (j in 1: length(p$Reloc_1000yr_HZ)) {
    if (!is.na(p$USACE_1000yr)[j]){ 
      if (p$LAND_USE[j] %in% residential){ #residential relocation costs
        p$Reloc_1000yr_HZ[j]<-FIA_depth_to_damage$Res_restime[FIA_depth_to_damage$Depth %in% round(p$USACE_1000yr[j])][1]*p$BLDG_Area[j]*res_rent + p$BLDG_Area[j]*res_disrup
        if (!is.na(p$USACE_1000yr[j]) & p$USACE_1000yr[j] > 10) { #control for wildly high depth grid values
          p$Reloc_1000yr_HZ[j]<-max(FIA_depth_to_damage$Res_restime)*p$BLDG_Area[j]*res_rent + p$BLDG_Area[j]*res_disrup
        }
      }else{ #nonres reloc costs
        p$Reloc_1000yr_HZ[j]<-FIA_depth_to_damage$Nonres_restime[FIA_depth_to_damage$Depth %in% round(p$USACE_1000yr[j])][1]*p$BLDG_Area[j]*nonres_rent + p$BLDG_Area[j]*nonres_disrup
        if (!is.na(p$USACE_1000yr[j]) & p$USACE_1000yr[j] > 10) { #control for wildly high depth grid values
          p$Reloc_1000yr_HZ[j]<-max(FIA_depth_to_damage$Nonres_restime)*p$BLDG_Area[j]*nonres_rent + p$BLDG_Area[j]*nonres_disrup
        }
      }
    }
  }
  
  p$Reloc_1000yr_SV<-NA
  for (j in 1: length(p$Reloc_1000yr_SV)){
    p$Reloc_1000yr_SV[j]<-FIA_depth_to_damage$Ave_reloc[FIA_depth_to_damage$Depth %in% round(p$USACE_1000yr[j])][1]*p$BLDG_Area[j]*reloc_cpd
    if  (!is.na(p$USACE_1000yr[j]) & p$USACE_1000yr[j] > 10) {
      p$Reloc_1000yr_SV[j]<-max(FIA_depth_to_damage$Ave_reloc)*p$BLDG_Area[j]*reloc_cpd
    }
  }
  
  #########################################################################################################################
  #Calculate relocation costs using Hazus and survey estimates for windshield and HAZUS depths, no freeboard scenario
  ########################################################################################################################
  
  p$Reloc_WD_HZ_freeb<-NA
  for (j in 1: length(p$Reloc_WD_HZ_freeb)){ 
    if (!is.na(p$DLEVEL_freeb)[j]){ 
      d<- p$DLEVEL_freeb[j]
      if (p$LAND_USE[j] %in% residential){ #pull relocation time and calculate reloc costs for residential buildings (function of rent and disruption costs)
        p$Reloc_WD_HZ_freeb[j]<-FIA_depth_to_damage$Res_restime[which(FIA_depth_to_damage$Depth == d)][1]*p$BLDG_Area[j]*res_rent + p$BLDG_Area[j]*res_disrup
      }else{ #same for non residential
        p$Reloc_WD_HZ_freeb[j]<-FIA_depth_to_damage$Nonres_restime[which(FIA_depth_to_damage$Depth == d)][1]*p$BLDG_Area[j]*nonres_rent + p$BLDG_Area[j]*nonres_disrup
      }
    }
  }
  
  p$Reloc_WD_SV_freeb<-NA #Calculate relocation costs
  for (j in 1: length(p$Reloc_WD_SV_freeb)){
    if (!is.na(p$DLEVEL_freeb)[j]){
      d<- p$DLEVEL_freeb[j]
      p$Reloc_WD_SV_freeb[j]<-FIA_depth_to_damage$Ave_reloc[which(FIA_depth_to_damage$Depth == d)][1]*p$BLDG_Area[j]*reloc_cpd
    }
  }
  
  p$Reloc_100_HZ_freeb<-NA
  for (j in 1: length(p$Reloc_100_HZ_freeb)) {
    if (!is.na(p$USACE_100_freeb)[j]){ 
      if (p$LAND_USE[j] %in% residential){ #residential relocation costs
        p$Reloc_100_HZ_freeb[j]<-FIA_depth_to_damage$Res_restime[FIA_depth_to_damage$Depth %in% round(p$USACE_100_freeb[j])][1]*p$BLDG_Area[j]*res_rent + p$BLDG_Area[j]*res_disrup
        if (!is.na(p$USACE_100_freeb[j]) & p$USACE_100_freeb[j] > 10) { #control for wildly high depth grid values
          p$Reloc_100_HZ_freeb[j]<-max(FIA_depth_to_damage$Res_restime)*p$BLDG_Area[j]*res_rent + p$BLDG_Area[j]*res_disrup
        }
      }else{ #nonres reloc costs
        p$Reloc_100_HZ_freeb[j]<-FIA_depth_to_damage$Nonres_restime[FIA_depth_to_damage$Depth %in% round(p$USACE_100_freeb[j])][1]*p$BLDG_Area[j]*nonres_rent + p$BLDG_Area[j]*nonres_disrup
        if (!is.na(p$USACE_100_freeb[j]) & p$USACE_100_freeb[j] > 10) { #control for wildly high depth grid values
          p$Reloc_100_HZ_freeb[j]<-max(FIA_depth_to_damage$Nonres_restime)*p$BLDG_Area[j]*nonres_rent + p$BLDG_Area[j]*nonres_disrup
        }
      }
    }
  }
  
  p$Reloc_100_SV_freeb<-NA
  for (j in 1: length(p$Reloc_100_SV_freeb)){
    p$Reloc_100_SV_freeb[j]<-FIA_depth_to_damage$Ave_reloc[FIA_depth_to_damage$Depth %in% round(p$USACE_100_freeb[j])][1]*p$BLDG_Area[j]*reloc_cpd
    if  (!is.na(p$USACE_100_freeb[j]) & p$USACE_100_freeb[j] > 10) {
      p$Reloc_100_SV_freeb[j]<-max(FIA_depth_to_damage$Ave_reloc)*p$BLDG_Area[j]*reloc_cpd
    }
  }
  
  p$Reloc_500yr_HZ_freeb<-NA
  for (j in 1: length(p$Reloc_500yr_HZ_freeb)) {
    if (!is.na(p$USACE_500yr_freeb)[j]){ 
      if (p$LAND_USE[j] %in% residential){ #residential relocation costs
        p$Reloc_500yr_HZ_freeb[j]<-FIA_depth_to_damage$Res_restime[FIA_depth_to_damage$Depth %in% round(p$USACE_500yr_freeb[j])][1]*p$BLDG_Area[j]*res_rent + p$BLDG_Area[j]*res_disrup
        if (!is.na(p$USACE_500yr_freeb[j]) & p$USACE_500yr_freeb[j] > 10) { #control for wildly high depth grid values
          p$Reloc_500yr_HZ_freeb[j]<-max(FIA_depth_to_damage$Res_restime)*p$BLDG_Area[j]*res_rent + p$BLDG_Area[j]*res_disrup
        }
      }else{ #nonres reloc costs
        p$Reloc_500yr_HZ_freeb[j]<-FIA_depth_to_damage$Nonres_restime[FIA_depth_to_damage$Depth %in% round(p$USACE_500yr_freeb[j])][1]*p$BLDG_Area[j]*nonres_rent + p$BLDG_Area[j]*nonres_disrup
        if (!is.na(p$USACE_500yr_freeb[j]) & p$USACE_500yr_freeb[j] > 10) { #control for wildly high depth grid values
          p$Reloc_500yr_HZ_freeb[j]<-max(FIA_depth_to_damage$Nonres_restime)*p$BLDG_Area[j]*nonres_rent + p$BLDG_Area[j]*nonres_disrup
        }
      }
    }
  }
  
  p$Reloc_500yr_SV_freeb<-NA
  for (j in 1: length(p$Reloc_500yr_SV_freeb)){
    p$Reloc_500yr_SV_freeb[j]<-FIA_depth_to_damage$Ave_reloc[FIA_depth_to_damage$Depth %in% round(p$USACE_500yr_freeb[j])][1]*p$BLDG_Area[j]*reloc_cpd
    if  (!is.na(p$USACE_500yr_freeb[j]) & p$USACE_500yr_freeb[j] > 10) {
      p$Reloc_500yr_SV_freeb[j]<-max(FIA_depth_to_damage$Ave_reloc)*p$BLDG_Area[j]*reloc_cpd
    }
  }
  
  p$Reloc_1000yr_HZ_freeb<-NA
  for (j in 1: length(p$Reloc_1000yr_HZ_freeb)) {
    if (!is.na(p$USACE_1000yr_freeb)[j]){ 
      if (p$LAND_USE[j] %in% residential){ #residential relocation costs
        p$Reloc_1000yr_HZ_freeb[j]<-FIA_depth_to_damage$Res_restime[FIA_depth_to_damage$Depth %in% round(p$USACE_1000yr_freeb[j])][1]*p$BLDG_Area[j]*res_rent + p$BLDG_Area[j]*res_disrup
        if (!is.na(p$USACE_1000yr_freeb[j]) & p$USACE_1000yr_freeb[j] > 10) { #control for wildly high depth grid values
          p$Reloc_1000yr_HZ_freeb[j]<-max(FIA_depth_to_damage$Res_restime)*p$BLDG_Area[j]*res_rent + p$BLDG_Area[j]*res_disrup
        }
      }else{ #nonres reloc costs
        p$Reloc_1000yr_HZ_freeb[j]<-FIA_depth_to_damage$Nonres_restime[FIA_depth_to_damage$Depth %in% round(p$USACE_1000yr_freeb[j])][1]*p$BLDG_Area[j]*nonres_rent + p$BLDG_Area[j]*nonres_disrup
        if (!is.na(p$USACE_1000yr_freeb[j]) & p$USACE_1000yr_freeb[j] > 10) { #control for wildly high depth grid values
          p$Reloc_1000yr_HZ_freeb[j]<-max(FIA_depth_to_damage$Nonres_restime)*p$BLDG_Area[j]*nonres_rent + p$BLDG_Area[j]*nonres_disrup
        }
      }
    }
  }
  
  p$Reloc_1000yr_SV_freeb<-NA
  for (j in 1: length(p$Reloc_1000yr_SV_freeb)){
    p$Reloc_1000yr_SV_freeb[j]<-FIA_depth_to_damage$Ave_reloc[FIA_depth_to_damage$Depth %in% round(p$USACE_1000yr_freeb[j])][1]*p$BLDG_Area[j]*reloc_cpd
    if  (!is.na(p$USACE_1000yr_freeb[j]) & p$USACE_1000yr_freeb[j] > 10) {
      p$Reloc_1000yr_SV_freeb[j]<-max(FIA_depth_to_damage$Ave_reloc)*p$BLDG_Area[j]*reloc_cpd
    }
  }
  
  #########################################################################################################################
  #Calculate labor costs using survey estimates for windshield and HAZUS depths, observed freeboard scenario
  ########################################################################################################################
  
  p$Labor_WD_SV<-NA #Calculate relocation costs
  for (j in 1: length(p$Labor_WD_SV)){
    if (!is.na(p$DLEVEL_depth)[j]){
      d<- p$DLEVEL_depth[j]
      p$Labor_WD_SV[j]<-FIA_depth_to_damage$Ave_labor_hrs[which(FIA_depth_to_damage$Depth == d)][1]*p$BLDG_Area[j]*labor_cph
    }
  }
  
  
  p$Labor_100_SV<-NA
  for (j in 1: length(p$Labor_100_SV)){
    p$Labor_100_SV[j]<-FIA_depth_to_damage$Ave_labor_hrs[FIA_depth_to_damage$Depth %in% round(p$USACE_100[j])][1]*p$BLDG_Area[j]*labor_cph
    if  (!is.na(p$USACE_100[j]) & p$USACE_100[j] > 10) {
      p$Labor_100_SV[j]<-max(FIA_depth_to_damage$Ave_labor_hrs)*p$BLDG_Area[j]*labor_cph
    }
  }
  
  
  p$Labor_500yr_SV<-NA
  for (j in 1: length(p$Labor_500yr_SV)){
    p$Labor_500yr_SV[j]<-FIA_depth_to_damage$Ave_labor_hrs[FIA_depth_to_damage$Depth %in% round(p$USACE_500yr[j])][1]*p$BLDG_Area[j]*labor_cph
    if  (!is.na(p$USACE_500yr[j]) & p$USACE_500yr[j] > 10) {
      p$Labor_500yr_SV[j]<-max(FIA_depth_to_damage$Ave_labor_hrs)*p$BLDG_Area[j]*labor_cph
    }
  }
  
  
  p$Labor_1000yr_SV<-NA
  for (j in 1: length(p$Labor_1000yr_SV)){
    p$Labor_1000yr_SV[j]<-FIA_depth_to_damage$Ave_labor_hrs[FIA_depth_to_damage$Depth %in% round(p$USACE_1000yr[j])][1]*p$BLDG_Area[j]*labor_cph
    if  (!is.na(p$USACE_1000yr[j]) & p$USACE_1000yr[j] > 10) {
      p$Labor_1000yr_SV[j]<-max(FIA_depth_to_damage$Ave_labor_hrs)*p$BLDG_Area[j]*labor_cph
    }
  }
  
  #########################################################################################################################
  #Calculate labor costs using  survey estimates for windshield and HAZUS depths, no freeboard scenario
  ########################################################################################################################
  
  p$Labor_WD_SV_freeb<-NA #Calculate relocation costs
  for (j in 1: length(p$Labor_WD_SV_freeb)){
    if (!is.na(p$DLEVEL_freeb)[j]){
      d<- p$DLEVEL_freeb[j]
      p$Labor_WD_SV_freeb[j]<-FIA_depth_to_damage$Ave_labor_hrs[which(FIA_depth_to_damage$Depth == d)][1]*p$BLDG_Area[j]*labor_cph
    }
  }
  
  
  p$Labor_100_SV_freeb<-NA
  for (j in 1: length(p$Labor_100_SV_freeb)){
    p$Labor_100_SV_freeb[j]<-FIA_depth_to_damage$Ave_labor_hrs[FIA_depth_to_damage$Depth %in% round(p$USACE_100_freeb[j])][1]*p$BLDG_Area[j]*labor_cph
    if  (!is.na(p$USACE_100_freeb[j]) & p$USACE_100_freeb[j] > 10) {
      p$Labor_100_SV_freeb[j]<-max(FIA_depth_to_damage$Ave_labor_hrs)*p$BLDG_Area[j]*labor_cph
    }
  }
  
  
  p$Labor_500yr_SV_freeb<-NA
  for (j in 1: length(p$Labor_500yr_SV_freeb)){
    p$Labor_500yr_SV_freeb[j]<-FIA_depth_to_damage$Ave_labor_hrs[FIA_depth_to_damage$Depth %in% round(p$USACE_500yr_freeb[j])][1]*p$BLDG_Area[j]*labor_cph
    if  (!is.na(p$USACE_500yr_freeb[j]) & p$USACE_500yr_freeb[j] > 10) {
      p$Labor_500yr_SV_freeb[j]<-max(FIA_depth_to_damage$Ave_labor_hrs)*p$BLDG_Area[j]*labor_cph
    }
  }
  
  
  p$Labor_1000yr_SV_freeb<-NA
  for (j in 1: length(p$Labor_1000yr_SV_freeb)){
    p$Labor_1000yr_SV_freeb[j]<-FIA_depth_to_damage$Ave_labor_hrs[FIA_depth_to_damage$Depth %in% round(p$USACE_1000yr_freeb[j])][1]*p$BLDG_Area[j]*labor_cph
    if  (!is.na(p$USACE_1000yr_freeb[j]) & p$USACE_1000yr_freeb[j] > 10) {
      p$Labor_1000yr_SV_freeb[j]<-max(FIA_depth_to_damage$Ave_labor_hrs)*p$BLDG_Area[j]*labor_cph
    }
  }
  saveRDS(p, paste0(wd,"/data/new/", "damagesplus", pbwsnames[i]) )
}

rm(FIA_depth_to_damage,annual_inflation_rate)
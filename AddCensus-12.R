#12

#This script joins ACS census data (census tract and block-group scale) with the created parcel files 
#and also adds zipcode scale FEMA data and parcel scale flood-insurance data.


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


#######################################################################################################################################################
#Intersect full data parcel file with HUC12, census tract, zipcode and block group boundaries
#######################################################################################################################################################
#ACS statistics for years 2010 (including 2006-2010 5yr estimates) and later are based on 2010 census boundaries, 2005-2009 ACS based on 2000 Census boundaries https://www.census.gov/programs-surveys/acs/geography-acs/geography-boundaries-by-year.html

zipcodes10<-shapefile(paste0(wd,"/data/","tl_2010_47_zcta510.shp")) 
zipcodes00<-shapefile(paste0(wd,"/data/","tl_2010_47_zcta500.shp")) 
tracts10<-shapefile(paste0(wd,"/data/","tl_2010_47037_tract10.shp")) 
tracts00<-shapefile(paste0(wd,"/data/","tl_2010_47037_tract00.shp")) 
bgs10<-shapefile(paste0(wd,"/data/","tl_2010_47037_bg10.shp")) 
bgs00<-shapefile(paste0(wd,"/data/","tl_2010_47037_bg00.shp")) 

zipcodes10<-st_as_sf(zipcodes10)
zipcodes00<-st_as_sf(zipcodes00)
tracts10<-st_as_sf(tracts10)
tracts00<-st_as_sf(tracts00)
bgs10<-st_as_sf(bgs10)
bgs00<-st_as_sf(bgs00)

zipcodes10<-st_transform(zipcodes10, projection)
zipcodes00<-st_transform(zipcodes00, projection)
tracts10<-st_transform(tracts10, projection)
tracts00<-st_transform(tracts00, projection)
bgs10<-st_transform(bgs10, projection)
bgs00<-st_transform(bgs00, projection)

zipcodes10<-zipcodes10[,c(2,3,12)]
zipcodes00<-zipcodes00[,c(2,3,12)]
tracts10<-tracts10[,c(3,4,13)]
tracts00<-tracts00[,c(3,4,13)]
bgs10<-bgs10[,c(5,13)]
bgs00<-bgs00[,c(5,13)]

colnames(zipcodes10)<-c("zipcode10","zgeoid10","geometry")
colnames(zipcodes00)<-c("zipcode00","zgeoid00","geometry")
colnames(tracts10)<-c("tractcode10","ctgeoid10","geometry")
colnames(tracts00)<-c("tractcode00","ctgeoid00","geometry")
colnames(bgs10)<-c("bggeoid10","geometry")
colnames(bgs00)<-c("bggeoid00","geometry")

pnames<-c("finalnewparcels2005ob.RDS", "finalnewparcels2006ob.RDS", "finalnewparcels2007ob.RDS", "finalnewparcels2008ob.RDS", "finalnewparcels2009ob.RDS", "finalnewparcels2010ob.RDS", "finalnewparcels2011ob.RDS",
          "finalnewparcels2012ob.RDS", "finalnewparcels2013ob.RDS","finalnewparcels2014ob.RDS","finalnewparcels2015ob.RDS", 
          "finalnewparcels2006nb.RDS", "finalnewparcels2007nb.RDS", "finalnewparcels2008nb.RDS", "finalnewparcels2009nb.RDS", "finalnewparcels2010nb.RDS", "finalnewparcels2011nb.RDS",
          "finalnewparcels2012nb.RDS", "finalnewparcels2013nb.RDS","finalnewparcels2014nb.RDS","finalnewparcels2015nb.RDS",
          "finalnewparcels2010ab.RDS", "finalnewparcels2011ab.RDS","finalnewparcels2012ab.RDS", "finalnewparcels2013ab.RDS","finalnewparcels2014ab.RDS","finalnewparcels2015ab.RDS",
          "finalnewparcels2010w.RDS", "finalnewparcels2011w.RDS","finalnewparcels2012w.RDS", "finalnewparcels2013w.RDS","finalnewparcels2014w.RDS","finalnewparcels2015w.RDS") 

year<-c(2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,
        2006,2007, 2008, 2009, 2010,2011,2012,2013,2014,2015,
        2010,2011,2012,2013,2014,2015,
        2010,2011,2012,2013,2014,2015)

n<-length(pnames)
registerDoParallel(n)
getDoParWorkers() 

foreach (i=1:n) %dopar% {
  
  p<-readRDS(paste0(wd,"/data/new/",pnames[i]))
  
  if (year[i] <= 2007){  #this is being overwritten in the second clause
    p2<-st_join(st_centroid(p),zipcodes00)
    p3<-st_join(p2,tracts00)
    p4<-st_join(p3,bgs00)
    st_geometry(p4)<-NULL
    p5<-left_join(p,p4[,c("PID","zipcode00","zgeoid00","tractcode00","ctgeoid00","bggeoid00")],by="PID")
    saveRDS(p5,paste0(wd,"/data/new/","ACS",pnames[i]))
  }
  if (year[i] > 2007){
    p2<-st_join(st_centroid(p),zipcodes10)
    p3<-st_join(p2,tracts10)
    p4<-st_join(p3,bgs10)
    st_geometry(p4)<-NULL
    p5<-left_join(p,p4[,c("PID","zipcode10","zgeoid10","tractcode10","ctgeoid10","bggeoid10")],by="PID")
    saveRDS(p5,paste0(wd,"/data/new/","ACS",pnames[i]))
  }
  
}

rm(bgs00,bgs10,tracts00,tracts10,zipcodes00,zipcodes10)

#####################################################
#Add parcel insurance data and FEMA zipcode data
####################################################

floodins<- shapefile(paste0(wd,"/data/","FloodInsuranceOverall.shp"))
floodins$Insur<-"Insured"
floodins<-floodins[ ,c(1,39)]
fi<-st_as_sf(floodins)
fi<-st_transform(fi, projection)


fema<-read.csv(paste0(wd,"/data/","FEMA owners.csv"), header = TRUE)
fema<-fema[!(is.na(fema$Disaster)),]
fema<-arrange(fema,desc(Total.Damage))
fema<-fema[duplicated(fema[,5])==FALSE, ] 
fema<-fema[,c(5,7:15)]
names(fema)<-c("ZIPCode","AveDamage","Inspect","TotDamage","NoDamage","LowDamage","MedLowD","MedHiD","HiD","FEMAApprov")
fema$ZIPCode<-as.character(fema$ZIPCode)

pnames<-c("finalnewparcels2005ob.RDS", "finalnewparcels2006ob.RDS", "finalnewparcels2007ob.RDS", "finalnewparcels2008ob.RDS", "finalnewparcels2009ob.RDS", "finalnewparcels2010ob.RDS", "finalnewparcels2011ob.RDS",
          "finalnewparcels2012ob.RDS", "finalnewparcels2013ob.RDS","finalnewparcels2014ob.RDS","finalnewparcels2015ob.RDS", 
          "finalnewparcels2006nb.RDS", "finalnewparcels2007nb.RDS", "finalnewparcels2008nb.RDS", "finalnewparcels2009nb.RDS", "finalnewparcels2010nb.RDS", "finalnewparcels2011nb.RDS",
          "finalnewparcels2012nb.RDS", "finalnewparcels2013nb.RDS","finalnewparcels2014nb.RDS","finalnewparcels2015nb.RDS",
          "finalnewparcels2010ab.RDS", "finalnewparcels2011ab.RDS","finalnewparcels2012ab.RDS", "finalnewparcels2013ab.RDS","finalnewparcels2014ab.RDS","finalnewparcels2015ab.RDS",
          "finalnewparcels2010w.RDS", "finalnewparcels2011w.RDS","finalnewparcels2012w.RDS", "finalnewparcels2013w.RDS","finalnewparcels2014w.RDS","finalnewparcels2015w.RDS") 

year<-c(2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,
        2006,2007, 2008, 2009, 2010,2011,2012,2013,2014,2015,
        2010,2011,2012,2013,2014,2015,
        2010,2011,2012,2013,2014,2015)

n<-length(pnames)
registerDoParallel(n)
getDoParWorkers() 

foreach (i=1:n) %dopar% {
  
  p<-readRDS(paste0(wd,"/data/new/","ACS",pnames[i]))
  st_geometry(fi)<-NULL
  p2<-left_join(p,fi, by="STANPAR")
  p3<-p2
  if (year[i] > 2007){ 
    p3<-left_join(p2,fema, by=c("zipcode10"= "ZIPCode"))
  }
  if (year[i] <= 2007){ 
    p3<-left_join(p2,fema, by=c("zipcode00"= "ZIPCode"))
  }
  
  
  saveRDS(p3,paste0(wd,"/data/new/","ACS",pnames[i]))
  
}


rm(fi,floodins,fema)

#p<-readRDS(paste0(wd,"/data/","ACS",pnames[1]))
##########################################################################################################
#Add Demographics
##########################################################################################################

#Demographics of interest include: Population, Black, Other Minority, Hispanic, Age, Seniors, Children, Gender, 
#Poverty, Income, English Speaking, Education, Employment, Occupation, Work transit, Foreign, Cost of Living, Home Ownership, Renters 
# ADD THESE LATER Seniors living alone, Group Housing 

#ACS 5 year estimates available for 2005-2009 to 2012-2016, associate with median years (2007-2014), drop 2005, 2006, and 2015
#Note that PUMA data has microdata for custom crosstabs and 1year estimates every year from 2000-2016 https://www.census.gov/programs-surveys/acs/data/pums.html
pnames<-c( "finalnewparcels2007ob.RDS", "finalnewparcels2008ob.RDS", "finalnewparcels2009ob.RDS", "finalnewparcels2010ob.RDS", "finalnewparcels2011ob.RDS",
           "finalnewparcels2012ob.RDS", "finalnewparcels2013ob.RDS","finalnewparcels2014ob.RDS", 
           "finalnewparcels2007nb.RDS", "finalnewparcels2008nb.RDS", "finalnewparcels2009nb.RDS", "finalnewparcels2010nb.RDS", "finalnewparcels2011nb.RDS",
           "finalnewparcels2012nb.RDS", "finalnewparcels2013nb.RDS","finalnewparcels2014nb.RDS",
           "finalnewparcels2010ab.RDS", "finalnewparcels2011ab.RDS","finalnewparcels2012ab.RDS", "finalnewparcels2013ab.RDS","finalnewparcels2014ab.RDS",
           "finalnewparcels2010w.RDS", "finalnewparcels2011w.RDS","finalnewparcels2012w.RDS", "finalnewparcels2013w.RDS","finalnewparcels2014w.RDS") 

year<-c(2007,2008,2009,2010,2011,2012,2013,2014,
        2007,2008,2009,2010,2011,2012,2013,2014,
        2010,2011,2012,2013,2014,
        2010,2011,2012,2013,2014)

#Citizenship, Race, Sex, Age, English, Education
ACS_S0601 <- c("ACS_09_5yr_S0601.csv","ACS_10_5yr_S0601.csv","ACS_11_5yr_S0601.csv","ACS_12_5yr_S0601.csv","ACS_13_5yr_S0601.csv",
               "ACS_14_5yr_S0601.csv","ACS_15_5yr_S0601.csv","ACS_16_5yr_S0601.csv",
               "ACS_09_5yr_S0601.csv","ACS_10_5yr_S0601.csv","ACS_11_5yr_S0601.csv","ACS_12_5yr_S0601.csv","ACS_13_5yr_S0601.csv",
               "ACS_14_5yr_S0601.csv","ACS_15_5yr_S0601.csv","ACS_16_5yr_S0601.csv",
               "ACS_12_5yr_S0601.csv","ACS_13_5yr_S0601.csv","ACS_14_5yr_S0601.csv","ACS_15_5yr_S0601.csv","ACS_16_5yr_S0601.csv",
               "ACS_12_5yr_S0601.csv","ACS_13_5yr_S0601.csv","ACS_14_5yr_S0601.csv","ACS_15_5yr_S0601.csv","ACS_16_5yr_S0601.csv")

#Owner Occupied Housing Vars
ACS_S2506 <- c("ACS_09_5yr_S2506.csv","ACS_10_5yr_S2506.csv","ACS_11_5yr_S2506.csv","ACS_12_5yr_S2506.csv","ACS_13_5yr_S2506.csv",
               "ACS_14_5yr_S2506.csv","ACS_15_5yr_S2506.csv","ACS_16_5yr_S2506.csv",
               "ACS_09_5yr_S2506.csv","ACS_10_5yr_S2506.csv","ACS_11_5yr_S2506.csv","ACS_12_5yr_S2506.csv","ACS_13_5yr_S2506.csv",
               "ACS_14_5yr_S2506.csv","ACS_15_5yr_S2506.csv","ACS_16_5yr_S2506.csv",
               "ACS_12_5yr_S2506.csv","ACS_13_5yr_S2506.csv","ACS_14_5yr_S2506.csv","ACS_15_5yr_S2506.csv","ACS_16_5yr_S2506.csv",
               "ACS_12_5yr_S2506.csv","ACS_13_5yr_S2506.csv","ACS_14_5yr_S2506.csv","ACS_15_5yr_S2506.csv","ACS_16_5yr_S2506.csv")

#Owner Occupied Housing (no Mortgage) Vars
ACS_S2507 <- c("ACS_09_5yr_S2507.csv","ACS_10_5yr_S2507.csv","ACS_11_5yr_S2507.csv","ACS_12_5yr_S2507.csv","ACS_13_5yr_S2507.csv",
               "ACS_14_5yr_S2507.csv","ACS_15_5yr_S2507.csv","ACS_16_5yr_S2507.csv",
               "ACS_09_5yr_S2507.csv","ACS_10_5yr_S2507.csv","ACS_11_5yr_S2507.csv","ACS_12_5yr_S2507.csv","ACS_13_5yr_S2507.csv",
               "ACS_14_5yr_S2507.csv","ACS_15_5yr_S2507.csv","ACS_16_5yr_S2507.csv",
               "ACS_12_5yr_S2507.csv","ACS_13_5yr_S2507.csv","ACS_14_5yr_S2507.csv","ACS_15_5yr_S2507.csv","ACS_16_5yr_S2507.csv",
               "ACS_12_5yr_S2507.csv","ACS_13_5yr_S2507.csv","ACS_14_5yr_S2507.csv","ACS_15_5yr_S2507.csv","ACS_16_5yr_S2507.csv")

#Renter Populations
ACS_B25008 <- c("ACS_09_5yr_B25008.csv","ACS_10_5yr_B25008.csv","ACS_11_5yr_B25008.csv","ACS_12_5yr_B25008.csv","ACS_13_5yr_B25008.csv",
                "ACS_14_5yr_B25008.csv","ACS_15_5yr_B25008.csv","ACS_16_5yr_B25008.csv",
                "ACS_09_5yr_B25008.csv","ACS_10_5yr_B25008.csv","ACS_11_5yr_B25008.csv","ACS_12_5yr_B25008.csv","ACS_13_5yr_B25008.csv",
                "ACS_14_5yr_B25008.csv","ACS_15_5yr_B25008.csv","ACS_16_5yr_B25008.csv",
                "ACS_12_5yr_B25008.csv","ACS_13_5yr_B25008.csv","ACS_14_5yr_B25008.csv","ACS_15_5yr_B25008.csv","ACS_16_5yr_B25008.csv",
                "ACS_12_5yr_B25008.csv","ACS_13_5yr_B25008.csv","ACS_14_5yr_B25008.csv","ACS_15_5yr_B25008.csv","ACS_16_5yr_B25008.csv")

#Occupations and Employment and Poverty and Transit to Work
ACS_DP03 <- c("ACS_09_5yr_DP03.csv","ACS_10_5yr_DP03.csv","ACS_11_5yr_DP03.csv","ACS_12_5yr_DP03.csv","ACS_13_5yr_DP03.csv",
              "ACS_14_5yr_DP03.csv","ACS_15_5yr_DP03.csv","ACS_16_5yr_DP03.csv",
              "ACS_09_5yr_DP03.csv","ACS_10_5yr_DP03.csv","ACS_11_5yr_DP03.csv","ACS_12_5yr_DP03.csv","ACS_13_5yr_DP03.csv",
              "ACS_14_5yr_DP03.csv","ACS_15_5yr_DP03.csv","ACS_16_5yr_DP03.csv",
              "ACS_12_5yr_DP03.csv","ACS_13_5yr_DP03.csv","ACS_14_5yr_DP03.csv","ACS_15_5yr_DP03.csv","ACS_16_5yr_DP03.csv",
              "ACS_12_5yr_DP03.csv","ACS_13_5yr_DP03.csv","ACS_14_5yr_DP03.csv","ACS_15_5yr_DP03.csv","ACS_16_5yr_DP03.csv")

#Block-group Populations
ACS_BG <- c("nhgis0003_ds195_20095_2009_blck_grp.csv","nhgis0003_ds176_20105_2010_blck_grp.csv","nhgis0003_ds184_20115_2011_blck_grp.csv","nhgis0003_ds191_20125_2012_blck_grp.csv",
            "nhgis0003_ds201_20135_2013_blck_grp.csv","nhgis0003_ds206_20145_2014_blck_grp.csv","nhgis0003_ds215_20155_2015_blck_grp.csv", "ACS_16_5yr_bg.csv",
            "nhgis0003_ds195_20095_2009_blck_grp.csv","nhgis0003_ds176_20105_2010_blck_grp.csv","nhgis0003_ds184_20115_2011_blck_grp.csv","nhgis0003_ds191_20125_2012_blck_grp.csv",
            "nhgis0003_ds201_20135_2013_blck_grp.csv","nhgis0003_ds206_20145_2014_blck_grp.csv","nhgis0003_ds215_20155_2015_blck_grp.csv", "ACS_16_5yr_bg.csv",
            "nhgis0003_ds191_20125_2012_blck_grp.csv","nhgis0003_ds201_20135_2013_blck_grp.csv","nhgis0003_ds206_20145_2014_blck_grp.csv","nhgis0003_ds215_20155_2015_blck_grp.csv", "ACS_16_5yr_bg.csv",
            "nhgis0003_ds191_20125_2012_blck_grp.csv","nhgis0003_ds201_20135_2013_blck_grp.csv","nhgis0003_ds206_20145_2014_blck_grp.csv","nhgis0003_ds215_20155_2015_blck_grp.csv", "ACS_16_5yr_bg.csv")

n<-length(pnames)
registerDoParallel(n)
getDoParWorkers() 

foreach (i=1:n) %dopar% {
  
  p<-readRDS(paste0(wd,"/data/new/","ACS",pnames[i]))
  #join with block group populations
  bg<-read.csv(paste0(wd, "/data/",ACS_BG[i]), header=TRUE, sep= ",")
  p2<-p
  if (year[i]==2007) {
    bg<-bg[bg$COUNTYA ==37,]
    bg$bggeoid00<-paste0(substr(bg$GISJOIN,2,4), substr(bg$GISJOIN,6,7), substr(bg$GISJOIN,9,15))
    bg<-bg[,c(35,37,38)]
  }
  if (year[i]==2008) {
    bg<-bg[bg$COUNTYA ==37,]
    bg$bggeoid00<-paste0(substr(bg$GISJOIN,2,4), substr(bg$GISJOIN,6,7), substr(bg$GISJOIN,9,15))
    bg<-bg[,c(37,39,40)] #year == 2008
  }
  if (year[i]>2008 & year[i] <2014) {
    bg<-bg[bg$COUNTYA ==37,]
    bg$bggeoid00<-paste0(substr(bg$GISJOIN,2,4), substr(bg$GISJOIN,6,7), substr(bg$GISJOIN,9,15))
    bg<-bg[,c(38,40,41)] #year == 2013, 2012, 2011, 2010, 2009
  }
  if (year[i]==2014){
    bg$bggeoid10<-paste0(substr(bg$Id, 10, 21))
    bg<-bg[,c(4,5,6)]
  }
  if (year[i]<=2007){
    colnames(bg)<-c("bg_pop_Est","bg_pop_ME","bggeoid00")
    bg$bggeoid00<-as.character(bg$bggeoid00)
    bg$bg_pop_Est<-as.numeric(bg$bg_pop_Est)
    p2<-left_join(p,bg,by="bggeoid00")
  }
  if (year[i]>2007){
    colnames(bg)<-c("bg_pop_Est","bg_pop_ME","bggeoid10")
    bg$bggeoid10<-as.character(bg$bggeoid10)
    p2<-left_join(p,bg,by="bggeoid10")
  }
  
  rm(bg)
  
  #join with census tract tables
  ct1<-read.csv(paste0(wd, "/data/",ACS_S0601[i]), header=TRUE, sep= ",")
  # ct1<-ct1[,c(2,4:5,10:13,60:61,68:69,76:77,84:85,92:93,108:109,116:117,164:165,172:173,
  #              188:189,204:205252:253,260:261,268:269,284:285,372:373,380:381,388:389)] #estimates and measurement error for all vars
  ct1<-ct1[,c(2,4:5,10,12,60,68,76,84,92,108,116,164,172,
              188,204,252,260,268,284,372,380,388)]
  p3<-p2
  
  if(year[i]<=2007){
    colnames(ct1)<-c("ctgeoid00","ct_pop_Est","ct_pop_ME","Foreign_born","Under_5","Age65_74","Over74","Med_Age",
                     "Male","Female","White_Only","Black_Only","Hisp_Lat","White_not_HL","Eng_SL","Eng_Poor",
                     "Over24","No_GED","GED","Bachelors","Med_Income","Pov_Pop","Below_Pov_Lvl")
    ct1$ctgeoid00<-as.character(ct1$ctgeoid00)
    p3<-left_join(p2,ct1,by=c("ctgeoid00"))
  }
  if (year[i]>2007){
    colnames(ct1)<-c("ctgeoid10","ct_pop_Est","ct_pop_ME","Foreign_born","Under_5","Age65_74","Over74","Med_Age",
                     "Male","Female","White_Only","Black_Only","Hisp_Lat","White_not_HL","Eng_SL","Eng_Poor",
                     "Over24","No_GED","GED","Bachelors","Med_Income","Pov_Pop","Below_Pov_Lvl")
    ct1$ctgeoid10<-as.character(ct1$ctgeoid10)
    p3<-left_join(p2,ct1,by=c("ctgeoid10"))
  }
  
  rm(ct1,p2)
  
  ct2<-read.csv(paste0(wd, "/data/",ACS_S2506[i]), header=TRUE, sep= ",")
  ct2<-ct2[,c(2,4,20,22,28,56,86,112)]
  p4<-p3
  
  if(year[i]<=2007){
    colnames(ct2)<-c("ctgeoid00","Own_Occ_HU","Med_Val_OOHU","OOHU_1Loan","OOHU_2Loans","OOHU_Mort_ValIncRatio4","OOHU_Mort_MedMonthly_HousCost",
                     "OOHU_Mort_CosttoInc50to75perc")
    ct2$ctgeoid00<-as.character(ct2$ctgeoid00)
    p4<-left_join(p3,ct2,by=c("ctgeoid00"))
  }
  if (year[i]>2007){
    colnames(ct2)<-c("ctgeoid10","Own_Occ_HU","Med_Val_OOHU","OOHU_1Loan","OOHU_2Loans","OOHU_Mort_ValIncRatio4","OOHU_Mort_MedMonthly_HousCost",
                     "OOHU_Mort_CosttoInc50to75perc")
    ct2$ctgeoid10<-as.character(ct2$ctgeoid10)
    p4<-left_join(p3,ct2,by=c("ctgeoid10"))
  }
  
  rm(ct2,p3)
  
  ct3<-read.csv(paste0(wd, "/data/",ACS_DP03[i]), header=TRUE, sep= ",")
  p5<-p4
  
  if(year[i]<=2007){
    ct3<-ct3[,c(2,4,12,20,22,72,84,100,110,114,118,126,130,252,270,294,302)]
    colnames(ct3)<-c("ctgeoid00","Pop_over16","CivLabor_Force","Unemployed","Perc_Unemployed","Commute",
                     "Public_Transit","Mean_Commute_Time","Perc_Management","Perc_Service","Perc_Office",
                     "Perc_Construction","Perc_Transport","Med_HH_Income","Perc_HH_withSS",
                     "Perc_HH_withPubAsst","Perc_HH_withSNAP")
    ct3$ctgeoid00<-as.character(ct3$ctgeoid00)
    p5<-left_join(p4,ct3,by=c("ctgeoid00"))
  }
  if (year[i]>2007){
    ct3<-ct3[,c(2,4,12,20,22,72,84,100,110,114,118,122,126,248,266,290,298)]
    colnames(ct3)<-c("ctgeoid10","Pop_over16","CivLabor_Force","Unemployed","Perc_Unemployed","Commute",
                     "Public_Transit","Mean_Commute_Time","Perc_Management","Perc_Service","Perc_Office",
                     "Perc_Construction","Perc_Transport","Med_HH_Income","Perc_HH_withSS",
                     "Perc_HH_withPubAsst","Perc_HH_withSNAP")
    ct3$ctgeoid10<-as.character(ct3$ctgeoid10)
    p5<-left_join(p4,ct3,by=c("ctgeoid10"))
  }
  
  rm(ct3,p4)
  
  ct4<-read.csv(paste0(wd, "/data/",ACS_B25008[i]), header=TRUE, sep= ",")
  p6<-p5
  
  if(year[i]<=2007){
    ct4<-ct4[,c(2,4,6,8)]
    colnames(ct4)<-c("ctgeoid00","PopinOccHU","PopinOwnOcc","PopinRentOcc")
    ct4$ctgeoid00<-as.character(ct4$ctgeoid00)
    p6<-left_join(p5,ct4,by=c("ctgeoid00"))
  }
  if (year[i]>2007){
    ct4<-ct4[,c(2,4,6,8)]
    colnames(ct4)<-c("ctgeoid10","PopinOccHU","PopinOwnOcc","PopinRentOcc")
    ct4$ctgeoid10<-as.character(ct4$ctgeoid10)
    p6<-left_join(p5,ct4,by=c("ctgeoid10"))
  }
  
  rm(ct4,p5)
  
  saveRDS(p6,paste0(wd,"/data/new/","Census",pnames[i]))
  
}

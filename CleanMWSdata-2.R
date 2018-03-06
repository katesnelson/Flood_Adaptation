#2

#This script reads in parcel data and building data, cleans the files and unifies columns across years, 
#and adds damage information from the Nashville Metro Water Services (MWS) windshield survey as well as buyout information provided by MWS

#THIS WOULD BE THE BEST PLACE TO ADD PID AND BLDGID THAT WORKS ACROSS YEARS



library(dplyr)
library(sp)
library(rgdal)
library(raster)
library(rgeos)



wd<- getwd()

##############################################
#Clean parcel data and unify for all years
##############################################

parcel2005<-readRDS(file="parcels2005.RDS")
parcel2006<-readRDS(file="parcels2006.RDS")
parcel2007<-readRDS(file="parcels2007.RDS")
parcel2008<-readRDS(file="parcels2008.RDS")
parcel2009<-readRDS(file="parcels2009.RDS")
parcel2010<-readRDS(file="parcels2010.RDS")
parcel2011<-readRDS(file="parcels2011.RDS")
parcel2012<-readRDS(file="parcels2012.RDS")
parcel2013<-readRDS(file="parcels2013.RDS")
parcel2014<-readRDS(file="parcels2014.RDS")
parcel2015<-readRDS(file="parcels2015.RDS")


names(parcel2005)
parcel2005<-parcel2005[ ,c(1,14,16,19,23,25:27,29:31,37,38,41,48,50)] #trim
parcel2005$LAND_USE_D<-NA #add empty field that is missing in this year
parcel2005<-parcel2005[ ,c(1,2,3,4,5,15,16,6,7,12,17,13,14,8,9,10,11)] #reorganize
names(parcel2005)
parcel2005$TOTAL_APPR<-as.integer(parcel2005$TOTAL_APPR) #fix data type
names(parcel2010) #use 2010 as our comparison group as it is the primary year of interest
parcel2010<-parcel2010[ ,c(1,7,8,10,15:17,21,26,29:35)] #trim
parcel2010$ASSD_DTE<-NA #add empty field that is missing in this year
parcel2010<-parcel2010[ ,c(1:13,17,14:16)] #reorganize
names(parcel2010)
parcel2010$ACREAGE_TE<-as.numeric(parcel2010$ACREAGE_TE)#fix data type
parcel2010$PROP_HOUSE<-as.numeric(parcel2010$PROP_HOUSE)#fix data type
names(parcel2005)<-names(parcel2010) #make column names the same

saveRDS(parcel2005,file="parcel2005.RDS")
saveRDS(parcel2010,file="parcel2010.RDS")
rm(parcel2005)

names(parcel2014)
parcel2014<-parcel2014[ ,c(2,8,9,11,16:18,22,27,30:33,40,34:36)] #trim
names(parcel2014)<-names(parcel2010) #consistent names
parcel2014$ACREAGE_TE<-as.numeric(parcel2014$ACREAGE_TE)#fix data type
parcel2014$PROP_HOUSE<-as.numeric(parcel2014$PROP_HOUSE)#fix data type
saveRDS(parcel2014,file="parcel2014.RDS")
rm(parcel2014)

names(parcel2006)
parcel2006<-parcel2006[ ,c(1,7,8,10,15:17,21,26,29:33)] #trim
parcel2006$LA_SQ_FT<-NA#add empty field that is missing in this year
parcel2006$DU_COUNT<-NA#add empty field that is missing in this year
parcel2006$ASSD_DTE<-NA #add empty field that is missing in this year
parcel2006<-parcel2006[ ,c(1:11,15:17,12:14)] #reorganize
names(parcel2006)<-names(parcel2010) #consistent names
parcel2006$PROP_HOUSE<-as.numeric(parcel2006$PROP_HOUSE)#fix data type
saveRDS(parcel2006,file="parcel2006.RDS")
rm(parcel2006)

names(parcel2007)
parcel2007<-parcel2007[ ,c(1,7,8,10,15:17,21,26,29:35)] #trim
parcel2007$ASSD_DTE<-NA #add empty field that is missing in this year
parcel2007<-parcel2007[ ,c(1:13,17,14:16)] #reorganize
names(parcel2007)
parcel2007$ACREAGE_TE<-as.numeric(parcel2007$ACREAGE_TE)#fix data type
parcel2007$PROP_HOUSE<-as.numeric(parcel2007$PROP_HOUSE)#fix data type
names(parcel2007)<-names(parcel2010) #make column names the same
saveRDS(parcel2007,file="parcel2007.RDS")
rm(parcel2007)

names(parcel2008)
parcel2008<-parcel2008[ ,c(1,7,8,10,15:17,21,26,29:35)] #trim
parcel2008$ASSD_DTE<-NA #add empty field that is missing in this year
parcel2008<-parcel2008[ ,c(1:13,17,14:16)] #reorganize
names(parcel2008)
parcel2008$ACREAGE_TE<-as.numeric(parcel2008$ACREAGE_TE)#fix data type
parcel2008$PROP_HOUSE<-as.numeric(parcel2008$PROP_HOUSE)#fix data type
names(parcel2008)<-names(parcel2010) #make column names the same
saveRDS(parcel2008,file="parcel2008.RDS")
rm(parcel2008)

names(parcel2009)
parcel2009<-parcel2009[ ,c(1,7,8,10,15:17,21,26,29:35)] #trim
parcel2009$ASSD_DTE<-NA #add empty field that is missing in this year
parcel2009<-parcel2009[ ,c(1:13,17,14:16)] #reorganize
names(parcel2009)
parcel2009$ACREAGE_TE<-as.numeric(parcel2009$ACREAGE_TE)#fix data type
parcel2009$PROP_HOUSE<-as.numeric(parcel2009$PROP_HOUSE)#fix data type
names(parcel2009)<-names(parcel2010) #make column names the same
saveRDS(parcel2009,file="parcel2009.RDS")
rm(parcel2009)

names(parcel2011)
parcel2011$ASSD_DTE<-NA #add empty field that is missing in this year
parcel2011<-parcel2011[ ,c(2,8,9,11,16:18,22,27,30:33,42,34:36)] #trim
names(parcel2011)
parcel2011$ACREAGE_TE<-as.numeric(parcel2011$ACREAGE_TE)#fix data type
parcel2011$PROP_HOUSE<-as.numeric(parcel2011$PROP_HOUSE)#fix data type
names(parcel2011)<-names(parcel2010) #make column names the same
saveRDS(parcel2011,file="parcel2011.RDS")
rm(parcel2011)

names(parcel2012)
parcel2012$ASSD_DTE<-NA #add empty field that is missing in this year
parcel2012<-parcel2012[ ,c(2,8,9,11,16:18,22,27,30:33,42,34:36)] #trim
names(parcel2012)
parcel2012$ACREAGE_TE<-as.numeric(parcel2012$ACREAGE_TE)#fix data type
parcel2012$PROP_HOUSE<-as.numeric(parcel2012$PROP_HOUSE)#fix data type
names(parcel2012)<-names(parcel2010) #make column names the same
saveRDS(parcel2012,file="parcel2012.RDS")
rm(parcel2012)

names(parcel2013)
parcel2013<-parcel2013[ ,c(2,8,9,11,16:18,22,27,30:33,40,34:36)] #trim
names(parcel2013)
parcel2013$ACREAGE_TE<-as.numeric(parcel2013$ACREAGE_TE)#fix data type
parcel2013$PROP_HOUSE<-as.numeric(parcel2013$PROP_HOUSE)#fix data type
names(parcel2013)<-names(parcel2010) #make column names the same
saveRDS(parcel2013,file="parcel2013.RDS")
rm(parcel2013)

names(parcel2015)
parcel2015<-parcel2015[ ,c(2,8,9,11,16:18,22,27,30:33,40,34:36)] #trim
names(parcel2015)
parcel2015$ACREAGE_TE<-as.numeric(parcel2015$ACREAGE_TE)#fix data type
parcel2015$PROP_HOUSE<-as.numeric(parcel2015$PROP_HOUSE)#fix data type
names(parcel2015)<-names(parcel2010) #make column names the same
saveRDS(parcel2015,file="parcel2015.RDS")
rm(parcel2015)

#################################################################################
#Add other pieces of data with parcel or address identifiers to parcel data sets
###################################################################################

#add buyout info
buyouts <-read.csv("MWS BuyoutList for R.csv", header=TRUE, sep =",", stringsAsFactors = FALSE) #read in the water rights sheet
buyouts$Parcel.ID<-as.character(buyouts$Parcel.ID) #convert parcel identifiers to character string
buyouts$Parcel.ID<-str_pad(buyouts$Parcel.ID, 11, pad = "0") #pad parcel ids with preceding zero when shorter than 11 digits long
buyouts<-buyouts[ ,-c(5,6)]
names(buyouts)<-c("PROP_ADDR","STANPAR","Met_Aq_D", "Aq_Cost", "Future")
buyouts$PROP_HOUSE<-str_extract(buyouts$PROP_ADDR, "[0-9]+") #extract the house number
buyouts$PROP_STREE<-str_extract(buyouts$PROP_ADDR, "[[aA-zZ] ]+") #extract the street names (doesn't work for streets that are a number)
buyouts$Aq_Cost<-as.numeric(buyouts$Aq_Cost) 
buyouts$PROP_HOUSE<-as.numeric(buyouts$PROP_HOUSE)
buyouts<-buyouts[duplicated(buyouts$STANPAR)==FALSE,]

parcel2005<-readRDS("parcel2005.RDS")
parcel2005<-merge(parcel2005, buyouts, by = c("STANPAR"), all.y = FALSE, sort=FALSE) #merge buyout info with parcel shapefile
parcel2005$buyout<-NA
parcel2005$buyout[!is.na(parcel2005$PROP_ADDR.y)]<-"YES"   #add a column for logical statement on buyout status
length(parcel2005@data$buyout[!is.na(parcel2005@data$buyout)]) #check to make sure all buyouts added
saveRDS(parcel2005,file="parcel2005.RDS")
rm(parcel2005)

parcel2006<-readRDS("parcel2006.RDS")
parcel2006<-merge(parcel2006, buyouts, by = c("STANPAR"), all.y = FALSE, sort=FALSE)
parcel2006$buyout<-NA
parcel2006$buyout[!is.na(parcel2006$PROP_ADDR.y)]<-"YES"
length(parcel2006@data$buyout[!is.na(parcel2006@data$buyout)])
saveRDS(parcel2006,file="parcel2006.RDS")
rm(parcel2006)

parcel2007<-readRDS("parcel2007.RDS")
parcel2007<-merge(parcel2007, buyouts, by = c("STANPAR"), all.y = FALSE, sort=FALSE)
parcel2007$buyout<-NA
parcel2007$buyout[!is.na(parcel2007$PROP_ADDR.y)]<-"YES"
length(parcel2007@data$buyout[!is.na(parcel2007@data$buyout)])
saveRDS(parcel2007,file="parcel2007.RDS")
rm(parcel2007)

parcel2008<-readRDS("parcel2008.RDS")
parcel2008<-merge(parcel2008, buyouts, by = c("STANPAR"), all.y = FALSE, sort=FALSE)
parcel2008$buyout<-NA
parcel2008$buyout[!is.na(parcel2008$PROP_ADDR.y)]<-"YES"
length(parcel2008@data$buyout[!is.na(parcel2008@data$buyout)])
saveRDS(parcel2008,file="parcel2008.RDS")
rm(parcel2008)

parcel2009<-readRDS("parcel2009.RDS")
parcel2009<-merge(parcel2009, buyouts, by = c("STANPAR"), all.y = FALSE, sort=FALSE)
parcel2009$buyout<-NA
parcel2009$buyout[!is.na(parcel2009$PROP_ADDR.y)]<-"YES"
length(parcel2009@data$buyout[!is.na(parcel2009@data$buyout)])
saveRDS(parcel2009,file="parcel2009.RDS")
rm(parcel2009)

parcel2010<-readRDS("parcel2010.RDS")
parcel2010<-merge(parcel2010, buyouts, by = c("STANPAR"), all.y = FALSE, sort=FALSE)
parcel2010$buyout<-NA
parcel2010$buyout[!is.na(parcel2010$PROP_ADDR.y)]<-"YES"
length(parcel2010@data$buyout[!is.na(parcel2010@data$buyout)])
saveRDS(parcel2010,file="parcel2010.RDS")
rm(parcel2010)

parcel2011<-readRDS("parcel2011.RDS")
parcel2011<-merge(parcel2011, buyouts, by = c("STANPAR"), all.y = FALSE, sort=FALSE)
parcel2011$buyout<-NA
parcel2011$buyout[!is.na(parcel2011$PROP_ADDR.y)]<-"YES"
length(parcel2011@data$buyout[!is.na(parcel2011@data$buyout)])
saveRDS(parcel2011,file="parcel2011.RDS")
rm(parcel2011)

parcel2012<-readRDS("parcel2012.RDS")
parcel2012<-merge(parcel2012, buyouts, by = c("STANPAR"), all.y = FALSE, sort=FALSE)
parcel2012$buyout<-NA
parcel2012$buyout[!is.na(parcel2012$PROP_ADDR.y)]<-"YES"
length(parcel2012@data$buyout[!is.na(parcel2012@data$buyout)])
saveRDS(parcel2012,file="parcel2012.RDS")
rm(parcel2012)

parcel2013<-readRDS("parcel2013.RDS")
parcel2013<-merge(parcel2013, buyouts, by = c("STANPAR"), all.y = FALSE, sort=FALSE)
parcel2013$buyout<-NA
parcel2013$buyout[!is.na(parcel2013$PROP_ADDR.y)]<-"YES"
length(parcel2013@data$buyout[!is.na(parcel2013@data$buyout)])
saveRDS(parcel2013,file="parcel2013.RDS")
rm(parcel2013)

parcel2014<-readRDS("parcel2014.RDS")
parcel2014<-merge(parcel2014, buyouts, by = c("STANPAR"), all.y = FALSE, sort=FALSE)
parcel2014$buyout<-NA
parcel2014$buyout[!is.na(parcel2014$PROP_ADDR.y)]<-"YES"
length(parcel2014@data$buyout[!is.na(parcel2014@data$buyout)])
saveRDS(parcel2014,file="parcel2014.RDS")
rm(parcel2014)

parcel2015<-readRDS("parcel2015.RDS")
parcel2015<-merge(parcel2015, buyouts, by = c("STANPAR"), all.y = FALSE, sort=FALSE)
parcel2015$buyout<-NA
parcel2015$buyout[!is.na(parcel2015$PROP_ADDR.y)]<-"YES"
length(parcel2015@data$buyout[!is.na(parcel2015@data$buyout)])
saveRDS(parcel2015,file="parcel2015.RDS")
rm(parcel2015)

#add damage data from windshield data
damage<-shapefile('damagepts111710.shp')
damage<-damage@data[ ,c(1,35:37)]
length(unique(damage$STANPAR))
damage<-damage[duplicated(damage$STANPAR)==FALSE,]

parcel2005<-readRDS("parcel2005.RDS")
parcel2005<-merge(parcel2005, damage, by = c("STANPAR"), all.y = FALSE, sort=FALSE) #merge damage info with parcel shapefile
saveRDS(parcel2005,file="parcel2005.RDS")
rm(parcel2005)

parcel2006<-readRDS("parcel2006.RDS")
parcel2006<-merge(parcel2006, damage, by = c("STANPAR"), all.y = FALSE, sort=FALSE) 
saveRDS(parcel2006,file="parcel2006.RDS")
rm(parcel2006)

parcel2007<-readRDS("parcel2007.RDS")
parcel2007<-merge(parcel2007, damage, by = c("STANPAR"), all.y = FALSE, sort=FALSE) 
saveRDS(parcel2007,file="parcel2007.RDS")
rm(parcel2007)

parcel2008<-readRDS("parcel2008.RDS")
parcel2008<-merge(parcel2008, damage, by = c("STANPAR"), all.y = FALSE, sort=FALSE) 
saveRDS(parcel2008,file="parcel2008.RDS")
rm(parcel2008)

parcel2009<-readRDS("parcel2009.RDS")
parcel2009<-merge(parcel2009, damage, by = c("STANPAR"), all.y = FALSE, sort=FALSE) 
saveRDS(parcel2009,file="parcel2009.RDS")
rm(parcel2009)

parcel2010<-readRDS("parcel2010.RDS")
parcel2010<-merge(parcel2010, damage, by = c("STANPAR"), all.y = FALSE, sort=FALSE) 
saveRDS(parcel2010,file="parcel2010.RDS")
rm(parcel2010)

parcel2011<-readRDS("parcel2011.RDS")
parcel2011<-merge(parcel2011, damage, by = c("STANPAR"), all.y = FALSE, sort=FALSE) 
saveRDS(parcel2011,file="parcel2011.RDS")
rm(parcel2011)

parcel2012<-readRDS("parcel2012.RDS")
parcel2012<-merge(parcel2012, damage, by = c("STANPAR"), all.y = FALSE, sort=FALSE) 
saveRDS(parcel2012,file="parcel2012.RDS")
rm(parcel2012)

parcel2013<-readRDS("parcel2013.RDS")
parcel2013<-merge(parcel2013, damage, by = c("STANPAR"), all.y = FALSE, sort=FALSE) 
saveRDS(parcel2013,file="parcel2013.RDS")
rm(parcel2013)

parcel2014<-readRDS("parcel2014.RDS")
parcel2014<-merge(parcel2014, damage, by = c("STANPAR"), all.y = FALSE, sort=FALSE) 
saveRDS(parcel2014,file="parcel2014.RDS")
rm(parcel2014)

parcel2015<-readRDS("parcel2015.RDS")
parcel2015<-merge(parcel2015, damage, by = c("STANPAR"), all.y = FALSE, sort=FALSE) 
saveRDS(parcel2015,file="parcel2015.RDS")
rm(parcel2015)

############################################
#Clean bldg data and unify for all years
##############################################

bldg2005<-readRDS(file="bld2005.RDS")
bldg2014<-readRDS(file="bld2014.RDS")
bldg2015<-readRDS(file="bld2015.RDS")


bldg2005$BLDG_Area<-gArea(bldg2005, byid=TRUE)
bldg2005$BLDG_ID<-seq(1,length(bldg2005[,1]))
names(bldg2005)
bldg2005<-bldg2005[ ,c(14,13)] #trim

saveRDS(bldg2005,file="bldg2005.RDS")
rm(bldg2005)

bldg2014$BLDG_Area<-gArea(bldg2014, byid=TRUE)
bldg2014$BLDG_ID<-seq(1,length(bldg2014[,1]))
names(bldg2014)
bldg2014<-bldg2014[ ,c(12,16)] #trim

saveRDS(bldg2014,file="bldg2014.RDS")
rm(bldg2014)

bldg2015$BLDG_Area<-gArea(bldg2015, byid=TRUE)
bldg2015$BLDG_ID<-seq(1,length(bldg2015[,1]))
names(bldg2015)
bldg2015<-bldg2015[ ,c(9,13)] #trim

saveRDS(bldg2015,file="bldg2015.RDS")
rm(bldg2015)
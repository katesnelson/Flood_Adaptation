#1

#This script simply reads in the raw datafiles obtained from Nashville Metro Water Services. 
#Tax parcel shapefiles are available for purchase at https://www.nashville.gov/Planning-Department/Mapping-and-GIS/PropertyMapping.aspx



library(sp)
library(rgdal)
library(raster)
library(rgeos)


wd<- getwd()


###############################################################
#Read in Davidson County parcel datasets set projection and save as RDS
##############################################################
projection<- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" #set proj info to work with

parcel_2001<-shapefile("metro/2001/property.shp")
parcel_2001<-spTransform(parcel_2001, CRS(projection)) #set to common proj
proj4string(parcel_2001)==projection #check proj
saveRDS(parcel_2001,file="parcels2001.RDS")

parcel_2002<-shapefile("metro/2002/property2002.shp")
parcel_2002<-spTransform(parcel_2002, CRS(projection)) #set to common proj
proj4string(parcel_2002)==projection #check proj
saveRDS(parcel_2002,file="parcels2002.RDS")

parcel_2003<-shapefile("metro/2003/Prop_with_LIS_12_30_03.shp")
parcel_2003<-spTransform(parcel_2003, CRS(projection)) #set to common proj
proj4string(parcel_2003)==projection #check proj
saveRDS(parcel_2003,file="parcels2003.RDS")

parcel_2004<-shapefile("metro/2004/Prop_with_LIS_9_1_04.shp")
parcel_2004<-spTransform(parcel_2004, CRS(projection)) #set to common proj
proj4string(parcel_2004)==projection #check proj
saveRDS(parcel_2004,file="parcels2004.RDS")

parcel_2005<-shapefile("metro/2005/Prop_with_LIS_12-29-05.shp")
parcel_2005<-spTransform(parcel_2005, CRS(projection)) #set to common proj
proj4string(parcel_2005)==projection #check proj
saveRDS(parcel_2005,file="parcels2005.RDS")

parcel_2006<-shapefile("metro/2006/prop2006.shp")
parcel_2006<-spTransform(parcel_2006, CRS(projection)) #set to common proj
proj4string(parcel_2006)==projection #check proj
saveRDS(parcel_2006,file="parcels2006.RDS")

parcel_2007<-shapefile("metro/2007/property_2007.shp")
parcel_2007<-spTransform(parcel_2007, CRS(projection)) #set to common proj
proj4string(parcel_2007)==projection #check proj
saveRDS(parcel_2007,file="parcels2007.RDS")

parcel_2008<-shapefile("metro/2008/property_yearend08.shp")
parcel_2008<-spTransform(parcel_2008, CRS(projection)) #set to common proj
proj4string(parcel_2008)==projection #check proj
saveRDS(parcel_2008,file="parcels2008.RDS")

parcel_2009<-shapefile("metro/2009/Property_YrEnd09.shp")
parcel_2009<-spTransform(parcel_2009, CRS(projection)) #set to common proj
proj4string(parcel_2009)==projection #check proj
saveRDS(parcel_2009,file="parcels2009.RDS")

parcel_2010<-shapefile("metro/2010/Property_YrEnd10.shp")
parcel_2010<-spTransform(parcel_2010, CRS(projection)) #set to common proj
proj4string(parcel_2010)==projection #check proj
saveRDS(parcel_2010,file="parcels2010.RDS")

parcel_2011<-shapefile("metro/2011/Property_YrEnd11.shp")
parcel_2011<-spTransform(parcel_2011, CRS(projection)) #set to common proj
proj4string(parcel_2011)==projection #check proj
saveRDS(parcel_2011,file="parcels2011.RDS")

parcel_2012<-shapefile("metro/2012/Property_YrEnd12.shp")
parcel_2012<-spTransform(parcel_2012, CRS(projection)) #set to common proj
proj4string(parcel_2012)==projection #check proj
saveRDS(parcel_2012,file="parcels2012.RDS")

parcel_2013<-shapefile("metro/2013/Property_YrEnd13.shp")
parcel_2013<-spTransform(parcel_2013, CRS(projection)) #set to common proj
proj4string(parcel_2013)==projection #check proj
saveRDS(parcel_2013,file="parcels2013.RDS")

parcel_2014<-shapefile("metro/2014/Property_YrEnd14.shp")
parcel_2014<-spTransform(parcel_2014, CRS(projection)) #set to common proj
proj4string(parcel_2014)==projection #check proj
saveRDS(parcel_2014,file="parcels2014.RDS")

parcel_2015<-shapefile("metro/2015/Property_YrEnd15.shp")
parcel_2015<-spTransform(parcel_2015, CRS(projection)) #set to common proj
proj4string(parcel_2015)==projection #check proj
saveRDS(parcel_2015,file="parcels2015.RDS")

parcel_2016<-shapefile("metro/2016/Property_YrEnd16.shp")
parcel_2016<-spTransform(parcel_2016, CRS(projection)) #set to common proj
proj4string(parcel_2016)==projection #check proj
saveRDS(parcel_2016,file="parcels2016.RDS")


############################################################################
#Read in Davidson County building datasets set projection and save as RDS
#######################################################################

bld2005<-shapefile('metro/2005/building2005.shp') #load building footprints
bld2005<-spTransform(bld2005, CRS(projection))
saveRDS(bld2005,file="bld2005.RDS")
rm(bld2005)

bld2014<-shapefile('metro/2014/Building_YrEnd14.shp') #load building footprints
bld2014<-spTransform(bld2014, CRS(projection))
saveRDS(bld2014,file="bld2014.RDS")
rm(bld2014)

bld2015<-shapefile('metro/2015/Building_YrEnd15.shp') #load building footprints
bld2015<-spTransform(bld2015, CRS(projection))
saveRDS(bld2015,file="bld2015.RDS")
rm(bld2015)

bld2016<-shapefile('metro/2016/Building_YrEnd16.shp') #load building footprints
bld2016<-spTransform(bld2016, CRS(projection))
saveRDS(bld2016,file="bld2016.RDS")
rm(bld2016)


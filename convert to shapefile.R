library(sp)
library(raster)
library(sf)

wd<-("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/")

file<-readRDS(paste0(wd,"builtApbwsGS2010ob.RDS"))

st_write(file,"builtA2010ob.shp", driver="ESRI Shapefile")

file<-readRDS(paste0(wd,"RApbwsGS2010ob.RDS"))

st_write(file,dsn=paste0(wd,"riparea2010ob.shp"), driver="ESRI Shapefile")

file<-readRDS(paste0(wd,"RApbwsGS2010ab.RDS"))

st_write(file,dsn=paste0(wd,"riparea2010ab.shp"), driver="ESRI Shapefile")

file<-readRDS(paste0(wd,"RApbwsGS2010nb.RDS"))

st_write(file,dsn=paste0(wd,"riparea2010nb.shp"), driver="ESRI Shapefile")

file<-readRDS(paste0(wd,"RApbwsGS2010w.RDS"))

st_write(file,dsn=paste0(wd,"riparea2010w.shp"), driver="ESRI Shapefile")
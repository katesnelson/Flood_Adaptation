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

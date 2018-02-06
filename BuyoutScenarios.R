#6

#This script creates building shapefiles for each year of three alternate buyout program adoption scenarios: 
#no buyouts, all buyouts, and buyouts + wishlist

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

##################################################################################################
#Create buyout scenarios
###############################################################################################

#add and remove buildings based on buyout program adoption scenarios 
#Buyout program adoption scenarios include: 2010 observed (files as is), 2010 no buyouts completed, 2010 all buyouts completed, 2010 all buyouts plus all wishlist completed

#Define a function that creates simulated scenario shapefiles
build_scenarios = function(projection, bldgfiles, savefiles_no_bo, savefiles_all_bo, savefiles_wish){
  
  n<-length(bldgfiles)
  registerDoParallel(n)
  getDoParWorkers() 
  
  foreach (i=1:n) %dopar% {
    test<-readRDS(paste0(wd,"/data/", bldgfiles[i]))
    test$Future<-as.character(test$Future)
    test$DistStr<-as.numeric(test$DistStr)
    
    ab<-filter(test, Future=="TRUE" | is.na(Future))  #remove all completed buyout buildings to create all buyout completed scenario
    ab<-st_as_sf(ab)
    saveRDS(ab, paste0(wd,"/data/", "new", savefiles_all_bo[i]))
    
    wish<-test[is.na(test$Future),] #remove all completed buyout buildings and buildings on wishlist to create all buyout plus wishlist scenario
    saveRDS(wish, paste0(wd,"/data/", "new", savefiles_wish[i]))
  }   
  
  n<-length(bldgfiles_nb)
  registerDoParallel(n)
  getDoParWorkers() 
  
  foreach (i=1:n) %dopar% {
    test<-readRDS(paste0(wd,"/data/", bldgfiles_nb[i]))
    test$Future<-as.character(test$Future)
    test$DistStr<-as.numeric(test$DistStr)
    
    pre<-readRDS(paste0(wd,"/data/", "streampbws2005b.RDS")) #pull in the earliest pre-2010 bldg records
    pre$DistStr<-as.numeric(pre$DistStr)
    pre<-st_transform(pre, projection)
    pre$PROP_ZIP<-as.character(pre$PROP_ZIP)
    pre$LAND_USE<-as.character(pre$LAND_USE)
    pre$Met_Aq_D<-as.Date(pre$Met_Aq_D, format = "%m/%d/%Y")
    pre$Future<-as.character(pre$Future)
    nb<-test[is.na(test$Future),] #remove all buyout bldgs
    nb$Met_Aq_D<-as.Date(nb$Met_Aq_D, format = "%m/%d/%Y")
    if (length(nb) > length(pre)){
      nb<-dplyr::select(nb, -PID.y)
    }
    pre_sub<-filter(pre, buyout=="YES") #create subset of all buyout bldgs in 2005
    pre_sub<-st_as_sf(pre_sub)
    colnames(pre_sub)<-colnames(nb)
    nb_2<-rbind(nb, pre_sub) #merge the current years bldgs minus all buyouts with all buyout designated bldgs present in 2005
    saveRDS(nb_2, paste0(wd, "/data/", "new", savefiles_no_bo[1]))
  }
}

bldgfiles<-c("streampbws2010b.RDS", "streampbws2011b.RDS","streampbws2012b.RDS", "streampbws2013b.RDS","streampbws2014b.RDS","streampbws2015b.RDS")
bldgfiles_nb<-c("streampbws2006b.RDS","streampbws2007b.RDS","streampbws2008b.RDS","streampbws2009b.RDS","streampbws2010b.RDS", "streampbws2011b.RDS","streampbws2012b.RDS", "streampbws2013b.RDS","streampbws2014b.RDS","streampbws2015b.RDS")

savefiles_no_bo<-c("pbws2006b_nb.RDS","pbws2007b_nb.RDS","pbws2008b_nb.RDS","pbws2009b_nb.RDS", "pbws2010b_nb.RDS", "pbws2011b_nb.RDS","pbws2012b_nb.RDS", "pbws2013b_nb.RDS","pbws2014b_nb.RDS","pbws2015b_nb.RDS")
savefiles_all_bo<-c("pbws2010b_ab.RDS", "pbws2011b_ab.RDS","pbws2012b_ab.RDS", "pbws2013b_ab.RDS","pbws2014b_ab.RDS","pbws2015b_ab.RDS")
savefiles_wish<-c("pbws2010b_w.RDS", "pbws2011b_w.RDS","pbws2012b_w.RDS", "pbws2013b_w.RDS","pbws2014b_w.RDS","pbws2015b_w.RDS")


build_scenarios(projection, bldgfiles, bldgfiles_nb, savefiles_no_bo, savefiles_all_bo, savefiles_wish)


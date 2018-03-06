#13

#This script dasymetrically assigns census tract populations to tax parcels using a version of the Cadastral-based Expert 
#Dasymetric System (CEDS) developed by Maantay, Maroko, and Herrmann (2013)(http://www.tandfonline.com/doi/abs/10.1559/152304007781002190). 
#Senior citizen populations and renter populations are further selectively redistributed to parcels according to the methods described 
#by Nelson, Camp, and Abkowitz (2015) (https://www.sciencedirect.com/science/article/pii/S0143622815001551).
#This work extends these methods by converting the continuous population distributions created into probabilistic,
#whole-number count (Poisson) distributions that retain the pycnophylactic property (sum of population redistributed from
#census tracts to parcels within that tract equals the original population of the tract).


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
#Full population dasymetry --- Use Census Tract data for dasym for consistency as most tables of demographics used are for census tracts 
#######################################################################################################################################################

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

n<-length(pnames)
registerDoParallel(n)
getDoParWorkers() 

foreach (i=1:n) %dopar% {
  
  dat<-readRDS(paste0(wd,"/data/new/", "Census", pnames[i]))
  
  #add back in broken land-use-description
  parcelfiles <- c("parcel2007.RDS", "parcel2008.RDS", "parcel2009.RDS", "parcel2010.RDS", "parcel2011.RDS",
                   "parcel2012.RDS", "parcel2013.RDS","parcel2014.RDS",
                   "parcel2007.RDS", "parcel2008.RDS", "parcel2009.RDS", "parcel2010.RDS", "parcel2011.RDS",
                   "parcel2012.RDS", "parcel2013.RDS","parcel2014.RDS",
                   "parcel2010.RDS", "parcel2011.RDS","parcel2012.RDS", "parcel2013.RDS","parcel2014.RDS",
                   "parcel2010.RDS", "parcel2011.RDS","parcel2012.RDS", "parcel2013.RDS","parcel2014.RDS")
  
  
  p<-readRDS(paste0(wd,"/data/",parcelfiles[9]))
  p<-p@data
  p<-p[,c(1,10,11)]
  dat<-dat[,!names(dat) %in% c("LAND_USE","LAND_USE_D")]
  
  ds<-left_join(dat, p, by="STANPAR")
  
  #remove non residential properties from columns used to distribute population
  residential<-c(11,12,13, 14, 15, 16, 17, 18, 19, 37,38,39,62, 81, 82, 83, 84, 88, 89, 92, 95, 97) #residential land use codes
  res_bldg<-c( "SINGLE FAMILY", "DUPLEX","RESIDENTIAL COMBO/MISC","MOBILE HOME","CONDO",
               "RURAL COMBO", "MODULAR HOME", "APARTMENT: LOW RISE (BUILT SINCE 1960)","DORMITORY/BOARDING HOUSE",
               "APARTMENT: WALK UP (BUILT PRIOR TO 1960)\r\nA", "TRIPLEX", "RESIDENTIAL CONDO", "NURSING HOME",
               "PARSONAGE","APARTMENT: HIGH RISE (3 STORIES OR GREATER)", "RES COMBO/MISC","QUADRAPLEX", "RES CONDOMINIUM",
               "MOBILE HOME PARK","APARTMENT: WALK UP (BUILT PRIOR TO 1960)\r\n\nA","APARTMENT WALK UP","APARTMENT HIGH-RISE",
               "NURSING HOME/SANITARIUM","ORPHANAGE/CHARITABLE SERV", "MOBILE HOME(S)", "RESIDENTIAL COMBO. OR MISC.",
               "MOBILE HOMES(S) - RURAL","SINGLE FAMILY DWELLING","RESIDENTIAL CONDOMINIUM" ,"SFD(S) - RURAL" ,"APARTMENT LOW-RISE",
               "TRIPLEX(S) - RURAL","DUPLEX(S) - RURAL"  )
  
  single_res<-c("SINGLE FAMILY", "SINGLE FAMILY DWELLING","SFD(S) - RURAL","RESIDENTIAL CONDO",
                "RES CONDOMINIUM","PARSONAGE", "MOBILE HOME(S)","RESIDENTIAL CONDOMINIUM","MODULAR HOME","MOBILE HOME")
  
  ds$DU<-ds$totDU
  ds$SQFT<-ds$totLA
  
  
  ds$LAND_USE<-as.numeric(ds$LAND_USE)
  ds$DU[!(ds$LAND_USE %in% residential)| !(ds$LAND_USE_D %in% res_bldg)]<-NA
  ds$SQFT[!(ds$LAND_USE  %in% residential)| !(ds$LAND_USE_D %in% res_bldg)]<-NA
  ds$DU[ds$DU==0]<-NA
  ds$SQFT[ds$SQFT==0]<-NA
  
  #Fix missing data in mobile homes categories (use totDU?)
  MH<-c("MOBILE HOME(S)", "MOBILE HOME", "MOBILE HOME PARK","MOBILE HOMES(S) - RURAL")
  
  for (j in 1:length(ds$STANPAR)){
    if (ds$LAND_USE_D[j] %in% MH){
      if (!is.na(ds$totBLDGIA[j]) & ds$totBLDGIA[j] > 0 & is.na(ds$totLA[j]) & is.na(ds$totDU[j])){
        ds$DU[j]<-round(ds$totBLDGIA[j]/1000) #if mobile home status and there is a bldg, but no recorded DU or LASQFT, use ratio of Bldg area, assume ave area of MH of 1,000 sqft
      }
      if (!is.na(ds$totBLDGIA[j]) & ds$totBLDGIA[j] > 0 & !is.na(ds$totLA[j]) & ds$totLA[j] > 0 & is.na(ds$totDU[j])){
        ds$DU[j]<-round((ds$totLA[j])/1000) #if mobile home status and there is a bldg and LASQFT, but no recorded DU, use ratio of LASQFT, assume ave area of MH of 1,000 sqft 
      }
      if (!is.na(ds$totBLDGIA[j]) & ds$totBLDGIA[j] > 0 & is.na(ds$totLA[j]) & is.na(ds$totDU[j])){
        ds$SQFT[j]<-round(ds$totBLDGIA[j]) #if mobile home status and there is a bldg, but no recorded DU or LASQFT, use Bldg area for living area
      }
      if (!is.na(ds$totBLDGIA[j]) & ds$totBLDGIA[j] > 0 & !is.na(ds$totDU[j]) & ds$totDU[j] > 0 & is.na(ds$totLA[j])){
        ds$SQFT[j]<-ds$totDU[j]*1000#if mobile home status and there is a bldg and DU, but no recorded LASQFT, use DU * 1,000 sqft for living area
        
      }
    }
  }
  
  #Fix missing data for other residential property categories
  for (j in 1:length(ds$STANPAR)){
    if (ds$LAND_USE[j] %in% residential){
      if(!is.na(ds$totBLDGIA[j]) & ds$totBLDGIA[j] > 0 & is.na(ds$totLA[j]) & is.na(ds$totDU[j])){
        ds$SQFT[j]<-round(ds$totBLDGIA[j]) #if residential but no DU or totLA, use bldg area for living area
      } } else {
        if(ds$LAND_USE_D[j] %in% res_bldg){
          if (!is.na(ds$totBLDGIA[j]) & ds$totBLDGIA[j] > 0 & is.na(ds$totLA[j]) & is.na(ds$totDU[j])){
            ds$SQFT[j]<-round(ds$totBLDGIA[j]) #if land_use not classified as res but with residential buildings on property, and no DU or LASQFT, use bldg area for living area
          }
        }
      }
  }
  
  
  
  ds$DU[is.na(ds$DU) &  !is.na(ds$totBLDGIA) & ds$totBLDGIA > 0 & ds$LAND_USE_D %in% single_res]<-1
  ds$DU[is.na(ds$DU) &  !is.na(ds$totBLDGIA) & ds$totBLDGIA > 0 & ds$LAND_USE_D %in% "DUPLEX"]<-2
  ds$DU[is.na(ds$DU) &  !is.na(ds$totBLDGIA) & ds$totBLDGIA > 0 & ds$LAND_USE_D %in% "TRIPLEX"]<-3
  ds$DU[is.na(ds$DU) &  !is.na(ds$totBLDGIA) & ds$totBLDGIA > 0 & ds$LAND_USE_D %in% "QUADRAPLEX"]<-4
  ds$DU[is.na(ds$DU) &  !is.na(ds$totBLDGIA) & ds$totBLDGIA > 0 & ds$LAND_USE_D %in% "DUPLEX(S) - RURAL"]<-2
  ds$DU[is.na(ds$DU) &  !is.na(ds$totBLDGIA) & ds$totBLDGIA > 0 & ds$LAND_USE_D %in% "TRIPLEX(S) - RURAL"]<-3
  
  #Dwelling unit and Living Area Census Unit Sums
  if (year[i]==2007){
    d1<- ds %>% group_by (bggeoid00) %>% summarise (DUBG = sum(DU, na.rm=TRUE), SQFTBG = sum(SQFT, na.rm=TRUE))
    d2<- ds %>% group_by (ctgeoid00) %>% summarise(DUCT = sum(DU, na.rm=T), SQFTCT = sum(SQFT, na.rm=T))
    ds<-left_join(ds,d1,by="bggeoid00")
    ds<-left_join(ds,d2,by="ctgeoid00")
  }
  if (year[i] > 2007){
    d1<-ds %>% group_by (bggeoid10) %>% summarise(DUBG = sum(DU, na.rm=T), SQFTBG = sum(SQFT, na.rm=T))
    d2<-ds %>% group_by (ctgeoid10) %>% summarise(DUCT = sum(DU, na.rm=T), SQFTCT = sum(SQFT, na.rm=T))
    ds<-left_join(ds,d1,by="bggeoid10")
    ds<-left_join(ds,d2,by="ctgeoid10")
  }
  #Full Population Dasym
  ds$CT_Pop_DU <- ds$DU*ds$ct_pop_Est /ds$DUCT  #dasym pop using census tract dsa is the number of DU at that record times the ratio of the tract pop and the total number of DU in the tract
  ds$CT_Pop_LA <- ds$SQFT*ds$ct_pop_Est/ds$SQFTCT
  # ds$BG_Pop_DU <- ds$DU*ds$B01001e1/SUMBGDU
  # ds$BG_Pop_LA <- ds$SQFT*ds$B01001e1/SUMBGSQFT
  
  #Choose the dasym option (DU or LASQFT) that reduces error between aggregated census tract dasym pop and actual block group pop
  # ds<-group_by (Tract) %>% summarise(SUMDUBG_POP = sum(ds$BG_Pop_DU), SUMSQFTBG_POP = sum(ds$BG_Pop_LA))
  if (year[i] == 2007){
    d1<-ds %>% group_by (bggeoid00) %>% summarise(SUMDUCT_POP = sum(CT_Pop_DU, na.rm=T), SUMSQFTCT_POP = sum(CT_Pop_LA, na.rm=T))
    ds<-left_join(ds,d1,by="bggeoid00")
  }
  if (year[i] > 2007){
    d1<-ds %>% group_by (bggeoid10) %>% summarise(SUMDUCT_POP = sum(CT_Pop_DU, na.rm=T), SUMSQFTCT_POP = sum(CT_Pop_LA, na.rm=T))
    ds<-left_join(ds,d1,by="bggeoid10")
  }
  
  # ds$LAPOP_Diff<-Abs(ds$DP0010001 - SUMSQFTBG_POP) #compare the census tract population with the aggregated dasymetric pop using block group dsa
  # ds$DUPOP_Diff<-abs(ds$DP0010001 - SUMDUBG_POP)
  ds$LAPOP_Diff<-abs(ds$bg_pop_Est - ds$SUMSQFTCT_POP) #compare the block group population with the aggregated dasymetric pop using census tract dsa
  ds$DUPOP_Diff<-abs(ds$bg_pop_Est - ds$SUMDUCT_POP)
  
  ds$dasym_pop<-NA
  # ds$dasym_pop[ds$DUPOP_Diff < ds$LAPOP_Diff]<-ds$BG_POP_DU #if diff for DU-based dasym pop is smaller than LASQFT-based dasym pop use DU-based dasym pop
  # ds$dasym_pop[ds$DUPOP_Diff >= ds$LAPOP_Diff]<-ds$BG_POP_LA
  
  for (j in 1:length(ds$dasym_pop)){
    if (!(is.na(ds$DUPOP_Diff[j])) & !(is.na(ds$LAPOP_Diff[j]))){
      if (ds$DUPOP_Diff[j] < ds$LAPOP_Diff[j]){
        ds$dasym_pop[j]<-ds$CT_Pop_DU[j]  #if diff for DU-based dasym pop is smaller than LASQFT-based dasym pop use DU-based dasym pop
      }
    }
  }
  
  for (j in 1:length(ds$dasym_pop)){
    if (!(is.na(ds$DUPOP_Diff[j])) & !(is.na(ds$LAPOP_Diff[j]))){
      if (ds$DUPOP_Diff[j] >= ds$LAPOP_Diff[j]){
        ds$dasym_pop[j]<-ds$CT_Pop_LA[j]
      }
    }
  }
  
  saveRDS(ds,paste0(wd,"/data/new/","dasym",pnames[i]))
}


#Check on erros in dasymetry, are they bigger or smaller than Census ME? Seems to be mostly smaller than measurement errors.
# dat$ME_Dasym_error_DU<-NA
# dat$ME_Dasym_error_DU[dat$bg_pop_ME < dat$DUPOP_Diff]<-"Greater"
# dat$ME_Dasym_error_DU[dat$bg_pop_ME > dat$DUPOP_Diff]<-"Less"
# dat$ME_Dasym_error_LA<-NA
# dat$ME_Dasym_error_LA[dat$bg_pop_ME < dat$LAPOP_Diff]<-"Greater"
# dat$ME_Dasym_error_LA[dat$bg_pop_ME > dat$LAPOP_Diff]<-"Less"
# dat$ME_ERROR_COMP<-NA
# dat$ME_ERROR_COMP[dat$ME_Dasym_error_DU =="Less" | dat$ME_Dasym_error_LA =="Less"]<-1

###################################################################################################
#To Create a Probablistic Poisson Population Distribution
###################################################################################################

pnames<-c( "finalnewparcels2007ob.RDS", "finalnewparcels2008ob.RDS", "finalnewparcels2009ob.RDS", "finalnewparcels2010ob.RDS", "finalnewparcels2011ob.RDS",
           "finalnewparcels2012ob.RDS", "finalnewparcels2013ob.RDS","finalnewparcels2014ob.RDS", 
           "finalnewparcels2007nb.RDS", "finalnewparcels2008nb.RDS", "finalnewparcels2009nb.RDS", "finalnewparcels2010nb.RDS", "finalnewparcels2011nb.RDS",
           "finalnewparcels2012nb.RDS", "finalnewparcels2013nb.RDS","finalnewparcels2014nb.RDS",
           "finalnewparcels2010ab.RDS", "finalnewparcels2011ab.RDS","finalnewparcels2012ab.RDS", "finalnewparcels2013ab.RDS","finalnewparcels2014ab.RDS",
           "finalnewparcels2010w.RDS", "finalnewparcels2011w.RDS","finalnewparcels2012w.RDS", "finalnewparcels2013w.RDS","finalnewparcels2014w.RDS") 

n<-length(pnames)
registerDoParallel(n)
getDoParWorkers() 

foreach (i=1:n) %dopar% {
  
  p <- readRDS(paste0(wd,"/data/new/","dasym",pnames[i]))
  
  #Create a rounded-up whole person population
  p$dasym_pop[is.infinite(p$dasym_pop)]<-NA
  p$P.round <- ceiling(p$dasym_pop)  #round up to nearest whole number so that we don't have parts of people and so that each non-vacant residence has a least one occupant
  
  # P <- p$dasym_pop #proportional population distribution
  # P[is.infinite(P)]<-NA
  # P.round <- ceiling(P)  
  
  #reconcile actual total population with rounded-up population
  TotP.actual <- sum(p$dasym_pop, na.rm=T)
  TotP.round <-sum(p$P.round, na.rm=T)
  deltaP <- TotP.round - round(TotP.actual)
  
  #reconcile actual total population with rounded-up population
  # TotP.actual <- sum(P, na.rm=T)
  # TotP.round <-sum(P.round, na.rm=T)
  # deltaP <- TotP.round - round(TotP.actual)
  
  #remove excess population using combination of random draws and probability
  rangenorm <- function(x) { #range standardize function
    return ((x - (min(x, na.rm=T)-0.00000001)) / (max(x,na.rm=T) - min(x,na.rm=T)))
  }
  
  p$P.prob <- rangenorm(p$dasym_pop) #scale assigned populations to between 0 and 1
  
  # assumumption that higher assigned population will have 
  # proportionally higher probability of having more actual people
  p$P.new <- p$P.round #new vector to reassign populations to
  
  #create a dataframe
  #d<-data.frame(P,P.round,P.prob,P.new)
  p$loc.id <- seq(1:length(p$P.round)) 
  
  #while there is a difference between assigned and actual whole person populations remove one person
  loc.ids <- p$loc.id[!is.na(p$dasym_pop) & p$dasym_pop > 1]
  loc.id2<-loc.ids
  
  
  prob <- p$P.prob[!is.na(p$dasym_pop) & p$dasym_pop > 1]
  j <- sample(loc.ids,deltaP,prob = prob) #sample from the set of locations with more than one person using the prob vector to select as many records as needed to reduce deltaP to zero
  
  p$P.new[p$loc.id %in% j] <- p$P.round[p$loc.id %in% j] - 1   #remove one person
  TotP.new <- sum(p$P.new, na.rm=T)
  deltaP <- TotP.new - round(TotP.actual) #recalculate the difference between the acutal population and predicited popualtion
  
  # hist(p$P.new,breaks=c(0,1,2,3,4,5,8,10,15,25,20000), xlim=c(0,15)) 
  # hist(p$dasym_pop,breaks=c(0,1,2,3,4,5,8,10,15,25,20000), xlim=c(0,15)) 
  saveRDS(p, paste0(wd, "/data/new/","dasym",pnames[i]))
}


#######################################################################################################
#Sub-population dasymetry - SENIORS
#########################################################################################################
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

n<-length(pnames)
registerDoParallel(n)
getDoParWorkers() 

foreach (i=1:n) %dopar% {
  
  dat<-readRDS(paste0(wd,"/data/new/","dasym",pnames[i]))
  
  
  #Selective distribution of seniors 
  
  notsenior<-c("DORMITORY/BOARDING HOUSE","SCHOOL OR COLLEGE")
  senior_res<-c("NURSING HOME", "ELDERLY HOUSING", "NURSING HOME/SANITARIUM" )
  
  dat$SeniorPop<-as.numeric(as.character(dat$Over74)) + as.numeric(as.character(dat$Age65_74))
  
  #Total dasym population at classified senior residences by block group
  if (year[i] == 2007){
    d1<-dat %>% filter(LAND_USE_D %in% senior_res ) %>% group_by(ctgeoid00) %>% summarise(Totpop_senior_res= sum(dasym_pop, na.rm=T))
    dat<-left_join(dat,d1, by=("ctgeoid00"))
  }
  if (year[i] > 2007){
    d1<-dat %>% filter(LAND_USE_D %in% senior_res ) %>% group_by(ctgeoid10) %>% summarise(Totpop_senior_res= sum(dasym_pop, na.rm=T))
    dat<-left_join(dat,d1, by=("ctgeoid10"))
  }
  
  #Distribute Senior Population to Assigned Senior Residence Parcels 
  #dat$dasym_senior[dat$LAND_USE_D %in% notsenior & !(dat$LAND_USE_D %in% "ELDERLY HOUSING") ]<-0 #NO ELDERLY HOUSING CATEGORY IN TAX PARCEL DATA -> Was this in bld description supplement?
  d1<-dat %>% filter (LAND_USE_D %in% senior_res & !is.na(SeniorPop) & (SeniorPop >= Totpop_senior_res)) %>% group_by (STANPAR) %>% summarise (dasym_senior=dasym_pop)
  dat<-left_join(dat,d1,by="STANPAR")
  d1<-dat %>% filter (LAND_USE_D %in% senior_res & !is.na(SeniorPop) & (SeniorPop < Totpop_senior_res)) %>% group_by (STANPAR) %>% summarise (dasym_senior = dasym_pop * (SeniorPop/Totpop_senior_res))
  dat<-left_join(dat,d1, by="STANPAR")
  
  dat$dasym_senior.x[is.na(dat$dasym_senior.x)]<-0
  dat$dasym_senior.y[is.na(dat$dasym_senior.y)]<-0
  dat$dasym_senior<-dat$dasym_senior.x + dat$dasym_senior.y
  dat<- dat[,!names(dat) %in% c("dasym_senior.x", "dasym_senior.y")]  
  
  #DistributeSenior Populstion to Parcels that are not classified as Assigned or Excluded 
  if (year[i] == 2007){
    d1<-dat %>% group_by(ctgeoid00) %>% summarise (TotSenPop_SenRes = sum(dasym_senior, na.rm=T), TotPop_NotSenRes = sum(dasym_pop[LAND_USE_D %in% notsenior], na.rm=T), 
                                                   TotPop_SenRes = sum(dasym_pop[LAND_USE_D %in% senior_res], na.rm=T), TotDasymPop = sum(dasym_pop, na.rm=T))
    dat<-left_join(dat,d1,by=("ctgeoid00"))
  }
  if (year[i] >2007){
    d1<-dat %>% group_by(ctgeoid10) %>% summarise (TotSenPop_SenRes = sum(dasym_senior, na.rm=T), TotPop_NotSenRes = sum(dasym_pop[LAND_USE_D %in% notsenior], na.rm=T), 
                                                   TotPop_SenRes = sum(dasym_pop[LAND_USE_D %in% senior_res], na.rm=T), TotDasymPop = sum(dasym_pop, na.rm=T))
    dat<-left_join(dat,d1,by=("ctgeoid10"))
  }
  
  dat$PID<-rownames(dat)
  d1<- dat %>% filter (!(LAND_USE_D %in% senior_res)) %>%  filter (!(LAND_USE_D %in% notsenior)) %>% group_by (PID) %>% 
    summarise (dasym_senior.x = (dasym_pop *(SeniorPop - TotSenPop_SenRes)/(ct_pop_Est - TotPop_NotSenRes - TotPop_SenRes)))
  dat<-left_join(dat,d1,by=c("PID"))
  dat$dasym_senior.x<-round(dat$dasym_senior.x,2)
  
  dat$dasym_senior.x[is.na(dat$dasym_senior.x)]<-0
  dat$dasym_senior<-dat$dasym_senior.x + dat$dasym_senior
  dat$dasym_senior[dat$dasym_senior==0]<-NA
  dat<- dat[,!names(dat) %in% c("dasym_senior.x")]  
  
  #Assign Senior Population of 0 to Excluded Parcels
  dat$dasym_senior[dat$LAND_USE_D %in% notsenior]<-0
  
  saveRDS(dat,paste0(wd,"/data/new/","dasym2",pnames[i]))
  
  
}

###########################################################################
#And now convert the senior pop to whole number, count-based  pop 
#########################################################################
pnames<-c( "finalnewparcels2007ob.RDS", "finalnewparcels2008ob.RDS", "finalnewparcels2009ob.RDS", "finalnewparcels2010ob.RDS", "finalnewparcels2011ob.RDS",
           "finalnewparcels2012ob.RDS", "finalnewparcels2013ob.RDS","finalnewparcels2014ob.RDS", 
           "finalnewparcels2007nb.RDS", "finalnewparcels2008nb.RDS", "finalnewparcels2009nb.RDS", "finalnewparcels2010nb.RDS", "finalnewparcels2011nb.RDS",
           "finalnewparcels2012nb.RDS", "finalnewparcels2013nb.RDS","finalnewparcels2014nb.RDS",
           "finalnewparcels2010ab.RDS", "finalnewparcels2011ab.RDS","finalnewparcels2012ab.RDS", "finalnewparcels2013ab.RDS","finalnewparcels2014ab.RDS",
           "finalnewparcels2010w.RDS", "finalnewparcels2011w.RDS","finalnewparcels2012w.RDS", "finalnewparcels2013w.RDS","finalnewparcels2014w.RDS") 

n<-length(pnames)
registerDoParallel(n)
getDoParWorkers() 

foreach (i=1:n) %dopar% {
  
  p <- readRDS(paste0(wd,"/data/new/","dasym2",pnames[i]))
  
  #Create a rounded-up whole person population
  p$dasym_senior[is.infinite(p$dasym_senior)]<-NA
  p$P.round <- floor(p$dasym_senior)  #round up to nearest whole number so that we don't have parts of people and so that each non-vacant residence has a least one occupant
  
  # P <- p$dasym_pop #proportional population distribution
  # P[is.infinite(P)]<-NA
  # P.round <- ceiling(P)  
  
  #reconcile actual total population with rounded-up population
  TotP.actual <- sum(p$dasym_senior, na.rm=T)
  TotP.round <-sum(p$P.round, na.rm=T)
  deltaP <- abs(TotP.round - round(TotP.actual))
  
  #reconcile actual total population with rounded-up population
  # TotP.actual <- sum(P, na.rm=T)
  # TotP.round <-sum(P.round, na.rm=T)
  # deltaP <- TotP.round - round(TotP.actual)
  
  #remove excess population using combination of random draws and probability
  rangenorm <- function(x) { #range standardize function
    return ((x - (min(x, na.rm=T)-0.00000000000001)) / (max(x,na.rm=T) - min(x,na.rm=T)))
  }
  
  p$P.prob <- rangenorm(p$dasym_senior) #scale assigned populations to between 0 and 1
  p$P.prob<-1-p$P.prob #inverse of prob, so we want to selectively select those with an expected low pop
  
  # assumumption that higher assigned population will have 
  # proportionally higher probability of having more actual people
  p$P.newsenior <- p$P.round #new vector to reassign populations to
  
  
  #create a dataframe
  #d<-data.frame(P,P.round,P.prob,P.new)
  #p$loc.id <- seq(1:length(p$P.round)) 
  
  #while there is a difference between assigned and actual whole person populations add one person
  loc.ids <- p$loc.id[!is.na(p$dasym_senior) & p$dasym_senior < 1]
  loc.id2<-loc.ids
  
  
  prob <- p$P.prob[!is.na(p$dasym_senior)  & p$dasym_senior < 1]
  j <- sample(loc.ids,deltaP,prob = prob) #sample from the set of locations with less than one person using the prob vector to select as many records as needed to reduce deltaP to zero
  
  p$P.newsenior[p$loc.id %in% j] <- p$P.round[p$loc.id %in% j] + 1   #add one person
  TotP.new<- sum(p$P.newsenior, na.rm=T)
  deltaP <- TotP.new - round(TotP.actual) #recalculate the difference between the acutal population and predicited popualtion
  
  # hist(p$P.newsenior,breaks=c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,8,10,15,25,35), xlim=c(0,20), ylim=c(0,200), freq=TRUE) 
  # hist(p$dasym_senior,breaks=c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,8,10,15,25,35), xlim=c(0,20), ylim=c(0,200), freq=TRUE) 
  saveRDS(p, paste0(wd, "/data/new/","dasym2",pnames[i]))
}

######################################
#Selective distribution of renters
#######################################

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

n<-length(pnames)
registerDoParallel(n)
getDoParWorkers() 

foreach (i=1:n) %dopar% {
  
  dat<-readRDS(paste0(wd,"/data/new/","dasym2",pnames[i]))
  
  
  #Selective distribution of seniors 
  
  rent<-c(12,13,14,17,37,38,39, 82, 83, 84, 95) #rental and temporary housing land use codes
  rent_bldg<-c(  "DUPLEX", "APARTMENT: LOW RISE (BUILT SINCE 1960)","DORMITORY/BOARDING HOUSE",
                 "APARTMENT: WALK UP (BUILT PRIOR TO 1960)\r\nA", "TRIPLEX",  "NURSING HOME",
                 "APARTMENT: HIGH RISE (3 STORIES OR GREATER)", "QUADRAPLEX", "APARTMENT: WALK UP (BUILT PRIOR TO 1960)\r\n\nA",
                 "APARTMENT WALK UP","APARTMENT HIGH-RISE", "NURSING HOME/SANITARIUM","APARTMENT LOW-RISE",
                 "TRIPLEX(S) - RURAL","DUPLEX(S) - RURAL"  )
  
  dat$RentPop<-as.numeric(as.character(dat$PopinRentOcc))
  
  #Total dasym population at classified rental residences by block group
  if (year[i] == 2007){
    d1<-dat %>% filter(LAND_USE_D %in% rent_bldg | LAND_USE %in% rent) %>% group_by(ctgeoid00) %>% summarise(Totpop_rent_res= sum(dasym_pop, na.rm=T))
    dat<-left_join(dat,d1, by=("ctgeoid00"))
  }
  if (year[i] > 2007){
    d1<-dat %>% filter(LAND_USE_D %in% rent_bldg | LAND_USE %in% rent) %>% group_by(ctgeoid10) %>% summarise(Totpop_rent_res= sum(dasym_pop, na.rm=T))
    dat<-left_join(dat,d1, by=("ctgeoid10"))
  }
  
  #Distribute Renter Population to Assigned Renter Residence Parcels 
  
  d1<-dat %>% filter ((LAND_USE_D %in% rent_bldg | LAND_USE %in% rent) & !is.na(RentPop) & (RentPop >= Totpop_rent_res)) %>% group_by (PID) %>% summarise (dasym_rent=dasym_pop)
  dat<-left_join(dat,d1,by="PID")
  d1<-dat %>% filter ((LAND_USE_D %in% rent_bldg | LAND_USE %in% rent) & !is.na(RentPop) & (RentPop < Totpop_rent_res)) %>% group_by (PID) %>% summarise (dasym_rent = dasym_pop * (RentPop/Totpop_rent_res))
  dat<-left_join(dat,d1, by="PID")
  
  dat$dasym_rent.x[is.na(dat$dasym_rent.x)]<-0
  dat$dasym_rent.y[is.na(dat$dasym_rent.y)]<-0
  dat$dasym_rent<-dat$dasym_rent.x + dat$dasym_rent.y
  dat<- dat[,!names(dat) %in% c("dasym_rent.x", "dasym_rent.y")]  
  
  #DistributeSenior Populstion to Parcels that are not classified as Assigned or Excluded 
  if (year[i] == 2007){
    d1<-dat %>% group_by(ctgeoid00) %>% summarise (TotRentPop_RentRes = sum(dasym_rent, na.rm=T),  TotDasymPop = sum(dasym_pop, na.rm=T))
    dat<-left_join(dat,d1,by=("ctgeoid00"))
  }
  if (year[i] >2007){
    d1<-dat %>% group_by(ctgeoid10) %>% summarise (TotRentPop_RentRes = sum(dasym_rent, na.rm=T),TotDasymPop = sum(dasym_pop, na.rm=T))
    dat<-left_join(dat,d1,by=("ctgeoid10"))
  }
  
  dat$PID<-rownames(dat)
  d1<- dat %>% filter (!(LAND_USE_D %in% rent_bldg) | ! (LAND_USE %in% rent))  %>% group_by (PID) %>% 
    summarise (dasym_rent.x = (dasym_pop *(RentPop - TotRentPop_RentRes)/(ct_pop_Est -  Totpop_rent_res)))
  dat<-left_join(dat,d1,by=c("PID"))
  dat$dasym_rent.x<-round(dat$dasym_rent.x,2)
  
  dat$dasym_rent.x[is.na(dat$dasym_rent.x)]<-0
  dat$dasym_rent<-dat$dasym_rent.x + dat$dasym_rent
  dat$dasym_rent[dat$dasym_rent==0]<-NA
  dat<- dat[,!names(dat) %in% c("dasym_rent.x")]  
  
  
  saveRDS(dat,paste0(wd,"/data/new/","dasym3",pnames[i]))
  
  
}

###########################################################################
#And now convert the renter pop to whole number, count-based  pop 
#########################################################################
pnames<-c( "finalnewparcels2007ob.RDS", "finalnewparcels2008ob.RDS", "finalnewparcels2009ob.RDS", "finalnewparcels2010ob.RDS", "finalnewparcels2011ob.RDS",
           "finalnewparcels2012ob.RDS", "finalnewparcels2013ob.RDS","finalnewparcels2014ob.RDS", 
           "finalnewparcels2007nb.RDS", "finalnewparcels2008nb.RDS", "finalnewparcels2009nb.RDS", "finalnewparcels2010nb.RDS", "finalnewparcels2011nb.RDS",
           "finalnewparcels2012nb.RDS", "finalnewparcels2013nb.RDS","finalnewparcels2014nb.RDS",
           "finalnewparcels2010ab.RDS", "finalnewparcels2011ab.RDS","finalnewparcels2012ab.RDS", "finalnewparcels2013ab.RDS","finalnewparcels2014ab.RDS",
           "finalnewparcels2010w.RDS", "finalnewparcels2011w.RDS","finalnewparcels2012w.RDS", "finalnewparcels2013w.RDS","finalnewparcels2014w.RDS") 

n<-length(pnames)
registerDoParallel(n)
getDoParWorkers() 

foreach (i=1:n) %dopar% {
  
  p <- readRDS(paste0(wd,"/data/new/","dasym3",pnames[i]))
  
  #Create a rounded-up whole person population
  p$dasym_rent[is.infinite(p$dasym_rent)]<-NA
  p$P.round <- floor(p$dasym_rent)  #round up to nearest whole number so that we don't have parts of people and so that each non-vacant residence has a least one occupant
  
  # P <- p$dasym_pop #proportional population distribution
  # P[is.infinite(P)]<-NA
  # P.round <- ceiling(P)  
  
  #reconcile actual total population with rounded-up population
  TotP.actual <- sum(p$dasym_rent, na.rm=T)
  TotP.round <-sum(p$P.round, na.rm=T)
  deltaP <- abs(TotP.round - round(TotP.actual))
  
  #reconcile actual total population with rounded-up population
  # TotP.actual <- sum(P, na.rm=T)
  # TotP.round <-sum(P.round, na.rm=T)
  # deltaP <- TotP.round - round(TotP.actual)
  
  #remove excess population using combination of random draws and probability
  rangenorm <- function(x) { #range standardize function
    return ((x - (min(x, na.rm=T)-0.00000000000001)) / (max(x,na.rm=T) - min(x,na.rm=T)))
  }
  
  p$P.prob <- rangenorm(p$dasym_rent) #scale assigned populations to between 0 and 1
  p$P.prob<-1-p$P.prob #inverse of prob, so we want to selectively select those with an expected low pop
  
  # assumumption that higher assigned population will have 
  # proportionally higher probability of having more actual people
  p$P.newrent <- p$P.round #new vector to reassign populations to
  
  
  #create a dataframe
  #d<-data.frame(P,P.round,P.prob,P.new)
  #p$loc.id <- seq(1:length(p$P.round)) 
  
  #while there is a difference between assigned and actual whole person populations add one person
  loc.ids <- p$loc.id[!is.na(p$dasym_rent) & p$dasym_rent < 1]
  loc.id2<-loc.ids
  
  
  prob <- p$P.prob[!is.na(p$dasym_rent)  & p$dasym_rent < 1]
  j <- sample(loc.ids,deltaP,prob = prob) #sample from the set of locations with less than one person using the prob vector to select as many records as needed to reduce deltaP to zero
  
  p$P.newrent[p$loc.id %in% j] <- p$P.round[p$loc.id %in% j] + 1   #add one person
  TotP.new<- sum(p$P.newrent, na.rm=T)
  deltaP <- TotP.new - round(TotP.actual) #recalculate the difference between the acutal population and predicited popualtion
  
  # hist(p$P.newrent,breaks=c(0,5,10,15,25,35,50,200,2000) ,xlim=c(0,300),ylim=c(0,300), freq=TRUE) 
  # hist(p$dasym_rent,breaks=c(0,5,10,15,25,35,50,200,2000) ,xlim=c(0,300),ylim=c(0,300), freq=TRUE) 
  saveRDS(p, paste0(wd, "/data/new/","dasym3",pnames[i]))
}

###########################################
#Selective distribution of children and other pops--> Not doing these for now, old pyhton code below
######################################

def comparison( BLDG_TYPE, LAND_USE_D, dasympop_tract_BG, child_pop_ratio ):
  if  BLDG_TYPE == 'NURSING HOME' :
  return 0 
elif BLDG_TYPE == 'ELDERLY HOUSING':
  return 0
elif BLDG_TYPE == 'JAIL':
  return 0 
elif BLDG_TYPE == 'MIXED JAIL':
  return 0
elif BLDG_TYPE == 'WOMENS JAIL':
  return 0 
elif LAND_USE_D == 'DORMITORY/BOARDING HOUSE':
  return 0
elif LAND_USE_D == 'SCHOOL OR COLLEGE':
  return 0 
elif LAND_USE_D == 'NURSING HOME/SANITARIUM':
  return 0             
else:
  return child_pop_ratio * dasympop_tract_BG 




#Selective distribution of home owners

#Selective distribution of renters

#Selective distribution of group population



#14

#This script produces summary tables and time series figures for quantities related to the MWS buyout program and flood damages.

library(dplyr)
library(ggplot2)

wd<-getwd()
#wd<-("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/")
#wd<-("E:/kate/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/")
############################################################################################################
#Create Table of Summary Statistics for Economic Damage Estimates in 2010 based on Parcel-scale sf objects
############################################################################################################

pnames<-c( "finalnewparcels2007ob.RDS", "finalnewparcels2008ob.RDS", "finalnewparcels2009ob.RDS", "finalnewparcels2010ob.RDS", "finalnewparcels2011ob.RDS",
           "finalnewparcels2012ob.RDS", "finalnewparcels2013ob.RDS","finalnewparcels2014ob.RDS", 
           "finalnewparcels2007nb.RDS", "finalnewparcels2008nb.RDS", "finalnewparcels2009nb.RDS", "finalnewparcels2010nb.RDS", "finalnewparcels2011nb.RDS",
           "finalnewparcels2012nb.RDS", "finalnewparcels2013nb.RDS","finalnewparcels2014nb.RDS",
           "finalnewparcels2010ab.RDS", "finalnewparcels2011ab.RDS","finalnewparcels2012ab.RDS", "finalnewparcels2013ab.RDS","finalnewparcels2014ab.RDS",
           "finalnewparcels2010w.RDS", "finalnewparcels2011w.RDS","finalnewparcels2012w.RDS", "finalnewparcels2013w.RDS","finalnewparcels2014w.RDS") 


#No buyout scenario - with freeboard
  p10_nb<-readRDS(paste0(wd,"dasym3", pnames[12]))
  #p10_nb<-readRDS(paste0(wd,"Census", pnames[12]))
 # p10_nb<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/", pnames[12]))
  
  #county-scale
  dmg_1<-round(sum(p10_nb$pDMGWDFIA, na.rm=T )/1000000,2) #structural damages
  dmg_2<-round(sum(p10_nb$pDMGWDSV, na.rm=T )/1000000,2)
  dmg_rng_cnty<-paste0(dmg_1,"-",dmg_2) #range of structrual damages
  cont_cnty<-round(sum(p10_nb$pDMGCONTWDFIA , na.rm=T )/1000000,2) #content damages
  dmg_3<-round(sum(p10_nb$prelocWDHZ, na.rm=T )/1000000,2) #relocation costs
  dmg_4<-round(sum(p10_nb$prelocWDSV, na.rm=T )/1000000,2)
  reloc_cnty<-paste0(dmg_3,"-",dmg_4)
  labor_cnty<-round(sum(p10_nb$plaborWDSV, na.rm=T)/1000000,2)#labor costs
  tot_1<-sum(dmg_1,cont_cnty,dmg_3,labor_cnty)
  tot_2<-sum(dmg_2,cont_cnty, dmg_4,labor_cnty)
  tot_cnty<-paste0(tot_1,"-",tot_2)
  
    #watershed-scale
    wish_wsid<-unique(p10_nb$WS_ID[!is.na(p10_nb$Future)]) #watershed id for watersheds with any homes on buyout wish list
    p10_nb_sub<-p10_nb[p10_nb$WS_ID %in% wish_wsid,]
    dmg_1<-round(sum(p10_nb_sub$pDMGWDFIA, na.rm=T )/1000000,2)
    dmg_2<-round(sum(p10_nb_sub$pDMGWDSV, na.rm=T )/1000000,2)
    dmg_rng_ws<-paste0(dmg_1,"-",dmg_2)
    cont_ws<-round(sum(p10_nb_sub$pDMGCONTWDFIA , na.rm=T )/1000000,2) #content damages
    dmg_3<-round(sum(p10_nb_sub$prelocWDHZ, na.rm=T )/1000000,2) #relocation costs
    dmg_4<-round(sum(p10_nb_sub$prelocWDSV, na.rm=T )/1000000,2)
    reloc_ws<-paste0(dmg_3,"-",dmg_4)
    labor_ws<-round(sum(p10_nb_sub$plaborWDSV, na.rm=T)/1000000,2)#labor costs
    tot_1<-sum(dmg_1,cont_ws,dmg_3,labor_ws)
    tot_2<-sum(dmg_2,cont_ws,dmg_4,labor_ws)
    tot_ws<-paste0(tot_1,"-",tot_2)
  
      #buyout-scale
      p10_nb_sub<-p10_nb[!is.na(p10_nb$buyout),]
      dmg_1<-round(sum(p10_nb_sub$pDMGWDFIA, na.rm=T )/1000000,2)
      dmg_2<-round(sum(p10_nb_sub$pDMGWDSV, na.rm=T )/1000000,2)
      dmg_rng_bo<-paste0(dmg_1,"-",dmg_2)
      cont_bo<-round(sum(p10_nb_sub$pDMGCONTWDFIA , na.rm=T )/1000000,2) #content damages
      dmg_3<-round(sum(p10_nb_sub$prelocWDHZ, na.rm=T )/1000000,2) #relocation costs
      dmg_4<-round(sum(p10_nb_sub$prelocWDSV, na.rm=T )/1000000,2)
      reloc_bo<-paste0(dmg_3,"-",dmg_4)
      labor_bo<-round(sum(p10_nb_sub$plaborWDSV, na.rm=T)/1000000,2)#labor costs
      tot_1<-sum(dmg_1,cont_bo,dmg_3,labor_bo)
      tot_2<-sum(dmg_2,cont_bo,dmg_4,labor_bo)
      tot_bo<-paste0(tot_1,"-",tot_2)
      
      aqcost<-round(sum(p10_nb_sub$pAq_Cost, na.rm=T)/1000000,2) #fix this
      appr<-round(sum(p10_nb_sub$pAPPR, na.rm=T)/1000000,2)
  
        #vectors to go into table
        damages_nb<-c(dmg_rng_cnty, dmg_rng_ws, dmg_rng_bo)
        cont_nb<-c(cont_cnty,cont_ws,cont_bo)
        reloc_nb<-c(reloc_cnty,reloc_ws,reloc_bo)
        labor_nb<-c(labor_cnty,labor_ws,labor_bo)
        tot_nb<-c(tot_cnty,tot_ws,tot_bo)
        aq_cost_nb<-c(NA,NA,aqcost)
        appr_nb<-c(NA,NA,appr)


#Observed buyout scenario - with freeboard
        p10_ob<-readRDS(paste0(wd,"dasym3",pnames[4]))
        #p10_ob<-readRDS(paste0(wd,"Census",pnames[4]))
#p10_ob<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/","dasym3",pnames[4]))
#p10_ob<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/", pnames[4]))
  #county-scale
  dmg_1<-round(sum(p10_ob$pDMGWDFIA, na.rm=T )/1000000,2) #structural damages
  dmg_2<-round(sum(p10_ob$pDMGWDSV, na.rm=T )/1000000,2)
  dmg_rng_cnty<-paste0(dmg_1,"-",dmg_2) #range of structrual damages
  cont_cnty<-round(sum(p10_ob$pDMGCONTWDFIA , na.rm=T )/1000000,2) #content damages
  dmg_3<-round(sum(p10_ob$prelocWDHZ, na.rm=T )/1000000,2) #relocation costs
  dmg_4<-round(sum(p10_ob$prelocWDSV, na.rm=T )/1000000,2)
  reloc_cnty<-paste0(dmg_3,"-",dmg_4)
  labor_cnty<-round(sum(p10_ob$plaborWDSV, na.rm=T)/1000000,2)#labor costs
  tot_1<-sum(dmg_1,cont_cnty,dmg_3,labor_cnty)
  tot_2<-sum(dmg_2,cont_cnty, dmg_4,labor_cnty)
  tot_cnty<-paste0(tot_1,"-",tot_2)
  
    #watershed-scale
   # wish_wsid<-unique(p10_ob$WS_ID[!is.na(p10_ob$Future)]) #watershed id for watersheds with any homes on buyout wish list
    p10_ob_sub<-p10_ob[p10_ob$WS_ID %in% wish_wsid,]
    dmg_1<-round(sum(p10_ob_sub$pDMGWDFIA, na.rm=T )/1000000,2)
    dmg_2<-round(sum(p10_ob_sub$pDMGWDSV, na.rm=T )/1000000,2)
    dmg_rng_ws<-paste0(dmg_1,"-",dmg_2)
    cont_ws<-round(sum(p10_ob_sub$pDMGCONTWDFIA , na.rm=T )/1000000,2) #content damages
    dmg_3<-round(sum(p10_ob_sub$prelocWDHZ, na.rm=T )/1000000,2) #relocation costs
    dmg_4<-round(sum(p10_ob_sub$prelocWDSV, na.rm=T )/1000000,2)
    reloc_ws<-paste0(dmg_3,"-",dmg_4)
    labor_ws<-round(sum(p10_ob_sub$plaborWDSV, na.rm=T)/1000000,2)#labor costs
    tot_1<-sum(dmg_1,cont_ws,dmg_3,labor_ws)
    tot_2<-sum(dmg_2,cont_ws,dmg_4,labor_ws)
    tot_ws<-paste0(tot_1,"-",tot_2)
    
      #buyout-scale
      p10_ob_sub<-p10_ob[!is.na(p10_ob$buyout),]
      dmg_1<-round(sum(p10_ob_sub$pDMGWDFIA, na.rm=T )/1000000,2)
      dmg_2<-round(sum(p10_ob_sub$pDMGWDSV, na.rm=T )/1000000,2)
      dmg_rng_bo<-paste0(dmg_1,"-",dmg_2)
      cont_bo<-round(sum(p10_ob_sub$pDMGCONTWDFIA , na.rm=T )/1000000,2) #content damages
      dmg_3<-round(sum(p10_ob_sub$prelocWDHZ, na.rm=T )/1000000,2) #relocation costs
      dmg_4<-round(sum(p10_ob_sub$prelocWDSV, na.rm=T )/1000000,2)
      reloc_bo<-paste0(dmg_3,"-",dmg_4)
      labor_bo<-round(sum(p10_ob_sub$plaborWDSV, na.rm=T)/1000000,2)#labor costs
      tot_1<-sum(dmg_1,cont_bo,dmg_3,labor_bo)
      tot_2<-sum(dmg_2,cont_bo,dmg_4,labor_bo)
      tot_bo<-paste0(tot_1,"-",tot_2)
      
      aqcost<-round(sum(p10_ob_sub$pAq_Cost, na.rm=T)/1000000,2) #fix this
      appr<-round(sum(p10_ob_sub$pAPPR, na.rm=T)/1000000,2)
      
        #vectors to go into table
        damages_ob<-c(dmg_rng_cnty, dmg_rng_ws, dmg_rng_bo)
        cont_ob<-c(cont_cnty,cont_ws,cont_bo)
        reloc_ob<-c(reloc_cnty,reloc_ws,reloc_bo)
        labor_ob<-c(labor_cnty,labor_ws,labor_bo)
        tot_ob<-c(tot_cnty,tot_ws,tot_bo)
        aq_cost_ob<-c(NA,NA,aqcost)
        appr_ob<-c(NA,NA,appr)

#All buyout scenario - with freeboard
 p10_ab<-readRDS(paste0(wd,"dasym3",pnames[17]))
 #p10_ab<-readRDS(paste0(wd,"Census",pnames[17]))
        
# p10_ab<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/","dasym3",pnames[17]))
# p10_ab<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/", pnames[17]))
  #county-scale
  dmg_1<-round(sum(p10_ab$pDMGWDFIA, na.rm=T )/1000000,2) #structural damages
  dmg_2<-round(sum(p10_ab$pDMGWDSV, na.rm=T )/1000000,2)
  dmg_rng_cnty<-paste0(dmg_1,"-",dmg_2) #range of structrual damages
  cont_cnty<-round(sum(p10_ab$pDMGCONTWDFIA , na.rm=T )/1000000,2) #content damages
  dmg_3<-round(sum(p10_ab$prelocWDHZ, na.rm=T )/1000000,2) #relocation costs
  dmg_4<-round(sum(p10_ab$prelocWDSV, na.rm=T )/1000000,2)
  reloc_cnty<-paste0(dmg_3,"-",dmg_4)
  labor_cnty<-round(sum(p10_ab$plaborWDSV, na.rm=T)/1000000,2)#labor costs
  tot_1<-sum(dmg_1,cont_cnty,dmg_3,labor_cnty)
  tot_2<-sum(dmg_2,cont_cnty, dmg_4,labor_cnty)
  tot_cnty<-paste0(tot_1,"-",tot_2)
  
    #watershed-scale
   # wish_wsid<-unique(p10_ab$WS_ID[!is.na(p10_ab$Future)]) #watershed id for watersheds with any homes on buyout wish list
    p10_ab_sub<-p10_ab[p10_ab$WS_ID %in% wish_wsid,]
    dmg_1<-round(sum(p10_ab_sub$pDMGWDFIA, na.rm=T )/1000000,2)
    dmg_2<-round(sum(p10_ab_sub$pDMGWDSV, na.rm=T )/1000000,2)
    dmg_rng_ws<-paste0(dmg_1,"-",dmg_2)
    cont_ws<-round(sum(p10_ab_sub$pDMGCONTWDFIA , na.rm=T )/1000000,2) #content damages
    dmg_3<-round(sum(p10_ab_sub$prelocWDHZ, na.rm=T )/1000000,2) #relocation costs
    dmg_4<-round(sum(p10_ab_sub$prelocWDSV, na.rm=T )/1000000,2)
    reloc_ws<-paste0(dmg_3,"-",dmg_4)
    labor_ws<-round(sum(p10_ab_sub$plaborWDSV, na.rm=T)/1000000,2)#labor costs
    tot_1<-sum(dmg_1,cont_ws,dmg_3,labor_ws)
    tot_2<-sum(dmg_2,cont_ws,dmg_4,labor_ws)
    tot_ws<-paste0(tot_1,"-",tot_2)
  
      #buyout-scale
      p10_ab_sub<-p10_ab[!is.na(p10_ab$buyout),]
      dmg_1<-round(sum(p10_ab_sub$pDMGWDFIA, na.rm=T )/1000000,2)
      dmg_2<-round(sum(p10_ab_sub$pDMGWDSV, na.rm=T )/1000000,2)
      dmg_rng_bo<-paste0(dmg_1,"-",dmg_2)
      cont_bo<-round(sum(p10_ab_sub$pDMGCONTWDFIA , na.rm=T )/1000000,2) #content damages
      dmg_3<-round(sum(p10_ab_sub$prelocWDHZ, na.rm=T )/1000000,2) #relocation costs
      dmg_4<-round(sum(p10_ab_sub$prelocWDSV, na.rm=T )/1000000,2)
      reloc_bo<-paste0(dmg_3,"-",dmg_4)
      labor_bo<-round(sum(p10_ab_sub$plaborWDSV, na.rm=T)/1000000,2)#labor costs
      tot_1<-sum(dmg_1,cont_bo,dmg_3,labor_bo)
      tot_2<-sum(dmg_2,cont_bo,dmg_4,labor_bo)
      tot_bo<-paste0(tot_1,"-",tot_2)
      
      aqcost<-round(sum(p10_ab_sub$pAq_Cost, na.rm=T)/1000000,2) #fix this
      appr<-round(sum(p10_ab_sub$pAPPR, na.rm=T)/1000000,2)
  
        #vectors to go into table
        damages_ab<-c(dmg_rng_cnty, dmg_rng_ws, dmg_rng_bo)
        cont_ab<-c(cont_cnty,cont_ws,cont_bo)
        reloc_ab<-c(reloc_cnty,reloc_ws,reloc_bo)
        labor_ab<-c(labor_cnty,labor_ws,labor_bo)
        tot_ab<-c(tot_cnty,tot_ws,tot_bo)
        aq_cost_ab<-c(NA,NA,aqcost)
        appr_ab<-c(NA,NA,appr)

#Wishlist scenario - with freeboard
p10_w<-readRDS(paste0(wd,"dasym3",pnames[22]))
#p10_w<-readRDS(paste0(wd,"Census",pnames[22]))
        
# p10_w<-readRDS(paste0("E:/kate/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/",pnames[22]))
# p10_w<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/", pnames[22]))
  #county-scale
  dmg_1<-round(sum(p10_w$pDMGWDFIA, na.rm=T )/1000000,2) #structural damages
  dmg_2<-round(sum(p10_w$pDMGWDSV, na.rm=T )/1000000,2)
  dmg_rng_cnty<-paste0(dmg_1,"-",dmg_2) #range of structrual damages
  cont_cnty<-round(sum(p10_w$pDMGCONTWDFIA , na.rm=T )/1000000,2) #content damages
  dmg_3<-round(sum(p10_w$prelocWDHZ, na.rm=T )/1000000,2) #relocation costs
  dmg_4<-round(sum(p10_w$prelocWDSV, na.rm=T )/1000000,2)
  reloc_cnty<-paste0(dmg_3,"-",dmg_4)
  labor_cnty<-round(sum(p10_w$plaborWDSV, na.rm=T)/1000000,2)#labor costs
  tot_1<-sum(dmg_1,cont_cnty,dmg_3,labor_cnty)
  tot_2<-sum(dmg_2,cont_cnty, dmg_4,labor_cnty)
  tot_cnty<-paste0(tot_1,"-",tot_2)
  
    #watershed-scale
      p10_w_sub<-p10_w[p10_w$WS_ID %in% wish_wsid,]
    dmg_1<-round(sum(p10_w_sub$pDMGWDFIA, na.rm=T )/1000000,2)
    dmg_2<-round(sum(p10_w_sub$pDMGWDSV, na.rm=T )/1000000,2)
    dmg_rng_ws<-paste0(dmg_1,"-",dmg_2)
    cont_ws<-round(sum(p10_w_sub$pDMGCONTWDFIA , na.rm=T )/1000000,2) #content damages
    dmg_3<-round(sum(p10_w_sub$prelocWDHZ, na.rm=T )/1000000,2) #relocation costs
    dmg_4<-round(sum(p10_w_sub$prelocWDSV, na.rm=T )/1000000,2)
    reloc_ws<-paste0(dmg_3,"-",dmg_4)
    labor_ws<-round(sum(p10_w_sub$plaborWDSV, na.rm=T)/1000000,2)#labor costs
    tot_1<-sum(dmg_1,cont_ws,dmg_3,labor_ws)
    tot_2<-sum(dmg_2,cont_ws,dmg_4,labor_ws)
    tot_ws<-paste0(tot_1,"-",tot_2)
  
      #buyout-scale
      p10_w_sub<-p10_w[!is.na(p10_w$buyout),]
      dmg_1<-round(sum(p10_w_sub$pDMGWDFIA, na.rm=T )/1000000,2)
      dmg_2<-round(sum(p10_w_sub$pDMGWDSV, na.rm=T )/1000000,2)
      dmg_rng_bo<-paste0(dmg_1,"-",dmg_2)
      cont_bo<-round(sum(p10_w_sub$pDMGCONTWDFIA , na.rm=T )/1000000,2) #content damages
      dmg_3<-round(sum(p10_w_sub$prelocWDHZ, na.rm=T )/1000000,2) #relocation costs
      dmg_4<-round(sum(p10_w_sub$prelocWDSV, na.rm=T )/1000000,2)
      reloc_bo<-paste0(dmg_3,"-",dmg_4)
      labor_bo<-round(sum(p10_w_sub$plaborWDSV, na.rm=T)/1000000,2)#labor costs
      tot_1<-sum(dmg_1,cont_bo,dmg_3,labor_bo)
      tot_2<-sum(dmg_2,cont_bo,dmg_4,labor_bo)
      tot_bo<-paste0(tot_1,"-",tot_2)
      
      aqcost<-round(sum(p10_w_sub$pAq_Cost, na.rm=T)/1000000,2) #fix this
      appr<-round(sum(p10_w_sub$pAPPR, na.rm=T)/1000000,2)
  
        #vectors to go into table
        damages_w<-c(dmg_rng_cnty, dmg_rng_ws, dmg_rng_bo)
        cont_w<-c(cont_cnty,cont_ws,cont_bo)
        reloc_w<-c(reloc_cnty,reloc_ws,reloc_bo)
        labor_w<-c(labor_cnty,labor_ws,labor_bo)
        tot_w<-c(tot_cnty,tot_ws,tot_bo)
        aq_cost_w<-c(NA,NA,aqcost)
        appr_w<-c(NA,NA,appr)

#Put together the table
  Scenario<-c("No Buyouts","No Buyouts","No Buyouts",
              "With Buyouts Prior to 2010","With Buyouts Prior to 2010","With Buyouts Prior to 2010",
              "All Buyouts Prior to 2010", "All Buyouts Prior to 2010","All Buyouts Prior to 2010",
              "Wishlist Prior to 2010","Wishlist Prior to 2010","Wishlist Prior to 2010")
  Scale<-c("County","Micro-watersheds with Buyouts","Buyout Parcels",
           "County","Micro-watersheds with Buyouts","Buyout Parcels",
           "County","Micro-watersheds with Buyouts","Buyout Parcels",
           "County","Micro-watersheds with Buyouts","Buyout Parcels")
  
  Structural_Damages<-c(damages_nb,damages_ob,damages_ab,damages_w)
  Contents_Damages<-c(cont_nb,cont_ob,cont_ab,cont_w)
  Relocation_Costs<-c(reloc_nb,reloc_ob,reloc_ab,reloc_w)
  Labor_Costs<-c(labor_nb,labor_ob,labor_ab,labor_w)
  Total_Damages<-c(tot_nb,tot_ob,tot_ab,tot_w)
  Acquisition<-c(aq_cost_nb,aq_cost_ob,aq_cost_ab,aq_cost_w)
  Appraisals<-c(appr_nb,appr_ob,appr_ab,appr_w)
  
  df<-data.frame(Scenario, Scale, Structural_Damages, Contents_Damages,Relocation_Costs,
                 Labor_Costs, Total_Damages, Acquisition, Appraisals)

  dt<-xtable(df)

#####################################################################################################################################
#Create Table of Summary Statistics for Direct Damage Counts in 2010 based on Parcel-scale sf objects and Building-scale sf object
###################################################################################################################################

#No buyout scenario - with freeboard
  
  p10_nb<-readRDS(paste0(wd,"dasym3", pnames[12]))
  

  
  #county-scale
  pcnt<-length(p10_nb$STANPAR) #total parcels
  pdicnt<-length(p10_nb$STANPAR[!is.na(p10_nb$DLEVEL)| !is.na(p10_nb$inundate) ]) #parcels damaged or inundated
  percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
  p_cnty<-pdicnt #count of damaged or inundated parcels
  
  # bcnt<-length(b10_nb$STANPAR) #total bldgs
  # bdicnt<-length(b10_nb$STANPAR[!is.na(b10_nb$DLEVEL)| !is.na(b10_nb$inundate)]) #bldgs damaged or inundated
  # percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
  # b_cnty<-bdicnt
  
  popcnt<-sum(p10_nb$P.new,na.rm=T)
  pop_di<-sum(p10_nb$P.new[!is.na(p10_nb$DLEVEL)| !is.na(p10_nb$inundate)], na.rm=T)
  percpop<-pop_di/popcnt
  pop_cnty<-pop_di
  
  senpop_cnt<-sum(p10_nb$P.newsenior,na.rm=T)
  senpop_di<-sum(p10_nb$P.newsenior[!is.na(p10_nb$DLEVEL)| !is.na(p10_nb$inundate)], na.rm=T)
  percsenpop<-senpop_di/senpopcnt
  senpop_cnty<-senpop_di
  
  rentpopcnt<-sum(p10_nb$P.newrent,na.rm=T)
  rentpop_di<-sum(p10_nb$P.newrent[!is.na(p10_nb$DLEVEL)| !is.na(p10_nb$inundate)], na.rm=T)
  rentpercpop<-rentpop_di/rentpopcnt
  rentpop_cnty<-rentpop_di

    #watershed-scale
    wish_wsid<-unique(p10_nb$WS_ID[!is.na(p10_nb$Future)]) #watershed id for watersheds with any homes on buyout wish list
    p10_nb_sub<-p10_nb[p10_nb$WS_ID %in% wish_wsid,]
    pcnt<-length(p10_nb_sub$STANPAR) #total parcels
    pdicnt<-length(p10_nb_sub$STANPAR[!is.na(p10_nb_sub$DLEVEL)| !is.na(p10_nb_sub$inundate)]) #parcels damaged or inundated
    percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
    p_ws<-pdicnt #count of damaged or inundated parcels
    
    # bcnt<-length(b10_nb$STANPAR) #total bldgs
    # bdicnt<-length(b10_nb$STANPAR[!is.na(b10_nb$DLEVEL)| !is.na(b10_nb$inundate)]) #bldgs damaged or inundated
    # percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
    # b_ws<-bdicnt
    
    popcnt<-sum(p10_nb_sub$P.new,na.rm=T) #tot population
    pop_di<-sum(p10_nb_sub$P.new[!is.na(p10_nb_sub$DLEVEL)| !is.na(p10_nb_sub$inundate)], na.rm=T) #population in damaged orinundated buildings
    percpop<-pop_di/popcnt
    pop_ws<-pop_di
    
    senpop_cnt<-sum(p10_nb_sub$P.newsenior,na.rm=T) #senior popualtion
    senpop_di<-sum(p10_nb_sub$P.newsenior[!is.na(p10_nb_sub$DLEVEL)| !is.na(p10_nb_sub$inundate)], na.rm=T)
    percsenpop<-senpop_di/senpopcnt
    senpop_ws<-senpop_di
    
    rentpopcnt<-sum(p10_nb_sub$P.newrent,na.rm=T) #renter population
    rentpop_di<-sum(p10_nb_sub$P.newrent[!is.na(p10_nb_sub$DLEVEL)| !is.na(p10_nb_sub$inundate)], na.rm=T)
    rentpercpop<-rentpop_di/rentpopcnt
    rentpop_ws<-rentpop_di


      #buyout-scale
      p10_nb_sub<-p10_nb[!is.na(p10_nb$buyout),]
      pcnt<-length(p10_nb_sub$STANPAR) #total parcels
      pdicnt<-length(p10_nb_sub$STANPAR[!is.na(p10_nb_sub$DLEVEL)| !is.na(p10_nb_sub$inundate)]) #parcels damaged or inundated
      percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
      p_bo<-pdicnt #count of damaged or inundated parcels
      
      # bcnt<-length(b10_nb$STANPAR) #total bldgs
      # bdicnt<-length(b10_nb$STANPAR[!is.na(b10_nb$DLEVEL)| !is.na(b10_nb$inundate)]) #bldgs damaged or inundated
      # percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
      # b_bo<-bdicnt
      
      popcnt<-sum(p10_nb_sub$P.new,na.rm=T) #tot population
      pop_di<-sum(p10_nb_sub$P.new[!is.na(p10_nb_sub$DLEVEL)| !is.na(p10_nb_sub$inundate)], na.rm=T) #population in damaged orinundated buildings
      percpop<-pop_di/popcnt
      pop_bo<-pop_di
      
      senpop_cnt<-sum(p10_nb_sub$P.newsenior,na.rm=T) #senior popualtion
      senpop_di<-sum(p10_nb_sub$P.newsenior[!is.na(p10_nb_sub$DLEVEL)| !is.na(p10_nb_sub$inundate)], na.rm=T)
      percsenpop<-senpop_di/senpopcnt
      senpop_bo<-senpop_di
      
      rentpopcnt<-sum(p10_nb_sub$P.newrent,na.rm=T) #renter population
      rentpop_di<-sum(p10_nb_sub$P.newrent[!is.na(p10_nb_sub$DLEVEL)| !is.na(p10_nb_sub$inundate)], na.rm=T)
      rentpercpop<-rentpop_di/rentpopcnt
      rentpop_bo<-rentpop_di

      
        #vectors to go into table
        parcels_nb<-c(p_cnty, p_ws, p_bo)
        pop_nb<-c(pop_cnty,pop_ws,pop_bo)
        senpop_nb<-c(senpop_cnty,senpop_ws,senpop_bo)
        rentpop_nb<-c(rentpop_cnty,rentpop_ws,rentpop_bo)
      


#Observed buyout scenario - with freeboard
 p10_ob<-readRDS(paste0(wd,"dasym3",pnames[4]))
#p10_ob<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/","dasym3",pnames[4]))

#county-scale
pcnt<-length(p10_ob$STANPAR) #total parcels
pdicnt<-length(p10_ob$STANPAR[!is.na(p10_ob$pUSACE100)]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_cnty<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_ob$STANPAR) #total bldgs
# bdicnt<-length(b10_ob$STANPAR[!is.na(b10_ob$DLEVEL)| !is.na(b10_ob$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_cnty<-bdicnt

popcnt<-sum(p10_ob$P.new,na.rm=T)
pop_di<-sum(p10_ob$P.new[!is.na(p10_ob$pUSACE100)], na.rm=T)
percpop<-pop_di/popcnt
pop_cnty<-pop_di

senpop_cnt<-sum(p10_ob$P.newsenior,na.rm=T)
senpop_di<-sum(p10_ob$P.newsenior[!is.na(p10_ob$pUSACE100)], na.rm=T)
percsenpop<-senpop_di/senpopcnt
senpop_cnty<-senpop_di

rentpopcnt<-sum(p10_ob$P.newrent,na.rm=T)
rentpop_di<-sum(p10_ob$P.newrent[!is.na(p10_ob$pUSACE100)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_cnty<-rentpop_di

#watershed-scale
#wish_wsid<-unique(p10_ob$WS_ID[!is.na(p10_ob$Future)]) #watershed id for watersheds with any homes on buyout wish list
p10_ob_sub<-p10_ob[p10_ob$WS_ID %in% wish_wsid,]
pcnt<-length(p10_ob_sub$STANPAR) #total parcels
pdicnt<-length(p10_ob_sub$STANPAR[!is.na(p10_ob_sub$DLEVEL)| !is.na(p10_ob_sub$inundate)]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_ws<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_ob$STANPAR) #total bldgs
# bdicnt<-length(b10_ob$STANPAR[!is.na(b10_ob$DLEVEL)| !is.na(b10_ob$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_ws<-bdicnt

popcnt<-sum(p10_ob_sub$P.new,na.rm=T) #tot population
pop_di<-sum(p10_ob_sub$P.new[!is.na(p10_ob_sub$DLEVEL)| !is.na(p10_ob_sub$inundate)], na.rm=T) #population in damaged orinundated buildings
percpop<-pop_di/popcnt
pop_ws<-pop_di

senpop_cnt<-sum(p10_ob_sub$P.newsenior,na.rm=T) #senior popualtion
senpop_di<-sum(p10_ob_sub$P.newsenior[!is.na(p10_ob_sub$DLEVEL)| !is.na(p10_ob_sub$inundate)], na.rm=T)
percsenpop<-senpop_di/senpopcnt
senpop_ws<-senpop_di

rentpopcnt<-sum(p10_ob_sub$P.newrent,na.rm=T) #renter population
rentpop_di<-sum(p10_ob_sub$P.newrent[!is.na(p10_ob_sub$DLEVEL)| !is.na(p10_ob_sub$inundate)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_ws<-rentpop_di


#buyout-scale
p10_ob_sub<-p10_ob[!is.na(p10_ob$buyout),]
pcnt<-length(p10_ob_sub$STANPAR) #total parcels
pdicnt<-length(p10_ob_sub$STANPAR[!is.na(p10_ob_sub$DLEVEL)| !is.na(p10_ob_sub$inundate)]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_bo<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_ob$STANPAR) #total bldgs
# bdicnt<-length(b10_ob$STANPAR[!is.na(b10_ob$DLEVEL)| !is.na(b10_ob$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_bo<-bdicnt

popcnt<-sum(p10_ob_sub$P.new,na.rm=T) #tot population
pop_di<-sum(p10_ob_sub$P.new[!is.na(p10_ob_sub$DLEVEL)| !is.na(p10_ob_sub$inundate)], na.rm=T) #population in damaged orinundated buildings
percpop<-pop_di/popcnt
pop_bo<-pop_di

senpop_cnt<-sum(p10_ob_sub$P.newsenior,na.rm=T) #senior popualtion
senpop_di<-sum(p10_ob_sub$P.newsenior[!is.na(p10_ob_sub$DLEVEL)| !is.na(p10_ob_sub$inundate)], na.rm=T)
percsenpop<-senpop_di/senpopcnt
senpop_bo<-senpop_di

rentpopcnt<-sum(p10_ob_sub$P.newrent,na.rm=T) #renter population
rentpop_di<-sum(p10_ob_sub$P.newrent[!is.na(p10_ob_sub$DLEVEL)| !is.na(p10_ob_sub$inundate)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_bo<-rentpop_di


#vectors to go into table
parcels_ob<-c(p_cnty, p_ws, p_bo)
pop_ob<-c(pop_cnty,pop_ws,pop_bo)
senpop_ob<-c(senpop_cnty,senpop_ws,senpop_bo)
rentpop_ob<-c(rentpop_cnty,rentpop_ws,rentpop_bo)


#All buyout scenario - with freeboard
p10_ab<-readRDS(paste0(wd,"dasym3",pnames[17]))
#p10_ab<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/","dasym3",pnames[17]))

#county-scale
pcnt<-length(p10_ab$STANPAR) #total parcels
pdicnt<-length(p10_ab$STANPAR[!is.na(p10_ab$DLEVEL)| !is.na(p10_ab$inundate)]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_cnty<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_ab$STANPAR) #total bldgs
# bdicnt<-length(b10_ab$STANPAR[!is.na(b10_ab$DLEVEL)| !is.na(b10_ab$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_cnty<-bdicnt

popcnt<-sum(p10_ab$P.new,na.rm=T)
pop_di<-sum(p10_ab$P.new[!is.na(p10_ab$DLEVEL)| !is.na(p10_ab$inundate)], na.rm=T)
percpop<-pop_di/popcnt
pop_cnty<-pop_di

senpop_cnt<-sum(p10_ab$P.newsenior,na.rm=T)
senpop_di<-sum(p10_ab$P.newsenior[!is.na(p10_ab$DLEVEL)| !is.na(p10_ab$inundate)], na.rm=T)
percsenpop<-senpop_di/senpopcnt
senpop_cnty<-senpop_di

rentpopcnt<-sum(p10_ab$P.newrent,na.rm=T)
rentpop_di<-sum(p10_ab$P.newrent[!is.na(p10_ab$DLEVEL)| !is.na(p10_ab$inundate)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_cnty<-rentpop_di

#watershed-scale
#wish_wsid<-unique(p10_ab$WS_ID[!is.na(p10_ab$Future)]) #watershed id for watersheds with any homes on buyout wish list
p10_ab_sub<-p10_ab[p10_ab$WS_ID %in% wish_wsid,]
pcnt<-length(p10_ab_sub$STANPAR) #total parcels
pdicnt<-length(p10_ab_sub$STANPAR[!is.na(p10_ab_sub$DLEVEL)| !is.na(p10_ab_sub$inundate)]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_ws<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_ab$STANPAR) #total bldgs
# bdicnt<-length(b10_ab$STANPAR[!is.na(b10_ab$DLEVEL)| !is.na(b10_ab$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_ws<-bdicnt

popcnt<-sum(p10_ab_sub$P.new,na.rm=T) #tot population
pop_di<-sum(p10_ab_sub$P.new[!is.na(p10_ab_sub$DLEVEL)| !is.na(p10_ab_sub$inundate)], na.rm=T) #population in damaged orinundated buildings
percpop<-pop_di/popcnt
pop_ws<-pop_di

senpop_cnt<-sum(p10_ab_sub$P.newsenior,na.rm=T) #senior popualtion
senpop_di<-sum(p10_ab_sub$P.newsenior[!is.na(p10_ab_sub$DLEVEL)| !is.na(p10_ab_sub$inundate)], na.rm=T)
percsenpop<-senpop_di/senpopcnt
senpop_ws<-senpop_di

rentpopcnt<-sum(p10_ab_sub$P.newrent,na.rm=T) #renter population
rentpop_di<-sum(p10_ab_sub$P.newrent[!is.na(p10_ab_sub$DLEVEL)| !is.na(p10_ab_sub$inundate)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_ws<-rentpop_di


#buyout-scale
p10_ab_sub<-p10_ab[!is.na(p10_ab$buyout),]
pcnt<-length(p10_ab_sub$STANPAR) #total parcels
pdicnt<-length(p10_ab_sub$STANPAR[!is.na(p10_ab_sub$DLEVEL)| !is.na(p10_ab_sub$inundate)]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_bo<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_ab$STANPAR) #total bldgs
# bdicnt<-length(b10_ab$STANPAR[!is.na(b10_ab$DLEVEL)| !is.na(b10_ab$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_bo<-bdicnt

popcnt<-sum(p10_ab_sub$P.new,na.rm=T) #tot population
pop_di<-sum(p10_ab_sub$P.new[!is.na(p10_ab_sub$DLEVEL)| !is.na(p10_ab_sub$inundate)], na.rm=T) #population in damaged orinundated buildings
percpop<-pop_di/popcnt
pop_bo<-pop_di

senpop_cnt<-sum(p10_ab_sub$P.newsenior,na.rm=T) #senior popualtion
senpop_di<-sum(p10_ab_sub$P.newsenior[!is.na(p10_ab_sub$DLEVEL)| !is.na(p10_ab_sub$inundate)], na.rm=T)
percsenpop<-senpop_di/senpopcnt
senpop_bo<-senpop_di

rentpopcnt<-sum(p10_ab_sub$P.newrent,na.rm=T) #renter population
rentpop_di<-sum(p10_ab_sub$P.newrent[!is.na(p10_ab_sub$DLEVEL)| !is.na(p10_ab_sub$inundate)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_bo<-rentpop_di


#vectors to go into table
parcels_ab<-c(p_cnty, p_ws, p_bo)
pop_ab<-c(pop_cnty,pop_ws,pop_bo)
senpop_ab<-c(senpop_cnty,senpop_ws,senpop_bo)
rentpop_ab<-c(rentpop_cnty,rentpop_ws,rentpop_bo)


#Wishlist scenario - with freeboard
p10_w<-readRDS(paste0(wd,"dasym3",pnames[22]))
#p10_w<-readRDS(paste0("E:/kate/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/",pnames[22]))

  #county-scale
  pcnt<-length(p10_w$STANPAR) #total parcels
  pdicnt<-length(p10_w$STANPAR[!is.na(p10_w$DLEVEL)| !is.na(p10_w$inundate)]) #parcels damaged or inundated
  percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
  p_cnty<-pdicnt #count of damaged or inundated parcels
  
  # bcnt<-length(b10_w$STANPAR) #total bldgs
  # bdicnt<-length(b10_w$STANPAR[!is.na(b10_w$DLEVEL)| !is.na(b10_w$inundate)]) #bldgs damaged or inundated
  # percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
  # b_cnty<-bdicnt
  
  popcnt<-sum(p10_w$P.new,na.rm=T)
  pop_di<-sum(p10_w$P.new[!is.na(p10_w$DLEVEL)| !is.na(p10_w$inundate)], na.rm=T)
  percpop<-pop_di/popcnt
  pop_cnty<-pop_di
  
  senpop_cnt<-sum(p10_w$P.newsenior,na.rm=T)
  senpop_di<-sum(p10_w$P.newsenior[!is.na(p10_w$DLEVEL)| !is.na(p10_w$inundate)], na.rm=T)
  percsenpop<-senpop_di/senpopcnt
  senpop_cnty<-senpop_di
  
  rentpopcnt<-sum(p10_w$P.newrent,na.rm=T)
  rentpop_di<-sum(p10_w$P.newrent[!is.na(p10_w$DLEVEL)| !is.na(p10_w$inundate)], na.rm=T)
  rentpercpop<-rentpop_di/rentpopcnt
  rentpop_cnty<-rentpop_di

    #watershed-scale
    p10_w_sub<-p10_w[p10_w$WS_ID %in% wish_wsid,]
    pcnt<-length(p10_w_sub$STANPAR) #total parcels
    pdicnt<-length(p10_w_sub$STANPAR[!is.na(p10_w_sub$DLEVEL)| !is.na(p10_w_sub$inundate)]) #parcels damaged or inundated
    percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
    p_ws<-pdicnt #count of damaged or inundated parcels
    
    # bcnt<-length(b10_w$STANPAR) #total bldgs
    # bdicnt<-length(b10_w$STANPAR[!is.na(b10_w$DLEVEL)| !is.na(b10_w$inundate)]) #bldgs damaged or inundated
    # percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
    # b_ws<-bdicnt
    
    popcnt<-sum(p10_w_sub$P.new,na.rm=T) #tot population
    pop_di<-sum(p10_w_sub$P.new[!is.na(p10_w_sub$DLEVEL)| !is.na(p10_w_sub$inundate)], na.rm=T) #population in damaged orinundated buildings
    percpop<-pop_di/popcnt
    pop_ws<-pop_di
    
    senpop_cnt<-sum(p10_w_sub$P.newsenior,na.rm=T) #senior popualtion
    senpop_di<-sum(p10_w_sub$P.newsenior[!is.na(p10_w_sub$DLEVEL)| !is.na(p10_w_sub$inundate)], na.rm=T)
    percsenpop<-senpop_di/senpopcnt
    senpop_ws<-senpop_di
    
    rentpopcnt<-sum(p10_w_sub$P.newrent,na.rm=T) #renter population
    rentpop_di<-sum(p10_w_sub$P.newrent[!is.na(p10_w_sub$DLEVEL)| !is.na(p10_w_sub$inundate)], na.rm=T)
    rentpercpop<-rentpop_di/rentpopcnt
    rentpop_ws<-rentpop_di


      #buyout-scale
      p10_w_sub<-p10_w[!is.na(p10_w$buyout),]
      pcnt<-length(p10_w_sub$STANPAR) #total parcels
      pdicnt<-length(p10_w_sub$STANPAR[!is.na(p10_w_sub$DLEVEL)| !is.na(p10_w_sub$inundate)]) #parcels damaged or inundated
      percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
      p_bo<-pdicnt #count of damaged or inundated parcels
      
      # bcnt<-length(b10_w$STANPAR) #total bldgs
      # bdicnt<-length(b10_w$STANPAR[!is.na(b10_w$DLEVEL)| !is.na(b10_w$inundate)]) #bldgs damaged or inundated
      # percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
      # b_bo<-bdicnt
      
      popcnt<-sum(p10_w_sub$P.new,na.rm=T) #tot population
      pop_di<-sum(p10_w_sub$P.new[!is.na(p10_w_sub$DLEVEL)| !is.na(p10_w_sub$inundate)], na.rm=T) #population in damaged orinundated buildings
      percpop<-pop_di/popcnt
      pop_bo<-pop_di
      
      senpop_cnt<-sum(p10_w_sub$P.newsenior,na.rm=T) #senior popualtion
      senpop_di<-sum(p10_w_sub$P.newsenior[!is.na(p10_w_sub$DLEVEL)| !is.na(p10_w_sub$inundate)], na.rm=T)
      percsenpop<-senpop_di/senpop_cnt
      senpop_bo<-senpop_di
      
      rentpopcnt<-sum(p10_w_sub$P.newrent,na.rm=T) #renter population
      rentpop_di<-sum(p10_w_sub$P.newrent[!is.na(p10_w_sub$DLEVEL)| !is.na(p10_w_sub$inundate)], na.rm=T)
      rentpercpop<-rentpop_di/rentpopcnt
      rentpop_bo<-rentpop_di

      
      #vectors to go into table
      parcels_w<-c(p_cnty, p_ws, p_bo)
      pop_w<-c(pop_cnty,pop_ws,pop_bo)
      senpop_w<-c(senpop_cnty,senpop_ws,senpop_bo)
      rentpop_w<-c(rentpop_cnty,rentpop_ws,rentpop_bo)


  #Put together the table
  Scenario<-c("No Buyouts","No Buyouts","No Buyouts",
              "With Buyouts Prior to 2010","With Buyouts Prior to 2010","With Buyouts Prior to 2010",
              "All Buyouts Prior to 2010", "All Buyouts Prior to 2010","All Buyouts Prior to 2010",
              "Wishlist Prior to 2010","Wishlist Prior to 2010","Wishlist Prior to 2010")
  Scale<-c("County","Micro-watersheds with Buyouts","Buyout Parcels",
           "County","Micro-watersheds with Buyouts","Buyout Parcels",
           "County","Micro-watersheds with Buyouts","Buyout Parcels",
           "County","Micro-watersheds with Buyouts","Buyout Parcels")
  Damaged_Properties<-c(parcels_nb,parcels_ob,parcels_ab,parcels_w)
  Exposed_Population<-c(pop_nb,pop_ob,pop_ab,pop_w)
  Exposed_Senior_Population<-c(senpop_nb,senpop_ob,senpop_ab,senpop_w)
  Exposed_Renter_Population<-c(rentpop_nb,rentpop_ob,rentpop_ab,rentpop_w)
  
  df<-data.frame(Scenario, Scale, Damaged_Properties,Exposed_Population,
                 Exposed_Senior_Population,Exposed_Renter_Population)
  
  dt<-xtable(df)


  #d1<-p10_ob %>% group_by(ctgeoid10) %>% summarise(SenPop= max(SeniorPop, na.rm=T))
  #sum(d1$SenPop,na.rm=T)
  
  
  
  

##########################################################################################################################################
#Calculate Table summary statistics for Impervious Area, Runoff, and Riparian Characteristics in 2010 using Watershed-scale sf objects
########################################################################################################################################
subwsnames<-c("subwsnewparcels2005ob.RDS", "subwsnewparcels2006ob.RDS", "subwsnewparcels2007ob.RDS", "subwsnewparcels2008ob.RDS", "subwsnewparcels2009ob.RDS", 
              "subwsnewparcels2010ob.RDS", "subwsnewparcels2011ob.RDS", "subwsnewparcels2012ob.RDS", "subwsnewparcels2013ob.RDS","subwsnewparcels2014ob.RDS","subwsnewparcels2015ob.RDS",
              "subwsnewparcels2006nb.RDS", "subwsnewparcels2007nb.RDS", "subwsnewparcels2008nb.RDS", "subwsnewparcels2009nb.RDS", "subwsnewparcels2010nb.RDS", "subwsnewparcels2011nb.RDS",
              "subwsnewparcels2012nb.RDS", "subwsnewparcels2013nb.RDS","subwsnewparcels2014nb.RDS","subwsnewparcels2015nb.RDS",
              "subwsnewparcels2010ab.RDS", "subwsnewparcels2011ab.RDS","subwsnewparcels2012ab.RDS", "subwsnewparcels2013ab.RDS","subwsnewparcels2014ab.RDS","subwsnewparcels2015ab.RDS",
              "subwsnewparcels2010w.RDS", "subwsnewparcels2011w.RDS","subwsnewparcels2012w.RDS", "subwsnewparcels2013w.RDS","subwsnewparcels2014w.RDS","subwsnewparcels2015w.RDS")

s10_nb<-readRDS(paste0(wd, "final", subwsnames[16])) #2010 no buyouts
wish_wsid<-filter(s10_nb, bowishcnt > 0) 
wish_wsid<-wish_wsid$WS_ID

  #county 
  runoff_c<-sum(s10_nb$Vrunoff, na.rm=T)/133680.556
  imp_area_c<-sum(s10_nb$wsbldgarea, na.rm=T)*2.29568e-5
  perc_imp_c<-sum(s10_nb$wsbldgarea, na.rm=T)/sum(s10_nb$wsarea, na.rm=T)*100
  trees<-sum(s10_nb$treecnt, na.rm=T)
  vtrees<-sum(s10_nb$TreeV, na.rm=T)
  totV2<-sum(s10_nb$totVro,na.rm=T)/133680.556
  
  rip_area_c<-sum(s10_nb$rip_area, na.rm=T)*2.29568e-5
  rip_area2<-mean(s10_nb$rip_area, na.rm=T)*2.29568e-5
  rip_width_c<-mean(s10_nb$AveRW, na.rm=T)
  
  vtrees<-sum(s10_nb$TreeV, na.rm=T)/133680.556
  
  #watersheds with buyouts
  s10_nb_sub<-filter(s10_nb, WS_ID %in% wish_wsid)
  
  runoff_ws<-sum(s10_nb_sub$Vrunoff, na.rm=T)/133680.556
  imp_area_ws<-sum(s10_nb_sub$wsbldgarea, na.rm=T)*2.29568e-5
  perc_imp_ws<-mean(s10_nb_sub$percbld, na.rm=T)
  trees<-sum(s10_nb_sub$treecnt, na.rm=T)
  vtrees<-sum(s10_nb_sub$TreeV, na.rm=T)
  totV2<-sum(s10_nb_sub$totVro,na.rm=T)/133680.556
  
  rip_area_ws<-sum(s10_nb_sub$rip_area, na.rm=T)*2.29568e-5
  rip_area2<-mean(s10_nb_sub$rip_area, na.rm=T)*2.29568e-5
  rip_width_ws<-mean(s10_nb_sub$AveRW, na.rm=T)
  
  vtrees<-sum(s10_nb_sub$TreeV, na.rm=T)/133680.556
  
  #Vectors for tables
  IA_nb<-c(imp_area_c,imp_area_ws)
  PIA_nb<-c(perc_imp_c,perc_imp_ws)
  RV_nb<-c(runoff_c,runoff_ws)
  RA_nb<-c(rip_area_c, rip_area_ws)
  RW_nb<-c(rip_width_c,rip_width_ws)
  
  

s10_ob<-readRDS(paste0(wd, "final", subwsnames[6])) #2010 observed

  #county 
  runoff_c<-sum(s10_ob$Vrunoff, na.rm=T)/133680.556
  imp_area_c<-sum(s10_ob$wsbldgarea, na.rm=T)*2.29568e-5
  perc_imp_c<-sum(s10_ob$wsbldgarea, na.rm=T)/sum(s10_ob$wsarea, na.rm=T)*100
  trees<-sum(s10_ob$treecnt, na.rm=T)
  vtrees<-sum(s10_ob$TreeV, na.rm=T)
  totV2<-sum(s10_ob$totVro,na.rm=T)/133680.556
  
  rip_area_c<-sum(s10_ob$rip_area, na.rm=T)*2.29568e-5
  rip_area2<-mean(s10_ob$rip_area, na.rm=T)*2.29568e-5
  rip_width_c<-mean(s10_ob$AveRW, na.rm=T)
  
  vtrees<-sum(s10_ob$TreeV, na.rm=T)/133680.556
  
  # s10_ob$rip_area<-s10_ob$rip_area*2.29568e-5
  # plot(s10_ob[ ,"rip_area"], key.pos = 1)
  # s<-s10_ob[,c(33,34,37)]
  # plot(s[ ,c("AveRW")], key.pos = 1, breaks = c(0,50,100,150,200,250,300,350))
  # plot(s[ ,c("MinRW")], key.pos = 1, breaks = c(0,50,100,150,200,250,300,350))
  # plot(s10_ob[, "MinRW"])
  
  #watersheds with buyouts
  s10_ob_sub<-filter(s10_ob, WS_ID %in% wish_wsid)
  
  runoff_ws<-sum(s10_ob_sub$Vrunoff, na.rm=T)/133680.556
  imp_area_ws<-sum(s10_ob_sub$wsbldgarea, na.rm=T)*2.29568e-5
  perc_imp_ws<-mean(s10_ob_sub$percbld, na.rm=T)
  
  rip_area_ws<-sum(s10_ob_sub$rip_area, na.rm=T)*2.29568e-5
  rip_area2<-mean(s10_ob_sub$rip_area, na.rm=T)*2.29568e-5
  rip_width_ws<-mean(s10_ob_sub$AveRW, na.rm=T)
  
  vtrees<-sum(s10_ob_sub$TreeV, na.rm=T)/133680.556
  
  #Vectors for tables
  IA_ob<-c(imp_area_c,imp_area_ws)
  PIA_ob<-c(perc_imp_c,perc_imp_ws)
  RV_ob<-c(runoff_c,runoff_ws)
  RA_ob<-c(rip_area_c, rip_area_ws)
  RW_ob<-c(rip_width_c,rip_width_ws)


s10_ab<-readRDS(paste0(wd, "final", subwsnames[22])) #2010 all buyouts

  #county 
  runoff_c<-sum(s10_ab$Vrunoff, na.rm=T)/133680.556
  imp_area_c<-sum(s10_ab$wsbldgarea, na.rm=T)*2.29568e-5
  perc_imp_c<-sum(s10_ab$wsbldgarea, na.rm=T)/sum(s10_ab$wsarea, na.rm=T)*100
  trees<-sum(s10_ab$treecnt, na.rm=T)
  vtrees<-sum(s10_ab$TreeV, na.rm=T)
  totV2<-sum(s10_ab$totVro,na.rm=T)/133680.556
  
  rip_area_c<-sum(s10_ab$rip_area, na.rm=T)*2.29568e-5
  rip_area2<-mean(s10_ab$rip_area, na.rm=T)*2.29568e-5
  rip_width_c<-mean(s10_ab$AveRW, na.rm=T)
  
  vtrees<-sum(s10_ab$TreeV, na.rm=T)/133680.556
  
  #watersheds with buyouts
  s10_ab_sub<-filter(s10_ab, WS_ID %in% wish_wsid)
  
  runoff_ws<-sum(s10_ab_sub$Vrunoff, na.rm=T)/133680.556
  imp_area_ws<-sum(s10_ab_sub$wsbldgarea, na.rm=T)*2.29568e-5
  perc_imp_ws<-mean(s10_ab_sub$percbld, na.rm=T)
  
  rip_area_ws<-sum(s10_ab_sub$rip_area, na.rm=T)*2.29568e-5
  rip_area2<-mean(s10_ab_sub$rip_area, na.rm=T)*2.29568e-5
  rip_width_ws<-mean(s10_ab_sub$AveRW, na.rm=T)
  
  vtrees<-sum(s10_ab_sub$TreeV, na.rm=T)/133680.556
  
  #Vectors for tables
  IA_ab<-c(imp_area_c,imp_area_ws)
  PIA_ab<-c(perc_imp_c,perc_imp_ws)
  RV_ab<-c(runoff_c,runoff_ws)
  RA_ab<-c(rip_area_c, rip_area_ws)
  RW_ab<-c(rip_width_c,rip_width_ws)


s10_w<-readRDS(paste0(wd, "final", subwsnames[28])) #2010 wishlist

  #county 
  runoff_c<-sum(s10_w$Vrunoff, na.rm=T)/133680.556
  imp_area_c<-sum(s10_w$wsbldgarea, na.rm=T)*2.29568e-5
  perc_imp_c<-sum(s10_w$wsbldgarea, na.rm=T)/sum(s10_w$wsarea, na.rm=T)*100
  
  rip_area_c<-sum(s10_w$rip_area, na.rm=T)*2.29568e-5
  rip_area2<-mean(s10_w$rip_area, na.rm=T)*2.29568e-5
  rip_width_c<-mean(s10_w$AveRW, na.rm=T)
  
  vtrees<-sum(s10_w$TreeV, na.rm=T)/133680.556
  
  #watersheds with buyouts
  s10_w_sub<-filter(s10_w, WS_ID %in% wish_wsid)
  
  runoff_ws<-sum(s10_w_sub$Vrunoff, na.rm=T)/133680.556
  imp_area_ws<-sum(s10_w_sub$wsbldgarea, na.rm=T)*2.29568e-5
  perc_imp_ws<-mean(s10_w_sub$percbld, na.rm=T)
  
  rip_area_ws<-sum(s10_w_sub$rip_area, na.rm=T)*2.29568e-5
  rip_area2<-mean(s10_w_sub$rip_area, na.rm=T)*2.29568e-5
  rip_width_ws<-mean(s10_w_sub$AveRW, na.rm=T)
  
  vtrees<-sum(s10_w_sub$TreeV, na.rm=T)/133680.556
  
  #Vectors for tables
  IA_w<-c(imp_area_c,imp_area_ws)
  PIA_w<-c(perc_imp_c,perc_imp_ws)
  RV_w<-c(runoff_c,runoff_ws)
  RA_w<-c(rip_area_c, rip_area_ws)
  RW_w<-c(rip_width_c,rip_width_ws)

  
#Put together the tables
  Scenario<-c("No Buyouts","No Buyouts",
              "With Buyouts Prior to 2010","With Buyouts Prior to 2010",
              "All Buyouts Prior to 2010", "All Buyouts Prior to 2010",
              "Wishlist Prior to 2010","Wishlist Prior to 2010")
  Scale<-c("County","Micro-watersheds with Buyouts",
           "County","Micro-watersheds with Buyouts",
           "County","Micro-watersheds with Buyouts",
           "County","Micro-watersheds with Buyouts")
  Impervious_Building_Area<-c(IA_nb,IA_ob,IA_ab,IA_w)
  Percent_Impervious_Area<-c(PIA_nb,PIA_ob,PIA_ab,PIA_w)
  Runoff<-c(RV_nb,RV_ob,RV_ab,RV_w)
  
  df1<-data.frame(Scenario,Scale,Impervious_Building_Area,Percent_Impervious_Area,Runoff)
  
  Riparian_Area<-c(RA_nb,RA_ob,RA_ab,RA_w)
  Riparian_Width<-c(RW_nb,RW_ob,RW_ab,RW_w)
  
  df2<-data.frame(Scenario, Scale, Riparian_Area,Riparian_Width)
  
#####################################################################################################
#Time Series Summary Table/Plots for Riparian Characteristics 
################################################################################################
  
  #No buyouts 2005-2015
  year<-seq(2005,2015)
  scenario<-rep("No buyouts",length(year))
  df_nb<-data.frame(year,scenario)
  df_nb$Rip_area<-NA
  df_nb$Rip_width<-NA
  df_nb$Impervious_Area<-NA
  df_nb$Runoff<-NA
  
  names<-c("subwsnewparcels2005ob.RDS", "subwsnewparcels2006nb.RDS", "subwsnewparcels2007nb.RDS", "subwsnewparcels2008nb.RDS", 
        "subwsnewparcels2009nb.RDS", "subwsnewparcels2010nb.RDS", "subwsnewparcels2011nb.RDS",
        "subwsnewparcels2012nb.RDS", "subwsnewparcels2013nb.RDS","subwsnewparcels2014nb.RDS","subwsnewparcels2015nb.RDS")
  
  for (i in 1:11){ #records of interest for no-buyout scenario
      
      d<-readRDS(paste0(wd, "final", names[i]))
      
      rip_area<-sum(d$rip_area, na.rm=T)*2.29568e-5
      rip_width<-mean(d$AveRW, na.rm=T)
      imp_area<-sum(d$wsbldgarea, na.rm=T)*2.29568e-5
      runoff<-sum(d$Vrunoff, na.rm=T)/133680.556
      
      df_nb$Rip_area[i]<-rip_area
      df_nb$Rip_width[i]<-rip_width
      df_nb$Impervious_Area[i]<-imp_area
      df_nb$Runoff[i]<-runoff
      
  }
  
  #Observed Buyouts 2005-2015
  year<-seq(2005,2015)
  scenario<-rep("With buyouts",length(year))
  df_ob<-data.frame(year,scenario)
  df_ob$Rip_area<-NA
  df_ob$Rip_width<-NA
  df_ob$Impervious_Area<-NA
  df_ob$Runoff<-NA
  
  names<-c("subwsnewparcels2005ob.RDS", "subwsnewparcels2006ob.RDS", "subwsnewparcels2007ob.RDS", "subwsnewparcels2008ob.RDS", "subwsnewparcels2009ob.RDS", 
           "subwsnewparcels2010ob.RDS", "subwsnewparcels2011ob.RDS", "subwsnewparcels2012ob.RDS", "subwsnewparcels2013ob.RDS","subwsnewparcels2014ob.RDS",
           "subwsnewparcels2015ob.RDS")
  
  for (i in 1:11){ #records of interest for no-buyout scenario
    
    d<-readRDS(paste0(wd, "final", names[i]))
    
    rip_area<-sum(d$rip_area, na.rm=T)*2.29568e-5
    rip_width<-mean(d$AveRW, na.rm=T)
    imp_area<-sum(d$wsbldgarea, na.rm=T)*2.29568e-5
    runoff<-sum(d$Vrunoff, na.rm=T)/133680.556
    
    df_ob$Rip_area[i]<-rip_area
    df_ob$Rip_width[i]<-rip_width
    df_ob$Impervious_Area[i]<-imp_area
    df_ob$Runoff[i]<-runoff
    
  }
  
  
  #All Buyouts 2005-2015
  year<-seq(2005,2015)
  scenario<-rep("All buyouts",length(year))
  df_ab<-data.frame(year,scenario)
  df_ab$Rip_area<-NA
  df_ab$Rip_width<-NA
  df_ab$Impervious_Area<-NA
  df_ab$Runoff<-NA
  
  names<-c("subwsnewparcels2005ob.RDS", "subwsnewparcels2006ob.RDS", "subwsnewparcels2007ob.RDS", "subwsnewparcels2008ob.RDS", "subwsnewparcels2009ob.RDS", 
           "subwsnewparcels2010ab.RDS", "subwsnewparcels2011ab.RDS","subwsnewparcels2012ab.RDS", "subwsnewparcels2013ab.RDS","subwsnewparcels2014ab.RDS",
           "subwsnewparcels2015ab.RDS")
  
  for (i in 1:11){ #records of interest for no-buyout scenario
    
    d<-readRDS(paste0(wd, "final", names[i]))
    
    rip_area<-sum(d$rip_area, na.rm=T)*2.29568e-5
    rip_width<-mean(d$AveRW, na.rm=T)
    imp_area<-sum(d$wsbldgarea, na.rm=T)*2.29568e-5
    runoff<-sum(d$Vrunoff, na.rm=T)/133680.556
    
    df_ab$Rip_area[i]<-rip_area
    df_ab$Rip_width[i]<-rip_width
    df_ab$Impervious_Area[i]<-imp_area
    df_ab$Runoff[i]<-runoff
    
  }
  
  
  #Wishlist Buyouts 2005-2015
  year<-seq(2005,2015)
  scenario<-rep("Wishlist buyouts",length(year))
  df_w<-data.frame(year,scenario)
  df_w$Rip_area<-NA
  df_w$Rip_width<-NA
  df_w$Impervious_Area<-NA
  df_w$Runoff<-NA
  
  names<-c("subwsnewparcels2005ob.RDS", "subwsnewparcels2006ob.RDS", "subwsnewparcels2007ob.RDS", "subwsnewparcels2008ob.RDS", "subwsnewparcels2009ob.RDS", 
           "subwsnewparcels2010w.RDS", "subwsnewparcels2011w.RDS","subwsnewparcels2012w.RDS", "subwsnewparcels2013w.RDS","subwsnewparcels2014w.RDS",
           "subwsnewparcels2015w.RDS")
  
  for (i in 1:11){ #records of interest for no-buyout scenario
    
    d<-readRDS(paste0(wd, "final", names[i]))
    
    rip_area<-sum(d$rip_area, na.rm=T)*2.29568e-5
    rip_width<-mean(d$AveRW, na.rm=T)
    imp_area<-sum(d$wsbldgarea, na.rm=T)*2.29568e-5
    runoff<-sum(d$Vrunoff, na.rm=T)/133680.556
    
    df_w$Rip_area[i]<-rip_area
    df_w$Rip_width[i]<-rip_width
    df_w$Impervious_Area[i]<-imp_area
    df_w$Runoff[i]<-runoff
    
  }
  
  
  df_ts<-rbind(df_nb,df_ob,df_ab,df_w) #only use 2007-2013
  #Full time-series table
  df_ts<-rbind(df_nb[3:9,],df_ob[3:9,],df_ab[3:9,],df_w[3:9,]) #only use 2007-2013

  #Now build plot for Riparian Charactersitics
  
    p <- ggplot(df_ts, aes(x = year))
    p <- p + geom_line(aes(y = Rip_width, colour = "Width", group=scenario, linetype=scenario, size=scenario)) 
    
    # adding the relative humidity data, transformed to match roughly the range of the temperature
    scale.factor<-mean(df_ts$Rip_area, na.rm=T)/mean(df_ts$Rip_width,na.rm=T)
    p <- p + geom_line(aes(y = Rip_area/scale.factor, colour = "Area", group=scenario,linetype=scenario, size=scenario)) 
    
    # now adding the secondary axis, following the example in the help file ?scale_y_continuous
    # and, very important, reverting the above transformation
    p <- p + scale_y_continuous(sec.axis = sec_axis(~.*scale.factor, name = "Riparian Area (acres)"))
    
    # modifying colours and theme options
      p <- p + labs(y = "Riparian Width (feet)", x = "Year") + scale_linetype_manual(values=c("solid", "longdash", "dotted","dotdash")) + 
      scale_size_manual(values=c(1,1,1,1)) + scale_x_discrete(limits=c(2007,2010,2013))  
         
      p <- p + theme(legend.position = "right")
      p
    
  
      
    #Now build plot for Impervisouness and Runoff
    
    p <- ggplot(df_ts, aes(x = year))
    p <- p + geom_line(aes(y = Impervious_Area, colour = "Impervious Area",group=scenario, linetype=scenario, size=scenario)) + 
      scale_linetype_manual(values=c("solid", "longdash", "dotted","dotdash")) + scale_size_manual(values=c(1,1,1,1))+
      scale_x_discrete(limits=c(2007,2010,2013))
  
    
    # adding the relative humidity data, transformed to match roughly the range of the temperature
    scale.factor<-(mean(df_ts$Runoff, na.rm=T)-20000)/mean(df_ts$Impervious_Area,na.rm=T)
    p <- p + geom_line(aes(y = ((Runoff-20000)/scale.factor), colour = "Runoff",group=scenario, linetype=scenario, size=scenario)) + 
      scale_linetype_manual(values=c("solid", "longdash", "dotted","dotdash")) + scale_size_manual(values=c(1,1,1,1))
    
    
    # now adding the secondary axis, following the example in the help file ?scale_y_continuous
    # and, very important, reverting the above transformation
    p <- p + scale_y_continuous(sec.axis = sec_axis(~.*scale.factor+20000, name = "Runoff Volume (Million gallons)"))
    
    # modifying colours and theme options
      p <- p + labs(y = "Impervious Area (acres)",
                  x = "Year")
    p <- p + theme(legend.position = "right")
    p
  
#####################################################################################################
#Time Series Summary Table/Plots for  Impervisousness and Runoff
################################################################################################
  
  #No buyouts 2005-2015
  year<-seq(2005,2015)
  scenario<-rep("No buyouts",length(year))
  df_nb<-data.frame(year,scenario)
  df_nb$Impervious_Area<-NA
  df_nb$Runoff<-NA
  
  names<-c("pbwsGS2005ob.RDS","pbwsGS2006nb.RDS","pbwsGS2007nb.RDS","pbwsGS2008nb.RDS","pbwsGS2009nb.RDS","pbwsGS2010nb.RDS", 
           "pbwsGS2011nb.RDS","pbwsGS2012nb.RDS", "pbwsGS2013nb.RDS","pbwsGS2014nb.RDS","pbwsGS2015nb.RDS")
  
  for (i in 1:11){ #records of interest for no-buyout scenario
    
    d<-readRDS(paste0(wd, "newsubws", names[i]))
    
    imp_area<-sum(d$wsbldgarea, na.rm=T)*2.29568e-5
    runoff<-sum(d$Vrunoff, na.rm=T)/133680.556
    
    df_nb$Impervious_Area[i]<-imp_area
    df_nb$Runoff[i]<-runoff
    
  }
  
  #Observed Buyouts 2005-2015
  year<-seq(2005,2015)
  scenario<-rep("With buyouts",length(year))
  df_ob<-data.frame(year,scenario)
  df_ob$Impervious_Area<-NA
  df_ob$Runoff<-NA
  
  names<-c("pbwsGS2005ob.RDS", "pbwsGS2006ob.RDS", "pbwsGS2007ob.RDS", "pbwsGS2008ob.RDS", "pbwsGS2009ob.RDS", "pbwsGS2010ob.RDS", "pbwsGS2011ob.RDS",
           "pbwsGS2012ob.RDS", "pbwsGS2013ob.RDS","pbwsGS2014ob.RDS","pbwsGS2015ob.RDS")
  
  for (i in 1:11){ #records of interest for no-buyout scenario
    
    d<-readRDS(paste0(wd, "newsubws", names[i]))
    
    imp_area<-sum(d$wsbldgarea, na.rm=T)*2.29568e-5
    runoff<-sum(d$Vrunoff, na.rm=T)/133680.556
    
    df_ob$Impervious_Area[i]<-imp_area
    df_ob$Runoff[i]<-runoff
    
  }
  
  
  #All Buyouts 2005-2015
  year<-seq(2005,2015)
  scenario<-rep("All buyouts",length(year))
  df_ab<-data.frame(year,scenario)
  df_ab$Impervious_Area<-NA
  df_ab$Runoff<-NA
  
  names<-c("pbwsGS2005ob.RDS", "pbwsGS2006ob.RDS", "pbwsGS2007ob.RDS", "pbwsGS2008ob.RDS", "pbwsGS2009ob.RDS",  
           "pbwsGS2010ab.RDS", "pbwsGS2011ab.RDS","pbwsGS2012ab.RDS", "pbwsGS2013ab.RDS","pbwsGS2014ab.RDS","pbwsGS2015ab.RDS")
  
  for (i in 1:11){ #records of interest for no-buyout scenario
    
    d<-readRDS(paste0(wd, "newsubws", names[i]))
    
    imp_area<-sum(d$wsbldgarea, na.rm=T)*2.29568e-5
    runoff<-sum(d$Vrunoff, na.rm=T)/133680.556
    
    df_ab$Impervious_Area[i]<-imp_area
    df_ab$Runoff[i]<-runoff
    
  }
  
  
  #Wishlist Buyouts 2005-2015
  year<-seq(2005,2015)
  scenario<-rep("Wishlist buyouts",length(year))
  df_w<-data.frame(year,scenario)
  df_w$Impervious_Area<-NA
  df_w$Runoff<-NA
  
  names<-c("pbwsGS2005ob.RDS", "pbwsGS2006ob.RDS", "pbwsGS2007ob.RDS", "pbwsGS2008ob.RDS", "pbwsGS2009ob.RDS", 
           "pbwsGS2010w.RDS", "pbwsGS2011w.RDS","pbwsGS2012w.RDS", "pbwsGS2013w.RDS","pbwsGS2014w.RDS","pbwsGS2015w.RDS")
  
  for (i in 1:11){ #records of interest for no-buyout scenario
    
    d<-readRDS(paste0(wd, "newsubws", names[i]))
    
    imp_area<-sum(d$wsbldgarea, na.rm=T)*2.29568e-5
    runoff<-sum(d$Vrunoff, na.rm=T)/133680.556
    
    df_w$Impervious_Area[i]<-imp_area
    df_w$Runoff[i]<-runoff
    
  }
  
  
  df_ts<-rbind(df_nb,df_ob,df_ab,df_w) #only use 2007-2013
  #Full time-series table
  df_ts<-rbind(df_nb[3:9,],df_ob[3:9,],df_ab[3:9,],df_w[3:9,]) #only use 2007-2013
  #df_ts<-rbind(df_nb,df_ob,df_ab,df_w) #only use 2007-2013
  

  
  #Now build plot for Impervisouness and Runoff
  
  p <- ggplot(df_ts, aes(x = year))
  p <- p + geom_line(aes(y = Impervious_Area, colour = "Impervious Area",group=scenario, linetype=scenario, size=scenario)) + 
    scale_linetype_manual(values=c("solid", "longdash", "dotted","dotdash")) + scale_size_manual(values=c(1,1,1,1))+
    scale_x_discrete(limits=c(2007,2010,2013))
  
  
  # adding the relative humidity data, transformed to match roughly the range of the temperature
  scale.factor<-(mean(df_ts$Runoff, na.rm=T)-20000)/mean(df_ts$Impervious_Area,na.rm=T)
  p <- p + geom_line(aes(y = ((Runoff-20000)/scale.factor), colour = "Runoff",group=scenario, linetype=scenario, size=scenario)) + 
    scale_linetype_manual(values=c("solid", "longdash", "dotted","dotdash")) + scale_size_manual(values=c(1,1,1,1))
  
  
  # now adding the secondary axis, following the example in the help file ?scale_y_continuous
  # and, very important, reverting the above transformation
  p <- p + scale_y_continuous(sec.axis = sec_axis(~.*scale.factor+20000, name = "Runoff Volume (Million gallons)"))
  
  # modifying colours and theme options
  p <- p + labs(y = "Impervious Area (acres)",
                x = "Year")
  p <- p + theme(legend.position = "right")
  p

  
#####################################################################################################
#Time Series Summary Table/Plot for Damages
################################################################################################

#No buyouts 2005-2015
year<-seq(2007,2014)
scenario<-rep("No buyouts",length(year))
df_nb<-data.frame(year,scenario)
df_nb$StrucCont<-NA
df_nb$RelocLab-NA
df_nb$HpredSC<-NA
df_nb$HpredRL<-NA
df_nb$LpredSC<-NA
df_nb$LpredRL<-NA
df_nb$HS<-NA
df_nb$LS<-NA

names<-c("finalnewparcels2007nb.RDS", "finalnewparcels2008nb.RDS", "finalnewparcels2009nb.RDS", "finalnewparcels2010nb.RDS", "finalnewparcels2011nb.RDS",
         "finalnewparcels2012nb.RDS", "finalnewparcels2013nb.RDS","finalnewparcels2014nb.RDS")

pnames<-c("pred_depth_2007nb.RDS", "pred_depth_2010nb.RDS","pred_depth_2013nb.RDS")


for (i in 1:length(year)){ #records of interest for no-buyout scenario
  
  d<-readRDS(paste0(wd, "dasym3",names[i]))
  
  dmg_1<-round(sum(d$pDMGWDFIA, na.rm=T )/1000000,2) #structural damages
  cont_cnty<-round(sum(d$pDMGCONTWDFIA , na.rm=T )/1000000,2) #content damages
  df_nb$StrucCont[i]<-sum(dmg_1,cont_cnty)
  
  dmg_3<-round(sum(d$prelocWDHZ, na.rm=T )/1000000,2) #relocation costs
  labor_cnty<-round(sum(d$plaborWDSV, na.rm=T)/1000000,2)#labor costs
  df_nb$RelocLab[i]<-sum(dmg_3,labor_cnty)
   
  if (year[i]==2007){
   d2<- readRDS(paste0(wd, "highdamages", pnames[1]))
   d3<- readRDS(paste0(wd, "lowdamages", pnames[1]))
   
   Hd1<-round(sum(d2$DMG_WD_FIA, na.rm=T )/1000000,2) #structural damages
   Hcont<-round(sum(d2$DMGCONT_WD_FIA , na.rm=T )/1000000,2) #content damages
   df_nb$HpredSC[1]<-sum(Hd1,Hcont)
   df_nb$HS[1]<-Hd1
   
   Hd3<-round(sum(d2$Reloc_WD_HZ, na.rm=T )/1000000,2) #relocation costs
   Hlabor<-round(sum(d2$Labor_WD_SV, na.rm=T)/1000000,2)#labor costs
   df_nb$HpredRL[1]<-sum(Hd3,Hlabor)
   
   Ld1<-round(sum(d3$DMG_WD_FIA, na.rm=T )/1000000,2) #structural damages
   Lcont<-round(sum(d3$DMGCONT_WD_FIA , na.rm=T )/1000000,2) #content damages
   df_nb$LpredSC[1]<-sum(Ld1,Lcont)
   df_nb$LS[1]<-Ld1
   
   Ld3<-round(sum(d3$Reloc_WD_HZ, na.rm=T )/1000000,2) #relocation costs
   Llabor<-round(sum(d3$Labor_WD_SV, na.rm=T)/1000000,2)#labor costs
   df_nb$LpredRL[1]<-sum(Ld3,Llabor)
   
  }
  
  if (year[i]==2010){
    d2<- readRDS(paste0(wd, "highdamages", pnames[2]))
    d3<- readRDS(paste0(wd, "lowdamages", pnames[2]))
    
    Hd1<-round(sum(d2$DMG_WD_FIA, na.rm=T )/1000000,2) #structural damages
    Hcont<-round(sum(d2$DMGCONT_WD_FIA , na.rm=T )/1000000,2) #content damages
    df_nb$HpredSC[4]<-sum(Hd1,Hcont)
    df_nb$HS[4]<-Hd1
    
    Hd3<-round(sum(d2$Reloc_WD_HZ, na.rm=T )/1000000,2) #relocation costs
    Hlabor<-round(sum(d2$Labor_WD_SV, na.rm=T)/1000000,2)#labor costs
    df_nb$HpredRL[4]<-sum(Hd3,Hlabor)
    
    Ld1<-round(sum(d3$DMG_WD_FIA, na.rm=T )/1000000,2) #structural damages
    Lcont<-round(sum(d3$DMGCONT_WD_FIA , na.rm=T )/1000000,2) #content damages
    df_nb$LpredSC[4]<-sum(Ld1,Lcont)
    df_nb$LS[4]<-Ld1
    
    Ld3<-round(sum(d3$Reloc_WD_HZ, na.rm=T )/1000000,2) #relocation costs
    Llabor<-round(sum(d3$Labor_WD_SV, na.rm=T)/1000000,2)#labor costs
    df_nb$LpredRL[4]<-sum(Ld3,Llabor)
    
  }
  
  if (year[i]==2013){
    d2<- readRDS(paste0(wd, "highdamages", pnames[3]))
    d3<- readRDS(paste0(wd, "lowdamages", pnames[3]))
    
    Hd1<-round(sum(d2$DMG_WD_FIA, na.rm=T )/1000000,2) #structural damages
    Hcont<-round(sum(d2$DMGCONT_WD_FIA , na.rm=T )/1000000,2) #content damages
    df_nb$HpredSC[7]<-sum(Hd1,Hcont)
    df_nb$HS[7]<-Hd1
    
    Hd3<-round(sum(d2$Reloc_WD_HZ, na.rm=T )/1000000,2) #relocation costs
    Hlabor<-round(sum(d2$Labor_WD_SV, na.rm=T)/1000000,2)#labor costs
    df_nb$HpredRL[7]<-sum(Hd3,Hlabor)
    
    Ld1<-round(sum(d3$DMG_WD_FIA, na.rm=T )/1000000,2) #structural damages
    Lcont<-round(sum(d3$DMGCONT_WD_FIA , na.rm=T )/1000000,2) #content damages
    df_nb$LpredSC[7]<-sum(Ld1,Lcont)
    df_nb$LS[7]<-Ld1
    
    Ld3<-round(sum(d3$Reloc_WD_HZ, na.rm=T )/1000000,2) #relocation costs
    Llabor<-round(sum(d3$Labor_WD_SV, na.rm=T)/1000000,2)#labor costs
    df_nb$LpredRL[7]<-sum(Ld3,Llabor)
    
  }
}

#Observed Buyouts 2005-2015
year<-seq(2007,2014)
scenario<-rep("With buyouts",length(year))
df_ob<-data.frame(year,scenario)
df_ob$HpredSC<-NA
df_ob$HpredRL<-NA
df_ob$LpredSC<-NA
df_ob$LpredRL<-NA
df_ob$HS<-NA
df_ob$LS<-NA

names<-c("finalnewparcels2007ob.RDS", "finalnewparcels2008ob.RDS", "finalnewparcels2009ob.RDS", "finalnewparcels2010ob.RDS", "finalnewparcels2011ob.RDS",
         "finalnewparcels2012ob.RDS", "finalnewparcels2013ob.RDS","finalnewparcels2014ob.RDS")

pnames<-c("pred_depth_2007ob.RDS",
          "pred_depth_2010ob.RDS",
          "pred_depth_2013ob.RDS")

for (i in 1:length(year)){ #records of interest for no-buyout scenario
  
  d<-readRDS(paste0(wd, "dasym3",names[i]))
  
  dmg_1<-round(sum(d$pDMGWDFIA, na.rm=T )/1000000,2) #structural damages
  cont_cnty<-round(sum(d$pDMGCONTWDFIA , na.rm=T )/1000000,2) #content damages
  df_ob$StrucCont[i]<-sum(dmg_1,cont_cnty)
  
  dmg_3<-round(sum(d$prelocWDHZ, na.rm=T )/1000000,2) #relocation costs
  labor_cnty<-round(sum(d$plaborWDSV, na.rm=T)/1000000,2)#labor costs
  df_ob$RelocLab[i]<-sum(dmg_3,labor_cnty)
  
  if (year[i]==2007){
    d2<- readRDS(paste0(wd, "highdamages", pnames[1]))
    d3<- readRDS(paste0(wd, "lowdamages", pnames[1]))
    
    Hd1<-round(sum(d2$DMG_WD_FIA, na.rm=T )/1000000,2) #structural damages
    Hcont<-round(sum(d2$DMGCONT_WD_FIA , na.rm=T )/1000000,2) #content damages
    df_ob$HpredSC[1]<-sum(Hd1,Hcont)
    df_ob$HS[1]<-Hd1
    
    Hd3<-round(sum(d2$Reloc_WD_HZ, na.rm=T )/1000000,2) #relocation costs
    Hlabor<-round(sum(d2$Labor_WD_SV, na.rm=T)/1000000,2)#labor costs
    df_ob$HpredRL[1]<-sum(Hd3,Hlabor)
    
    Ld1<-round(sum(d3$DMG_WD_FIA, na.rm=T )/1000000,2) #structural damages
    Lcont<-round(sum(d3$DMGCONT_WD_FIA , na.rm=T )/1000000,2) #content damages
    df_ob$LpredSC[1]<-sum(Ld1,Lcont)
    df_ob$LS[1]<-Ld1
    
    Ld3<-round(sum(d3$Reloc_WD_HZ, na.rm=T )/1000000,2) #relocation costs
    Llabor<-round(sum(d3$Labor_WD_SV, na.rm=T)/1000000,2)#labor costs
    df_ob$LpredRL[1]<-sum(Ld3,Llabor)
    
  }
  
  if (year[i]==2010){
    d2<- readRDS(paste0(wd, "highdamages", pnames[2]))
    d3<- readRDS(paste0(wd, "lowdamages", pnames[2]))
    
    Hd1<-round(sum(d2$DMG_WD_FIA, na.rm=T )/1000000,2) #structural damages
    Hcont<-round(sum(d2$DMGCONT_WD_FIA , na.rm=T )/1000000,2) #content damages
    df_ob$HpredSC[4]<-sum(Hd1,Hcont)
    df_ob$HS[4]<-Hd1
    
    Hd3<-round(sum(d2$Reloc_WD_HZ, na.rm=T )/1000000,2) #relocation costs
    Hlabor<-round(sum(d2$Labor_WD_SV, na.rm=T)/1000000,2)#labor costs
    df_ob$HpredRL[4]<-sum(Hd3,Hlabor)
    
    Ld1<-round(sum(d3$DMG_WD_FIA, na.rm=T )/1000000,2) #structural damages
    Lcont<-round(sum(d3$DMGCONT_WD_FIA , na.rm=T )/1000000,2) #content damages
    df_ob$LpredSC[4]<-sum(Ld1,Lcont)
    df_ob$LS[4]<-Ld1
    
    Ld3<-round(sum(d3$Reloc_WD_HZ, na.rm=T )/1000000,2) #relocation costs
    Llabor<-round(sum(d3$Labor_WD_SV, na.rm=T)/1000000,2)#labor costs
    df_ob$LpredRL[4]<-sum(Ld3,Llabor)
    
  }
  
  if (year[i]==2013){
    d2<- readRDS(paste0(wd, "highdamages", pnames[3]))
    d3<- readRDS(paste0(wd, "lowdamages", pnames[3]))
    
    Hd1<-round(sum(d2$DMG_WD_FIA, na.rm=T )/1000000,2) #structural damages
    Hcont<-round(sum(d2$DMGCONT_WD_FIA , na.rm=T )/1000000,2) #content damages
    df_ob$HpredSC[7]<-sum(Hd1,Hcont)
    df_ob$HS[7]<-Hd1
    
    Hd3<-round(sum(d2$Reloc_WD_HZ, na.rm=T )/1000000,2) #relocation costs
    Hlabor<-round(sum(d2$Labor_WD_SV, na.rm=T)/1000000,2)#labor costs
    df_ob$HpredRL[7]<-sum(Hd3,Hlabor)
    
    Ld1<-round(sum(d3$DMG_WD_FIA, na.rm=T )/1000000,2) #structural damages
    Lcont<-round(sum(d3$DMGCONT_WD_FIA , na.rm=T )/1000000,2) #content damages
    df_ob$LpredSC[7]<-sum(Ld1,Lcont)
    df_ob$LS[7]<-Ld1
    
    Ld3<-round(sum(d3$Reloc_WD_HZ, na.rm=T )/1000000,2) #relocation costs
    Llabor<-round(sum(d3$Labor_WD_SV, na.rm=T)/1000000,2)#labor costs
    df_ob$LpredRL[7]<-sum(Ld3,Llabor)
    
  }
}


#All Buyouts 2005-2015
year<-seq(2007,2014)
scenario<-rep("All buyouts",length(year))
df_ab<-data.frame(year,scenario)
df_ab$StrucCont<-NA
df_ab$RelocLab-NA
df_ab$HpredSC<-NA
df_ab$HpredRL<-NA
df_ab$LpredSC<-NA
df_ab$LpredRL<-NA
df_ab$HS<-NA
df_ab$LS<-NA

names<-c("finalnewparcels2007ob.RDS", "finalnewparcels2008ob.RDS", "finalnewparcels2009ob.RDS", 
         "finalnewparcels2010ab.RDS", "finalnewparcels2011ab.RDS","finalnewparcels2012ab.RDS", "finalnewparcels2013ab.RDS","finalnewparcels2014ab.RDS")

pnames<-c("pred_depth_2007ab.RDS",
          "pred_depth_2010ab.RDS",
          "pred_depth_2013ab.RDS")

for (i in 1:length(year)){ #records of interest for no-buyout scenario
  
  d<-readRDS(paste0(wd, "dasym3",names[i]))
  
  dmg_1<-round(sum(d$pDMGWDFIA, na.rm=T )/1000000,2) #structural damages
  cont_cnty<-round(sum(d$pDMGCONTWDFIA , na.rm=T )/1000000,2) #content damages
  df_ab$StrucCont[i]<-sum(dmg_1,cont_cnty)
  
  dmg_3<-round(sum(d$prelocWDHZ, na.rm=T )/1000000,2) #relocation costs
  labor_cnty<-round(sum(d$plaborWDSV, na.rm=T)/1000000,2)#labor costs
  df_ab$RelocLab[i]<-sum(dmg_3,labor_cnty)
  
  if (year[i]==2007){
    d2<- readRDS(paste0(wd, "highdamages", pnames[1]))
    d3<- readRDS(paste0(wd, "lowdamages", pnames[1]))
    
    Hd1<-round(sum(d2$DMG_WD_FIA, na.rm=T )/1000000,2) #structural damages
    Hcont<-round(sum(d2$DMGCONT_WD_FIA , na.rm=T )/1000000,2) #content damages
    df_ab$HpredSC[1]<-sum(Hd1,Hcont)
    df_ab$HS[1]<-Hd1
    
    Hd3<-round(sum(d2$Reloc_WD_HZ, na.rm=T )/1000000,2) #relocation costs
    Hlabor<-round(sum(d2$Labor_WD_SV, na.rm=T)/1000000,2)#labor costs
    df_ab$HpredRL[1]<-sum(Hd3,Hlabor)
    
    Ld1<-round(sum(d3$DMG_WD_FIA, na.rm=T )/1000000,2) #structural damages
    Lcont<-round(sum(d3$DMGCONT_WD_FIA , na.rm=T )/1000000,2) #content damages
    df_ab$LpredSC[1]<-sum(Ld1,Lcont)
    df_ab$LS[1]<-Ld1
    
    Ld3<-round(sum(d3$Reloc_WD_HZ, na.rm=T )/1000000,2) #relocation costs
    Llabor<-round(sum(d3$Labor_WD_SV, na.rm=T)/1000000,2)#labor costs
    df_ab$LpredRL[1]<-sum(Ld3,Llabor)
    
  }
  
  if (year[i]==2010){
    d2<- readRDS(paste0(wd, "highdamages", pnames[2]))
    d3<- readRDS(paste0(wd, "lowdamages", pnames[2]))
    
    Hd1<-round(sum(d2$DMG_WD_FIA, na.rm=T )/1000000,2) #structural damages
    Hcont<-round(sum(d2$DMGCONT_WD_FIA , na.rm=T )/1000000,2) #content damages
    df_ab$HpredSC[4]<-sum(Hd1,Hcont)
    df_ab$HS[4]<-Hd1
    
    Hd3<-round(sum(d2$Reloc_WD_HZ, na.rm=T )/1000000,2) #relocation costs
    Hlabor<-round(sum(d2$Labor_WD_SV, na.rm=T)/1000000,2)#labor costs
    df_ab$HpredRL[4]<-sum(Hd3,Hlabor)
    
    Ld1<-round(sum(d3$DMG_WD_FIA, na.rm=T )/1000000,2) #structural damages
    Lcont<-round(sum(d3$DMGCONT_WD_FIA , na.rm=T )/1000000,2) #content damages
    df_ab$LpredSC[4]<-sum(Ld1,Lcont)
    df_ab$LS[4]<-Ld1
    
    Ld3<-round(sum(d3$Reloc_WD_HZ, na.rm=T )/1000000,2) #relocation costs
    Llabor<-round(sum(d3$Labor_WD_SV, na.rm=T)/1000000,2)#labor costs
    df_ab$LpredRL[4]<-sum(Ld3,Llabor)
    
  }
  
  if (year[i]==2013){
    d2<- readRDS(paste0(wd, "highdamages", pnames[3]))
    d3<- readRDS(paste0(wd, "lowdamages", pnames[3]))
    
    Hd1<-round(sum(d2$DMG_WD_FIA, na.rm=T )/1000000,2) #structural damages
    Hcont<-round(sum(d2$DMGCONT_WD_FIA , na.rm=T )/1000000,2) #content damages
    df_ab$HpredSC[7]<-sum(Hd1,Hcont)
    df_ab$HS[7]<-Hd1
    
    Hd3<-round(sum(d2$Reloc_WD_HZ, na.rm=T )/1000000,2) #relocation costs
    Hlabor<-round(sum(d2$Labor_WD_SV, na.rm=T)/1000000,2)#labor costs
    df_ab$HpredRL[7]<-sum(Hd3,Hlabor)
    
    Ld1<-round(sum(d3$DMG_WD_FIA, na.rm=T )/1000000,2) #structural damages
    Lcont<-round(sum(d3$DMGCONT_WD_FIA , na.rm=T )/1000000,2) #content damages
    df_ab$LpredSC[7]<-sum(Ld1,Lcont)
    df_ab$LS[7]<-Ld1
    
    Ld3<-round(sum(d3$Reloc_WD_HZ, na.rm=T )/1000000,2) #relocation costs
    Llabor<-round(sum(d3$Labor_WD_SV, na.rm=T)/1000000,2)#labor costs
    df_ab$LpredRL[7]<-sum(Ld3,Llabor)
    
  }
}


#Wishlist Buyouts 2005-2015
year<-seq(2007,2014)
scenario<-rep("Wishlist buyouts",length(year))
df_w<-data.frame(year,scenario)
df_w$StrucCont<-NA
df_w$RelocLab-NA
df_w$HpredSC<-NA
df_w$HpredRL<-NA
df_w$LpredSC<-NA
df_w$LpredRL<-NA
df_w$HS<-NA
df_w$LS<-NA

names<-c("finalnewparcels2007ob.RDS", "finalnewparcels2008ob.RDS", "finalnewparcels2009ob.RDS", 
         "finalnewparcels2010w.RDS", "finalnewparcels2011w.RDS","finalnewparcels2012w.RDS", "finalnewparcels2013w.RDS","finalnewparcels2014w.RDS")

pnames<-c("pred_depth_2007w.RDS",
          "pred_depth_2010w.RDS",
          "pred_depth_2013w.RDS")

for (i in 1:length(year)){ #records of interest for no-buyout scenario
  
  d<-readRDS(paste0(wd, "dasym3", names[i]))
  
  dmg_1<-round(sum(d$pDMGWDFIA, na.rm=T )/1000000,2) #structural damages
  cont_cnty<-round(sum(d$pDMGCONTWDFIA , na.rm=T )/1000000,2) #content damages
  df_w$StrucCont[i]<-sum(dmg_1,cont_cnty)
  
  dmg_3<-round(sum(d$prelocWDHZ, na.rm=T )/1000000,2) #relocation costs
  labor_cnty<-round(sum(d$plaborWDSV, na.rm=T)/1000000,2)#labor costs
  df_w$RelocLab[i]<-sum(dmg_3,labor_cnty)
  
  if (year[i]==2007){
    d2<- readRDS(paste0(wd, "highdamages", pnames[1]))
    d3<- readRDS(paste0(wd, "lowdamages", pnames[1]))
    
    Hd1<-round(sum(d2$DMG_WD_FIA, na.rm=T )/1000000,2) #structural damages
    Hcont<-round(sum(d2$DMGCONT_WD_FIA , na.rm=T )/1000000,2) #content damages
    df_w$HpredSC[1]<-sum(Hd1,Hcont)
    df_w$HS[1]<-Hd1
    
    Hd3<-round(sum(d2$Reloc_WD_HZ, na.rm=T )/1000000,2) #relocation costs
    Hlabor<-round(sum(d2$Labor_WD_SV, na.rm=T)/1000000,2)#labor costs
    df_w$HpredRL[1]<-sum(Hd3,Hlabor)
    
    Ld1<-round(sum(d3$DMG_WD_FIA, na.rm=T )/1000000,2) #structural damages
    Lcont<-round(sum(d3$DMGCONT_WD_FIA , na.rm=T )/1000000,2) #content damages
    df_w$LpredSC[1]<-sum(Ld1,Lcont)
    df_w$LS[1]<-Ld1
    
    Ld3<-round(sum(d3$Reloc_WD_HZ, na.rm=T )/1000000,2) #relocation costs
    Llabor<-round(sum(d3$Labor_WD_SV, na.rm=T)/1000000,2)#labor costs
    df_w$LpredRL[1]<-sum(Ld3,Llabor)
    
  }
  
  if (year[i]==2010){
    d2<- readRDS(paste0(wd, "highdamages", pnames[2]))
    d3<- readRDS(paste0(wd, "lowdamages", pnames[2]))
    
    Hd1<-round(sum(d2$DMG_WD_FIA, na.rm=T )/1000000,2) #structural damages
    Hcont<-round(sum(d2$DMGCONT_WD_FIA , na.rm=T )/1000000,2) #content damages
    df_w$HpredSC[4]<-sum(Hd1,Hcont)
    df_w$HS[4]<-Hd1
    
    Hd3<-round(sum(d2$Reloc_WD_HZ, na.rm=T )/1000000,2) #relocation costs
    Hlabor<-round(sum(d2$Labor_WD_SV, na.rm=T)/1000000,2)#labor costs
    df_w$HpredRL[4]<-sum(Hd3,Hlabor)
    
    Ld1<-round(sum(d3$DMG_WD_FIA, na.rm=T )/1000000,2) #structural damages
    Lcont<-round(sum(d3$DMGCONT_WD_FIA , na.rm=T )/1000000,2) #content damages
    df_w$LpredSC[4]<-sum(Ld1,Lcont)
    df_w$LS[4]<-Ld1
    
    Ld3<-round(sum(d3$Reloc_WD_HZ, na.rm=T )/1000000,2) #relocation costs
    Llabor<-round(sum(d3$Labor_WD_SV, na.rm=T)/1000000,2)#labor costs
    df_w$LpredRL[4]<-sum(Ld3,Llabor)
    
  }
  
  if (year[i]==2013){
    d2<- readRDS(paste0(wd, "highdamages", pnames[3]))
    d3<- readRDS(paste0(wd, "lowdamages", pnames[3]))
    
    Hd1<-round(sum(d2$DMG_WD_FIA, na.rm=T )/1000000,2) #structural damages
    Hcont<-round(sum(d2$DMGCONT_WD_FIA , na.rm=T )/1000000,2) #content damages
    df_w$HpredSC[7]<-sum(Hd1,Hcont)
    df_w$HS[7]<-Hd1
    
    Hd3<-round(sum(d2$Reloc_WD_HZ, na.rm=T )/1000000,2) #relocation costs
    Hlabor<-round(sum(d2$Labor_WD_SV, na.rm=T)/1000000,2)#labor costs
    df_w$HpredRL[7]<-sum(Hd3,Hlabor)
    
    Ld1<-round(sum(d3$DMG_WD_FIA, na.rm=T )/1000000,2) #structural damages
    Lcont<-round(sum(d3$DMGCONT_WD_FIA , na.rm=T )/1000000,2) #content damages
    df_w$LpredSC[7]<-sum(Ld1,Lcont)
    df_w$LS[7]<-Ld1
    
    Ld3<-round(sum(d3$Reloc_WD_HZ, na.rm=T )/1000000,2) #relocation costs
    Llabor<-round(sum(d3$Labor_WD_SV, na.rm=T)/1000000,2)#labor costs
    df_w$LpredRL[7]<-sum(Ld3,Llabor)
    
  }
  
}



#Full time-series table
df_ts<-rbind(df_nb[1:7,],df_ob[1:7,],df_ab[1:7,],df_w[1:7,])#for years 2007-2013


#Now build plot for Damages

    p <- ggplot(df_ts, aes(x = year))
    p <- p + geom_line(aes(y = StrucCont, colour = "Structural and Contents Damage", group=scenario, linetype=scenario, size=scenario)) 
    
    # adding the relative humidity data, transformed to match roughly the range of the temperature
    scale.factor<-mean(df_ts$StrucCont, na.rm=T)/mean(df_ts$RelocLab,na.rm=T)
    p <- p + geom_line(aes(y = RelocLab*scale.factor, colour = "Relocation and Labor Costs", group=scenario,linetype=scenario, size=scenario)) 
    
    # now adding the secondary axis, following the example in the help file ?scale_y_continuous
    # and, very important, reverting the above transformation
    p <- p + scale_y_continuous(sec.axis = sec_axis(~./scale.factor, name = "Relocation and Labor Costs ($M)"))
    
    # modifying colours and theme options
    p <- p + labs(y = "Structural and Contents Damages ($M)", x = "Year") + scale_linetype_manual(values=c("solid", "longdash", "dotted","dotdash")) + 
      scale_size_manual(values=c(1,1,1,1)) + scale_x_discrete(limits=c(2007,2010,2013))  
    
    
    p <- p + theme(legend.position = "right")
    p

#Now build plot for Structural Damages with Predicted Bounds

    df_ts2<-df_ts[-c(2:3,5:6,9:10,12:13,16:17,19:20,23:24,26:27),]
    df_ts2$Htot<-df_ts2$HpredRL +df_ts2$HpredSC
    df_ts2$Ltot<-df_ts2$LpredRL+df_ts2$LpredSC
    df_ts2$Origtot<-df_ts2$StrucCont+df_ts2$RelocLab
    
    p <- ggplot(df_ts2, aes(x = year))
    p <- p + geom_ribbon(aes(ymin=Ltot,ymax=Htot, fill=scenario),alpha=0.5)
           
    
    # adding the relative humidity data, transformed to match roughly the range of the temperature
    scale.factor<-df_ts2$Origtot[1]/ df_ts2$Htot[1]
    p <- p + geom_line(aes(y = Origtot/scale.factor -500,  group=scenario,linetype=scenario, size=scenario)) 
    
    # now adding the secondary axis, following the example in the help file ?scale_y_continuous
    # and, very important, reverting the above transformation
    p <- p + scale_y_continuous(sec.axis = sec_axis((~.*scale.factor+500), name = "Original Estimated Damages ($M)"))
    
    # modifying colours and theme options
    p <- p + labs(y = "Predicted Damages ($M)", x = "Year") + scale_linetype_manual(values=c("solid", "longdash", "dotted","dotdash")) + 
      scale_size_manual(values=c(1,1,1,1)) + scale_x_discrete(limits=c(2007,2010,2013))  
    
    
    p <- p + theme(legend.position = "right")#+ ylim(1400,2000)
    p



#####################################################################################################
#Time Series Summary Table/Plots for Damage Counts and Exposed Population
################################################################################################

pnames<-c( "finalnewparcels2007ob.RDS", "finalnewparcels2008ob.RDS", "finalnewparcels2009ob.RDS", "finalnewparcels2010ob.RDS", "finalnewparcels2011ob.RDS",
           "finalnewparcels2012ob.RDS", "finalnewparcels2013ob.RDS","finalnewparcels2014ob.RDS", 
           "finalnewparcels2007nb.RDS", "finalnewparcels2008nb.RDS", "finalnewparcels2009nb.RDS", "finalnewparcels2010nb.RDS", "finalnewparcels2011nb.RDS",
           "finalnewparcels2012nb.RDS", "finalnewparcels2013nb.RDS","finalnewparcels2014nb.RDS",
           "finalnewparcels2010ab.RDS", "finalnewparcels2011ab.RDS","finalnewparcels2012ab.RDS", "finalnewparcels2013ab.RDS","finalnewparcels2014ab.RDS",
           "finalnewparcels2010w.RDS", "finalnewparcels2011w.RDS","finalnewparcels2012w.RDS", "finalnewparcels2013w.RDS","finalnewparcels2014w.RDS") 

#No buyouts 2007-2015
year<-seq(2007,2014)
scenario<-rep("No buyouts",length(year))
df_nb<-data.frame(year,scenario)
df_nb$Parcels<-NA
df_nb$Pop<-NA
df_nb$Sendi<-NA
df_nb$Sen<-NA
df_nb$Rentdi<-NA
df_nb$Rent<-NA

names<-c("finalnewparcels2007nb.RDS", "finalnewparcels2008nb.RDS", "finalnewparcels2009nb.RDS", "finalnewparcels2010nb.RDS", "finalnewparcels2011nb.RDS",
         "finalnewparcels2012nb.RDS", "finalnewparcels2013nb.RDS","finalnewparcels2014nb.RDS")


for (i in 1:length(year)){ #records of interest for no-buyout scenario
  
  d<-readRDS(paste0(wd,"dasym3", names[i]))
  
  pdicnt<-length(d$STANPAR[!is.na(d$DLEVEL)| !is.na(d$inundate) | !is.na(d$buyout)]) #parcels damaged or inundated
  
  pop_cnt<-sum(d$P.new, na.rm=T) #population 
  pop_di<-sum(d$P.new[!is.na(d$DLEVEL)| !is.na(d$inundate)], na.rm=T) #population exposed
  
  senpop_cnt<-sum(d$P.newsenior,na.rm=T) #senior Pop
  percsenpop<-senpop_cnt/pop_cnt #percent pop that are senior citizens
  senpop_di<-sum(d$P.newsenior[!is.na(d$DLEVEL)| !is.na(d$inundate)], na.rm=T)#exposed senior pop
  percsenpopdi<-senpop_di/pop_di #percent of exposed pop that are senior citizens
  
  rentpop_cnt<-sum(d$P.newrent,na.rm=T)
  percrentpop<-rentpop_cnt/pop_cnt #percent pop that are renters
  rentpop_di<-sum(d$P.newrent[!is.na(d$DLEVEL)| !is.na(d$inundate)], na.rm=T)#exposed renter pop
  rentpercpopdi<-rentpop_di/pop_di #percent exposed pop that are renters
  rentpop_cnty<-rentpop_di
  
  df_nb$Parcels[i]<-pdicnt
  df_nb$Pop[i]<-pop_di
  df_nb$Sendi[i]<-percsenpopdi
  df_nb$Sen[i]<-percsenpop
  df_nb$Rentdi[i]<-rentpercpopdi
  df_nb$Rent[i]<-percrentpop
  
  
}

#Observed Buyouts 2005-2015
year<-seq(2007,2014)
scenario<-rep("With buyouts",length(year))
df_ob<-data.frame(year,scenario)
df_ob$Parcels<-NA
df_ob$Pop<-NA
df_ob$Sendi<-NA
df_ob$Sen<-NA
df_ob$Rentdi<-NA
df_ob$Rent<-NA

names<-c("finalnewparcels2007ob.RDS", "finalnewparcels2008ob.RDS", "finalnewparcels2009ob.RDS", "finalnewparcels2010ob.RDS", "finalnewparcels2011ob.RDS",
         "finalnewparcels2012ob.RDS", "finalnewparcels2013ob.RDS","finalnewparcels2014ob.RDS")

for (i in 1:length(year)){ #records of interest for no-buyout scenario
  
  d<-readRDS(paste0(wd,"dasym3", names[i]))
  
  pdicnt<-length(d$STANPAR[!is.na(d$DLEVEL)| !is.na(d$inundate) | !is.na(d$buyout)]) #parcels damaged or inundated
  
  pop_cnt<-sum(d$P.new, na.rm=T) #population 
  pop_di<-sum(d$P.new[!is.na(d$DLEVEL)| !is.na(d$inundate)], na.rm=T) #population exposed
  
  senpop_cnt<-sum(d$P.newsenior,na.rm=T) #senior Pop
  percsenpop<-senpop_cnt/pop_cnt #percent pop that are senior citizens
  senpop_di<-sum(d$P.newsenior[!is.na(d$DLEVEL)| !is.na(d$inundate)], na.rm=T)#exposed senior pop
  percsenpopdi<-senpop_di/pop_di #percent of exposed pop that are senior citizens
  
  rentpop_cnt<-sum(d$P.newrent,na.rm=T)
  percrentpop<-rentpop_cnt/pop_cnt #percent pop that are renters
  rentpop_di<-sum(d$P.newrent[!is.na(d$DLEVEL)| !is.na(d$inundate)], na.rm=T)#exposed renter pop
  rentpercpopdi<-rentpop_di/pop_di #percent exposed pop that are renters
  rentpop_cnty<-rentpop_di
  
  df_ob$Parcels[i]<-pdicnt
  df_ob$Pop[i]<-pop_di
  df_ob$Sendi[i]<-percsenpopdi
  df_ob$Sen[i]<-percsenpop
  df_ob$Rentdi[i]<-rentpercpopdi
  df_ob$Rent[i]<-percrentpop
  
}


#All Buyouts 2005-2015
year<-seq(2007,2014)
scenario<-rep("All buyouts",length(year))
df_ab<-data.frame(year,scenario)
df_ab$Parcels<-NA
df_ab$Pop<-NA
df_ab$Sendi<-NA
df_ab$Sen<-NA
df_ab$Rentdi<-NA
df_ab$Rent<-NA

names<-c("finalnewparcels2007ob.RDS", "finalnewparcels2008ob.RDS", "finalnewparcels2009ob.RDS", 
         "finalnewparcels2010ab.RDS", "finalnewparcels2011ab.RDS","finalnewparcels2012ab.RDS", "finalnewparcels2013ab.RDS","finalnewparcels2014ab.RDS")

for (i in 1:length(year)){ #records of interest for no-buyout scenario
  
  d<-readRDS(paste0(wd,"dasym3", names[i]))
  
  pdicnt<-length(d$STANPAR[!is.na(d$DLEVEL)| !is.na(d$inundate) | !is.na(d$buyout)]) #parcels damaged or inundated
  
  pop_cnt<-sum(d$P.new, na.rm=T) #population 
  pop_di<-sum(d$P.new[!is.na(d$DLEVEL)| !is.na(d$inundate)], na.rm=T) #population exposed
  
  senpop_cnt<-sum(d$P.newsenior,na.rm=T) #senior Pop
  percsenpop<-senpop_cnt/pop_cnt #percent pop that are senior citizens
  senpop_di<-sum(d$P.newsenior[!is.na(d$DLEVEL)| !is.na(d$inundate)], na.rm=T)#exposed senior pop
  percsenpopdi<-senpop_di/pop_di #percent of exposed pop that are senior citizens
  
  rentpop_cnt<-sum(d$P.newrent,na.rm=T)
  percrentpop<-rentpop_cnt/pop_cnt #percent pop that are renters
  rentpop_di<-sum(d$P.newrent[!is.na(d$DLEVEL)| !is.na(d$inundate)], na.rm=T)#exposed renter pop
  rentpercpopdi<-rentpop_di/pop_di #percent exposed pop that are renters
  rentpop_cnty<-rentpop_di
  
  df_ab$Parcels[i]<-pdicnt
  df_ab$Pop[i]<-pop_di
  df_ab$Sendi[i]<-percsenpopdi
  df_ab$Sen[i]<-percsenpop
  df_ab$Rentdi[i]<-rentpercpopdi
  df_ab$Rent[i]<-percrentpop
  
}


#Wishlist Buyouts 2005-2015
year<-seq(2007,2014)
scenario<-rep("Wishlist buyouts",length(year))
df_w<-data.frame(year,scenario)
df_w$Parcels<-NA
df_w$Pop<-NA
df_w$Sendi<-NA
df_w$Sen<-NA
df_w$Rentdi<-NA
df_w$Rent<-NA

names<-c("finalnewparcels2007ob.RDS", "finalnewparcels2008ob.RDS", "finalnewparcels2009ob.RDS", 
         "finalnewparcels2010w.RDS", "finalnewparcels2011w.RDS","finalnewparcels2012w.RDS", "finalnewparcels2013w.RDS","finalnewparcels2014w.RDS")

for (i in 1:length(year)){ #records of interest for no-buyout scenario
  
  d<-readRDS(paste0(wd, "dasym3", names[i]))
  
  pdicnt<-length(d$STANPAR[!is.na(d$DLEVEL)| !is.na(d$inundate) | !is.na(d$buyout)]) #parcels damaged or inundated
  
  pop_cnt<-sum(d$P.new, na.rm=T) #population 
  pop_di<-sum(d$P.new[!is.na(d$DLEVEL)| !is.na(d$inundate)], na.rm=T) #population exposed
  
  senpop_cnt<-sum(d$P.newsenior,na.rm=T) #senior Pop
  percsenpop<-senpop_cnt/pop_cnt #percent pop that are senior citizens
  senpop_di<-sum(d$P.newsenior[!is.na(d$DLEVEL)| !is.na(d$inundate)], na.rm=T)#exposed senior pop
  percsenpopdi<-senpop_di/pop_di #percent of exposed pop that are senior citizens
  
  rentpop_cnt<-sum(d$P.newrent,na.rm=T)
  percrentpop<-rentpop_cnt/pop_cnt #percent pop that are renters
  rentpop_di<-sum(d$P.newrent[!is.na(d$DLEVEL)| !is.na(d$inundate)], na.rm=T)#exposed renter pop
  rentpercpopdi<-rentpop_di/pop_di #percent exposed pop that are renters
  rentpop_cnty<-rentpop_di
  
  df_w$Parcels[i]<-pdicnt
  df_w$Pop[i]<-pop_di
  df_w$Sendi[i]<-percsenpopdi
  df_w$Sen[i]<-percsenpop
  df_w$Rentdi[i]<-rentpercpopdi
  df_w$Rent[i]<-percrentpop
  
}


df_ts<-rbind(df_nb[,],df_ob[,],df_ab[,],df_w[,])
#Full time-series table
df_ts<-rbind(df_nb[1:7,],df_ob[1:7,],df_ab[1:7,],df_w[1:7,])


#Now build plot for PArcel and Pop counts

p <- ggplot(df_ts, aes(x = year))
p <- p + geom_line(aes(y = Parcels, colour = "Parcels Damaged or Inundated", group=scenario, linetype=scenario, size=scenario)) 

# adding the relative humidity data, transformed to match roughly the range of the temperature
scale.factor<-mean(df_ts$Parcels, na.rm=T)/mean(df_ts$Pop,na.rm=T)
p <- p + geom_line(aes(y = Pop*scale.factor, colour = "Population Exposed", group=scenario,linetype=scenario, size=scenario)) 

# now adding the secondary axis, following the example in the help file ?scale_y_continuous
# and, very important, reverting the above transformation
p <- p + scale_y_continuous(sec.axis = sec_axis(~./scale.factor, name = "Population Exposed"))

# modifying colours and theme options
p <- p + labs(y = "Parcels Damaged or Inundated", x = "Year") + scale_linetype_manual(values=c("solid", "longdash", "dotted","dotdash")) + 
  scale_size_manual(values=c(1,1,1,1)) + scale_x_discrete(limits=c(2007,2010,2013))  


p <- p + theme(legend.position = "right")
p


#Now build plot for Senior and Rent percents

p <- ggplot(df_ts, aes(x = year))
p <- p + geom_line(aes(y = Sendi, colour = "Percent Exposed Population Senior Citizens", group=scenario, linetype=scenario, size=scenario)) + 
        geom_line(aes(y = Sen, colour = "Percent Total Population Senior Citizens", group=scenario, linetype=scenario, size=scenario)) 

# adding the relative humidity data, transformed to match roughly the range of the temperature
scale.factor<-mean(df_ts$Sen, na.rm=T)/mean(df_ts$Rent,na.rm=T)
p <- p + geom_line(aes(y = Rentdi*scale.factor, colour = "Percent Exposed Population Renters", group=scenario,linetype=scenario, size=scenario)) +
      geom_line(aes(y = Rent*scale.factor, colour = "Percent Total Population Renters", group=scenario,linetype=scenario, size=scenario)) 

# now adding the secondary axis, following the example in the help file ?scale_y_continuous
# and, very important, reverting the above transformation
p <- p + scale_y_continuous(sec.axis = sec_axis(~./scale.factor, name = "Percent Renters"))

# modifying colours and theme options
p <- p + labs(y = "Percent Senior Citizens", x = "Year") + scale_linetype_manual(values=c("solid", "longdash", "dotted","dotdash")) + 
  scale_size_manual(values=c(1,1,1,1)) + scale_x_discrete(limits=c(2007,2010,2013))  


p <- p + theme(legend.position = "right")
p


#####################################################################################################
#Time Series Summary Table/Plot for Property Tax, Property Value, and Property Revenue
################################################################################################

#No buyouts 2005-2015
year<-seq(2007,2014)
scenario<-rep("No buyouts",length(year))
df_nb<-data.frame(year,scenario)
df_nb$ptax<-NA
df_nb$pval<-NA
df_nb$ploss<-NA
df_nb$netrev<-NA

names<-c("finalnewparcels2007nb.RDS", "finalnewparcels2008nb.RDS", "finalnewparcels2009nb.RDS", "finalnewparcels2010nb.RDS", "finalnewparcels2011nb.RDS",
         "finalnewparcels2012nb.RDS", "finalnewparcels2013nb.RDS","finalnewparcels2014nb.RDS")

rate<-c(4.69,4.69,4.13,4.13,4.13,4.66,4.516,4.516)

for (i in 1:length(year)){ #records of interest for no-buyout scenario
  
  d<-readRDS(paste0(wd, "dasym3", names[i]))
  
  pval<-sum(d$pAPPR, na.rm=T)
  df_nb$pval[i]<-pval/1000000
  
  ptax<-sum(d$pAPPR, na.rm=T)*0.25/100*rate[i]
  df_nb$ptax[i]<-ptax/1000000
  
  dmg_1<-round(sum(d$pDMGWDFIA, na.rm=T )/1000000,2) #structural damages
  df_nb$ploss[i]<-dmg_1
  df_nb$netrev[i]<-ptax/1000000-dmg_1
  
}

#Observed Buyouts 2005-2015
year<-seq(2007,2014)
scenario<-rep("With buyouts",length(year))
df_ob<-data.frame(year,scenario)
df_ob$ptax<-NA
df_ob$pval<-NA
df_ob$ploss<-NA
df_ob$netrev<-NA

names<-c("finalnewparcels2007ob.RDS", "finalnewparcels2008ob.RDS", "finalnewparcels2009ob.RDS", "finalnewparcels2010ob.RDS", "finalnewparcels2011ob.RDS",
         "finalnewparcels2012ob.RDS", "finalnewparcels2013ob.RDS","finalnewparcels2014ob.RDS")
rate<-c(4.69,4.69,4.13,4.13,4.13,4.66,4.516,4.516)

for (i in 1:length(year)){ #records of interest for no-buyout scenario
  
  d<-readRDS(paste0(wd, "dasym3", names[i]))
  
  pval<-sum(d$pAPPR, na.rm=T)
  df_ob$pval[i]<-pval/1000000
  
  ptax<-sum(d$pAPPR, na.rm=T)*0.25/100*rate[i]
  df_ob$ptax[i]<-ptax/1000000
  
  dmg_1<-round(sum(d$pDMGWDFIA, na.rm=T )/1000000,2) #structural damages
  df_ob$ploss[i]<-dmg_1
  df_ob$netrev[i]<-ptax/1000000-dmg_1
  
}


#All Buyouts 2005-2015
year<-seq(2007,2014)
scenario<-rep("All buyouts",length(year))
df_ab<-data.frame(year,scenario)
df_ab$ptax<-NA
df_ab$pval<-NA
df_ab$ploss<-NA
df_ab$netrev<-NA

names<-c("finalnewparcels2007ob.RDS", "finalnewparcels2008ob.RDS", "finalnewparcels2009ob.RDS", 
         "finalnewparcels2010ab.RDS", "finalnewparcels2011ab.RDS","finalnewparcels2012ab.RDS", "finalnewparcels2013ab.RDS","finalnewparcels2014ab.RDS")
rate<-c(4.69,4.69,4.13,4.13,4.13,4.66,4.516,4.516)

for (i in 1:length(year)){ #records of interest for no-buyout scenario
  
  d<-readRDS(paste0(wd, "dasym3",names[i]))
  
  pval<-sum(d$pAPPR, na.rm=T)
  df_ab$pval[i]<-pval/1000000
  
  ptax<-sum(d$pAPPR, na.rm=T)*0.25/100*rate[i]
  df_ab$ptax[i]<-ptax/1000000
  
  dmg_1<-round(sum(d$pDMGWDFIA, na.rm=T )/1000000,2) #structural damages
  df_ab$ploss[i]<-dmg_1
  df_ab$netrev[i]<-ptax/1000000-dmg_1
  
}


#Wishlist Buyouts 2005-2015
year<-seq(2007,2014)
scenario<-rep("Wishlist buyouts",length(year))
df_w<-data.frame(year,scenario)
df_w$ptax<-NA
df_w$pval<-NA
df_w$ploss<-NA
df_w$netrev<-NA

names<-c("finalnewparcels2007ob.RDS", "finalnewparcels2008ob.RDS", "finalnewparcels2009ob.RDS", 
         "finalnewparcels2010w.RDS", "finalnewparcels2011w.RDS","finalnewparcels2012w.RDS", "finalnewparcels2013w.RDS","finalnewparcels2014w.RDS")

for (i in 1:length(year)){ #records of interest for no-buyout scenario
  
  d<-readRDS(paste0(wd, "dasym3", names[i]))
  
  pval<-sum(d$pAPPR, na.rm=T)
  df_w$pval[i]<-pval/1000000
  
  ptax<-sum(d$pAPPR, na.rm=T)*0.25/100*rate[i]
  df_w$ptax[i]<-ptax/1000000
  
  dmg_1<-round(sum(d$pDMGWDFIA, na.rm=T )/1000000,2) #structural damages
  df_w$ploss[i]<-dmg_1
  df_w$netrev[i]<-ptax/1000000-dmg_1
  
}



#Full time-series table
df_ts<-rbind(df_nb[1:7,],df_ob[1:7,],df_ab[1:7,],df_w[1:7,])


#Now build plot for Property Values

p <- ggplot(df_ts, aes(x = year))
p <- p + geom_line(aes(y = pval, colour = "Property Value", group=scenario, linetype=scenario, size=scenario)) 

# adding the relative humidity data, transformed to match roughly the range of the temperature
# scale.factor<-mean(df_ts$pval, na.rm=T)/mean(df_ts$ptax,na.rm=T)
# p <- p + geom_line(aes(y = (ptax)*scale.factor , colour = "Property Tax", group=scenario,linetype=scenario, size=scenario))
# 
# # now adding the secondary axis, following the example in the help file ?scale_y_continuous
# # and, very important, reverting the above transformation
# p <- p + scale_y_continuous(sec.axis = sec_axis(~./scale.factor , name = "Property Tax ($)"))

# modifying colours and theme options
p <- p + labs(y = "Property Value ($M)", x = "Year") + scale_linetype_manual(values=c("solid", "longdash", "dotted","dotdash")) + 
  scale_size_manual(values=c(1,1,1,1)) + scale_x_discrete(limits=c(2007,2010,2013))  


p <- p + theme(legend.position = "right")
p

#Now build plot for Property taxes

p <- ggplot(df_ts, aes(x = year))
p <- p + geom_line(aes(y = ptax, colour = "Property Taxes", group=scenario, linetype=scenario, size=scenario)) 

# adding the relative humidity data, transformed to match roughly the range of the temperature
# scale.factor<-mean(df_ts$pval, na.rm=T)/mean(df_ts$ptax,na.rm=T)
# p <- p + geom_line(aes(y = (ptax)*scale.factor , colour = "Property Tax", group=scenario,linetype=scenario, size=scenario))
# 
# # now adding the secondary axis, following the example in the help file ?scale_y_continuous
# # and, very important, reverting the above transformation
# p <- p + scale_y_continuous(sec.axis = sec_axis(~./scale.factor , name = "Property Tax ($)"))

# modifying colours and theme options
p <- p + labs(y = "Property Taxes ($M)", x = "Year") + scale_linetype_manual(values=c("solid", "longdash", "dotted","dotdash")) + 
  scale_size_manual(values=c(1,1,1,1)) + scale_x_discrete(limits=c(2007,2010,2013))  


p <- p + theme(legend.position = "right")
p


#Now build plot for Net Property Revenue

p <- ggplot(df_ts, aes(x = year))
p <- p + geom_line(aes(y = netrev, colour = "Net Property Revenue", group=scenario, linetype=scenario, size=scenario)) 

# adding the relative humidity data, transformed to match roughly the range of the temperature
scale.factor<-abs(mean(df_ts$netrev, na.rm=T)/mean(df_ts$ptax,na.rm=T))
p <- p + geom_line(aes(y = (ptax)*scale.factor-900 , colour = "Property Tax", group=scenario,linetype=scenario, size=scenario))

# # now adding the secondary axis, following the example in the help file ?scale_y_continuous
# and, very important, reverting the above transformation
p <- p + scale_y_continuous(sec.axis = sec_axis(~./scale.factor+900 , name = "Property Tax ($M)"))

# modifying colours and theme options
p <- p + labs(y = "Net Property Revenue: Property Taxes minus Property Damages ($M)", x = "Year") + scale_linetype_manual(values=c("solid", "longdash", "dotted","dotdash")) + 
  scale_size_manual(values=c(1,1,1,1)) + scale_x_discrete(limits=c(2007,2010,2013))  


p <- p + theme(legend.position = "right")
p

#Now build plot for Net Revenue with Predicted Bounds

#df_ts2<-df_ts[-c(2:3,5:6,9:10,12:13,16:17,19:20,23:24,26:27),]
df_ts3<-df_ts[-c(2:3,5:6,9:10,12:13,16:17,19:20,23:24,26:27),]
df_ts4<-cbind(df_ts2,df_ts3)
df_ts4$netrevH<-df_ts4$ptax-df_ts4$HS
df_ts4$netrevL<-df_ts4$ptax-df_ts4$LS

p <- ggplot(df_ts4, aes(x = year))
p <- p + geom_ribbon(aes(ymin=netrevH,ymax=netrevL, fill=scenario),alpha=0.5)


# adding the relative humidity data, transformed to match roughly the range of the temperature
scale.factor<-df_ts4$netrev[1]/ df_ts4$netrevH[1]
p <- p + geom_line(aes(y = netrev/scale.factor + 50 ,  group=scenario,linetype=scenario, size=scenario)) 

# now adding the secondary axis, following the example in the help file ?scale_y_continuous
# and, very important, reverting the above transformation
p <- p + scale_y_continuous(sec.axis = sec_axis((~.*scale.factor-50), name = "Original Estimated Net Property Revenue ($M)"))

# modifying colours and theme options
p <- p + labs(y = "Predicted Net Property Revenue ($M)", x = "Year") + scale_linetype_manual(values=c("solid", "longdash", "dotted","dotdash")) + 
  scale_size_manual(values=c(1,1,1,1)) + scale_x_discrete(limits=c(2007,2010,2013))  


p <- p + theme(legend.position = "right")#+ ylim(1400,2000)
p





####################################################################################################################################
#Create Table of Summary Statistics for Economic Damage Estimates in 2010 for no-freeboard scenarios based on Parcel-scale sf objects
#########################################################################################################################################

pnames<-c( "newparcels2007ob.RDS", "newparcels2008ob.RDS", "newparcels2009ob.RDS", "newparcels2010ob.RDS", "newparcels2011ob.RDS",
           "newparcels2012ob.RDS", "newparcels2013ob.RDS","newparcels2014ob.RDS",
           "newparcels2007nb.RDS", "newparcels2008nb.RDS", "newparcels2009nb.RDS", "newparcels2010nb.RDS", "newparcels2011nb.RDS",
           "newparcels2012nb.RDS", "newparcels2013nb.RDS","newparcels2014nb.RDS",
           "newparcels2010ab.RDS", "newparcels2011ab.RDS","newparcels2012ab.RDS", "newparcels2013ab.RDS","newparcels2014ab.RDS",
           "newparcels2010w.RDS", "newparcels2011w.RDS","newparcels2012w.RDS", "newparcels2013w.RDS","newparcels2014w.RDS")
#p10_nb<-readRDS(paste0("E:/kate/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/",pnames[12]))
#p10_nb<-readRDS(paste0("E:/kate/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/","dasym3", pnames[12]))


#No buyout scenario - no freeboard
p10_nb<-readRDS(paste0(wd, pnames[12]))
#p10_nb<-readRDS(paste0(wd,"Census", pnames[12]))
# p10_nb<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/", pnames[12]))

#county-scale
dmg_1<-round(sum(p10_nb$pDMGWDFIAfreeb , na.rm=T )/1000000,2) #structural damages
dmg_2<-round(sum(p10_nb$pDMGWDSVfreeb, na.rm=T )/1000000,2)
dmg_rng_cnty<-paste0(dmg_1,"-",dmg_2) #range of structrual damages
cont_cnty<-round(sum(p10_nb$pDMGCONTWDFIAfreeb , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_nb$prelocWDHZfreeb, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_nb$prelocWDSVfreeb, na.rm=T )/1000000,2)
reloc_cnty<-paste0(dmg_3,"-",dmg_4)
labor_cnty<-round(sum(p10_nb$plaborWDSVfreeb, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_cnty,dmg_3,labor_cnty)
tot_2<-sum(dmg_2,cont_cnty, dmg_4,labor_cnty)
tot_cnty<-paste0(tot_1,"-",tot_2)

#watershed-scale
wish_wsid<-unique(p10_nb$WS_ID.x[!is.na(p10_nb$Future)]) #watershed id for watersheds with any homes on buyout wish list
p10_nb_sub<-p10_nb[p10_nb$WS_ID.x %in% wish_wsid,]
dmg_1<-round(sum(p10_nb_sub$pDMGWDFIAfreeb, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_nb_sub$pDMGWDSVfreeb, na.rm=T )/1000000,2)
dmg_rng_ws<-paste0(dmg_1,"-",dmg_2)
cont_ws<-round(sum(p10_nb_sub$pDMGCONTWDFIAfreeb , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_nb_sub$prelocWDHZfreeb, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_nb_sub$prelocWDSVfreeb, na.rm=T )/1000000,2)
reloc_ws<-paste0(dmg_3,"-",dmg_4)
labor_ws<-round(sum(p10_nb_sub$plaborWDSVfreeb, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_ws,dmg_3,labor_ws)
tot_2<-sum(dmg_2,cont_ws,dmg_4,labor_ws)
tot_ws<-paste0(tot_1,"-",tot_2)

#buyout-scale
p10_nb_sub<-p10_nb[!is.na(p10_nb$buyout),]
dmg_1<-round(sum(p10_nb_sub$pDMGWDFIAfreeb, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_nb_sub$pDMGWDSVfreeb, na.rm=T )/1000000,2)
dmg_rng_bo<-paste0(dmg_1,"-",dmg_2)
cont_bo<-round(sum(p10_nb_sub$pDMGCONTWDFIAfreeb , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_nb_sub$prelocWDHZfreeb, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_nb_sub$prelocWDSVfreeb, na.rm=T )/1000000,2)
reloc_bo<-paste0(dmg_3,"-",dmg_4)
labor_bo<-round(sum(p10_nb_sub$plaborWDSVfreeb, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_bo,dmg_3,labor_bo)
tot_2<-sum(dmg_2,cont_bo,dmg_4,labor_bo)
tot_bo<-paste0(tot_1,"-",tot_2)

aqcost<-round(sum(p10_nb_sub$pAq_Cost, na.rm=T)/1000000,2) #fix this
appr<-round(sum(p10_nb_sub$pAPPR, na.rm=T)/1000000,2)

#vectors to go into table
damages_nb<-c(dmg_rng_cnty, dmg_rng_ws, dmg_rng_bo)
cont_nb<-c(cont_cnty,cont_ws,cont_bo)
reloc_nb<-c(reloc_cnty,reloc_ws,reloc_bo)
labor_nb<-c(labor_cnty,labor_ws,labor_bo)
tot_nb<-c(tot_cnty,tot_ws,tot_bo)
aq_cost_nb<-c(NA,NA,aqcost)
appr_nb<-c(NA,NA,appr)


#Observed buyout scenario - with freeboard
p10_ob<-readRDS(paste0(wd,pnames[4]))
#p10_ob<-readRDS(paste0(wd,"Census",pnames[4]))
#p10_ob<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/","dasym3",pnames[4]))
#p10_ob<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/", pnames[4]))
#county-scale
dmg_1<-round(sum(p10_ob$pDMGWDFIAfreeb, na.rm=T )/1000000,2) #structural damages
dmg_2<-round(sum(p10_ob$pDMGWDSVfreeb, na.rm=T )/1000000,2)
dmg_rng_cnty<-paste0(dmg_1,"-",dmg_2) #range of structrual damages
cont_cnty<-round(sum(p10_ob$pDMGCONTWDFIAfreeb , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_ob$prelocWDHZfreeb, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_ob$prelocWDSVfreeb, na.rm=T )/1000000,2)
reloc_cnty<-paste0(dmg_3,"-",dmg_4)
labor_cnty<-round(sum(p10_ob$plaborWDSVfreeb, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_cnty,dmg_3,labor_cnty)
tot_2<-sum(dmg_2,cont_cnty, dmg_4,labor_cnty)
tot_cnty<-paste0(tot_1,"-",tot_2)

#watershed-scale
#wish_wsid<-unique(p10_ob$WS_ID[!is.na(p10_ob$Future)]) #watershed id for watersheds with any homes on buyout wish list
p10_ob_sub<-p10_ob[p10_ob$WS_ID.x %in% wish_wsid,]
dmg_1<-round(sum(p10_ob_sub$pDMGWDFIAfreeb, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_ob_sub$pDMGWDSVfreeb, na.rm=T )/1000000,2)
dmg_rng_ws<-paste0(dmg_1,"-",dmg_2)
cont_ws<-round(sum(p10_ob_sub$pDMGCONTWDFIAfreeb , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_ob_sub$prelocWDHZfreeb, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_ob_sub$prelocWDSVfreeb, na.rm=T )/1000000,2)
reloc_ws<-paste0(dmg_3,"-",dmg_4)
labor_ws<-round(sum(p10_ob_sub$plaborWDSVfreeb, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_ws,dmg_3,labor_ws)
tot_2<-sum(dmg_2,cont_ws,dmg_4,labor_ws)
tot_ws<-paste0(tot_1,"-",tot_2)

#buyout-scale
p10_ob_sub<-p10_ob[!is.na(p10_ob$buyout),]
dmg_1<-round(sum(p10_ob_sub$pDMGWDFIAfreeb, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_ob_sub$pDMGWDSVfreeb, na.rm=T )/1000000,2)
dmg_rng_bo<-paste0(dmg_1,"-",dmg_2)
cont_bo<-round(sum(p10_ob_sub$pDMGCONTWDFIAfreeb , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_ob_sub$prelocWDHZfreeb, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_ob_sub$prelocWDSVfreeb, na.rm=T )/1000000,2)
reloc_bo<-paste0(dmg_3,"-",dmg_4)
labor_bo<-round(sum(p10_ob_sub$plaborWDSVfreeb, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_bo,dmg_3,labor_bo)
tot_2<-sum(dmg_2,cont_bo,dmg_4,labor_bo)
tot_bo<-paste0(tot_1,"-",tot_2)

aqcost<-round(sum(p10_ob_sub$pAq_Cost, na.rm=T)/1000000,2) #fix this
appr<-round(sum(p10_ob_sub$pAPPR, na.rm=T)/1000000,2)

#vectors to go into table
damages_ob<-c(dmg_rng_cnty, dmg_rng_ws, dmg_rng_bo)
cont_ob<-c(cont_cnty,cont_ws,cont_bo)
reloc_ob<-c(reloc_cnty,reloc_ws,reloc_bo)
labor_ob<-c(labor_cnty,labor_ws,labor_bo)
tot_ob<-c(tot_cnty,tot_ws,tot_bo)
aq_cost_ob<-c(NA,NA,aqcost)
appr_ob<-c(NA,NA,appr)

#All buyout scenario - with freeboard
p10_ab<-readRDS(paste0(wd,pnames[17]))
#p10_ab<-readRDS(paste0(wd,"Census",pnames[17]))

# p10_ab<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/","dasym3",pnames[17]))
# p10_ab<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/", pnames[17]))
#county-scale
dmg_1<-round(sum(p10_ab$pDMGWDFIAfreeb, na.rm=T )/1000000,2) #structural damages
dmg_2<-round(sum(p10_ab$pDMGWDSVfreeb, na.rm=T )/1000000,2)
dmg_rng_cnty<-paste0(dmg_1,"-",dmg_2) #range of structrual damages
cont_cnty<-round(sum(p10_ab$pDMGCONTWDFIAfreeb , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_ab$prelocWDHZfreeb, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_ab$prelocWDSVfreeb, na.rm=T )/1000000,2)
reloc_cnty<-paste0(dmg_3,"-",dmg_4)
labor_cnty<-round(sum(p10_ab$plaborWDSVfreeb, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_cnty,dmg_3,labor_cnty)
tot_2<-sum(dmg_2,cont_cnty, dmg_4,labor_cnty)
tot_cnty<-paste0(tot_1,"-",tot_2)

#watershed-scale
#wish_wsid<-unique(p10_ab$WS_ID[!is.na(p10_ab$Future)]) #watershed id for watersheds with any homes on buyout wish list
p10_ab_sub<-p10_ab[p10_ab$WS_ID.x %in% wish_wsid,]
dmg_1<-round(sum(p10_ab_sub$pDMGWDFIAfreeb, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_ab_sub$pDMGWDSVfreeb, na.rm=T )/1000000,2)
dmg_rng_ws<-paste0(dmg_1,"-",dmg_2)
cont_ws<-round(sum(p10_ab_sub$pDMGCONTWDFIAfreeb , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_ab_sub$prelocWDHZfreeb, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_ab_sub$prelocWDSVfreeb, na.rm=T )/1000000,2)
reloc_ws<-paste0(dmg_3,"-",dmg_4)
labor_ws<-round(sum(p10_ab_sub$plaborWDSVfreeb, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_ws,dmg_3,labor_ws)
tot_2<-sum(dmg_2,cont_ws,dmg_4,labor_ws)
tot_ws<-paste0(tot_1,"-",tot_2)

#buyout-scale
p10_ab_sub<-p10_ab[!is.na(p10_ab$buyout),]
dmg_1<-round(sum(p10_ab_sub$pDMGWDFIAfreeb, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_ab_sub$pDMGWDSVfreeb, na.rm=T )/1000000,2)
dmg_rng_bo<-paste0(dmg_1,"-",dmg_2)
cont_bo<-round(sum(p10_ab_sub$pDMGCONTWDFIAfreeb , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_ab_sub$prelocWDHZfreeb, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_ab_sub$prelocWDSVfreeb, na.rm=T )/1000000,2)
reloc_bo<-paste0(dmg_3,"-",dmg_4)
labor_bo<-round(sum(p10_ab_sub$plaborWDSVfreeb, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_bo,dmg_3,labor_bo)
tot_2<-sum(dmg_2,cont_bo,dmg_4,labor_bo)
tot_bo<-paste0(tot_1,"-",tot_2)

aqcost<-round(sum(p10_ab_sub$pAq_Cost, na.rm=T)/1000000,2) #fix this
appr<-round(sum(p10_ab_sub$pAPPR, na.rm=T)/1000000,2)

#vectors to go into table
damages_ab<-c(dmg_rng_cnty, dmg_rng_ws, dmg_rng_bo)
cont_ab<-c(cont_cnty,cont_ws,cont_bo)
reloc_ab<-c(reloc_cnty,reloc_ws,reloc_bo)
labor_ab<-c(labor_cnty,labor_ws,labor_bo)
tot_ab<-c(tot_cnty,tot_ws,tot_bo)
aq_cost_ab<-c(NA,NA,aqcost)
appr_ab<-c(NA,NA,appr)

#Wishlist scenario - with freeboard
p10_w<-readRDS(paste0(wd,pnames[22]))
#p10_w<-readRDS(paste0(wd,"Census",pnames[22]))

# p10_w<-readRDS(paste0("E:/kate/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/",pnames[22]))
# p10_w<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/", pnames[22]))
#county-scale
dmg_1<-round(sum(p10_w$pDMGWDFIAfreeb, na.rm=T )/1000000,2) #structural damages
dmg_2<-round(sum(p10_w$pDMGWDSVfreeb, na.rm=T )/1000000,2)
dmg_rng_cnty<-paste0(dmg_1,"-",dmg_2) #range of structrual damages
cont_cnty<-round(sum(p10_w$pDMGCONTWDFIAfreeb , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_w$prelocWDHZfreeb, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_w$prelocWDSVfreeb, na.rm=T )/1000000,2)
reloc_cnty<-paste0(dmg_3,"-",dmg_4)
labor_cnty<-round(sum(p10_w$plaborWDSVfreeb, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_cnty,dmg_3,labor_cnty)
tot_2<-sum(dmg_2,cont_cnty, dmg_4,labor_cnty)
tot_cnty<-paste0(tot_1,"-",tot_2)

#watershed-scale
p10_w_sub<-p10_w[p10_w$WS_ID.x %in% wish_wsid,]
dmg_1<-round(sum(p10_w_sub$pDMGWDFIAfreeb, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_w_sub$pDMGWDSVfreeb, na.rm=T )/1000000,2)
dmg_rng_ws<-paste0(dmg_1,"-",dmg_2)
cont_ws<-round(sum(p10_w_sub$pDMGCONTWDFIAfreeb , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_w_sub$prelocWDHZfreeb, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_w_sub$prelocWDSVfreeb, na.rm=T )/1000000,2)
reloc_ws<-paste0(dmg_3,"-",dmg_4)
labor_ws<-round(sum(p10_w_sub$plaborWDSVfreeb, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_ws,dmg_3,labor_ws)
tot_2<-sum(dmg_2,cont_ws,dmg_4,labor_ws)
tot_ws<-paste0(tot_1,"-",tot_2)

#buyout-scale
p10_w_sub<-p10_w[!is.na(p10_w$buyout),]
dmg_1<-round(sum(p10_w_sub$pDMGWDFIAfreeb, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_w_sub$pDMGWDSVfreeb, na.rm=T )/1000000,2)
dmg_rng_bo<-paste0(dmg_1,"-",dmg_2)
cont_bo<-round(sum(p10_w_sub$pDMGCONTWDFIAfreeb , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_w_sub$prelocWDHZfreeb, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_w_sub$prelocWDSVfreeb, na.rm=T )/1000000,2)
reloc_bo<-paste0(dmg_3,"-",dmg_4)
labor_bo<-round(sum(p10_w_sub$plaborWDSVfreeb, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_bo,dmg_3,labor_bo)
tot_2<-sum(dmg_2,cont_bo,dmg_4,labor_bo)
tot_bo<-paste0(tot_1,"-",tot_2)

aqcost<-round(sum(p10_w_sub$pAq_Cost, na.rm=T)/1000000,2) #fix this
appr<-round(sum(p10_w_sub$pAPPR, na.rm=T)/1000000,2)

#vectors to go into table
damages_w<-c(dmg_rng_cnty, dmg_rng_ws, dmg_rng_bo)
cont_w<-c(cont_cnty,cont_ws,cont_bo)
reloc_w<-c(reloc_cnty,reloc_ws,reloc_bo)
labor_w<-c(labor_cnty,labor_ws,labor_bo)
tot_w<-c(tot_cnty,tot_ws,tot_bo)
aq_cost_w<-c(NA,NA,aqcost)
appr_w<-c(NA,NA,appr)

#Put together the table
Scenario<-c("No Buyouts","No Buyouts","No Buyouts",
            "With Buyouts Prior to 2010","With Buyouts Prior to 2010","With Buyouts Prior to 2010",
            "All Buyouts Prior to 2010", "All Buyouts Prior to 2010","All Buyouts Prior to 2010",
            "Wishlist Prior to 2010","Wishlist Prior to 2010","Wishlist Prior to 2010")
Scale<-c("County","Micro-watersheds with Buyouts","Buyout Parcels",
         "County","Micro-watersheds with Buyouts","Buyout Parcels",
         "County","Micro-watersheds with Buyouts","Buyout Parcels",
         "County","Micro-watersheds with Buyouts","Buyout Parcels")

Structural_Damages<-c(damages_nb,damages_ob,damages_ab,damages_w)
Contents_Damages<-c(cont_nb,cont_ob,cont_ab,cont_w)
Relocation_Costs<-c(reloc_nb,reloc_ob,reloc_ab,reloc_w)
Labor_Costs<-c(labor_nb,labor_ob,labor_ab,labor_w)
Total_Damages<-c(tot_nb,tot_ob,tot_ab,tot_w)
Acquisition<-c(aq_cost_nb,aq_cost_ob,aq_cost_ab,aq_cost_w)
Appraisals<-c(appr_nb,appr_ob,appr_ab,appr_w)

df<-data.frame(Scenario, Scale, Structural_Damages, Contents_Damages,Relocation_Costs,
               Labor_Costs, Total_Damages, Acquisition, Appraisals)

################################################################################################################################################################
#Create Table of Summary Statistics for Economic Damage Estimates in 2010 based on Parcel-scale sf objects with total structure replacement for highest DLEVEL
##############################################################################################################################################################

pnames<-c( "finalnewparcels2007ob.RDS", "finalnewparcels2008ob.RDS", "finalnewparcels2009ob.RDS", "finalnewparcels2010ob.RDS", "finalnewparcels2011ob.RDS",
           "finalnewparcels2012ob.RDS", "finalnewparcels2013ob.RDS","finalnewparcels2014ob.RDS", 
           "finalnewparcels2007nb.RDS", "finalnewparcels2008nb.RDS", "finalnewparcels2009nb.RDS", "finalnewparcels2010nb.RDS", "finalnewparcels2011nb.RDS",
           "finalnewparcels2012nb.RDS", "finalnewparcels2013nb.RDS","finalnewparcels2014nb.RDS",
           "finalnewparcels2010ab.RDS", "finalnewparcels2011ab.RDS","finalnewparcels2012ab.RDS", "finalnewparcels2013ab.RDS","finalnewparcels2014ab.RDS",
           "finalnewparcels2010w.RDS", "finalnewparcels2011w.RDS","finalnewparcels2012w.RDS", "finalnewparcels2013w.RDS","finalnewparcels2014w.RDS") 


#No buyout scenario - with freeboard
p10_nb<-readRDS(paste0(wd,"dasym3", pnames[12]))
ids<-which(p10_nb$pdleveldepth >=7)
p10_nb$pDMGWDFIA[ids]<-p10_nb$pAPPR[ids]
#p10_nb<-readRDS(paste0(wd,"Census", pnames[12]))
# p10_nb<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/", pnames[12]))

#county-scale
dmg_1<-round(sum(p10_nb$pDMGWDFIA, na.rm=T )/1000000,2) #structural damages
dmg_2<-round(sum(p10_nb$pDMGWDSV, na.rm=T )/1000000,2)
dmg_rng_cnty<-paste0(dmg_1,"-",dmg_2) #range of structrual damages
cont_cnty<-round(sum(p10_nb$pDMGCONTWDFIA , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_nb$prelocWDHZ, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_nb$prelocWDSV, na.rm=T )/1000000,2)
reloc_cnty<-paste0(dmg_3,"-",dmg_4)
labor_cnty<-round(sum(p10_nb$plaborWDSV, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_cnty,dmg_3,labor_cnty)
tot_2<-sum(dmg_2,cont_cnty, dmg_4,labor_cnty)
tot_cnty<-paste0(tot_1,"-",tot_2)

#watershed-scale
wish_wsid<-unique(p10_nb$WS_ID[!is.na(p10_nb$Future)]) #watershed id for watersheds with any homes on buyout wish list
p10_nb_sub<-p10_nb[p10_nb$WS_ID %in% wish_wsid,]
dmg_1<-round(sum(p10_nb_sub$pDMGWDFIA, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_nb_sub$pDMGWDSV, na.rm=T )/1000000,2)
dmg_rng_ws<-paste0(dmg_1,"-",dmg_2)
cont_ws<-round(sum(p10_nb_sub$pDMGCONTWDFIA , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_nb_sub$prelocWDHZ, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_nb_sub$prelocWDSV, na.rm=T )/1000000,2)
reloc_ws<-paste0(dmg_3,"-",dmg_4)
labor_ws<-round(sum(p10_nb_sub$plaborWDSV, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_ws,dmg_3,labor_ws)
tot_2<-sum(dmg_2,cont_ws,dmg_4,labor_ws)
tot_ws<-paste0(tot_1,"-",tot_2)

#buyout-scale
p10_nb_sub<-p10_nb[!is.na(p10_nb$buyout),]
dmg_1<-round(sum(p10_nb_sub$pDMGWDFIA, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_nb_sub$pDMGWDSV, na.rm=T )/1000000,2)
dmg_rng_bo<-paste0(dmg_1,"-",dmg_2)
cont_bo<-round(sum(p10_nb_sub$pDMGCONTWDFIA , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_nb_sub$prelocWDHZ, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_nb_sub$prelocWDSV, na.rm=T )/1000000,2)
reloc_bo<-paste0(dmg_3,"-",dmg_4)
labor_bo<-round(sum(p10_nb_sub$plaborWDSV, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_bo,dmg_3,labor_bo)
tot_2<-sum(dmg_2,cont_bo,dmg_4,labor_bo)
tot_bo<-paste0(tot_1,"-",tot_2)

aqcost<-round(sum(p10_nb_sub$pAq_Cost, na.rm=T)/1000000,2) #fix this
appr<-round(sum(p10_nb_sub$pAPPR, na.rm=T)/1000000,2)

#vectors to go into table
damages_nb<-c(dmg_rng_cnty, dmg_rng_ws, dmg_rng_bo)
cont_nb<-c(cont_cnty,cont_ws,cont_bo)
reloc_nb<-c(reloc_cnty,reloc_ws,reloc_bo)
labor_nb<-c(labor_cnty,labor_ws,labor_bo)
tot_nb<-c(tot_cnty,tot_ws,tot_bo)
aq_cost_nb<-c(NA,NA,aqcost)
appr_nb<-c(NA,NA,appr)


#Observed buyout scenario - with freeboard
p10_ob<-readRDS(paste0(wd,"dasym3",pnames[4]))
ids<-which(p10_ob$pdleveldepth >=7)
p10_ob$pDMGWDFIA[ids]<-p10_ob$pAPPR[ids]
#p10_ob<-readRDS(paste0(wd,"Census",pnames[4]))
#p10_ob<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/","dasym3",pnames[4]))
#p10_ob<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/", pnames[4]))
#county-scale
dmg_1<-round(sum(p10_ob$pDMGWDFIA, na.rm=T )/1000000,2) #structural damages
dmg_2<-round(sum(p10_ob$pDMGWDSV, na.rm=T )/1000000,2)
dmg_rng_cnty<-paste0(dmg_1,"-",dmg_2) #range of structrual damages
cont_cnty<-round(sum(p10_ob$pDMGCONTWDFIA , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_ob$prelocWDHZ, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_ob$prelocWDSV, na.rm=T )/1000000,2)
reloc_cnty<-paste0(dmg_3,"-",dmg_4)
labor_cnty<-round(sum(p10_ob$plaborWDSV, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_cnty,dmg_3,labor_cnty)
tot_2<-sum(dmg_2,cont_cnty, dmg_4,labor_cnty)
tot_cnty<-paste0(tot_1,"-",tot_2)

#watershed-scale
# wish_wsid<-unique(p10_ob$WS_ID[!is.na(p10_ob$Future)]) #watershed id for watersheds with any homes on buyout wish list
p10_ob_sub<-p10_ob[p10_ob$WS_ID %in% wish_wsid,]
dmg_1<-round(sum(p10_ob_sub$pDMGWDFIA, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_ob_sub$pDMGWDSV, na.rm=T )/1000000,2)
dmg_rng_ws<-paste0(dmg_1,"-",dmg_2)
cont_ws<-round(sum(p10_ob_sub$pDMGCONTWDFIA , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_ob_sub$prelocWDHZ, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_ob_sub$prelocWDSV, na.rm=T )/1000000,2)
reloc_ws<-paste0(dmg_3,"-",dmg_4)
labor_ws<-round(sum(p10_ob_sub$plaborWDSV, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_ws,dmg_3,labor_ws)
tot_2<-sum(dmg_2,cont_ws,dmg_4,labor_ws)
tot_ws<-paste0(tot_1,"-",tot_2)

#buyout-scale
p10_ob_sub<-p10_ob[!is.na(p10_ob$buyout),]
dmg_1<-round(sum(p10_ob_sub$pDMGWDFIA, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_ob_sub$pDMGWDSV, na.rm=T )/1000000,2)
dmg_rng_bo<-paste0(dmg_1,"-",dmg_2)
cont_bo<-round(sum(p10_ob_sub$pDMGCONTWDFIA , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_ob_sub$prelocWDHZ, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_ob_sub$prelocWDSV, na.rm=T )/1000000,2)
reloc_bo<-paste0(dmg_3,"-",dmg_4)
labor_bo<-round(sum(p10_ob_sub$plaborWDSV, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_bo,dmg_3,labor_bo)
tot_2<-sum(dmg_2,cont_bo,dmg_4,labor_bo)
tot_bo<-paste0(tot_1,"-",tot_2)

aqcost<-round(sum(p10_ob_sub$pAq_Cost, na.rm=T)/1000000,2) #fix this
appr<-round(sum(p10_ob_sub$pAPPR, na.rm=T)/1000000,2)

#vectors to go into table
damages_ob<-c(dmg_rng_cnty, dmg_rng_ws, dmg_rng_bo)
cont_ob<-c(cont_cnty,cont_ws,cont_bo)
reloc_ob<-c(reloc_cnty,reloc_ws,reloc_bo)
labor_ob<-c(labor_cnty,labor_ws,labor_bo)
tot_ob<-c(tot_cnty,tot_ws,tot_bo)
aq_cost_ob<-c(NA,NA,aqcost)
appr_ob<-c(NA,NA,appr)

#All buyout scenario - with freeboard
p10_ab<-readRDS(paste0(wd,"dasym3",pnames[17]))
ids<-which(p10_ab$pdleveldepth >=7)
p10_ab$pDMGWDFIA[ids]<-p10_ab$pAPPR[ids]
#p10_ab<-readRDS(paste0(wd,"Census",pnames[17]))

# p10_ab<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/","dasym3",pnames[17]))
# p10_ab<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/", pnames[17]))
#county-scale
dmg_1<-round(sum(p10_ab$pDMGWDFIA, na.rm=T )/1000000,2) #structural damages
dmg_2<-round(sum(p10_ab$pDMGWDSV, na.rm=T )/1000000,2)
dmg_rng_cnty<-paste0(dmg_1,"-",dmg_2) #range of structrual damages
cont_cnty<-round(sum(p10_ab$pDMGCONTWDFIA , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_ab$prelocWDHZ, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_ab$prelocWDSV, na.rm=T )/1000000,2)
reloc_cnty<-paste0(dmg_3,"-",dmg_4)
labor_cnty<-round(sum(p10_ab$plaborWDSV, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_cnty,dmg_3,labor_cnty)
tot_2<-sum(dmg_2,cont_cnty, dmg_4,labor_cnty)
tot_cnty<-paste0(tot_1,"-",tot_2)

#watershed-scale
# wish_wsid<-unique(p10_ab$WS_ID[!is.na(p10_ab$Future)]) #watershed id for watersheds with any homes on buyout wish list
p10_ab_sub<-p10_ab[p10_ab$WS_ID %in% wish_wsid,]
dmg_1<-round(sum(p10_ab_sub$pDMGWDFIA, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_ab_sub$pDMGWDSV, na.rm=T )/1000000,2)
dmg_rng_ws<-paste0(dmg_1,"-",dmg_2)
cont_ws<-round(sum(p10_ab_sub$pDMGCONTWDFIA , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_ab_sub$prelocWDHZ, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_ab_sub$prelocWDSV, na.rm=T )/1000000,2)
reloc_ws<-paste0(dmg_3,"-",dmg_4)
labor_ws<-round(sum(p10_ab_sub$plaborWDSV, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_ws,dmg_3,labor_ws)
tot_2<-sum(dmg_2,cont_ws,dmg_4,labor_ws)
tot_ws<-paste0(tot_1,"-",tot_2)

#buyout-scale
p10_ab_sub<-p10_ab[!is.na(p10_ab$buyout),]
dmg_1<-round(sum(p10_ab_sub$pDMGWDFIA, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_ab_sub$pDMGWDSV, na.rm=T )/1000000,2)
dmg_rng_bo<-paste0(dmg_1,"-",dmg_2)
cont_bo<-round(sum(p10_ab_sub$pDMGCONTWDFIA , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_ab_sub$prelocWDHZ, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_ab_sub$prelocWDSV, na.rm=T )/1000000,2)
reloc_bo<-paste0(dmg_3,"-",dmg_4)
labor_bo<-round(sum(p10_ab_sub$plaborWDSV, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_bo,dmg_3,labor_bo)
tot_2<-sum(dmg_2,cont_bo,dmg_4,labor_bo)
tot_bo<-paste0(tot_1,"-",tot_2)

aqcost<-round(sum(p10_ab_sub$pAq_Cost, na.rm=T)/1000000,2) #fix this
appr<-round(sum(p10_ab_sub$pAPPR, na.rm=T)/1000000,2)

#vectors to go into table
damages_ab<-c(dmg_rng_cnty, dmg_rng_ws, dmg_rng_bo)
cont_ab<-c(cont_cnty,cont_ws,cont_bo)
reloc_ab<-c(reloc_cnty,reloc_ws,reloc_bo)
labor_ab<-c(labor_cnty,labor_ws,labor_bo)
tot_ab<-c(tot_cnty,tot_ws,tot_bo)
aq_cost_ab<-c(NA,NA,aqcost)
appr_ab<-c(NA,NA,appr)

#Wishlist scenario - with freeboard
p10_w<-readRDS(paste0(wd,"dasym3",pnames[22]))
ids<-which(p10_w$pdleveldepth >=7)
p10_w$pDMGWDFIA[ids]<-p10_w$pAPPR[ids]
#p10_w<-readRDS(paste0(wd,"Census",pnames[22]))

# p10_w<-readRDS(paste0("E:/kate/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/",pnames[22]))
# p10_w<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/", pnames[22]))
#county-scale
dmg_1<-round(sum(p10_w$pDMGWDFIA, na.rm=T )/1000000,2) #structural damages
dmg_2<-round(sum(p10_w$pDMGWDSV, na.rm=T )/1000000,2)
dmg_rng_cnty<-paste0(dmg_1,"-",dmg_2) #range of structrual damages
cont_cnty<-round(sum(p10_w$pDMGCONTWDFIA , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_w$prelocWDHZ, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_w$prelocWDSV, na.rm=T )/1000000,2)
reloc_cnty<-paste0(dmg_3,"-",dmg_4)
labor_cnty<-round(sum(p10_w$plaborWDSV, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_cnty,dmg_3,labor_cnty)
tot_2<-sum(dmg_2,cont_cnty, dmg_4,labor_cnty)
tot_cnty<-paste0(tot_1,"-",tot_2)

#watershed-scale
p10_w_sub<-p10_w[p10_w$WS_ID %in% wish_wsid,]
dmg_1<-round(sum(p10_w_sub$pDMGWDFIA, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_w_sub$pDMGWDSV, na.rm=T )/1000000,2)
dmg_rng_ws<-paste0(dmg_1,"-",dmg_2)
cont_ws<-round(sum(p10_w_sub$pDMGCONTWDFIA , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_w_sub$prelocWDHZ, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_w_sub$prelocWDSV, na.rm=T )/1000000,2)
reloc_ws<-paste0(dmg_3,"-",dmg_4)
labor_ws<-round(sum(p10_w_sub$plaborWDSV, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_ws,dmg_3,labor_ws)
tot_2<-sum(dmg_2,cont_ws,dmg_4,labor_ws)
tot_ws<-paste0(tot_1,"-",tot_2)

#buyout-scale
p10_w_sub<-p10_w[!is.na(p10_w$buyout),]
dmg_1<-round(sum(p10_w_sub$pDMGWDFIA, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_w_sub$pDMGWDSV, na.rm=T )/1000000,2)
dmg_rng_bo<-paste0(dmg_1,"-",dmg_2)
cont_bo<-round(sum(p10_w_sub$pDMGCONTWDFIA , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_w_sub$prelocWDHZ, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_w_sub$prelocWDSV, na.rm=T )/1000000,2)
reloc_bo<-paste0(dmg_3,"-",dmg_4)
labor_bo<-round(sum(p10_w_sub$plaborWDSV, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_bo,dmg_3,labor_bo)
tot_2<-sum(dmg_2,cont_bo,dmg_4,labor_bo)
tot_bo<-paste0(tot_1,"-",tot_2)

aqcost<-round(sum(p10_w_sub$pAq_Cost, na.rm=T)/1000000,2) #fix this
appr<-round(sum(p10_w_sub$pAPPR, na.rm=T)/1000000,2)

#vectors to go into table
damages_w<-c(dmg_rng_cnty, dmg_rng_ws, dmg_rng_bo)
cont_w<-c(cont_cnty,cont_ws,cont_bo)
reloc_w<-c(reloc_cnty,reloc_ws,reloc_bo)
labor_w<-c(labor_cnty,labor_ws,labor_bo)
tot_w<-c(tot_cnty,tot_ws,tot_bo)
aq_cost_w<-c(NA,NA,aqcost)
appr_w<-c(NA,NA,appr)

#Put together the table
Scenario<-c("No Buyouts","No Buyouts","No Buyouts",
            "With Buyouts Prior to 2010","With Buyouts Prior to 2010","With Buyouts Prior to 2010",
            "All Buyouts Prior to 2010", "All Buyouts Prior to 2010","All Buyouts Prior to 2010",
            "Wishlist Prior to 2010","Wishlist Prior to 2010","Wishlist Prior to 2010")
Scale<-c("County","Micro-watersheds with Buyouts","Buyout Parcels",
         "County","Micro-watersheds with Buyouts","Buyout Parcels",
         "County","Micro-watersheds with Buyouts","Buyout Parcels",
         "County","Micro-watersheds with Buyouts","Buyout Parcels")

Structural_Damages<-c(damages_nb,damages_ob,damages_ab,damages_w)
Contents_Damages<-c(cont_nb,cont_ob,cont_ab,cont_w)
Relocation_Costs<-c(reloc_nb,reloc_ob,reloc_ab,reloc_w)
Labor_Costs<-c(labor_nb,labor_ob,labor_ab,labor_w)
Total_Damages<-c(tot_nb,tot_ob,tot_ab,tot_w)
Acquisition<-c(aq_cost_nb,aq_cost_ob,aq_cost_ab,aq_cost_w)
Appraisals<-c(appr_nb,appr_ob,appr_ab,appr_w)

df<-data.frame(Scenario, Scale, Structural_Damages, Contents_Damages,Relocation_Costs,
               Labor_Costs, Total_Damages, Acquisition, Appraisals)






####################################################################################################################################
#Create Table of Summary Statistics for Economic Damage Estimates in 2010 for no-freeboard scenarios based on Parcel-scale sf objects with total structure replacement for highest DLEVEL
#########################################################################################################################################

pnames<-c( "newparcels2007ob.RDS", "newparcels2008ob.RDS", "newparcels2009ob.RDS", "newparcels2010ob.RDS", "newparcels2011ob.RDS",
           "newparcels2012ob.RDS", "newparcels2013ob.RDS","newparcels2014ob.RDS",
           "newparcels2007nb.RDS", "newparcels2008nb.RDS", "newparcels2009nb.RDS", "newparcels2010nb.RDS", "newparcels2011nb.RDS",
           "newparcels2012nb.RDS", "newparcels2013nb.RDS","newparcels2014nb.RDS",
           "newparcels2010ab.RDS", "newparcels2011ab.RDS","newparcels2012ab.RDS", "newparcels2013ab.RDS","newparcels2014ab.RDS",
           "newparcels2010w.RDS", "newparcels2011w.RDS","newparcels2012w.RDS", "newparcels2013w.RDS","newparcels2014w.RDS")
#p10_nb<-readRDS(paste0("E:/kate/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/",pnames[12]))
#p10_nb<-readRDS(paste0("E:/kate/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/","dasym3", pnames[12]))
######################################

#No buyout scenario - no freeboard
p10_nb<-readRDS(paste0(wd, pnames[12]))
ids<-which(p10_nb$pdleveldepth >=7)
p10_nb$pDMGWDFIAfreeb[ids]<-p10_nb$pAPPR[ids]
#p10_nb<-readRDS(paste0(wd,"Census", pnames[12]))
# p10_nb<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/", pnames[12]))

#county-scale
dmg_1<-round(sum(p10_nb$pDMGWDFIAfreeb , na.rm=T )/1000000,2) #structural damages
dmg_2<-round(sum(p10_nb$pDMGWDSVfreeb, na.rm=T )/1000000,2)
dmg_rng_cnty<-paste0(dmg_1,"-",dmg_2) #range of structrual damages
cont_cnty<-round(sum(p10_nb$pDMGCONTWDFIAfreeb , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_nb$prelocWDHZfreeb, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_nb$prelocWDSVfreeb, na.rm=T )/1000000,2)
reloc_cnty<-paste0(dmg_3,"-",dmg_4)
labor_cnty<-round(sum(p10_nb$plaborWDSVfreeb, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_cnty,dmg_3,labor_cnty)
tot_2<-sum(dmg_2,cont_cnty, dmg_4,labor_cnty)
tot_cnty<-paste0(tot_1,"-",tot_2)

#watershed-scale
wish_wsid<-unique(p10_nb$WS_ID.x[!is.na(p10_nb$Future)]) #watershed id for watersheds with any homes on buyout wish list
p10_nb_sub<-p10_nb[p10_nb$WS_ID.x %in% wish_wsid,]
dmg_1<-round(sum(p10_nb_sub$pDMGWDFIAfreeb, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_nb_sub$pDMGWDSVfreeb, na.rm=T )/1000000,2)
dmg_rng_ws<-paste0(dmg_1,"-",dmg_2)
cont_ws<-round(sum(p10_nb_sub$pDMGCONTWDFIAfreeb , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_nb_sub$prelocWDHZfreeb, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_nb_sub$prelocWDSVfreeb, na.rm=T )/1000000,2)
reloc_ws<-paste0(dmg_3,"-",dmg_4)
labor_ws<-round(sum(p10_nb_sub$plaborWDSVfreeb, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_ws,dmg_3,labor_ws)
tot_2<-sum(dmg_2,cont_ws,dmg_4,labor_ws)
tot_ws<-paste0(tot_1,"-",tot_2)

#buyout-scale
p10_nb_sub<-p10_nb[!is.na(p10_nb$buyout),]
dmg_1<-round(sum(p10_nb_sub$pDMGWDFIAfreeb, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_nb_sub$pDMGWDSVfreeb, na.rm=T )/1000000,2)
dmg_rng_bo<-paste0(dmg_1,"-",dmg_2)
cont_bo<-round(sum(p10_nb_sub$pDMGCONTWDFIAfreeb , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_nb_sub$prelocWDHZfreeb, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_nb_sub$prelocWDSVfreeb, na.rm=T )/1000000,2)
reloc_bo<-paste0(dmg_3,"-",dmg_4)
labor_bo<-round(sum(p10_nb_sub$plaborWDSVfreeb, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_bo,dmg_3,labor_bo)
tot_2<-sum(dmg_2,cont_bo,dmg_4,labor_bo)
tot_bo<-paste0(tot_1,"-",tot_2)

aqcost<-round(sum(p10_nb_sub$pAq_Cost, na.rm=T)/1000000,2) #fix this
appr<-round(sum(p10_nb_sub$pAPPR, na.rm=T)/1000000,2)

#vectors to go into table
damages_nb<-c(dmg_rng_cnty, dmg_rng_ws, dmg_rng_bo)
cont_nb<-c(cont_cnty,cont_ws,cont_bo)
reloc_nb<-c(reloc_cnty,reloc_ws,reloc_bo)
labor_nb<-c(labor_cnty,labor_ws,labor_bo)
tot_nb<-c(tot_cnty,tot_ws,tot_bo)
aq_cost_nb<-c(NA,NA,aqcost)
appr_nb<-c(NA,NA,appr)


#Observed buyout scenario - with freeboard
p10_ob<-readRDS(paste0(wd,pnames[4]))
ids<-which(p10_ob$pdleveldepth >=7)
p10_ob$pDMGWDFIAfreeb[ids]<-p10_ob$pAPPR[ids]
#p10_ob<-readRDS(paste0(wd,"Census",pnames[4]))
#p10_ob<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/","dasym3",pnames[4]))
#p10_ob<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/", pnames[4]))
#county-scale
dmg_1<-round(sum(p10_ob$pDMGWDFIAfreeb, na.rm=T )/1000000,2) #structural damages
dmg_2<-round(sum(p10_ob$pDMGWDSVfreeb, na.rm=T )/1000000,2)
dmg_rng_cnty<-paste0(dmg_1,"-",dmg_2) #range of structrual damages
cont_cnty<-round(sum(p10_ob$pDMGCONTWDFIAfreeb , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_ob$prelocWDHZfreeb, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_ob$prelocWDSVfreeb, na.rm=T )/1000000,2)
reloc_cnty<-paste0(dmg_3,"-",dmg_4)
labor_cnty<-round(sum(p10_ob$plaborWDSVfreeb, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_cnty,dmg_3,labor_cnty)
tot_2<-sum(dmg_2,cont_cnty, dmg_4,labor_cnty)
tot_cnty<-paste0(tot_1,"-",tot_2)

#watershed-scale
#wish_wsid<-unique(p10_ob$WS_ID[!is.na(p10_ob$Future)]) #watershed id for watersheds with any homes on buyout wish list
p10_ob_sub<-p10_ob[p10_ob$WS_ID.x %in% wish_wsid,]
dmg_1<-round(sum(p10_ob_sub$pDMGWDFIAfreeb, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_ob_sub$pDMGWDSVfreeb, na.rm=T )/1000000,2)
dmg_rng_ws<-paste0(dmg_1,"-",dmg_2)
cont_ws<-round(sum(p10_ob_sub$pDMGCONTWDFIAfreeb , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_ob_sub$prelocWDHZfreeb, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_ob_sub$prelocWDSVfreeb, na.rm=T )/1000000,2)
reloc_ws<-paste0(dmg_3,"-",dmg_4)
labor_ws<-round(sum(p10_ob_sub$plaborWDSVfreeb, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_ws,dmg_3,labor_ws)
tot_2<-sum(dmg_2,cont_ws,dmg_4,labor_ws)
tot_ws<-paste0(tot_1,"-",tot_2)

#buyout-scale
p10_ob_sub<-p10_ob[!is.na(p10_ob$buyout),]
dmg_1<-round(sum(p10_ob_sub$pDMGWDFIAfreeb, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_ob_sub$pDMGWDSVfreeb, na.rm=T )/1000000,2)
dmg_rng_bo<-paste0(dmg_1,"-",dmg_2)
cont_bo<-round(sum(p10_ob_sub$pDMGCONTWDFIAfreeb , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_ob_sub$prelocWDHZfreeb, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_ob_sub$prelocWDSVfreeb, na.rm=T )/1000000,2)
reloc_bo<-paste0(dmg_3,"-",dmg_4)
labor_bo<-round(sum(p10_ob_sub$plaborWDSVfreeb, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_bo,dmg_3,labor_bo)
tot_2<-sum(dmg_2,cont_bo,dmg_4,labor_bo)
tot_bo<-paste0(tot_1,"-",tot_2)

aqcost<-round(sum(p10_ob_sub$pAq_Cost, na.rm=T)/1000000,2) #fix this
appr<-round(sum(p10_ob_sub$pAPPR, na.rm=T)/1000000,2)

#vectors to go into table
damages_ob<-c(dmg_rng_cnty, dmg_rng_ws, dmg_rng_bo)
cont_ob<-c(cont_cnty,cont_ws,cont_bo)
reloc_ob<-c(reloc_cnty,reloc_ws,reloc_bo)
labor_ob<-c(labor_cnty,labor_ws,labor_bo)
tot_ob<-c(tot_cnty,tot_ws,tot_bo)
aq_cost_ob<-c(NA,NA,aqcost)
appr_ob<-c(NA,NA,appr)

#All buyout scenario - with freeboard
p10_ab<-readRDS(paste0(wd,pnames[17]))
ids<-which(p10_ab$pdleveldepth >=7)
p10_ab$pDMGWDFIAfreeb[ids]<-p10_ab$pAPPR[ids]
#p10_ab<-readRDS(paste0(wd,"Census",pnames[17]))

# p10_ab<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/","dasym3",pnames[17]))
# p10_ab<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/", pnames[17]))
#county-scale
dmg_1<-round(sum(p10_ab$pDMGWDFIAfreeb, na.rm=T )/1000000,2) #structural damages
dmg_2<-round(sum(p10_ab$pDMGWDSVfreeb, na.rm=T )/1000000,2)
dmg_rng_cnty<-paste0(dmg_1,"-",dmg_2) #range of structrual damages
cont_cnty<-round(sum(p10_ab$pDMGCONTWDFIAfreeb , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_ab$prelocWDHZfreeb, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_ab$prelocWDSVfreeb, na.rm=T )/1000000,2)
reloc_cnty<-paste0(dmg_3,"-",dmg_4)
labor_cnty<-round(sum(p10_ab$plaborWDSVfreeb, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_cnty,dmg_3,labor_cnty)
tot_2<-sum(dmg_2,cont_cnty, dmg_4,labor_cnty)
tot_cnty<-paste0(tot_1,"-",tot_2)

#watershed-scale
#wish_wsid<-unique(p10_ab$WS_ID[!is.na(p10_ab$Future)]) #watershed id for watersheds with any homes on buyout wish list
p10_ab_sub<-p10_ab[p10_ab$WS_ID.x %in% wish_wsid,]
dmg_1<-round(sum(p10_ab_sub$pDMGWDFIAfreeb, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_ab_sub$pDMGWDSVfreeb, na.rm=T )/1000000,2)
dmg_rng_ws<-paste0(dmg_1,"-",dmg_2)
cont_ws<-round(sum(p10_ab_sub$pDMGCONTWDFIAfreeb , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_ab_sub$prelocWDHZfreeb, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_ab_sub$prelocWDSVfreeb, na.rm=T )/1000000,2)
reloc_ws<-paste0(dmg_3,"-",dmg_4)
labor_ws<-round(sum(p10_ab_sub$plaborWDSVfreeb, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_ws,dmg_3,labor_ws)
tot_2<-sum(dmg_2,cont_ws,dmg_4,labor_ws)
tot_ws<-paste0(tot_1,"-",tot_2)

#buyout-scale
p10_ab_sub<-p10_ab[!is.na(p10_ab$buyout),]
dmg_1<-round(sum(p10_ab_sub$pDMGWDFIAfreeb, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_ab_sub$pDMGWDSVfreeb, na.rm=T )/1000000,2)
dmg_rng_bo<-paste0(dmg_1,"-",dmg_2)
cont_bo<-round(sum(p10_ab_sub$pDMGCONTWDFIAfreeb , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_ab_sub$prelocWDHZfreeb, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_ab_sub$prelocWDSVfreeb, na.rm=T )/1000000,2)
reloc_bo<-paste0(dmg_3,"-",dmg_4)
labor_bo<-round(sum(p10_ab_sub$plaborWDSVfreeb, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_bo,dmg_3,labor_bo)
tot_2<-sum(dmg_2,cont_bo,dmg_4,labor_bo)
tot_bo<-paste0(tot_1,"-",tot_2)

aqcost<-round(sum(p10_ab_sub$pAq_Cost, na.rm=T)/1000000,2) #fix this
appr<-round(sum(p10_ab_sub$pAPPR, na.rm=T)/1000000,2)

#vectors to go into table
damages_ab<-c(dmg_rng_cnty, dmg_rng_ws, dmg_rng_bo)
cont_ab<-c(cont_cnty,cont_ws,cont_bo)
reloc_ab<-c(reloc_cnty,reloc_ws,reloc_bo)
labor_ab<-c(labor_cnty,labor_ws,labor_bo)
tot_ab<-c(tot_cnty,tot_ws,tot_bo)
aq_cost_ab<-c(NA,NA,aqcost)
appr_ab<-c(NA,NA,appr)

#Wishlist scenario - with freeboard
p10_w<-readRDS(paste0(wd,pnames[22]))
ids<-which(p10_w$pdleveldepth >=7)
p10_w$pDMGWDFIAfreeb[ids]<-p10_w$pAPPR[ids]
#p10_w<-readRDS(paste0(wd,"Census",pnames[22]))

# p10_w<-readRDS(paste0("E:/kate/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/",pnames[22]))
# p10_w<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/", pnames[22]))
#county-scale
dmg_1<-round(sum(p10_w$pDMGWDFIAfreeb, na.rm=T )/1000000,2) #structural damages
dmg_2<-round(sum(p10_w$pDMGWDSVfreeb, na.rm=T )/1000000,2)
dmg_rng_cnty<-paste0(dmg_1,"-",dmg_2) #range of structrual damages
cont_cnty<-round(sum(p10_w$pDMGCONTWDFIAfreeb , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_w$prelocWDHZfreeb, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_w$prelocWDSVfreeb, na.rm=T )/1000000,2)
reloc_cnty<-paste0(dmg_3,"-",dmg_4)
labor_cnty<-round(sum(p10_w$plaborWDSVfreeb, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_cnty,dmg_3,labor_cnty)
tot_2<-sum(dmg_2,cont_cnty, dmg_4,labor_cnty)
tot_cnty<-paste0(tot_1,"-",tot_2)

#watershed-scale
p10_w_sub<-p10_w[p10_w$WS_ID.x %in% wish_wsid,]
dmg_1<-round(sum(p10_w_sub$pDMGWDFIAfreeb, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_w_sub$pDMGWDSVfreeb, na.rm=T )/1000000,2)
dmg_rng_ws<-paste0(dmg_1,"-",dmg_2)
cont_ws<-round(sum(p10_w_sub$pDMGCONTWDFIAfreeb , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_w_sub$prelocWDHZfreeb, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_w_sub$prelocWDSVfreeb, na.rm=T )/1000000,2)
reloc_ws<-paste0(dmg_3,"-",dmg_4)
labor_ws<-round(sum(p10_w_sub$plaborWDSVfreeb, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_ws,dmg_3,labor_ws)
tot_2<-sum(dmg_2,cont_ws,dmg_4,labor_ws)
tot_ws<-paste0(tot_1,"-",tot_2)

#buyout-scale
p10_w_sub<-p10_w[!is.na(p10_w$buyout),]
dmg_1<-round(sum(p10_w_sub$pDMGWDFIAfreeb, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_w_sub$pDMGWDSVfreeb, na.rm=T )/1000000,2)
dmg_rng_bo<-paste0(dmg_1,"-",dmg_2)
cont_bo<-round(sum(p10_w_sub$pDMGCONTWDFIAfreeb , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_w_sub$prelocWDHZfreeb, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_w_sub$prelocWDSVfreeb, na.rm=T )/1000000,2)
reloc_bo<-paste0(dmg_3,"-",dmg_4)
labor_bo<-round(sum(p10_w_sub$plaborWDSVfreeb, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_bo,dmg_3,labor_bo)
tot_2<-sum(dmg_2,cont_bo,dmg_4,labor_bo)
tot_bo<-paste0(tot_1,"-",tot_2)

aqcost<-round(sum(p10_w_sub$pAq_Cost, na.rm=T)/1000000,2) #fix this
appr<-round(sum(p10_w_sub$pAPPR, na.rm=T)/1000000,2)

#vectors to go into table
damages_w<-c(dmg_rng_cnty, dmg_rng_ws, dmg_rng_bo)
cont_w<-c(cont_cnty,cont_ws,cont_bo)
reloc_w<-c(reloc_cnty,reloc_ws,reloc_bo)
labor_w<-c(labor_cnty,labor_ws,labor_bo)
tot_w<-c(tot_cnty,tot_ws,tot_bo)
aq_cost_w<-c(NA,NA,aqcost)
appr_w<-c(NA,NA,appr)

#Put together the table
Scenario<-c("No Buyouts","No Buyouts","No Buyouts",
            "With Buyouts Prior to 2010","With Buyouts Prior to 2010","With Buyouts Prior to 2010",
            "All Buyouts Prior to 2010", "All Buyouts Prior to 2010","All Buyouts Prior to 2010",
            "Wishlist Prior to 2010","Wishlist Prior to 2010","Wishlist Prior to 2010")
Scale<-c("County","Micro-watersheds with Buyouts","Buyout Parcels",
         "County","Micro-watersheds with Buyouts","Buyout Parcels",
         "County","Micro-watersheds with Buyouts","Buyout Parcels",
         "County","Micro-watersheds with Buyouts","Buyout Parcels")

Structural_Damages<-c(damages_nb,damages_ob,damages_ab,damages_w)
Contents_Damages<-c(cont_nb,cont_ob,cont_ab,cont_w)
Relocation_Costs<-c(reloc_nb,reloc_ob,reloc_ab,reloc_w)
Labor_Costs<-c(labor_nb,labor_ob,labor_ab,labor_w)
Total_Damages<-c(tot_nb,tot_ob,tot_ab,tot_w)
Acquisition<-c(aq_cost_nb,aq_cost_ob,aq_cost_ab,aq_cost_w)
Appraisals<-c(appr_nb,appr_ob,appr_ab,appr_w)

df<-data.frame(Scenario, Scale, Structural_Damages, Contents_Damages,Relocation_Costs,
               Labor_Costs, Total_Damages, Acquisition, Appraisals)

############################################################################################################
#Create Table of Summary Statistics for Economic Damage Estimates in 2010 based on Climate Scenario HAZUS 100 MODEL
############################################################################################################

pnames<-c( "finalnewparcels2007ob.RDS", "finalnewparcels2008ob.RDS", "finalnewparcels2009ob.RDS", "finalnewparcels2010ob.RDS", "finalnewparcels2011ob.RDS",
           "finalnewparcels2012ob.RDS", "finalnewparcels2013ob.RDS","finalnewparcels2014ob.RDS", 
           "finalnewparcels2007nb.RDS", "finalnewparcels2008nb.RDS", "finalnewparcels2009nb.RDS", "finalnewparcels2010nb.RDS", "finalnewparcels2011nb.RDS",
           "finalnewparcels2012nb.RDS", "finalnewparcels2013nb.RDS","finalnewparcels2014nb.RDS",
           "finalnewparcels2010ab.RDS", "finalnewparcels2011ab.RDS","finalnewparcels2012ab.RDS", "finalnewparcels2013ab.RDS","finalnewparcels2014ab.RDS",
           "finalnewparcels2010w.RDS", "finalnewparcels2011w.RDS","finalnewparcels2012w.RDS", "finalnewparcels2013w.RDS","finalnewparcels2014w.RDS") 


#No buyout scenario - with freeboard
p10_nb<-readRDS(paste0(wd,"dasym3", pnames[12]))
#p10_nb<-readRDS(paste0(wd,"Census", pnames[12]))
# p10_nb<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/", pnames[12]))

#county-scale
dmg_1<-round(sum(p10_nb$pDMG100FIA, na.rm=T )/1000000,2) #structural damages
dmg_2<-round(sum(p10_nb$pDMG100SV, na.rm=T )/1000000,2)
dmg_rng_cnty<-paste0(dmg_1,"-",dmg_2) #range of structrual damages
cont_cnty<-round(sum(p10_nb$pDMGCONT100FIA , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_nb$preloc100HZ, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_nb$preloc100SV, na.rm=T )/1000000,2)
reloc_cnty<-paste0(dmg_3,"-",dmg_4)
labor_cnty<-round(sum(p10_nb$plabor100SV, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_cnty,dmg_3,labor_cnty)
tot_2<-sum(dmg_2,cont_cnty, dmg_4,labor_cnty)
tot_cnty<-paste0(tot_1,"-",tot_2)

#watershed-scale
wish_wsid<-unique(p10_nb$WS_ID[!is.na(p10_nb$Future)]) #watershed id for watersheds with any homes on buyout wish list
p10_nb_sub<-p10_nb[p10_nb$WS_ID %in% wish_wsid,]
dmg_1<-round(sum(p10_nb_sub$pDMG100FIA, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_nb_sub$pDMG100SV, na.rm=T )/1000000,2)
dmg_rng_ws<-paste0(dmg_1,"-",dmg_2)
cont_ws<-round(sum(p10_nb_sub$pDMGCONT100FIA , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_nb_sub$preloc100HZ, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_nb_sub$preloc100SV, na.rm=T )/1000000,2)
reloc_ws<-paste0(dmg_3,"-",dmg_4)
labor_ws<-round(sum(p10_nb_sub$plabor100SV, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_ws,dmg_3,labor_ws)
tot_2<-sum(dmg_2,cont_ws,dmg_4,labor_ws)
tot_ws<-paste0(tot_1,"-",tot_2)

#buyout-scale
p10_nb_sub<-p10_nb[!is.na(p10_nb$buyout),]
dmg_1<-round(sum(p10_nb_sub$pDMG100FIA, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_nb_sub$pDMG100SV, na.rm=T )/1000000,2)
dmg_rng_bo<-paste0(dmg_1,"-",dmg_2)
cont_bo<-round(sum(p10_nb_sub$pDMGCONT100FIA , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_nb_sub$preloc100HZ, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_nb_sub$preloc100SV, na.rm=T )/1000000,2)
reloc_bo<-paste0(dmg_3,"-",dmg_4)
labor_bo<-round(sum(p10_nb_sub$plabor100SV, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_bo,dmg_3,labor_bo)
tot_2<-sum(dmg_2,cont_bo,dmg_4,labor_bo)
tot_bo<-paste0(tot_1,"-",tot_2)

aqcost<-round(sum(p10_nb_sub$pAq_Cost, na.rm=T)/1000000,2) #fix this
appr<-round(sum(p10_nb_sub$pAPPR, na.rm=T)/1000000,2)

#vectors to go into table
damages_nb<-c(dmg_rng_cnty, dmg_rng_ws, dmg_rng_bo)
cont_nb<-c(cont_cnty,cont_ws,cont_bo)
reloc_nb<-c(reloc_cnty,reloc_ws,reloc_bo)
labor_nb<-c(labor_cnty,labor_ws,labor_bo)
tot_nb<-c(tot_cnty,tot_ws,tot_bo)
aq_cost_nb<-c(NA,NA,aqcost)
appr_nb<-c(NA,NA,appr)


#Observed buyout scenario - with freeboard
p10_ob<-readRDS(paste0(wd,"dasym3",pnames[4]))
#p10_ob<-readRDS(paste0(wd,"Census",pnames[4]))
#p10_ob<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/","dasym3",pnames[4]))
#p10_ob<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/", pnames[4]))
#county-scale
dmg_1<-round(sum(p10_ob$pDMG100FIA, na.rm=T )/1000000,2) #structural damages
dmg_2<-round(sum(p10_ob$pDMG100SV, na.rm=T )/1000000,2)
dmg_rng_cnty<-paste0(dmg_1,"-",dmg_2) #range of structrual damages
cont_cnty<-round(sum(p10_ob$pDMGCONT100FIA , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_ob$preloc100HZ, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_ob$preloc100SV, na.rm=T )/1000000,2)
reloc_cnty<-paste0(dmg_3,"-",dmg_4)
labor_cnty<-round(sum(p10_ob$plabor100SV, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_cnty,dmg_3,labor_cnty)
tot_2<-sum(dmg_2,cont_cnty, dmg_4,labor_cnty)
tot_cnty<-paste0(tot_1,"-",tot_2)

#watershed-scale
# wish_wsid<-unique(p10_ob$WS_ID[!is.na(p10_ob$Future)]) #watershed id for watersheds with any homes on buyout wish list
p10_ob_sub<-p10_ob[p10_ob$WS_ID %in% wish_wsid,]
dmg_1<-round(sum(p10_ob_sub$pDMG100FIA, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_ob_sub$pDMG100SV, na.rm=T )/1000000,2)
dmg_rng_ws<-paste0(dmg_1,"-",dmg_2)
cont_ws<-round(sum(p10_ob_sub$pDMGCONT100FIA , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_ob_sub$preloc100HZ, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_ob_sub$preloc100SV, na.rm=T )/1000000,2)
reloc_ws<-paste0(dmg_3,"-",dmg_4)
labor_ws<-round(sum(p10_ob_sub$plabor100SV, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_ws,dmg_3,labor_ws)
tot_2<-sum(dmg_2,cont_ws,dmg_4,labor_ws)
tot_ws<-paste0(tot_1,"-",tot_2)

#buyout-scale
p10_ob_sub<-p10_ob[!is.na(p10_ob$buyout),]
dmg_1<-round(sum(p10_ob_sub$pDMG100FIA, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_ob_sub$pDMG100SV, na.rm=T )/1000000,2)
dmg_rng_bo<-paste0(dmg_1,"-",dmg_2)
cont_bo<-round(sum(p10_ob_sub$pDMGCONT100FIA , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_ob_sub$preloc100HZ, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_ob_sub$preloc100SV, na.rm=T )/1000000,2)
reloc_bo<-paste0(dmg_3,"-",dmg_4)
labor_bo<-round(sum(p10_ob_sub$plabor100SV, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_bo,dmg_3,labor_bo)
tot_2<-sum(dmg_2,cont_bo,dmg_4,labor_bo)
tot_bo<-paste0(tot_1,"-",tot_2)

aqcost<-round(sum(p10_ob_sub$pAq_Cost, na.rm=T)/1000000,2) #fix this
appr<-round(sum(p10_ob_sub$pAPPR, na.rm=T)/1000000,2)

#vectors to go into table
damages_ob<-c(dmg_rng_cnty, dmg_rng_ws, dmg_rng_bo)
cont_ob<-c(cont_cnty,cont_ws,cont_bo)
reloc_ob<-c(reloc_cnty,reloc_ws,reloc_bo)
labor_ob<-c(labor_cnty,labor_ws,labor_bo)
tot_ob<-c(tot_cnty,tot_ws,tot_bo)
aq_cost_ob<-c(NA,NA,aqcost)
appr_ob<-c(NA,NA,appr)

#All buyout scenario - with freeboard
p10_ab<-readRDS(paste0(wd,"dasym3",pnames[17]))
#p10_ab<-readRDS(paste0(wd,"Census",pnames[17]))

# p10_ab<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/","dasym3",pnames[17]))
# p10_ab<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/", pnames[17]))
#county-scale
dmg_1<-round(sum(p10_ab$pDMG100FIA, na.rm=T )/1000000,2) #structural damages
dmg_2<-round(sum(p10_ab$pDMG100SV, na.rm=T )/1000000,2)
dmg_rng_cnty<-paste0(dmg_1,"-",dmg_2) #range of structrual damages
cont_cnty<-round(sum(p10_ab$pDMGCONT100FIA , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_ab$preloc100HZ, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_ab$preloc100SV, na.rm=T )/1000000,2)
reloc_cnty<-paste0(dmg_3,"-",dmg_4)
labor_cnty<-round(sum(p10_ab$plabor100SV, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_cnty,dmg_3,labor_cnty)
tot_2<-sum(dmg_2,cont_cnty, dmg_4,labor_cnty)
tot_cnty<-paste0(tot_1,"-",tot_2)

#watershed-scale
# wish_wsid<-unique(p10_ab$WS_ID[!is.na(p10_ab$Future)]) #watershed id for watersheds with any homes on buyout wish list
p10_ab_sub<-p10_ab[p10_ab$WS_ID %in% wish_wsid,]
dmg_1<-round(sum(p10_ab_sub$pDMG100FIA, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_ab_sub$pDMG100SV, na.rm=T )/1000000,2)
dmg_rng_ws<-paste0(dmg_1,"-",dmg_2)
cont_ws<-round(sum(p10_ab_sub$pDMGCONT100FIA , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_ab_sub$preloc100HZ, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_ab_sub$preloc100SV, na.rm=T )/1000000,2)
reloc_ws<-paste0(dmg_3,"-",dmg_4)
labor_ws<-round(sum(p10_ab_sub$plabor100SV, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_ws,dmg_3,labor_ws)
tot_2<-sum(dmg_2,cont_ws,dmg_4,labor_ws)
tot_ws<-paste0(tot_1,"-",tot_2)

#buyout-scale
p10_ab_sub<-p10_ab[!is.na(p10_ab$buyout),]
dmg_1<-round(sum(p10_ab_sub$pDMG100FIA, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_ab_sub$pDMG100SV, na.rm=T )/1000000,2)
dmg_rng_bo<-paste0(dmg_1,"-",dmg_2)
cont_bo<-round(sum(p10_ab_sub$pDMGCONT100FIA , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_ab_sub$preloc100HZ, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_ab_sub$preloc100SV, na.rm=T )/1000000,2)
reloc_bo<-paste0(dmg_3,"-",dmg_4)
labor_bo<-round(sum(p10_ab_sub$plabor100SV, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_bo,dmg_3,labor_bo)
tot_2<-sum(dmg_2,cont_bo,dmg_4,labor_bo)
tot_bo<-paste0(tot_1,"-",tot_2)

aqcost<-round(sum(p10_ab_sub$pAq_Cost, na.rm=T)/1000000,2) #fix this
appr<-round(sum(p10_ab_sub$pAPPR, na.rm=T)/1000000,2)

#vectors to go into table
damages_ab<-c(dmg_rng_cnty, dmg_rng_ws, dmg_rng_bo)
cont_ab<-c(cont_cnty,cont_ws,cont_bo)
reloc_ab<-c(reloc_cnty,reloc_ws,reloc_bo)
labor_ab<-c(labor_cnty,labor_ws,labor_bo)
tot_ab<-c(tot_cnty,tot_ws,tot_bo)
aq_cost_ab<-c(NA,NA,aqcost)
appr_ab<-c(NA,NA,appr)

#Wishlist scenario - with freeboard
p10_w<-readRDS(paste0(wd,"dasym3",pnames[22]))
#p10_w<-readRDS(paste0(wd,"Census",pnames[22]))

# p10_w<-readRDS(paste0("E:/kate/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/",pnames[22]))
# p10_w<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/", pnames[22]))
#county-scale
dmg_1<-round(sum(p10_w$pDMG100FIA, na.rm=T )/1000000,2) #structural damages
dmg_2<-round(sum(p10_w$pDMG100SV, na.rm=T )/1000000,2)
dmg_rng_cnty<-paste0(dmg_1,"-",dmg_2) #range of structrual damages
cont_cnty<-round(sum(p10_w$pDMGCONT100FIA , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_w$preloc100HZ, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_w$preloc100SV, na.rm=T )/1000000,2)
reloc_cnty<-paste0(dmg_3,"-",dmg_4)
labor_cnty<-round(sum(p10_w$plabor100SV, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_cnty,dmg_3,labor_cnty)
tot_2<-sum(dmg_2,cont_cnty, dmg_4,labor_cnty)
tot_cnty<-paste0(tot_1,"-",tot_2)

#watershed-scale
p10_w_sub<-p10_w[p10_w$WS_ID %in% wish_wsid,]
dmg_1<-round(sum(p10_w_sub$pDMG100FIA, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_w_sub$pDMG100SV, na.rm=T )/1000000,2)
dmg_rng_ws<-paste0(dmg_1,"-",dmg_2)
cont_ws<-round(sum(p10_w_sub$pDMGCONT100FIA , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_w_sub$preloc100HZ, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_w_sub$preloc100SV, na.rm=T )/1000000,2)
reloc_ws<-paste0(dmg_3,"-",dmg_4)
labor_ws<-round(sum(p10_w_sub$plabor100SV, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_ws,dmg_3,labor_ws)
tot_2<-sum(dmg_2,cont_ws,dmg_4,labor_ws)
tot_ws<-paste0(tot_1,"-",tot_2)

#buyout-scale
p10_w_sub<-p10_w[!is.na(p10_w$buyout),]
dmg_1<-round(sum(p10_w_sub$pDMG100FIA, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_w_sub$pDMG100SV, na.rm=T )/1000000,2)
dmg_rng_bo<-paste0(dmg_1,"-",dmg_2)
cont_bo<-round(sum(p10_w_sub$pDMGCONT100FIA , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_w_sub$preloc100HZ, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_w_sub$preloc100SV, na.rm=T )/1000000,2)
reloc_bo<-paste0(dmg_3,"-",dmg_4)
labor_bo<-round(sum(p10_w_sub$plabor100SV, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_bo,dmg_3,labor_bo)
tot_2<-sum(dmg_2,cont_bo,dmg_4,labor_bo)
tot_bo<-paste0(tot_1,"-",tot_2)

aqcost<-round(sum(p10_w_sub$pAq_Cost, na.rm=T)/1000000,2) #fix this
appr<-round(sum(p10_w_sub$pAPPR, na.rm=T)/1000000,2)

#vectors to go into table
damages_w<-c(dmg_rng_cnty, dmg_rng_ws, dmg_rng_bo)
cont_w<-c(cont_cnty,cont_ws,cont_bo)
reloc_w<-c(reloc_cnty,reloc_ws,reloc_bo)
labor_w<-c(labor_cnty,labor_ws,labor_bo)
tot_w<-c(tot_cnty,tot_ws,tot_bo)
aq_cost_w<-c(NA,NA,aqcost)
appr_w<-c(NA,NA,appr)

#Put together the table
Scenario<-c("No Buyouts","No Buyouts","No Buyouts",
            "With Buyouts Prior to 2010","With Buyouts Prior to 2010","With Buyouts Prior to 2010",
            "All Buyouts Prior to 2010", "All Buyouts Prior to 2010","All Buyouts Prior to 2010",
            "Wishlist Prior to 2010","Wishlist Prior to 2010","Wishlist Prior to 2010")
Scale<-c("County","Micro-watersheds with Buyouts","Buyout Parcels",
         "County","Micro-watersheds with Buyouts","Buyout Parcels",
         "County","Micro-watersheds with Buyouts","Buyout Parcels",
         "County","Micro-watersheds with Buyouts","Buyout Parcels")

Structural_Damages<-c(damages_nb,damages_ob,damages_ab,damages_w)
Contents_Damages<-c(cont_nb,cont_ob,cont_ab,cont_w)
Relocation_Costs<-c(reloc_nb,reloc_ob,reloc_ab,reloc_w)
Labor_Costs<-c(labor_nb,labor_ob,labor_ab,labor_w)
Total_Damages<-c(tot_nb,tot_ob,tot_ab,tot_w)
Acquisition<-c(aq_cost_nb,aq_cost_ob,aq_cost_ab,aq_cost_w)
Appraisals<-c(appr_nb,appr_ob,appr_ab,appr_w)

df<-data.frame(Scenario, Scale, Structural_Damages, Contents_Damages,Relocation_Costs,
               Labor_Costs, Total_Damages, Acquisition, Appraisals)



############################################################################################################
#Create Table of Summary Statistics for Economic Damage Estimates in 2010 based on Climate Scenario HAZUS 1000 MODEL
############################################################################################################

pnames<-c( "finalnewparcels2007ob.RDS", "finalnewparcels2008ob.RDS", "finalnewparcels2009ob.RDS", "finalnewparcels2010ob.RDS", "finalnewparcels2011ob.RDS",
           "finalnewparcels2012ob.RDS", "finalnewparcels2013ob.RDS","finalnewparcels2014ob.RDS", 
           "finalnewparcels2007nb.RDS", "finalnewparcels2008nb.RDS", "finalnewparcels2009nb.RDS", "finalnewparcels2010nb.RDS", "finalnewparcels2011nb.RDS",
           "finalnewparcels2012nb.RDS", "finalnewparcels2013nb.RDS","finalnewparcels2014nb.RDS",
           "finalnewparcels2010ab.RDS", "finalnewparcels2011ab.RDS","finalnewparcels2012ab.RDS", "finalnewparcels2013ab.RDS","finalnewparcels2014ab.RDS",
           "finalnewparcels2010w.RDS", "finalnewparcels2011w.RDS","finalnewparcels2012w.RDS", "finalnewparcels2013w.RDS","finalnewparcels2014w.RDS") 


#No buyout scenario - with freeboard
p10_nb<-readRDS(paste0(wd,"dasym3", pnames[12]))
#p10_nb<-readRDS(paste0(wd,"Census", pnames[12]))
# p10_nb<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/", pnames[12]))

#county-scale
dmg_1<-round(sum(p10_nb$pDMG1000FIA, na.rm=T )/1000000,2) #structural damages
dmg_2<-round(sum(p10_nb$pDMG1000SV, na.rm=T )/1000000,2)
dmg_rng_cnty<-paste0(dmg_1,"-",dmg_2) #range of structrual damages
cont_cnty<-round(sum(p10_nb$pDMGCONT1000FIA , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_nb$preloc1000HZ, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_nb$preloc1000SV, na.rm=T )/1000000,2)
reloc_cnty<-paste0(dmg_3,"-",dmg_4)
labor_cnty<-round(sum(p10_nb$plabor1000SV, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_cnty,dmg_3,labor_cnty)
tot_2<-sum(dmg_2,cont_cnty, dmg_4,labor_cnty)
tot_cnty<-paste0(tot_1,"-",tot_2)

#watershed-scale
wish_wsid<-unique(p10_nb$WS_ID[!is.na(p10_nb$Future)]) #watershed id for watersheds with any homes on buyout wish list
p10_nb_sub<-p10_nb[p10_nb$WS_ID %in% wish_wsid,]
dmg_1<-round(sum(p10_nb_sub$pDMG1000FIA, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_nb_sub$pDMG1000SV, na.rm=T )/1000000,2)
dmg_rng_ws<-paste0(dmg_1,"-",dmg_2)
cont_ws<-round(sum(p10_nb_sub$pDMGCONT1000FIA , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_nb_sub$preloc1000HZ, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_nb_sub$preloc1000SV, na.rm=T )/1000000,2)
reloc_ws<-paste0(dmg_3,"-",dmg_4)
labor_ws<-round(sum(p10_nb_sub$plabor1000SV, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_ws,dmg_3,labor_ws)
tot_2<-sum(dmg_2,cont_ws,dmg_4,labor_ws)
tot_ws<-paste0(tot_1,"-",tot_2)

#buyout-scale
p10_nb_sub<-p10_nb[!is.na(p10_nb$buyout),]
dmg_1<-round(sum(p10_nb_sub$pDMG1000FIA, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_nb_sub$pDMG1000SV, na.rm=T )/1000000,2)
dmg_rng_bo<-paste0(dmg_1,"-",dmg_2)
cont_bo<-round(sum(p10_nb_sub$pDMGCONT1000FIA , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_nb_sub$preloc1000HZ, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_nb_sub$preloc1000SV, na.rm=T )/1000000,2)
reloc_bo<-paste0(dmg_3,"-",dmg_4)
labor_bo<-round(sum(p10_nb_sub$plabor1000SV, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_bo,dmg_3,labor_bo)
tot_2<-sum(dmg_2,cont_bo,dmg_4,labor_bo)
tot_bo<-paste0(tot_1,"-",tot_2)

aqcost<-round(sum(p10_nb_sub$pAq_Cost, na.rm=T)/1000000,2) #fix this
appr<-round(sum(p10_nb_sub$pAPPR, na.rm=T)/1000000,2)

#vectors to go into table
damages_nb<-c(dmg_rng_cnty, dmg_rng_ws, dmg_rng_bo)
cont_nb<-c(cont_cnty,cont_ws,cont_bo)
reloc_nb<-c(reloc_cnty,reloc_ws,reloc_bo)
labor_nb<-c(labor_cnty,labor_ws,labor_bo)
tot_nb<-c(tot_cnty,tot_ws,tot_bo)
aq_cost_nb<-c(NA,NA,aqcost)
appr_nb<-c(NA,NA,appr)


#Observed buyout scenario - with freeboard
p10_ob<-readRDS(paste0(wd,"dasym3",pnames[4]))
#p10_ob<-readRDS(paste0(wd,"Census",pnames[4]))
#p10_ob<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/","dasym3",pnames[4]))
#p10_ob<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/", pnames[4]))
#county-scale
dmg_1<-round(sum(p10_ob$pDMG1000FIA, na.rm=T )/1000000,2) #structural damages
dmg_2<-round(sum(p10_ob$pDMG1000SV, na.rm=T )/1000000,2)
dmg_rng_cnty<-paste0(dmg_1,"-",dmg_2) #range of structrual damages
cont_cnty<-round(sum(p10_ob$pDMGCONT1000FIA , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_ob$preloc1000HZ, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_ob$preloc1000SV, na.rm=T )/1000000,2)
reloc_cnty<-paste0(dmg_3,"-",dmg_4)
labor_cnty<-round(sum(p10_ob$plabor1000SV, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_cnty,dmg_3,labor_cnty)
tot_2<-sum(dmg_2,cont_cnty, dmg_4,labor_cnty)
tot_cnty<-paste0(tot_1,"-",tot_2)

#watershed-scale
# wish_wsid<-unique(p10_ob$WS_ID[!is.na(p10_ob$Future)]) #watershed id for watersheds with any homes on buyout wish list
p10_ob_sub<-p10_ob[p10_ob$WS_ID %in% wish_wsid,]
dmg_1<-round(sum(p10_ob_sub$pDMG1000FIA, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_ob_sub$pDMG1000SV, na.rm=T )/1000000,2)
dmg_rng_ws<-paste0(dmg_1,"-",dmg_2)
cont_ws<-round(sum(p10_ob_sub$pDMGCONT1000FIA , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_ob_sub$preloc1000HZ, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_ob_sub$preloc1000SV, na.rm=T )/1000000,2)
reloc_ws<-paste0(dmg_3,"-",dmg_4)
labor_ws<-round(sum(p10_ob_sub$plabor1000SV, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_ws,dmg_3,labor_ws)
tot_2<-sum(dmg_2,cont_ws,dmg_4,labor_ws)
tot_ws<-paste0(tot_1,"-",tot_2)

#buyout-scale
p10_ob_sub<-p10_ob[!is.na(p10_ob$buyout),]
dmg_1<-round(sum(p10_ob_sub$pDMG1000FIA, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_ob_sub$pDMG1000SV, na.rm=T )/1000000,2)
dmg_rng_bo<-paste0(dmg_1,"-",dmg_2)
cont_bo<-round(sum(p10_ob_sub$pDMGCONT1000FIA , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_ob_sub$preloc1000HZ, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_ob_sub$preloc1000SV, na.rm=T )/1000000,2)
reloc_bo<-paste0(dmg_3,"-",dmg_4)
labor_bo<-round(sum(p10_ob_sub$plabor1000SV, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_bo,dmg_3,labor_bo)
tot_2<-sum(dmg_2,cont_bo,dmg_4,labor_bo)
tot_bo<-paste0(tot_1,"-",tot_2)

aqcost<-round(sum(p10_ob_sub$pAq_Cost, na.rm=T)/1000000,2) #fix this
appr<-round(sum(p10_ob_sub$pAPPR, na.rm=T)/1000000,2)

#vectors to go into table
damages_ob<-c(dmg_rng_cnty, dmg_rng_ws, dmg_rng_bo)
cont_ob<-c(cont_cnty,cont_ws,cont_bo)
reloc_ob<-c(reloc_cnty,reloc_ws,reloc_bo)
labor_ob<-c(labor_cnty,labor_ws,labor_bo)
tot_ob<-c(tot_cnty,tot_ws,tot_bo)
aq_cost_ob<-c(NA,NA,aqcost)
appr_ob<-c(NA,NA,appr)

#All buyout scenario - with freeboard
p10_ab<-readRDS(paste0(wd,"dasym3",pnames[17]))
#p10_ab<-readRDS(paste0(wd,"Census",pnames[17]))

# p10_ab<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/","dasym3",pnames[17]))
# p10_ab<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/", pnames[17]))
#county-scale
dmg_1<-round(sum(p10_ab$pDMG1000FIA, na.rm=T )/1000000,2) #structural damages
dmg_2<-round(sum(p10_ab$pDMG1000SV, na.rm=T )/1000000,2)
dmg_rng_cnty<-paste0(dmg_1,"-",dmg_2) #range of structrual damages
cont_cnty<-round(sum(p10_ab$pDMGCONT1000FIA , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_ab$preloc1000HZ, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_ab$preloc1000SV, na.rm=T )/1000000,2)
reloc_cnty<-paste0(dmg_3,"-",dmg_4)
labor_cnty<-round(sum(p10_ab$plabor1000SV, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_cnty,dmg_3,labor_cnty)
tot_2<-sum(dmg_2,cont_cnty, dmg_4,labor_cnty)
tot_cnty<-paste0(tot_1,"-",tot_2)

#watershed-scale
# wish_wsid<-unique(p10_ab$WS_ID[!is.na(p10_ab$Future)]) #watershed id for watersheds with any homes on buyout wish list
p10_ab_sub<-p10_ab[p10_ab$WS_ID %in% wish_wsid,]
dmg_1<-round(sum(p10_ab_sub$pDMG1000FIA, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_ab_sub$pDMG1000SV, na.rm=T )/1000000,2)
dmg_rng_ws<-paste0(dmg_1,"-",dmg_2)
cont_ws<-round(sum(p10_ab_sub$pDMGCONT1000FIA , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_ab_sub$preloc1000HZ, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_ab_sub$preloc1000SV, na.rm=T )/1000000,2)
reloc_ws<-paste0(dmg_3,"-",dmg_4)
labor_ws<-round(sum(p10_ab_sub$plabor1000SV, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_ws,dmg_3,labor_ws)
tot_2<-sum(dmg_2,cont_ws,dmg_4,labor_ws)
tot_ws<-paste0(tot_1,"-",tot_2)

#buyout-scale
p10_ab_sub<-p10_ab[!is.na(p10_ab$buyout),]
dmg_1<-round(sum(p10_ab_sub$pDMG1000FIA, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_ab_sub$pDMG1000SV, na.rm=T )/1000000,2)
dmg_rng_bo<-paste0(dmg_1,"-",dmg_2)
cont_bo<-round(sum(p10_ab_sub$pDMGCONT1000FIA , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_ab_sub$preloc1000HZ, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_ab_sub$preloc1000SV, na.rm=T )/1000000,2)
reloc_bo<-paste0(dmg_3,"-",dmg_4)
labor_bo<-round(sum(p10_ab_sub$plabor1000SV, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_bo,dmg_3,labor_bo)
tot_2<-sum(dmg_2,cont_bo,dmg_4,labor_bo)
tot_bo<-paste0(tot_1,"-",tot_2)

aqcost<-round(sum(p10_ab_sub$pAq_Cost, na.rm=T)/1000000,2) #fix this
appr<-round(sum(p10_ab_sub$pAPPR, na.rm=T)/1000000,2)

#vectors to go into table
damages_ab<-c(dmg_rng_cnty, dmg_rng_ws, dmg_rng_bo)
cont_ab<-c(cont_cnty,cont_ws,cont_bo)
reloc_ab<-c(reloc_cnty,reloc_ws,reloc_bo)
labor_ab<-c(labor_cnty,labor_ws,labor_bo)
tot_ab<-c(tot_cnty,tot_ws,tot_bo)
aq_cost_ab<-c(NA,NA,aqcost)
appr_ab<-c(NA,NA,appr)

#Wishlist scenario - with freeboard
p10_w<-readRDS(paste0(wd,"dasym3",pnames[22]))
#p10_w<-readRDS(paste0(wd,"Census",pnames[22]))

# p10_w<-readRDS(paste0("E:/kate/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/",pnames[22]))
# p10_w<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/", pnames[22]))
#county-scale
dmg_1<-round(sum(p10_w$pDMG1000FIA, na.rm=T )/1000000,2) #structural damages
dmg_2<-round(sum(p10_w$pDMG1000SV, na.rm=T )/1000000,2)
dmg_rng_cnty<-paste0(dmg_1,"-",dmg_2) #range of structrual damages
cont_cnty<-round(sum(p10_w$pDMGCONT1000FIA , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_w$preloc1000HZ, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_w$preloc1000SV, na.rm=T )/1000000,2)
reloc_cnty<-paste0(dmg_3,"-",dmg_4)
labor_cnty<-round(sum(p10_w$plabor1000SV, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_cnty,dmg_3,labor_cnty)
tot_2<-sum(dmg_2,cont_cnty, dmg_4,labor_cnty)
tot_cnty<-paste0(tot_1,"-",tot_2)

#watershed-scale
p10_w_sub<-p10_w[p10_w$WS_ID %in% wish_wsid,]
dmg_1<-round(sum(p10_w_sub$pDMG1000FIA, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_w_sub$pDMG1000SV, na.rm=T )/1000000,2)
dmg_rng_ws<-paste0(dmg_1,"-",dmg_2)
cont_ws<-round(sum(p10_w_sub$pDMGCONT1000FIA , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_w_sub$preloc1000HZ, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_w_sub$preloc1000SV, na.rm=T )/1000000,2)
reloc_ws<-paste0(dmg_3,"-",dmg_4)
labor_ws<-round(sum(p10_w_sub$plabor1000SV, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_ws,dmg_3,labor_ws)
tot_2<-sum(dmg_2,cont_ws,dmg_4,labor_ws)
tot_ws<-paste0(tot_1,"-",tot_2)

#buyout-scale
p10_w_sub<-p10_w[!is.na(p10_w$buyout),]
dmg_1<-round(sum(p10_w_sub$pDMG1000FIA, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_w_sub$pDMG1000SV, na.rm=T )/1000000,2)
dmg_rng_bo<-paste0(dmg_1,"-",dmg_2)
cont_bo<-round(sum(p10_w_sub$pDMGCONT1000FIA , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_w_sub$preloc1000HZ, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_w_sub$preloc1000SV, na.rm=T )/1000000,2)
reloc_bo<-paste0(dmg_3,"-",dmg_4)
labor_bo<-round(sum(p10_w_sub$plabor1000SV, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_bo,dmg_3,labor_bo)
tot_2<-sum(dmg_2,cont_bo,dmg_4,labor_bo)
tot_bo<-paste0(tot_1,"-",tot_2)

aqcost<-round(sum(p10_w_sub$pAq_Cost, na.rm=T)/1000000,2) #fix this
appr<-round(sum(p10_w_sub$pAPPR, na.rm=T)/1000000,2)

#vectors to go into table
damages_w<-c(dmg_rng_cnty, dmg_rng_ws, dmg_rng_bo)
cont_w<-c(cont_cnty,cont_ws,cont_bo)
reloc_w<-c(reloc_cnty,reloc_ws,reloc_bo)
labor_w<-c(labor_cnty,labor_ws,labor_bo)
tot_w<-c(tot_cnty,tot_ws,tot_bo)
aq_cost_w<-c(NA,NA,aqcost)
appr_w<-c(NA,NA,appr)

#Put together the table
Scenario<-c("No Buyouts","No Buyouts","No Buyouts",
            "With Buyouts Prior to 2010","With Buyouts Prior to 2010","With Buyouts Prior to 2010",
            "All Buyouts Prior to 2010", "All Buyouts Prior to 2010","All Buyouts Prior to 2010",
            "Wishlist Prior to 2010","Wishlist Prior to 2010","Wishlist Prior to 2010")
Scale<-c("County","Micro-watersheds with Buyouts","Buyout Parcels",
         "County","Micro-watersheds with Buyouts","Buyout Parcels",
         "County","Micro-watersheds with Buyouts","Buyout Parcels",
         "County","Micro-watersheds with Buyouts","Buyout Parcels")

Structural_Damages<-c(damages_nb,damages_ob,damages_ab,damages_w)
Contents_Damages<-c(cont_nb,cont_ob,cont_ab,cont_w)
Relocation_Costs<-c(reloc_nb,reloc_ob,reloc_ab,reloc_w)
Labor_Costs<-c(labor_nb,labor_ob,labor_ab,labor_w)
Total_Damages<-c(tot_nb,tot_ob,tot_ab,tot_w)
Acquisition<-c(aq_cost_nb,aq_cost_ob,aq_cost_ab,aq_cost_w)
Appraisals<-c(appr_nb,appr_ob,appr_ab,appr_w)

df<-data.frame(Scenario, Scale, Structural_Damages, Contents_Damages,Relocation_Costs,
               Labor_Costs, Total_Damages, Acquisition, Appraisals)

############################################################################################################
#Create Table of Summary Statistics for Economic Damage Estimates in 2010 based on Climate Scenario HAZUS 500 MODEL
############################################################################################################

pnames<-c( "finalnewparcels2007ob.RDS", "finalnewparcels2008ob.RDS", "finalnewparcels2009ob.RDS", "finalnewparcels2010ob.RDS", "finalnewparcels2011ob.RDS",
           "finalnewparcels2012ob.RDS", "finalnewparcels2013ob.RDS","finalnewparcels2014ob.RDS", 
           "finalnewparcels2007nb.RDS", "finalnewparcels2008nb.RDS", "finalnewparcels2009nb.RDS", "finalnewparcels2010nb.RDS", "finalnewparcels2011nb.RDS",
           "finalnewparcels2012nb.RDS", "finalnewparcels2013nb.RDS","finalnewparcels2014nb.RDS",
           "finalnewparcels2010ab.RDS", "finalnewparcels2011ab.RDS","finalnewparcels2012ab.RDS", "finalnewparcels2013ab.RDS","finalnewparcels2014ab.RDS",
           "finalnewparcels2010w.RDS", "finalnewparcels2011w.RDS","finalnewparcels2012w.RDS", "finalnewparcels2013w.RDS","finalnewparcels2014w.RDS") 


#No buyout scenario - with freeboard
p10_nb<-readRDS(paste0(wd,"dasym3", pnames[12]))
#p10_nb<-readRDS(paste0(wd,"Census", pnames[12]))
# p10_nb<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/", pnames[12]))

#county-scale
dmg_1<-round(sum(p10_nb$pDMG500FIA, na.rm=T )/1000000,2) #structural damages
dmg_2<-round(sum(p10_nb$pDMG500SV, na.rm=T )/1000000,2)
dmg_rng_cnty<-paste0(dmg_1,"-",dmg_2) #range of structrual damages
cont_cnty<-round(sum(p10_nb$pDMGCONT500FIA , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_nb$preloc500HZ, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_nb$preloc500SV, na.rm=T )/1000000,2)
reloc_cnty<-paste0(dmg_3,"-",dmg_4)
labor_cnty<-round(sum(p10_nb$plabor500SV, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_cnty,dmg_3,labor_cnty)
tot_2<-sum(dmg_2,cont_cnty, dmg_4,labor_cnty)
tot_cnty<-paste0(tot_1,"-",tot_2)

#watershed-scale
wish_wsid<-unique(p10_nb$WS_ID[!is.na(p10_nb$Future)]) #watershed id for watersheds with any homes on buyout wish list
p10_nb_sub<-p10_nb[p10_nb$WS_ID %in% wish_wsid,]
dmg_1<-round(sum(p10_nb_sub$pDMG500FIA, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_nb_sub$pDMG500SV, na.rm=T )/1000000,2)
dmg_rng_ws<-paste0(dmg_1,"-",dmg_2)
cont_ws<-round(sum(p10_nb_sub$pDMGCONT500FIA , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_nb_sub$preloc500HZ, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_nb_sub$preloc500SV, na.rm=T )/1000000,2)
reloc_ws<-paste0(dmg_3,"-",dmg_4)
labor_ws<-round(sum(p10_nb_sub$plabor500SV, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_ws,dmg_3,labor_ws)
tot_2<-sum(dmg_2,cont_ws,dmg_4,labor_ws)
tot_ws<-paste0(tot_1,"-",tot_2)

#buyout-scale
p10_nb_sub<-p10_nb[!is.na(p10_nb$buyout),]
dmg_1<-round(sum(p10_nb_sub$pDMG500FIA, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_nb_sub$pDMG500SV, na.rm=T )/1000000,2)
dmg_rng_bo<-paste0(dmg_1,"-",dmg_2)
cont_bo<-round(sum(p10_nb_sub$pDMGCONT500FIA , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_nb_sub$preloc500HZ, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_nb_sub$preloc500SV, na.rm=T )/1000000,2)
reloc_bo<-paste0(dmg_3,"-",dmg_4)
labor_bo<-round(sum(p10_nb_sub$plabor500SV, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_bo,dmg_3,labor_bo)
tot_2<-sum(dmg_2,cont_bo,dmg_4,labor_bo)
tot_bo<-paste0(tot_1,"-",tot_2)

aqcost<-round(sum(p10_nb_sub$pAq_Cost, na.rm=T)/1000000,2) #fix this
appr<-round(sum(p10_nb_sub$pAPPR, na.rm=T)/1000000,2)

#vectors to go into table
damages_nb<-c(dmg_rng_cnty, dmg_rng_ws, dmg_rng_bo)
cont_nb<-c(cont_cnty,cont_ws,cont_bo)
reloc_nb<-c(reloc_cnty,reloc_ws,reloc_bo)
labor_nb<-c(labor_cnty,labor_ws,labor_bo)
tot_nb<-c(tot_cnty,tot_ws,tot_bo)
aq_cost_nb<-c(NA,NA,aqcost)
appr_nb<-c(NA,NA,appr)


#Observed buyout scenario - with freeboard
p10_ob<-readRDS(paste0(wd,"dasym3",pnames[4]))
#p10_ob<-readRDS(paste0(wd,"Census",pnames[4]))
#p10_ob<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/","dasym3",pnames[4]))
#p10_ob<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/", pnames[4]))
#county-scale
dmg_1<-round(sum(p10_ob$pDMG500FIA, na.rm=T )/1000000,2) #structural damages
dmg_2<-round(sum(p10_ob$pDMG500SV, na.rm=T )/1000000,2)
dmg_rng_cnty<-paste0(dmg_1,"-",dmg_2) #range of structrual damages
cont_cnty<-round(sum(p10_ob$pDMGCONT500FIA , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_ob$preloc500HZ, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_ob$preloc500SV, na.rm=T )/1000000,2)
reloc_cnty<-paste0(dmg_3,"-",dmg_4)
labor_cnty<-round(sum(p10_ob$plabor500SV, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_cnty,dmg_3,labor_cnty)
tot_2<-sum(dmg_2,cont_cnty, dmg_4,labor_cnty)
tot_cnty<-paste0(tot_1,"-",tot_2)

#watershed-scale
# wish_wsid<-unique(p10_ob$WS_ID[!is.na(p10_ob$Future)]) #watershed id for watersheds with any homes on buyout wish list
p10_ob_sub<-p10_ob[p10_ob$WS_ID %in% wish_wsid,]
dmg_1<-round(sum(p10_ob_sub$pDMG500FIA, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_ob_sub$pDMG500SV, na.rm=T )/1000000,2)
dmg_rng_ws<-paste0(dmg_1,"-",dmg_2)
cont_ws<-round(sum(p10_ob_sub$pDMGCONT500FIA , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_ob_sub$preloc500HZ, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_ob_sub$preloc500SV, na.rm=T )/1000000,2)
reloc_ws<-paste0(dmg_3,"-",dmg_4)
labor_ws<-round(sum(p10_ob_sub$plabor500SV, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_ws,dmg_3,labor_ws)
tot_2<-sum(dmg_2,cont_ws,dmg_4,labor_ws)
tot_ws<-paste0(tot_1,"-",tot_2)

#buyout-scale
p10_ob_sub<-p10_ob[!is.na(p10_ob$buyout),]
dmg_1<-round(sum(p10_ob_sub$pDMG500FIA, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_ob_sub$pDMG500SV, na.rm=T )/1000000,2)
dmg_rng_bo<-paste0(dmg_1,"-",dmg_2)
cont_bo<-round(sum(p10_ob_sub$pDMGCONT500FIA , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_ob_sub$preloc500HZ, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_ob_sub$preloc500SV, na.rm=T )/1000000,2)
reloc_bo<-paste0(dmg_3,"-",dmg_4)
labor_bo<-round(sum(p10_ob_sub$plabor500SV, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_bo,dmg_3,labor_bo)
tot_2<-sum(dmg_2,cont_bo,dmg_4,labor_bo)
tot_bo<-paste0(tot_1,"-",tot_2)

aqcost<-round(sum(p10_ob_sub$pAq_Cost, na.rm=T)/1000000,2) #fix this
appr<-round(sum(p10_ob_sub$pAPPR, na.rm=T)/1000000,2)

#vectors to go into table
damages_ob<-c(dmg_rng_cnty, dmg_rng_ws, dmg_rng_bo)
cont_ob<-c(cont_cnty,cont_ws,cont_bo)
reloc_ob<-c(reloc_cnty,reloc_ws,reloc_bo)
labor_ob<-c(labor_cnty,labor_ws,labor_bo)
tot_ob<-c(tot_cnty,tot_ws,tot_bo)
aq_cost_ob<-c(NA,NA,aqcost)
appr_ob<-c(NA,NA,appr)

#All buyout scenario - with freeboard
p10_ab<-readRDS(paste0(wd,"dasym3",pnames[17]))
#p10_ab<-readRDS(paste0(wd,"Census",pnames[17]))

# p10_ab<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/","dasym3",pnames[17]))
# p10_ab<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/", pnames[17]))
#county-scale
dmg_1<-round(sum(p10_ab$pDMG500FIA, na.rm=T )/1000000,2) #structural damages
dmg_2<-round(sum(p10_ab$pDMG500SV, na.rm=T )/1000000,2)
dmg_rng_cnty<-paste0(dmg_1,"-",dmg_2) #range of structrual damages
cont_cnty<-round(sum(p10_ab$pDMGCONT500FIA , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_ab$preloc500HZ, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_ab$preloc500SV, na.rm=T )/1000000,2)
reloc_cnty<-paste0(dmg_3,"-",dmg_4)
labor_cnty<-round(sum(p10_ab$plabor500SV, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_cnty,dmg_3,labor_cnty)
tot_2<-sum(dmg_2,cont_cnty, dmg_4,labor_cnty)
tot_cnty<-paste0(tot_1,"-",tot_2)

#watershed-scale
# wish_wsid<-unique(p10_ab$WS_ID[!is.na(p10_ab$Future)]) #watershed id for watersheds with any homes on buyout wish list
p10_ab_sub<-p10_ab[p10_ab$WS_ID %in% wish_wsid,]
dmg_1<-round(sum(p10_ab_sub$pDMG500FIA, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_ab_sub$pDMG500SV, na.rm=T )/1000000,2)
dmg_rng_ws<-paste0(dmg_1,"-",dmg_2)
cont_ws<-round(sum(p10_ab_sub$pDMGCONT500FIA , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_ab_sub$preloc500HZ, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_ab_sub$preloc500SV, na.rm=T )/1000000,2)
reloc_ws<-paste0(dmg_3,"-",dmg_4)
labor_ws<-round(sum(p10_ab_sub$plabor500SV, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_ws,dmg_3,labor_ws)
tot_2<-sum(dmg_2,cont_ws,dmg_4,labor_ws)
tot_ws<-paste0(tot_1,"-",tot_2)

#buyout-scale
p10_ab_sub<-p10_ab[!is.na(p10_ab$buyout),]
dmg_1<-round(sum(p10_ab_sub$pDMG500FIA, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_ab_sub$pDMG500SV, na.rm=T )/1000000,2)
dmg_rng_bo<-paste0(dmg_1,"-",dmg_2)
cont_bo<-round(sum(p10_ab_sub$pDMGCONT500FIA , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_ab_sub$preloc500HZ, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_ab_sub$preloc500SV, na.rm=T )/1000000,2)
reloc_bo<-paste0(dmg_3,"-",dmg_4)
labor_bo<-round(sum(p10_ab_sub$plabor500SV, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_bo,dmg_3,labor_bo)
tot_2<-sum(dmg_2,cont_bo,dmg_4,labor_bo)
tot_bo<-paste0(tot_1,"-",tot_2)

aqcost<-round(sum(p10_ab_sub$pAq_Cost, na.rm=T)/1000000,2) #fix this
appr<-round(sum(p10_ab_sub$pAPPR, na.rm=T)/1000000,2)

#vectors to go into table
damages_ab<-c(dmg_rng_cnty, dmg_rng_ws, dmg_rng_bo)
cont_ab<-c(cont_cnty,cont_ws,cont_bo)
reloc_ab<-c(reloc_cnty,reloc_ws,reloc_bo)
labor_ab<-c(labor_cnty,labor_ws,labor_bo)
tot_ab<-c(tot_cnty,tot_ws,tot_bo)
aq_cost_ab<-c(NA,NA,aqcost)
appr_ab<-c(NA,NA,appr)

#Wishlist scenario - with freeboard
p10_w<-readRDS(paste0(wd,"dasym3",pnames[22]))
#p10_w<-readRDS(paste0(wd,"Census",pnames[22]))

# p10_w<-readRDS(paste0("E:/kate/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/",pnames[22]))
# p10_w<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/", pnames[22]))
#county-scale
dmg_1<-round(sum(p10_w$pDMG500FIA, na.rm=T )/1000000,2) #structural damages
dmg_2<-round(sum(p10_w$pDMG500SV, na.rm=T )/1000000,2)
dmg_rng_cnty<-paste0(dmg_1,"-",dmg_2) #range of structrual damages
cont_cnty<-round(sum(p10_w$pDMGCONT500FIA , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_w$preloc500HZ, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_w$preloc500SV, na.rm=T )/1000000,2)
reloc_cnty<-paste0(dmg_3,"-",dmg_4)
labor_cnty<-round(sum(p10_w$plabor500SV, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_cnty,dmg_3,labor_cnty)
tot_2<-sum(dmg_2,cont_cnty, dmg_4,labor_cnty)
tot_cnty<-paste0(tot_1,"-",tot_2)

#watershed-scale
p10_w_sub<-p10_w[p10_w$WS_ID %in% wish_wsid,]
dmg_1<-round(sum(p10_w_sub$pDMG500FIA, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_w_sub$pDMG500SV, na.rm=T )/1000000,2)
dmg_rng_ws<-paste0(dmg_1,"-",dmg_2)
cont_ws<-round(sum(p10_w_sub$pDMGCONT500FIA , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_w_sub$preloc500HZ, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_w_sub$preloc500SV, na.rm=T )/1000000,2)
reloc_ws<-paste0(dmg_3,"-",dmg_4)
labor_ws<-round(sum(p10_w_sub$plabor500SV, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_ws,dmg_3,labor_ws)
tot_2<-sum(dmg_2,cont_ws,dmg_4,labor_ws)
tot_ws<-paste0(tot_1,"-",tot_2)

#buyout-scale
p10_w_sub<-p10_w[!is.na(p10_w$buyout),]
dmg_1<-round(sum(p10_w_sub$pDMG500FIA, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_w_sub$pDMG500SV, na.rm=T )/1000000,2)
dmg_rng_bo<-paste0(dmg_1,"-",dmg_2)
cont_bo<-round(sum(p10_w_sub$pDMGCONT500FIA , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_w_sub$preloc500HZ, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_w_sub$preloc500SV, na.rm=T )/1000000,2)
reloc_bo<-paste0(dmg_3,"-",dmg_4)
labor_bo<-round(sum(p10_w_sub$plabor500SV, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_bo,dmg_3,labor_bo)
tot_2<-sum(dmg_2,cont_bo,dmg_4,labor_bo)
tot_bo<-paste0(tot_1,"-",tot_2)

aqcost<-round(sum(p10_w_sub$pAq_Cost, na.rm=T)/1000000,2) #fix this
appr<-round(sum(p10_w_sub$pAPPR, na.rm=T)/1000000,2)

#vectors to go into table
damages_w<-c(dmg_rng_cnty, dmg_rng_ws, dmg_rng_bo)
cont_w<-c(cont_cnty,cont_ws,cont_bo)
reloc_w<-c(reloc_cnty,reloc_ws,reloc_bo)
labor_w<-c(labor_cnty,labor_ws,labor_bo)
tot_w<-c(tot_cnty,tot_ws,tot_bo)
aq_cost_w<-c(NA,NA,aqcost)
appr_w<-c(NA,NA,appr)

#Put together the table
Scenario<-c("No Buyouts","No Buyouts","No Buyouts",
            "With Buyouts Prior to 2010","With Buyouts Prior to 2010","With Buyouts Prior to 2010",
            "All Buyouts Prior to 2010", "All Buyouts Prior to 2010","All Buyouts Prior to 2010",
            "Wishlist Prior to 2010","Wishlist Prior to 2010","Wishlist Prior to 2010")
Scale<-c("County","Micro-watersheds with Buyouts","Buyout Parcels",
         "County","Micro-watersheds with Buyouts","Buyout Parcels",
         "County","Micro-watersheds with Buyouts","Buyout Parcels",
         "County","Micro-watersheds with Buyouts","Buyout Parcels")

Structural_Damages<-c(damages_nb,damages_ob,damages_ab,damages_w)
Contents_Damages<-c(cont_nb,cont_ob,cont_ab,cont_w)
Relocation_Costs<-c(reloc_nb,reloc_ob,reloc_ab,reloc_w)
Labor_Costs<-c(labor_nb,labor_ob,labor_ab,labor_w)
Total_Damages<-c(tot_nb,tot_ob,tot_ab,tot_w)
Acquisition<-c(aq_cost_nb,aq_cost_ob,aq_cost_ab,aq_cost_w)
Appraisals<-c(appr_nb,appr_ob,appr_ab,appr_w)

df<-data.frame(Scenario, Scale, Structural_Damages, Contents_Damages,Relocation_Costs,
               Labor_Costs, Total_Damages, Acquisition, Appraisals)

############################################################################################################
#Create Table of Summary Statistics for Economic Damage Estimates in 2010 based on Climate Scenario HAZUS 500 MODELand no-freeboard 
############################################################################################################

pnames<-c( "finalnewparcels2007ob.RDS", "finalnewparcels2008ob.RDS", "finalnewparcels2009ob.RDS", "finalnewparcels2010ob.RDS", "finalnewparcels2011ob.RDS",
           "finalnewparcels2012ob.RDS", "finalnewparcels2013ob.RDS","finalnewparcels2014ob.RDS", 
           "finalnewparcels2007nb.RDS", "finalnewparcels2008nb.RDS", "finalnewparcels2009nb.RDS", "finalnewparcels2010nb.RDS", "finalnewparcels2011nb.RDS",
           "finalnewparcels2012nb.RDS", "finalnewparcels2013nb.RDS","finalnewparcels2014nb.RDS",
           "finalnewparcels2010ab.RDS", "finalnewparcels2011ab.RDS","finalnewparcels2012ab.RDS", "finalnewparcels2013ab.RDS","finalnewparcels2014ab.RDS",
           "finalnewparcels2010w.RDS", "finalnewparcels2011w.RDS","finalnewparcels2012w.RDS", "finalnewparcels2013w.RDS","finalnewparcels2014w.RDS") 


#No buyout scenario - with freeboard
p10_nb<-readRDS(paste0(wd,"dasym3", pnames[12]))
#p10_nb<-readRDS(paste0(wd,"Census", pnames[12]))
# p10_nb<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/", pnames[12]))

#county-scale
dmg_1<-round(sum(p10_nb$pDMG500FIAfreeb, na.rm=T )/1000000,2) #structural damages
dmg_2<-round(sum(p10_nb$pDMG500SVfreeb, na.rm=T )/1000000,2)
dmg_rng_cnty<-paste0(dmg_1,"-",dmg_2) #range of structrual damages
cont_cnty<-round(sum(p10_nb$pDMGCONT500FIAfreeb , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_nb$preloc500HZfreeb, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_nb$preloc500SVfreeb, na.rm=T )/1000000,2)
reloc_cnty<-paste0(dmg_3,"-",dmg_4)
labor_cnty<-round(sum(p10_nb$plabor500SVfreeb, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_cnty,dmg_3,labor_cnty)
tot_2<-sum(dmg_2,cont_cnty, dmg_4,labor_cnty)
tot_cnty<-paste0(tot_1,"-",tot_2)

#watershed-scale
wish_wsid<-unique(p10_nb$WS_ID[!is.na(p10_nb$Future)]) #watershed id for watersheds with any homes on buyout wish list
p10_nb_sub<-p10_nb[p10_nb$WS_ID %in% wish_wsid,]
dmg_1<-round(sum(p10_nb_sub$pDMG500FIAfreeb, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_nb_sub$pDMG500SVfreeb, na.rm=T )/1000000,2)
dmg_rng_ws<-paste0(dmg_1,"-",dmg_2)
cont_ws<-round(sum(p10_nb_sub$pDMGCONT500FIAfreeb , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_nb_sub$preloc500HZfreeb, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_nb_sub$preloc500SVfreeb, na.rm=T )/1000000,2)
reloc_ws<-paste0(dmg_3,"-",dmg_4)
labor_ws<-round(sum(p10_nb_sub$plabor500SVfreeb, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_ws,dmg_3,labor_ws)
tot_2<-sum(dmg_2,cont_ws,dmg_4,labor_ws)
tot_ws<-paste0(tot_1,"-",tot_2)

#buyout-scale
p10_nb_sub<-p10_nb[!is.na(p10_nb$buyout),]
dmg_1<-round(sum(p10_nb_sub$pDMG500FIAfreeb, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_nb_sub$pDMG500SVfreeb, na.rm=T )/1000000,2)
dmg_rng_bo<-paste0(dmg_1,"-",dmg_2)
cont_bo<-round(sum(p10_nb_sub$pDMGCONT500FIAfreeb , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_nb_sub$preloc500HZfreeb, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_nb_sub$preloc500SVfreeb, na.rm=T )/1000000,2)
reloc_bo<-paste0(dmg_3,"-",dmg_4)
labor_bo<-round(sum(p10_nb_sub$plabor500SVfreeb, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_bo,dmg_3,labor_bo)
tot_2<-sum(dmg_2,cont_bo,dmg_4,labor_bo)
tot_bo<-paste0(tot_1,"-",tot_2)

aqcost<-round(sum(p10_nb_sub$pAq_Cost, na.rm=T)/1000000,2) #fix this
appr<-round(sum(p10_nb_sub$pAPPR, na.rm=T)/1000000,2)

#vectors to go into table
damages_nb<-c(dmg_rng_cnty, dmg_rng_ws, dmg_rng_bo)
cont_nb<-c(cont_cnty,cont_ws,cont_bo)
reloc_nb<-c(reloc_cnty,reloc_ws,reloc_bo)
labor_nb<-c(labor_cnty,labor_ws,labor_bo)
tot_nb<-c(tot_cnty,tot_ws,tot_bo)
aq_cost_nb<-c(NA,NA,aqcost)
appr_nb<-c(NA,NA,appr)


#Observed buyout scenario - with freeboard
p10_ob<-readRDS(paste0(wd,"dasym3",pnames[4]))
#p10_ob<-readRDS(paste0(wd,"Census",pnames[4]))
#p10_ob<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/","dasym3",pnames[4]))
#p10_ob<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/", pnames[4]))
#county-scale
dmg_1<-round(sum(p10_ob$pDMG500FIAfreeb, na.rm=T )/1000000,2) #structural damages
dmg_2<-round(sum(p10_ob$pDMG500SVfreeb, na.rm=T )/1000000,2)
dmg_rng_cnty<-paste0(dmg_1,"-",dmg_2) #range of structrual damages
cont_cnty<-round(sum(p10_ob$pDMGCONT500FIAfreeb , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_ob$preloc500HZfreeb, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_ob$preloc500SVfreeb, na.rm=T )/1000000,2)
reloc_cnty<-paste0(dmg_3,"-",dmg_4)
labor_cnty<-round(sum(p10_ob$plabor500SVfreeb, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_cnty,dmg_3,labor_cnty)
tot_2<-sum(dmg_2,cont_cnty, dmg_4,labor_cnty)
tot_cnty<-paste0(tot_1,"-",tot_2)

#watershed-scale
# wish_wsid<-unique(p10_ob$WS_ID[!is.na(p10_ob$Future)]) #watershed id for watersheds with any homes on buyout wish list
p10_ob_sub<-p10_ob[p10_ob$WS_ID %in% wish_wsid,]
dmg_1<-round(sum(p10_ob_sub$pDMG500FIAfreeb, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_ob_sub$pDMG500SVfreeb, na.rm=T )/1000000,2)
dmg_rng_ws<-paste0(dmg_1,"-",dmg_2)
cont_ws<-round(sum(p10_ob_sub$pDMGCONT500FIAfreeb , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_ob_sub$preloc500HZfreeb, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_ob_sub$preloc500SVfreeb, na.rm=T )/1000000,2)
reloc_ws<-paste0(dmg_3,"-",dmg_4)
labor_ws<-round(sum(p10_ob_sub$plabor500SVfreeb, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_ws,dmg_3,labor_ws)
tot_2<-sum(dmg_2,cont_ws,dmg_4,labor_ws)
tot_ws<-paste0(tot_1,"-",tot_2)

#buyout-scale
p10_ob_sub<-p10_ob[!is.na(p10_ob$buyout),]
dmg_1<-round(sum(p10_ob_sub$pDMG500FIAfreeb, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_ob_sub$pDMG500SVfreeb, na.rm=T )/1000000,2)
dmg_rng_bo<-paste0(dmg_1,"-",dmg_2)
cont_bo<-round(sum(p10_ob_sub$pDMGCONT500FIAfreeb , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_ob_sub$preloc500HZfreeb, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_ob_sub$preloc500SVfreeb, na.rm=T )/1000000,2)
reloc_bo<-paste0(dmg_3,"-",dmg_4)
labor_bo<-round(sum(p10_ob_sub$plabor500SVfreeb, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_bo,dmg_3,labor_bo)
tot_2<-sum(dmg_2,cont_bo,dmg_4,labor_bo)
tot_bo<-paste0(tot_1,"-",tot_2)

aqcost<-round(sum(p10_ob_sub$pAq_Cost, na.rm=T)/1000000,2) #fix this
appr<-round(sum(p10_ob_sub$pAPPR, na.rm=T)/1000000,2)

#vectors to go into table
damages_ob<-c(dmg_rng_cnty, dmg_rng_ws, dmg_rng_bo)
cont_ob<-c(cont_cnty,cont_ws,cont_bo)
reloc_ob<-c(reloc_cnty,reloc_ws,reloc_bo)
labor_ob<-c(labor_cnty,labor_ws,labor_bo)
tot_ob<-c(tot_cnty,tot_ws,tot_bo)
aq_cost_ob<-c(NA,NA,aqcost)
appr_ob<-c(NA,NA,appr)

#All buyout scenario - with freeboard
p10_ab<-readRDS(paste0(wd,"dasym3",pnames[17]))
#p10_ab<-readRDS(paste0(wd,"Census",pnames[17]))

# p10_ab<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/","dasym3",pnames[17]))
# p10_ab<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/", pnames[17]))
#county-scale
dmg_1<-round(sum(p10_ab$pDMG500FIAfreeb, na.rm=T )/1000000,2) #structural damages
dmg_2<-round(sum(p10_ab$pDMG500SVfreeb, na.rm=T )/1000000,2)
dmg_rng_cnty<-paste0(dmg_1,"-",dmg_2) #range of structrual damages
cont_cnty<-round(sum(p10_ab$pDMGCONT500FIAfreeb , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_ab$preloc500HZfreeb, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_ab$preloc500SVfreeb, na.rm=T )/1000000,2)
reloc_cnty<-paste0(dmg_3,"-",dmg_4)
labor_cnty<-round(sum(p10_ab$plabor500SVfreeb, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_cnty,dmg_3,labor_cnty)
tot_2<-sum(dmg_2,cont_cnty, dmg_4,labor_cnty)
tot_cnty<-paste0(tot_1,"-",tot_2)

#watershed-scale
# wish_wsid<-unique(p10_ab$WS_ID[!is.na(p10_ab$Future)]) #watershed id for watersheds with any homes on buyout wish list
p10_ab_sub<-p10_ab[p10_ab$WS_ID %in% wish_wsid,]
dmg_1<-round(sum(p10_ab_sub$pDMG500FIAfreeb, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_ab_sub$pDMG500SVfreeb, na.rm=T )/1000000,2)
dmg_rng_ws<-paste0(dmg_1,"-",dmg_2)
cont_ws<-round(sum(p10_ab_sub$pDMGCONT500FIAfreeb , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_ab_sub$preloc500HZfreeb, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_ab_sub$preloc500SVfreeb, na.rm=T )/1000000,2)
reloc_ws<-paste0(dmg_3,"-",dmg_4)
labor_ws<-round(sum(p10_ab_sub$plabor500SVfreeb, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_ws,dmg_3,labor_ws)
tot_2<-sum(dmg_2,cont_ws,dmg_4,labor_ws)
tot_ws<-paste0(tot_1,"-",tot_2)

#buyout-scale
p10_ab_sub<-p10_ab[!is.na(p10_ab$buyout),]
dmg_1<-round(sum(p10_ab_sub$pDMG500FIAfreeb, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_ab_sub$pDMG500SVfreeb, na.rm=T )/1000000,2)
dmg_rng_bo<-paste0(dmg_1,"-",dmg_2)
cont_bo<-round(sum(p10_ab_sub$pDMGCONT500FIAfreeb , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_ab_sub$preloc500HZfreeb, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_ab_sub$preloc500SVfreeb, na.rm=T )/1000000,2)
reloc_bo<-paste0(dmg_3,"-",dmg_4)
labor_bo<-round(sum(p10_ab_sub$plabor500SVfreeb, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_bo,dmg_3,labor_bo)
tot_2<-sum(dmg_2,cont_bo,dmg_4,labor_bo)
tot_bo<-paste0(tot_1,"-",tot_2)

aqcost<-round(sum(p10_ab_sub$pAq_Cost, na.rm=T)/1000000,2) #fix this
appr<-round(sum(p10_ab_sub$pAPPR, na.rm=T)/1000000,2)

#vectors to go into table
damages_ab<-c(dmg_rng_cnty, dmg_rng_ws, dmg_rng_bo)
cont_ab<-c(cont_cnty,cont_ws,cont_bo)
reloc_ab<-c(reloc_cnty,reloc_ws,reloc_bo)
labor_ab<-c(labor_cnty,labor_ws,labor_bo)
tot_ab<-c(tot_cnty,tot_ws,tot_bo)
aq_cost_ab<-c(NA,NA,aqcost)
appr_ab<-c(NA,NA,appr)

#Wishlist scenario - with freeboard
p10_w<-readRDS(paste0(wd,"dasym3",pnames[22]))
#p10_w<-readRDS(paste0(wd,"Census",pnames[22]))

# p10_w<-readRDS(paste0("E:/kate/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/",pnames[22]))
# p10_w<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/", pnames[22]))
#county-scale
dmg_1<-round(sum(p10_w$pDMG500FIAfreeb, na.rm=T )/1000000,2) #structural damages
dmg_2<-round(sum(p10_w$pDMG500SVfreeb, na.rm=T )/1000000,2)
dmg_rng_cnty<-paste0(dmg_1,"-",dmg_2) #range of structrual damages
cont_cnty<-round(sum(p10_w$pDMGCONT500FIAfreeb , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_w$preloc500HZfreeb, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_w$preloc500SVfreeb, na.rm=T )/1000000,2)
reloc_cnty<-paste0(dmg_3,"-",dmg_4)
labor_cnty<-round(sum(p10_w$plabor500SVfreeb, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_cnty,dmg_3,labor_cnty)
tot_2<-sum(dmg_2,cont_cnty, dmg_4,labor_cnty)
tot_cnty<-paste0(tot_1,"-",tot_2)

#watershed-scale
p10_w_sub<-p10_w[p10_w$WS_ID %in% wish_wsid,]
dmg_1<-round(sum(p10_w_sub$pDMG500FIAfreeb, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_w_sub$pDMG500SVfreeb, na.rm=T )/1000000,2)
dmg_rng_ws<-paste0(dmg_1,"-",dmg_2)
cont_ws<-round(sum(p10_w_sub$pDMGCONT500FIAfreeb , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_w_sub$preloc500HZfreeb, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_w_sub$preloc500SVfreeb, na.rm=T )/1000000,2)
reloc_ws<-paste0(dmg_3,"-",dmg_4)
labor_ws<-round(sum(p10_w_sub$plabor500SVfreeb, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_ws,dmg_3,labor_ws)
tot_2<-sum(dmg_2,cont_ws,dmg_4,labor_ws)
tot_ws<-paste0(tot_1,"-",tot_2)

#buyout-scale
p10_w_sub<-p10_w[!is.na(p10_w$buyout),]
dmg_1<-round(sum(p10_w_sub$pDMG500FIAfreeb, na.rm=T )/1000000,2)
dmg_2<-round(sum(p10_w_sub$pDMG500SVfreeb, na.rm=T )/1000000,2)
dmg_rng_bo<-paste0(dmg_1,"-",dmg_2)
cont_bo<-round(sum(p10_w_sub$pDMGCONT500FIAfreeb , na.rm=T )/1000000,2) #content damages
dmg_3<-round(sum(p10_w_sub$preloc500HZfreeb, na.rm=T )/1000000,2) #relocation costs
dmg_4<-round(sum(p10_w_sub$preloc500SVfreeb, na.rm=T )/1000000,2)
reloc_bo<-paste0(dmg_3,"-",dmg_4)
labor_bo<-round(sum(p10_w_sub$plabor500SVfreeb, na.rm=T)/1000000,2)#labor costs
tot_1<-sum(dmg_1,cont_bo,dmg_3,labor_bo)
tot_2<-sum(dmg_2,cont_bo,dmg_4,labor_bo)
tot_bo<-paste0(tot_1,"-",tot_2)

aqcost<-round(sum(p10_w_sub$pAq_Cost, na.rm=T)/1000000,2) #fix this
appr<-round(sum(p10_w_sub$pAPPR, na.rm=T)/1000000,2)

#vectors to go into table
damages_w<-c(dmg_rng_cnty, dmg_rng_ws, dmg_rng_bo)
cont_w<-c(cont_cnty,cont_ws,cont_bo)
reloc_w<-c(reloc_cnty,reloc_ws,reloc_bo)
labor_w<-c(labor_cnty,labor_ws,labor_bo)
tot_w<-c(tot_cnty,tot_ws,tot_bo)
aq_cost_w<-c(NA,NA,aqcost)
appr_w<-c(NA,NA,appr)

#Put together the table
Scenario<-c("No Buyouts","No Buyouts","No Buyouts",
            "With Buyouts Prior to 2010","With Buyouts Prior to 2010","With Buyouts Prior to 2010",
            "All Buyouts Prior to 2010", "All Buyouts Prior to 2010","All Buyouts Prior to 2010",
            "Wishlist Prior to 2010","Wishlist Prior to 2010","Wishlist Prior to 2010")
Scale<-c("County","Micro-watersheds with Buyouts","Buyout Parcels",
         "County","Micro-watersheds with Buyouts","Buyout Parcels",
         "County","Micro-watersheds with Buyouts","Buyout Parcels",
         "County","Micro-watersheds with Buyouts","Buyout Parcels")

Structural_Damages<-c(damages_nb,damages_ob,damages_ab,damages_w)
Contents_Damages<-c(cont_nb,cont_ob,cont_ab,cont_w)
Relocation_Costs<-c(reloc_nb,reloc_ob,reloc_ab,reloc_w)
Labor_Costs<-c(labor_nb,labor_ob,labor_ab,labor_w)
Total_Damages<-c(tot_nb,tot_ob,tot_ab,tot_w)
Acquisition<-c(aq_cost_nb,aq_cost_ob,aq_cost_ab,aq_cost_w)
Appraisals<-c(appr_nb,appr_ob,appr_ab,appr_w)

df<-data.frame(Scenario, Scale, Structural_Damages, Contents_Damages,Relocation_Costs,
               Labor_Costs, Total_Damages, Acquisition, Appraisals)



#####################################################################################################################################
#Create Table of Summary Statistics for Direct Damage Counts in 2010 based on Parcel-scale sf objects and Building-scale sf object for HAZUS 100
###################################################################################################################################
pnames<-c( "finalnewparcels2007ob.RDS", "finalnewparcels2008ob.RDS", "finalnewparcels2009ob.RDS", "finalnewparcels2010ob.RDS", "finalnewparcels2011ob.RDS",
           "finalnewparcels2012ob.RDS", "finalnewparcels2013ob.RDS","finalnewparcels2014ob.RDS", 
           "finalnewparcels2007nb.RDS", "finalnewparcels2008nb.RDS", "finalnewparcels2009nb.RDS", "finalnewparcels2010nb.RDS", "finalnewparcels2011nb.RDS",
           "finalnewparcels2012nb.RDS", "finalnewparcels2013nb.RDS","finalnewparcels2014nb.RDS",
           "finalnewparcels2010ab.RDS", "finalnewparcels2011ab.RDS","finalnewparcels2012ab.RDS", "finalnewparcels2013ab.RDS","finalnewparcels2014ab.RDS",
           "finalnewparcels2010w.RDS", "finalnewparcels2011w.RDS","finalnewparcels2012w.RDS", "finalnewparcels2013w.RDS","finalnewparcels2014w.RDS") 
#No buyout scenario - with freeboard

p10_nb<-readRDS(paste0(wd,"dasym3", pnames[12]))

#county-scale
pcnt<-length(p10_nb$STANPAR) #total parcels
pdicnt<-length(p10_nb$STANPAR[!is.na(p10_nb$pUSACE100) ]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_cnty<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_nb$STANPAR) #total bldgs
# bdicnt<-length(b10_nb$STANPAR[!is.na(b10_nb$DLEVEL)| !is.na(b10_nb$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_cnty<-bdicnt

popcnt<-sum(p10_nb$P.new,na.rm=T)
pop_di<-sum(p10_nb$P.new[!is.na(p10_nb$pUSACE100)], na.rm=T)
percpop<-pop_di/popcnt
pop_cnty<-pop_di

senpop_cnt<-sum(p10_nb$P.newsenior,na.rm=T)
senpop_di<-sum(p10_nb$P.newsenior[!is.na(p10_nb$pUSACE100)], na.rm=T)
percsenpop<-senpop_di/senpopcnt
senpop_cnty<-senpop_di

rentpopcnt<-sum(p10_nb$P.newrent,na.rm=T)
rentpop_di<-sum(p10_nb$P.newrent[!is.na(p10_nb$pUSACE100)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_cnty<-rentpop_di

#watershed-scale
wish_wsid<-unique(p10_nb$WS_ID[!is.na(p10_nb$Future)]) #watershed id for watersheds with any homes on buyout wish list
p10_nb_sub<-p10_nb[p10_nb$WS_ID %in% wish_wsid,]
pcnt<-length(p10_nb_sub$STANPAR) #total parcels
pdicnt<-length(p10_nb_sub$STANPAR[!is.na(p10_nb_sub$pUSACE100)]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_ws<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_nb$STANPAR) #total bldgs
# bdicnt<-length(b10_nb$STANPAR[!is.na(b10_nb$DLEVEL)| !is.na(b10_nb$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_ws<-bdicnt

popcnt<-sum(p10_nb_sub$P.new,na.rm=T) #tot population
pop_di<-sum(p10_nb_sub$P.new[!is.na(p10_nb$pUSACE100)], na.rm=T) #population in damaged orinundated buildings
percpop<-pop_di/popcnt
pop_ws<-pop_di

senpop_cnt<-sum(p10_nb_sub$P.newsenior,na.rm=T) #senior popualtion
senpop_di<-sum(p10_nb_sub$P.newsenior[!is.na(p10_nb$pUSACE100)], na.rm=T)
percsenpop<-senpop_di/senpopcnt
senpop_ws<-senpop_di

rentpopcnt<-sum(p10_nb_sub$P.newrent,na.rm=T) #renter population
rentpop_di<-sum(p10_nb_sub$P.newrent[!is.na(p10_nb$pUSACE100)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_ws<-rentpop_di


#buyout-scale
p10_nb_sub<-p10_nb[!is.na(p10_nb$buyout),]
pcnt<-length(p10_nb_sub$STANPAR) #total parcels
pdicnt<-length(p10_nb_sub$STANPAR[!is.na(p10_nb_sub$pUSACE100)]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_bo<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_nb$STANPAR) #total bldgs
# bdicnt<-length(b10_nb$STANPAR[!is.na(b10_nb$DLEVEL)| !is.na(b10_nb$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_bo<-bdicnt

popcnt<-sum(p10_nb_sub$P.new,na.rm=T) #tot population
pop_di<-sum(p10_nb_sub$P.new[!is.na(p10_nb$pUSACE100)], na.rm=T) #population in damaged orinundated buildings
percpop<-pop_di/popcnt
pop_bo<-pop_di

senpop_cnt<-sum(p10_nb_sub$P.newsenior,na.rm=T) #senior popualtion
senpop_di<-sum(p10_nb_sub$P.newsenior[!is.na(p10_nb$pUSACE100)], na.rm=T)
percsenpop<-senpop_di/senpopcnt
senpop_bo<-senpop_di

rentpopcnt<-sum(p10_nb_sub$P.newrent,na.rm=T) #renter population
rentpop_di<-sum(p10_nb_sub$P.newrent[!is.na(p10_nb$pUSACE100)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_bo<-rentpop_di


#vectors to go into table
parcels_nb<-c(p_cnty, p_ws, p_bo)
pop_nb<-c(pop_cnty,pop_ws,pop_bo)
senpop_nb<-c(senpop_cnty,senpop_ws,senpop_bo)
rentpop_nb<-c(rentpop_cnty,rentpop_ws,rentpop_bo)



#Observed buyout scenario - with freeboard
p10_ob<-readRDS(paste0(wd,"dasym3",pnames[4]))
#p10_ob<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/","dasym3",pnames[4]))

#county-scale
pcnt<-length(p10_ob$STANPAR) #total parcels
pdicnt<-length(p10_ob$STANPAR[!is.na(p10_ob$pUSACE100)]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_cnty<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_ob$STANPAR) #total bldgs
# bdicnt<-length(b10_ob$STANPAR[!is.na(b10_ob$DLEVEL)| !is.na(b10_ob$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_cnty<-bdicnt

popcnt<-sum(p10_ob$P.new,na.rm=T)
pop_di<-sum(p10_ob$P.new[!is.na(p10_ob$pUSACE100)], na.rm=T)
percpop<-pop_di/popcnt
pop_cnty<-pop_di

senpop_cnt<-sum(p10_ob$P.newsenior,na.rm=T)
senpop_di<-sum(p10_ob$P.newsenior[!is.na(p10_ob$pUSACE100)], na.rm=T)
percsenpop<-senpop_di/senpopcnt
senpop_cnty<-senpop_di

rentpopcnt<-sum(p10_ob$P.newrent,na.rm=T)
rentpop_di<-sum(p10_ob$P.newrent[!is.na(p10_ob$pUSACE100)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_cnty<-rentpop_di

#watershed-scale
#wish_wsid<-unique(p10_ob$WS_ID[!is.na(p10_ob$Future)]) #watershed id for watersheds with any homes on buyout wish list
p10_ob_sub<-p10_ob[p10_ob$WS_ID %in% wish_wsid,]
pcnt<-length(p10_ob_sub$STANPAR) #total parcels
pdicnt<-length(p10_ob_sub$STANPAR[!is.na(p10_ob_sub$pUSACE100)]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_ws<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_ob$STANPAR) #total bldgs
# bdicnt<-length(b10_ob$STANPAR[!is.na(b10_ob$DLEVEL)| !is.na(b10_ob$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_ws<-bdicnt

popcnt<-sum(p10_ob_sub$P.new,na.rm=T) #tot population
pop_di<-sum(p10_ob_sub$P.new[!is.na(p10_ob$pUSACE100)], na.rm=T) #population in damaged orinundated buildings
percpop<-pop_di/popcnt
pop_ws<-pop_di

senpop_cnt<-sum(p10_ob_sub$P.newsenior,na.rm=T) #senior popualtion
senpop_di<-sum(p10_ob_sub$P.newsenior[!is.na(p10_ob$pUSACE100)], na.rm=T)
percsenpop<-senpop_di/senpopcnt
senpop_ws<-senpop_di

rentpopcnt<-sum(p10_ob_sub$P.newrent,na.rm=T) #renter population
rentpop_di<-sum(p10_ob_sub$P.newrent[!is.na(p10_ob$pUSACE100)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_ws<-rentpop_di


#buyout-scale
p10_ob_sub<-p10_ob[!is.na(p10_ob$buyout),]
pcnt<-length(p10_ob_sub$STANPAR) #total parcels
pdicnt<-length(p10_ob_sub$STANPAR[!is.na(p10_ob_sub$pUSACE100)]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_bo<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_ob$STANPAR) #total bldgs
# bdicnt<-length(b10_ob$STANPAR[!is.na(b10_ob$DLEVEL)| !is.na(b10_ob$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_bo<-bdicnt

popcnt<-sum(p10_ob_sub$P.new,na.rm=T) #tot population
pop_di<-sum(p10_ob_sub$P.new[!is.na(p10_ob$pUSACE100)], na.rm=T) #population in damaged orinundated buildings
percpop<-pop_di/popcnt
pop_bo<-pop_di

senpop_cnt<-sum(p10_ob_sub$P.newsenior,na.rm=T) #senior popualtion
senpop_di<-sum(p10_ob_sub$P.newsenior[!is.na(p10_ob$pUSACE100)], na.rm=T)
percsenpop<-senpop_di/senpopcnt
senpop_bo<-senpop_di

rentpopcnt<-sum(p10_ob_sub$P.newrent,na.rm=T) #renter population
rentpop_di<-sum(p10_ob_sub$P.newrent[!is.na(p10_ob$pUSACE100)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_bo<-rentpop_di


#vectors to go into table
parcels_ob<-c(p_cnty, p_ws, p_bo)
pop_ob<-c(pop_cnty,pop_ws,pop_bo)
senpop_ob<-c(senpop_cnty,senpop_ws,senpop_bo)
rentpop_ob<-c(rentpop_cnty,rentpop_ws,rentpop_bo)


#All buyout scenario - with freeboard
p10_ab<-readRDS(paste0(wd,"dasym3",pnames[17]))
#p10_ab<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/","dasym3",pnames[17]))

#county-scale
pcnt<-length(p10_ab$STANPAR) #total parcels
pdicnt<-length(p10_ab$STANPAR[!is.na(p10_ab$pUSACE100)]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_cnty<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_ab$STANPAR) #total bldgs
# bdicnt<-length(b10_ab$STANPAR[!is.na(b10_ab$DLEVEL)| !is.na(b10_ab$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_cnty<-bdicnt

popcnt<-sum(p10_ab$P.new,na.rm=T)
pop_di<-sum(p10_ab$P.new[!is.na(p10_ab$pUSACE100)], na.rm=T)
percpop<-pop_di/popcnt
pop_cnty<-pop_di

senpop_cnt<-sum(p10_ab$P.newsenior,na.rm=T)
senpop_di<-sum(p10_ab$P.newsenior[!is.na(p10_ab$pUSACE100)], na.rm=T)
percsenpop<-senpop_di/senpopcnt
senpop_cnty<-senpop_di

rentpopcnt<-sum(p10_ab$P.newrent,na.rm=T)
rentpop_di<-sum(p10_ab$P.newrent[!is.na(p10_ab$pUSACE100)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_cnty<-rentpop_di

#watershed-scale
#wish_wsid<-unique(p10_ab$WS_ID[!is.na(p10_ab$Future)]) #watershed id for watersheds with any homes on buyout wish list
p10_ab_sub<-p10_ab[p10_ab$WS_ID %in% wish_wsid,]
pcnt<-length(p10_ab_sub$STANPAR) #total parcels
pdicnt<-length(p10_ab_sub$STANPAR[!is.na(p10_ab_sub$pUSACE100)]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_ws<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_ab$STANPAR) #total bldgs
# bdicnt<-length(b10_ab$STANPAR[!is.na(b10_ab$DLEVEL)| !is.na(b10_ab$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_ws<-bdicnt

popcnt<-sum(p10_ab_sub$P.new,na.rm=T) #tot population
pop_di<-sum(p10_ab_sub$P.new[!is.na(p10_ab$pUSACE100)], na.rm=T) #population in damaged orinundated buildings
percpop<-pop_di/popcnt
pop_ws<-pop_di

senpop_cnt<-sum(p10_ab_sub$P.newsenior,na.rm=T) #senior popualtion
senpop_di<-sum(p10_ab_sub$P.newsenior[!is.na(p10_ab$pUSACE100)], na.rm=T)
percsenpop<-senpop_di/senpopcnt
senpop_ws<-senpop_di

rentpopcnt<-sum(p10_ab_sub$P.newrent,na.rm=T) #renter population
rentpop_di<-sum(p10_ab_sub$P.newrent[!is.na(p10_ab$pUSACE100)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_ws<-rentpop_di


#buyout-scale
p10_ab_sub<-p10_ab[!is.na(p10_ab$buyout),]
pcnt<-length(p10_ab_sub$STANPAR) #total parcels
pdicnt<-length(p10_ab_sub$STANPAR[!is.na(p10_ab_sub$pUSACE100)]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_bo<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_ab$STANPAR) #total bldgs
# bdicnt<-length(b10_ab$STANPAR[!is.na(b10_ab$DLEVEL)| !is.na(b10_ab$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_bo<-bdicnt

popcnt<-sum(p10_ab_sub$P.new,na.rm=T) #tot population
pop_di<-sum(p10_ab_sub$P.new[!is.na(p10_ab$pUSACE100)], na.rm=T) #population in damaged orinundated buildings
percpop<-pop_di/popcnt
pop_bo<-pop_di

senpop_cnt<-sum(p10_ab_sub$P.newsenior,na.rm=T) #senior popualtion
senpop_di<-sum(p10_ab_sub$P.newsenior[!is.na(p10_ab$pUSACE100)], na.rm=T)
percsenpop<-senpop_di/senpopcnt
senpop_bo<-senpop_di

rentpopcnt<-sum(p10_ab_sub$P.newrent,na.rm=T) #renter population
rentpop_di<-sum(p10_ab_sub$P.newrent[!is.na(p10_ab$pUSACE100)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_bo<-rentpop_di


#vectors to go into table
parcels_ab<-c(p_cnty, p_ws, p_bo)
pop_ab<-c(pop_cnty,pop_ws,pop_bo)
senpop_ab<-c(senpop_cnty,senpop_ws,senpop_bo)
rentpop_ab<-c(rentpop_cnty,rentpop_ws,rentpop_bo)


#Wishlist scenario - with freeboard
p10_w<-readRDS(paste0(wd,"dasym3",pnames[22]))
#p10_w<-readRDS(paste0("E:/kate/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/",pnames[22]))

#county-scale
pcnt<-length(p10_w$STANPAR) #total parcels
pdicnt<-length(p10_w$STANPAR[!is.na(p10_w$pUSACE100)]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_cnty<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_w$STANPAR) #total bldgs
# bdicnt<-length(b10_w$STANPAR[!is.na(b10_w$DLEVEL)| !is.na(b10_w$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_cnty<-bdicnt

popcnt<-sum(p10_w$P.new,na.rm=T)
pop_di<-sum(p10_w$P.new[!is.na(p10_w$pUSACE100)], na.rm=T)
percpop<-pop_di/popcnt
pop_cnty<-pop_di

senpop_cnt<-sum(p10_w$P.newsenior,na.rm=T)
senpop_di<-sum(p10_w$P.newsenior[!is.na(p10_w$pUSACE100)], na.rm=T)
percsenpop<-senpop_di/senpopcnt
senpop_cnty<-senpop_di

rentpopcnt<-sum(p10_w$P.newrent,na.rm=T)
rentpop_di<-sum(p10_w$P.newrent[!is.na(p10_w$pUSACE100)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_cnty<-rentpop_di

#watershed-scale
p10_w_sub<-p10_w[p10_w$WS_ID %in% wish_wsid,]
pcnt<-length(p10_w_sub$STANPAR) #total parcels
pdicnt<-length(p10_w_sub$STANPAR[!is.na(p10_w_sub$pUSACE100)]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_ws<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_w$STANPAR) #total bldgs
# bdicnt<-length(b10_w$STANPAR[!is.na(b10_w$DLEVEL)| !is.na(b10_w$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_ws<-bdicnt

popcnt<-sum(p10_w_sub$P.new,na.rm=T) #tot population
pop_di<-sum(p10_w_sub$P.new[!is.na(p10_w$pUSACE100)], na.rm=T) #population in damaged orinundated buildings
percpop<-pop_di/popcnt
pop_ws<-pop_di

senpop_cnt<-sum(p10_w_sub$P.newsenior,na.rm=T) #senior popualtion
senpop_di<-sum(p10_w_sub$P.newsenior[!is.na(p10_w$pUSACE100)], na.rm=T)
percsenpop<-senpop_di/senpopcnt
senpop_ws<-senpop_di

rentpopcnt<-sum(p10_w_sub$P.newrent,na.rm=T) #renter population
rentpop_di<-sum(p10_w_sub$P.newrent[!is.na(p10_w$pUSACE100)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_ws<-rentpop_di


#buyout-scale
p10_w_sub<-p10_w[!is.na(p10_w$buyout),]
pcnt<-length(p10_w_sub$STANPAR) #total parcels
pdicnt<-length(p10_w_sub$STANPAR[!is.na(p10_w_sub$pUSACE100)]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_bo<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_w$STANPAR) #total bldgs
# bdicnt<-length(b10_w$STANPAR[!is.na(b10_w$DLEVEL)| !is.na(b10_w$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_bo<-bdicnt

popcnt<-sum(p10_w_sub$P.new,na.rm=T) #tot population
pop_di<-sum(p10_w_sub$P.new[!is.na(p10_w$pUSACE100)], na.rm=T) #population in damaged orinundated buildings
percpop<-pop_di/popcnt
pop_bo<-pop_di

senpop_cnt<-sum(p10_w_sub$P.newsenior,na.rm=T) #senior popualtion
senpop_di<-sum(p10_w_sub$P.newsenior[!is.na(p10_w$pUSACE100)], na.rm=T)
percsenpop<-senpop_di/senpop_cnt
senpop_bo<-senpop_di

rentpopcnt<-sum(p10_w_sub$P.newrent,na.rm=T) #renter population
rentpop_di<-sum(p10_w_sub$P.newrent[!is.na(p10_w$pUSACE100)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_bo<-rentpop_di


#vectors to go into table
parcels_w<-c(p_cnty, p_ws, p_bo)
pop_w<-c(pop_cnty,pop_ws,pop_bo)
senpop_w<-c(senpop_cnty,senpop_ws,senpop_bo)
rentpop_w<-c(rentpop_cnty,rentpop_ws,rentpop_bo)


#Put together the table
Scenario<-c("No Buyouts","No Buyouts","No Buyouts",
            "With Buyouts Prior to 2010","With Buyouts Prior to 2010","With Buyouts Prior to 2010",
            "All Buyouts Prior to 2010", "All Buyouts Prior to 2010","All Buyouts Prior to 2010",
            "Wishlist Prior to 2010","Wishlist Prior to 2010","Wishlist Prior to 2010")
Scale<-c("County","Micro-watersheds with Buyouts","Buyout Parcels",
         "County","Micro-watersheds with Buyouts","Buyout Parcels",
         "County","Micro-watersheds with Buyouts","Buyout Parcels",
         "County","Micro-watersheds with Buyouts","Buyout Parcels")
Damaged_Properties<-c(parcels_nb,parcels_ob,parcels_ab,parcels_w)
Exposed_Population<-c(pop_nb,pop_ob,pop_ab,pop_w)
Exposed_Senior_Population<-c(senpop_nb,senpop_ob,senpop_ab,senpop_w)
Exposed_Renter_Population<-c(rentpop_nb,rentpop_ob,rentpop_ab,rentpop_w)

df<-data.frame(Scenario, Scale, Damaged_Properties,Exposed_Population,
               Exposed_Senior_Population,Exposed_Renter_Population)




####################################################################################################################################
#Create Table of Summary Statistics for Direct Damage Counts in 2010 based on Parcel-scale sf objects and Building-scale sf object for HAZUS 500
###################################################################################################################################

pnames<-c( "finalnewparcels2007ob.RDS", "finalnewparcels2008ob.RDS", "finalnewparcels2009ob.RDS", "finalnewparcels2010ob.RDS", "finalnewparcels2011ob.RDS",
           "finalnewparcels2012ob.RDS", "finalnewparcels2013ob.RDS","finalnewparcels2014ob.RDS", 
           "finalnewparcels2007nb.RDS", "finalnewparcels2008nb.RDS", "finalnewparcels2009nb.RDS", "finalnewparcels2010nb.RDS", "finalnewparcels2011nb.RDS",
           "finalnewparcels2012nb.RDS", "finalnewparcels2013nb.RDS","finalnewparcels2014nb.RDS",
           "finalnewparcels2010ab.RDS", "finalnewparcels2011ab.RDS","finalnewparcels2012ab.RDS", "finalnewparcels2013ab.RDS","finalnewparcels2014ab.RDS",
           "finalnewparcels2010w.RDS", "finalnewparcels2011w.RDS","finalnewparcels2012w.RDS", "finalnewparcels2013w.RDS","finalnewparcels2014w.RDS") 
#No buyout scenario - with freeboard

p10_nb<-readRDS(paste0(wd,"dasym3", pnames[12]))

#county-scale
pcnt<-length(p10_nb$STANPAR) #total parcels
pdicnt<-length(p10_nb$STANPAR[!is.na(p10_nb$pUSACE500) ]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_cnty<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_nb$STANPAR) #total bldgs
# bdicnt<-length(b10_nb$STANPAR[!is.na(b10_nb$DLEVEL)| !is.na(b10_nb$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_cnty<-bdicnt

popcnt<-sum(p10_nb$P.new,na.rm=T)
pop_di<-sum(p10_nb$P.new[!is.na(p10_nb$pUSACE500)], na.rm=T)
percpop<-pop_di/popcnt
pop_cnty<-pop_di

senpop_cnt<-sum(p10_nb$P.newsenior,na.rm=T)
senpop_di<-sum(p10_nb$P.newsenior[!is.na(p10_nb$pUSACE500)], na.rm=T)
percsenpop<-senpop_di/senpopcnt
senpop_cnty<-senpop_di

rentpopcnt<-sum(p10_nb$P.newrent,na.rm=T)
rentpop_di<-sum(p10_nb$P.newrent[!is.na(p10_nb$pUSACE500)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_cnty<-rentpop_di

#watershed-scale
wish_wsid<-unique(p10_nb$WS_ID[!is.na(p10_nb$Future)]) #watershed id for watersheds with any homes on buyout wish list
p10_nb_sub<-p10_nb[p10_nb$WS_ID %in% wish_wsid,]
pcnt<-length(p10_nb_sub$STANPAR) #total parcels
pdicnt<-length(p10_nb_sub$STANPAR[!is.na(p10_nb_sub$pUSACE500)]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_ws<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_nb$STANPAR) #total bldgs
# bdicnt<-length(b10_nb$STANPAR[!is.na(b10_nb$DLEVEL)| !is.na(b10_nb$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_ws<-bdicnt

popcnt<-sum(p10_nb_sub$P.new,na.rm=T) #tot population
pop_di<-sum(p10_nb_sub$P.new[!is.na(p10_nb$pUSACE500)], na.rm=T) #population in damaged orinundated buildings
percpop<-pop_di/popcnt
pop_ws<-pop_di

senpop_cnt<-sum(p10_nb_sub$P.newsenior,na.rm=T) #senior popualtion
senpop_di<-sum(p10_nb_sub$P.newsenior[!is.na(p10_nb$pUSACE500)], na.rm=T)
percsenpop<-senpop_di/senpopcnt
senpop_ws<-senpop_di

rentpopcnt<-sum(p10_nb_sub$P.newrent,na.rm=T) #renter population
rentpop_di<-sum(p10_nb_sub$P.newrent[!is.na(p10_nb$pUSACE500)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_ws<-rentpop_di


#buyout-scale
p10_nb_sub<-p10_nb[!is.na(p10_nb$buyout),]
pcnt<-length(p10_nb_sub$STANPAR) #total parcels
pdicnt<-length(p10_nb_sub$STANPAR[!is.na(p10_nb_sub$pUSACE500)]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_bo<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_nb$STANPAR) #total bldgs
# bdicnt<-length(b10_nb$STANPAR[!is.na(b10_nb$DLEVEL)| !is.na(b10_nb$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_bo<-bdicnt

popcnt<-sum(p10_nb_sub$P.new,na.rm=T) #tot population
pop_di<-sum(p10_nb_sub$P.new[!is.na(p10_nb$pUSACE500)], na.rm=T) #population in damaged orinundated buildings
percpop<-pop_di/popcnt
pop_bo<-pop_di

senpop_cnt<-sum(p10_nb_sub$P.newsenior,na.rm=T) #senior popualtion
senpop_di<-sum(p10_nb_sub$P.newsenior[!is.na(p10_nb$pUSACE500)], na.rm=T)
percsenpop<-senpop_di/senpopcnt
senpop_bo<-senpop_di

rentpopcnt<-sum(p10_nb_sub$P.newrent,na.rm=T) #renter population
rentpop_di<-sum(p10_nb_sub$P.newrent[!is.na(p10_nb$pUSACE500)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_bo<-rentpop_di


#vectors to go into table
parcels_nb<-c(p_cnty, p_ws, p_bo)
pop_nb<-c(pop_cnty,pop_ws,pop_bo)
senpop_nb<-c(senpop_cnty,senpop_ws,senpop_bo)
rentpop_nb<-c(rentpop_cnty,rentpop_ws,rentpop_bo)



#Observed buyout scenario - with freeboard
p10_ob<-readRDS(paste0(wd,"dasym3",pnames[4]))
#p10_ob<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/","dasym3",pnames[4]))

#county-scale
pcnt<-length(p10_ob$STANPAR) #total parcels
pdicnt<-length(p10_ob$STANPAR[!is.na(p10_ob$pUSACE500)]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_cnty<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_ob$STANPAR) #total bldgs
# bdicnt<-length(b10_ob$STANPAR[!is.na(b10_ob$DLEVEL)| !is.na(b10_ob$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_cnty<-bdicnt

popcnt<-sum(p10_ob$P.new,na.rm=T)
pop_di<-sum(p10_ob$P.new[!is.na(p10_ob$pUSACE500)], na.rm=T)
percpop<-pop_di/popcnt
pop_cnty<-pop_di

senpop_cnt<-sum(p10_ob$P.newsenior,na.rm=T)
senpop_di<-sum(p10_ob$P.newsenior[!is.na(p10_ob$pUSACE500)], na.rm=T)
percsenpop<-senpop_di/senpopcnt
senpop_cnty<-senpop_di

rentpopcnt<-sum(p10_ob$P.newrent,na.rm=T)
rentpop_di<-sum(p10_ob$P.newrent[!is.na(p10_ob$pUSACE500)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_cnty<-rentpop_di

#watershed-scale
#wish_wsid<-unique(p10_ob$WS_ID[!is.na(p10_ob$Future)]) #watershed id for watersheds with any homes on buyout wish list
p10_ob_sub<-p10_ob[p10_ob$WS_ID %in% wish_wsid,]
pcnt<-length(p10_ob_sub$STANPAR) #total parcels
pdicnt<-length(p10_ob_sub$STANPAR[!is.na(p10_ob_sub$pUSACE500)]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_ws<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_ob$STANPAR) #total bldgs
# bdicnt<-length(b10_ob$STANPAR[!is.na(b10_ob$DLEVEL)| !is.na(b10_ob$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_ws<-bdicnt

popcnt<-sum(p10_ob_sub$P.new,na.rm=T) #tot population
pop_di<-sum(p10_ob_sub$P.new[!is.na(p10_ob$pUSACE500)], na.rm=T) #population in damaged orinundated buildings
percpop<-pop_di/popcnt
pop_ws<-pop_di

senpop_cnt<-sum(p10_ob_sub$P.newsenior,na.rm=T) #senior popualtion
senpop_di<-sum(p10_ob_sub$P.newsenior[!is.na(p10_ob$pUSACE500)], na.rm=T)
percsenpop<-senpop_di/senpopcnt
senpop_ws<-senpop_di

rentpopcnt<-sum(p10_ob_sub$P.newrent,na.rm=T) #renter population
rentpop_di<-sum(p10_ob_sub$P.newrent[!is.na(p10_ob$pUSACE500)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_ws<-rentpop_di


#buyout-scale
p10_ob_sub<-p10_ob[!is.na(p10_ob$buyout),]
pcnt<-length(p10_ob_sub$STANPAR) #total parcels
pdicnt<-length(p10_ob_sub$STANPAR[!is.na(p10_ob_sub$pUSACE500)]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_bo<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_ob$STANPAR) #total bldgs
# bdicnt<-length(b10_ob$STANPAR[!is.na(b10_ob$DLEVEL)| !is.na(b10_ob$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_bo<-bdicnt

popcnt<-sum(p10_ob_sub$P.new,na.rm=T) #tot population
pop_di<-sum(p10_ob_sub$P.new[!is.na(p10_ob$pUSACE500)], na.rm=T) #population in damaged orinundated buildings
percpop<-pop_di/popcnt
pop_bo<-pop_di

senpop_cnt<-sum(p10_ob_sub$P.newsenior,na.rm=T) #senior popualtion
senpop_di<-sum(p10_ob_sub$P.newsenior[!is.na(p10_ob$pUSACE500)], na.rm=T)
percsenpop<-senpop_di/senpopcnt
senpop_bo<-senpop_di

rentpopcnt<-sum(p10_ob_sub$P.newrent,na.rm=T) #renter population
rentpop_di<-sum(p10_ob_sub$P.newrent[!is.na(p10_ob$pUSACE500)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_bo<-rentpop_di


#vectors to go into table
parcels_ob<-c(p_cnty, p_ws, p_bo)
pop_ob<-c(pop_cnty,pop_ws,pop_bo)
senpop_ob<-c(senpop_cnty,senpop_ws,senpop_bo)
rentpop_ob<-c(rentpop_cnty,rentpop_ws,rentpop_bo)


#All buyout scenario - with freeboard
p10_ab<-readRDS(paste0(wd,"dasym3",pnames[17]))
#p10_ab<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/","dasym3",pnames[17]))

#county-scale
pcnt<-length(p10_ab$STANPAR) #total parcels
pdicnt<-length(p10_ab$STANPAR[!is.na(p10_ab$pUSACE500)]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_cnty<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_ab$STANPAR) #total bldgs
# bdicnt<-length(b10_ab$STANPAR[!is.na(b10_ab$DLEVEL)| !is.na(b10_ab$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_cnty<-bdicnt

popcnt<-sum(p10_ab$P.new,na.rm=T)
pop_di<-sum(p10_ab$P.new[!is.na(p10_ab$pUSACE500)], na.rm=T)
percpop<-pop_di/popcnt
pop_cnty<-pop_di

senpop_cnt<-sum(p10_ab$P.newsenior,na.rm=T)
senpop_di<-sum(p10_ab$P.newsenior[!is.na(p10_ab$pUSACE500)], na.rm=T)
percsenpop<-senpop_di/senpopcnt
senpop_cnty<-senpop_di

rentpopcnt<-sum(p10_ab$P.newrent,na.rm=T)
rentpop_di<-sum(p10_ab$P.newrent[!is.na(p10_ab$pUSACE500)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_cnty<-rentpop_di

#watershed-scale
#wish_wsid<-unique(p10_ab$WS_ID[!is.na(p10_ab$Future)]) #watershed id for watersheds with any homes on buyout wish list
p10_ab_sub<-p10_ab[p10_ab$WS_ID %in% wish_wsid,]
pcnt<-length(p10_ab_sub$STANPAR) #total parcels
pdicnt<-length(p10_ab_sub$STANPAR[!is.na(p10_ab_sub$pUSACE500)]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_ws<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_ab$STANPAR) #total bldgs
# bdicnt<-length(b10_ab$STANPAR[!is.na(b10_ab$DLEVEL)| !is.na(b10_ab$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_ws<-bdicnt

popcnt<-sum(p10_ab_sub$P.new,na.rm=T) #tot population
pop_di<-sum(p10_ab_sub$P.new[!is.na(p10_ab$pUSACE500)], na.rm=T) #population in damaged orinundated buildings
percpop<-pop_di/popcnt
pop_ws<-pop_di

senpop_cnt<-sum(p10_ab_sub$P.newsenior,na.rm=T) #senior popualtion
senpop_di<-sum(p10_ab_sub$P.newsenior[!is.na(p10_ab$pUSACE500)], na.rm=T)
percsenpop<-senpop_di/senpopcnt
senpop_ws<-senpop_di

rentpopcnt<-sum(p10_ab_sub$P.newrent,na.rm=T) #renter population
rentpop_di<-sum(p10_ab_sub$P.newrent[!is.na(p10_ab$pUSACE500)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_ws<-rentpop_di


#buyout-scale
p10_ab_sub<-p10_ab[!is.na(p10_ab$buyout),]
pcnt<-length(p10_ab_sub$STANPAR) #total parcels
pdicnt<-length(p10_ab_sub$STANPAR[!is.na(p10_ab_sub$pUSACE500)]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_bo<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_ab$STANPAR) #total bldgs
# bdicnt<-length(b10_ab$STANPAR[!is.na(b10_ab$DLEVEL)| !is.na(b10_ab$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_bo<-bdicnt

popcnt<-sum(p10_ab_sub$P.new,na.rm=T) #tot population
pop_di<-sum(p10_ab_sub$P.new[!is.na(p10_ab$pUSACE500)], na.rm=T) #population in damaged orinundated buildings
percpop<-pop_di/popcnt
pop_bo<-pop_di

senpop_cnt<-sum(p10_ab_sub$P.newsenior,na.rm=T) #senior popualtion
senpop_di<-sum(p10_ab_sub$P.newsenior[!is.na(p10_ab$pUSACE500)], na.rm=T)
percsenpop<-senpop_di/senpopcnt
senpop_bo<-senpop_di

rentpopcnt<-sum(p10_ab_sub$P.newrent,na.rm=T) #renter population
rentpop_di<-sum(p10_ab_sub$P.newrent[!is.na(p10_ab$pUSACE500)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_bo<-rentpop_di


#vectors to go into table
parcels_ab<-c(p_cnty, p_ws, p_bo)
pop_ab<-c(pop_cnty,pop_ws,pop_bo)
senpop_ab<-c(senpop_cnty,senpop_ws,senpop_bo)
rentpop_ab<-c(rentpop_cnty,rentpop_ws,rentpop_bo)


#Wishlist scenario - with freeboard
p10_w<-readRDS(paste0(wd,"dasym3",pnames[22]))
#p10_w<-readRDS(paste0("E:/kate/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/",pnames[22]))

#county-scale
pcnt<-length(p10_w$STANPAR) #total parcels
pdicnt<-length(p10_w$STANPAR[!is.na(p10_w$pUSACE500)]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_cnty<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_w$STANPAR) #total bldgs
# bdicnt<-length(b10_w$STANPAR[!is.na(b10_w$DLEVEL)| !is.na(b10_w$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_cnty<-bdicnt

popcnt<-sum(p10_w$P.new,na.rm=T)
pop_di<-sum(p10_w$P.new[!is.na(p10_w$pUSACE500)], na.rm=T)
percpop<-pop_di/popcnt
pop_cnty<-pop_di

senpop_cnt<-sum(p10_w$P.newsenior,na.rm=T)
senpop_di<-sum(p10_w$P.newsenior[!is.na(p10_w$pUSACE500)], na.rm=T)
percsenpop<-senpop_di/senpopcnt
senpop_cnty<-senpop_di

rentpopcnt<-sum(p10_w$P.newrent,na.rm=T)
rentpop_di<-sum(p10_w$P.newrent[!is.na(p10_w$pUSACE500)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_cnty<-rentpop_di

#watershed-scale
p10_w_sub<-p10_w[p10_w$WS_ID %in% wish_wsid,]
pcnt<-length(p10_w_sub$STANPAR) #total parcels
pdicnt<-length(p10_w_sub$STANPAR[!is.na(p10_w_sub$pUSACE500)]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_ws<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_w$STANPAR) #total bldgs
# bdicnt<-length(b10_w$STANPAR[!is.na(b10_w$DLEVEL)| !is.na(b10_w$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_ws<-bdicnt

popcnt<-sum(p10_w_sub$P.new,na.rm=T) #tot population
pop_di<-sum(p10_w_sub$P.new[!is.na(p10_w$pUSACE500)], na.rm=T) #population in damaged orinundated buildings
percpop<-pop_di/popcnt
pop_ws<-pop_di

senpop_cnt<-sum(p10_w_sub$P.newsenior,na.rm=T) #senior popualtion
senpop_di<-sum(p10_w_sub$P.newsenior[!is.na(p10_w$pUSACE500)], na.rm=T)
percsenpop<-senpop_di/senpopcnt
senpop_ws<-senpop_di

rentpopcnt<-sum(p10_w_sub$P.newrent,na.rm=T) #renter population
rentpop_di<-sum(p10_w_sub$P.newrent[!is.na(p10_w$pUSACE500)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_ws<-rentpop_di


#buyout-scale
p10_w_sub<-p10_w[!is.na(p10_w$buyout),]
pcnt<-length(p10_w_sub$STANPAR) #total parcels
pdicnt<-length(p10_w_sub$STANPAR[!is.na(p10_w_sub$pUSACE500)]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_bo<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_w$STANPAR) #total bldgs
# bdicnt<-length(b10_w$STANPAR[!is.na(b10_w$DLEVEL)| !is.na(b10_w$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_bo<-bdicnt

popcnt<-sum(p10_w_sub$P.new,na.rm=T) #tot population
pop_di<-sum(p10_w_sub$P.new[!is.na(p10_w$pUSACE500)], na.rm=T) #population in damaged orinundated buildings
percpop<-pop_di/popcnt
pop_bo<-pop_di

senpop_cnt<-sum(p10_w_sub$P.newsenior,na.rm=T) #senior popualtion
senpop_di<-sum(p10_w_sub$P.newsenior[!is.na(p10_w$pUSACE500)], na.rm=T)
percsenpop<-senpop_di/senpop_cnt
senpop_bo<-senpop_di

rentpopcnt<-sum(p10_w_sub$P.newrent,na.rm=T) #renter population
rentpop_di<-sum(p10_w_sub$P.newrent[!is.na(p10_w$pUSACE500)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_bo<-rentpop_di


#vectors to go into table
parcels_w<-c(p_cnty, p_ws, p_bo)
pop_w<-c(pop_cnty,pop_ws,pop_bo)
senpop_w<-c(senpop_cnty,senpop_ws,senpop_bo)
rentpop_w<-c(rentpop_cnty,rentpop_ws,rentpop_bo)


#Put together the table
Scenario<-c("No Buyouts","No Buyouts","No Buyouts",
            "With Buyouts Prior to 2010","With Buyouts Prior to 2010","With Buyouts Prior to 2010",
            "All Buyouts Prior to 2010", "All Buyouts Prior to 2010","All Buyouts Prior to 2010",
            "Wishlist Prior to 2010","Wishlist Prior to 2010","Wishlist Prior to 2010")
Scale<-c("County","Micro-watersheds with Buyouts","Buyout Parcels",
         "County","Micro-watersheds with Buyouts","Buyout Parcels",
         "County","Micro-watersheds with Buyouts","Buyout Parcels",
         "County","Micro-watersheds with Buyouts","Buyout Parcels")
Damaged_Properties<-c(parcels_nb,parcels_ob,parcels_ab,parcels_w)
Exposed_Population<-c(pop_nb,pop_ob,pop_ab,pop_w)
Exposed_Senior_Population<-c(senpop_nb,senpop_ob,senpop_ab,senpop_w)
Exposed_Renter_Population<-c(rentpop_nb,rentpop_ob,rentpop_ab,rentpop_w)

df<-data.frame(Scenario, Scale, Damaged_Properties,Exposed_Population,
               Exposed_Senior_Population,Exposed_Renter_Population)




####################################################################################################################################
#Create Table of Summary Statistics for Direct Damage Counts in 2010 based on Parcel-scale sf objects and Building-scale sf object for HAZUS 1000
###################################################################################################################################
pnames<-c( "finalnewparcels2007ob.RDS", "finalnewparcels2008ob.RDS", "finalnewparcels2009ob.RDS", "finalnewparcels2010ob.RDS", "finalnewparcels2011ob.RDS",
           "finalnewparcels2012ob.RDS", "finalnewparcels2013ob.RDS","finalnewparcels2014ob.RDS", 
           "finalnewparcels2007nb.RDS", "finalnewparcels2008nb.RDS", "finalnewparcels2009nb.RDS", "finalnewparcels2010nb.RDS", "finalnewparcels2011nb.RDS",
           "finalnewparcels2012nb.RDS", "finalnewparcels2013nb.RDS","finalnewparcels2014nb.RDS",
           "finalnewparcels2010ab.RDS", "finalnewparcels2011ab.RDS","finalnewparcels2012ab.RDS", "finalnewparcels2013ab.RDS","finalnewparcels2014ab.RDS",
           "finalnewparcels2010w.RDS", "finalnewparcels2011w.RDS","finalnewparcels2012w.RDS", "finalnewparcels2013w.RDS","finalnewparcels2014w.RDS") 
#No buyout scenario - with freeboard

p10_nb<-readRDS(paste0(wd,"dasym3", pnames[12]))

#county-scale
pcnt<-length(p10_nb$STANPAR) #total parcels
pdicnt<-length(p10_nb$STANPAR[!is.na(p10_nb$pUSACE1000) ]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_cnty<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_nb$STANPAR) #total bldgs
# bdicnt<-length(b10_nb$STANPAR[!is.na(b10_nb$DLEVEL)| !is.na(b10_nb$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_cnty<-bdicnt

popcnt<-sum(p10_nb$P.new,na.rm=T)
pop_di<-sum(p10_nb$P.new[!is.na(p10_nb$pUSACE1000)], na.rm=T)
percpop<-pop_di/popcnt
pop_cnty<-pop_di

senpop_cnt<-sum(p10_nb$P.newsenior,na.rm=T)
senpop_di<-sum(p10_nb$P.newsenior[!is.na(p10_nb$pUSACE1000)], na.rm=T)
percsenpop<-senpop_di/senpopcnt
senpop_cnty<-senpop_di

rentpopcnt<-sum(p10_nb$P.newrent,na.rm=T)
rentpop_di<-sum(p10_nb$P.newrent[!is.na(p10_nb$pUSACE1000)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_cnty<-rentpop_di

#watershed-scale
wish_wsid<-unique(p10_nb$WS_ID[!is.na(p10_nb$Future)]) #watershed id for watersheds with any homes on buyout wish list
p10_nb_sub<-p10_nb[p10_nb$WS_ID %in% wish_wsid,]
pcnt<-length(p10_nb_sub$STANPAR) #total parcels
pdicnt<-length(p10_nb_sub$STANPAR[!is.na(p10_nb_sub$pUSACE1000)]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_ws<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_nb$STANPAR) #total bldgs
# bdicnt<-length(b10_nb$STANPAR[!is.na(b10_nb$DLEVEL)| !is.na(b10_nb$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_ws<-bdicnt

popcnt<-sum(p10_nb_sub$P.new,na.rm=T) #tot population
pop_di<-sum(p10_nb_sub$P.new[!is.na(p10_nb$pUSACE1000)], na.rm=T) #population in damaged orinundated buildings
percpop<-pop_di/popcnt
pop_ws<-pop_di

senpop_cnt<-sum(p10_nb_sub$P.newsenior,na.rm=T) #senior popualtion
senpop_di<-sum(p10_nb_sub$P.newsenior[!is.na(p10_nb$pUSACE1000)], na.rm=T)
percsenpop<-senpop_di/senpopcnt
senpop_ws<-senpop_di

rentpopcnt<-sum(p10_nb_sub$P.newrent,na.rm=T) #renter population
rentpop_di<-sum(p10_nb_sub$P.newrent[!is.na(p10_nb$pUSACE1000)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_ws<-rentpop_di


#buyout-scale
p10_nb_sub<-p10_nb[!is.na(p10_nb$buyout),]
pcnt<-length(p10_nb_sub$STANPAR) #total parcels
pdicnt<-length(p10_nb_sub$STANPAR[!is.na(p10_nb_sub$pUSACE1000)]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_bo<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_nb$STANPAR) #total bldgs
# bdicnt<-length(b10_nb$STANPAR[!is.na(b10_nb$DLEVEL)| !is.na(b10_nb$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_bo<-bdicnt

popcnt<-sum(p10_nb_sub$P.new,na.rm=T) #tot population
pop_di<-sum(p10_nb_sub$P.new[!is.na(p10_nb$pUSACE1000)], na.rm=T) #population in damaged orinundated buildings
percpop<-pop_di/popcnt
pop_bo<-pop_di

senpop_cnt<-sum(p10_nb_sub$P.newsenior,na.rm=T) #senior popualtion
senpop_di<-sum(p10_nb_sub$P.newsenior[!is.na(p10_nb$pUSACE1000)], na.rm=T)
percsenpop<-senpop_di/senpopcnt
senpop_bo<-senpop_di

rentpopcnt<-sum(p10_nb_sub$P.newrent,na.rm=T) #renter population
rentpop_di<-sum(p10_nb_sub$P.newrent[!is.na(p10_nb$pUSACE1000)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_bo<-rentpop_di


#vectors to go into table
parcels_nb<-c(p_cnty, p_ws, p_bo)
pop_nb<-c(pop_cnty,pop_ws,pop_bo)
senpop_nb<-c(senpop_cnty,senpop_ws,senpop_bo)
rentpop_nb<-c(rentpop_cnty,rentpop_ws,rentpop_bo)



#Observed buyout scenario - with freeboard
p10_ob<-readRDS(paste0(wd,"dasym3",pnames[4]))
#p10_ob<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/","dasym3",pnames[4]))

#county-scale
pcnt<-length(p10_ob$STANPAR) #total parcels
pdicnt<-length(p10_ob$STANPAR[!is.na(p10_ob$pUSACE1000)]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_cnty<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_ob$STANPAR) #total bldgs
# bdicnt<-length(b10_ob$STANPAR[!is.na(b10_ob$DLEVEL)| !is.na(b10_ob$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_cnty<-bdicnt

popcnt<-sum(p10_ob$P.new,na.rm=T)
pop_di<-sum(p10_ob$P.new[!is.na(p10_ob$pUSACE1000)], na.rm=T)
percpop<-pop_di/popcnt
pop_cnty<-pop_di

senpop_cnt<-sum(p10_ob$P.newsenior,na.rm=T)
senpop_di<-sum(p10_ob$P.newsenior[!is.na(p10_ob$pUSACE1000)], na.rm=T)
percsenpop<-senpop_di/senpopcnt
senpop_cnty<-senpop_di

rentpopcnt<-sum(p10_ob$P.newrent,na.rm=T)
rentpop_di<-sum(p10_ob$P.newrent[!is.na(p10_ob$pUSACE1000)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_cnty<-rentpop_di

#watershed-scale
#wish_wsid<-unique(p10_ob$WS_ID[!is.na(p10_ob$Future)]) #watershed id for watersheds with any homes on buyout wish list
p10_ob_sub<-p10_ob[p10_ob$WS_ID %in% wish_wsid,]
pcnt<-length(p10_ob_sub$STANPAR) #total parcels
pdicnt<-length(p10_ob_sub$STANPAR[!is.na(p10_ob_sub$pUSACE1000)]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_ws<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_ob$STANPAR) #total bldgs
# bdicnt<-length(b10_ob$STANPAR[!is.na(b10_ob$DLEVEL)| !is.na(b10_ob$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_ws<-bdicnt

popcnt<-sum(p10_ob_sub$P.new,na.rm=T) #tot population
pop_di<-sum(p10_ob_sub$P.new[!is.na(p10_ob$pUSACE1000)], na.rm=T) #population in damaged orinundated buildings
percpop<-pop_di/popcnt
pop_ws<-pop_di

senpop_cnt<-sum(p10_ob_sub$P.newsenior,na.rm=T) #senior popualtion
senpop_di<-sum(p10_ob_sub$P.newsenior[!is.na(p10_ob$pUSACE1000)], na.rm=T)
percsenpop<-senpop_di/senpopcnt
senpop_ws<-senpop_di

rentpopcnt<-sum(p10_ob_sub$P.newrent,na.rm=T) #renter population
rentpop_di<-sum(p10_ob_sub$P.newrent[!is.na(p10_ob$pUSACE1000)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_ws<-rentpop_di


#buyout-scale
p10_ob_sub<-p10_ob[!is.na(p10_ob$buyout),]
pcnt<-length(p10_ob_sub$STANPAR) #total parcels
pdicnt<-length(p10_ob_sub$STANPAR[!is.na(p10_ob_sub$pUSACE1000)]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_bo<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_ob$STANPAR) #total bldgs
# bdicnt<-length(b10_ob$STANPAR[!is.na(b10_ob$DLEVEL)| !is.na(b10_ob$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_bo<-bdicnt

popcnt<-sum(p10_ob_sub$P.new,na.rm=T) #tot population
pop_di<-sum(p10_ob_sub$P.new[!is.na(p10_ob$pUSACE1000)], na.rm=T) #population in damaged orinundated buildings
percpop<-pop_di/popcnt
pop_bo<-pop_di

senpop_cnt<-sum(p10_ob_sub$P.newsenior,na.rm=T) #senior popualtion
senpop_di<-sum(p10_ob_sub$P.newsenior[!is.na(p10_ob$pUSACE1000)], na.rm=T)
percsenpop<-senpop_di/senpopcnt
senpop_bo<-senpop_di

rentpopcnt<-sum(p10_ob_sub$P.newrent,na.rm=T) #renter population
rentpop_di<-sum(p10_ob_sub$P.newrent[!is.na(p10_ob$pUSACE1000)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_bo<-rentpop_di


#vectors to go into table
parcels_ob<-c(p_cnty, p_ws, p_bo)
pop_ob<-c(pop_cnty,pop_ws,pop_bo)
senpop_ob<-c(senpop_cnty,senpop_ws,senpop_bo)
rentpop_ob<-c(rentpop_cnty,rentpop_ws,rentpop_bo)


#All buyout scenario - with freeboard
p10_ab<-readRDS(paste0(wd,"dasym3",pnames[17]))
#p10_ab<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/","dasym3",pnames[17]))

#county-scale
pcnt<-length(p10_ab$STANPAR) #total parcels
pdicnt<-length(p10_ab$STANPAR[!is.na(p10_ab$pUSACE1000)]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_cnty<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_ab$STANPAR) #total bldgs
# bdicnt<-length(b10_ab$STANPAR[!is.na(b10_ab$DLEVEL)| !is.na(b10_ab$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_cnty<-bdicnt

popcnt<-sum(p10_ab$P.new,na.rm=T)
pop_di<-sum(p10_ab$P.new[!is.na(p10_ab$pUSACE1000)], na.rm=T)
percpop<-pop_di/popcnt
pop_cnty<-pop_di

senpop_cnt<-sum(p10_ab$P.newsenior,na.rm=T)
senpop_di<-sum(p10_ab$P.newsenior[!is.na(p10_ab$pUSACE1000)], na.rm=T)
percsenpop<-senpop_di/senpopcnt
senpop_cnty<-senpop_di

rentpopcnt<-sum(p10_ab$P.newrent,na.rm=T)
rentpop_di<-sum(p10_ab$P.newrent[!is.na(p10_ab$pUSACE1000)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_cnty<-rentpop_di

#watershed-scale
#wish_wsid<-unique(p10_ab$WS_ID[!is.na(p10_ab$Future)]) #watershed id for watersheds with any homes on buyout wish list
p10_ab_sub<-p10_ab[p10_ab$WS_ID %in% wish_wsid,]
pcnt<-length(p10_ab_sub$STANPAR) #total parcels
pdicnt<-length(p10_ab_sub$STANPAR[!is.na(p10_ab_sub$pUSACE1000)]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_ws<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_ab$STANPAR) #total bldgs
# bdicnt<-length(b10_ab$STANPAR[!is.na(b10_ab$DLEVEL)| !is.na(b10_ab$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_ws<-bdicnt

popcnt<-sum(p10_ab_sub$P.new,na.rm=T) #tot population
pop_di<-sum(p10_ab_sub$P.new[!is.na(p10_ab$pUSACE1000)], na.rm=T) #population in damaged orinundated buildings
percpop<-pop_di/popcnt
pop_ws<-pop_di

senpop_cnt<-sum(p10_ab_sub$P.newsenior,na.rm=T) #senior popualtion
senpop_di<-sum(p10_ab_sub$P.newsenior[!is.na(p10_ab$pUSACE1000)], na.rm=T)
percsenpop<-senpop_di/senpopcnt
senpop_ws<-senpop_di

rentpopcnt<-sum(p10_ab_sub$P.newrent,na.rm=T) #renter population
rentpop_di<-sum(p10_ab_sub$P.newrent[!is.na(p10_ab$pUSACE1000)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_ws<-rentpop_di


#buyout-scale
p10_ab_sub<-p10_ab[!is.na(p10_ab$buyout),]
pcnt<-length(p10_ab_sub$STANPAR) #total parcels
pdicnt<-length(p10_ab_sub$STANPAR[!is.na(p10_ab_sub$pUSACE1000)]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_bo<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_ab$STANPAR) #total bldgs
# bdicnt<-length(b10_ab$STANPAR[!is.na(b10_ab$DLEVEL)| !is.na(b10_ab$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_bo<-bdicnt

popcnt<-sum(p10_ab_sub$P.new,na.rm=T) #tot population
pop_di<-sum(p10_ab_sub$P.new[!is.na(p10_ab$pUSACE1000)], na.rm=T) #population in damaged orinundated buildings
percpop<-pop_di/popcnt
pop_bo<-pop_di

senpop_cnt<-sum(p10_ab_sub$P.newsenior,na.rm=T) #senior popualtion
senpop_di<-sum(p10_ab_sub$P.newsenior[!is.na(p10_ab$pUSACE1000)], na.rm=T)
percsenpop<-senpop_di/senpopcnt
senpop_bo<-senpop_di

rentpopcnt<-sum(p10_ab_sub$P.newrent,na.rm=T) #renter population
rentpop_di<-sum(p10_ab_sub$P.newrent[!is.na(p10_ab$pUSACE1000)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_bo<-rentpop_di


#vectors to go into table
parcels_ab<-c(p_cnty, p_ws, p_bo)
pop_ab<-c(pop_cnty,pop_ws,pop_bo)
senpop_ab<-c(senpop_cnty,senpop_ws,senpop_bo)
rentpop_ab<-c(rentpop_cnty,rentpop_ws,rentpop_bo)


#Wishlist scenario - with freeboard
p10_w<-readRDS(paste0(wd,"dasym3",pnames[22]))
#p10_w<-readRDS(paste0("E:/kate/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/",pnames[22]))

#county-scale
pcnt<-length(p10_w$STANPAR) #total parcels
pdicnt<-length(p10_w$STANPAR[!is.na(p10_w$pUSACE1000)]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_cnty<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_w$STANPAR) #total bldgs
# bdicnt<-length(b10_w$STANPAR[!is.na(b10_w$DLEVEL)| !is.na(b10_w$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_cnty<-bdicnt

popcnt<-sum(p10_w$P.new,na.rm=T)
pop_di<-sum(p10_w$P.new[!is.na(p10_w$pUSACE1000)], na.rm=T)
percpop<-pop_di/popcnt
pop_cnty<-pop_di

senpop_cnt<-sum(p10_w$P.newsenior,na.rm=T)
senpop_di<-sum(p10_w$P.newsenior[!is.na(p10_w$pUSACE1000)], na.rm=T)
percsenpop<-senpop_di/senpopcnt
senpop_cnty<-senpop_di

rentpopcnt<-sum(p10_w$P.newrent,na.rm=T)
rentpop_di<-sum(p10_w$P.newrent[!is.na(p10_w$pUSACE1000)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_cnty<-rentpop_di

#watershed-scale
p10_w_sub<-p10_w[p10_w$WS_ID %in% wish_wsid,]
pcnt<-length(p10_w_sub$STANPAR) #total parcels
pdicnt<-length(p10_w_sub$STANPAR[!is.na(p10_w_sub$pUSACE1000)]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_ws<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_w$STANPAR) #total bldgs
# bdicnt<-length(b10_w$STANPAR[!is.na(b10_w$DLEVEL)| !is.na(b10_w$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_ws<-bdicnt

popcnt<-sum(p10_w_sub$P.new,na.rm=T) #tot population
pop_di<-sum(p10_w_sub$P.new[!is.na(p10_w$pUSACE1000)], na.rm=T) #population in damaged orinundated buildings
percpop<-pop_di/popcnt
pop_ws<-pop_di

senpop_cnt<-sum(p10_w_sub$P.newsenior,na.rm=T) #senior popualtion
senpop_di<-sum(p10_w_sub$P.newsenior[!is.na(p10_w$pUSACE1000)], na.rm=T)
percsenpop<-senpop_di/senpopcnt
senpop_ws<-senpop_di

rentpopcnt<-sum(p10_w_sub$P.newrent,na.rm=T) #renter population
rentpop_di<-sum(p10_w_sub$P.newrent[!is.na(p10_w$pUSACE1000)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_ws<-rentpop_di


#buyout-scale
p10_w_sub<-p10_w[!is.na(p10_w$buyout),]
pcnt<-length(p10_w_sub$STANPAR) #total parcels
pdicnt<-length(p10_w_sub$STANPAR[!is.na(p10_w_sub$pUSACE1000)]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_bo<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_w$STANPAR) #total bldgs
# bdicnt<-length(b10_w$STANPAR[!is.na(b10_w$DLEVEL)| !is.na(b10_w$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_bo<-bdicnt

popcnt<-sum(p10_w_sub$P.new,na.rm=T) #tot population
pop_di<-sum(p10_w_sub$P.new[!is.na(p10_w$pUSACE1000)], na.rm=T) #population in damaged orinundated buildings
percpop<-pop_di/popcnt
pop_bo<-pop_di

senpop_cnt<-sum(p10_w_sub$P.newsenior,na.rm=T) #senior popualtion
senpop_di<-sum(p10_w_sub$P.newsenior[!is.na(p10_w$pUSACE1000)], na.rm=T)
percsenpop<-senpop_di/senpop_cnt
senpop_bo<-senpop_di

rentpopcnt<-sum(p10_w_sub$P.newrent,na.rm=T) #renter population
rentpop_di<-sum(p10_w_sub$P.newrent[!is.na(p10_w$pUSACE1000)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_bo<-rentpop_di


#vectors to go into table
parcels_w<-c(p_cnty, p_ws, p_bo)
pop_w<-c(pop_cnty,pop_ws,pop_bo)
senpop_w<-c(senpop_cnty,senpop_ws,senpop_bo)
rentpop_w<-c(rentpop_cnty,rentpop_ws,rentpop_bo)


#Put together the table
Scenario<-c("No Buyouts","No Buyouts","No Buyouts",
            "With Buyouts Prior to 2010","With Buyouts Prior to 2010","With Buyouts Prior to 2010",
            "All Buyouts Prior to 2010", "All Buyouts Prior to 2010","All Buyouts Prior to 2010",
            "Wishlist Prior to 2010","Wishlist Prior to 2010","Wishlist Prior to 2010")
Scale<-c("County","Micro-watersheds with Buyouts","Buyout Parcels",
         "County","Micro-watersheds with Buyouts","Buyout Parcels",
         "County","Micro-watersheds with Buyouts","Buyout Parcels",
         "County","Micro-watersheds with Buyouts","Buyout Parcels")
Damaged_Properties<-c(parcels_nb,parcels_ob,parcels_ab,parcels_w)
Exposed_Population<-c(pop_nb,pop_ob,pop_ab,pop_w)
Exposed_Senior_Population<-c(senpop_nb,senpop_ob,senpop_ab,senpop_w)
Exposed_Renter_Population<-c(rentpop_nb,rentpop_ob,rentpop_ab,rentpop_w)

df<-data.frame(Scenario, Scale, Damaged_Properties,Exposed_Population,
               Exposed_Senior_Population,Exposed_Renter_Population)

dt<-xtable(df)


#d1<-p10_ob %>% group_by(ctgeoid10) %>% summarise(SenPop= max(SeniorPop, na.rm=T))
#sum(d1$SenPop,na.rm=T)




####################################################################################################################################
#Create Table of Summary Statistics for Direct Damage Counts in 2010 based on Parcel-scale sf objects and Building-scale sf object for HAZUS 500 no-freeboard
###################################################################################################################################

pnames<-c( "finalnewparcels2007ob.RDS", "finalnewparcels2008ob.RDS", "finalnewparcels2009ob.RDS", "finalnewparcels2010ob.RDS", "finalnewparcels2011ob.RDS",
           "finalnewparcels2012ob.RDS", "finalnewparcels2013ob.RDS","finalnewparcels2014ob.RDS", 
           "finalnewparcels2007nb.RDS", "finalnewparcels2008nb.RDS", "finalnewparcels2009nb.RDS", "finalnewparcels2010nb.RDS", "finalnewparcels2011nb.RDS",
           "finalnewparcels2012nb.RDS", "finalnewparcels2013nb.RDS","finalnewparcels2014nb.RDS",
           "finalnewparcels2010ab.RDS", "finalnewparcels2011ab.RDS","finalnewparcels2012ab.RDS", "finalnewparcels2013ab.RDS","finalnewparcels2014ab.RDS",
           "finalnewparcels2010w.RDS", "finalnewparcels2011w.RDS","finalnewparcels2012w.RDS", "finalnewparcels2013w.RDS","finalnewparcels2014w.RDS") 
#No buyout scenario - with freeboard

p10_nb<-readRDS(paste0(wd,"dasym3", pnames[12]))

#county-scale
pcnt<-length(p10_nb$STANPAR) #total parcels
pdicnt<-length(p10_nb$STANPAR[!is.na(p10_nb$pUSACE500freeb) ]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_cnty<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_nb$STANPAR) #total bldgs
# bdicnt<-length(b10_nb$STANPAR[!is.na(b10_nb$DLEVEL)| !is.na(b10_nb$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_cnty<-bdicnt

popcnt<-sum(p10_nb$P.new,na.rm=T)
pop_di<-sum(p10_nb$P.new[!is.na(p10_nb$pUSACE500freeb)], na.rm=T)
percpop<-pop_di/popcnt
pop_cnty<-pop_di

senpop_cnt<-sum(p10_nb$P.newsenior,na.rm=T)
senpop_di<-sum(p10_nb$P.newsenior[!is.na(p10_nb$pUSACE500freeb)], na.rm=T)
percsenpop<-senpop_di/senpopcnt
senpop_cnty<-senpop_di

rentpopcnt<-sum(p10_nb$P.newrent,na.rm=T)
rentpop_di<-sum(p10_nb$P.newrent[!is.na(p10_nb$pUSACE500freeb)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_cnty<-rentpop_di

#watershed-scale
wish_wsid<-unique(p10_nb$WS_ID[!is.na(p10_nb$Future)]) #watershed id for watersheds with any homes on buyout wish list
p10_nb_sub<-p10_nb[p10_nb$WS_ID %in% wish_wsid,]
pcnt<-length(p10_nb_sub$STANPAR) #total parcels
pdicnt<-length(p10_nb_sub$STANPAR[!is.na(p10_nb_sub$pUSACE500freeb)]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_ws<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_nb$STANPAR) #total bldgs
# bdicnt<-length(b10_nb$STANPAR[!is.na(b10_nb$DLEVEL)| !is.na(b10_nb$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_ws<-bdicnt

popcnt<-sum(p10_nb_sub$P.new,na.rm=T) #tot population
pop_di<-sum(p10_nb_sub$P.new[!is.na(p10_nb$pUSACE500freeb)], na.rm=T) #population in damaged orinundated buildings
percpop<-pop_di/popcnt
pop_ws<-pop_di

senpop_cnt<-sum(p10_nb_sub$P.newsenior,na.rm=T) #senior popualtion
senpop_di<-sum(p10_nb_sub$P.newsenior[!is.na(p10_nb$pUSACE500freeb)], na.rm=T)
percsenpop<-senpop_di/senpopcnt
senpop_ws<-senpop_di

rentpopcnt<-sum(p10_nb_sub$P.newrent,na.rm=T) #renter population
rentpop_di<-sum(p10_nb_sub$P.newrent[!is.na(p10_nb$pUSACE500freeb)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_ws<-rentpop_di


#buyout-scale
p10_nb_sub<-p10_nb[!is.na(p10_nb$buyout),]
pcnt<-length(p10_nb_sub$STANPAR) #total parcels
pdicnt<-length(p10_nb_sub$STANPAR[!is.na(p10_nb_sub$pUSACE500freeb)]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_bo<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_nb$STANPAR) #total bldgs
# bdicnt<-length(b10_nb$STANPAR[!is.na(b10_nb$DLEVEL)| !is.na(b10_nb$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_bo<-bdicnt

popcnt<-sum(p10_nb_sub$P.new,na.rm=T) #tot population
pop_di<-sum(p10_nb_sub$P.new[!is.na(p10_nb$pUSACE500freeb)], na.rm=T) #population in damaged orinundated buildings
percpop<-pop_di/popcnt
pop_bo<-pop_di

senpop_cnt<-sum(p10_nb_sub$P.newsenior,na.rm=T) #senior popualtion
senpop_di<-sum(p10_nb_sub$P.newsenior[!is.na(p10_nb$pUSACE500freeb)], na.rm=T)
percsenpop<-senpop_di/senpopcnt
senpop_bo<-senpop_di

rentpopcnt<-sum(p10_nb_sub$P.newrent,na.rm=T) #renter population
rentpop_di<-sum(p10_nb_sub$P.newrent[!is.na(p10_nb$pUSACE500freeb)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_bo<-rentpop_di


#vectors to go into table
parcels_nb<-c(p_cnty, p_ws, p_bo)
pop_nb<-c(pop_cnty,pop_ws,pop_bo)
senpop_nb<-c(senpop_cnty,senpop_ws,senpop_bo)
rentpop_nb<-c(rentpop_cnty,rentpop_ws,rentpop_bo)



#Observed buyout scenario - with freeboard
p10_ob<-readRDS(paste0(wd,"dasym3",pnames[4]))
#p10_ob<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/","dasym3",pnames[4]))

#county-scale
pcnt<-length(p10_ob$STANPAR) #total parcels
pdicnt<-length(p10_ob$STANPAR[!is.na(p10_ob$pUSACE500freeb)]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_cnty<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_ob$STANPAR) #total bldgs
# bdicnt<-length(b10_ob$STANPAR[!is.na(b10_ob$DLEVEL)| !is.na(b10_ob$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_cnty<-bdicnt

popcnt<-sum(p10_ob$P.new,na.rm=T)
pop_di<-sum(p10_ob$P.new[!is.na(p10_ob$pUSACE500freeb)], na.rm=T)
percpop<-pop_di/popcnt
pop_cnty<-pop_di

senpop_cnt<-sum(p10_ob$P.newsenior,na.rm=T)
senpop_di<-sum(p10_ob$P.newsenior[!is.na(p10_ob$pUSACE500freeb)], na.rm=T)
percsenpop<-senpop_di/senpopcnt
senpop_cnty<-senpop_di

rentpopcnt<-sum(p10_ob$P.newrent,na.rm=T)
rentpop_di<-sum(p10_ob$P.newrent[!is.na(p10_ob$pUSACE500freeb)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_cnty<-rentpop_di

#watershed-scale
#wish_wsid<-unique(p10_ob$WS_ID[!is.na(p10_ob$Future)]) #watershed id for watersheds with any homes on buyout wish list
p10_ob_sub<-p10_ob[p10_ob$WS_ID %in% wish_wsid,]
pcnt<-length(p10_ob_sub$STANPAR) #total parcels
pdicnt<-length(p10_ob_sub$STANPAR[!is.na(p10_ob_sub$pUSACE500freeb)]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_ws<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_ob$STANPAR) #total bldgs
# bdicnt<-length(b10_ob$STANPAR[!is.na(b10_ob$DLEVEL)| !is.na(b10_ob$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_ws<-bdicnt

popcnt<-sum(p10_ob_sub$P.new,na.rm=T) #tot population
pop_di<-sum(p10_ob_sub$P.new[!is.na(p10_ob$pUSACE500freeb)], na.rm=T) #population in damaged orinundated buildings
percpop<-pop_di/popcnt
pop_ws<-pop_di

senpop_cnt<-sum(p10_ob_sub$P.newsenior,na.rm=T) #senior popualtion
senpop_di<-sum(p10_ob_sub$P.newsenior[!is.na(p10_ob$pUSACE500freeb)], na.rm=T)
percsenpop<-senpop_di/senpopcnt
senpop_ws<-senpop_di

rentpopcnt<-sum(p10_ob_sub$P.newrent,na.rm=T) #renter population
rentpop_di<-sum(p10_ob_sub$P.newrent[!is.na(p10_ob$pUSACE500freeb)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_ws<-rentpop_di


#buyout-scale
p10_ob_sub<-p10_ob[!is.na(p10_ob$buyout),]
pcnt<-length(p10_ob_sub$STANPAR) #total parcels
pdicnt<-length(p10_ob_sub$STANPAR[!is.na(p10_ob_sub$pUSACE500freeb)]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_bo<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_ob$STANPAR) #total bldgs
# bdicnt<-length(b10_ob$STANPAR[!is.na(b10_ob$DLEVEL)| !is.na(b10_ob$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_bo<-bdicnt

popcnt<-sum(p10_ob_sub$P.new,na.rm=T) #tot population
pop_di<-sum(p10_ob_sub$P.new[!is.na(p10_ob$pUSACE500freeb)], na.rm=T) #population in damaged orinundated buildings
percpop<-pop_di/popcnt
pop_bo<-pop_di

senpop_cnt<-sum(p10_ob_sub$P.newsenior,na.rm=T) #senior popualtion
senpop_di<-sum(p10_ob_sub$P.newsenior[!is.na(p10_ob$pUSACE500freeb)], na.rm=T)
percsenpop<-senpop_di/senpopcnt
senpop_bo<-senpop_di

rentpopcnt<-sum(p10_ob_sub$P.newrent,na.rm=T) #renter population
rentpop_di<-sum(p10_ob_sub$P.newrent[!is.na(p10_ob$pUSACE500freeb)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_bo<-rentpop_di


#vectors to go into table
parcels_ob<-c(p_cnty, p_ws, p_bo)
pop_ob<-c(pop_cnty,pop_ws,pop_bo)
senpop_ob<-c(senpop_cnty,senpop_ws,senpop_bo)
rentpop_ob<-c(rentpop_cnty,rentpop_ws,rentpop_bo)


#All buyout scenario - with freeboard
p10_ab<-readRDS(paste0(wd,"dasym3",pnames[17]))
#p10_ab<-readRDS(paste0("C:/Users/nelsonks/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/","dasym3",pnames[17]))

#county-scale
pcnt<-length(p10_ab$STANPAR) #total parcels
pdicnt<-length(p10_ab$STANPAR[!is.na(p10_ab$pUSACE500freeb)]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_cnty<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_ab$STANPAR) #total bldgs
# bdicnt<-length(b10_ab$STANPAR[!is.na(b10_ab$DLEVEL)| !is.na(b10_ab$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_cnty<-bdicnt

popcnt<-sum(p10_ab$P.new,na.rm=T)
pop_di<-sum(p10_ab$P.new[!is.na(p10_ab$pUSACE500freeb)], na.rm=T)
percpop<-pop_di/popcnt
pop_cnty<-pop_di

senpop_cnt<-sum(p10_ab$P.newsenior,na.rm=T)
senpop_di<-sum(p10_ab$P.newsenior[!is.na(p10_ab$pUSACE500freeb)], na.rm=T)
percsenpop<-senpop_di/senpopcnt
senpop_cnty<-senpop_di

rentpopcnt<-sum(p10_ab$P.newrent,na.rm=T)
rentpop_di<-sum(p10_ab$P.newrent[!is.na(p10_ab$pUSACE500freeb)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_cnty<-rentpop_di

#watershed-scale
#wish_wsid<-unique(p10_ab$WS_ID[!is.na(p10_ab$Future)]) #watershed id for watersheds with any homes on buyout wish list
p10_ab_sub<-p10_ab[p10_ab$WS_ID %in% wish_wsid,]
pcnt<-length(p10_ab_sub$STANPAR) #total parcels
pdicnt<-length(p10_ab_sub$STANPAR[!is.na(p10_ab_sub$pUSACE500freeb)]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_ws<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_ab$STANPAR) #total bldgs
# bdicnt<-length(b10_ab$STANPAR[!is.na(b10_ab$DLEVEL)| !is.na(b10_ab$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_ws<-bdicnt

popcnt<-sum(p10_ab_sub$P.new,na.rm=T) #tot population
pop_di<-sum(p10_ab_sub$P.new[!is.na(p10_ab$pUSACE500freeb)], na.rm=T) #population in damaged orinundated buildings
percpop<-pop_di/popcnt
pop_ws<-pop_di

senpop_cnt<-sum(p10_ab_sub$P.newsenior,na.rm=T) #senior popualtion
senpop_di<-sum(p10_ab_sub$P.newsenior[!is.na(p10_ab$pUSACE500freeb)], na.rm=T)
percsenpop<-senpop_di/senpopcnt
senpop_ws<-senpop_di

rentpopcnt<-sum(p10_ab_sub$P.newrent,na.rm=T) #renter population
rentpop_di<-sum(p10_ab_sub$P.newrent[!is.na(p10_ab$pUSACE500freeb)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_ws<-rentpop_di


#buyout-scale
p10_ab_sub<-p10_ab[!is.na(p10_ab$buyout),]
pcnt<-length(p10_ab_sub$STANPAR) #total parcels
pdicnt<-length(p10_ab_sub$STANPAR[!is.na(p10_ab_sub$pUSACE500freeb)]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_bo<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_ab$STANPAR) #total bldgs
# bdicnt<-length(b10_ab$STANPAR[!is.na(b10_ab$DLEVEL)| !is.na(b10_ab$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_bo<-bdicnt

popcnt<-sum(p10_ab_sub$P.new,na.rm=T) #tot population
pop_di<-sum(p10_ab_sub$P.new[!is.na(p10_ab$pUSACE500freeb)], na.rm=T) #population in damaged orinundated buildings
percpop<-pop_di/popcnt
pop_bo<-pop_di

senpop_cnt<-sum(p10_ab_sub$P.newsenior,na.rm=T) #senior popualtion
senpop_di<-sum(p10_ab_sub$P.newsenior[!is.na(p10_ab$pUSACE500freeb)], na.rm=T)
percsenpop<-senpop_di/senpopcnt
senpop_bo<-senpop_di

rentpopcnt<-sum(p10_ab_sub$P.newrent,na.rm=T) #renter population
rentpop_di<-sum(p10_ab_sub$P.newrent[!is.na(p10_ab$pUSACE500freeb)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_bo<-rentpop_di


#vectors to go into table
parcels_ab<-c(p_cnty, p_ws, p_bo)
pop_ab<-c(pop_cnty,pop_ws,pop_bo)
senpop_ab<-c(senpop_cnty,senpop_ws,senpop_bo)
rentpop_ab<-c(rentpop_cnty,rentpop_ws,rentpop_bo)


#Wishlist scenario - with freeboard
p10_w<-readRDS(paste0(wd,"dasym3",pnames[22]))
#p10_w<-readRDS(paste0("E:/kate/Box Sync/Kate_Kevona/NashvilleFloodData/MWS plots and figures/new data/",pnames[22]))

#county-scale
pcnt<-length(p10_w$STANPAR) #total parcels
pdicnt<-length(p10_w$STANPAR[!is.na(p10_w$pUSACE500freeb)]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_cnty<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_w$STANPAR) #total bldgs
# bdicnt<-length(b10_w$STANPAR[!is.na(b10_w$DLEVEL)| !is.na(b10_w$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_cnty<-bdicnt

popcnt<-sum(p10_w$P.new,na.rm=T)
pop_di<-sum(p10_w$P.new[!is.na(p10_w$pUSACE500freeb)], na.rm=T)
percpop<-pop_di/popcnt
pop_cnty<-pop_di

senpop_cnt<-sum(p10_w$P.newsenior,na.rm=T)
senpop_di<-sum(p10_w$P.newsenior[!is.na(p10_w$pUSACE500freeb)], na.rm=T)
percsenpop<-senpop_di/senpopcnt
senpop_cnty<-senpop_di

rentpopcnt<-sum(p10_w$P.newrent,na.rm=T)
rentpop_di<-sum(p10_w$P.newrent[!is.na(p10_w$pUSACE500freeb)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_cnty<-rentpop_di

#watershed-scale
p10_w_sub<-p10_w[p10_w$WS_ID %in% wish_wsid,]
pcnt<-length(p10_w_sub$STANPAR) #total parcels
pdicnt<-length(p10_w_sub$STANPAR[!is.na(p10_w_sub$pUSACE500freeb)]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_ws<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_w$STANPAR) #total bldgs
# bdicnt<-length(b10_w$STANPAR[!is.na(b10_w$DLEVEL)| !is.na(b10_w$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_ws<-bdicnt

popcnt<-sum(p10_w_sub$P.new,na.rm=T) #tot population
pop_di<-sum(p10_w_sub$P.new[!is.na(p10_w$pUSACE500freeb)], na.rm=T) #population in damaged orinundated buildings
percpop<-pop_di/popcnt
pop_ws<-pop_di

senpop_cnt<-sum(p10_w_sub$P.newsenior,na.rm=T) #senior popualtion
senpop_di<-sum(p10_w_sub$P.newsenior[!is.na(p10_w$pUSACE500freeb)], na.rm=T)
percsenpop<-senpop_di/senpopcnt
senpop_ws<-senpop_di

rentpopcnt<-sum(p10_w_sub$P.newrent,na.rm=T) #renter population
rentpop_di<-sum(p10_w_sub$P.newrent[!is.na(p10_w$pUSACE500freeb)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_ws<-rentpop_di


#buyout-scale
p10_w_sub<-p10_w[!is.na(p10_w$buyout),]
pcnt<-length(p10_w_sub$STANPAR) #total parcels
pdicnt<-length(p10_w_sub$STANPAR[!is.na(p10_w_sub$pUSACE500freeb)]) #parcels damaged or inundated
percp<-pdicnt/pcnt#percent of all parcels damaged or inundated
p_bo<-pdicnt #count of damaged or inundated parcels

# bcnt<-length(b10_w$STANPAR) #total bldgs
# bdicnt<-length(b10_w$STANPAR[!is.na(b10_w$DLEVEL)| !is.na(b10_w$inundate)]) #bldgs damaged or inundated
# percb<-bdicnt/bcnt#percent of all bldgs damaged or inundated
# b_bo<-bdicnt

popcnt<-sum(p10_w_sub$P.new,na.rm=T) #tot population
pop_di<-sum(p10_w_sub$P.new[!is.na(p10_w$pUSACE500freeb)], na.rm=T) #population in damaged orinundated buildings
percpop<-pop_di/popcnt
pop_bo<-pop_di

senpop_cnt<-sum(p10_w_sub$P.newsenior,na.rm=T) #senior popualtion
senpop_di<-sum(p10_w_sub$P.newsenior[!is.na(p10_w$pUSACE500freeb)], na.rm=T)
percsenpop<-senpop_di/senpop_cnt
senpop_bo<-senpop_di

rentpopcnt<-sum(p10_w_sub$P.newrent,na.rm=T) #renter population
rentpop_di<-sum(p10_w_sub$P.newrent[!is.na(p10_w$pUSACE500freeb)], na.rm=T)
rentpercpop<-rentpop_di/rentpopcnt
rentpop_bo<-rentpop_di


#vectors to go into table
parcels_w<-c(p_cnty, p_ws, p_bo)
pop_w<-c(pop_cnty,pop_ws,pop_bo)
senpop_w<-c(senpop_cnty,senpop_ws,senpop_bo)
rentpop_w<-c(rentpop_cnty,rentpop_ws,rentpop_bo)


#Put together the table
Scenario<-c("No Buyouts","No Buyouts","No Buyouts",
            "With Buyouts Prior to 2010","With Buyouts Prior to 2010","With Buyouts Prior to 2010",
            "All Buyouts Prior to 2010", "All Buyouts Prior to 2010","All Buyouts Prior to 2010",
            "Wishlist Prior to 2010","Wishlist Prior to 2010","Wishlist Prior to 2010")
Scale<-c("County","Micro-watersheds with Buyouts","Buyout Parcels",
         "County","Micro-watersheds with Buyouts","Buyout Parcels",
         "County","Micro-watersheds with Buyouts","Buyout Parcels",
         "County","Micro-watersheds with Buyouts","Buyout Parcels")
Damaged_Properties<-c(parcels_nb,parcels_ob,parcels_ab,parcels_w)
Exposed_Population<-c(pop_nb,pop_ob,pop_ab,pop_w)
Exposed_Senior_Population<-c(senpop_nb,senpop_ob,senpop_ab,senpop_w)
Exposed_Renter_Population<-c(rentpop_nb,rentpop_ob,rentpop_ab,rentpop_w)

df<-data.frame(Scenario, Scale, Damaged_Properties,Exposed_Population,
               Exposed_Senior_Population,Exposed_Renter_Population)



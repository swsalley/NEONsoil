
# NEON soil laboratory data from neonUtilities
# Shawn W. Salley, 20230127, Shawn.Salley@usda.gov
# NEON megapit,  n = 47, 
# NEON distributed plots,  n = 727

library(neonUtilities) 
library(dplyr)
library(aqp)

## Download Soil Characterization Data (distributed and Initial) ####

# these are from the 2022 release, rest of the data might be available in 2023

NEON.DP1.10047 <- loadByProduct(dpID="DP1.10047.001", site="all", package="expanded") # Distributed Plots
# Answer = Y
NEON.DP1.00096 <- loadByProduct(dpID="DP1.00096.001", site="all", package="expanded") # Mega Pits 
# Answer = Y


names(NEON.DP1.10047)
names(NEON.DP1.00096)


#  Distributed Plots SPC
spc_perplot <- NEON.DP1.10047$spc_perplot
spc_perplot <- spc_perplot %>% select("domainID", "siteID", "plotID",  "plotType", "nrcsDescriptionID", 
                                      "decimalLatitude", "decimalLongitude", "elevation", "collectDate", "soilSamplingMethod", 
                                      "pitDepth", "soilSeries",  "soilFamily", "soilSubgroup", "soilGreatGroup", "soilSuborder",  "soilOrder")


# check to see if NEON has uploaded the last site, TEAK

spc_perplot %>% filter(siteID == "TEAK") # yes! now we can finish the NEON soil variability manuscript with all the NEON distributed soil data 

#
spc_perhorizon <- NEON.DP1.10047$spc_perhorizon
spc_perhorizon <- spc_perhorizon %>% select("plotID", 
                                            "horizonID", "horizonName", "horizonTopDepth", "horizonBottomDepth", "remarks")
#
spc_bulkdensity <- NEON.DP1.10047$spc_bulkdensity
spc_bulkdensity <- spc_bulkdensity %>% select("horizonID", "bulkDensSampleType", "bulkDensSampleType",  "bulkDensOvenDry", "remarks")
#  
spc_particlesize <- NEON.DP1.10047$spc_particlesize
spc_particlesize <- spc_particlesize %>% select("horizonID", "coarseFrag2To5", "coarseFrag5To20", "sandTotal", "siltTotal", 
                                                "clayTotal", "carbonateClay", "clayFineContent", "siltFineContent", 
                                                "siltCoarseContent", "sandVeryFineContent", "sandFineContent", 
                                                "sandMediumContent", "sandCoarseContent", "sandVeryCoarseContent")
#
spc_biogeochem <- NEON.DP1.10047$spc_biogeochem
spc_biogeochem <- spc_biogeochem %>% select("horizonID", "airDryOvenDryRatio", "carbonTot", "nitrogenTot", "sulfurTot", "estimatedOC", "gypsumConc", "caco3Conc", "caNh4d", "kNh4d", 
                                            "phCacl2", "phH2o", "ec12pre", "mgNh4d", "naNh4d", "cecdNh4", "alSatCecd33", "baseSumCecd10", "bsesatCecd10", 
                                            "ececCecd33", "alKcl", "feKcl", "mnKcl", "bSatx", "brSatx", "caSatx", "clSatx", "co3Satx", "ecSatp", "flSatx", 
                                            "waterSatx", "hco3Sx", "kSatx", "mgSatx", "naSatx", "no2Satx", "no3Satx", "pSatx", "phSp","so4Satx", "processingRemarks") 


# Mega Pits MGP

# the neon mega pit horizon ID's do not use the NRCS horizon codes. 
# recommend NEON to update so relationship joins are easier

mgp_permegapit <- NEON.DP1.00096$mgp_permegapit
mgp_permegapit$plotID <- paste0(mgp_permegapit$siteID, "_mp")
mgp_permegapit$plotType <- "mega"
mgp_permegapit$soilSamplingMethod <- "soil pit"
#
mgp_perhorizon <- NEON.DP1.00096$mgp_perhorizon
mgp_perhorizon$plotID <- paste0(mgp_perhorizon$siteID, "_mp")
#
mgp_perbulksample <- NEON.DP1.00096$mgp_perbulksample
mgp_perbulksample <- mgp_perbulksample %>% filter(bulkDensSampleType == "Regular") #use the regular, ask NEON what audit data actually are
#
mgp_perbiogeosample <- NEON.DP1.00096$mgp_perbiogeosample
mgp_perbiogeosample <- mgp_perbiogeosample %>% filter(biogeoSampleType == "Regular") # use the regular, not audit

# filter out TEAK

# TEAK  # n = 2 # split 19-43, 43-67
mgp_permegapit %>% filter(siteID == "TEAK") 
mgp_perhorizon %>% filter(siteID == "TEAK") %>% arrange(horizonTopDepth) 
mgp_perbulksample %>% filter(siteID == "TEAK") %>% arrange(bulkDensTopDepth)
mgp_perbiogeosample %>% filter(siteID == "TEAK") %>% arrange(biogeoTopDepth)
mgp_perbiogeosample %>% filter(horizonID == "17_TEAK_PIT1_C1") %>% arrange("biogeoTopDepth")
#
mgp_perhorizon[mgp_perhorizon$uid == "ebca4d8c-eb9c-4381-9f66-2aeec23b5ed8",]
mgp_perhorizon <- rbind(mgp_perhorizon, mgp_perhorizon[mgp_perhorizon$uid == "ebca4d8c-eb9c-4381-9f66-2aeec23b5ed8",])
mgp_perhorizon[mgp_perhorizon$horizonID == "17_TEAK_PIT1_C1",]
#
mgp_perhorizon[mgp_perhorizon$horizonID == "17_TEAK_PIT1_C1",][1,6] <- "17_TEAK_PIT1_C1_19-43"
mgp_perhorizon[mgp_perhorizon$horizonID == "17_TEAK_PIT1_C1",][1,6] <- "17_TEAK_PIT1_C1_43-67"
mgp_perhorizon[mgp_perhorizon$uid == "ebca4d8c-eb9c-4381-9f66-2aeec23b5ed8",]
mgp_perhorizon[mgp_perhorizon$horizonID == "17_TEAK_PIT1_C1_19-43",][11] <- 43
mgp_perhorizon[mgp_perhorizon$horizonID == "17_TEAK_PIT1_C1_43-67",][10] <- 43
mgp_perhorizon[mgp_perhorizon$uid == "ebca4d8c-eb9c-4381-9f66-2aeec23b5ed8",]
#
mgp_perbulksample[mgp_perbulksample$uid == "590426f8-37e1-4e5c-853e-798fa256c7c9",][6] <- "17_TEAK_PIT1_C1_19-43"
mgp_perbulksample[mgp_perbulksample$uid == "ea29f217-7c63-4ad1-b3e6-fc8f71cb3deb",][6] <- "17_TEAK_PIT1_C1_43-67"
mgp_perbulksample %>% filter(siteID == "TEAK") %>% filter(bulkDensSampleType  == "Regular") %>% arrange(bulkDensTopDepth)
#
mgp_perbiogeosample %>% filter(siteID == "TEAK")
mgp_perbiogeosample[mgp_perbiogeosample$uid == "8b2c1884-4b5f-4cd6-923e-420547b70845",][6] <- "17_TEAK_PIT1_C1_19-43"
mgp_perbiogeosample[mgp_perbiogeosample$uid == "b117265e-0e3d-45eb-bcaa-28003b642418",][6] <- "17_TEAK_PIT1_C1_43-67"
#
mgp_permegapit %>% filter(siteID == "TEAK") 
mgp_perhorizon %>% filter(siteID == "TEAK") %>% arrange(horizonTopDepth) 
mgp_perbulksample %>% filter(siteID == "TEAK") %>% arrange(bulkDensTopDepth)
mgp_perbiogeosample %>% filter(siteID == "TEAK") %>% arrange(biogeoTopDepth)
mgp_perbiogeosample %>% filter(horizonID == "17_TEAK_PIT1_C1") %>% arrange("biogeoTopDepth")
mgp_perbiogeosample %>% filter(horizonID == "17_TEAK_PIT1_C1_19-43") %>% arrange("biogeoTopDepth")
mgp_perbiogeosample %>% filter(horizonID == "17_TEAK_PIT1_C1_43-67") %>% arrange("biogeoTopDepth")
#

# RMNP # split 60 / 40 best to weighted average these data
mgp_permegapit %>% filter(siteID == "RMNP") 
mgp_perhorizon %>% filter(siteID == "RMNP") %>% arrange(horizonTopDepth) 
mgp_perbulksample %>% filter(siteID == "RMNP") %>% arrange(bulkDensTopDepth)
mgp_perbiogeosample %>% filter(siteID == "RMNP") %>% arrange(biogeoTopDepth)
#
mgp_perhorizon %>% filter(horizonID == "10_CASTNET_PIT1_C/B") %>% arrange(horizonTopDepth) 
mgp_perbulksample %>% filter(horizonID == "10_CASTNET_PIT1_C/B") %>% arrange(bulkDensTopDepth)
mgp_perbiogeosample %>% filter(horizonID == "10_CASTNET_PIT1_C/B") %>% arrange("biogeoTopDepth")
#

# biogeo weighted average

mgp_perbiogeosample.rmnp <- mgp_perbiogeosample %>% filter(horizonID == "10_CASTNET_PIT1_C/B") 
#
mgp_perbiogeosample.rmnp <- rbind(mgp_perbiogeosample.rmnp, mgp_perbiogeosample.rmnp[1,])
mgp_perbiogeosample.rmnp[3,c(13:78)] <- ( mgp_perbiogeosample.rmnp[1,c(13:78)] * 0.4) + ( mgp_perbiogeosample.rmnp[2,c(13:78)] * 0.6)
mgp_perbiogeosample.rmnp[3,1] <- "new_weighted_average"
mgp_perbiogeosample.rmnp[3,9] <- 1
mgp_perbiogeosample.rmnp[3,6] <- "10_CASTNET_PIT1_C/B"
mgp_perbiogeosample.rmnp[3,7] <- "10_CASTNET_PIT1_33-84_BIOGEO_C/B_weighted_avg"

#
mgp_perbiogeosample <- mgp_perbiogeosample[mgp_perbiogeosample$horizonID != "10_CASTNET_PIT1_C/B",]
mgp_perbiogeosample <- rbind(mgp_perbiogeosample,mgp_perbiogeosample.rmnp[3,])
mgp_perbiogeosample <- mgp_perbiogeosample %>% arrange(siteID, biogeoTopDepth )
#

mgp_perbulksample[mgp_perbulksample$uid == "2c43ead9-78fd-44b0-9277-f9284b59ccf2", 18] <- (1.1387 * 0.6) + ( 1.3126 * 0.4)
mgp_perbulksample <- mgp_perbulksample[mgp_perbulksample$uid != "4931483e-792b-4042-a17f-d40acba062cb",]
#

#
mgp_permegapit <- mgp_permegapit %>% select("domainID", "siteID", "plotID", "plotType", "nrcsDescriptionID", "decimalLatitude",
                                            "decimalLongitude", "elevation", "collectDate", "soilSamplingMethod", "pitDepth",
                                            "soilSeries", "soilFamily", "soilSubgroup", "soilGreatGroup", "soilSuborder",  "soilOrder" )
#
mgp_perhorizon <- mgp_perhorizon %>% select("plotID",  "horizonID", "horizonName", "horizonTopDepth", "horizonBottomDepth", "remarks")

#
mgp_perbulksample <- mgp_perbulksample %>% select("horizonID", "bulkDensSampleType", "bulkDensSampleType", "bulkDensExclCoarseFrag", "remarks")
#
mgp_particlesize <- mgp_perbiogeosample %>% select("horizonID", "coarseFrag2To5", "coarseFrag5To20", "sandTotal", "siltTotal", 
                                                   "clayTotal", "carbonateClay", "clayFineContent", "siltFineContent", 
                                                   "siltCoarseContent", "sandVeryFineContent", "sandFineContent", "sandMediumContent",
                                                   "sandCoarseContent", "sandVeryCoarseContent")

#
mgp_biogeochem <- mgp_perbiogeosample %>% select("horizonID", "airDryOvenDry", "carbonTot", "nitrogenTot", "sulfurTot", "estimatedOC", "gypsumConc", "caco3Conc", "caNh4d", "kNh4d",
                                                 "phCacl2", "phH2o", "ec12pre", "mgNh4d", "naNh4d", "cecdNh4", "alSatCecd33", "baseSumCecd10", "bsesatCecd10", 
                                                 "ececCecd33", "alKcl", "feKcl", "mnKcl", "bSatx", "brSatx", "caSatx", "clSatx", "co3Satx", "ecSatp", "flSatx", 
                                                 "waterSatx", "hco3Sx", "kSatx", "mgSatx", "naSatx", "no2Satx", "no3Satx", "pSatx", "phSp", "so4Satx", "remarks")
#



# Plot data, spc_perplot & mgp_permegapit are best replicated in the NASIS data
names(spc_perplot) == names(mgp_permegapit)

# Plot data, spc_perplot & mgp_permegapit 
names(spc_perhorizon) == names(mgp_perhorizon)

# Plot data, spc_bulkdensity & mgp_perbulksample 
names(spc_bulkdensity) == names(mgp_perbulksample)
colnames(spc_bulkdensity)[3] <- "bulkDen"
colnames(mgp_perbulksample)[3]  <- "bulkDen"

# Plot data, spc_bulkdensity & mgp_perbulksample 
names(spc_particlesize) == names(mgp_particlesize)

# Plot data, spc_bulkdensity & mgp_perbulksample 
names(spc_biogeochem)  == names(mgp_biogeochem)
colnames(spc_biogeochem)[2] <- "airDryOvenDryRatio"
colnames(mgp_biogeochem)[2] <- "airDryOvenDryRatio"
colnames(spc_biogeochem)[41] <- "remarks"

#
SPC_LAB <- full_join(spc_perplot, spc_perhorizon, by = "plotID")
SPC_LAB <- left_join(SPC_LAB, spc_bulkdensity, by = "horizonID")
SPC_LAB <- left_join(SPC_LAB, spc_particlesize, by = "horizonID")
SPC_LAB <- left_join(SPC_LAB, spc_biogeochem, by = "horizonID")

#
MGP_LAB <- full_join(mgp_permegapit, mgp_perhorizon, by = "plotID")
MGP_LAB <- left_join(MGP_LAB, mgp_perbulksample, by = "horizonID")
MGP_LAB <- left_join(MGP_LAB, mgp_particlesize, by = "horizonID")
MGP_LAB <- left_join(MGP_LAB, mgp_biogeochem, by = "horizonID")
MGP_LAB <- MGP_LAB %>% arrange(plotID, horizonTopDepth)

MGP_LAB %>% filter(siteID == "TEAK")
MGP_LAB %>% filter(siteID == "RMNP")

#
colnames(SPC_LAB) == colnames(MGP_LAB)
colnames(SPC_LAB)
colnames(MGP_LAB)
NEON_LAB <- rbind(SPC_LAB, MGP_LAB)
#

#
write.csv(NEON_LAB, "D:/r/NEON/NEON_Lab.csv")
#
depths(NEON_LAB) <- plotID ~ horizonTopDepth + horizonBottomDepth
#

saveRDS(NEON_LAB, "D:/r/NEON/NEON_Lab.RDS")
NEON_LAB<- readRDS("D:/r/NEON/NEON_Lab.RDS")



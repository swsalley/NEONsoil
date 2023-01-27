# NEON network soils data to quantify soil variability
#
# this file checks all pedon depth logic to generate a clean pedon dataset for profile compare
# this file makes: "NEON_Lab_clean_20230127.csv"
#
# Author: S.W. Salley, shawn.salley@usda.gov
#

# Libraries
library(dplyr)
library(aqp)

#### load NEON soil lab dataset ####
# NEON_Lab_20230127.csv
# generated from : NEON_lab_filter_20230126.R
soil.all <- read.csv("D:/r/NEON/NEON_Lab_20230127.csv")
soil.all <- soil.all[,c(3:6,19:22,29:31)] %>% arrange(plotID, horizonTopDepth)
str(soil.all)

#### SoilProfileCollection-class object ####
depths(soil.all) <- plotID  ~ horizonTopDepth + horizonBottomDepth
soil.all.chk <- checkHzDepthLogic(soil.all)
soil.all.chk[soil.all.chk$valid == F,]

# Pedons fail depth logic: overlapOrGap
# BARR_001, DELA_010, DELA_028, LAJA_029,  SCBI_011, SCBI_013, SJER_030, TEAK_016, TEAK_023, TOOL_017, TOOL_072

subset(soil.all, plotID == 'BARR_001') # hzid == 83, 84
subset(soil.all, plotID == 'DELA_010') # hzID == 747, 748
subset(soil.all, plotID == 'DELA_028') # hzID == 785, 786
subset(soil.all, plotID == 'LAJA_029') # hzID == 1648, 1649
subset(soil.all, plotID == 'SCBI_011') # hzID == 2466, 2467
subset(soil.all, plotID == 'SCBI_013') # hzID == 2471 , 2472             
subset(soil.all, plotID == 'SJER_030') # hzID == 2682, 2683
subset(soil.all, plotID == 'TEAK_016') # hzID == 3077, 3078
subset(soil.all, plotID == 'TEAK_023') # hzID == 3089
subset(soil.all, plotID == 'TOOL_017') # hzID == 3154, 3155
subset(soil.all, plotID == 'TOOL_072') # hzID == 3209, 3210

# mpedon fail depth logic: sameDepth
# OAES_mp
subset(soil.all, plotID == 'OAES_mp')  # hzID == 2096


# correct overlapOrGap
soil.all <- read.csv("D:/r/NEON/NEON_Lab_20230127.csv")
soil.all <- soil.all %>% arrange(plotID, horizonTopDepth)
# 
subset(soil.all, plotID == 'BARR_001') # hzid == 83, 84
soil.all <- soil.all[soil.all$horizonID != "18N05555",] # remove hz == 84
#
subset(soil.all, plotID == 'DELA_010') # hzID == 747, 748
# unable to expalin depth , correct to average boundary between 72 and 79
soil.all[soil.all$horizonID == "16N02389", 22] <- 75
soil.all[soil.all$horizonID == "16N02390", 21] <- 75
#
subset(soil.all, plotID == 'DELA_028') # hzID == 785, 786
soil.all <- soil.all[soil.all$horizonID != "5351147",] # remove hz == 786
#
subset(soil.all, plotID == 'LAJA_029') # hzID == 1648, 1649
# remove seond 18N02724, or 1649
soil.all <- soil.all[soil.all$X != "836",] # remove hz == 786 or x=836
#
subset(soil.all, plotID == 'SCBI_011') # hzID == 2466, 2467
soil.all <- soil.all[soil.all$X != "317",] # remove hz == 317
#
subset(soil.all, plotID == 'SCBI_013') # hzID == 2471 , 2472             
soil.all <- soil.all[soil.all$X != "326",] # remove hz == 326
#
subset(soil.all, plotID == 'SJER_030') # hzID == 2682, 2683
soil.all <- soil.all[soil.all$horizonID != "5711903",] # remove hz == 5711903
#
subset(soil.all, plotID == 'SJER_030') # hzID == 2682, 2683
soil.all <- soil.all[soil.all$horizonID != "17N04062",] # remove hz == 17N04062
#
subset(soil.all, plotID == 'TEAK_016') # hzID == 22N00772
soil.all <- soil.all[soil.all$horizonID != "22N00772",] # remove hz == 22N00772
#
subset(soil.all, plotID == 'TEAK_023') # hzID == 22N00755
soil.all <- soil.all[soil.all$horizonID != "22N00755",] # remove hz == 22N00755
#
subset(soil.all, plotID == 'TOOL_017') # hzID == 19N00226
soil.all <- soil.all[soil.all$horizonID != "19N00226",] # remove hz == 19N00226
#
subset(soil.all, plotID == 'TOOL_072') # hzID == 19N00278
soil.all <- soil.all[soil.all$horizonID != "19N00278",] # remove hz == 19N00278
#

# correct sameDepth
# OAES_mp
subset(soil.all, plotID == 'OAES_mp')  # hzID == 11_KLEMME_PIT1_R
soil.all <- soil.all[soil.all$horizonID != "11_KLEMME_PIT1_R",] # remove hz == 11_KLEMME_PIT1_R 
# recomend replacing these names "11_KLEMME_PIT1_R" with the NRCS ID ie. : like "19N00278"
#

# recheck depth logic

soil.all.corrected <- soil.all
depths(soil.all.corrected) <- plotID  ~ horizonTopDepth + horizonBottomDepth
soil.all.chk <- checkHzDepthLogic(soil.all.corrected)
soil.all.chk[soil.all.chk$valid == F,]
#

write.csv(soil.all, "D:/r/NEON/NEON_Lab_clean_20230127.csv")

#

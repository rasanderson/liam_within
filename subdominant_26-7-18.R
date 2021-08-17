# Patch statistics

rm(list=ls())

library(vegan)
library(dplyr)
library(tidyverse)
library(SDMTools)

x = seq(from = 10, to = 100, by = 10) #set x axis 10 to 100 by 10 for image
y = seq(from = 10, to = 120, by = 10) #set y axis 10 to 100 by 10 for image
nx = 10 # number of grid cells on x axis
ny = 12 # number of grid cells on y axis

###################################################################################
##                            COLUMN 1                                           ##
################################################################################### 
##               PATCH STATS ON ALL 167 QUADRATS: DOMINANT                       ##
###################################################################################
sub_dominant <- data.frame(read.csv("Data/Dominant-sub_New_Versions/Subdominant_26-07-18.csv", header = TRUE)) # main datafram (all inputs)
colnames(sub_dominant) <- c("quad_no", "Row", 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
# sub_dominant[1,] = "" ## add empty line to streamline with other quadrat rows

quad1_to_167 <- sub_dominant
quad_sub_mat_all <- as.matrix(quad1_to_167[,-1])
quad_sub_tab_all <- table(quad_sub_mat_all[,-1]) # gives count of each species in the quadrat
quad_sub_tab_all
df_sub <- data.frame(quad_sub_tab_all)

#quad_lng <- quad1_to_167 %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
#summary(quad_lng)
#quad_lng <- mutate(quad_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
#levels(quad_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
#             "60", "70", "80", "90", "100") # This is always used before filtering by species name

####### BEGINNING OF LOOP FOR 1 TO 167 QUADRATS #######

#for (quad_no in quad93_to_10){
#quad_dom_tab <- quad93_dom_tab
#quad_lng     <- quad93_lng
spp_in_quad  <- names(quad_dom_tab)
spat_all     <- NULL
for (spp_name in seq_along(spp_in_quad)){
  spp_text <- names(quad_dom_tab)[spp_name]
  quad_spp_lng <- quad_lng %>% filter(Species == spp_text)
  quad_spp_wde <- quad_spp_lng %>% reshape2::dcast(Row ~ Col, drop = FALSE, fill = 0)
  # Recode text to number
  quad_spp_wde[,-1] <- ifelse(quad_spp_wde[,-1]!="0",1,0)
  quad_spp_mat <- as.matrix(quad_spp_wde[,-1])
  
  quad_ccl = ConnCompLabel(quad_spp_mat)
  #print(spp_text)
  #print(quad_ccl)
  #image(t(quad_ccl[10:1,]),col=c('white',rainbow(length(unique(quad_ccl))-1)))
  #grid(nx, ny, col = "grey", lty = 1) # added post roy
  #print(PatchStat(quad_ccl))
  
  # Store the spatial stats
  # Need to duplicate spp text names in final output table
  spp_duplicate <- rep(spp_text, max(quad_ccl)+1)
  patch_stats <- PatchStat(quad_ccl)
  patch_stats <- cbind(spp_duplicate, patch_stats)
  spat_all <- rbind(spat_all, patch_stats)
  #readline()
}
write.csv(spat_all, file = "Results/Patch_Stats/Quad_dom_1-167_spaced.csv")
# cbind in the quadrat no. into spat_all
# }
#write.csv(spat_all, file = "Results/Patch_Stats/Quad_dom.csv")



quad1_to_5 <- Dominant[1:55,]
quad1_to_5 <- quad1_to_5[-1,]
#quad93_to_2 <- Dominant[1:21,]
#quad93_to_2 <- read.csv("Results/Patch_Stats/TEST.csv")
#quad93_to_2 <- quad93_to_2[,-1]
#colnames(quad93_to_2) <- c("quad_no", "Row", 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
#quad93_to_2 <- quad93_to_2[-1,]
# quad93_to_10 <- data.frame(Dominant[2:101,])

################################################################################### 
##                         TEST ON INDIVIDUAL QUADRATS                           ##
###################################################################################

## QUADRAT 1

#quad_no <- quad93_to_5$quad_no
#quad93_dom <- as.data.frame(Dominant[1:11,])
#quad93_dom <- quad93_dom[-1,]
#quad93_dom <- quad93_dom[,-1]
#colnames(quad93_dom) <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
#quad93_dom <- cbind(Quad = 1, Row = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100), quad93_dom)

quad1_sub <- subset(sub_dominant, quad_no == "1")
quad1_sub_mat <- as.matrix(quad1_sub[,-1])
quad1_sub_tab <- table(quad1_sub_mat[,-1]) # gives count of each species in the quadrat
quad1_sub_tab
quad1_lng <- quad1_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad1_lng)
quad1_lng <- mutate(quad1_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad1_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                           "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad1_dom_tab 
quad_lng     <- quad1_lng

## QUADRAT 2
quad2_sub <- subset(sub_dominant, quad_no == "2")
quad2_sub_mat <- as.matrix(quad2_sub[,-1])
quad2_sub_tab <- table(quad2_sub_mat[,-1]) # gives count of each species in the quadrat
quad2_sub_tab
quad2_lng <- quad2_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad2_lng)
quad2_lng <- mutate(quad2_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad2_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                           "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad2_sub_tab 
quad_lng     <- quad2_lng

## QUADRAT 3
quad3_sub <- subset(sub_dominant, quad_no == "3")
quad3_sub_mat <- as.matrix(quad3_sub[,-1])
quad3_sub_tab <- table(quad3_sub_mat[,-1]) # gives count of each species in the quadrat
quad3_sub_tab
quad3_lng <- quad3_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad3_lng)
quad3_lng <- mutate(quad3_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad3_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                           "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad3_sub_tab 
quad_lng     <- quad3_lng

## QUADRAT 4
quad4_sub <- subset(sub_dominant, quad_no == "4")
quad4_sub_mat <- as.matrix(quad4_sub[,-1])
quad4_sub_tab <- table(quad4_sub_mat[,-1]) # gives count of each species in the quadrat
quad4_sub_tab
quad4_lng <- quad4_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad4_lng)
quad4_lng <- mutate(quad4_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad4_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                           "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad4_sub_tab 
quad_lng     <- quad4_lng

## QUADRAT 5
quad5_sub <- subset(sub_dominant, quad_no == "5")
quad5_sub_mat <- as.matrix(quad5_sub[,-1])
quad5_sub_tab <- table(quad5_sub_mat[,-1]) # gives count of each species in the quadrat
quad5_sub_tab
quad5_lng <- quad5_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad5_lng)
quad5_lng <- mutate(quad5_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad5_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                           "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad5_sub_tab 
quad_lng     <- quad5_lng

## QUAD 6
quad6_sub <- subset(sub_dominant, quad_no == "6")
quad6_sub_mat <- as.matrix(quad6_sub[,-1])
quad6_sub_tab <- table(quad6_sub_mat[,-1]) # gives count of each species in the quadrat
quad6_sub_tab
quad6_lng <- quad6_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad6_lng)
quad6_lng <- mutate(quad6_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad6_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                           "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad6_sub_tab 
quad_lng     <- quad6_lng

## QUAD 7
quad7_sub <- subset(sub_dominant, quad_no == "7")
quad7_sub_mat <- as.matrix(quad7_sub[,-1])
quad7_sub_tab <- table(quad7_sub_mat[,-1]) # gives count of each species in the quadrat
quad7_sub_tab
quad7_lng <- quad7_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad7_lng)
quad7_lng <- mutate(quad7_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad7_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                           "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad7_sub_tab 
quad_lng     <- quad7_lng

## QUAD 8
quad8_sub <- subset(sub_dominant, quad_no == "8")
quad8_sub_mat <- as.matrix(quad8_sub[,-1])
quad8_sub_tab <- table(quad8_sub_mat[,-1]) # gives count of each species in the quadrat
quad8_sub_tab
quad8_lng <- quad8_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad8_lng)
quad8_lng <- mutate(quad8_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad8_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                           "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad8_sub_tab 
quad_lng     <- quad8_lng

## QUAD 9
quad9_sub <- subset(sub_dominant, quad_no == "9")
quad9_sub_mat <- as.matrix(quad9_sub[,-1])
quad9_sub_tab <- table(quad9_sub_mat[,-1]) # gives count of each species in the quadrat
quad9_sub_tab
quad9_lng <- quad9_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad9_lng)
quad9_lng <- mutate(quad9_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad9_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                           "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad9_sub_tab 
quad_lng     <- quad9_lng

## quad10
quad10_sub <- subset(sub_dominant, quad_no == "10")
quad10_sub_mat <- as.matrix(quad10_sub[,-1])
quad10_sub_tab <- table(quad10_sub_mat[,-1]) # gives count of each species in the quadrat
quad10_sub_tab
quad10_lng <- quad10_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad10_lng)
quad10_lng <- mutate(quad10_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad10_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad10_sub_tab 
quad_lng     <- quad10_lng

## quad11
quad11_sub <- subset(sub_dominant, quad_no == "11")
quad11_sub_mat <- as.matrix(quad11_sub[,-1])
quad11_sub_tab <- table(quad11_sub_mat[,-1]) # gives count of each species in the quadrat
quad11_sub_tab
quad11_lng <- quad11_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad11_lng)
quad11_lng <- mutate(quad11_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad11_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad11_sub_tab 
quad_lng     <- quad11_lng

## QUAD 12
quad12_sub <- subset(sub_dominant, quad_no == "12")
quad12_sub_mat <- as.matrix(quad12_sub[,-1])
quad12_sub_tab <- table(quad12_sub_mat[,-1]) # gives count of each species in the quadrat
quad12_sub_tab
quad12_lng <- quad12_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad12_lng)
quad12_lng <- mutate(quad12_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad12_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad12_sub_tab 
quad_lng     <- quad12_lng

## QUAD 13
quad13_sub <- subset(sub_dominant, quad_no == "13")
quad13_sub_mat <- as.matrix(quad13_sub[,-1])
quad13_sub_tab <- table(quad13_sub_mat[,-1]) # gives count of each species in the quadrat
quad13_sub_tab
quad13_lng <- quad13_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad13_lng)
quad13_lng <- mutate(quad13_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad13_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad13_sub_tab 
quad_lng     <- quad13_lng

## QUAD 14
quad14_sub <- subset(sub_dominant, quad_no == "14")
quad14_sub_mat <- as.matrix(quad14_sub[,-1])
quad14_sub_tab <- table(quad14_sub_mat[,-1]) # gives count of each species in the quadrat
quad14_sub_tab
quad14_lng <- quad14_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad14_lng)
quad14_lng <- mutate(quad14_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad14_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad14_sub_tab 
quad_lng     <- quad14_lng

## QUAD 15
quad15_sub <- subset(sub_dominant, quad_no == "15")
quad15_sub_mat <- as.matrix(quad15_sub[,-1])
quad15_sub_tab <- table(quad15_sub_mat[,-1]) # gives count of each species in the quadrat
quad15_sub_tab
quad15_lng <- quad15_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad15_lng)
quad15_lng <- mutate(quad15_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad15_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad15_sub_tab 
quad_lng     <- quad15_lng

## QUAD 16
quad16_sub <- subset(sub_dominant, quad_no == "16")
quad16_sub_mat <- as.matrix(quad16_sub[,-1])
quad16_sub_tab <- table(quad16_sub_mat[,-1]) # gives count of each species in the quadrat
quad16_sub_tab
quad16_lng <- quad16_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad16_lng)
quad16_lng <- mutate(quad16_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad16_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad16_sub_tab 
quad_lng     <- quad16_lng

## QUAD 17
quad17_sub <- subset(sub_dominant, quad_no == "17")
quad17_sub_mat <- as.matrix(quad17_sub[,-1])
quad17_sub_tab <- table(quad17_sub_mat[,-1]) # gives count of each species in the quadrat
quad17_sub_tab
quad17_lng <- quad17_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad17_lng)
quad17_lng <- mutate(quad17_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad17_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad17_sub_tab 
quad_lng     <- quad17_lng

## QUAD 18
quad18_sub <- subset(sub_dominant, quad_no == "18")
quad18_sub_mat <- as.matrix(quad18_sub[,-1])
quad18_sub_tab <- table(quad18_sub_mat[,-1]) # gives count of each species in the quadrat
quad18_sub_tab
quad18_lng <- quad18_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad18_lng)
quad18_lng <- mutate(quad18_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad18_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad18_sub_tab 
quad_lng     <- quad18_lng

## QUAD 19
quad19_sub <- subset(sub_dominant, quad_no == "19")
quad19_sub_mat <- as.matrix(quad19_sub[,-1])
quad19_sub_tab <- table(quad19_sub_mat[,-1]) # gives count of each species in the quadrat
quad19_sub_tab
quad19_lng <- quad19_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad19_lng)
quad19_lng <- mutate(quad19_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad19_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad19_sub_tab 
quad_lng     <- quad19_lng

## QUAD 20
quad20_sub <- subset(sub_dominant, quad_no == "20")
quad20_sub_mat <- as.matrix(quad20_sub[,-1])
quad20_sub_tab <- table(quad20_sub_mat[,-1]) # gives count of each species in the quadrat
quad20_sub_tab
quad20_lng <- quad20_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad20_lng)
quad20_lng <- mutate(quad20_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad20_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad20_sub_tab 
quad_lng     <- quad20_lng

## QUAD 21
quad21_sub <- subset(sub_dominant, quad_no == "21")
quad21_sub_mat <- as.matrix(quad21_sub[,-1])
quad21_sub_tab <- table(quad21_sub_mat[,-1]) # gives count of each species in the quadrat
quad21_sub_tab
quad21_lng <- quad21_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad21_lng)
quad21_lng <- mutate(quad21_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad21_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad21_sub_tab 
quad_lng     <- quad21_lng

## QUAD 22
quad22_sub <- subset(sub_dominant, quad_no == "22")
quad22_sub_mat <- as.matrix(quad22_sub[,-1])
quad22_sub_tab <- table(quad22_sub_mat[,-1]) # gives count of each species in the quadrat
quad22_sub_tab
quad22_lng <- quad22_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad22_lng)
quad22_lng <- mutate(quad22_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad22_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad22_sub_tab 
quad_lng     <- quad22_lng

## QUAD 23
quad23_sub <- subset(sub_dominant, quad_no == "23")
quad23_sub_mat <- as.matrix(quad23_sub[,-1])
quad23_sub_tab <- table(quad23_sub_mat[,-1]) # gives count of each species in the quadrat
quad23_sub_tab
quad23_lng <- quad23_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad23_lng)
quad23_lng <- mutate(quad23_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad23_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad23_sub_tab 
quad_lng     <- quad23_lng

## QUAD 24
quad24_sub <- subset(sub_dominant, quad_no == "24")
quad24_sub_mat <- as.matrix(quad24_sub[,-1])
quad24_sub_tab <- table(quad24_sub_mat[,-1]) # gives count of each species in the quadrat
quad24_sub_tab
quad24_lng <- quad24_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad24_lng)
quad24_lng <- mutate(quad24_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad24_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad24_sub_tab 
quad_lng     <- quad24_lng

## QUAD 25
quad25_sub <- subset(sub_dominant, quad_no == "25")
quad25_sub_mat <- as.matrix(quad25_sub[,-1])
quad25_sub_tab <- table(quad25_sub_mat[,-1]) # gives count of each species in the quadrat
quad25_sub_tab
quad25_lng <- quad25_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad25_lng)
quad25_lng <- mutate(quad25_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad25_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad25_sub_tab 
quad_lng     <- quad25_lng

## QUAD 26
quad26_sub <- subset(sub_dominant, quad_no == "26")
quad26_sub_mat <- as.matrix(quad26_sub[,-1])
quad26_sub_tab <- table(quad26_sub_mat[,-1]) # gives count of each species in the quadrat
quad26_sub_tab
quad26_lng <- quad26_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad26_lng)
quad26_lng <- mutate(quad26_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad26_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad26_sub_tab 
quad_lng     <- quad26_lng

## QUAD 27
quad27_sub <- subset(sub_dominant, quad_no == "27")
quad27_sub_mat <- as.matrix(quad27_sub[,-1])
quad27_sub_tab <- table(quad27_sub_mat[,-1]) # gives count of each species in the quadrat
quad27_sub_tab
quad27_lng <- quad27_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad27_lng)
quad27_lng <- mutate(quad27_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad27_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad27_sub_tab 
quad_lng     <- quad27_lng

## QUAD 28
quad28_sub <- subset(sub_dominant, quad_no == "28")
quad28_sub_mat <- as.matrix(quad28_sub[,-1])
quad28_sub_tab <- table(quad28_sub_mat[,-1]) # gives count of each species in the quadrat
quad28_sub_tab
quad28_lng <- quad28_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad28_lng)
quad28_lng <- mutate(quad28_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad28_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad28_sub_tab 
quad_lng     <- quad28_lng

## QUAD 30
quad29_sub <- subset(sub_dominant, quad_no == "29")
quad29_sub_mat <- as.matrix(quad29_sub[,-1])
quad29_sub_tab <- table(quad29_sub_mat[,-1]) # gives count of each species in the quadrat
quad29_sub_tab
quad29_lng <- quad29_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad29_lng)
quad29_lng <- mutate(quad29_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad29_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad29_sub_tab 
quad_lng     <- quad29_lng

## QUAD 30
quad30_sub <- subset(sub_dominant, quad_no == "30")
quad30_sub_mat <- as.matrix(quad30_sub[,-1])
quad30_sub_tab <- table(quad30_sub_mat[,-1]) # gives count of each species in the quadrat
quad30_sub_tab
quad30_lng <- quad30_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad30_lng)
quad30_lng <- mutate(quad30_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad30_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad30_sub_tab 
quad_lng     <- quad30_lng

## QUAD 31
quad31_sub <- subset(sub_dominant, quad_no == "31")
quad31_sub_mat <- as.matrix(quad31_sub[,-1])
quad31_sub_tab <- table(quad31_sub_mat[,-1]) # gives count of each species in the quadrat
quad31_sub_tab
quad31_lng <- quad31_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad31_lng)
quad31_lng <- mutate(quad31_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad31_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad31_sub_tab 
quad_lng     <- quad31_lng

## QUAD 32
quad32_sub <- subset(sub_dominant, quad_no == "32")
quad32_sub_mat <- as.matrix(quad32_sub[,-1])
quad32_sub_tab <- table(quad32_sub_mat[,-1]) # gives count of each species in the quadrat
quad32_sub_tab
quad32_lng <- quad32_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad32_lng)
quad32_lng <- mutate(quad32_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad32_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad32_sub_tab 
quad_lng     <- quad32_lng

## QUAD 33
quad33_sub <- subset(sub_dominant, quad_no == "33")
quad33_sub_mat <- as.matrix(quad33_sub[,-1])
quad33_sub_tab <- table(quad33_sub_mat[,-1]) # gives count of each species in the quadrat
quad33_sub_tab
quad33_lng <- quad33_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad33_lng)
quad33_lng <- mutate(quad33_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad33_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad33_sub_tab 
quad_lng     <- quad33_lng

## QUAD 34
quad34_sub <- subset(sub_dominant, quad_no == "34")
quad34_sub_mat <- as.matrix(quad34_sub[,-1])
quad34_sub_tab <- table(quad34_sub_mat[,-1]) # gives count of each species in the quadrat
quad34_sub_tab
quad34_lng <- quad34_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad34_lng)
quad34_lng <- mutate(quad34_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad34_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad34_sub_tab 
quad_lng     <- quad34_lng

## QUAD 35
quad35_sub <- subset(sub_dominant, quad_no == "35")
quad35_sub_mat <- as.matrix(quad35_sub[,-1])
quad35_sub_tab <- table(quad35_sub_mat[,-1]) # gives count of each species in the quadrat
quad35_sub_tab
quad35_lng <- quad35_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad35_lng)
quad35_lng <- mutate(quad35_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad35_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad35_sub_tab 
quad_lng     <- quad35_lng

## QUAD 36
quad36_sub <- subset(sub_dominant, quad_no == "36")
quad36_sub_mat <- as.matrix(quad36_sub[,-1])
quad36_sub_tab <- table(quad36_sub_mat[,-1]) # gives count of each species in the quadrat
quad36_sub_tab
quad36_lng <- quad36_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad36_lng)
quad36_lng <- mutate(quad36_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad36_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad36_sub_tab 
quad_lng     <- quad36_lng

## QUAD 37
quad37_sub <- subset(sub_dominant, quad_no == "37")
quad37_sub_mat <- as.matrix(quad37_sub[,-1])
quad37_sub_tab <- table(quad37_sub_mat[,-1]) # gives count of each species in the quadrat
quad37_sub_tab
quad37_lng <- quad37_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad37_lng)
quad37_lng <- mutate(quad37_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad37_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad37_sub_tab 
quad_lng     <- quad37_lng

## QUAD 38
quad38_sub <- subset(sub_dominant, quad_no == "38")
quad38_sub_mat <- as.matrix(quad38_sub[,-1])
quad38_sub_tab <- table(quad38_sub_mat[,-1]) # gives count of each species in the quadrat
quad38_sub_tab
quad38_lng <- quad38_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad38_lng)
quad38_lng <- mutate(quad38_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad38_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad38_sub_tab 
quad_lng     <- quad38_lng

## QUAD 40
quad39_sub <- subset(sub_dominant, quad_no == "39")
quad39_sub_mat <- as.matrix(quad39_sub[,-1])
quad39_sub_tab <- table(quad39_sub_mat[,-1]) # gives count of each species in the quadrat
quad39_sub_tab
quad39_lng <- quad39_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad39_lng)
quad39_lng <- mutate(quad39_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad39_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad39_sub_tab 
quad_lng     <- quad39_lng

## QUAD 40
quad40_sub <- subset(sub_dominant, quad_no == "40")
quad40_sub_mat <- as.matrix(quad40_sub[,-1])
quad40_sub_tab <- table(quad40_sub_mat[,-1]) # gives count of each species in the quadrat
quad40_sub_tab
quad40_lng <- quad40_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad40_lng)
quad40_lng <- mutate(quad40_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad40_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad40_sub_tab 
quad_lng     <- quad40_lng

## QUAD 41
quad41_sub <- subset(sub_dominant, quad_no == "41")
quad41_sub_mat <- as.matrix(quad41_sub[,-1])
quad41_sub_tab <- table(quad41_sub_mat[,-1]) # gives count of each species in the quadrat
quad41_sub_tab
quad41_lng <- quad41_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad41_lng)
quad41_lng <- mutate(quad41_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad41_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad40_sub_tab 
quad_lng     <- quad40_lng

## QUAD 42
quad42_sub <- subset(sub_dominant, quad_no == "42")
quad42_sub_mat <- as.matrix(quad42_sub[,-1])
quad42_sub_tab <- table(quad42_sub_mat[,-1]) # gives count of each species in the quadrat
quad42_sub_tab
quad42_lng <- quad42_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad42_lng)
quad42_lng <- mutate(quad42_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad42_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad42_sub_tab 
quad_lng     <- quad42_lng

## QUAD 43
quad43_sub <- subset(sub_dominant, quad_no == "43")
quad43_sub_mat <- as.matrix(quad43_sub[,-1])
quad43_sub_tab <- table(quad43_sub_mat[,-1]) # gives count of each species in the quadrat
quad43_sub_tab
quad43_lng <- quad43_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad43_lng)
quad43_lng <- mutate(quad43_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad43_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad43_sub_tab 
quad_lng     <- quad43_lng

## QUAD 44
quad44_sub <- subset(sub_dominant, quad_no == "44")
quad44_sub_mat <- as.matrix(quad44_sub[,-1])
quad44_sub_tab <- table(quad44_sub_mat[,-1]) # gives count of each species in the quadrat
quad44_sub_tab
quad44_lng <- quad44_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad44_lng)
quad44_lng <- mutate(quad44_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad44_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad44_sub_tab 
quad_lng     <- quad44_lng

## QUAD 45
quad45_sub <- subset(sub_dominant, quad_no == "45")
quad45_sub_mat <- as.matrix(quad45_sub[,-1])
quad45_sub_tab <- table(quad45_sub_mat[,-1]) # gives count of each species in the quadrat
quad45_sub_tab
quad45_lng <- quad45_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad45_lng)
quad45_lng <- mutate(quad45_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad45_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad45_sub_tab 
quad_lng     <- quad45_lng

## QUAD 46
quad46_sub <- subset(sub_dominant, quad_no == "46")
quad46_sub_mat <- as.matrix(quad46_sub[,-1])
quad46_sub_tab <- table(quad46_sub_mat[,-1]) # gives count of each species in the quadrat
quad46_sub_tab
quad46_lng <- quad46_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad46_lng)
quad46_lng <- mutate(quad46_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad46_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad46_sub_tab 
quad_lng     <- quad46_lng

## QUAD 47
quad47_sub <- subset(sub_dominant, quad_no == "47")
quad47_sub_mat <- as.matrix(quad47_sub[,-1])
quad47_sub_tab <- table(quad47_sub_mat[,-1]) # gives count of each species in the quadrat
quad47_sub_tab
quad47_lng <- quad47_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad47_lng)
quad47_lng <- mutate(quad47_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad47_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad47_sub_tab 
quad_lng     <- quad47_lng

## QUAD 48
quad48_sub <- subset(sub_dominant, quad_no == "48")
quad48_sub_mat <- as.matrix(quad48_sub[,-1])
quad48_sub_tab <- table(quad48_sub_mat[,-1]) # gives count of each species in the quadrat
quad48_sub_tab
quad48_lng <- quad48_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad48_lng)
quad48_lng <- mutate(quad48_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad48_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad48_sub_tab 
quad_lng     <- quad48_lng

## QUAD 49
quad49_sub <- subset(sub_dominant, quad_no == "49")
quad49_sub_mat <- as.matrix(quad49_sub[,-1])
quad49_sub_tab <- table(quad49_sub_mat[,-1]) # gives count of each species in the quadrat
quad49_sub_tab
quad49_lng <- quad49_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad49_lng)
quad49_lng <- mutate(quad49_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad49_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad49_sub_tab 
quad_lng     <- quad49_lng

## QUAD 50
quad50_sub <- subset(sub_dominant, quad_no == "50")
quad50_sub_mat <- as.matrix(quad50_sub[,-1])
quad50_sub_tab <- table(quad50_sub_mat[,-1]) # gives count of each species in the quadrat
quad50_sub_tab
quad50_lng <- quad50_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad50_lng)
quad50_lng <- mutate(quad50_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad50_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad50_sub_tab 
quad_lng     <- quad50_lng

## QUAD 51
quad51_sub <- subset(sub_dominant, quad_no == "51")
quad51_sub_mat <- as.matrix(quad51_sub[,-1])
quad51_sub_tab <- table(quad51_sub_mat[,-1]) # gives count of each species in the quadrat
quad51_sub_tab
quad51_lng <- quad51_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad51_lng)
quad51_lng <- mutate(quad51_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad51_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad51_sub_tab 
quad_lng     <- quad51_lng

## QUAD 52
quad52_sub <- subset(sub_dominant, quad_no == "52")
quad52_sub_mat <- as.matrix(quad52_sub[,-1])
quad52_sub_tab <- table(quad52_sub_mat[,-1]) # gives count of each species in the quadrat
quad52_sub_tab
quad52_lng <- quad52_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad52_lng)
quad52_lng <- mutate(quad52_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad52_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad52_sub_tab 
quad_lng     <- quad52_lng


## QUAD 53
quad53_sub <- subset(sub_dominant, quad_no == "53")
quad53_sub_mat <- as.matrix(quad53_sub[,-1])
quad53_sub_tab <- table(quad53_sub_mat[,-1]) # gives count of each species in the quadrat
quad53_sub_tab
quad53_lng <- quad53_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad53_lng)
quad53_lng <- mutate(quad53_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad53_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad53_sub_tab 
quad_lng     <- quad53_lng

## QUAD 54
quad54_sub <- subset(sub_dominant, quad_no == "54")
quad54_sub_mat <- as.matrix(quad54_sub[,-1])
quad54_sub_tab <- table(quad54_sub_mat[,-1]) # gives count of each species in the quadrat
quad54_sub_tab
quad54_lng <- quad54_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad54_lng)
quad54_lng <- mutate(quad54_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad54_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad54_sub_tab 
quad_lng     <- quad54_lng

## QUAD 55
quad55_sub <- subset(sub_dominant, quad_no == "55")
quad55_sub_mat <- as.matrix(quad55_sub[,-1])
quad55_sub_tab <- table(quad55_sub_mat[,-1]) # gives count of each species in the quadrat
quad55_sub_tab
quad55_lng <- quad55_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad55_lng)
quad55_lng <- mutate(quad55_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad55_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad55_sub_tab 
quad_lng     <- quad55_lng

## QUAD 56
quad56_sub <- subset(sub_dominant, quad_no == "56")
quad56_sub_mat <- as.matrix(quad56_sub[,-1])
quad56_sub_tab <- table(quad56_sub_mat[,-1]) # gives count of each species in the quadrat
quad56_sub_tab
quad56_lng <- quad56_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad56_lng)
quad56_lng <- mutate(quad56_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad56_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad56_sub_tab 
quad_lng     <- quad56_lng

## QUAD 57
quad57_sub <- subset(sub_dominant, quad_no == "57")
quad57_sub_mat <- as.matrix(quad57_sub[,-1])
quad57_sub_tab <- table(quad57_sub_mat[,-1]) # gives count of each species in the quadrat
quad57_sub_tab
quad57_lng <- quad57_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad57_lng)
quad57_lng <- mutate(quad57_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad57_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad57_sub_tab 
quad_lng     <- quad57_lng

## QUAD 58
quad58_sub <- subset(sub_dominant, quad_no == "58")
quad58_sub_mat <- as.matrix(quad58_sub[,-1])
quad58_sub_tab <- table(quad58_sub_mat[,-1]) # gives count of each species in the quadrat
quad58_sub_tab
quad58_lng <- quad58_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad58_lng)
quad58_lng <- mutate(quad58_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad58_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad58_sub_tab 
quad_lng     <- quad58_lng

## QUAD 59
quad59_sub <- subset(sub_dominant, quad_no == "59")
quad59_sub_mat <- as.matrix(quad59_sub[,-1])
quad59_sub_tab <- table(quad59_sub_mat[,-1]) # gives count of each species in the quadrat
quad59_sub_tab
quad59_lng <- quad59_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad59_lng)
quad59_lng <- mutate(quad59_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad59_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad59_sub_tab 
quad_lng     <- quad59_lng

## QUAD 60
quad60_sub <- subset(sub_dominant, quad_no == "60")
quad60_sub_mat <- as.matrix(quad60_sub[,-1])
quad60_sub_tab <- table(quad60_sub_mat[,-1]) # gives count of each species in the quadrat
quad60_sub_tab
quad60_lng <- quad60_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad60_lng)
quad60_lng <- mutate(quad60_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad60_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad60_sub_tab 
quad_lng     <- quad60_lng

## QUAD 61
quad61_sub <- subset(sub_dominant, quad_no == "61")
quad61_sub_mat <- as.matrix(quad61_sub[,-1])
quad61_sub_tab <- table(quad61_sub_mat[,-1]) # gives count of each species in the quadrat
quad61_sub_tab
quad61_lng <- quad61_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad61_lng)
quad61_lng <- mutate(quad61_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad61_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad61_sub_tab 
quad_lng     <- quad61_lng

## QUAD 62
quad62_sub <- subset(sub_dominant, quad_no == "62")
quad62_sub_mat <- as.matrix(quad62_sub[,-1])
quad62_sub_tab <- table(quad62_sub_mat[,-1]) # gives count of each species in the quadrat
quad62_sub_tab
quad62_lng <- quad62_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad62_lng)
quad62_lng <- mutate(quad62_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad62_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad62_sub_tab 
quad_lng     <- quad62_lng

## QUAD 63
quad63_sub <- subset(sub_dominant, quad_no == "63")
quad63_sub_mat <- as.matrix(quad63_sub[,-1])
quad63_sub_tab <- table(quad63_sub_mat[,-1]) # gives count of each species in the quadrat
quad63_sub_tab
quad63_lng <- quad63_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad63_lng)
quad63_lng <- mutate(quad63_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad63_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad63_sub_tab 
quad_lng     <- quad63_lng

## QUAD 64
quad64_sub <- subset(sub_dominant, quad_no == "64")
quad64_sub_mat <- as.matrix(quad64_sub[,-1])
quad64_sub_tab <- table(quad64_sub_mat[,-1]) # gives count of each species in the quadrat
quad64_sub_tab
quad64_lng <- quad64_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad64_lng)
quad64_lng <- mutate(quad64_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad64_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad64_sub_tab 
quad_lng     <- quad64_lng

## QUAD 65
quad65_sub <- subset(sub_dominant, quad_no == "65")
quad65_sub_mat <- as.matrix(quad65_sub[,-1])
quad65_sub_tab <- table(quad65_sub_mat[,-1]) # gives count of each species in the quadrat
quad65_sub_tab
quad65_lng <- quad65_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad65_lng)
quad65_lng <- mutate(quad65_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad65_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad65_sub_tab 
quad_lng     <- quad65_lng

## QUAD 66
quad66_sub <- subset(sub_dominant, quad_no == "66")
quad66_sub_mat <- as.matrix(quad66_sub[,-1])
quad66_sub_tab <- table(quad66_sub_mat[,-1]) # gives count of each species in the quadrat
quad66_sub_tab
quad66_lng <- quad66_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad66_lng)
quad66_lng <- mutate(quad66_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad66_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad66_sub_tab 
quad_lng     <- quad66_lng

## QUAD 67
quad67_sub <- subset(sub_dominant, quad_no == "67")
quad67_sub_mat <- as.matrix(quad67_sub[,-1])
quad67_sub_tab <- table(quad67_sub_mat[,-1]) # gives count of each species in the quadrat
quad67_sub_tab
quad67_lng <- quad67_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad67_lng)
quad67_lng <- mutate(quad67_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad67_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad67_sub_tab 
quad_lng     <- quad67_lng

## QUAD 68
quad68_sub <- subset(sub_dominant, quad_no == "68")
quad68_sub_mat <- as.matrix(quad68_sub[,-1])
quad68_sub_tab <- table(quad68_sub_mat[,-1]) # gives count of each species in the quadrat
quad68_sub_tab
quad68_lng <- quad68_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad68_lng)
quad68_lng <- mutate(quad68_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad68_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad68_sub_tab 
quad_lng     <- quad68_lng

## QUAD 69
quad69_sub <- subset(sub_dominant, quad_no == "69")
quad69_sub_mat <- as.matrix(quad69_sub[,-1])
quad69_sub_tab <- table(quad69_sub_mat[,-1]) # gives count of each species in the quadrat
quad69_sub_tab
quad69_lng <- quad69_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad69_lng)
quad69_lng <- mutate(quad69_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad69_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad69_sub_tab 
quad_lng     <- quad69_lng

## QUAD 70
quad70_sub <- subset(sub_dominant, quad_no == "70")
quad70_sub_mat <- as.matrix(quad70_sub[,-1])
quad70_sub_tab <- table(quad70_sub_mat[,-1]) # gives count of each species in the quadrat
quad70_sub_tab
quad70_lng <- quad70_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad70_lng)
quad70_lng <- mutate(quad70_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad70_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad70_sub_tab 
quad_lng     <- quad70_lng

## QUAD 71
quad71_sub <- subset(sub_dominant, quad_no == "71")
quad71_sub_mat <- as.matrix(quad71_sub[,-1])
quad71_sub_tab <- table(quad71_sub_mat[,-1]) # gives count of each species in the quadrat
quad71_sub_tab
quad71_lng <- quad71_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad71_lng)
quad71_lng <- mutate(quad71_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad71_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad71_sub_tab 
quad_lng     <- quad71_lng

## QUAD 72
quad72_sub <- subset(sub_dominant, quad_no == "72")
quad72_sub_mat <- as.matrix(quad72_sub[,-1])
quad72_sub_tab <- table(quad72_sub_mat[,-1]) # gives count of each species in the quadrat
quad72_sub_tab
quad72_lng <- quad72_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad72_lng)
quad72_lng <- mutate(quad72_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad72_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad72_sub_tab 
quad_lng     <- quad72_lng

## QUAD 73
quad73_sub <- subset(sub_dominant, quad_no == "73")
quad73_sub_mat <- as.matrix(quad73_sub[,-1])
quad73_sub_tab <- table(quad73_sub_mat[,-1]) # gives count of each species in the quadrat
quad73_sub_tab
quad73_lng <- quad73_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad73_lng)
quad73_lng <- mutate(quad73_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad73_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad73_sub_tab 
quad_lng     <- quad73_lng

## QUAD 74
quad74_sub <- subset(sub_dominant, quad_no == "74")
quad74_sub_mat <- as.matrix(quad74_sub[,-1])
quad74_sub_tab <- table(quad74_sub_mat[,-1]) # gives count of each species in the quadrat
quad74_sub_tab
quad74_lng <- quad74_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad74_lng)
quad74_lng <- mutate(quad74_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad74_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad74_sub_tab 
quad_lng     <- quad74_lng

## QUAD 75
quad75_sub <- subset(sub_dominant, quad_no == "75")
quad75_sub_mat <- as.matrix(quad75_sub[,-1])
quad75_sub_tab <- table(quad75_sub_mat[,-1]) # gives count of each species in the quadrat
quad75_sub_tab
quad75_lng <- quad75_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad75_lng)
quad75_lng <- mutate(quad75_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad75_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad75_sub_tab 
quad_lng     <- quad75_lng

## QUAD 76
quad76_sub <- subset(sub_dominant, quad_no == "1")
quad76_sub_mat <- as.matrix(quad76_sub[,-1])
quad76_sub_tab <- table(quad76_sub_mat[,-1]) # gives count of each species in the quadrat
quad76_sub_tab
quad76_lng <- quad76_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad76_lng)
quad76_lng <- mutate(quad76_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad76_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad76_sub_tab 
quad_lng     <- quad76_lng

## QUAD 77
quad77_sub <- subset(sub_dominant, quad_no == "77")
quad77_sub_mat <- as.matrix(quad77_sub[,-1])
quad77_sub_tab <- table(quad77_sub_mat[,-1]) # gives count of each species in the quadrat
quad77_sub_tab
quad77_lng <- quad77_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad77_lng)
quad77_lng <- mutate(quad77_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad77_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad77_sub_tab 
quad_lng     <- quad77_lng

## QUAD 78
quad78_sub <- subset(sub_dominant, quad_no == "78")
quad78_sub_mat <- as.matrix(quad78_sub[,-1])
quad78_sub_tab <- table(quad78_sub_mat[,-1]) # gives count of each species in the quadrat
quad78_sub_tab
quad78_lng <- quad78_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad78_lng)
quad78_lng <- mutate(quad78_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad78_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad78_sub_tab 
quad_lng     <- quad78_lng

## QUAD 79
quad79_sub <- subset(sub_dominant, quad_no == "79")
quad79_sub_mat <- as.matrix(quad79_sub[,-1])
quad79_sub_tab <- table(quad79_sub_mat[,-1]) # gives count of each species in the quadrat
quad79_sub_tab
quad79_lng <- quad79_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad79_lng)
quad79_lng <- mutate(quad79_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad79_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad79_sub_tab 
quad_lng     <- quad79_lng

## QUAD 80
quad80_sub <- subset(sub_dominant, quad_no == "80")
quad80_sub_mat <- as.matrix(quad80_sub[,-1])
quad80_sub_tab <- table(quad80_sub_mat[,-1]) # gives count of each species in the quadrat
quad80_sub_tab
quad80_lng <- quad80_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad80_lng)
quad80_lng <- mutate(quad80_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad80_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad80_sub_tab 
quad_lng     <- quad80_lng

## QUAD 81
quad81_sub <- subset(sub_dominant, quad_no == "81")
quad81_sub_mat <- as.matrix(quad81_sub[,-1])
quad81_sub_tab <- table(quad81_sub_mat[,-1]) # gives count of each species in the quadrat
quad81_sub_tab
quad81_lng <- quad81_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad81_lng)
quad81_lng <- mutate(quad81_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad81_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad81_sub_tab 
quad_lng     <- quad81_lng

## QUAD 82
quad82_sub <- subset(sub_dominant, quad_no == "82")
quad82_sub_mat <- as.matrix(quad82_sub[,-1])
quad82_sub_tab <- table(quad82_sub_mat[,-1]) # gives count of each species in the quadrat
quad82_sub_tab
quad82_lng <- quad82_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad82_lng)
quad82_lng <- mutate(quad82_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad82_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad82_sub_tab 
quad_lng     <- quad82_lng

## QUAD 83
quad83_sub <- subset(sub_dominant, quad_no == "83")
quad83_sub_mat <- as.matrix(quad83_sub[,-1])
quad83_sub_tab <- table(quad83_sub_mat[,-1]) # gives count of each species in the quadrat
quad83_sub_tab
quad83_lng <- quad83_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad83_lng)
quad83_lng <- mutate(quad83_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad83_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad83_sub_tab <- quad83_sub_tab 
quad_lng     <- quad83_lng

## QUAD 84
quad84_sub <- subset(sub_dominant, quad_no == "84")
quad84_sub_mat <- as.matrix(quad84_sub[,-1])
quad84_sub_tab <- table(quad84_sub_mat[,-1]) # gives count of each species in the quadrat
quad84_sub_tab
quad84_lng <- quad84_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad84_lng)
quad84_lng <- mutate(quad84_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad84_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad84_sub_tab 
quad_lng     <- quad84_lng

## QUAD 85
quad85_sub <- subset(sub_dominant, quad_no == "85")
quad85_sub_mat <- as.matrix(quad85_sub[,-1])
quad85_sub_tab <- table(quad85_sub_mat[,-1]) # gives count of each species in the quadrat
quad85_sub_tab
quad85_lng <- quad85_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad85_lng)
quad85_lng <- mutate(quad85_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad85_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad85_sub_tab 
quad_lng     <- quad85_lng

## QUAD 86
quad86_sub <- subset(sub_dominant, quad_no == "86")
quad86_sub_mat <- as.matrix(quad86_sub[,-1])
quad86_sub_tab <- table(quad86_sub_mat[,-1]) # gives count of each species in the quadrat
quad86_sub_tab
quad86_lng <- quad86_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad86_lng)
quad86_lng <- mutate(quad86_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad86_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad86_sub_tab 
quad_lng     <- quad86_lng

## QUAD 87
quad87_sub <- subset(sub_dominant, quad_no == "87")
quad87_sub_mat <- as.matrix(quad87_sub[,-1])
quad87_sub_tab <- table(quad87_sub_mat[,-1]) # gives count of each species in the quadrat
quad87_sub_tab
quad87_lng <- quad87_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad87_lng)
quad87_lng <- mutate(quad87_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad87_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad87_sub_tab <- quad87_sub_tab 
quad_lng     <- quad87_lng

## QUAD 88
quad88_sub <- subset(sub_dominant, quad_no == "88")
quad88_sub_mat <- as.matrix(quad88_sub[,-1])
quad88_sub_tab <- table(quad88_sub_mat[,-1]) # gives count of each species in the quadrat
quad88_sub_tab
quad88_lng <- quad88_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad88_lng)
quad88_lng <- mutate(quad88_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad88_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad88_sub_tab 
quad_lng     <- quad88_lng

## QUAD 89
quad89_sub <- subset(sub_dominant, quad_no == "89")
quad89_sub_mat <- as.matrix(quad89_sub[,-1])
quad89_sub_tab <- table(quad89_sub_mat[,-1]) # gives count of each species in the quadrat
quad89_sub_tab
quad89_lng <- quad89_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad89_lng)
quad89_lng <- mutate(quad89_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad89_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad89_sub_tab 
quad_lng     <- quad89_lng

## QUAD 90
quad90_sub <- subset(sub_dominant, quad_no == "90")
quad90_sub_mat <- as.matrix(quad90_sub[,-1])
quad90_sub_tab <- table(quad90_sub_mat[,-1]) # gives count of each species in the quadrat
quad90_sub_tab
quad90_lng <- quad90_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad90_lng)
quad90_lng <- mutate(quad90_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad90_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad90_sub_tab 
quad_lng     <- quad90_lng


## QUAD 91
quad91_sub<- subset(sub_dominant, quad_no == "91")
quad91_sub_mat <- as.matrix(quad91_sub[,-1])
quad91_sub_tab <- table(quad91_sub_mat[,-1]) # gives count of each species in the quadrat
quad91_sub_tab
quad91_lng <- quad91_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad91_lng)
quad91_lng <- mutate(quad91_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad91_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad91_sub_tab 
quad_lng     <- quad91_lng

## QUAD 92
quad92_sub<- subset(sub_dominant, quad_no == "92")
quad92_sub_mat <- as.matrix(quad92_sub[,-1])
quad92_sub_tab <- table(quad92_sub_mat[,-1]) # gives count of each species in the quadrat
quad92_sub_tab
quad92_lng <- quad92_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad92_lng)
quad92_lng <- mutate(quad92_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad92_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad92_sub_tab 
quad_lng     <- quad92_lng

## QUAD 93
quad93_sub<- subset(sub_dominant, quad_no == "93")
quad93_sub_mat <- as.matrix(quad93_sub[,-1])
quad93_sub_tab <- table(quad93_sub_mat[,-1]) # gives count of each species in the quadrat
quad93_sub_tab
quad93_lng <- quad93_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad93_lng)
quad93_lng <- mutate(quad93_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad93_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad93_sub_tab 
quad_lng     <- quad93_lng

## QUAD 94
quad94_sub <- subset(sub_dominant, quad_no == "94")
quad94_sub_mat <- as.matrix(quad94_sub[,-1])
quad94_sub_tab <- table(quad94_sub_mat[,-1]) # gives count of each species in the quadrat
quad94_sub_tab
quad94_lng <- quad94_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad94_lng)
quad94_lng <- mutate(quad94_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad94_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad94_sub_tab 
quad_lng     <- quad94_lng

## QUAD 95
quad95_sub <- subset(sub_dominant, quad_no == "95")
quad95_sub_mat <- as.matrix(quad95_sub[,-1])
quad95_sub_tab <- table(quad95_sub_mat[,-1]) # gives count of each species in the quadrat
quad95_sub_tab
quad95_lng <- quad95_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad95_lng)
quad95_lng <- mutate(quad95_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad95_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad95_sub_tab 
quad_lng     <- quad95_lng

## QUAD 96
quad96_sub <- subset(sub_dominant, quad_no == "96")
quad96_sub_mat <- as.matrix(quad96_sub[,-1])
quad96_sub_tab <- table(quad96_sub_mat[,-1]) # gives count of each species in the quadrat
quad96_sub_tab
quad96_lng <- quad96_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad96_lng)
quad96_lng <- mutate(quad96_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad96_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad96_sub_tab 
quad_lng     <- quad96_lng

## QUAD 97
quad97_sub <- subset(sub_dominant, quad_no == "97")
quad97_sub_mat <- as.matrix(quad97_sub[,-1])
quad97_sub_tab <- table(quad97_sub_mat[,-1]) # gives count of each species in the quadrat
quad97_sub_tab
quad97_lng <- quad97_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad97_lng)
quad97_lng <- mutate(quad97_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad97_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad97_sub_tab 
quad_lng     <- quad97_lng

## QUAD 98
quad98_sub <- subset(sub_dominant, quad_no == "98")
quad98_sub_mat <- as.matrix(quad98_sub[,-1])
quad98_sub_tab <- table(quad98_sub_mat[,-1]) # gives count of each species in the quadrat
quad98_sub_tab
quad98_lng <- quad98_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad98_lng)
quad98_lng <- mutate(quad98_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad98_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad98_sub_tab 
quad_lng     <- quad98_lng

## QUAD 99
quad99_sub <- subset(sub_dominant, quad_no == "99")
quad99_sub_mat <- as.matrix(quad99_sub[,-1])
quad99_sub_tab <- table(quad99_sub_mat[,-1]) # gives count of each species in the quadrat
quad99_sub_tab
quad99_lng <- quad99_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad99_lng)
quad99_lng <- mutate(quad99_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad99_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                            "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad99_sub_tab 
quad_lng     <- quad99_lng

## QUAD 100
quad100_sub <- subset(sub_dominant, quad_no == "100")
quad100_sub_mat <- as.matrix(quad100_sub[,-1])
quad100_sub_tab <- table(quad100_sub_mat[,-1]) # gives count of each species in the quadrat
quad100_sub_tab
quad100_lng <- quad100_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad100_lng)
quad100_lng <- mutate(quad100_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad100_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad100_sub_tab 
quad_lng     <- quad100_lng

## QUAD 101
quad101_sub <- subset(sub_dominant, quad_no == "101")
quad101_sub_mat <- as.matrix(quad101_sub[,-1])
quad101_sub_tab <- table(quad101_sub_mat[,-1]) # gives count of each species in the quadrat
quad101_sub_tab
quad101_lng <- quad101_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad101_lng)
quad101_lng <- mutate(quad101_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad101_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad101_sub_tab 
quad_lng     <- quad101_lng


## QUAD 102
quad102_sub <- subset(sub_dominant, quad_no == "102")
quad102_sub_mat <- as.matrix(quad102_sub[,-1])
quad102_sub_tab <- table(quad102_sub_mat[,-1]) # gives count of each species in the quadrat
quad102_sub_tab
quad102_lng <- quad102_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad102_lng)
quad102_lng <- mutate(quad102_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad102_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad102_sub_tab 
quad_lng     <- quad102_lng

## QUAD 103
quad103_sub <- subset(sub_dominant, quad_no == "103")
quad103_sub_mat <- as.matrix(quad103_sub[,-1])
quad103_sub_tab <- table(quad103_sub_mat[,-1]) # gives count of each species in the quadrat
quad103_sub_tab
quad103_lng <- quad103_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad103_lng)
quad103_lng <- mutate(quad103_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad103_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad103_sub_tab 
quad_lng     <- quad103_lng

## QUAD 104
quad104_sub <- subset(sub_dominant, quad_no == "104")
quad104_sub_mat <- as.matrix(quad104_sub[,-1])
quad104_sub_tab <- table(quad104_sub_mat[,-1]) # gives count of each species in the quadrat
quad104_sub_tab
quad104_lng <- quad104_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad104_lng)
quad104_lng <- mutate(quad104_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad104_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad104_sub_tab 
quad_lng     <- quad104_lng

## QUAD 105
quad105_sub <- subset(sub_dominant, quad_no == "105")
quad105_sub_mat <- as.matrix(quad105_sub[,-1])
quad105_sub_tab <- table(quad105_sub_mat[,-1]) # gives count of each species in the quadrat
quad105_sub_tab
quad105_lng <- quad105_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad105_lng)
quad105_lng <- mutate(quad105_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad105_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad105_sub_tab 
quad_lng     <- quad105_lng

## QUAD 106
quad106_sub <- subset(sub_dominant, quad_no == "106")
quad106_sub_mat <- as.matrix(quad106_sub[,-1])
quad106_sub_tab <- table(quad106_sub_mat[,-1]) # gives count of each species in the quadrat
quad106_sub_tab
quad106_lng <- quad106_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad106_lng)
quad106_lng <- mutate(quad106_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad106_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad106_sub_tab 
quad_lng     <- quad106_lng

## QUAD 107
quad107_sub <- subset(sub_dominant, quad_no == "107")
quad107_sub_mat <- as.matrix(quad107_sub[,-1])
quad107_sub_tab <- table(quad107_sub_mat[,-1]) # gives count of each species in the quadrat
quad107_sub_tab
quad107_lng <- quad107_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad107_lng)
quad107_lng <- mutate(quad107_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad107_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad107_sub_tab 
quad_lng     <- quad107_lng

## QUAD 108
quad108_sub <- subset(sub_dominant, quad_no == "108")
quad108_sub_mat <- as.matrix(quad108_sub[,-1])
quad108_sub_tab <- table(quad108_sub_mat[,-1]) # gives count of each species in the quadrat
quad108_sub_tab
quad108_lng <- quad108_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad108_lng)
quad108_lng <- mutate(quad108_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad108_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad108_sub_tab 
quad_lng     <- quad108_lng

## QUAD 109
quad109_sub <- subset(sub_dominant, quad_no == "109")
quad109_sub_mat <- as.matrix(quad109_sub[,-1])
quad109_sub_tab <- table(quad109_sub_mat[,-1]) # gives count of each species in the quadrat
quad109_sub_tab
quad109_lng <- quad109_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad109_lng)
quad109_lng <- mutate(quad109_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad109_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad109_sub_tab 
quad_lng     <- quad109_lng

## QUAD 110
quad110_sub <- subset(sub_dominant, quad_no == "110")
quad110_sub_mat <- as.matrix(quad110_sub[,-1])
quad110_sub_tab <- table(quad110_sub_mat[,-1]) # gives count of each species in the quadrat
quad110_sub_tab
quad110_lng <- quad110_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad110_lng)
quad110_lng <- mutate(quad110_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad110_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad110_sub_tab 
quad_lng     <- quad110_lng

## QUAD 111
quad111_sub <- subset(sub_dominant, quad_no == "111")
quad111_sub_mat <- as.matrix(quad111_sub[,-1])
quad111_sub_tab <- table(quad112_sub_mat[,-1]) # gives count of each species in the quadrat
quad111_sub_tab
quad111_lng <- quad111_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad111_lng)
quad111_lng <- mutate(quad111_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad111_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad1_sub_tab 
quad_lng     <- quad1_lng

## QUAD 112
quad112_sub <- subset(sub_dominant, quad_no == "112")
quad112_sub_mat <- as.matrix(quad112_sub[,-1])
quad112_sub_tab <- table(quad112_sub_mat[,-1]) # gives count of each species in the quadrat
quad112_sub_tab
quad112_lng <- quad112_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad112_lng)
quad112_lng <- mutate(quad112_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad112_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad112_sub_tab 
quad_lng     <- quad112_lng

## QUAD 113
quad113_sub <- subset(sub_dominant, quad_no == "113")
quad113_sub_mat <- as.matrix(quad113_sub[,-1])
quad113_sub_tab <- table(quad113_sub_mat[,-1]) # gives count of each species in the quadrat
quad113_sub_tab
quad113_lng <- quad113_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad113_lng)
quad113_lng <- mutate(quad113_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad113_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad113_sub_tab 
quad_lng     <- quad113_lng

## QUAD 114
quad114_sub <- subset(sub_dominant, quad_no == "114")
quad114_sub_mat <- as.matrix(quad114_sub[,-1])
quad114_sub_tab <- table(quad114_sub_mat[,-1]) # gives count of each species in the quadrat
quad114_sub_tab
quad114_lng <- quad114_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad114_lng)
quad114_lng <- mutate(quad114_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad114_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad114_sub_tab 
quad_lng     <- quad114_lng

## QUAD 115
quad115_sub <- subset(sub_dominant, quad_no == "115")
quad115_sub_mat <- as.matrix(quad115_sub[,-1])
quad115_sub_tab <- table(quad115_sub_mat[,-1]) # gives count of each species in the quadrat
quad115_sub_tab
quad115_lng <- quad115_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad115_lng)
quad115_lng <- mutate(quad115_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad115_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad115_sub_tab 
quad_lng     <- quad115_lng

## QUAD 116
quad116_sub <- subset(sub_dominant, quad_no == "116")
quad116_sub_mat <- as.matrix(quad116_sub[,-1])
quad116_sub_tab <- table(quad116_sub_mat[,-1]) # gives count of each species in the quadrat
quad116_sub_tab
quad116_lng <- quad116_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad116_lng)
quad116_lng <- mutate(quad116_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad116_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad116_sub_tab 
quad_lng     <- quad116_lng

## QUAD 117
quad117_sub <- subset(sub_dominant, quad_no == "117")
quad117_sub_mat <- as.matrix(quad117_sub[,-1])
quad117_sub_tab <- table(quad117_sub_mat[,-1]) # gives count of each species in the quadrat
quad117_sub_tab
quad117_lng <- quad117_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad117_lng)
quad117_lng <- mutate(quad117_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad117_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad117_sub_tab 
quad_lng     <- quad117_lng

## QUAD 118
quad118_sub <- subset(sub_dominant, quad_no == "118")
quad118_sub_mat <- as.matrix(quad118_sub[,-1])
quad118_sub_tab <- table(quad118_sub_mat[,-1]) # gives count of each species in the quadrat
quad118_sub_tab
quad118_lng <- quad118_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad118_lng)
quad118_lng <- mutate(quad118_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad118_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad118_sub_tab 
quad_lng     <- quad118_lng

## QUAD 119
quad119_sub <- subset(sub_dominant, quad_no == "119")
quad119_sub_mat <- as.matrix(quad119_sub[,-1])
quad119_sub_tab <- table(quad119_sub_mat[,-1]) # gives count of each species in the quadrat
quad119_sub_tab
quad119_lng <- quad119_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad119_lng)
quad119_lng <- mutate(quad119_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad119_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad119_sub_tab 
quad_lng     <- quad119_lng

## QUAD 120
quad120_sub <- subset(sub_dominant, quad_no == "120")
quad120_sub_mat <- as.matrix(quad120_sub[,-1])
quad120_sub_tab <- table(quad120_sub_mat[,-1]) # gives count of each species in the quadrat
quad120_sub_tab
quad120_lng <- quad120_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad120_lng)
quad120_lng <- mutate(quad120_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad120_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad120_sub_tab 
quad_lng     <- quad120_lng

## QUAD 121
quad121_sub <- subset(sub_dominant, quad_no == "121")
quad121_sub_mat <- as.matrix(quad121_sub[,-1])
quad121_sub_tab <- table(quad121_sub_mat[,-1]) # gives count of each species in the quadrat
quad121_sub_tab
quad121_lng <- quad121_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad121_lng)
quad121_lng <- mutate(quad121_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad121_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad121_sub_tab 
quad_lng     <- quad121_lng

## QUAD 122
quad122_sub <- subset(sub_dominant, quad_no == "122")
quad122_sub_mat <- as.matrix(quad122_sub[,-1])
quad122_sub_tab <- table(quad122_sub_mat[,-1]) # gives count of each species in the quadrat
quad122_sub_tab
quad122_lng <- quad122_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad122_lng)
quad122_lng <- mutate(quad122_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad122_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad122_sub_tab 
quad_lng     <- quad122_lng

## QUAD 123
quad123_sub <- subset(sub_dominant, quad_no == "123")
quad123_sub_mat <- as.matrix(quad123_sub[,-1])
quad123_sub_tab <- table(quad123_sub_mat[,-1]) # gives count of each species in the quadrat
quad123_sub_tab
quad123_lng <- quad123_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad123_lng)
quad123_lng <- mutate(quad123_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad123_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad123_sub_tab 
quad_lng     <- quad123_lng

## QUAD 124
quad124_sub <- subset(sub_dominant, quad_no == "124")
quad124_sub_mat <- as.matrix(quad124_sub[,-1])
quad124_sub_tab <- table(quad124_sub_mat[,-1]) # gives count of each species in the quadrat
quad124_sub_tab
quad124_lng <- quad124_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad124_lng)
quad124_lng <- mutate(quad124_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad124_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad124_sub_tab 
quad_lng     <- quad124_lng

## QUAD 125
quad125_sub <- subset(sub_dominant, quad_no == "125")
quad125_sub_mat <- as.matrix(quad125_sub[,-1])
quad125_sub_tab <- table(quad125_sub_mat[,-1]) # gives count of each species in the quadrat
quad125_sub_tab
quad125_lng <- quad125_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad125_lng)
quad125_lng <- mutate(quad125_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad125_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad125_sub_tab 
quad_lng     <- quad125_lng

## QUAD 126
quad126_sub <- subset(sub_dominant, quad_no == "126")
quad126_sub_mat <- as.matrix(quad126_sub[,-1])
quad126_sub_tab <- table(quad126_sub_mat[,-1]) # gives count of each species in the quadrat
quad126_sub_tab
quad126_lng <- quad126_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad126_lng)
quad126_lng <- mutate(quad126_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad126_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad126_sub_tab 
quad_lng     <- quad126_lng

## QUAD 127
quad127_sub <- subset(sub_dominant, quad_no == "127")
quad127_sub_mat <- as.matrix(quad127_sub[,-1])
quad127_sub_tab <- table(quad127_sub_mat[,-1]) # gives count of each species in the quadrat
quad127_sub_tab
quad127_lng <- quad127_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad127_lng)
quad127_lng <- mutate(quad127_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad127_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad127_sub_tab 
quad_lng     <- quad127_lng

## QUAD 128
quad128_sub <- subset(sub_dominant, quad_no == "128")
quad128_sub_mat <- as.matrix(quad128_sub[,-1])
quad128_sub_tab <- table(quad128_sub_mat[,-1]) # gives count of each species in the quadrat
quad128_sub_tab
quad128_lng <- quad128_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad128_lng)
quad128_lng <- mutate(quad128_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad128_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad128_sub_tab 
quad_lng     <- quad128_lng

## QUAD 129
quad129_sub <- subset(sub_dominant, quad_no == "129")
quad129_sub_mat <- as.matrix(quad129_sub[,-1])
quad129_sub_tab <- table(quad129_sub_mat[,-1]) # gives count of each species in the quadrat
quad129_sub_tab
quad129_lng <- quad129_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad129_lng)
quad129_lng <- mutate(quad129_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad129_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad129_sub_tab 
quad_lng     <- quad129_lng

## QUAD 130
quad130_sub <- subset(sub_dominant, quad_no == "130")
quad130_sub_mat <- as.matrix(quad130_sub[,-1])
quad130_sub_tab <- table(quad130_sub_mat[,-1]) # gives count of each species in the quadrat
quad130_sub_tab
quad130_lng <- quad130_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad130_lng)
quad130_lng <- mutate(quad130_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad130_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad130_sub_tab 
quad_lng     <- quad130_lng

## QUAD 131
quad131_sub <- subset(sub_dominant, quad_no == "131")
quad131_sub_mat <- as.matrix(quad131_sub[,-1])
quad131_sub_tab <- table(quad131_sub_mat[,-1]) # gives count of each species in the quadrat
quad131_sub_tab
quad131_lng <- quad131_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad131_lng)
quad131_lng <- mutate(quad131_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad131_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad131_sub_tab 
quad_lng     <- quad131_lng

## QUAD 132
quad132_sub <- subset(sub_dominant, quad_no == "132")
quad132_sub_mat <- as.matrix(quad132_sub[,-1])
quad132_sub_tab <- table(quad132_sub_mat[,-1]) # gives count of each species in the quadrat
quad132_sub_tab
quad132_lng <- quad132_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad132_lng)
quad132_lng <- mutate(quad132_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad132_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad132_sub_tab 
quad_lng     <- quad132_lng

## QUAD 133
quad133_sub <- subset(sub_dominant, quad_no == "133")
quad133_sub_mat <- as.matrix(quad133_sub[,-1])
quad133_sub_tab <- table(quad133_sub_mat[,-1]) # gives count of each species in the quadrat
quad133_sub_tab
quad133_lng <- quad133_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad133_lng)
quad133_lng <- mutate(quad133_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad133_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad133_sub_tab 
quad_lng     <- quad133_lng

## QUAD 134
quad134_sub <- subset(sub_dominant, quad_no == "134")
quad134_sub_mat <- as.matrix(quad134_sub[,-1])
quad134_sub_tab <- table(quad134_sub_mat[,-1]) # gives count of each species in the quadrat
quad134_sub_tab
quad134_lng <- quad134_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad134_lng)
quad134_lng <- mutate(quad134_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad134_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad134_sub_tab 
quad_lng     <- quad134_lng

## QUAD 135
quad135_sub <- subset(sub_dominant, quad_no == "135")
quad135_sub_mat <- as.matrix(quad135_sub[,-1])
quad135_sub_tab <- table(quad135_sub_mat[,-1]) # gives count of each species in the quadrat
quad135_sub_tab
quad135_lng <- quad135_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad135_lng)
quad135_lng <- mutate(quad135_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad135_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad135_sub_tab 
quad_lng     <- quad135_lng

## QUAD 136
quad136_sub <- subset(sub_dominant, quad_no == "136")
quad136_sub_mat <- as.matrix(quad136_sub[,-1])
quad136_sub_tab <- table(quad136_sub_mat[,-1]) # gives count of each species in the quadrat
quad136_sub_tab
quad136_lng <- quad136_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad136_lng)
quad136_lng <- mutate(quad136_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad136_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad136_sub_tab 
quad_lng     <- quad136_lng

## QUAD 137
quad137_sub <- subset(sub_dominant, quad_no == "137")
quad137_sub_mat <- as.matrix(quad137_sub[,-1])
quad137_sub_tab <- table(quad137_sub_mat[,-1]) # gives count of each species in the quadrat
quad137_sub_tab
quad137_lng <- quad137_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad137_lng)
quad137_lng <- mutate(quad137_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad137_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad137_sub_tab 
quad_lng     <- quad137_lng

## QUAD 138
quad138_sub <- subset(sub_dominant, quad_no == "138")
quad138_sub_mat <- as.matrix(quad138_sub[,-1])
quad138_sub_tab <- table(quad138_sub_mat[,-1]) # gives count of each species in the quadrat
quad138_sub_tab
quad138_lng <- quad138_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad138_lng)
quad138_lng <- mutate(quad138_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad138_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad138_sub_tab 
quad_lng     <- quad138_lng

## QUAD 139
quad139_sub <- subset(sub_dominant, quad_no == "139")
quad139_sub_mat <- as.matrix(quad139_sub[,-1])
quad139_sub_tab <- table(quad139_sub_mat[,-1]) # gives count of each species in the quadrat
quad139_sub_tab
quad139_lng <- quad139_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad139_lng)
quad139_lng <- mutate(quad139_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad139_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad139_sub_tab 
quad_lng     <- quad139_lng

## QUAD 140
quad140_sub <- subset(sub_dominant, quad_no == "140")
quad140_sub_mat <- as.matrix(quad140_sub[,-1])
quad140_sub_tab <- table(quad140_sub_mat[,-1]) # gives count of each species in the quadrat
quad140_sub_tab
quad140_lng <- quad140_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad140_lng)
quad140_lng <- mutate(quad140_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad140_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad140_sub_tab 
quad_lng     <- quad140_lng

## QUAD 141
quad141_sub <- subset(sub_dominant, quad_no == "140")
quad141_sub_mat <- as.matrix(quad141_sub[,-1])
quad141_sub_tab <- table(quad141_sub_mat[,-1]) # gives count of each species in the quadrat
quad141_sub_tab
quad141_lng <- quad141_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad141_lng)
quad141_lng <- mutate(quad141_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad141_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad141_sub_tab 
quad_lng     <- quad141_lng

## QUAD 142
quad142_sub <- subset(sub_dominant, quad_no == "142")
quad142_sub_mat <- as.matrix(quad142_sub[,-1])
quad142_sub_tab <- table(quad142_sub_mat[,-1]) # gives count of each species in the quadrat
quad142_sub_tab
quad142_lng <- quad142_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad142_lng)
quad142_lng <- mutate(quad142_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad142_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad142_sub_tab 
quad_lng     <- quad142_lng

## QUAD 143
quad143_sub <- subset(sub_dominant, quad_no == "143")
quad143_sub_mat <- as.matrix(quad143_sub[,-1])
quad143_sub_tab <- table(quad143_sub_mat[,-1]) # gives count of each species in the quadrat
quad143_sub_tab
quad143_lng <- quad143_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad143_lng)
quad143_lng <- mutate(quad143_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad143_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad143_sub_tab 
quad_lng     <- quad143_lng

## QUAD 144
quad144_sub <- subset(sub_dominant, quad_no == "144")
quad144_sub_mat <- as.matrix(quad144_sub[,-1])
quad144_sub_tab <- table(quad144_sub_mat[,-1]) # gives count of each species in the quadrat
quad144_sub_tab
quad144_lng <- quad144_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad144_lng)
quad144_lng <- mutate(quad144_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad144_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad144_sub_tab 
quad_lng     <- quad144_lng

## QUAD 145
quad145_sub <- subset(sub_dominant, quad_no == "145")
quad145_sub_mat <- as.matrix(quad145_sub[,-1])
quad145_sub_tab <- table(quad145_sub_mat[,-1]) # gives count of each species in the quadrat
quad145_sub_tab
quad145_lng <- quad145_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad145_lng)
quad145_lng <- mutate(quad145_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad145_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad145_sub_tab 
quad_lng     <- quad145_lng

## QUAD 146
quad146_sub <- subset(sub_dominant, quad_no == "146")
quad146_sub_mat <- as.matrix(quad146_sub[,-1])
quad146_sub_tab <- table(quad146_sub_mat[,-1]) # gives count of each species in the quadrat
quad146_sub_tab
quad146_lng <- quad146_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad146_lng)
quad146_lng <- mutate(quad146_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad146_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad146_sub_tab 
quad_lng     <- quad146_lng

## QUAD 147
quad147_sub <- subset(sub_dominant, quad_no == "147")
quad147_sub_mat <- as.matrix(quad147_sub[,-1])
quad147_sub_tab <- table(quad147_sub_mat[,-1]) # gives count of each species in the quadrat
quad147_sub_tab
quad147_lng <- quad147_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad147_lng)
quad147_lng <- mutate(quad147_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad147_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad147_sub_tab <- quad147_sub_tab 
quad_lng     <- quad147_lng

## QUAD 148
quad148_sub <- subset(sub_dominant, quad_no == "148")
quad148_sub_mat <- as.matrix(quad148_sub[,-1])
quad148_sub_tab <- table(quad148_sub_mat[,-1]) # gives count of each species in the quadrat
quad148_sub_tab
quad148_lng <- quad148_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad148_lng)
quad148_lng <- mutate(quad148_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad148_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad148_sub_tab 
quad_lng     <- quad148_lng

## QUAD 149
quad149_sub <- subset(sub_dominant, quad_no == "149")
quad149_sub_mat <- as.matrix(quad149_sub[,-1])
quad149_sub_tab <- table(quad149_sub_mat[,-1]) # gives count of each species in the quadrat
quad149_sub_tab
quad149_lng <- quad149_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad149_lng)
quad149_lng <- mutate(quad149_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad149_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad149_sub_tab 
quad_lng     <- quad149_lng

## QUAD 150
quad150_sub <- subset(sub_dominant, quad_no == "150")
quad150_sub_mat <- as.matrix(quad150_sub[,-1])
quad150_sub_tab <- table(quad150_sub_mat[,-1]) # gives count of each species in the quadrat
quad150_sub_tab
quad150_lng <- quad150_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad150_lng)
quad150_lng <- mutate(quad150_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad150_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad150_sub_tab 
quad_lng     <- quad150_lng

## QUAD 151
quad151_sub <- subset(sub_dominant, quad_no == "151")
quad151_sub_mat <- as.matrix(quad151_sub[,-1])
quad151_sub_tab <- table(quad151_sub_mat[,-1]) # gives count of each species in the quadrat
quad151_sub_tab
quad151_lng <- quad151_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad151_lng)
quad151_lng <- mutate(quad151_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad151_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad151_sub_tab 
quad_lng     <- quad151_lng

## QUAD 152
quad152_sub <- subset(sub_dominant, quad_no == "152")
quad152_sub_mat <- as.matrix(quad152_sub[,-1])
quad152_sub_tab <- table(quad152_sub_mat[,-1]) # gives count of each species in the quadrat
quad152_sub_tab
quad152_lng <- quad152_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad152_lng)
quad152_lng <- mutate(quad152_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad152_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad152_sub_tab 
quad_lng     <- quad152_lng

## QUAD 153
quad153_sub <- subset(sub_dominant, quad_no == "153")
quad153_sub_mat <- as.matrix(quad153_sub[,-1])
quad153_sub_tab <- table(quad153_sub_mat[,-1]) # gives count of each species in the quadrat
quad153_sub_tab
quad153_lng <- quad153_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad153_lng)
quad153_lng <- mutate(quad153_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad153_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad153_sub_tab 
quad_lng     <- quad153_lng

## QUAD 154
quad154_sub <- subset(sub_dominant, quad_no == "154")
quad154_sub_mat <- as.matrix(quad154_sub[,-1])
quad154_sub_tab <- table(quad154_sub_mat[,-1]) # gives count of each species in the quadrat
quad154_sub_tab
quad154_lng <- quad154_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad154_lng)
quad154_lng <- mutate(quad154_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad154_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad154_sub_tab 
quad_lng     <- quad154_lng

## QUAD 155
quad155_sub <- subset(sub_dominant, quad_no == "155")
quad155_sub_mat <- as.matrix(quad155_sub[,-1])
quad155_sub_tab <- table(quad155_sub_mat[,-1]) # gives count of each species in the quadrat
quad155_sub_tab
quad155_lng <- quad155_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad155_lng)
quad155_lng <- mutate(quad155_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad155_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad155_sub_tab 
quad_lng     <- quad155_lng

## QUAD 156
quad156_sub <- subset(sub_dominant, quad_no == "156")
quad156_sub_mat <- as.matrix(quad156_sub[,-1])
quad156_sub_tab <- table(quad156_sub_mat[,-1]) # gives count of each species in the quadrat
quad156_sub_tab
quad156_lng <- quad156_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad156_lng)
quad156_lng <- mutate(quad156_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad156_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad156_sub_tab 
quad_lng     <- quad156_lng

## QUAD 157
quad157_sub <- subset(sub_dominant, quad_no == "157")
quad157_sub_mat <- as.matrix(quad157_sub[,-1])
quad157_sub_tab <- table(quad157_sub_mat[,-1]) # gives count of each species in the quadrat
quad157_sub_tab
quad157_lng <- quad157_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad157_lng)
quad157_lng <- mutate(quad157_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad157_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad157_sub_tab 
quad_lng     <- quad157_lng

## QUAD 158
quad158_sub <- subset(sub_dominant, quad_no == "158")
quad158_sub_mat <- as.matrix(quad158_sub[,-1])
quad158_sub_tab <- table(quad158_sub_mat[,-1]) # gives count of each species in the quadrat
quad158_sub_tab
quad158_lng <- quad158_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad158_lng)
quad158_lng <- mutate(quad158_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad158_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad158_sub_tab 
quad_lng     <- quad158_lng

## QUAD 159
quad159_sub <- subset(sub_dominant, quad_no == "159")
quad159_sub_mat <- as.matrix(quad159_sub[,-1])
quad159_sub_tab <- table(quad159_sub_mat[,-1]) # gives count of each species in the quadrat
quad159_sub_tab
quad159_lng <- quad159_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad159_lng)
quad159_lng <- mutate(quad159_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad159_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad159_sub_tab 
quad_lng     <- quad159_lng

## QUAD 160
quad160_sub <- subset(sub_dominant, quad_no == "160")
quad160_sub_mat <- as.matrix(quad160_sub[,-1])
quad160_sub_tab <- table(quad160_sub_mat[,-1]) # gives count of each species in the quadrat
quad160_sub_tab
quad160_lng <- quad160_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad160_lng)
quad160_lng <- mutate(quad160_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad160_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad160_sub_tab 
quad_lng     <- quad160_lng

## QUAD 161
quad161_sub <- subset(sub_dominant, quad_no == "1")
quad161_sub_mat <- as.matrix(quad161_sub[,-1])
quad161_sub_tab <- table(quad161_sub_mat[,-1]) # gives count of each species in the quadrat
quad161_sub_tab
quad161_lng <- quad161_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad161_lng)
quad161_lng <- mutate(quad161_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad161_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad161_sub_tab 
quad_lng     <- quad161_lng

## QUAD 162
quad162_sub <- subset(sub_dominant, quad_no == "162")
quad162_sub_mat <- as.matrix(quad162_sub[,-1])
quad162_sub_tab <- table(quad162_sub_mat[,-1]) # gives count of each species in the quadrat
quad162_sub_tab
quad162_lng <- quad162_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad162_lng)
quad162_lng <- mutate(quad162_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad162_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad162_sub_tab 
quad_lng     <- quad162_lng

## QUAD 163
quad163_sub <- subset(sub_dominant, quad_no == "163")
quad163_sub_mat <- as.matrix(quad163_sub[,-1])
quad163_sub_tab <- table(quad163_sub_mat[,-1]) # gives count of each species in the quadrat
quad163_sub_tab
quad163_lng <- quad163_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad163_lng)
quad163_lng <- mutate(quad163_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad163_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad163_sub_tab 
quad_lng     <- quad163_lng

## QUAD 164
quad164_sub <- subset(sub_dominant, quad_no == "164")
quad164_sub_mat <- as.matrix(quad164_sub[,-1])
quad164_sub_tab <- table(quad164_sub_mat[,-1]) # gives count of each species in the quadrat
quad164_sub_tab
quad164_lng <- quad164_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad164_lng)
quad164_lng <- mutate(quad164_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad164_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad164_sub_tab 
quad_lng     <- quad164_lng

## QUAD 165
quad165_sub <- subset(sub_dominant, quad_no == "165")
quad165_sub_mat <- as.matrix(quad165_sub[,-1])
quad165_sub_tab <- table(quad165_sub_mat[,-1]) # gives count of each species in the quadrat
quad165_sub_tab
quad165_lng <- quad165_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad165_lng)
quad165_lng <- mutate(quad165_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad165_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad165_sub_tab 
quad_lng     <- quad165_lng

## QUAD 166
quad166_sub <- subset(sub_dominant, quad_no == "166")
quad166_sub_mat <- as.matrix(quad166_sub[,-1])
quad166_sub_tab <- table(quad166_sub_mat[,-1]) # gives count of each species in the quadrat
quad166_sub_tab
quad166_lng <- quad166_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad166_lng)
quad166_lng <- mutate(quad166_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad166_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad166_sub_tab 
quad_lng     <- quad166_lng

## QUAD 167
quad167_sub <- subset(sub_dominant, quad_no == "167")
quad167_sub_mat <- as.matrix(quad167_sub[,-1])
quad167_sub_tab <- table(quad167_sub_mat[,-1]) # gives count of each species in the quadrat
quad167_sub_tab
quad167_lng <- quad167_sub %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad167_lng)
quad167_lng <- mutate(quad167_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad167_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                             "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_sub_tab <- quad167_sub_tab 
quad_lng     <- quad167_lng

################################################################################### 
##                       RUN BEFORE LOOP, DEPENDING ON QUADRART NUMBER           ##
###################################################################################

quad_sub_tab <- quad22_sub_tab ## number to be replaced by quadrat number required before running loop 
quad_lng     <- quad22_lng

################################################################################### 
##                         NEEDED - BEGINNING OF LOOP                            ##
###################################################################################

## CURRENT LOOP READS ALL OF THE FIRST 5 QUADRATS TOGETHER AND SUMMARISES PATCHES ACROSS ALL 5 QUADRATS RATHER THAN
## EACH QUADRAT ON ITS OWN

#for (quad_no in 1:167) {
quad_sub_tab <- quad32_sub_tab ## change according to quadrat number needed for patch stats
quad_lng     <- quad32_lng

spp_in_quad  <- names(quad_sub_tab)
spat_all     <- NULL
for (spp_name in seq_along(spp_in_quad)){
  quad_no <- 32
  spp_text <- names(quad_sub_tab)[spp_name]
  quad_spp_lng <- quad_lng %>% filter(Species == spp_text)
  quad_spp_wde <- quad_spp_lng %>% reshape2::dcast(Row ~ Col, drop = FALSE, fill = 0)
  # Recode text to number
  quad_spp_wde[,-1] <- ifelse(quad_spp_wde[,-1]!="0",1,0)
  quad_spp_mat <- as.matrix(quad_spp_wde[,-1])
  
  quad_ccl = ConnCompLabel(quad_spp_mat)
  #print(spp_text)
  #print(quad_ccl)
  #image(t(quad_ccl[10:1,]), col=c('white',rainbow(length(unique(quad_ccl))-1)), main = print(spp_text)) #due to spaces matrix is 12 (y-axis)x10 (x-axis)
  #grid(nx = 10, ny = 10, col = "grey", lty = 1) # added post roy
  #print(PatchStat(quad_ccl))
  
  # Store the spatial stats
  # Need to duplicate spp text names in final output table
  spp_duplicate <- rep(spp_text, max(quad_ccl)+1)
  patch_stats <- PatchStat(quad_ccl)
  patch_stats <- cbind(quad_no, spp_duplicate, patch_stats)
  spat_all <- rbind(spat_all, patch_stats)
  #readline()
}
write.csv(spat_all, file = "Results/Patch_Stats/Individual_quadrats/Q32_SUB.csv")
# cbind in the quadrat no. into spat_all
#}

###########################################################

sub_abund <- read.csv("Results/Patch_Stats/Individual_quadrats/04-08-18_SUB.csv")
sub_abund <- sub_abund[,-1]
sub_abund <- subset(sub_abund, patchID > 0) # removes patch ID 0

area_sub_spp <- NULL

## Getting data from each quadrat separately
sub_quad1 <- subset(sub_abund, quad_no == "1")
total_sub_spp_1<- aggregate(sub_quad1$area, by = list(Species = sub_quad1$spp_duplicate), FUN = sum)
total_sub_spp_1 <- as.data.frame(total_sub_spp_1)
total_sub_spp_1 <- cbind(quad_no = 1, total_sub_spp_1)
rownames(total_sub_spp_1) <- total_sub_spp_1$Species
total_sub_spp_1 <- data.frame(t(total_sub_spp_1))
total_sub_spp_1 <- total_sub_spp_1[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_1)

sub_quad2 <- subset(sub_abund, quad_no == "2")
total_sub_spp_2<- aggregate(sub_quad2$area, by = list(Species = sub_quad2$spp_duplicate), FUN = sum)
total_sub_spp_2 <- as.data.frame(total_sub_spp_2)
total_sub_spp_2 <- cbind(quad_no = 2, total_sub_spp_2)
rownames(total_sub_spp_2) <- total_sub_spp_2$Species
total_sub_spp_2 <- data.frame(t(total_sub_spp_2))
total_sub_spp_2 <- total_sub_spp_2[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_2)

sub_quad3 <- subset(sub_abund, quad_no == "3")
total_sub_spp_3<- aggregate(sub_quad3$area, by = list(Species = sub_quad3$spp_duplicate), FUN = sum)
total_sub_spp_3 <- as.data.frame(total_sub_spp_3)
total_sub_spp_3 <- cbind(quad_no = 3, total_sub_spp_3)
rownames(total_sub_spp_3) <- total_sub_spp_3$Species
total_sub_spp_3 <- data.frame(t(total_sub_spp_3))
total_sub_spp_3 <- total_sub_spp_3[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_3)

sub_quad4 <- subset(sub_abund, quad_no == "4")
total_sub_spp_4<- aggregate(sub_quad4$area, by = list(Species = sub_quad4$spp_duplicate), FUN = sum)
total_sub_spp_4 <- as.data.frame(total_sub_spp_4)
total_sub_spp_4 <- cbind(quad_no = 4, total_sub_spp_4)
rownames(total_sub_spp_4) <- total_sub_spp_4$Species
total_sub_spp_4 <- data.frame(t(total_sub_spp_4))
total_sub_spp_4 <- total_sub_spp_4[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_4)

sub_quad5 <- subset(sub_abund, quad_no == "5")
total_sub_spp_5<- aggregate(sub_quad5$area, by = list(Species = sub_quad5$spp_duplicate), FUN = sum)
total_sub_spp_5 <- as.data.frame(total_sub_spp_5)
total_sub_spp_5 <- cbind(quad_no = 5, total_sub_spp_5)
rownames(total_sub_spp_5) <- total_sub_spp_5$Species
total_sub_spp_5 <- data.frame(t(total_sub_spp_5))
total_sub_spp_5 <- total_sub_spp_5[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_5)

sub_quad6 <- subset(sub_abund, quad_no == "6")
total_sub_spp_6<- aggregate(sub_quad6$area, by = list(Species = sub_quad6$spp_duplicate), FUN = sum)
total_sub_spp_6 <- as.data.frame(total_sub_spp_6)
total_sub_spp_6 <- cbind(quad_no = 6, total_sub_spp_6)
rownames(total_sub_spp_6) <- total_sub_spp_6$Species
total_sub_spp_6 <- data.frame(t(total_sub_spp_6))
total_sub_spp_6 <- total_sub_spp_6[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_6)

sub_quad7 <- subset(sub_abund, quad_no == "7")
total_sub_spp_7<- aggregate(sub_quad7$area, by = list(Species = sub_quad7$spp_duplicate), FUN = sum)
total_sub_spp_7 <- as.data.frame(total_sub_spp_7)
total_sub_spp_7 <- cbind(quad_no = 7, total_sub_spp_7)
rownames(total_sub_spp_7) <- total_sub_spp_7$Species
total_sub_spp_7 <- data.frame(t(total_sub_spp_7))
total_sub_spp_7 <- total_sub_spp_7[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_7)

sub_quad8 <- subset(sub_abund, quad_no == "8")
total_sub_spp_8<- aggregate(sub_quad8$area, by = list(Species = sub_quad8$spp_duplicate), FUN = sum)
total_sub_spp_8 <- as.data.frame(total_sub_spp_8)
total_sub_spp_8 <- cbind(quad_no = 8, total_sub_spp_8)
rownames(total_sub_spp_8) <- total_sub_spp_8$Species
total_sub_spp_8 <- data.frame(t(total_sub_spp_8))
total_sub_spp_8 <- total_sub_spp_8[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_8)

sub_quad9 <- subset(sub_abund, quad_no == "9")
total_sub_spp_9<- aggregate(sub_quad9$area, by = list(Species = sub_quad9$spp_duplicate), FUN = sum)
total_sub_spp_9 <- as.data.frame(total_sub_spp_9)
total_sub_spp_9 <- cbind(quad_no = 9, total_sub_spp_9)
rownames(total_sub_spp_9) <- total_sub_spp_9$Species
total_sub_spp_9 <- data.frame(t(total_sub_spp_9))
total_sub_spp_9 <- total_sub_spp_9[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_9)

sub_quad10 <- subset(sub_abund, quad_no == "10")
total_sub_spp_10<- aggregate(sub_quad10$area, by = list(Species = sub_quad10$spp_duplicate), FUN = sum)
total_sub_spp_10 <- as.data.frame(total_sub_spp_10)
total_sub_spp_10 <- cbind(quad_no = 10, total_sub_spp_10)
rownames(total_sub_spp_10) <- total_sub_spp_10$Species
total_sub_spp_10 <- data.frame(t(total_sub_spp_10))
total_sub_spp_10 <- total_sub_spp_10[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_10)

sub_quad11 <- subset(sub_abund, quad_no == "11")
total_sub_spp_11<- aggregate(sub_quad11$area, by = list(Species = sub_quad11$spp_duplicate), FUN = sum)
total_sub_spp_11 <- as.data.frame(total_sub_spp_11)
total_sub_spp_11 <- cbind(quad_no = 11, total_sub_spp_11)
rownames(total_sub_spp_11) <- total_sub_spp_11$Species
total_sub_spp_11 <- data.frame(t(total_sub_spp_11))
total_sub_spp_11 <- total_sub_spp_11[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_11)

sub_quad12 <- subset(sub_abund, quad_no == "12")
total_sub_spp_12<- aggregate(sub_quad12$area, by = list(Species = sub_quad12$spp_duplicate), FUN = sum)
total_sub_spp_12 <- as.data.frame(total_sub_spp_12)
total_sub_spp_12 <- cbind(quad_no = 12, total_sub_spp_12)
rownames(total_sub_spp_12) <- total_sub_spp_12$Species
total_sub_spp_12 <- data.frame(t(total_sub_spp_12))
total_sub_spp_12 <- total_sub_spp_12[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_12)

sub_quad13 <- subset(sub_abund, quad_no == "13")
total_sub_spp_13<- aggregate(sub_quad13$area, by = list(Species = sub_quad13$spp_duplicate), FUN = sum)
total_sub_spp_13 <- as.data.frame(total_sub_spp_13)
total_sub_spp_13 <- cbind(quad_no = 13, total_sub_spp_13)
rownames(total_sub_spp_13) <- total_sub_spp_13$Species
total_sub_spp_13 <- data.frame(t(total_sub_spp_13))
total_sub_spp_13 <- total_sub_spp_13[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_13)

sub_quad14 <- subset(sub_abund, quad_no == "14")
total_sub_spp_14<- aggregate(sub_quad14$area, by = list(Species = sub_quad14$spp_duplicate), FUN = sum)
total_sub_spp_14 <- as.data.frame(total_sub_spp_14)
total_sub_spp_14 <- cbind(quad_no = 14, total_sub_spp_14)
rownames(total_sub_spp_14) <- total_sub_spp_14$Species
total_sub_spp_14 <- data.frame(t(total_sub_spp_14))
total_sub_spp_14 <- total_sub_spp_14[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_14)

sub_quad15 <- subset(sub_abund, quad_no == "15")
total_sub_spp_15<- aggregate(sub_quad15$area, by = list(Species = sub_quad15$spp_duplicate), FUN = sum)
total_sub_spp_15 <- as.data.frame(total_sub_spp_15)
total_sub_spp_15 <- cbind(quad_no = 15, total_sub_spp_15)
rownames(total_sub_spp_15) <- total_sub_spp_15$Species
total_sub_spp_15 <- data.frame(t(total_sub_spp_15))
total_sub_spp_15 <- total_sub_spp_15[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_15)

sub_quad16 <- subset(sub_abund, quad_no == "16")
total_sub_spp_16<- aggregate(sub_quad16$area, by = list(Species = sub_quad16$spp_duplicate), FUN = sum)
total_sub_spp_16 <- as.data.frame(total_sub_spp_16)
total_sub_spp_16 <- cbind(quad_no = 16, total_sub_spp_16)
rownames(total_sub_spp_16) <- total_sub_spp_16$Species
total_sub_spp_16 <- data.frame(t(total_sub_spp_16))
total_sub_spp_16 <- total_sub_spp_16[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_16)

sub_quad17 <- subset(sub_abund, quad_no == "17")
total_sub_spp_17<- aggregate(sub_quad17$area, by = list(Species = sub_quad17$spp_duplicate), FUN = sum)
total_sub_spp_17 <- as.data.frame(total_sub_spp_17)
total_sub_spp_17 <- cbind(quad_no = 17, total_sub_spp_17)
rownames(total_sub_spp_17) <- total_sub_spp_17$Species
total_sub_spp_17 <- data.frame(t(total_sub_spp_17))
total_sub_spp_17 <- total_sub_spp_17[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_17)

sub_quad18 <- subset(sub_abund, quad_no == "18")
total_sub_spp_18<- aggregate(sub_quad18$area, by = list(Species = sub_quad18$spp_duplicate), FUN = sum)
total_sub_spp_18 <- as.data.frame(total_sub_spp_18)
total_sub_spp_18 <- cbind(quad_no = 18, total_sub_spp_18)
rownames(total_sub_spp_18) <- total_sub_spp_18$Species
total_sub_spp_18 <- data.frame(t(total_sub_spp_18))
total_sub_spp_18 <- total_sub_spp_18[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_18)

sub_quad19 <- subset(sub_abund, quad_no == "19") ######### NEED TO CHECK - EMPTU
sub_quad19 <- read.csv("Results/Patch_Stats/Individual_quadrats/Q19_SUB.csv")
sub_quad19 <- subset(sub_quad19, patchID > 0)
total_sub_spp_19<- aggregate(sub_quad19$area, by = list(Species = sub_quad19$spp_duplicate), FUN = sum)
total_sub_spp_19 <- as.data.frame(total_sub_spp_19)
total_sub_spp_19 <- cbind(quad_no = 19, total_sub_spp_19)
rownames(total_sub_spp_19) <- total_sub_spp_19$Species
total_sub_spp_19 <- data.frame(t(total_sub_spp_19))
total_sub_spp_19 <- total_sub_spp_19[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_19)

sub_quad20 <- subset(sub_abund, quad_no == "20")
total_sub_spp_20<- aggregate(sub_quad20$area, by = list(Species = sub_quad20$spp_duplicate), FUN = sum)
total_sub_spp_20 <- as.data.frame(total_sub_spp_20)
total_sub_spp_20 <- cbind(quad_no = 20, total_sub_spp_20)
rownames(total_sub_spp_20) <- total_sub_spp_20$Species
total_sub_spp_20 <- data.frame(t(total_sub_spp_20))
total_sub_spp_20 <- total_sub_spp_20[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_20)

sub_quad21 <- subset(sub_abund, quad_no == "21")
total_sub_spp_21<- aggregate(sub_quad21$area, by = list(Species = sub_quad21$spp_duplicate), FUN = sum)
total_sub_spp_21 <- as.data.frame(total_sub_spp_21)
total_sub_spp_21 <- cbind(quad_no = 21, total_sub_spp_21)
rownames(total_sub_spp_21) <- total_sub_spp_21$Species
total_sub_spp_21 <- data.frame(t(total_sub_spp_21))
total_sub_spp_21 <- total_sub_spp_21[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_21)

sub_quad22 <- subset(sub_abund, quad_no == "22")
total_sub_spp_22<- aggregate(sub_quad22$area, by = list(Species = sub_quad22$spp_duplicate), FUN = sum)
total_sub_spp_22 <- as.data.frame(total_sub_spp_22)
total_sub_spp_22 <- cbind(quad_no = 22, total_sub_spp_22)
rownames(total_sub_spp_22) <- total_sub_spp_22$Species
total_sub_spp_22 <- data.frame(t(total_sub_spp_22))
total_sub_spp_22 <- total_sub_spp_22[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_22)

sub_quad23 <- subset(sub_abund, quad_no == "23")
total_sub_spp_23<- aggregate(sub_quad23$area, by = list(Species = sub_quad23$spp_duplicate), FUN = sum)
total_sub_spp_23 <- as.data.frame(total_sub_spp_23)
total_sub_spp_23 <- cbind(quad_no = 23, total_sub_spp_23)
rownames(total_sub_spp_23) <- total_sub_spp_23$Species
total_sub_spp_23 <- data.frame(t(total_sub_spp_23))
total_sub_spp_23 <- total_sub_spp_23[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_23)

sub_quad24 <- subset(sub_abund, quad_no == "24")
total_sub_spp_24<- aggregate(sub_quad24$area, by = list(Species = sub_quad24$spp_duplicate), FUN = sum)
total_sub_spp_24 <- as.data.frame(total_sub_spp_24)
total_sub_spp_24 <- cbind(quad_no = 24, total_sub_spp_24)
rownames(total_sub_spp_24) <- total_sub_spp_24$Species
total_sub_spp_24 <- data.frame(t(total_sub_spp_24))
total_sub_spp_24 <- total_sub_spp_24[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_24)

sub_quad25 <- subset(sub_abund, quad_no == "25")
total_sub_spp_25<- aggregate(sub_quad25$area, by = list(Species = sub_quad25$spp_duplicate), FUN = sum)
total_sub_spp_25 <- as.data.frame(total_sub_spp_25)
total_sub_spp_25 <- cbind(quad_no = 25, total_sub_spp_25)
rownames(total_sub_spp_25) <- total_sub_spp_25$Species
total_sub_spp_25 <- data.frame(t(total_sub_spp_25))
total_sub_spp_25 <- total_sub_spp_25[-1,]                           
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_25)

sub_quad26 <- subset(sub_abund, quad_no == "26")
total_sub_spp_26<- aggregate(sub_quad26$area, by = list(Species = sub_quad26$spp_duplicate), FUN = sum)
total_sub_spp_26 <- as.data.frame(total_sub_spp_26)
total_sub_spp_26 <- cbind(quad_no = 26, total_sub_spp_26)
rownames(total_sub_spp_26) <- total_sub_spp_26$Species
total_sub_spp_26 <- data.frame(t(total_sub_spp_26))
total_sub_spp_26 <- total_sub_spp_26[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_26)

sub_quad27 <- subset(sub_abund, quad_no == "27")
total_sub_spp_27<- aggregate(sub_quad27$area, by = list(Species = sub_quad27$spp_duplicate), FUN = sum)
total_sub_spp_27 <- as.data.frame(total_sub_spp_27)
total_sub_spp_27 <- cbind(quad_no = 27, total_sub_spp_27)
rownames(total_sub_spp_27) <- total_sub_spp_27$Species
total_sub_spp_27 <- data.frame(t(total_sub_spp_27))
total_sub_spp_27 <- total_sub_spp_27[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_27)

sub_quad_28 <- subset(sub_abund, quad_no == "28")
total_sub_spp_28<- aggregate(sub_quad_28$area, by = list(Species = sub_quad_28$spp_duplicate), FUN = sum)
total_sub_spp_28 <- as.data.frame(total_sub_spp_28)
total_sub_spp_28 <- cbind(quad_no = 28, total_sub_spp_28)
rownames(total_sub_spp_28) <- total_sub_spp_28$Species
total_sub_spp_28 <- data.frame(t(total_sub_spp_28))
total_sub_spp_28 <- total_sub_spp_28[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_28)

sub_quad29 <- subset(sub_abund, quad_no == "29")
total_sub_spp_29<- aggregate(sub_quad29$area, by = list(Species = sub_quad29$spp_duplicate), FUN = sum)
total_sub_spp_29 <- as.data.frame(total_sub_spp_29)
total_sub_spp_29 <- cbind(quad_no = 29, total_sub_spp_29)
rownames(total_sub_spp_29) <- total_sub_spp_29$Species
total_sub_spp_29 <- data.frame(t(total_sub_spp_29))
total_sub_spp_29 <- total_sub_spp_29[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_29)

sub_quad30 <- subset(sub_abund, quad_no == "30")
total_sub_spp_30<- aggregate(sub_quad30$area, by = list(Species = sub_quad30$spp_duplicate), FUN = sum)
total_sub_spp_30 <- as.data.frame(total_sub_spp_30)
total_sub_spp_30 <- cbind(quad_no = 30, total_sub_spp_30)
rownames(total_sub_spp_30) <- total_sub_spp_30$Species
total_sub_spp_30 <- data.frame(t(total_sub_spp_30))
total_sub_spp_30 <- total_sub_spp_30[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_30)

sub_quad31 <- subset(sub_abund, quad_no == "31")
total_sub_spp_31<- aggregate(sub_quad31$area, by = list(Species = sub_quad31$spp_duplicate), FUN = sum)
total_sub_spp_31 <- as.data.frame(total_sub_spp_31)
total_sub_spp_31 <- cbind(quad_no = 31, total_sub_spp_31)
rownames(total_sub_spp_31) <- total_sub_spp_31$Species
total_sub_spp_31 <- data.frame(t(total_sub_spp_31))
total_sub_spp_31 <- total_sub_spp_31[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_31)

sub_quad32 <- subset(sub_abund, quad_no == "32") ### NEED TO CHECK - EMPTY
sub_quad32 <- read.csv("Results/Patch_Stats/Individual_quadrats/Q32_SUB.csv")
sub_quad32 <- subset(sub_quad32, patchID > 0)
total_sub_spp_32<- aggregate(sub_quad32$area, by = list(Species = sub_quad32$spp_duplicate), FUN = sum)
total_sub_spp_32 <- as.data.frame(total_sub_spp_32)
total_sub_spp_32 <- cbind(quad_no = 32, total_sub_spp_32)
rownames(total_sub_spp_32) <- total_sub_spp_32$Species
total_sub_spp_32 <- data.frame(t(total_sub_spp_32))
total_sub_spp_32 <- total_sub_spp_32[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_32)

sub_quad33 <- subset(sub_abund, quad_no == "33")
total_sub_spp_33<- aggregate(sub_quad33$area, by = list(Species = sub_quad33$spp_duplicate), FUN = sum)
total_sub_spp_33 <- as.data.frame(total_sub_spp_33)
total_sub_spp_33 <- cbind(quad_no = 33, total_sub_spp_33)
rownames(total_sub_spp_33) <- total_sub_spp_33$Species
total_sub_spp_33 <- data.frame(t(total_sub_spp_33))
total_sub_spp_33 <- total_sub_spp_33[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_33)

sub_quad34 <- subset(sub_abund, quad_no == "34")
total_sub_spp_34<- aggregate(sub_quad34$area, by = list(Species = sub_quad34$spp_duplicate), FUN = sum)
total_sub_spp_34 <- as.data.frame(total_sub_spp_34)
total_sub_spp_34 <- cbind(quad_no = 34, total_sub_spp_34)
rownames(total_sub_spp_34) <- total_sub_spp_34$Species
total_sub_spp_34 <- data.frame(t(total_sub_spp_34))
total_sub_spp_34 <- total_sub_spp_34[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_34)

sub_quad35 <- subset(sub_abund, quad_no == "35")
total_sub_spp_35<- aggregate(sub_quad35$area, by = list(Species = sub_quad35$spp_duplicate), FUN = sum)
total_sub_spp_35 <- as.data.frame(total_sub_spp_35)
total_sub_spp_35 <- cbind(quad_no = 35, total_sub_spp_35)
rownames(total_sub_spp_35) <- total_sub_spp_35$Species
total_sub_spp_35 <- data.frame(t(total_sub_spp_35))
total_sub_spp_35 <- total_sub_spp_35[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_35)

sub_quad36 <- subset(sub_abund, quad_no == "36")
total_sub_spp_36<- aggregate(sub_quad36$area, by = list(Species = sub_quad36$spp_duplicate), FUN = sum)
total_sub_spp_36 <- as.data.frame(total_sub_spp_36)
total_sub_spp_36 <- cbind(quad_no = 36, total_sub_spp_36)
rownames(total_sub_spp_36) <- total_sub_spp_36$Species
total_sub_spp_36 <- total_sub_spp_36[-1,]
total_sub_spp_36 <- data.frame(t(total_sub_spp_36))
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_36)

sub_quad37 <- subset(sub_abund, quad_no == "37")
total_sub_spp_37<- aggregate(sub_quad37$area, by = list(Species = sub_quad37$spp_duplicate), FUN = sum)
total_sub_spp_37 <- as.data.frame(total_sub_spp_37)
total_sub_spp_37 <- cbind(quad_no = 37, total_sub_spp_37)
rownames(total_sub_spp_37) <- total_sub_spp_37$Species
total_sub_spp_37 <- data.frame(t(total_sub_spp_37))
total_sub_spp_37 <- total_sub_spp_37[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_37)

sub_quad38 <- subset(sub_abund, quad_no == "38")
total_sub_spp_38<- aggregate(sub_quad38$area, by = list(Species = sub_quad38$spp_duplicate), FUN = sum)
total_sub_spp_38 <- as.data.frame(total_sub_spp_38)
total_sub_spp_38 <- cbind(quad_no = 38, total_sub_spp_38)
rownames(total_sub_spp_38) <- total_sub_spp_38$Species
total_sub_spp_38 <- data.frame(t(total_sub_spp_38))
total_sub_spp_38 <- total_sub_spp_38[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_38)

sub_quad39 <- subset(sub_abund, quad_no == "39")
total_sub_spp_39<- aggregate(sub_quad39$area, by = list(Species = sub_quad39$spp_duplicate), FUN = sum)
total_sub_spp_39 <- as.data.frame(total_sub_spp_39)
total_sub_spp_39 <- cbind(quad_no = 39, total_sub_spp_39)
rownames(total_sub_spp_39) <- total_sub_spp_39$Species
total_sub_spp_39 <- data.frame(t(total_sub_spp_39))
total_sub_spp_39 <- total_sub_spp_39[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_39)

sub_quad40 <- subset(sub_abund, quad_no == "40")
total_sub_spp_40<- aggregate(sub_quad40$area, by = list(Species = sub_quad40$spp_duplicate), FUN = sum)
total_sub_spp_40 <- as.data.frame(total_sub_spp_40)
total_sub_spp_40 <- cbind(quad_no = 40, total_sub_spp_40)
rownames(total_sub_spp_40) <- total_sub_spp_40$Species
total_sub_spp_40 <- data.frame(t(total_sub_spp_40))
total_sub_spp_40 <- total_sub_spp_40[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_40)

sub_quad41 <- subset(sub_abund, quad_no == "41")
total_sub_spp_41<- aggregate(sub_quad41$area, by = list(Species = sub_quad41$spp_duplicate), FUN = sum)
total_sub_spp_41 <- as.data.frame(total_sub_spp_41)
total_sub_spp_41 <- cbind(quad_no = 41, total_sub_spp_41)
rownames(total_sub_spp_41) <- total_sub_spp_41$Species
total_sub_spp_41 <- data.frame(t(total_sub_spp_41))
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_41)

sub_quad42 <- subset(sub_abund, quad_no == "42")
total_sub_spp_42<- aggregate(sub_quad42$area, by = list(Species = sub_quad42$spp_duplicate), FUN = sum)
total_sub_spp_42 <- as.data.frame(total_sub_spp_42)
total_sub_spp_42 <- cbind(quad_no = 42, total_sub_spp_42)
rownames(total_sub_spp_42) <- total_sub_spp_42$Species
total_sub_spp_42 <- data.frame(t(total_sub_spp_42))
total_sub_spp_41 <- total_sub_spp_41[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_42)

sub_quad43 <- subset(sub_abund, quad_no == "43")
total_sub_spp_43<- aggregate(sub_quad43$area, by = list(Species = sub_quad43$spp_duplicate), FUN = sum)
total_sub_spp_43 <- as.data.frame(total_sub_spp_43)
total_sub_spp_43 <- cbind(quad_no = 43, total_sub_spp_43)
rownames(total_sub_spp_43) <- total_sub_spp_43$Species
total_sub_spp_43 <- data.frame(t(total_sub_spp_43))
total_sub_spp_43 <- total_sub_spp_43[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_43)

sub_quad44 <- subset(sub_abund, quad_no == "44")
total_sub_spp_44<- aggregate(sub_quad44$area, by = list(Species = sub_quad44$spp_duplicate), FUN = sum)
total_sub_spp_44 <- as.data.frame(total_sub_spp_44)
total_sub_spp_44 <- cbind(quad_no = 44, total_sub_spp_44)
rownames(total_sub_spp_44) <- total_sub_spp_44$Species
total_sub_spp_44 <- data.frame(t(total_sub_spp_44))
total_sub_spp_44 <- total_sub_spp_44[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_44)

sub_quad45 <- subset(sub_abund, quad_no == "45")
total_sub_spp_45<- aggregate(sub_quad45$area, by = list(Species = sub_quad45$spp_duplicate), FUN = sum)
total_sub_spp_45 <- as.data.frame(total_sub_spp_45)
total_sub_spp_45 <- cbind(quad_no = 45, total_sub_spp_45)
rownames(total_sub_spp_45) <- total_sub_spp_45$Species
total_sub_spp_45 <- data.frame(t(total_sub_spp_45))
total_sub_spp_45 <- total_sub_spp_45[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_45)

sub_quad46 <- subset(sub_abund, quad_no == "46")
total_sub_spp_46<- aggregate(sub_quad46$area, by = list(Species = sub_quad46$spp_duplicate), FUN = sum)
total_sub_spp_46 <- as.data.frame(total_sub_spp_46)
total_sub_spp_46 <- cbind(quad_no = 46, total_sub_spp_46)
rownames(total_sub_spp_46) <- total_sub_spp_46$Species
total_sub_spp_46 <- data.frame(t(total_sub_spp_46))
total_sub_spp_46 <- total_sub_spp_46[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_46)

sub_quad47 <- subset(sub_abund, quad_no == "47")
total_sub_spp_47<- aggregate(sub_quad47$area, by = list(Species = sub_quad47$spp_duplicate), FUN = sum)
total_sub_spp_47 <- as.data.frame(total_sub_spp_47)
total_sub_spp_47 <- cbind(quad_no = 47, total_sub_spp_47)
rownames(total_sub_spp_47) <- total_sub_spp_47$Species
total_sub_spp_47 <- data.frame(t(total_sub_spp_47))
total_sub_spp_47 <- total_sub_spp_47[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_47)

sub_quad48 <- subset(sub_abund, quad_no == "48")
total_sub_spp_48<- aggregate(sub_quad48$area, by = list(Species = sub_quad48$spp_duplicate), FUN = sum)
total_sub_spp_48 <- as.data.frame(total_sub_spp_48)
total_sub_spp_48 <- cbind(quad_no = 48, total_sub_spp_48)
rownames(total_sub_spp_48) <- total_sub_spp_48$Species
total_sub_spp_48 <- data.frame(t(total_sub_spp_48))
total_sub_spp_48 <- total_sub_spp_48[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_48)

sub_quad49 <- subset(sub_abund, quad_no == "49")
total_sub_spp_49<- aggregate(sub_quad49$area, by = list(Species = sub_quad49$spp_duplicate), FUN = sum)
total_sub_spp_49 <- as.data.frame(total_sub_spp_49)
total_sub_spp_49 <- cbind(quad_no = 49, total_sub_spp_49)
rownames(total_sub_spp_49) <- total_sub_spp_49$Species
total_sub_spp_49 <- data.frame(t(total_sub_spp_49))
total_sub_spp_49 <- total_sub_spp_49[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_49)

sub_quad50 <- subset(sub_abund, quad_no == "50")
total_sub_spp_50<- aggregate(sub_quad50$area, by = list(Species = sub_quad50$spp_duplicate), FUN = sum)
total_sub_spp_50 <- as.data.frame(total_sub_spp_50)
total_sub_spp_50 <- cbind(quad_no = 50, total_sub_spp_50)
rownames(total_sub_spp_50) <- total_sub_spp_50$Species
total_sub_spp_50 <- data.frame(t(total_sub_spp_50))
total_sub_spp_50 <- total_sub_spp_50[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_50)

sub_quad51 <- subset(sub_abund, quad_no == "51")
total_sub_spp_51<- aggregate(sub_quad51$area, by = list(Species = sub_quad51$spp_duplicate), FUN = sum)
total_sub_spp_51 <- as.data.frame(total_sub_spp_51)
total_sub_spp_51 <- cbind(quad_no = 51, total_sub_spp_51)
rownames(total_sub_spp_51) <- total_sub_spp_51$Species
total_sub_spp_51 <- data.frame(t(total_sub_spp_51))
total_sub_spp_51 <- total_sub_spp_51[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_51)

sub_quad52 <- subset(sub_abund, quad_no == "52")
total_sub_spp_52<- aggregate(sub_quad52$area, by = list(Species = sub_quad52$spp_duplicate), FUN = sum)
total_sub_spp_52 <- as.data.frame(total_sub_spp_52)
total_sub_spp_52 <- cbind(quad_no = 52, total_sub_spp_52)
rownames(total_sub_spp_52) <- total_sub_spp_52$Species
total_sub_spp_52 <- data.frame(t(total_sub_spp_52))
total_sub_spp_52 <- total_sub_spp_52[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_52)

sub_quad53 <- subset(sub_abund, quad_no == "53")
total_sub_spp_53<- aggregate(sub_quad53$area, by = list(Species = sub_quad53$spp_duplicate), FUN = sum)
total_sub_spp_53 <- as.data.frame(total_sub_spp_53)
total_sub_spp_53 <- cbind(quad_no = 53, total_sub_spp_53)
rownames(total_sub_spp_53) <- total_sub_spp_53$Species
total_sub_spp_53 <- data.frame(t(total_sub_spp_53))
total_sub_spp_53 <- total_sub_spp_53[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_53)

sub_quad54 <- subset(sub_abund, quad_no == "54")
total_sub_spp_54<- aggregate(sub_quad54$area, by = list(Species = sub_quad54$spp_duplicate), FUN = sum)
total_sub_spp_54 <- as.data.frame(total_sub_spp_54)
total_sub_spp_54 <- cbind(quad_no = 54, total_sub_spp_54)
rownames(total_sub_spp_54) <- total_sub_spp_54$Species
total_sub_spp_54 <- data.frame(t(total_sub_spp_54))
total_sub_spp_54 <- total_sub_spp_54[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_54)

sub_quad55 <- subset(sub_abund, quad_no == "55")
total_sub_spp_55<- aggregate(sub_quad55$area, by = list(Species = sub_quad55$spp_duplicate), FUN = sum)
total_sub_spp_55 <- as.data.frame(total_sub_spp_55)
total_sub_spp_55 <- cbind(quad_no = 55, total_sub_spp_55)
rownames(total_sub_spp_55) <- total_sub_spp_55$Species
total_sub_spp_55 <- data.frame(t(total_sub_spp_55))
total_sub_spp_55 <- total_sub_spp_55[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_55)

sub_quad56 <- subset(sub_abund, quad_no == "56")
total_sub_spp_56<- aggregate(sub_quad56$area, by = list(Species = sub_quad56$spp_duplicate), FUN = sum)
total_sub_spp_56 <- as.data.frame(total_sub_spp_56)
total_sub_spp_56 <- cbind(quad_no = 56, total_sub_spp_56)
rownames(total_sub_spp_56) <- total_sub_spp_56$Species
total_sub_spp_56 <- data.frame(t(total_sub_spp_56))
total_sub_spp_56 <- total_sub_spp_56[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_56)

sub_quad57 <- subset(sub_abund, quad_no == "57")
total_sub_spp_57<- aggregate(sub_quad57$area, by = list(Species = sub_quad57$spp_duplicate), FUN = sum)
total_sub_spp_57 <- as.data.frame(total_sub_spp_57)
total_sub_spp_57 <- cbind(quad_no = 57, total_sub_spp_57)
rownames(total_sub_spp_57) <- total_sub_spp_57$Species
total_sub_spp_57 <- data.frame(t(total_sub_spp_57))
total_sub_spp_57 <- total_sub_spp_57[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_57)

sub_quad58 <- subset(sub_abund, quad_no == "58")
total_sub_spp_58<- aggregate(sub_quad58$area, by = list(Species = sub_quad58$spp_duplicate), FUN = sum)
total_sub_spp_58 <- as.data.frame(total_sub_spp_58)
total_sub_spp_58 <- cbind(quad_no = 58, total_sub_spp_58)
rownames(total_sub_spp_58) <- total_sub_spp_58$Species
total_sub_spp_58 <- data.frame(t(total_sub_spp_58))
total_sub_spp_58 <- total_sub_spp_58[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_58)

sub_quad59 <- subset(sub_abund, quad_no == "59")
total_sub_spp_59<- aggregate(sub_quad59$area, by = list(Species = sub_quad59$spp_duplicate), FUN = sum)
total_sub_spp_59 <- as.data.frame(total_sub_spp_59)
total_sub_spp_59 <- cbind(quad_no = 59, total_sub_spp_59)
rownames(total_sub_spp_59) <- total_sub_spp_59$Species
total_sub_spp_59 <- data.frame(t(total_sub_spp_59))
total_sub_spp_59 <- total_sub_spp_59[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_59)

sub_quad60 <- subset(sub_abund, quad_no == "60")
total_sub_spp_60<- aggregate(sub_quad60$area, by = list(Species = sub_quad60$spp_duplicate), FUN = sum)
total_sub_spp_60 <- as.data.frame(total_sub_spp_60)
total_sub_spp_60 <- cbind(quad_no = 60, total_sub_spp_60)
rownames(total_sub_spp_60) <- total_sub_spp_60$Species
total_sub_spp_60 <- data.frame(t(total_sub_spp_60))
total_sub_spp_60 <- total_sub_spp_60[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_60)

sub_quad61 <- subset(sub_abund, quad_no == "61")
total_sub_spp_61<- aggregate(sub_quad61$area, by = list(Species = sub_quad61$spp_duplicate), FUN = sum)
total_sub_spp_61 <- as.data.frame(total_sub_spp_61)
total_sub_spp_61 <- cbind(quad_no = 61, total_sub_spp_61)
rownames(total_sub_spp_61) <- total_sub_spp_61$Species
total_sub_spp_61 <- data.frame(t(total_sub_spp_61))
total_sub_spp_61 <- total_sub_spp_61[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_61)

sub_quad62 <- subset(sub_abund, quad_no == "62")
total_sub_spp_62<- aggregate(sub_quad62$area, by = list(Species = sub_quad62$spp_duplicate), FUN = sum)
total_sub_spp_62 <- as.data.frame(total_sub_spp_62)
total_sub_spp_62 <- cbind(quad_no = 62, total_sub_spp_62)
rownames(total_sub_spp_62) <- total_sub_spp_62$Species
total_sub_spp_62 <- data.frame(t(total_sub_spp_62))
total_sub_spp_62 <- total_sub_spp_62[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_62)

sub_quad63 <- subset(sub_abund, quad_no == "63")
total_sub_spp_63<- aggregate(sub_quad63$area, by = list(Species = sub_quad63$spp_duplicate), FUN = sum)
total_sub_spp_63 <- as.data.frame(total_sub_spp_63)
total_sub_spp_63 <- cbind(quad_no = 63, total_sub_spp_63)
rownames(total_sub_spp_63) <- total_sub_spp_63$Species
total_sub_spp_63 <- data.frame(t(total_sub_spp_63))
total_sub_spp_63 <- total_sub_spp_63[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_63)

sub_quad64 <- subset(sub_abund, quad_no == "64")
total_sub_spp_64<- aggregate(sub_quad64$area, by = list(Species = sub_quad64$spp_duplicate), FUN = sum)
total_sub_spp_64 <- as.data.frame(total_sub_spp_64)
total_sub_spp_64 <- cbind(quad_no = 64, total_sub_spp_64)
rownames(total_sub_spp_64) <- total_sub_spp_64$Species
total_sub_spp_64 <- data.frame(t(total_sub_spp_64))
total_sub_spp_64 <- total_sub_spp_64[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_64)

sub_quad65 <- subset(sub_abund, quad_no == "65")
total_sub_spp_65<- aggregate(sub_quad65$area, by = list(Species = sub_quad65$spp_duplicate), FUN = sum)
total_sub_spp_65 <- as.data.frame(total_sub_spp_65)
total_sub_spp_65 <- cbind(quad_no = 65, total_sub_spp_65)
rownames(total_sub_spp_65) <- total_sub_spp_65$Species
total_sub_spp_65 <- data.frame(t(total_sub_spp_65))
total_sub_spp_65 <- total_sub_spp_65[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_65)

sub_quad66 <- subset(sub_abund, quad_no == "66")
total_sub_spp_66<- aggregate(sub_quad66$area, by = list(Species = sub_quad66$spp_duplicate), FUN = sum)
total_sub_spp_66 <- as.data.frame(total_sub_spp_66)
total_sub_spp_66 <- cbind(quad_no = 66, total_sub_spp_66)
rownames(total_sub_spp_66) <- total_sub_spp_66$Species
total_sub_spp_66 <- data.frame(t(total_sub_spp_66))
total_sub_spp_66 <- total_sub_spp_66[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_66)

sub_quad67 <- subset(sub_abund, quad_no == "67")
total_sub_spp_67<- aggregate(sub_quad67$area, by = list(Species = sub_quad67$spp_duplicate), FUN = sum)
total_sub_spp_67 <- as.data.frame(total_sub_spp_67)
total_sub_spp_67 <- cbind(quad_no = 67, total_sub_spp_67)
rownames(total_sub_spp_67) <- total_sub_spp_67$Species
total_sub_spp_67 <- data.frame(t(total_sub_spp_67))
total_sub_spp_67 <- total_sub_spp_67[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_67)

sub_quad68 <- subset(sub_abund, quad_no == "68")
total_sub_spp_68<- aggregate(sub_quad68$area, by = list(Species = sub_quad68$spp_duplicate), FUN = sum)
total_sub_spp_68 <- as.data.frame(total_sub_spp_68)
total_sub_spp_68 <- cbind(quad_no = 68, total_sub_spp_68)
rownames(total_sub_spp_68) <- total_sub_spp_68$Species
total_sub_spp_68 <- data.frame(t(total_sub_spp_68))
total_sub_spp_68 <- total_sub_spp_68[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_68)

sub_quad69 <- subset(sub_abund, quad_no == "69")
total_sub_spp_69<- aggregate(sub_quad69$area, by = list(Species = sub_quad69$spp_duplicate), FUN = sum)
total_sub_spp_69 <- as.data.frame(total_sub_spp_69)
total_sub_spp_69 <- cbind(quad_no = 69, total_sub_spp_69)
rownames(total_sub_spp_69) <- total_sub_spp_69$Species
total_sub_spp_69 <- data.frame(t(total_sub_spp_69))
total_sub_spp_69 <- total_sub_spp_69[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_69)

sub_quad70 <- subset(sub_abund, quad_no == "70")
total_sub_spp_70<- aggregate(sub_quad70$area, by = list(Species = sub_quad70$spp_duplicate), FUN = sum)
total_sub_spp_70 <- as.data.frame(total_sub_spp_70)
total_sub_spp_70 <- cbind(quad_no = 70, total_sub_spp_70)
rownames(total_sub_spp_70) <- total_sub_spp_70$Species
total_sub_spp_70 <- data.frame(t(total_sub_spp_70))
total_sub_spp_70 <- total_sub_spp_70[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_70)

sub_quad71 <- subset(sub_abund, quad_no == "71")
total_sub_spp_71<- aggregate(sub_quad71$area, by = list(Species = sub_quad71$spp_duplicate), FUN = sum)
total_sub_spp_71 <- as.data.frame(total_sub_spp_71)
total_sub_spp_71 <- cbind(quad_no = 71, total_sub_spp_71)
rownames(total_sub_spp_71) <- total_sub_spp_71$Species
total_sub_spp_71 <- data.frame(t(total_sub_spp_71))
total_sub_spp_71 <- total_sub_spp_71[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_71)

sub_quad72 <- subset(sub_abund, quad_no == "72")
total_sub_spp_72<- aggregate(sub_quad72$area, by = list(Species = sub_quad72$spp_duplicate), FUN = sum)
total_sub_spp_72 <- as.data.frame(total_sub_spp_72)
total_sub_spp_72 <- cbind(quad_no = 72, total_sub_spp_72)
rownames(total_sub_spp_72) <- total_sub_spp_72$Species
total_sub_spp_72 <- data.frame(t(total_sub_spp_72))
total_sub_spp_72 <- total_sub_spp_72[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_72)

sub_quad73 <- subset(sub_abund, quad_no == "73")
total_sub_spp_73<- aggregate(sub_quad73$area, by = list(Species = sub_quad73$spp_duplicate), FUN = sum)
total_sub_spp_73 <- as.data.frame(total_sub_spp_73)
total_sub_spp_73 <- cbind(quad_no = 73, total_sub_spp_73)
rownames(total_sub_spp_73) <- total_sub_spp_73$Species
total_sub_spp_73 <- data.frame(t(total_sub_spp_73))
total_sub_spp_73 <- total_sub_spp_73[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_73)

sub_quad74 <- subset(sub_abund, quad_no == "74")
total_sub_spp_74<- aggregate(sub_quad74$area, by = list(Species = sub_quad74$spp_duplicate), FUN = sum)
total_sub_spp_74 <- as.data.frame(total_sub_spp_74)
total_sub_spp_74 <- cbind(quad_no = 74, total_sub_spp_74)
rownames(total_sub_spp_74) <- total_sub_spp_74$Species
total_sub_spp_74 <- data.frame(t(total_sub_spp_74))
total_sub_spp_74 <- total_sub_spp_74[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_74)

sub_quad75 <- subset(sub_abund, quad_no == "75")
total_sub_spp_75<- aggregate(sub_quad75$area, by = list(Species = sub_quad75$spp_duplicate), FUN = sum)
total_sub_spp_75 <- as.data.frame(total_sub_spp_75)
total_sub_spp_75 <- cbind(quad_no = 75, total_sub_spp_75)
rownames(total_sub_spp_75) <- total_sub_spp_75$Species
total_sub_spp_75 <- data.frame(t(total_sub_spp_75))
total_sub_spp_75 <- total_sub_spp_75[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_75)

sub_quad76 <- subset(sub_abund, quad_no == "76")
total_sub_spp_76<- aggregate(sub_quad76$area, by = list(Species = sub_quad76$spp_duplicate), FUN = sum)
total_sub_spp_76 <- as.data.frame(total_sub_spp_76)
total_sub_spp_76 <- cbind(quad_no = 76, total_sub_spp_76)
rownames(total_sub_spp_76) <- total_sub_spp_76$Species
total_sub_spp_76 <- data.frame(t(total_sub_spp_76))
total_sub_spp_76 <- total_sub_spp_76[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_76)

sub_quad77 <- subset(sub_abund, quad_no == "77")
total_sub_spp_77<- aggregate(sub_quad77$area, by = list(Species = sub_quad77$spp_duplicate), FUN = sum)
total_sub_spp_77 <- as.data.frame(total_sub_spp_77)
total_sub_spp_77 <- cbind(quad_no = 77, total_sub_spp_77)
rownames(total_sub_spp_77) <- total_sub_spp_77$Species
total_sub_spp_77 <- data.frame(t(total_sub_spp_77))
total_sub_spp_77 <- total_sub_spp_77[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_77)

sub_quad78 <- subset(sub_abund, quad_no == "78")
total_sub_spp_78<- aggregate(sub_quad78$area, by = list(Species = sub_quad78$spp_duplicate), FUN = sum)
total_sub_spp_78 <- as.data.frame(total_sub_spp_78)
total_sub_spp_78 <- cbind(quad_no = 78, total_sub_spp_78)
rownames(total_sub_spp_78) <- total_sub_spp_78$Species
total_sub_spp_78 <- data.frame(t(total_sub_spp_78))
total_sub_spp_78 <- total_sub_spp_78[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_78)

sub_quad79 <- subset(sub_abund, quad_no == "79")
total_sub_spp_79<- aggregate(sub_quad79$area, by = list(Species = sub_quad79$spp_duplicate), FUN = sum)
total_sub_spp_79 <- as.data.frame(total_sub_spp_79)
total_sub_spp_79 <- cbind(quad_no = 79, total_sub_spp_79)
rownames(total_sub_spp_79) <- total_sub_spp_79$Species
total_sub_spp_79 <- data.frame(t(total_sub_spp_79))
total_sub_spp_79 <- total_sub_spp_79[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_79)

sub_quad80 <- subset(sub_abund, quad_no == "80")
total_sub_spp_80<- aggregate(sub_quad80$area, by = list(Species = sub_quad80$spp_duplicate), FUN = sum)
total_sub_spp_80 <- as.data.frame(total_sub_spp_80)
total_sub_spp_80 <- cbind(quad_no = 80, total_sub_spp_80)
rownames(total_sub_spp_80) <- total_sub_spp_80$Species
total_sub_spp_80 <- data.frame(t(total_sub_spp_80))
total_sub_spp_80 <- total_sub_spp_80[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_80)

sub_quad81 <- subset(sub_abund, quad_no == "81")
total_sub_spp_81<- aggregate(sub_quad81$area, by = list(Species = sub_quad81$spp_duplicate), FUN = sum)
total_sub_spp_81 <- as.data.frame(total_sub_spp_81)
total_sub_spp_81 <- cbind(quad_no = 81, total_sub_spp_81)
rownames(total_sub_spp_81) <- total_sub_spp_81$Species
total_sub_spp_81 <- data.frame(t(total_sub_spp_81))
total_sub_spp_81 <- total_sub_spp_81[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_81)

sub_quad82 <- subset(sub_abund, quad_no == "82")
total_sub_spp_82<- aggregate(sub_quad82$area, by = list(Species = sub_quad82$spp_duplicate), FUN = sum)
total_sub_spp_82 <- as.data.frame(total_sub_spp_82)
total_sub_spp_82 <- cbind(quad_no = 82, total_sub_spp_82)
rownames(total_sub_spp_82) <- total_sub_spp_82$Species
total_sub_spp_82 <- data.frame(t(total_sub_spp_82))
total_sub_spp_82 <- total_sub_spp_82[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_82)

sub_quad83 <- subset(sub_abund, quad_no == "83")
total_sub_spp_83<- aggregate(sub_quad83$area, by = list(Species = sub_quad83$spp_duplicate), FUN = sum)
total_sub_spp_83 <- as.data.frame(total_sub_spp_83)
total_sub_spp_83 <- cbind(quad_no = 83, total_sub_spp_83)
rownames(total_sub_spp_83) <- total_sub_spp_83$Species
total_sub_spp_83 <- data.frame(t(total_sub_spp_83))
total_sub_spp_83 <- total_sub_spp_83[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_83)

sub_quad84 <- subset(sub_abund, quad_no == "84")
total_sub_spp_84<- aggregate(sub_quad84$area, by = list(Species = sub_quad84$spp_duplicate), FUN = sum)
total_sub_spp_84 <- as.data.frame(total_sub_spp_84)
total_sub_spp_84 <- cbind(quad_no = 84, total_sub_spp_84)
rownames(total_sub_spp_84) <- total_sub_spp_84$Species
total_sub_spp_84 <- data.frame(t(total_sub_spp_84))
total_sub_spp_84 <- total_sub_spp_84[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_84)

sub_quad85 <- subset(sub_abund, quad_no == "85")
total_sub_spp_85<- aggregate(sub_quad85$area, by = list(Species = sub_quad85$spp_duplicate), FUN = sum)
total_sub_spp_85 <- as.data.frame(total_sub_spp_85)
total_sub_spp_85 <- cbind(quad_no = 85, total_sub_spp_85)
rownames(total_sub_spp_85) <- total_sub_spp_85$Species
total_sub_spp_85 <- data.frame(t(total_sub_spp_85))
total_sub_spp_85 <- total_sub_spp_85[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_85)

sub_quad86 <- subset(sub_abund, quad_no == "86")
total_sub_spp_86<- aggregate(sub_quad86$area, by = list(Species = sub_quad86$spp_duplicate), FUN = sum)
total_sub_spp_86 <- as.data.frame(total_sub_spp_86)
total_sub_spp_86 <- cbind(quad_no = 86, total_sub_spp_86)
rownames(total_sub_spp_86) <- total_sub_spp_86$Species
total_sub_spp_86 <- data.frame(t(total_sub_spp_86))
total_sub_spp_86 <- total_sub_spp_86[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_86)

sub_quad87 <- subset(sub_abund, quad_no == "87")
total_sub_spp_87<- aggregate(sub_quad87$area, by = list(Species = sub_quad87$spp_duplicate), FUN = sum)
total_sub_spp_87 <- as.data.frame(total_sub_spp_87)
total_sub_spp_87 <- cbind(quad_no = 87, total_sub_spp_87)
rownames(total_sub_spp_87) <- total_sub_spp_87$Species
total_sub_spp_87 <- data.frame(t(total_sub_spp_87))
total_sub_spp_87 <- total_sub_spp_87[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_87)

sub_quad88 <- subset(sub_abund, quad_no == "88")
total_sub_spp_88<- aggregate(sub_quad88$area, by = list(Species = sub_quad88$spp_duplicate), FUN = sum)
total_sub_spp_88 <- as.data.frame(total_sub_spp_88)
total_sub_spp_88 <- cbind(quad_no = 88, total_sub_spp_88)
rownames(total_sub_spp_88) <- total_sub_spp_88$Species
total_sub_spp_88 <- data.frame(t(total_sub_spp_88))
total_sub_spp_88 <- total_sub_spp_88[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_88)

sub_quad89 <- subset(sub_abund, quad_no == "89")
total_sub_spp_89<- aggregate(sub_quad89$area, by = list(Species = sub_quad89$spp_duplicate), FUN = sum)
total_sub_spp_89 <- as.data.frame(total_sub_spp_89)
total_sub_spp_89 <- cbind(quad_no = 89, total_sub_spp_89)
rownames(total_sub_spp_89) <- total_sub_spp_89$Species
total_sub_spp_89 <- data.frame(t(total_sub_spp_89))
total_sub_spp_89 <- total_sub_spp_89[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_89)

sub_quad90 <- subset(sub_abund, quad_no == "90")
total_sub_spp_90<- aggregate(sub_quad90$area, by = list(Species = sub_quad90$spp_duplicate), FUN = sum)
total_sub_spp_90 <- as.data.frame(total_sub_spp_90)
total_sub_spp_90 <- cbind(quad_no = 90, total_sub_spp_90)
rownames(total_sub_spp_90) <- total_sub_spp_90$Species
total_sub_spp_90 <- data.frame(t(total_sub_spp_90))
total_sub_spp_90 <- total_sub_spp_90[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_90)

sub_quad91 <- subset(sub_abund, quad_no == "91")
total_sub_spp_91<- aggregate(sub_quad91$area, by = list(Species = sub_quad91$spp_duplicate), FUN = sum)
total_sub_spp_91 <- as.data.frame(total_sub_spp_91)
total_sub_spp_91 <- cbind(quad_no = 91, total_sub_spp_91)
rownames(total_sub_spp_91) <- total_sub_spp_91$Species
total_sub_spp_91 <- data.frame(t(total_sub_spp_91))
total_sub_spp_91 <- total_sub_spp_91[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_91)

sub_quad92 <- subset(sub_abund, quad_no == "92")
total_sub_spp_92<- aggregate(sub_quad92$area, by = list(Species = sub_quad92$spp_duplicate), FUN = sum)
total_sub_spp_92 <- as.data.frame(total_sub_spp_92)
total_sub_spp_92 <- cbind(quad_no = 92, total_sub_spp_92)
rownames(total_sub_spp_92) <- total_sub_spp_92$Species
total_sub_spp_92 <- data.frame(t(total_sub_spp_92))
total_sub_spp_92 <- total_sub_spp_92[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_92)

sub_quad93 <- subset(sub_abund, quad_no == "93")
total_sub_spp_93<- aggregate(sub_quad93$area, by = list(Species = sub_quad93$spp_duplicate), FUN = sum)
total_sub_spp_93 <- as.data.frame(total_sub_spp_93)
total_sub_spp_93 <- cbind(quad_no = 93, total_sub_spp_93)
rownames(total_sub_spp_93) <- total_sub_spp_93$Species
total_sub_spp_93 <- data.frame(t(total_sub_spp_93))
total_sub_spp_93 <- total_sub_spp_93[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_93)

sub_quad94 <- subset(sub_abund, quad_no == "94")
total_sub_spp_94<- aggregate(sub_quad94$area, by = list(Species = sub_quad94$spp_duplicate), FUN = sum)
total_sub_spp_94 <- as.data.frame(total_sub_spp_94)
total_sub_spp_94 <- cbind(quad_no = 94, total_sub_spp_94)
rownames(total_sub_spp_94) <- total_sub_spp_94$Species
total_sub_spp_94 <- data.frame(t(total_sub_spp_94))
total_sub_spp_94 <- total_sub_spp_94[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_94)

sub_quad95 <- subset(sub_abund, quad_no == "95")
total_sub_spp_95<- aggregate(sub_quad95$area, by = list(Species = sub_quad95$spp_duplicate), FUN = sum)
total_sub_spp_95 <- as.data.frame(total_sub_spp_95)
total_sub_spp_95 <- cbind(quad_no = 95, total_sub_spp_95)
rownames(total_sub_spp_95) <- total_sub_spp_95$Species
total_sub_spp_95 <- data.frame(t(total_sub_spp_95))
total_sub_spp_95 <- total_sub_spp_95[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_95)

sub_quad96 <- subset(sub_abund, quad_no == "96")
total_sub_spp_96<- aggregate(sub_quad96$area, by = list(Species = sub_quad96$spp_duplicate), FUN = sum)
total_sub_spp_96 <- as.data.frame(total_sub_spp_96)
total_sub_spp_96 <- cbind(quad_no = 96, total_sub_spp_96)
rownames(total_sub_spp_96) <- total_sub_spp_96$Species
total_sub_spp_96 <- data.frame(t(total_sub_spp_96))
total_sub_spp_96 <- total_sub_spp_96[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_96)

sub_quad97 <- subset(sub_abund, quad_no == "97")
total_sub_spp_97<- aggregate(sub_quad97$area, by = list(Species = sub_quad97$spp_duplicate), FUN = sum)
total_sub_spp_97 <- as.data.frame(total_sub_spp_97)
total_sub_spp_97 <- cbind(quad_no = 97, total_sub_spp_97)
rownames(total_sub_spp_97) <- total_sub_spp_97$Species
total_sub_spp_97 <- data.frame(t(total_sub_spp_97))
total_sub_spp_97 <- total_sub_spp_97[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_97)

sub_quad98 <- subset(sub_abund, quad_no == "98")
total_sub_spp_98<- aggregate(sub_quad98$area, by = list(Species = sub_quad98$spp_duplicate), FUN = sum)
total_sub_spp_98 <- as.data.frame(total_sub_spp_98)
total_sub_spp_98 <- cbind(quad_no = 98, total_sub_spp_98)
rownames(total_sub_spp_98) <- total_sub_spp_98$Species
total_sub_spp_98 <- data.frame(t(total_sub_spp_98))
total_sub_spp_98 <- total_sub_spp_98[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_98)

sub_quad99 <- subset(sub_abund, quad_no == "99")
total_sub_spp_99<- aggregate(sub_quad99$area, by = list(Species = sub_quad99$spp_duplicate), FUN = sum)
total_sub_spp_99 <- as.data.frame(total_sub_spp_99)
total_sub_spp_99 <- cbind(quad_no = 99, total_sub_spp_99)
rownames(total_sub_spp_99) <- total_sub_spp_99$Species
total_sub_spp_99 <- data.frame(t(total_sub_spp_99))
total_sub_spp_99 <- total_sub_spp_99[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_99)

sub_quad100 <- subset(sub_abund, quad_no == "100")
total_sub_spp_100<- aggregate(sub_quad100$area, by = list(Species = sub_quad100$spp_duplicate), FUN = sum)
total_sub_spp_100 <- as.data.frame(total_sub_spp_100)
total_sub_spp_100 <- cbind(quad_no = 100, total_sub_spp_100)
rownames(total_sub_spp_100) <- total_sub_spp_100$Species
total_sub_spp_100 <- data.frame(t(total_sub_spp_100))
total_sub_spp_100 <- total_sub_spp_100[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_100)

sub_quad101 <- subset(sub_abund, quad_no == "101")
total_sub_spp_101<- aggregate(sub_quad101$area, by = list(Species = sub_quad101$spp_duplicate), FUN = sum)
total_sub_spp_101 <- as.data.frame(total_sub_spp_101)
total_sub_spp_101 <- cbind(quad_no = 101, total_sub_spp_101)
rownames(total_sub_spp_101) <- total_sub_spp_101$Species
total_sub_spp_101 <- data.frame(t(total_sub_spp_101))
total_sub_spp_101 <- total_sub_spp_101[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_101)

sub_quad102 <- subset(sub_abund, quad_no == "102")
total_sub_spp_102<- aggregate(sub_quad102$area, by = list(Species = sub_quad102$spp_duplicate), FUN = sum)
total_sub_spp_102 <- as.data.frame(total_sub_spp_102)
total_sub_spp_102 <- cbind(quad_no = 102, total_sub_spp_102)
rownames(total_sub_spp_102) <- total_sub_spp_102$Species
total_sub_spp_102 <- data.frame(t(total_sub_spp_102))
total_sub_spp_102 <- total_sub_spp_102[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_102)

sub_quad103 <- subset(sub_abund, quad_no == "103")
total_sub_spp_103<- aggregate(sub_quad103$area, by = list(Species = sub_quad103$spp_duplicate), FUN = sum)
total_sub_spp_103 <- as.data.frame(total_sub_spp_103)
total_sub_spp_103 <- cbind(quad_no = 103, total_sub_spp_103)
rownames(total_sub_spp_103) <- total_sub_spp_103$Species
total_sub_spp_103 <- data.frame(t(total_sub_spp_103))
total_sub_spp_103 <- total_sub_spp_103[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_103)

sub_quad104 <- subset(sub_abund, quad_no == "104") ###### NEED TO CHECK - EMPTY
sub_quad104 <- read.csv("Results/Patch_Stats/Individual_quadrats/Q104_SUB.csv") # TEMPORARY FIX
sub_quad104 <- subset(sub_quad104, patchID > 0)
total_sub_spp_104<- aggregate(sub_quad104$area, by = list(Species = sub_quad104$spp_duplicate), FUN = sum)
total_sub_spp_104 <- as.data.frame(total_sub_spp_104)
total_sub_spp_104 <- cbind(quad_no = 104, total_sub_spp_104)
rownames(total_sub_spp_104) <- total_sub_spp_104$Species
total_sub_spp_104 <- data.frame(t(total_sub_spp_104))
total_sub_spp_104 <- total_sub_spp_104[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_104)

sub_quad105 <- subset(sub_abund, quad_no == "105")
total_sub_spp_105<- aggregate(sub_quad105$area, by = list(Species = sub_quad105$spp_duplicate), FUN = sum)
total_sub_spp_105 <- as.data.frame(total_sub_spp_105)
total_sub_spp_105 <- cbind(quad_no = 105, total_sub_spp_105)
rownames(total_sub_spp_105) <- total_sub_spp_105$Species
total_sub_spp_105 <- data.frame(t(total_sub_spp_105))
total_sub_spp_105 <- total_sub_spp_105[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_105)

sub_quad106 <- subset(sub_abund, quad_no == "106")
total_sub_spp_106<- aggregate(sub_quad106$area, by = list(Species = sub_quad106$spp_duplicate), FUN = sum)
total_sub_spp_106 <- as.data.frame(total_sub_spp_106)
total_sub_spp_106 <- cbind(quad_no = 106, total_sub_spp_106)
rownames(total_sub_spp_106) <- total_sub_spp_106$Species
total_sub_spp_106 <- data.frame(t(total_sub_spp_106))
total_sub_spp_106 <- total_sub_spp_106[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_106)

sub_quad107 <- subset(sub_abund, quad_no == "107")
total_sub_spp_107<- aggregate(sub_quad107$area, by = list(Species = sub_quad107$spp_duplicate), FUN = sum)
total_sub_spp_107 <- as.data.frame(total_sub_spp_107)
total_sub_spp_107 <- cbind(quad_no = 107, total_sub_spp_107)
rownames(total_sub_spp_107) <- total_sub_spp_107$Species
total_sub_spp_107 <- data.frame(t(total_sub_spp_107))
total_sub_spp_107 <- total_sub_spp_107[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_107)

sub_quad108 <- subset(sub_abund, quad_no == "108")
total_sub_spp_108<- aggregate(sub_quad108$area, by = list(Species = sub_quad108$spp_duplicate), FUN = sum)
total_sub_spp_108 <- as.data.frame(total_sub_spp_108)
total_sub_spp_108 <- cbind(quad_no = 108, total_sub_spp_108)
rownames(total_sub_spp_108) <- total_sub_spp_108$Species
total_sub_spp_108 <- data.frame(t(total_sub_spp_108))
total_sub_spp_108 <- total_sub_spp_108[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_108)

sub_quad109 <- subset(sub_abund, quad_no == "109")
total_sub_spp_109<- aggregate(sub_quad109$area, by = list(Species = sub_quad109$spp_duplicate), FUN = sum)
total_sub_spp_109 <- as.data.frame(total_sub_spp_109)
total_sub_spp_109 <- cbind(quad_no = 109, total_sub_spp_109)
rownames(total_sub_spp_109) <- total_sub_spp_109$Species
total_sub_spp_109 <- data.frame(t(total_sub_spp_109))
total_sub_spp_109 <- total_sub_spp_109[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_109)

sub_quad110 <- subset(sub_abund, quad_no == "110")
total_sub_spp_110<- aggregate(sub_quad110$area, by = list(Species = sub_quad110$spp_duplicate), FUN = sum)
total_sub_spp_110 <- as.data.frame(total_sub_spp_110)
total_sub_spp_110 <- cbind(quad_no = 110, total_sub_spp_110)
rownames(total_sub_spp_110) <- total_sub_spp_110$Species
total_sub_spp_110 <- data.frame(t(total_sub_spp_110))
total_sub_spp_110 <- total_sub_spp_110[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_110)

sub_quad111 <- subset(sub_abund, quad_no == "111")
total_sub_spp_111<- aggregate(sub_quad111$area, by = list(Species = sub_quad111$spp_duplicate), FUN = sum)
total_sub_spp_111 <- as.data.frame(total_sub_spp_111)
total_sub_spp_111 <- cbind(quad_no = 111, total_sub_spp_111)
rownames(total_sub_spp_111) <- total_sub_spp_111$Species
total_sub_spp_111 <- data.frame(t(total_sub_spp_111))
total_sub_spp_111 <- total_sub_spp_111[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_111)

sub_quad112 <- subset(sub_abund, quad_no == "112")
total_sub_spp_112<- aggregate(sub_quad112$area, by = list(Species = sub_quad112$spp_duplicate), FUN = sum)
total_sub_spp_112 <- as.data.frame(total_sub_spp_112)
total_sub_spp_112 <- cbind(quad_no = 112, total_sub_spp_112)
rownames(total_sub_spp_112) <- total_sub_spp_112$Species
total_sub_spp_112 <- data.frame(t(total_sub_spp_112))
total_sub_spp_112 <- total_sub_spp_112[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_112)

sub_quad113 <- subset(sub_abund, quad_no == "113")
total_sub_spp_113<- aggregate(sub_quad113$area, by = list(Species = sub_quad113$spp_duplicate), FUN = sum)
total_sub_spp_113 <- as.data.frame(total_sub_spp_113)
total_sub_spp_113 <- cbind(quad_no = 113, total_sub_spp_113)
rownames(total_sub_spp_113) <- total_sub_spp_113$Species
total_sub_spp_113 <- data.frame(t(total_sub_spp_113))
total_sub_spp_113 <- total_sub_spp_113[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_113)

sub_quad114 <- subset(sub_abund, quad_no == "114")
total_sub_spp_114<- aggregate(sub_quad114$area, by = list(Species = sub_quad114$spp_duplicate), FUN = sum)
total_sub_spp_114 <- as.data.frame(total_sub_spp_114)
total_sub_spp_114 <- cbind(quad_no = 114, total_sub_spp_114)
rownames(total_sub_spp_114) <- total_sub_spp_114$Species
total_sub_spp_114 <- data.frame(t(total_sub_spp_114))
total_sub_spp_114 <- total_sub_spp_114[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_114)

sub_quad115 <- subset(sub_abund, quad_no == "115")
total_sub_spp_115<- aggregate(sub_quad115$area, by = list(Species = sub_quad115$spp_duplicate), FUN = sum)
total_sub_spp_115 <- as.data.frame(total_sub_spp_115)
total_sub_spp_115 <- cbind(quad_no = 115, total_sub_spp_115)
rownames(total_sub_spp_115) <- total_sub_spp_115$Species
total_sub_spp_115 <- data.frame(t(total_sub_spp_115))
total_sub_spp_115 <- total_sub_spp_115[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_115)

sub_quad116 <- subset(sub_abund, quad_no == "116")
total_sub_spp_116<- aggregate(sub_quad116$area, by = list(Species = sub_quad116$spp_duplicate), FUN = sum)
total_sub_spp_116 <- as.data.frame(total_sub_spp_116)
total_sub_spp_116 <- cbind(quad_no = 116, total_sub_spp_116)
rownames(total_sub_spp_116) <- total_sub_spp_116$Species
total_sub_spp_116 <- data.frame(t(total_sub_spp_116))
total_sub_spp_116 <- total_sub_spp_116[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_116)

sub_quad117 <- subset(sub_abund, quad_no == "117")
total_sub_spp_117<- aggregate(sub_quad117$area, by = list(Species = sub_quad117$spp_duplicate), FUN = sum)
total_sub_spp_117 <- as.data.frame(total_sub_spp_117)
total_sub_spp_117 <- cbind(quad_no = 117, total_sub_spp_117)
rownames(total_sub_spp_117) <- total_sub_spp_117$Species
total_sub_spp_117 <- data.frame(t(total_sub_spp_117))
total_sub_spp_117 <- total_sub_spp_117[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_117)

sub_quad118 <- subset(sub_abund, quad_no == "118")
total_sub_spp_118<- aggregate(sub_quad118$area, by = list(Species = sub_quad118$spp_duplicate), FUN = sum)
total_sub_spp_118 <- as.data.frame(total_sub_spp_118)
total_sub_spp_118 <- cbind(quad_no = 118, total_sub_spp_118)
rownames(total_sub_spp_118) <- total_sub_spp_118$Species
total_sub_spp_118 <- data.frame(t(total_sub_spp_118))
total_sub_spp_118 <- total_sub_spp_118[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_118)

sub_quad119 <- subset(sub_abund, quad_no == "119")
total_sub_spp_119<- aggregate(sub_quad119$area, by = list(Species = sub_quad119$spp_duplicate), FUN = sum)
total_sub_spp_119 <- as.data.frame(total_sub_spp_119)
total_sub_spp_119 <- cbind(quad_no = 119, total_sub_spp_119)
rownames(total_sub_spp_119) <- total_sub_spp_119$Species
total_sub_spp_119 <- data.frame(t(total_sub_spp_119))
total_sub_spp_119 <- total_sub_spp_119[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_119)

sub_quad120 <- subset(sub_abund, quad_no == "120")
total_sub_spp_120<- aggregate(sub_quad120$area, by = list(Species = sub_quad120$spp_duplicate), FUN = sum)
total_sub_spp_120 <- as.data.frame(total_sub_spp_120)
total_sub_spp_120 <- cbind(quad_no = 120, total_sub_spp_120)
rownames(total_sub_spp_120) <- total_sub_spp_120$Species
total_sub_spp_120 <- data.frame(t(total_sub_spp_120))
total_sub_spp_120 <- total_sub_spp_120[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_120)

sub_quad121 <- subset(sub_abund, quad_no == "121")
total_sub_spp_121<- aggregate(sub_quad121$area, by = list(Species = sub_quad121$spp_duplicate), FUN = sum)
total_sub_spp_121 <- as.data.frame(total_sub_spp_121)
total_sub_spp_121 <- cbind(quad_no = 121, total_sub_spp_121)
rownames(total_sub_spp_121) <- total_sub_spp_121$Species
total_sub_spp_121 <- data.frame(t(total_sub_spp_121))
total_sub_spp_121 <- total_sub_spp_121[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_121)

sub_quad122 <- subset(sub_abund, quad_no == "122")
total_sub_spp_122<- aggregate(sub_quad122$area, by = list(Species = sub_quad122$spp_duplicate), FUN = sum)
total_sub_spp_122 <- as.data.frame(total_sub_spp_122)
total_sub_spp_122 <- cbind(quad_no = 122, total_sub_spp_122)
rownames(total_sub_spp_122) <- total_sub_spp_122$Species
total_sub_spp_122 <- data.frame(t(total_sub_spp_122))
total_sub_spp_122 <- total_sub_spp_122[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_122)

sub_quad123 <- subset(sub_abund, quad_no == "123")
total_sub_spp_123<- aggregate(sub_quad123$area, by = list(Species = sub_quad123$spp_duplicate), FUN = sum)
total_sub_spp_123 <- as.data.frame(total_sub_spp_123)
total_sub_spp_123 <- cbind(quad_no = 123, total_sub_spp_123)
rownames(total_sub_spp_123) <- total_sub_spp_123$Species
total_sub_spp_123 <- data.frame(t(total_sub_spp_123))
total_sub_spp_123 <- total_sub_spp_123[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_123)

sub_quad124 <- subset(sub_abund, quad_no == "124")
total_sub_spp_124<- aggregate(sub_quad124$area, by = list(Species = sub_quad124$spp_duplicate), FUN = sum)
total_sub_spp_124 <- as.data.frame(total_sub_spp_124)
total_sub_spp_124 <- cbind(quad_no = 124, total_sub_spp_124)
rownames(total_sub_spp_124) <- total_sub_spp_124$Species
total_sub_spp_124 <- data.frame(t(total_sub_spp_124))
total_sub_spp_124 <- total_sub_spp_124[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_124)

sub_quad125 <- subset(sub_abund, quad_no == "125")
total_sub_spp_125<- aggregate(sub_quad125$area, by = list(Species = sub_quad125$spp_duplicate), FUN = sum)
total_sub_spp_125 <- as.data.frame(total_sub_spp_125)
total_sub_spp_125 <- cbind(quad_no = 125, total_sub_spp_125)
rownames(total_sub_spp_125) <- total_sub_spp_125$Species
total_sub_spp_125 <- data.frame(t(total_sub_spp_125))
total_sub_spp_125 <- total_sub_spp_125[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_125)

sub_quad126 <- subset(sub_abund, quad_no == "126")
total_sub_spp_126<- aggregate(sub_quad126$area, by = list(Species = sub_quad126$spp_duplicate), FUN = sum)
total_sub_spp_126 <- as.data.frame(total_sub_spp_126)
total_sub_spp_126 <- cbind(quad_no = 126, total_sub_spp_126)
rownames(total_sub_spp_126) <- total_sub_spp_126$Species
total_sub_spp_126 <- data.frame(t(total_sub_spp_126))
total_sub_spp_126 <- total_sub_spp_126[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_126)

sub_quad127 <- subset(sub_abund, quad_no == "127")
total_sub_spp_127<- aggregate(sub_quad127$area, by = list(Species = sub_quad127$spp_duplicate), FUN = sum)
total_sub_spp_127 <- as.data.frame(total_sub_spp_127)
total_sub_spp_127 <- cbind(quad_no = 127, total_sub_spp_127)
rownames(total_sub_spp_127) <- total_sub_spp_127$Species
total_sub_spp_127 <- data.frame(t(total_sub_spp_127))
total_sub_spp_127 <- total_sub_spp_127[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_127)

sub_quad128 <- subset(sub_abund, quad_no == "128")
total_sub_spp_128<- aggregate(sub_quad128$area, by = list(Species = sub_quad128$spp_duplicate), FUN = sum)
total_sub_spp_128 <- as.data.frame(total_sub_spp_128)
total_sub_spp_128 <- cbind(quad_no = 128, total_sub_spp_128)
rownames(total_sub_spp_128) <- total_sub_spp_128$Species
total_sub_spp_128 <- data.frame(t(total_sub_spp_128))
total_sub_spp_128 <- total_sub_spp_128[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_128)

sub_quad129 <- subset(sub_abund, quad_no == "129")
total_sub_spp_129<- aggregate(sub_quad129$area, by = list(Species = sub_quad129$spp_duplicate), FUN = sum)
total_sub_spp_129 <- as.data.frame(total_sub_spp_129)
total_sub_spp_129 <- cbind(quad_no = 129, total_sub_spp_129)
rownames(total_sub_spp_129) <- total_sub_spp_129$Species
total_sub_spp_129 <- data.frame(t(total_sub_spp_129))
total_sub_spp_129 <- total_sub_spp_129[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_129)

sub_quad130 <- subset(sub_abund, quad_no == "130")
total_sub_spp_130<- aggregate(sub_quad130$area, by = list(Species = sub_quad130$spp_duplicate), FUN = sum)
total_sub_spp_130 <- as.data.frame(total_sub_spp_130)
total_sub_spp_130 <- cbind(quad_no = 130, total_sub_spp_130)
rownames(total_sub_spp_130) <- total_sub_spp_130$Species
total_sub_spp_130 <- data.frame(t(total_sub_spp_130))
total_sub_spp_130 <- total_sub_spp_130[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_130)

sub_quad131 <- subset(sub_abund, quad_no == "131")
total_sub_spp_131<- aggregate(sub_quad131$area, by = list(Species = sub_quad131$spp_duplicate), FUN = sum)
total_sub_spp_131 <- as.data.frame(total_sub_spp_131)
total_sub_spp_131 <- cbind(quad_no = 131, total_sub_spp_131)
rownames(total_sub_spp_131) <- total_sub_spp_131$Species
total_sub_spp_131 <- data.frame(t(total_sub_spp_131))
total_sub_spp_131 <- total_sub_spp_131[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_131)

sub_quad132 <- subset(sub_abund, quad_no == "132")
total_sub_spp_132<- aggregate(sub_quad132$area, by = list(Species = sub_quad132$spp_duplicate), FUN = sum)
total_sub_spp_132 <- as.data.frame(total_sub_spp_132)
total_sub_spp_132 <- cbind(quad_no = 132, total_sub_spp_132)
rownames(total_sub_spp_132) <- total_sub_spp_132$Species
total_sub_spp_132 <- data.frame(t(total_sub_spp_132))
total_sub_spp_132 <- total_sub_spp_132[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_132)

sub_quad133 <- subset(sub_abund, quad_no == "133")
total_sub_spp_133<- aggregate(sub_quad133$area, by = list(Species = sub_quad133$spp_duplicate), FUN = sum)
total_sub_spp_133 <- as.data.frame(total_sub_spp_133)
total_sub_spp_133 <- cbind(quad_no = 133, total_sub_spp_133)
rownames(total_sub_spp_133) <- total_sub_spp_133$Species
total_sub_spp_133 <- data.frame(t(total_sub_spp_133))
total_sub_spp_133 <- total_sub_spp_133[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_133)

sub_quad134 <- subset(sub_abund, quad_no == "134")
total_sub_spp_134<- aggregate(sub_quad134$area, by = list(Species = sub_quad134$spp_duplicate), FUN = sum)
total_sub_spp_134 <- as.data.frame(total_sub_spp_134)
total_sub_spp_134 <- cbind(quad_no = 134, total_sub_spp_134)
rownames(total_sub_spp_134) <- total_sub_spp_134$Species
total_sub_spp_134 <- data.frame(t(total_sub_spp_134))
total_sub_spp_134 <- total_sub_spp_134[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_134)

sub_quad135 <- subset(sub_abund, quad_no == "135")
total_sub_spp_135<- aggregate(sub_quad135$area, by = list(Species = sub_quad135$spp_duplicate), FUN = sum)
total_sub_spp_135 <- as.data.frame(total_sub_spp_135)
total_sub_spp_135 <- cbind(quad_no = 135, total_sub_spp_135)
rownames(total_sub_spp_135) <- total_sub_spp_135$Species
total_sub_spp_135 <- data.frame(t(total_sub_spp_135))
total_sub_spp_135 <- total_sub_spp_135[-1,]
area_sub_spp <- rbind(area_sub_spp, total_sub_spp_135)

sub_quad136 <- subset(sub_abund, quad_no == "136")
total_sub_spp_136<- aggregate(sub_quad136$area, by = list(Species = sub_quad136$spp_duplicate), FUN = sum)
total_sub_spp_136 <- as.data.frame(total_sub_spp_136)
total_sub_spp_136 <- cbind(quad_no = 136, total_sub_spp_136)
rownames(total_sub_spp_136) <- total_sub_spp_136$Species
total_sub_spp_136 <- data.frame(t(total_sub_spp_136))
total_sub_spp_136 <- total_sub_spp_136[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_136)

sub_quad137 <- subset(sub_abund, quad_no == "137")
total_sub_spp_137<- aggregate(sub_quad137$area, by = list(Species = sub_quad137$spp_duplicate), FUN = sum)
total_sub_spp_137 <- as.data.frame(total_sub_spp_137)
total_sub_spp_137 <- cbind(quad_no = 137, total_sub_spp_137)
rownames(total_sub_spp_137) <- total_sub_spp_137$Species
total_sub_spp_137 <- data.frame(t(total_sub_spp_137))
total_sub_spp_137 <- total_sub_spp_137[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_137)

sub_quad138 <- subset(sub_abund, quad_no == "138")
total_sub_spp_138<- aggregate(sub_quad138$area, by = list(Species = sub_quad138$spp_duplicate), FUN = sum)
total_sub_spp_138 <- as.data.frame(total_sub_spp_138)
total_sub_spp_138 <- cbind(quad_no = 138, total_sub_spp_138)
rownames(total_sub_spp_138) <- total_sub_spp_138$Species
total_sub_spp_138 <- data.frame(t(total_sub_spp_138))
total_sub_spp_138 <- total_sub_spp_138[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_138)

sub_quad139 <- subset(sub_abund, quad_no == "139")
total_sub_spp_139<- aggregate(sub_quad139$area, by = list(Species = sub_quad139$spp_duplicate), FUN = sum)
total_sub_spp_139 <- as.data.frame(total_sub_spp_139)
total_sub_spp_139 <- cbind(quad_no = 139, total_sub_spp_139)
rownames(total_sub_spp_139) <- total_sub_spp_139$Species
total_sub_spp_139 <- data.frame(t(total_sub_spp_139))
total_sub_spp_139 <- total_sub_spp_139[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_139)

sub_quad140 <- subset(sub_abund, quad_no == "140")
total_sub_spp_140<- aggregate(sub_quad140$area, by = list(Species = sub_quad140$spp_duplicate), FUN = sum)
total_sub_spp_140 <- as.data.frame(total_sub_spp_140)
total_sub_spp_140 <- cbind(quad_no = 140, total_sub_spp_140)
rownames(total_sub_spp_140) <- total_sub_spp_140$Species
total_sub_spp_140 <- data.frame(t(total_sub_spp_140))
total_sub_spp_140 <- total_sub_spp_140[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_140)

sub_quad141 <- subset(sub_abund, quad_no == "141")
total_sub_spp_141<- aggregate(sub_quad141$area, by = list(Species = sub_quad141$spp_duplicate), FUN = sum)
total_sub_spp_141 <- as.data.frame(total_sub_spp_141)
total_sub_spp_141 <- cbind(quad_no = 141, total_sub_spp_141)
rownames(total_sub_spp_141) <- total_sub_spp_141$Species
total_sub_spp_141 <- data.frame(t(total_sub_spp_141))
total_sub_spp_141 <- total_sub_spp_141[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_141)

sub_quad142 <- subset(sub_abund, quad_no == "142")
total_sub_spp_142<- aggregate(sub_quad142$area, by = list(Species = sub_quad142$spp_duplicate), FUN = sum)
total_sub_spp_142 <- as.data.frame(total_sub_spp_142)
total_sub_spp_142 <- cbind(quad_no = 142, total_sub_spp_142)
rownames(total_sub_spp_142) <- total_sub_spp_142$Species
total_sub_spp_142 <- data.frame(t(total_sub_spp_142))
total_sub_spp_142 <- total_sub_spp_142[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_142)

sub_quad143 <- subset(sub_abund, quad_no == "143")
total_sub_spp_143<- aggregate(sub_quad143$area, by = list(Species = sub_quad143$spp_duplicate), FUN = sum)
total_sub_spp_143 <- as.data.frame(total_sub_spp_143)
total_sub_spp_143 <- cbind(quad_no = 143, total_sub_spp_143)
rownames(total_sub_spp_143) <- total_sub_spp_143$Species
total_sub_spp_143 <- data.frame(t(total_sub_spp_143))
total_sub_spp_143 <- total_sub_spp_143[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_143)

sub_quad144 <- subset(sub_abund, quad_no == "144")
total_sub_spp_144<- aggregate(sub_quad144$area, by = list(Species = sub_quad144$spp_duplicate), FUN = sum)
total_sub_spp_144 <- as.data.frame(total_sub_spp_144)
total_sub_spp_144 <- cbind(quad_no = 144, total_sub_spp_144)
rownames(total_sub_spp_144) <- total_sub_spp_144$Species
total_sub_spp_144 <- data.frame(t(total_sub_spp_144))
total_sub_spp_144 <- total_sub_spp_144[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_144)

sub_quad145 <- subset(sub_abund, quad_no == "145")
total_sub_spp_145<- aggregate(sub_quad145$area, by = list(Species = sub_quad145$spp_duplicate), FUN = sum)
total_sub_spp_145 <- as.data.frame(total_sub_spp_145)
total_sub_spp_145 <- cbind(quad_no = 145, total_sub_spp_145)
rownames(total_sub_spp_145) <- total_sub_spp_145$Species
total_sub_spp_145 <- data.frame(t(total_sub_spp_145))
total_sub_spp_145 <- total_sub_spp_145[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_145)

sub_quad146 <- subset(sub_abund, quad_no == "146")
total_sub_spp_146<- aggregate(sub_quad146$area, by = list(Species = sub_quad146$spp_duplicate), FUN = sum)
total_sub_spp_146 <- as.data.frame(total_sub_spp_146)
total_sub_spp_146 <- cbind(quad_no = 146, total_sub_spp_146)
rownames(total_sub_spp_146) <- total_sub_spp_146$Species
total_sub_spp_146 <- data.frame(t(total_sub_spp_146))
total_sub_spp_146 <- total_sub_spp_146[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_146)

sub_quad147 <- subset(sub_abund, quad_no == "147")
total_sub_spp_147<- aggregate(sub_quad147$area, by = list(Species = sub_quad147$spp_duplicate), FUN = sum)
total_sub_spp_147 <- as.data.frame(total_sub_spp_147)
total_sub_spp_147 <- cbind(quad_no = 147, total_sub_spp_147)
rownames(total_sub_spp_147) <- total_sub_spp_147$Species
total_sub_spp_147 <- data.frame(t(total_sub_spp_147))
total_sub_spp_147 <- total_sub_spp_147[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_147)

sub_quad148 <- subset(sub_abund, quad_no == "148")
total_sub_spp_148<- aggregate(sub_quad148$area, by = list(Species = sub_quad148$spp_duplicate), FUN = sum)
total_sub_spp_148 <- as.data.frame(total_sub_spp_148)
total_sub_spp_148 <- cbind(quad_no = 148, total_sub_spp_148)
rownames(total_sub_spp_148) <- total_sub_spp_148$Species
total_sub_spp_148 <- data.frame(t(total_sub_spp_148))
total_sub_spp_148 <- total_sub_spp_148[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_148)

sub_quad149 <- subset(sub_abund, quad_no == "149")
total_sub_spp_149<- aggregate(sub_quad149$area, by = list(Species = sub_quad149$spp_duplicate), FUN = sum)
total_sub_spp_149 <- as.data.frame(total_sub_spp_149)
total_sub_spp_149 <- cbind(quad_no = 149, total_sub_spp_149)
rownames(total_sub_spp_149) <- total_sub_spp_149$Species
total_sub_spp_149 <- data.frame(t(total_sub_spp_149))
total_sub_spp_149 <- total_sub_spp_149[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_149)

sub_quad150 <- subset(sub_abund, quad_no == "150")
total_sub_spp_150<- aggregate(sub_quad150$area, by = list(Species = sub_quad150$spp_duplicate), FUN = sum)
total_sub_spp_150 <- as.data.frame(total_sub_spp_150)
total_sub_spp_150 <- cbind(quad_no = 150, total_sub_spp_150)
rownames(total_sub_spp_150) <- total_sub_spp_150$Species
total_sub_spp_150 <- data.frame(t(total_sub_spp_150))
total_sub_spp_150 <- total_sub_spp_150[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_150)

sub_quad151 <- subset(sub_abund, quad_no == "151")
total_sub_spp_151<- aggregate(sub_quad151$area, by = list(Species = sub_quad151$spp_duplicate), FUN = sum)
total_sub_spp_151 <- as.data.frame(total_sub_spp_151)
total_sub_spp_151 <- cbind(quad_no = 151, total_sub_spp_151)
rownames(total_sub_spp_151) <- total_sub_spp_151$Species
total_sub_spp_151 <- data.frame(t(total_sub_spp_151))
total_sub_spp_151 <- total_sub_spp_151[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_151)

sub_quad152 <- subset(sub_abund, quad_no == "152")
total_sub_spp_152<- aggregate(sub_quad152$area, by = list(Species = sub_quad152$spp_duplicate), FUN = sum)
total_sub_spp_152 <- as.data.frame(total_sub_spp_152)
total_sub_spp_152 <- cbind(quad_no = 152, total_sub_spp_152)
rownames(total_sub_spp_152) <- total_sub_spp_152$Species
total_sub_spp_152 <- data.frame(t(total_sub_spp_152))
total_sub_spp_152 <- total_sub_spp_152[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_152)

sub_quad153 <- subset(sub_abund, quad_no == "153")
total_sub_spp_153<- aggregate(sub_quad153$area, by = list(Species = sub_quad153$spp_duplicate), FUN = sum)
total_sub_spp_153 <- as.data.frame(total_sub_spp_153)
total_sub_spp_153 <- cbind(quad_no = 153, total_sub_spp_153)
rownames(total_sub_spp_153) <- total_sub_spp_153$Species
total_sub_spp_153 <- data.frame(t(total_sub_spp_153))
total_sub_spp_153 <- total_sub_spp_153[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_153)

sub_quad154 <- subset(sub_abund, quad_no == "154")
total_sub_spp_154<- aggregate(sub_quad154$area, by = list(Species = sub_quad154$spp_duplicate), FUN = sum)
total_sub_spp_154 <- as.data.frame(total_sub_spp_154)
total_sub_spp_154 <- cbind(quad_no = 154, total_sub_spp_154)
rownames(total_sub_spp_154) <- total_sub_spp_154$Species
total_sub_spp_154 <- data.frame(t(total_sub_spp_154))
total_sub_spp_154 <- total_sub_spp_154[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_154)

sub_quad155 <- subset(sub_abund, quad_no == "155")
total_sub_spp_155<- aggregate(sub_quad155$area, by = list(Species = sub_quad155$spp_duplicate), FUN = sum)
total_sub_spp_155 <- as.data.frame(total_sub_spp_155)
total_sub_spp_155 <- cbind(quad_no = 155, total_sub_spp_155)
rownames(total_sub_spp_155) <- total_sub_spp_155$Species
total_sub_spp_155 <- data.frame(t(total_sub_spp_155))
total_sub_spp_155 <- total_sub_spp_155[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_155)

sub_quad156 <- subset(sub_abund, quad_no == "156")
total_sub_spp_156<- aggregate(sub_quad156$area, by = list(Species = sub_quad156$spp_duplicate), FUN = sum)
total_sub_spp_156 <- as.data.frame(total_sub_spp_156)
total_sub_spp_156 <- cbind(quad_no = 156, total_sub_spp_156)
rownames(total_sub_spp_156) <- total_sub_spp_156$Species
total_sub_spp_156 <- data.frame(t(total_sub_spp_156))
total_sub_spp_156 <- total_sub_spp_156[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_156)

sub_quad157 <- subset(sub_abund, quad_no == "157")
total_sub_spp_157<- aggregate(sub_quad157$area, by = list(Species = sub_quad157$spp_duplicate), FUN = sum)
total_sub_spp_157 <- as.data.frame(total_sub_spp_157)
total_sub_spp_157 <- cbind(quad_no = 157, total_sub_spp_157)
rownames(total_sub_spp_157) <- total_sub_spp_157$Species
total_sub_spp_157 <- data.frame(t(total_sub_spp_157))
total_sub_spp_157 <- total_sub_spp_157[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_157)

sub_quad158 <- subset(sub_abund, quad_no == "158")
total_sub_spp_158<- aggregate(sub_quad158$area, by = list(Species = sub_quad158$spp_duplicate), FUN = sum)
total_sub_spp_158 <- as.data.frame(total_sub_spp_158)
total_sub_spp_158 <- cbind(quad_no = 158, total_sub_spp_158)
rownames(total_sub_spp_158) <- total_sub_spp_158$Species
total_sub_spp_158 <- data.frame(t(total_sub_spp_158))
total_sub_spp_158 <- total_sub_spp_158[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_158)

sub_quad159 <- subset(sub_abund, quad_no == "159")
total_sub_spp_159<- aggregate(sub_quad159$area, by = list(Species = sub_quad159$spp_duplicate), FUN = sum)
total_sub_spp_159 <- as.data.frame(total_sub_spp_159)
total_sub_spp_159 <- cbind(quad_no = 159, total_sub_spp_159)
rownames(total_sub_spp_159) <- total_sub_spp_159$Species
total_sub_spp_159 <- data.frame(t(total_sub_spp_159))
total_sub_spp_159 <- total_sub_spp_159[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_159)

sub_quad160 <- subset(sub_abund, quad_no == "160")
total_sub_spp_160<- aggregate(sub_quad160$area, by = list(Species = sub_quad160$spp_duplicate), FUN = sum)
total_sub_spp_160 <- as.data.frame(total_sub_spp_160)
total_sub_spp_160 <- cbind(quad_no = 160, total_sub_spp_160)
rownames(total_sub_spp_160) <- total_sub_spp_160$Species
total_sub_spp_160 <- data.frame(t(total_sub_spp_160))
total_sub_spp_160 <- total_sub_spp_160[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_160)

sub_quad161 <- subset(sub_abund, quad_no == "161")
total_sub_spp_161<- aggregate(sub_quad161$area, by = list(Species = sub_quad161$spp_duplicate), FUN = sum)
total_sub_spp_161 <- as.data.frame(total_sub_spp_161)
total_sub_spp_161 <- cbind(quad_no = 161, total_sub_spp_161)
rownames(total_sub_spp_161) <- total_sub_spp_161$Species
total_sub_spp_161 <- data.frame(t(total_sub_spp_161))
total_sub_spp_161 <- total_sub_spp_161[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_161)

sub_quad162 <- subset(sub_abund, quad_no == "162")
total_sub_spp_162<- aggregate(sub_quad162$area, by = list(Species = sub_quad162$spp_duplicate), FUN = sum)
total_sub_spp_162 <- as.data.frame(total_sub_spp_162)
total_sub_spp_162 <- cbind(quad_no = 162, total_sub_spp_162)
rownames(total_sub_spp_162) <- total_sub_spp_162$Species
total_sub_spp_162 <- data.frame(t(total_sub_spp_162))
total_sub_spp_162 <- total_sub_spp_162[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_162)

sub_quad163 <- subset(sub_abund, quad_no == "163")
total_sub_spp_163<- aggregate(sub_quad163$area, by = list(Species = sub_quad163$spp_duplicate), FUN = sum)
total_sub_spp_163 <- as.data.frame(total_sub_spp_163)
total_sub_spp_163 <- cbind(quad_no = 163, total_sub_spp_163)
rownames(total_sub_spp_163) <- total_sub_spp_163$Species
total_sub_spp_163 <- data.frame(t(total_sub_spp_163))
total_sub_spp_163 <- total_sub_spp_163[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_163)

sub_quad164 <- subset(sub_abund, quad_no == "164")
total_sub_spp_164<- aggregate(sub_quad164$area, by = list(Species = sub_quad164$spp_duplicate), FUN = sum)
total_sub_spp_164 <- as.data.frame(total_sub_spp_164)
total_sub_spp_164 <- cbind(quad_no = 164, total_sub_spp_164)
rownames(total_sub_spp_164) <- total_sub_spp_164$Species
total_sub_spp_164 <- data.frame(t(total_sub_spp_164))
total_sub_spp_164 <- total_sub_spp_164[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_164)

sub_quad165 <- subset(sub_abund, quad_no == "165")
total_sub_spp_165<- aggregate(sub_quad165$area, by = list(Species = sub_quad165$spp_duplicate), FUN = sum)
total_sub_spp_165 <- as.data.frame(total_sub_spp_165)
total_sub_spp_165 <- cbind(quad_no = 165, total_sub_spp_165)
rownames(total_sub_spp_165) <- total_sub_spp_165$Species
total_sub_spp_165 <- data.frame(t(total_sub_spp_165))
total_sub_spp_165 <- total_sub_spp_165[-1,]
area_sub_spp <- rbind(area_sub_spp,  total_sub_spp_165)

sub_quad166 <- subset(sub_abund, quad_no == "166")
total_sub_spp_166<- aggregate(sub_quad166$area, by = list(Species = sub_quad166$spp_duplicate), FUN = sum)
total_sub_spp_166 <- as.data.frame(total_sub_spp_166)
total_sub_spp_166 <- cbind(quad_no = 166, total_sub_spp_166)
rownames(total_sub_spp_166) <- total_sub_spp_166$Species
total_sub_spp_166 <- data.frame(t(total_sub_spp_166))
total_sub_spp_166 <- total_sub_spp_166[-1,]
area_sub_spp <- rbind(area_sub_spp, total_sub_spp_166)

sub_quad167 <- subset(sub_abund, quad_no == "167")
total_sub_spp_167<- aggregate(sub_quad167$area, by = list(Species = sub_quad167$spp_duplicate), FUN = sum)
total_sub_spp_167 <- as.data.frame(total_sub_spp_167)
total_sub_spp_167 <- cbind(quad_no = 167, total_sub_spp_167)
rownames(total_sub_spp_167) <- total_sub_spp_167$Species
total_sub_spp_167 <- data.frame(t(total_sub_spp_167))
total_sub_spp_167 <- total_sub_spp_167[-1,]
area_sub_spp <- rbind(area_sub_spp, total_sub_spp_167)

write.csv(area_sub_spp, file = "Results/Patch_Stats/Ordination/Area_patches_spp/area_sub_spp_04-08-18_final.csv")


################################################################################### 
##         SHAPE INDEX PER SPECIES: FOR USE WITH SPECIES MATRIX + GLMs     ##
###################################################################################

sub_abund <- read.csv("Results/Patch_Stats/Individual_quadrats/04-08-18_SUB.csv")
sub_abund <- sub_abund[,-1]
sub_abund <- subset(sub_abund, patchID > 0) # removes patch ID 0

shape_sub_spp <- NULL

## Getting data from each quadrat separately
subdom_quad1 <- subset(sub_abund, quad_no == "1")
shape_spp_1<- aggregate(subdom_quad1$shape.index, by = list(Species = subdom_quad1$spp_duplicate), FUN = mean)
shape_spp_1 <- as.data.frame(shape_spp_1)
shape_spp_1 <- cbind(quad_no = 1, shape_spp_1)
rownames(shape_spp_1) <- shape_spp_1$Species
shape_spp_1 <- data.frame(t(shape_spp_1))
shape_spp_1 <- shape_spp_1[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_1)

subdom_quad2 <- subset(sub_abund, quad_no == "2")
shape_spp_2<- aggregate(subdom_quad2$shape.index, by = list(Species = subdom_quad2$spp_duplicate), FUN = mean)
shape_spp_2 <- as.data.frame(shape_spp_2)
shape_spp_2 <- cbind(quad_no = 2, shape_spp_2)
rownames(shape_spp_2) <- shape_spp_2$Species
shape_spp_2 <- data.frame(t(shape_spp_2))
shape_spp_2 <- shape_spp_2[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_2)

subdom_quad3 <- subset(sub_abund, quad_no == "3")
shape_spp_3<- aggregate(subdom_quad3$shape.index, by = list(Species = subdom_quad3$spp_duplicate), FUN = mean)
shape_spp_3 <- as.data.frame(shape_spp_3)
shape_spp_3 <- cbind(quad_no = 3, shape_spp_3)
rownames(shape_spp_3) <- shape_spp_3$Species
shape_spp_3 <- data.frame(t(shape_spp_3))
shape_spp_3 <- shape_spp_3[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_3)

subdom_quad4 <- subset(sub_abund, quad_no == "4")
shape_spp_4<- aggregate(subdom_quad4$shape.index, by = list(Species = subdom_quad4$spp_duplicate), FUN = mean)
shape_spp_4 <- as.data.frame(shape_spp_4)
shape_spp_4 <- cbind(quad_no = 4, shape_spp_4)
rownames(shape_spp_4) <- shape_spp_4$Species
shape_spp_4 <- data.frame(t(shape_spp_4))
shape_spp_4 <- shape_spp_4[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_4)

subdom_quad5 <- subset(sub_abund, quad_no == "5")
shape_spp_5<- aggregate(subdom_quad5$shape.index, by = list(Species = subdom_quad5$spp_duplicate), FUN = mean)
shape_spp_5 <- as.data.frame(shape_spp_5)
shape_spp_5 <- cbind(quad_no = 5, shape_spp_5)
rownames(shape_spp_5) <- shape_spp_5$Species
shape_spp_5 <- data.frame(t(shape_spp_5))
shape_spp_5 <- shape_spp_5[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_5)

subdom_quad6 <- subset(sub_abund, quad_no == "6")
shape_spp_6<- aggregate(subdom_quad6$shape.index, by = list(Species = subdom_quad6$spp_duplicate), FUN = mean)
shape_spp_6 <- as.data.frame(shape_spp_6)
shape_spp_6 <- cbind(quad_no = 6, shape_spp_6)
rownames(shape_spp_6) <- shape_spp_6$Species
shape_spp_6 <- data.frame(t(shape_spp_6))
shape_spp_6 <- shape_spp_6[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_6)

subdom_quad7 <- subset(sub_abund, quad_no == "7")
shape_spp_7<- aggregate(subdom_quad7$shape.index, by = list(Species = subdom_quad7$spp_duplicate), FUN = mean)
shape_spp_7 <- as.data.frame(shape_spp_7)
shape_spp_7 <- cbind(quad_no = 7, shape_spp_7)
rownames(shape_spp_7) <- shape_spp_7$Species
shape_spp_7 <- data.frame(t(shape_spp_7))
shape_spp_7 <- shape_spp_7[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_7)

subdom_quad8 <- subset(sub_abund, quad_no == "8")
shape_spp_8<- aggregate(subdom_quad8$shape.index, by = list(Species = subdom_quad8$spp_duplicate), FUN = mean)
shape_spp_8 <- as.data.frame(shape_spp_8)
shape_spp_8 <- cbind(quad_no = 8, shape_spp_8)
rownames(shape_spp_8) <- shape_spp_8$Species
shape_spp_8 <- data.frame(t(shape_spp_8))
shape_spp_8 <- shape_spp_8[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_8)

subdom_quad9 <- subset(sub_abund, quad_no == "9")
shape_spp_9<- aggregate(subdom_quad9$shape.index, by = list(Species = subdom_quad9$spp_duplicate), FUN = mean)
shape_spp_9 <- as.data.frame(shape_spp_9)
shape_spp_9 <- cbind(quad_no = 9, shape_spp_9)
rownames(shape_spp_9) <- shape_spp_9$Species
shape_spp_9 <- data.frame(t(shape_spp_9))
shape_spp_9 <- shape_spp_9[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_9)

subdom_quad10 <- subset(sub_abund, quad_no == "10")
shape_spp_10<- aggregate(subdom_quad10$shape.index, by = list(Species = subdom_quad10$spp_duplicate), FUN = mean)
shape_spp_10 <- as.data.frame(shape_spp_10)
shape_spp_10 <- cbind(quad_no = 10, shape_spp_10)
rownames(shape_spp_10) <- shape_spp_10$Species
shape_spp_10 <- data.frame(t(shape_spp_10))
shape_spp_10 <- shape_spp_10[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_10)

subdom_quad11 <- subset(sub_abund, quad_no == "11")
shape_spp_11<- aggregate(subdom_quad11$shape.index, by = list(Species = subdom_quad11$spp_duplicate), FUN = mean)
shape_spp_11 <- as.data.frame(shape_spp_11)
shape_spp_11 <- cbind(quad_no = 11, shape_spp_11)
rownames(shape_spp_11) <- shape_spp_11$Species
shape_spp_11 <- data.frame(t(shape_spp_11))
shape_spp_11 <- shape_spp_11[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_11)

subdom_quad12 <- subset(sub_abund, quad_no == "12")
shape_spp_12<- aggregate(subdom_quad12$shape.index, by = list(Species = subdom_quad12$spp_duplicate), FUN = mean)
shape_spp_12 <- as.data.frame(shape_spp_12)
shape_spp_12 <- cbind(quad_no = 12, shape_spp_12)
rownames(shape_spp_12) <- shape_spp_12$Species
shape_spp_12 <- data.frame(t(shape_spp_12))
shape_spp_12 <- shape_spp_12[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_12)

subdom_quad13 <- subset(sub_abund, quad_no == "13")
shape_spp_13<- aggregate(subdom_quad13$shape.index, by = list(Species = subdom_quad13$spp_duplicate), FUN = mean)
shape_spp_13 <- as.data.frame(shape_spp_13)
shape_spp_13 <- cbind(quad_no = 13, shape_spp_13)
rownames(shape_spp_13) <- shape_spp_13$Species
shape_spp_13 <- data.frame(t(shape_spp_13))
shape_spp_13 <- shape_spp_13[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_13)

subdom_quad14 <- subset(sub_abund, quad_no == "14")
shape_spp_14<- aggregate(subdom_quad14$shape.index, by = list(Species = subdom_quad14$spp_duplicate), FUN = mean)
shape_spp_14 <- as.data.frame(shape_spp_14)
shape_spp_14 <- cbind(quad_no = 14, shape_spp_14)
rownames(shape_spp_14) <- shape_spp_14$Species
shape_spp_14 <- data.frame(t(shape_spp_14))
shape_spp_14 <- shape_spp_14[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_14)

subdom_quad15 <- subset(sub_abund, quad_no == "15") 
shape_spp_15<- aggregate(subdom_quad15$shape.index, by = list(Species = subdom_quad15$spp_duplicate), FUN = mean)
shape_spp_15 <- as.data.frame(shape_spp_15)
shape_spp_15 <- cbind(quad_no = 15, shape_spp_15)
rownames(shape_spp_15) <- shape_spp_15$Species
shape_spp_15 <- data.frame(t(shape_spp_15))
shape_spp_15 <- shape_spp_15[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_15)

subdom_quad16 <- subset(sub_abund, quad_no == "16")
shape_spp_16<- aggregate(subdom_quad16$shape.index, by = list(Species = subdom_quad16$spp_duplicate), FUN = mean)
shape_spp_16 <- as.data.frame(shape_spp_16)
shape_spp_16 <- cbind(quad_no = 16, shape_spp_16)
rownames(shape_spp_16) <- shape_spp_16$Species
shape_spp_16 <- data.frame(t(shape_spp_16))
shape_spp_16 <- shape_spp_16[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_16)

subdom_quad17 <- subset(sub_abund, quad_no == "17")
shape_spp_17<- aggregate(subdom_quad17$shape.index, by = list(Species = subdom_quad17$spp_duplicate), FUN = mean)
shape_spp_17 <- as.data.frame(shape_spp_17)
shape_spp_17 <- cbind(quad_no = 17, shape_spp_17)
rownames(shape_spp_17) <- shape_spp_17$Species
shape_spp_17 <- data.frame(t(shape_spp_17))
shape_spp_17 <- shape_spp_17[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_17)

subdom_quad18 <- subset(sub_abund, quad_no == "18")
shape_spp_18<- aggregate(subdom_quad18$shape.index, by = list(Species = subdom_quad18$spp_duplicate), FUN = mean)
shape_spp_18 <- as.data.frame(shape_spp_18)
shape_spp_18 <- cbind(quad_no = 18, shape_spp_18)
rownames(shape_spp_18) <- shape_spp_18$Species
shape_spp_18 <- data.frame(t(shape_spp_18))
shape_spp_18 <- shape_spp_18[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_18)

subdom_quad19 <- subset(sub_abund, quad_no == "19")
shape_spp_19<- aggregate(subdom_quad19$shape.index, by = list(Species = subdom_quad19$spp_duplicate), FUN = mean)
shape_spp_19 <- as.data.frame(shape_spp_19)
shape_spp_19 <- cbind(quad_no = 19, shape_spp_19)
rownames(shape_spp_19) <- shape_spp_19$Species
shape_spp_19 <- data.frame(t(shape_spp_19))
shape_spp_19 <- shape_spp_19[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_19)

subdom_quad20 <- subset(sub_abund, quad_no == "20")
shape_spp_20<- aggregate(subdom_quad20$shape.index, by = list(Species = subdom_quad20$spp_duplicate), FUN = mean)
shape_spp_20 <- as.data.frame(shape_spp_20)
shape_spp_20 <- cbind(quad_no = 20, shape_spp_20)
rownames(shape_spp_20) <- shape_spp_20$Species
shape_spp_20 <- data.frame(t(shape_spp_20))
shape_spp_20 <- shape_spp_20[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_20)

subdom_quad21 <- subset(sub_abund, quad_no == "21")
shape_spp_21<- aggregate(subdom_quad21$shape.index, by = list(Species = subdom_quad21$spp_duplicate), FUN = mean)
shape_spp_21 <- as.data.frame(shape_spp_21)
shape_spp_21 <- cbind(quad_no = 21, shape_spp_21)
rownames(shape_spp_21) <- shape_spp_21$Species
shape_spp_21 <- data.frame(t(shape_spp_21))
shape_spp_21 <- shape_spp_21[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_21)

subdom_quad22 <- subset(sub_abund, quad_no == "22")
shape_spp_22<- aggregate(subdom_quad22$shape.index, by = list(Species = subdom_quad22$spp_duplicate), FUN = mean)
shape_spp_22 <- as.data.frame(shape_spp_22)
shape_spp_22 <- cbind(quad_no = 22, shape_spp_22)
rownames(shape_spp_22) <- shape_spp_22$Species
shape_spp_22 <- data.frame(t(shape_spp_22))
shape_spp_22 <- shape_spp_22[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_22)

subdom_quad23 <- subset(sub_abund, quad_no == "23")
shape_spp_23<- aggregate(subdom_quad23$shape.index, by = list(Species = subdom_quad23$spp_duplicate), FUN = mean)
shape_spp_23 <- as.data.frame(shape_spp_23)
shape_spp_23 <- cbind(quad_no = 23, shape_spp_23)
rownames(shape_spp_23) <- shape_spp_23$Species
shape_spp_23 <- data.frame(t(shape_spp_23))
shape_spp_23 <- shape_spp_23[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_23)

subdom_quad24 <- subset(sub_abund, quad_no == "24")
shape_spp_24<- aggregate(subdom_quad24$shape.index, by = list(Species = subdom_quad24$spp_duplicate), FUN = mean)
shape_spp_24 <- as.data.frame(shape_spp_24)
shape_spp_24 <- cbind(quad_no = 24, shape_spp_24)
rownames(shape_spp_24) <- shape_spp_24$Species
shape_spp_24 <- data.frame(t(shape_spp_24))
shape_spp_24 <- shape_spp_24[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_24)

subdom_quad25 <- subset(sub_abund, quad_no == "25")
shape_spp_25<- aggregate(subdom_quad25$shape.index, by = list(Species = subdom_quad25$spp_duplicate), FUN = mean)
shape_spp_25 <- as.data.frame(shape_spp_25)
shape_spp_25 <- cbind(quad_no = 25, shape_spp_25)
rownames(shape_spp_25) <- shape_spp_25$Species
shape_spp_25 <- data.frame(t(shape_spp_25))
shape_spp_25 <- shape_spp_25[-1,]                           
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_25)

subdom_quad26 <- subset(sub_abund, quad_no == "26")
shape_spp_26<- aggregate(subdom_quad26$shape.index, by = list(Species = subdom_quad26$spp_duplicate), FUN = mean)
shape_spp_26 <- as.data.frame(shape_spp_26)
shape_spp_26 <- cbind(quad_no = 26, shape_spp_26)
rownames(shape_spp_26) <- shape_spp_26$Species
shape_spp_26 <- data.frame(t(shape_spp_26))
shape_spp_26 <- shape_spp_26[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_26)

subdom_quad27 <- subset(sub_abund, quad_no == "27")
shape_spp_27<- aggregate(subdom_quad27$shape.index, by = list(Species = subdom_quad27$spp_duplicate), FUN = mean)
shape_spp_27 <- as.data.frame(shape_spp_27)
shape_spp_27 <- cbind(quad_no = 27, shape_spp_27)
rownames(shape_spp_27) <- shape_spp_27$Species
shape_spp_27 <- data.frame(t(shape_spp_27))
shape_spp_27 <- shape_spp_27[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_27)

subdom_quad_28 <- subset(sub_abund, quad_no == "28")
shape_spp_28<- aggregate(subdom_quad_28$shape.index, by = list(Species = subdom_quad_28$spp_duplicate), FUN = mean)
shape_spp_28 <- as.data.frame(shape_spp_28)
shape_spp_28 <- cbind(quad_no = 28, shape_spp_28)
rownames(shape_spp_28) <- shape_spp_28$Species
shape_spp_28 <- data.frame(t(shape_spp_28))
shape_spp_28 <- shape_spp_28[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_28)

subdom_quad29 <- subset(sub_abund, quad_no == "29")
shape_spp_29<- aggregate(subdom_quad29$shape.index, by = list(Species = subdom_quad29$spp_duplicate), FUN = mean)
shape_spp_29 <- as.data.frame(shape_spp_29)
shape_spp_29 <- cbind(quad_no = 29, shape_spp_29)
rownames(shape_spp_29) <- shape_spp_29$Species
shape_spp_29 <- data.frame(t(shape_spp_29))
shape_spp_29 <- shape_spp_29[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_29)

subdom_quad30 <- subset(sub_abund, quad_no == "30")
shape_spp_30<- aggregate(subdom_quad30$shape.index, by = list(Species = subdom_quad30$spp_duplicate), FUN = mean)
shape_spp_30 <- as.data.frame(shape_spp_30)
shape_spp_30 <- cbind(quad_no = 30, shape_spp_30)
rownames(shape_spp_30) <- shape_spp_30$Species
shape_spp_30 <- data.frame(t(shape_spp_30))
shape_spp_30 <- shape_spp_30[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_30)

subdom_quad31 <- subset(sub_abund, quad_no == "31")
shape_spp_31<- aggregate(subdom_quad31$shape.index, by = list(Species = subdom_quad31$spp_duplicate), FUN = mean)
shape_spp_31 <- as.data.frame(shape_spp_31)
shape_spp_31 <- cbind(quad_no = 31, shape_spp_31)
rownames(shape_spp_31) <- shape_spp_31$Species
shape_spp_31 <- data.frame(t(shape_spp_31))
shape_spp_31 <- shape_spp_31[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_31)

subdom_quad32 <- subset(sub_abund, quad_no == "32")
subdom_quad32 <- read.csv("Results/Patch_Stats/Individual_quadrats/Q32_DOMINANT.csv")
subdom_quad32 <- subset(subdom_quad32, patchID > 0) ## to quickly fix it
shape_spp_32<- aggregate(subdom_quad32$shape.index, by = list(Species = subdom_quad32$spp_duplicate), FUN = mean)
shape_spp_32 <- as.data.frame(shape_spp_32)
shape_spp_32 <- cbind(quad_no = 32, shape_spp_32)
rownames(shape_spp_32) <- shape_spp_32$Species
shape_spp_32 <- data.frame(t(shape_spp_32))
shape_spp_32 <- shape_spp_32[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_32)

subdom_quad33 <- subset(sub_abund, quad_no == "33")
shape_spp_33<- aggregate(subdom_quad33$shape.index, by = list(Species = subdom_quad33$spp_duplicate), FUN = mean)
shape_spp_33 <- as.data.frame(shape_spp_33)
shape_spp_33 <- cbind(quad_no = 33, shape_spp_33)
rownames(shape_spp_33) <- shape_spp_33$Species
shape_spp_33 <- data.frame(t(shape_spp_33))
shape_spp_33 <- shape_spp_33[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_33)

subdom_quad34 <- subset(sub_abund, quad_no == "34")
shape_spp_34<- aggregate(subdom_quad34$shape.index, by = list(Species = subdom_quad34$spp_duplicate), FUN = mean)
shape_spp_34 <- as.data.frame(shape_spp_34)
shape_spp_34 <- cbind(quad_no = 34, shape_spp_34)
rownames(shape_spp_34) <- shape_spp_34$Species
shape_spp_34 <- data.frame(t(shape_spp_34))
shape_spp_34 <- shape_spp_34[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_34)

subdom_quad35 <- subset(sub_abund, quad_no == "35")
shape_spp_35<- aggregate(subdom_quad35$shape.index, by = list(Species = subdom_quad35$spp_duplicate), FUN = mean)
shape_spp_35 <- as.data.frame(shape_spp_35)
shape_spp_35 <- cbind(quad_no = 35, shape_spp_35)
rownames(shape_spp_35) <- shape_spp_35$Species
shape_spp_35 <- data.frame(t(shape_spp_35))
shape_spp_35 <- shape_spp_35[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_35)

subdom_quad36 <- subset(sub_abund, quad_no == "36")
shape_spp_36<- aggregate(subdom_quad36$shape.index, by = list(Species = subdom_quad36$spp_duplicate), FUN = mean)
shape_spp_36 <- as.data.frame(shape_spp_36)
shape_spp_36 <- cbind(quad_no = 36, shape_spp_36)
rownames(shape_spp_36) <- shape_spp_36$Species
shape_spp_36 <- shape_spp_36[-1,]
shape_spp_36 <- data.frame(t(shape_spp_36))
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_36)

subdom_quad37 <- subset(sub_abund, quad_no == "37")
shape_spp_37<- aggregate(subdom_quad37$shape.index, by = list(Species = subdom_quad37$spp_duplicate), FUN = mean)
shape_spp_37 <- as.data.frame(shape_spp_37)
shape_spp_37 <- cbind(quad_no = 37, shape_spp_37)
rownames(shape_spp_37) <- shape_spp_37$Species
shape_spp_37 <- data.frame(t(shape_spp_37))
shape_spp_37 <- shape_spp_37[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_37)

subdom_quad38 <- subset(sub_abund, quad_no == "38")
shape_spp_38<- aggregate(subdom_quad38$shape.index, by = list(Species = subdom_quad38$spp_duplicate), FUN = mean)
shape_spp_38 <- as.data.frame(shape_spp_38)
shape_spp_38 <- cbind(quad_no = 38, shape_spp_38)
rownames(shape_spp_38) <- shape_spp_38$Species
shape_spp_38 <- data.frame(t(shape_spp_38))
shape_spp_38 <- shape_spp_38[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_38)

subdom_quad39 <- subset(sub_abund, quad_no == "39")
shape_spp_39<- aggregate(subdom_quad39$shape.index, by = list(Species = subdom_quad39$spp_duplicate), FUN = mean)
shape_spp_39 <- as.data.frame(shape_spp_39)
shape_spp_39 <- cbind(quad_no = 39, shape_spp_39)
rownames(shape_spp_39) <- shape_spp_39$Species
shape_spp_39 <- data.frame(t(shape_spp_39))
shape_spp_39 <- shape_spp_39[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_39)

subdom_quad40 <- subset(sub_abund, quad_no == "40")
shape_spp_40<- aggregate(subdom_quad40$shape.index, by = list(Species = subdom_quad40$spp_duplicate), FUN = mean)
shape_spp_40 <- as.data.frame(shape_spp_40)
shape_spp_40 <- cbind(quad_no = 40, shape_spp_40)
rownames(shape_spp_40) <- shape_spp_40$Species
shape_spp_40 <- data.frame(t(shape_spp_40))
shape_spp_40 <- shape_spp_40[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_40)

subdom_quad41 <- subset(sub_abund, quad_no == "41")
shape_spp_41<- aggregate(subdom_quad41$shape.index, by = list(Species = subdom_quad41$spp_duplicate), FUN = mean)
shape_spp_41 <- as.data.frame(shape_spp_41)
shape_spp_41 <- cbind(quad_no = 41, shape_spp_41)
rownames(shape_spp_41) <- shape_spp_41$Species
shape_spp_41 <- data.frame(t(shape_spp_41))
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_41)

subdom_quad42 <- subset(sub_abund, quad_no == "42")
shape_spp_42<- aggregate(subdom_quad42$shape.index, by = list(Species = subdom_quad42$spp_duplicate), FUN = mean)
shape_spp_42 <- as.data.frame(shape_spp_42)
shape_spp_42 <- cbind(quad_no = 42, shape_spp_42)
rownames(shape_spp_42) <- shape_spp_42$Species
shape_spp_42 <- data.frame(t(shape_spp_42))
shape_spp_41 <- shape_spp_41[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_42)

subdom_quad43 <- subset(sub_abund, quad_no == "43")
shape_spp_43<- aggregate(subdom_quad43$shape.index, by = list(Species = subdom_quad43$spp_duplicate), FUN = mean)
shape_spp_43 <- as.data.frame(shape_spp_43)
shape_spp_43 <- cbind(quad_no = 43, shape_spp_43)
rownames(shape_spp_43) <- shape_spp_43$Species
shape_spp_43 <- data.frame(t(shape_spp_43))
shape_spp_43 <- shape_spp_43[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_43)

subdom_quad44 <- subset(sub_abund, quad_no == "44")
shape_spp_44<- aggregate(subdom_quad44$shape.index, by = list(Species = subdom_quad44$spp_duplicate), FUN = mean)
shape_spp_44 <- as.data.frame(shape_spp_44)
shape_spp_44 <- cbind(quad_no = 44, shape_spp_44)
rownames(shape_spp_44) <- shape_spp_44$Species
shape_spp_44 <- data.frame(t(shape_spp_44))
shape_spp_44 <- shape_spp_44[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_44)

subdom_quad45 <- subset(sub_abund, quad_no == "45")
shape_spp_45<- aggregate(subdom_quad45$shape.index, by = list(Species = subdom_quad45$spp_duplicate), FUN = mean)
shape_spp_45 <- as.data.frame(shape_spp_45)
shape_spp_45 <- cbind(quad_no = 45, shape_spp_45)
rownames(shape_spp_45) <- shape_spp_45$Species
shape_spp_45 <- data.frame(t(shape_spp_45))
shape_spp_45 <- shape_spp_45[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_45)

subdom_quad46 <- subset(sub_abund, quad_no == "46")
shape_spp_46<- aggregate(subdom_quad46$shape.index, by = list(Species = subdom_quad46$spp_duplicate), FUN = mean)
shape_spp_46 <- as.data.frame(shape_spp_46)
shape_spp_46 <- cbind(quad_no = 46, shape_spp_46)
rownames(shape_spp_46) <- shape_spp_46$Species
shape_spp_46 <- data.frame(t(shape_spp_46))
shape_spp_46 <- shape_spp_46[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_46)

subdom_quad47 <- subset(sub_abund, quad_no == "47")
shape_spp_47<- aggregate(subdom_quad47$shape.index, by = list(Species = subdom_quad47$spp_duplicate), FUN = mean)
shape_spp_47 <- as.data.frame(shape_spp_47)
shape_spp_47 <- cbind(quad_no = 47, shape_spp_47)
rownames(shape_spp_47) <- shape_spp_47$Species
shape_spp_47 <- data.frame(t(shape_spp_47))
shape_spp_47 <- shape_spp_47[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_47)

subdom_quad48 <- subset(sub_abund, quad_no == "48")
shape_spp_48<- aggregate(subdom_quad48$shape.index, by = list(Species = subdom_quad48$spp_duplicate), FUN = mean)
shape_spp_48 <- as.data.frame(shape_spp_48)
shape_spp_48 <- cbind(quad_no = 48, shape_spp_48)
rownames(shape_spp_48) <- shape_spp_48$Species
shape_spp_48 <- data.frame(t(shape_spp_48))
shape_spp_48 <- shape_spp_48[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_48)

subdom_quad49 <- subset(sub_abund, quad_no == "49")
shape_spp_49<- aggregate(subdom_quad49$shape.index, by = list(Species = subdom_quad49$spp_duplicate), FUN = mean)
shape_spp_49 <- as.data.frame(shape_spp_49)
shape_spp_49 <- cbind(quad_no = 49, shape_spp_49)
rownames(shape_spp_49) <- shape_spp_49$Species
shape_spp_49 <- data.frame(t(shape_spp_49))
shape_spp_49 <- shape_spp_49[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_49)

subdom_quad50 <- subset(sub_abund, quad_no == "50")
shape_spp_50<- aggregate(subdom_quad50$shape.index, by = list(Species = subdom_quad50$spp_duplicate), FUN = mean)
shape_spp_50 <- as.data.frame(shape_spp_50)
shape_spp_50 <- cbind(quad_no = 50, shape_spp_50)
rownames(shape_spp_50) <- shape_spp_50$Species
shape_spp_50 <- data.frame(t(shape_spp_50))
shape_spp_50 <- shape_spp_50[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_50)

subdom_quad51 <- subset(sub_abund, quad_no == "51")
shape_spp_51<- aggregate(subdom_quad51$shape.index, by = list(Species = subdom_quad51$spp_duplicate), FUN = mean)
shape_spp_51 <- as.data.frame(shape_spp_51)
shape_spp_51 <- cbind(quad_no = 51, shape_spp_51)
rownames(shape_spp_51) <- shape_spp_51$Species
shape_spp_51 <- data.frame(t(shape_spp_51))
shape_spp_51 <- shape_spp_51[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_51)

subdom_quad52 <- subset(sub_abund, quad_no == "52")
shape_spp_52<- aggregate(subdom_quad52$shape.index, by = list(Species = subdom_quad52$spp_duplicate), FUN = mean)
shape_spp_52 <- as.data.frame(shape_spp_52)
shape_spp_52 <- cbind(quad_no = 52, shape_spp_52)
rownames(shape_spp_52) <- shape_spp_52$Species
shape_spp_52 <- data.frame(t(shape_spp_52))
shape_spp_52 <- shape_spp_52[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_52)

subdom_quad53 <- subset(sub_abund, quad_no == "53")
shape_spp_53<- aggregate(subdom_quad53$shape.index, by = list(Species = subdom_quad53$spp_duplicate), FUN = mean)
shape_spp_53 <- as.data.frame(shape_spp_53)
shape_spp_53 <- cbind(quad_no = 53, shape_spp_53)
rownames(shape_spp_53) <- shape_spp_53$Species
shape_spp_53 <- data.frame(t(shape_spp_53))
shape_spp_53 <- shape_spp_53[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_53)

subdom_quad54 <- subset(sub_abund, quad_no == "54")
shape_spp_54<- aggregate(subdom_quad54$shape.index, by = list(Species = subdom_quad54$spp_duplicate), FUN = mean)
shape_spp_54 <- as.data.frame(shape_spp_54)
shape_spp_54 <- cbind(quad_no = 54, shape_spp_54)
rownames(shape_spp_54) <- shape_spp_54$Species
shape_spp_54 <- data.frame(t(shape_spp_54))
shape_spp_54 <- shape_spp_54[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_54)

subdom_quad55 <- subset(sub_abund, quad_no == "55")
shape_spp_55<- aggregate(subdom_quad55$shape.index, by = list(Species = subdom_quad55$spp_duplicate), FUN = mean)
shape_spp_55 <- as.data.frame(shape_spp_55)
shape_spp_55 <- cbind(quad_no = 55, shape_spp_55)
rownames(shape_spp_55) <- shape_spp_55$Species
shape_spp_55 <- data.frame(t(shape_spp_55))
shape_spp_55 <- shape_spp_55[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_55)

subdom_quad56 <- subset(sub_abund, quad_no == "56")
shape_spp_56<- aggregate(subdom_quad56$shape.index, by = list(Species = subdom_quad56$spp_duplicate), FUN = mean)
shape_spp_56 <- as.data.frame(shape_spp_56)
shape_spp_56 <- cbind(quad_no = 56, shape_spp_56)
rownames(shape_spp_56) <- shape_spp_56$Species
shape_spp_56 <- data.frame(t(shape_spp_56))
shape_spp_56 <- shape_spp_56[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_56)

subdom_quad57 <- subset(sub_abund, quad_no == "57")
shape_spp_57<- aggregate(subdom_quad57$shape.index, by = list(Species = subdom_quad57$spp_duplicate), FUN = mean)
shape_spp_57 <- as.data.frame(shape_spp_57)
shape_spp_57 <- cbind(quad_no = 57, shape_spp_57)
rownames(shape_spp_57) <- shape_spp_57$Species
shape_spp_57 <- data.frame(t(shape_spp_57))
shape_spp_57 <- shape_spp_57[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_57)

subdom_quad58 <- subset(sub_abund, quad_no == "58")
shape_spp_58<- aggregate(subdom_quad58$shape.index, by = list(Species = subdom_quad58$spp_duplicate), FUN = mean)
shape_spp_58 <- as.data.frame(shape_spp_58)
shape_spp_58 <- cbind(quad_no = 58, shape_spp_58)
rownames(shape_spp_58) <- shape_spp_58$Species
shape_spp_58 <- data.frame(t(shape_spp_58))
shape_spp_58 <- shape_spp_58[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_58)

subdom_quad59 <- subset(sub_abund, quad_no == "59")
shape_spp_59<- aggregate(subdom_quad59$shape.index, by = list(Species = subdom_quad59$spp_duplicate), FUN = mean)
shape_spp_59 <- as.data.frame(shape_spp_59)
shape_spp_59 <- cbind(quad_no = 59, shape_spp_59)
rownames(shape_spp_59) <- shape_spp_59$Species
shape_spp_59 <- data.frame(t(shape_spp_59))
shape_spp_59 <- shape_spp_59[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_59)

subdom_quad60 <- subset(sub_abund, quad_no == "60")
shape_spp_60<- aggregate(subdom_quad60$shape.index, by = list(Species = subdom_quad60$spp_duplicate), FUN = mean)
shape_spp_60 <- as.data.frame(shape_spp_60)
shape_spp_60 <- cbind(quad_no = 60, shape_spp_60)
rownames(shape_spp_60) <- shape_spp_60$Species
shape_spp_60 <- data.frame(t(shape_spp_60))
shape_spp_60 <- shape_spp_60[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_60)

subdom_quad61 <- subset(sub_abund, quad_no == "61")
shape_spp_61<- aggregate(subdom_quad61$shape.index, by = list(Species = subdom_quad61$spp_duplicate), FUN = mean)
shape_spp_61 <- as.data.frame(shape_spp_61)
shape_spp_61 <- cbind(quad_no = 61, shape_spp_61)
rownames(shape_spp_61) <- shape_spp_61$Species
shape_spp_61 <- data.frame(t(shape_spp_61))
shape_spp_61 <- shape_spp_61[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_61)

subdom_quad62 <- subset(sub_abund, quad_no == "62")
shape_spp_62<- aggregate(subdom_quad62$shape.index, by = list(Species = subdom_quad62$spp_duplicate), FUN = mean)
shape_spp_62 <- as.data.frame(shape_spp_62)
shape_spp_62 <- cbind(quad_no = 62, shape_spp_62)
rownames(shape_spp_62) <- shape_spp_62$Species
shape_spp_62 <- data.frame(t(shape_spp_62))
shape_spp_62 <- shape_spp_62[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_62)

subdom_quad63 <- subset(sub_abund, quad_no == "63")
shape_spp_63<- aggregate(subdom_quad63$shape.index, by = list(Species = subdom_quad63$spp_duplicate), FUN = mean)
shape_spp_63 <- as.data.frame(shape_spp_63)
shape_spp_63 <- cbind(quad_no = 63, shape_spp_63)
rownames(shape_spp_63) <- shape_spp_63$Species
shape_spp_63 <- data.frame(t(shape_spp_63))
shape_spp_63 <- shape_spp_63[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_63)

subdom_quad64 <- subset(sub_abund, quad_no == "64")
shape_spp_64<- aggregate(subdom_quad64$shape.index, by = list(Species = subdom_quad64$spp_duplicate), FUN = mean)
shape_spp_64 <- as.data.frame(shape_spp_64)
shape_spp_64 <- cbind(quad_no = 64, shape_spp_64)
rownames(shape_spp_64) <- shape_spp_64$Species
shape_spp_64 <- data.frame(t(shape_spp_64))
shape_spp_64 <- shape_spp_64[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_64)

subdom_quad65 <- subset(sub_abund, quad_no == "65")
shape_spp_65<- aggregate(subdom_quad65$shape.index, by = list(Species = subdom_quad65$spp_duplicate), FUN = mean)
shape_spp_65 <- as.data.frame(shape_spp_65)
shape_spp_65 <- cbind(quad_no = 65, shape_spp_65)
rownames(shape_spp_65) <- shape_spp_65$Species
shape_spp_65 <- data.frame(t(shape_spp_65))
shape_spp_65 <- shape_spp_65[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_65)

subdom_quad66 <- subset(sub_abund, quad_no == "66")
shape_spp_66<- aggregate(subdom_quad66$shape.index, by = list(Species = subdom_quad66$spp_duplicate), FUN = mean)
shape_spp_66 <- as.data.frame(shape_spp_66)
shape_spp_66 <- cbind(quad_no = 66, shape_spp_66)
rownames(shape_spp_66) <- shape_spp_66$Species
shape_spp_66 <- data.frame(t(shape_spp_66))
shape_spp_66 <- shape_spp_66[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_66)

subdom_quad67 <- subset(sub_abund, quad_no == "67")
shape_spp_67<- aggregate(subdom_quad67$shape.index, by = list(Species = subdom_quad67$spp_duplicate), FUN = mean)
shape_spp_67 <- as.data.frame(shape_spp_67)
shape_spp_67 <- cbind(quad_no = 67, shape_spp_67)
rownames(shape_spp_67) <- shape_spp_67$Species
shape_spp_67 <- data.frame(t(shape_spp_67))
shape_spp_67 <- shape_spp_67[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_67)

subdom_quad68 <- subset(sub_abund, quad_no == "68")
shape_spp_68<- aggregate(subdom_quad68$shape.index, by = list(Species = subdom_quad68$spp_duplicate), FUN = mean)
shape_spp_68 <- as.data.frame(shape_spp_68)
shape_spp_68 <- cbind(quad_no = 68, shape_spp_68)
rownames(shape_spp_68) <- shape_spp_68$Species
shape_spp_68 <- data.frame(t(shape_spp_68))
shape_spp_68 <- shape_spp_68[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_68)

subdom_quad69 <- subset(sub_abund, quad_no == "69")
shape_spp_69<- aggregate(subdom_quad69$shape.index, by = list(Species = subdom_quad69$spp_duplicate), FUN = mean)
shape_spp_69 <- as.data.frame(shape_spp_69)
shape_spp_69 <- cbind(quad_no = 69, shape_spp_69)
rownames(shape_spp_69) <- shape_spp_69$Species
shape_spp_69 <- data.frame(t(shape_spp_69))
shape_spp_69 <- shape_spp_69[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_69)

subdom_quad70 <- subset(sub_abund, quad_no == "70")
shape_spp_70<- aggregate(subdom_quad70$shape.index, by = list(Species = subdom_quad70$spp_duplicate), FUN = mean)
shape_spp_70 <- as.data.frame(shape_spp_70)
shape_spp_70 <- cbind(quad_no = 70, shape_spp_70)
rownames(shape_spp_70) <- shape_spp_70$Species
shape_spp_70 <- data.frame(t(shape_spp_70))
shape_spp_70 <- shape_spp_70[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_70)

subdom_quad71 <- subset(sub_abund, quad_no == "71")
shape_spp_71<- aggregate(subdom_quad71$shape.index, by = list(Species = subdom_quad71$spp_duplicate), FUN = mean)
shape_spp_71 <- as.data.frame(shape_spp_71)
shape_spp_71 <- cbind(quad_no = 71, shape_spp_71)
rownames(shape_spp_71) <- shape_spp_71$Species
shape_spp_71 <- data.frame(t(shape_spp_71))
shape_spp_71 <- shape_spp_71[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_71)

subdom_quad72 <- subset(sub_abund, quad_no == "72")
shape_spp_72<- aggregate(subdom_quad72$shape.index, by = list(Species = subdom_quad72$spp_duplicate), FUN = mean)
shape_spp_72 <- as.data.frame(shape_spp_72)
shape_spp_72 <- cbind(quad_no = 72, shape_spp_72)
rownames(shape_spp_72) <- shape_spp_72$Species
shape_spp_72 <- data.frame(t(shape_spp_72))
shape_spp_72 <- shape_spp_72[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_72)

subdom_quad73 <- subset(sub_abund, quad_no == "73")
shape_spp_73<- aggregate(subdom_quad73$shape.index, by = list(Species = subdom_quad73$spp_duplicate), FUN = mean)
shape_spp_73 <- as.data.frame(shape_spp_73)
shape_spp_73 <- cbind(quad_no = 73, shape_spp_73)
rownames(shape_spp_73) <- shape_spp_73$Species
shape_spp_73 <- data.frame(t(shape_spp_73))
shape_spp_73 <- shape_spp_73[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_73)

subdom_quad74 <- subset(sub_abund, quad_no == "74")
shape_spp_74<- aggregate(subdom_quad74$shape.index, by = list(Species = subdom_quad74$spp_duplicate), FUN = mean)
shape_spp_74 <- as.data.frame(shape_spp_74)
shape_spp_74 <- cbind(quad_no = 74, shape_spp_74)
rownames(shape_spp_74) <- shape_spp_74$Species
shape_spp_74 <- data.frame(t(shape_spp_74))
shape_spp_74 <- shape_spp_74[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_74)

subdom_quad75 <- subset(sub_abund, quad_no == "75")
shape_spp_75<- aggregate(subdom_quad75$shape.index, by = list(Species = subdom_quad75$spp_duplicate), FUN = mean)
shape_spp_75 <- as.data.frame(shape_spp_75)
shape_spp_75 <- cbind(quad_no = 75, shape_spp_75)
rownames(shape_spp_75) <- shape_spp_75$Species
shape_spp_75 <- data.frame(t(shape_spp_75))
shape_spp_75 <- shape_spp_75[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_75)

subdom_quad76 <- subset(sub_abund, quad_no == "76")
shape_spp_76<- aggregate(subdom_quad76$shape.index, by = list(Species = subdom_quad76$spp_duplicate), FUN = mean)
shape_spp_76 <- as.data.frame(shape_spp_76)
shape_spp_76 <- cbind(quad_no = 76, shape_spp_76)
rownames(shape_spp_76) <- shape_spp_76$Species
shape_spp_76 <- data.frame(t(shape_spp_76))
shape_spp_76 <- shape_spp_76[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_76)

subdom_quad77 <- subset(sub_abund, quad_no == "77")
shape_spp_77<- aggregate(subdom_quad77$shape.index, by = list(Species = subdom_quad77$spp_duplicate), FUN = mean)
shape_spp_77 <- as.data.frame(shape_spp_77)
shape_spp_77 <- cbind(quad_no = 77, shape_spp_77)
rownames(shape_spp_77) <- shape_spp_77$Species
shape_spp_77 <- data.frame(t(shape_spp_77))
shape_spp_77 <- shape_spp_77[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_77)

subdom_quad78 <- subset(sub_abund, quad_no == "78")
subdom_quad78 <- read.csv("Results/Patch_Stats/Individual_quadrats/Q78_DOMINANT.csv")
subdom_quad78 <- subset(subdom_quad78, patchID > 0)
shape_spp_78<- aggregate(subdom_quad78$shape.index, by = list(Species = subdom_quad78$spp_duplicate), FUN = mean)
shape_spp_78 <- as.data.frame(shape_spp_78)
shape_spp_78 <- cbind(quad_no = 78, shape_spp_78)
rownames(shape_spp_78) <- shape_spp_78$Species
shape_spp_78 <- data.frame(t(shape_spp_78))
shape_spp_78 <- shape_spp_78[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_78)

subdom_quad79 <- subset(sub_abund, quad_no == "79")
shape_spp_79<- aggregate(subdom_quad79$shape.index, by = list(Species = subdom_quad79$spp_duplicate), FUN = mean)
shape_spp_79 <- as.data.frame(shape_spp_79)
shape_spp_79 <- cbind(quad_no = 79, shape_spp_79)
rownames(shape_spp_79) <- shape_spp_79$Species
shape_spp_79 <- data.frame(t(shape_spp_79))
shape_spp_79 <- shape_spp_79[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_79)

subdom_quad80 <- subset(sub_abund, quad_no == "80")
shape_spp_80<- aggregate(subdom_quad80$shape.index, by = list(Species = subdom_quad80$spp_duplicate), FUN = mean)
shape_spp_80 <- as.data.frame(shape_spp_80)
shape_spp_80 <- cbind(quad_no = 80, shape_spp_80)
rownames(shape_spp_80) <- shape_spp_80$Species
shape_spp_80 <- data.frame(t(shape_spp_80))
shape_spp_80 <- shape_spp_80[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_80)

subdom_quad81 <- subset(sub_abund, quad_no == "81")
shape_spp_81<- aggregate(subdom_quad81$shape.index, by = list(Species = subdom_quad81$spp_duplicate), FUN = mean)
shape_spp_81 <- as.data.frame(shape_spp_81)
shape_spp_81 <- cbind(quad_no = 81, shape_spp_81)
rownames(shape_spp_81) <- shape_spp_81$Species
shape_spp_81 <- data.frame(t(shape_spp_81))
shape_spp_81 <- shape_spp_81[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_81)

subdom_quad82 <- subset(sub_abund, quad_no == "82")
shape_spp_82<- aggregate(subdom_quad82$shape.index, by = list(Species = subdom_quad82$spp_duplicate), FUN = mean)
shape_spp_82 <- as.data.frame(shape_spp_82)
shape_spp_82 <- cbind(quad_no = 82, shape_spp_82)
rownames(shape_spp_82) <- shape_spp_82$Species
shape_spp_82 <- data.frame(t(shape_spp_82))
shape_spp_82 <- shape_spp_82[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_82)

subdom_quad83 <- subset(sub_abund, quad_no == "83")
shape_spp_83<- aggregate(subdom_quad83$shape.index, by = list(Species = subdom_quad83$spp_duplicate), FUN = mean)
shape_spp_83 <- as.data.frame(shape_spp_83)
shape_spp_83 <- cbind(quad_no = 83, shape_spp_83)
rownames(shape_spp_83) <- shape_spp_83$Species
shape_spp_83 <- data.frame(t(shape_spp_83))
shape_spp_83 <- shape_spp_83[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_83)

subdom_quad84 <- subset(sub_abund, quad_no == "84")
shape_spp_84<- aggregate(subdom_quad84$shape.index, by = list(Species = subdom_quad84$spp_duplicate), FUN = mean)
shape_spp_84 <- as.data.frame(shape_spp_84)
shape_spp_84 <- cbind(quad_no = 84, shape_spp_84)
rownames(shape_spp_84) <- shape_spp_84$Species
shape_spp_84 <- data.frame(t(shape_spp_84))
shape_spp_84 <- shape_spp_84[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_84)

subdom_quad85 <- subset(sub_abund, quad_no == "85")
shape_spp_85<- aggregate(subdom_quad85$shape.index, by = list(Species = subdom_quad85$spp_duplicate), FUN = mean)
shape_spp_85 <- as.data.frame(shape_spp_85)
shape_spp_85 <- cbind(quad_no = 85, shape_spp_85)
rownames(shape_spp_85) <- shape_spp_85$Species
shape_spp_85 <- data.frame(t(shape_spp_85))
shape_spp_85 <- shape_spp_85[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_85)

subdom_quad86 <- subset(sub_abund, quad_no == "86")
shape_spp_86<- aggregate(subdom_quad86$shape.index, by = list(Species = subdom_quad86$spp_duplicate), FUN = mean)
shape_spp_86 <- as.data.frame(shape_spp_86)
shape_spp_86 <- cbind(quad_no = 86, shape_spp_86)
rownames(shape_spp_86) <- shape_spp_86$Species
shape_spp_86 <- data.frame(t(shape_spp_86))
shape_spp_86 <- shape_spp_86[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_86)

subdom_quad87 <- subset(sub_abund, quad_no == "87")
shape_spp_87<- aggregate(subdom_quad87$shape.index, by = list(Species = subdom_quad87$spp_duplicate), FUN = mean)
shape_spp_87 <- as.data.frame(shape_spp_87)
shape_spp_87 <- cbind(quad_no = 87, shape_spp_87)
rownames(shape_spp_87) <- shape_spp_87$Species
shape_spp_87 <- data.frame(t(shape_spp_87))
shape_spp_87 <- shape_spp_87[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_87)

subdom_quad88 <- subset(sub_abund, quad_no == "88")
shape_spp_88<- aggregate(subdom_quad88$shape.index, by = list(Species = subdom_quad88$spp_duplicate), FUN = mean)
shape_spp_88 <- as.data.frame(shape_spp_88)
shape_spp_88 <- cbind(quad_no = 88, shape_spp_88)
rownames(shape_spp_88) <- shape_spp_88$Species
shape_spp_88 <- data.frame(t(shape_spp_88))
shape_spp_88 <- shape_spp_88[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_88)

subdom_quad89 <- subset(sub_abund, quad_no == "89")
shape_spp_89<- aggregate(subdom_quad89$shape.index, by = list(Species = subdom_quad89$spp_duplicate), FUN = mean)
shape_spp_89 <- as.data.frame(shape_spp_89)
shape_spp_89 <- cbind(quad_no = 89, shape_spp_89)
rownames(shape_spp_89) <- shape_spp_89$Species
shape_spp_89 <- data.frame(t(shape_spp_89))
shape_spp_89 <- shape_spp_89[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_89)

subdom_quad90 <- subset(sub_abund, quad_no == "90")
shape_spp_90<- aggregate(subdom_quad90$shape.index, by = list(Species = subdom_quad90$spp_duplicate), FUN = mean)
shape_spp_90 <- as.data.frame(shape_spp_90)
shape_spp_90 <- cbind(quad_no = 90, shape_spp_90)
rownames(shape_spp_90) <- shape_spp_90$Species
shape_spp_90 <- data.frame(t(shape_spp_90))
shape_spp_90 <- shape_spp_90[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_90)

subdom_quad91 <- subset(sub_abund, quad_no == "91")
shape_spp_91<- aggregate(subdom_quad91$shape.index, by = list(Species = subdom_quad91$spp_duplicate), FUN = mean)
shape_spp_91 <- as.data.frame(shape_spp_91)
shape_spp_91 <- cbind(quad_no = 91, shape_spp_91)
rownames(shape_spp_91) <- shape_spp_91$Species
shape_spp_91 <- data.frame(t(shape_spp_91))
shape_spp_91 <- shape_spp_91[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_91)

subdom_quad92 <- subset(sub_abund, quad_no == "92")
shape_spp_92<- aggregate(subdom_quad92$shape.index, by = list(Species = subdom_quad92$spp_duplicate), FUN = mean)
shape_spp_92 <- as.data.frame(shape_spp_92)
shape_spp_92 <- cbind(quad_no = 92, shape_spp_92)
rownames(shape_spp_92) <- shape_spp_92$Species
shape_spp_92 <- data.frame(t(shape_spp_92))
shape_spp_92 <- shape_spp_92[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_92)

subdom_quad93 <- subset(sub_abund, quad_no == "93")
shape_spp_93<- aggregate(subdom_quad93$shape.index, by = list(Species = subdom_quad93$spp_duplicate), FUN = mean)
shape_spp_93 <- as.data.frame(shape_spp_93)
shape_spp_93 <- cbind(quad_no = 93, shape_spp_93)
rownames(shape_spp_93) <- shape_spp_93$Species
shape_spp_93 <- data.frame(t(shape_spp_93))
shape_spp_93 <- shape_spp_93[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_93)

subdom_quad94 <- subset(sub_abund, quad_no == "94")
shape_spp_94<- aggregate(subdom_quad94$shape.index, by = list(Species = subdom_quad94$spp_duplicate), FUN = mean)
shape_spp_94 <- as.data.frame(shape_spp_94)
shape_spp_94 <- cbind(quad_no = 94, shape_spp_94)
rownames(shape_spp_94) <- shape_spp_94$Species
shape_spp_94 <- data.frame(t(shape_spp_94))
shape_spp_94 <- shape_spp_94[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_94)

subdom_quad95 <- subset(sub_abund, quad_no == "95")
shape_spp_95<- aggregate(subdom_quad95$shape.index, by = list(Species = subdom_quad95$spp_duplicate), FUN = mean)
shape_spp_95 <- as.data.frame(shape_spp_95)
shape_spp_95 <- cbind(quad_no = 95, shape_spp_95)
rownames(shape_spp_95) <- shape_spp_95$Species
shape_spp_95 <- data.frame(t(shape_spp_95))
shape_spp_95 <- shape_spp_95[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_95)

subdom_quad96 <- subset(sub_abund, quad_no == "96")
shape_spp_96<- aggregate(subdom_quad96$shape.index, by = list(Species = subdom_quad96$spp_duplicate), FUN = mean)
shape_spp_96 <- as.data.frame(shape_spp_96)
shape_spp_96 <- cbind(quad_no = 96, shape_spp_96)
rownames(shape_spp_96) <- shape_spp_96$Species
shape_spp_96 <- data.frame(t(shape_spp_96))
shape_spp_96 <- shape_spp_96[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_96)

subdom_quad97 <- subset(sub_abund, quad_no == "97")
shape_spp_97<- aggregate(subdom_quad97$shape.index, by = list(Species = subdom_quad97$spp_duplicate), FUN = mean)
shape_spp_97 <- as.data.frame(shape_spp_97)
shape_spp_97 <- cbind(quad_no = 97, shape_spp_97)
rownames(shape_spp_97) <- shape_spp_97$Species
shape_spp_97 <- data.frame(t(shape_spp_97))
shape_spp_97 <- shape_spp_97[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_97)

subdom_quad98 <- subset(sub_abund, quad_no == "98")
shape_spp_98<- aggregate(subdom_quad98$shape.index, by = list(Species = subdom_quad98$spp_duplicate), FUN = mean)
shape_spp_98 <- as.data.frame(shape_spp_98)
shape_spp_98 <- cbind(quad_no = 98, shape_spp_98)
rownames(shape_spp_98) <- shape_spp_98$Species
shape_spp_98 <- data.frame(t(shape_spp_98))
shape_spp_98 <- shape_spp_98[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_98)

subdom_quad99 <- subset(sub_abund, quad_no == "99")
shape_spp_99<- aggregate(subdom_quad99$shape.index, by = list(Species = subdom_quad99$spp_duplicate), FUN = mean)
shape_spp_99 <- as.data.frame(shape_spp_99)
shape_spp_99 <- cbind(quad_no = 99, shape_spp_99)
rownames(shape_spp_99) <- shape_spp_99$Species
shape_spp_99 <- data.frame(t(shape_spp_99))
shape_spp_99 <- shape_spp_99[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_99)

subdom_quad100 <- subset(sub_abund, quad_no == "100")
shape_spp_100<- aggregate(subdom_quad100$shape.index, by = list(Species = subdom_quad100$spp_duplicate), FUN = mean)
shape_spp_100 <- as.data.frame(shape_spp_100)
shape_spp_100 <- cbind(quad_no = 100, shape_spp_100)
rownames(shape_spp_100) <- shape_spp_100$Species
shape_spp_100 <- data.frame(t(shape_spp_100))
shape_spp_100 <- shape_spp_100[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_100)

subdom_quad101 <- subset(sub_abund, quad_no == "101")
shape_spp_101<- aggregate(subdom_quad101$shape.index, by = list(Species = subdom_quad101$spp_duplicate), FUN = mean)
shape_spp_101 <- as.data.frame(shape_spp_101)
shape_spp_101 <- cbind(quad_no = 101, shape_spp_101)
rownames(shape_spp_101) <- shape_spp_101$Species
shape_spp_101 <- data.frame(t(shape_spp_101))
shape_spp_101 <- shape_spp_101[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_101)

subdom_quad102 <- subset(sub_abund, quad_no == "102")
shape_spp_102<- aggregate(subdom_quad102$shape.index, by = list(Species = subdom_quad102$spp_duplicate), FUN = mean)
shape_spp_102 <- as.data.frame(shape_spp_102)
shape_spp_102 <- cbind(quad_no = 102, shape_spp_102)
rownames(shape_spp_102) <- shape_spp_102$Species
shape_spp_102 <- data.frame(t(shape_spp_102))
shape_spp_102 <- shape_spp_102[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_102)

subdom_quad103 <- subset(sub_abund, quad_no == "103")
shape_spp_103<- aggregate(subdom_quad103$shape.index, by = list(Species = subdom_quad103$spp_duplicate), FUN = mean)
shape_spp_103 <- as.data.frame(shape_spp_103)
shape_spp_103 <- cbind(quad_no = 103, shape_spp_103)
rownames(shape_spp_103) <- shape_spp_103$Species
shape_spp_103 <- data.frame(t(shape_spp_103))
shape_spp_103 <- shape_spp_103[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_103)

subdom_quad104 <- subset(sub_abund, quad_no == "104")
subdom_quad104 <- read.csv("Results/Patch_Stats/Individual_quadrats/Q104_DOMINANT.csv")
subdom_quad104 <- subset(subdom_quad104, patchID > 0)
shape_spp_104<- aggregate(subdom_quad104$shape.index, by = list(Species = subdom_quad104$spp_duplicate), FUN = mean)
shape_spp_104 <- as.data.frame(shape_spp_104)
shape_spp_104 <- cbind(quad_no = 104, shape_spp_104)
rownames(shape_spp_104) <- shape_spp_104$Species
shape_spp_104 <- data.frame(t(shape_spp_104))
shape_spp_104 <- shape_spp_104[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_104)

subdom_quad105 <- subset(sub_abund, quad_no == "105")
shape_spp_105<- aggregate(subdom_quad105$shape.index, by = list(Species = subdom_quad105$spp_duplicate), FUN = mean)
shape_spp_105 <- as.data.frame(shape_spp_105)
shape_spp_105 <- cbind(quad_no = 105, shape_spp_105)
rownames(shape_spp_105) <- shape_spp_105$Species
shape_spp_105 <- data.frame(t(shape_spp_105))
shape_spp_105 <- shape_spp_105[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_105)

subdom_quad106 <- subset(sub_abund, quad_no == "106")
shape_spp_106<- aggregate(subdom_quad106$shape.index, by = list(Species = subdom_quad106$spp_duplicate), FUN = mean)
shape_spp_106 <- as.data.frame(shape_spp_106)
shape_spp_106 <- cbind(quad_no = 106, shape_spp_106)
rownames(shape_spp_106) <- shape_spp_106$Species
shape_spp_106 <- data.frame(t(shape_spp_106))
shape_spp_106 <- shape_spp_106[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_106)

subdom_quad107 <- subset(sub_abund, quad_no == "107")
shape_spp_107<- aggregate(subdom_quad107$shape.index, by = list(Species = subdom_quad107$spp_duplicate), FUN = mean)
shape_spp_107 <- as.data.frame(shape_spp_107)
shape_spp_107 <- cbind(quad_no = 107, shape_spp_107)
rownames(shape_spp_107) <- shape_spp_107$Species
shape_spp_107 <- data.frame(t(shape_spp_107))
shape_spp_107 <- shape_spp_107[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_107)

subdom_quad108 <- subset(sub_abund, quad_no == "108")
shape_spp_108<- aggregate(subdom_quad108$shape.index, by = list(Species = subdom_quad108$spp_duplicate), FUN = mean)
shape_spp_108 <- as.data.frame(shape_spp_108)
shape_spp_108 <- cbind(quad_no = 108, shape_spp_108)
rownames(shape_spp_108) <- shape_spp_108$Species
shape_spp_108 <- data.frame(t(shape_spp_108))
shape_spp_108 <- shape_spp_108[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_108)

subdom_quad109 <- subset(sub_abund, quad_no == "109")
shape_spp_109<- aggregate(subdom_quad109$shape.index, by = list(Species = subdom_quad109$spp_duplicate), FUN = mean)
shape_spp_109 <- as.data.frame(shape_spp_109)
shape_spp_109 <- cbind(quad_no = 109, shape_spp_109)
rownames(shape_spp_109) <- shape_spp_109$Species
shape_spp_109 <- data.frame(t(shape_spp_109))
shape_spp_109 <- shape_spp_109[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_109)

subdom_quad110 <- subset(sub_abund, quad_no == "110")
shape_spp_110<- aggregate(subdom_quad110$shape.index, by = list(Species = subdom_quad110$spp_duplicate), FUN = mean)
shape_spp_110 <- as.data.frame(shape_spp_110)
shape_spp_110 <- cbind(quad_no = 110, shape_spp_110)
rownames(shape_spp_110) <- shape_spp_110$Species
shape_spp_110 <- data.frame(t(shape_spp_110))
shape_spp_110 <- shape_spp_110[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_110)

subdom_quad111 <- subset(sub_abund, quad_no == "111")
subdom_quad111 <- read.csv("Results/Patch_Stats/Individual_quadrats/Q111_DOMINANT.csv") ## TEMPORARY = TO FIX
subdom_quad111 <- subset(subdom_quad111, patchID > 0)
shape_spp_111<- aggregate(subdom_quad111$shape.index, by = list(Species = subdom_quad111$spp_duplicate), FUN = mean)
shape_spp_111 <- as.data.frame(shape_spp_111)
shape_spp_111 <- cbind(quad_no = 111, shape_spp_111)
rownames(shape_spp_111) <- shape_spp_111$Species
shape_spp_111 <- data.frame(t(shape_spp_111))
shape_spp_111 <- shape_spp_111[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_111)

subdom_quad112 <- subset(sub_abund, quad_no == "112")
shape_spp_112<- aggregate(subdom_quad112$shape.index, by = list(Species = subdom_quad112$spp_duplicate), FUN = mean)
shape_spp_112 <- as.data.frame(shape_spp_112)
shape_spp_112 <- cbind(quad_no = 112, shape_spp_112)
rownames(shape_spp_112) <- shape_spp_112$Species
shape_spp_112 <- data.frame(t(shape_spp_112))
shape_spp_112 <- shape_spp_112[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_112)

subdom_quad113 <- subset(sub_abund, quad_no == "113")
shape_spp_113<- aggregate(subdom_quad113$shape.index, by = list(Species = subdom_quad113$spp_duplicate), FUN = mean)
shape_spp_113 <- as.data.frame(shape_spp_113)
shape_spp_113 <- cbind(quad_no = 113, shape_spp_113)
rownames(shape_spp_113) <- shape_spp_113$Species
shape_spp_113 <- data.frame(t(shape_spp_113))
shape_spp_113 <- shape_spp_113[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_113)

subdom_quad114 <- subset(sub_abund, quad_no == "114")
shape_spp_114<- aggregate(subdom_quad114$shape.index, by = list(Species = subdom_quad114$spp_duplicate), FUN = mean)
shape_spp_114 <- as.data.frame(shape_spp_114)
shape_spp_114 <- cbind(quad_no = 114, shape_spp_114)
rownames(shape_spp_114) <- shape_spp_114$Species
shape_spp_114 <- data.frame(t(shape_spp_114))
shape_spp_114 <- shape_spp_114[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_114)

subdom_quad115 <- subset(sub_abund, quad_no == "115")
shape_spp_115<- aggregate(subdom_quad115$shape.index, by = list(Species = subdom_quad115$spp_duplicate), FUN = mean)
shape_spp_115 <- as.data.frame(shape_spp_115)
shape_spp_115 <- cbind(quad_no = 115, shape_spp_115)
rownames(shape_spp_115) <- shape_spp_115$Species
shape_spp_115 <- data.frame(t(shape_spp_115))
shape_spp_115 <- shape_spp_115[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_115)

subdom_quad116 <- subset(sub_abund, quad_no == "116")
shape_spp_116<- aggregate(subdom_quad116$shape.index, by = list(Species = subdom_quad116$spp_duplicate), FUN = mean)
shape_spp_116 <- as.data.frame(shape_spp_116)
shape_spp_116 <- cbind(quad_no = 116, shape_spp_116)
rownames(shape_spp_116) <- shape_spp_116$Species
shape_spp_116 <- data.frame(t(shape_spp_116))
shape_spp_116 <- shape_spp_116[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_116)

subdom_quad117 <- subset(sub_abund, quad_no == "117")
shape_spp_117<- aggregate(subdom_quad117$shape.index, by = list(Species = subdom_quad117$spp_duplicate), FUN = mean)
shape_spp_117 <- as.data.frame(shape_spp_117)
shape_spp_117 <- cbind(quad_no = 117, shape_spp_117)
rownames(shape_spp_117) <- shape_spp_117$Species
shape_spp_117 <- data.frame(t(shape_spp_117))
shape_spp_117 <- shape_spp_117[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_117)

subdom_quad118 <- subset(sub_abund, quad_no == "118")
shape_spp_118<- aggregate(subdom_quad118$shape.index, by = list(Species = subdom_quad118$spp_duplicate), FUN = mean)
shape_spp_118 <- as.data.frame(shape_spp_118)
shape_spp_118 <- cbind(quad_no = 118, shape_spp_118)
rownames(shape_spp_118) <- shape_spp_118$Species
shape_spp_118 <- data.frame(t(shape_spp_118))
shape_spp_118 <- shape_spp_118[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_118)

subdom_quad119 <- subset(sub_abund, quad_no == "119")
shape_spp_119<- aggregate(subdom_quad119$shape.index, by = list(Species = subdom_quad119$spp_duplicate), FUN = mean)
shape_spp_119 <- as.data.frame(shape_spp_119)
shape_spp_119 <- cbind(quad_no = 119, shape_spp_119)
rownames(shape_spp_119) <- shape_spp_119$Species
shape_spp_119 <- data.frame(t(shape_spp_119))
shape_spp_119 <- shape_spp_119[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_119)

subdom_quad120 <- subset(sub_abund, quad_no == "120")
shape_spp_120<- aggregate(subdom_quad120$shape.index, by = list(Species = subdom_quad120$spp_duplicate), FUN = mean)
shape_spp_120 <- as.data.frame(shape_spp_120)
shape_spp_120 <- cbind(quad_no = 120, shape_spp_120)
rownames(shape_spp_120) <- shape_spp_120$Species
shape_spp_120 <- data.frame(t(shape_spp_120))
shape_spp_120 <- shape_spp_120[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_120)

subdom_quad121 <- subset(sub_abund, quad_no == "121")
shape_spp_121<- aggregate(subdom_quad121$shape.index, by = list(Species = subdom_quad121$spp_duplicate), FUN = mean)
shape_spp_121 <- as.data.frame(shape_spp_121)
shape_spp_121 <- cbind(quad_no = 121, shape_spp_121)
rownames(shape_spp_121) <- shape_spp_121$Species
shape_spp_121 <- data.frame(t(shape_spp_121))
shape_spp_121 <- shape_spp_121[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_121)

subdom_quad122 <- subset(sub_abund, quad_no == "122")
shape_spp_122<- aggregate(subdom_quad122$shape.index, by = list(Species = subdom_quad122$spp_duplicate), FUN = mean)
shape_spp_122 <- as.data.frame(shape_spp_122)
shape_spp_122 <- cbind(quad_no = 122, shape_spp_122)
rownames(shape_spp_122) <- shape_spp_122$Species
shape_spp_122 <- data.frame(t(shape_spp_122))
shape_spp_122 <- shape_spp_122[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_122)

subdom_quad123 <- subset(sub_abund, quad_no == "123")
shape_spp_123<- aggregate(subdom_quad123$shape.index, by = list(Species = subdom_quad123$spp_duplicate), FUN = mean)
shape_spp_123 <- as.data.frame(shape_spp_123)
shape_spp_123 <- cbind(quad_no = 123, shape_spp_123)
rownames(shape_spp_123) <- shape_spp_123$Species
shape_spp_123 <- data.frame(t(shape_spp_123))
shape_spp_123 <- shape_spp_123[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_123)

subdom_quad124 <- subset(sub_abund, quad_no == "124")
shape_spp_124<- aggregate(subdom_quad124$shape.index, by = list(Species = subdom_quad124$spp_duplicate), FUN = mean)
shape_spp_124 <- as.data.frame(shape_spp_124)
shape_spp_124 <- cbind(quad_no = 124, shape_spp_124)
rownames(shape_spp_124) <- shape_spp_124$Species
shape_spp_124 <- data.frame(t(shape_spp_124))
shape_spp_124 <- shape_spp_124[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_124)

subdom_quad125 <- subset(sub_abund, quad_no == "125")
shape_spp_125<- aggregate(subdom_quad125$shape.index, by = list(Species = subdom_quad125$spp_duplicate), FUN = mean)
shape_spp_125 <- as.data.frame(shape_spp_125)
shape_spp_125 <- cbind(quad_no = 125, shape_spp_125)
rownames(shape_spp_125) <- shape_spp_125$Species
shape_spp_125 <- data.frame(t(shape_spp_125))
shape_spp_125 <- shape_spp_125[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_125)

subdom_quad126 <- subset(sub_abund, quad_no == "126")
shape_spp_126<- aggregate(subdom_quad126$shape.index, by = list(Species = subdom_quad126$spp_duplicate), FUN = mean)
shape_spp_126 <- as.data.frame(shape_spp_126)
shape_spp_126 <- cbind(quad_no = 126, shape_spp_126)
rownames(shape_spp_126) <- shape_spp_126$Species
shape_spp_126 <- data.frame(t(shape_spp_126))
shape_spp_126 <- shape_spp_126[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_126)

subdom_quad127 <- subset(sub_abund, quad_no == "127")
shape_spp_127<- aggregate(subdom_quad127$shape.index, by = list(Species = subdom_quad127$spp_duplicate), FUN = mean)
shape_spp_127 <- as.data.frame(shape_spp_127)
shape_spp_127 <- cbind(quad_no = 127, shape_spp_127)
rownames(shape_spp_127) <- shape_spp_127$Species
shape_spp_127 <- data.frame(t(shape_spp_127))
shape_spp_127 <- shape_spp_127[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_127)

subdom_quad128 <- subset(sub_abund, quad_no == "128")
shape_spp_128<- aggregate(subdom_quad128$shape.index, by = list(Species = subdom_quad128$spp_duplicate), FUN = mean)
shape_spp_128 <- as.data.frame(shape_spp_128)
shape_spp_128 <- cbind(quad_no = 128, shape_spp_128)
rownames(shape_spp_128) <- shape_spp_128$Species
shape_spp_128 <- data.frame(t(shape_spp_128))
shape_spp_128 <- shape_spp_128[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_128)

subdom_quad129 <- subset(sub_abund, quad_no == "129")
shape_spp_129<- aggregate(subdom_quad129$shape.index, by = list(Species = subdom_quad129$spp_duplicate), FUN = mean)
shape_spp_129 <- as.data.frame(shape_spp_129)
shape_spp_129 <- cbind(quad_no = 129, shape_spp_129)
rownames(shape_spp_129) <- shape_spp_129$Species
shape_spp_129 <- data.frame(t(shape_spp_129))
shape_spp_129 <- shape_spp_129[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_129)

subdom_quad130 <- subset(sub_abund, quad_no == "130")
shape_spp_130<- aggregate(subdom_quad130$shape.index, by = list(Species = subdom_quad130$spp_duplicate), FUN = mean)
shape_spp_130 <- as.data.frame(shape_spp_130)
shape_spp_130 <- cbind(quad_no = 130, shape_spp_130)
rownames(shape_spp_130) <- shape_spp_130$Species
shape_spp_130 <- data.frame(t(shape_spp_130))
shape_spp_130 <- shape_spp_130[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_130)

subdom_quad131 <- subset(sub_abund, quad_no == "131")
shape_spp_131<- aggregate(subdom_quad131$shape.index, by = list(Species = subdom_quad131$spp_duplicate), FUN = mean)
shape_spp_131 <- as.data.frame(shape_spp_131)
shape_spp_131 <- cbind(quad_no = 131, shape_spp_131)
rownames(shape_spp_131) <- shape_spp_131$Species
shape_spp_131 <- data.frame(t(shape_spp_131))
shape_spp_131 <- shape_spp_131[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_131)

subdom_quad132 <- subset(sub_abund, quad_no == "132")
shape_spp_132<- aggregate(subdom_quad132$shape.index, by = list(Species = subdom_quad132$spp_duplicate), FUN = mean)
shape_spp_132 <- as.data.frame(shape_spp_132)
shape_spp_132 <- cbind(quad_no = 132, shape_spp_132)
rownames(shape_spp_132) <- shape_spp_132$Species
shape_spp_132 <- data.frame(t(shape_spp_132))
shape_spp_132 <- shape_spp_132[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_132)

subdom_quad133 <- subset(sub_abund, quad_no == "133")
shape_spp_133<- aggregate(subdom_quad133$shape.index, by = list(Species = subdom_quad133$spp_duplicate), FUN = mean)
shape_spp_133 <- as.data.frame(shape_spp_133)
shape_spp_133 <- cbind(quad_no = 133, shape_spp_133)
rownames(shape_spp_133) <- shape_spp_133$Species
shape_spp_133 <- data.frame(t(shape_spp_133))
shape_spp_133 <- shape_spp_133[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_133)

subdom_quad134 <- subset(sub_abund, quad_no == "134")
shape_spp_134<- aggregate(subdom_quad134$shape.index, by = list(Species = subdom_quad134$spp_duplicate), FUN = mean)
shape_spp_134 <- as.data.frame(shape_spp_134)
shape_spp_134 <- cbind(quad_no = 134, shape_spp_134)
rownames(shape_spp_134) <- shape_spp_134$Species
shape_spp_134 <- data.frame(t(shape_spp_134))
shape_spp_134 <- shape_spp_134[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_134)

subdom_quad135 <- subset(sub_abund, quad_no == "135")
shape_spp_135<- aggregate(subdom_quad135$shape.index, by = list(Species = subdom_quad135$spp_duplicate), FUN = mean)
shape_spp_135 <- as.data.frame(shape_spp_135)
shape_spp_135 <- cbind(quad_no = 135, shape_spp_135)
rownames(shape_spp_135) <- shape_spp_135$Species
shape_spp_135 <- data.frame(t(shape_spp_135))
shape_spp_135 <- shape_spp_135[-1,]
shape_sub_spp<- rbind(shape_sub_spp, shape_spp_135)

subdom_quad136 <- subset(sub_abund, quad_no == "136")
shape_spp_136<- aggregate(subdom_quad136$shape.index, by = list(Species = subdom_quad136$spp_duplicate), FUN = mean)
shape_spp_136 <- as.data.frame(shape_spp_136)
shape_spp_136 <- cbind(quad_no = 136, shape_spp_136)
rownames(shape_spp_136) <- shape_spp_136$Species
shape_spp_136 <- data.frame(t(shape_spp_136))
shape_spp_136 <- shape_spp_136[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_136)

subdom_quad137 <- subset(sub_abund, quad_no == "137")
shape_spp_137<- aggregate(subdom_quad137$shape.index, by = list(Species = subdom_quad137$spp_duplicate), FUN = mean)
shape_spp_137 <- as.data.frame(shape_spp_137)
shape_spp_137 <- cbind(quad_no = 137, shape_spp_137)
rownames(shape_spp_137) <- shape_spp_137$Species
shape_spp_137 <- data.frame(t(shape_spp_137))
shape_spp_137 <- shape_spp_137[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_137)

subdom_quad138 <- subset(sub_abund, quad_no == "138")
subdom_quad138 <- read.csv("Results/Patch_Stats/Individual_quadrats/Q138_DOMINANT.csv")
subdom_quad138 <- subset(subdom_quad138, patchID > 0)
shape_spp_138<- aggregate(subdom_quad138$shape.index, by = list(Species = subdom_quad138$spp_duplicate), FUN = mean)
shape_spp_138 <- as.data.frame(shape_spp_138)
shape_spp_138 <- cbind(quad_no = 138, shape_spp_138)
rownames(shape_spp_138) <- shape_spp_138$Species
shape_spp_138 <- data.frame(t(shape_spp_138))
shape_spp_138 <- shape_spp_138[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_138)

subdom_quad139 <- subset(sub_abund, quad_no == "139")
shape_spp_139<- aggregate(subdom_quad139$shape.index, by = list(Species = subdom_quad139$spp_duplicate), FUN = mean)
shape_spp_139 <- as.data.frame(shape_spp_139)
shape_spp_139 <- cbind(quad_no = 139, shape_spp_139)
rownames(shape_spp_139) <- shape_spp_139$Species
shape_spp_139 <- data.frame(t(shape_spp_139))
shape_spp_139 <- shape_spp_139[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_139)

subdom_quad140 <- subset(sub_abund, quad_no == "140")
shape_spp_140<- aggregate(subdom_quad140$shape.index, by = list(Species = subdom_quad140$spp_duplicate), FUN = mean)
shape_spp_140 <- as.data.frame(shape_spp_140)
shape_spp_140 <- cbind(quad_no = 140, shape_spp_140)
rownames(shape_spp_140) <- shape_spp_140$Species
shape_spp_140 <- data.frame(t(shape_spp_140))
shape_spp_140 <- shape_spp_140[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_140)

subdom_quad141 <- subset(sub_abund, quad_no == "141")
shape_spp_141<- aggregate(subdom_quad141$shape.index, by = list(Species = subdom_quad141$spp_duplicate), FUN = mean)
shape_spp_141 <- as.data.frame(shape_spp_141)
shape_spp_141 <- cbind(quad_no = 141, shape_spp_141)
rownames(shape_spp_141) <- shape_spp_141$Species
shape_spp_141 <- data.frame(t(shape_spp_141))
shape_spp_141 <- shape_spp_141[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_141)

subdom_quad142 <- subset(sub_abund, quad_no == "142")
shape_spp_142<- aggregate(subdom_quad142$shape.index, by = list(Species = subdom_quad142$spp_duplicate), FUN = mean)
shape_spp_142 <- as.data.frame(shape_spp_142)
shape_spp_142 <- cbind(quad_no = 142, shape_spp_142)
rownames(shape_spp_142) <- shape_spp_142$Species
shape_spp_142 <- data.frame(t(shape_spp_142))
shape_spp_142 <- shape_spp_142[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_142)

subdom_quad143 <- subset(sub_abund, quad_no == "143")
shape_spp_143<- aggregate(subdom_quad143$shape.index, by = list(Species = subdom_quad143$spp_duplicate), FUN = mean)
shape_spp_143 <- as.data.frame(shape_spp_143)
shape_spp_143 <- cbind(quad_no = 143, shape_spp_143)
rownames(shape_spp_143) <- shape_spp_143$Species
shape_spp_143 <- data.frame(t(shape_spp_143))
shape_spp_143 <- shape_spp_143[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_143)

subdom_quad144 <- subset(sub_abund, quad_no == "144")
shape_spp_144<- aggregate(subdom_quad144$shape.index, by = list(Species = subdom_quad144$spp_duplicate), FUN = mean)
shape_spp_144 <- as.data.frame(shape_spp_144)
shape_spp_144 <- cbind(quad_no = 144, shape_spp_144)
rownames(shape_spp_144) <- shape_spp_144$Species
shape_spp_144 <- data.frame(t(shape_spp_144))
shape_spp_144 <- shape_spp_144[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_144)

subdom_quad145 <- subset(sub_abund, quad_no == "145")
shape_spp_145<- aggregate(subdom_quad145$shape.index, by = list(Species = subdom_quad145$spp_duplicate), FUN = mean)
shape_spp_145 <- as.data.frame(shape_spp_145)
shape_spp_145 <- cbind(quad_no = 145, shape_spp_145)
rownames(shape_spp_145) <- shape_spp_145$Species
shape_spp_145 <- data.frame(t(shape_spp_145))
shape_spp_145 <- shape_spp_145[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_145)

subdom_quad146 <- subset(sub_abund, quad_no == "146")
shape_spp_146<- aggregate(subdom_quad146$shape.index, by = list(Species = subdom_quad146$spp_duplicate), FUN = mean)
shape_spp_146 <- as.data.frame(shape_spp_146)
shape_spp_146 <- cbind(quad_no = 146, shape_spp_146)
rownames(shape_spp_146) <- shape_spp_146$Species
shape_spp_146 <- data.frame(t(shape_spp_146))
shape_spp_146 <- shape_spp_146[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_146)

subdom_quad147 <- subset(sub_abund, quad_no == "147")
shape_spp_147<- aggregate(subdom_quad147$shape.index, by = list(Species = subdom_quad147$spp_duplicate), FUN = mean)
shape_spp_147 <- as.data.frame(shape_spp_147)
shape_spp_147 <- cbind(quad_no = 147, shape_spp_147)
rownames(shape_spp_147) <- shape_spp_147$Species
shape_spp_147 <- data.frame(t(shape_spp_147))
shape_spp_147 <- shape_spp_147[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_147)

subdom_quad148 <- subset(sub_abund, quad_no == "148")
shape_spp_148<- aggregate(subdom_quad148$shape.index, by = list(Species = subdom_quad148$spp_duplicate), FUN = mean)
shape_spp_148 <- as.data.frame(shape_spp_148)
shape_spp_148 <- cbind(quad_no = 148, shape_spp_148)
rownames(shape_spp_148) <- shape_spp_148$Species
shape_spp_148 <- data.frame(t(shape_spp_148))
shape_spp_148 <- shape_spp_148[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_148)

subdom_quad149 <- subset(sub_abund, quad_no == "149")
shape_spp_149<- aggregate(subdom_quad149$shape.index, by = list(Species = subdom_quad149$spp_duplicate), FUN = mean)
shape_spp_149 <- as.data.frame(shape_spp_149)
shape_spp_149 <- cbind(quad_no = 149, shape_spp_149)
rownames(shape_spp_149) <- shape_spp_149$Species
shape_spp_149 <- data.frame(t(shape_spp_149))
shape_spp_149 <- shape_spp_149[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_149)

subdom_quad150 <- subset(sub_abund, quad_no == "150")
shape_spp_150<- aggregate(subdom_quad150$shape.index, by = list(Species = subdom_quad150$spp_duplicate), FUN = mean)
shape_spp_150 <- as.data.frame(shape_spp_150)
shape_spp_150 <- cbind(quad_no = 150, shape_spp_150)
rownames(shape_spp_150) <- shape_spp_150$Species
shape_spp_150 <- data.frame(t(shape_spp_150))
shape_spp_150 <- shape_spp_150[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_150)

subdom_quad151 <- subset(sub_abund, quad_no == "151")
shape_spp_151<- aggregate(subdom_quad151$shape.index, by = list(Species = subdom_quad151$spp_duplicate), FUN = mean)
shape_spp_151 <- as.data.frame(shape_spp_151)
shape_spp_151 <- cbind(quad_no = 151, shape_spp_151)
rownames(shape_spp_151) <- shape_spp_151$Species
shape_spp_151 <- data.frame(t(shape_spp_151))
shape_spp_151 <- shape_spp_151[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_151)

subdom_quad152 <- subset(sub_abund, quad_no == "152")
shape_spp_152<- aggregate(subdom_quad152$shape.index, by = list(Species = subdom_quad152$spp_duplicate), FUN = mean)
shape_spp_152 <- as.data.frame(shape_spp_152)
shape_spp_152 <- cbind(quad_no = 152, shape_spp_152)
rownames(shape_spp_152) <- shape_spp_152$Species
shape_spp_152 <- data.frame(t(shape_spp_152))
shape_spp_152 <- shape_spp_152[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_152)

subdom_quad153 <- subset(sub_abund, quad_no == "153")
shape_spp_153<- aggregate(subdom_quad153$shape.index, by = list(Species = subdom_quad153$spp_duplicate), FUN = mean)
shape_spp_153 <- as.data.frame(shape_spp_153)
shape_spp_153 <- cbind(quad_no = 153, shape_spp_153)
rownames(shape_spp_153) <- shape_spp_153$Species
shape_spp_153 <- data.frame(t(shape_spp_153))
shape_spp_153 <- shape_spp_153[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_153)

subdom_quad154 <- subset(sub_abund, quad_no == "154")
shape_spp_154<- aggregate(subdom_quad154$shape.index, by = list(Species = subdom_quad154$spp_duplicate), FUN = mean)
shape_spp_154 <- as.data.frame(shape_spp_154)
shape_spp_154 <- cbind(quad_no = 154, shape_spp_154)
rownames(shape_spp_154) <- shape_spp_154$Species
shape_spp_154 <- data.frame(t(shape_spp_154))
shape_spp_154 <- shape_spp_154[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_154)

subdom_quad155 <- subset(sub_abund, quad_no == "155")
shape_spp_155<- aggregate(subdom_quad155$shape.index, by = list(Species = subdom_quad155$spp_duplicate), FUN = mean)
shape_spp_155 <- as.data.frame(shape_spp_155)
shape_spp_155 <- cbind(quad_no = 155, shape_spp_155)
rownames(shape_spp_155) <- shape_spp_155$Species
shape_spp_155 <- data.frame(t(shape_spp_155))
shape_spp_155 <- shape_spp_155[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_155)

subdom_quad156 <- subset(sub_abund, quad_no == "156")
shape_spp_156<- aggregate(subdom_quad156$shape.index, by = list(Species = subdom_quad156$spp_duplicate), FUN = mean)
shape_spp_156 <- as.data.frame(shape_spp_156)
shape_spp_156 <- cbind(quad_no = 156, shape_spp_156)
rownames(shape_spp_156) <- shape_spp_156$Species
shape_spp_156 <- data.frame(t(shape_spp_156))
shape_spp_156 <- shape_spp_156[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_156)

subdom_quad157 <- subset(sub_abund, quad_no == "157")
shape_spp_157<- aggregate(subdom_quad157$shape.index, by = list(Species = subdom_quad157$spp_duplicate), FUN = mean)
shape_spp_157 <- as.data.frame(shape_spp_157)
shape_spp_157 <- cbind(quad_no = 157, shape_spp_157)
rownames(shape_spp_157) <- shape_spp_157$Species
shape_spp_157 <- data.frame(t(shape_spp_157))
shape_spp_157 <- shape_spp_157[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_157)

subdom_quad158 <- subset(sub_abund, quad_no == "158")
shape_spp_158<- aggregate(subdom_quad158$shape.index, by = list(Species = subdom_quad158$spp_duplicate), FUN = mean)
shape_spp_158 <- as.data.frame(shape_spp_158)
shape_spp_158 <- cbind(quad_no = 158, shape_spp_158)
rownames(shape_spp_158) <- shape_spp_158$Species
shape_spp_158 <- data.frame(t(shape_spp_158))
shape_spp_158 <- shape_spp_158[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_158)

subdom_quad159 <- subset(sub_abund, quad_no == "159")
shape_spp_159<- aggregate(subdom_quad159$shape.index, by = list(Species = subdom_quad159$spp_duplicate), FUN = mean)
shape_spp_159 <- as.data.frame(shape_spp_159)
shape_spp_159 <- cbind(quad_no = 159, shape_spp_159)
rownames(shape_spp_159) <- shape_spp_159$Species
shape_spp_159 <- data.frame(t(shape_spp_159))
shape_spp_159 <- shape_spp_159[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_159)

subdom_quad160 <- subset(sub_abund, quad_no == "160")
shape_spp_160<- aggregate(subdom_quad160$shape.index, by = list(Species = subdom_quad160$spp_duplicate), FUN = mean)
shape_spp_160 <- as.data.frame(shape_spp_160)
shape_spp_160 <- cbind(quad_no = 160, shape_spp_160)
rownames(shape_spp_160) <- shape_spp_160$Species
shape_spp_160 <- data.frame(t(shape_spp_160))
shape_spp_160 <- shape_spp_160[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_160)

subdom_quad161 <- subset(sub_abund, quad_no == "161")
shape_spp_161<- aggregate(subdom_quad161$shape.index, by = list(Species = subdom_quad161$spp_duplicate), FUN = mean)
shape_spp_161 <- as.data.frame(shape_spp_161)
shape_spp_161 <- cbind(quad_no = 161, shape_spp_161)
rownames(shape_spp_161) <- shape_spp_161$Species
shape_spp_161 <- data.frame(t(shape_spp_161))
shape_spp_161 <- shape_spp_161[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_161)

subdom_quad162 <- subset(sub_abund, quad_no == "162")
shape_spp_162<- aggregate(subdom_quad162$shape.index, by = list(Species = subdom_quad162$spp_duplicate), FUN = mean)
shape_spp_162 <- as.data.frame(shape_spp_162)
shape_spp_162 <- cbind(quad_no = 162, shape_spp_162)
rownames(shape_spp_162) <- shape_spp_162$Species
shape_spp_162 <- data.frame(t(shape_spp_162))
shape_spp_162 <- shape_spp_162[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_162)

subdom_quad163 <- subset(sub_abund, quad_no == "163")
shape_spp_163<- aggregate(subdom_quad163$shape.index, by = list(Species = subdom_quad163$spp_duplicate), FUN = mean)
shape_spp_163 <- as.data.frame(shape_spp_163)
shape_spp_163 <- cbind(quad_no = 163, shape_spp_163)
rownames(shape_spp_163) <- shape_spp_163$Species
shape_spp_163 <- data.frame(t(shape_spp_163))
shape_spp_163 <- shape_spp_163[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_163)

subdom_quad164 <- subset(sub_abund, quad_no == "164")
shape_spp_164<- aggregate(subdom_quad164$shape.index, by = list(Species = subdom_quad164$spp_duplicate), FUN = mean)
shape_spp_164 <- as.data.frame(shape_spp_164)
shape_spp_164 <- cbind(quad_no = 164, shape_spp_164)
rownames(shape_spp_164) <- shape_spp_164$Species
shape_spp_164 <- data.frame(t(shape_spp_164))
shape_spp_164 <- shape_spp_164[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_164)

subdom_quad165 <- subset(sub_abund, quad_no == "165")
shape_spp_165<- aggregate(subdom_quad165$shape.index, by = list(Species = subdom_quad165$spp_duplicate), FUN = mean)
shape_spp_165 <- as.data.frame(shape_spp_165)
shape_spp_165 <- cbind(quad_no = 165, shape_spp_165)
rownames(shape_spp_165) <- shape_spp_165$Species
shape_spp_165 <- data.frame(t(shape_spp_165))
shape_spp_165 <- shape_spp_165[-1,]
shape_sub_spp<- rbind(shape_sub_spp,  shape_spp_165)

subdom_quad166 <- subset(sub_abund, quad_no == "166")
shape_spp_166<- aggregate(subdom_quad166$shape.index, by = list(Species = subdom_quad166$spp_duplicate), FUN = mean)
shape_spp_166 <- as.data.frame(shape_spp_166)
shape_spp_166 <- cbind(quad_no = 166, shape_spp_166)
rownames(shape_spp_166) <- shape_spp_166$Species
shape_spp_166 <- data.frame(t(shape_spp_166))
shape_spp_166 <- shape_spp_166[-1,]
shape_sub_spp<- rbind(shape_sub_spp, shape_spp_166)

subdom_quad167 <- subset(sub_abund, quad_no == "167")
shape_spp_167<- aggregate(subdom_quad167$shape.index, by = list(Species = subdom_quad167$spp_duplicate), FUN = mean)
shape_spp_167 <- as.data.frame(shape_spp_167)
shape_spp_167 <- cbind(quad_no = 167, shape_spp_167)
rownames(shape_spp_167) <- shape_spp_167$Species
shape_spp_167 <- data.frame(t(shape_spp_167))
shape_spp_167 <- shape_spp_167[-1,]
shape_sub_spp<- rbind(shape_sub_spp, shape_spp_167)


write.csv(shape_sub_spp, file = "Results/Patch_Stats/Ordination/Shape_SUBDOM_12-08-18.csv")


################################################################################### 
##         NUMBER OF PATCHES PER SPECIES: FOR USE WITH SPECIES MATRIX + GLMs     ##
###################################################################################

sub_abund <- read.csv("Results/Patch_Stats/Individual_quadrats/04-08-18_SUB.csv")
sub_abund <- sub_abund[,-1]
sub_abund <- subset(sub_abund, patchID > 0) # removes patch ID 0

patchno_sub_spp <- NULL


## Getting data from each quadrat separately
subdom_quad1 <- subset(sub_abund, quad_no == "1")
patchno_spp_1<- aggregate(subdom_quad1$patchID, by = list(Species = subdom_quad1$spp_duplicate), FUN = max)
patchno_spp_1 <- as.data.frame(patchno_spp_1)
patchno_spp_1 <- cbind(quad_no = 1, patchno_spp_1)
rownames(patchno_spp_1) <- patchno_spp_1$Species
patchno_spp_1 <- data.frame(t(patchno_spp_1))
patchno_spp_1 <- patchno_spp_1[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_1)

subdom_quad2 <- subset(sub_abund, quad_no == "2")
patchno_spp_2<- aggregate(subdom_quad2$patchID, by = list(Species = subdom_quad2$spp_duplicate), FUN = max)
patchno_spp_2 <- as.data.frame(patchno_spp_2)
patchno_spp_2 <- cbind(quad_no = 2, patchno_spp_2)
rownames(patchno_spp_2) <- patchno_spp_2$Species
patchno_spp_2 <- data.frame(t(patchno_spp_2))
patchno_spp_2 <- patchno_spp_2[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_2)

subdom_quad3 <- subset(sub_abund, quad_no == "3")
patchno_spp_3<- aggregate(subdom_quad3$patchID, by = list(Species = subdom_quad3$spp_duplicate), FUN = max)
patchno_spp_3 <- as.data.frame(patchno_spp_3)
patchno_spp_3 <- cbind(quad_no = 3, patchno_spp_3)
rownames(patchno_spp_3) <- patchno_spp_3$Species
patchno_spp_3 <- data.frame(t(patchno_spp_3))
patchno_spp_3 <- patchno_spp_3[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_3)

subdom_quad4 <- subset(sub_abund, quad_no == "4")
patchno_spp_4<- aggregate(subdom_quad4$patchID, by = list(Species = subdom_quad4$spp_duplicate), FUN = max)
patchno_spp_4 <- as.data.frame(patchno_spp_4)
patchno_spp_4 <- cbind(quad_no = 4, patchno_spp_4)
rownames(patchno_spp_4) <- patchno_spp_4$Species
patchno_spp_4 <- data.frame(t(patchno_spp_4))
patchno_spp_4 <- patchno_spp_4[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_4)

subdom_quad5 <- subset(sub_abund, quad_no == "5")
patchno_spp_5<- aggregate(subdom_quad5$patchID, by = list(Species = subdom_quad5$spp_duplicate), FUN = max)
patchno_spp_5 <- as.data.frame(patchno_spp_5)
patchno_spp_5 <- cbind(quad_no = 5, patchno_spp_5)
rownames(patchno_spp_5) <- patchno_spp_5$Species
patchno_spp_5 <- data.frame(t(patchno_spp_5))
patchno_spp_5 <- patchno_spp_5[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_5)

subdom_quad6 <- subset(sub_abund, quad_no == "6")
patchno_spp_6<- aggregate(subdom_quad6$patchID, by = list(Species = subdom_quad6$spp_duplicate), FUN = max)
patchno_spp_6 <- as.data.frame(patchno_spp_6)
patchno_spp_6 <- cbind(quad_no = 6, patchno_spp_6)
rownames(patchno_spp_6) <- patchno_spp_6$Species
patchno_spp_6 <- data.frame(t(patchno_spp_6))
patchno_spp_6 <- patchno_spp_6[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_6)

subdom_quad7 <- subset(sub_abund, quad_no == "7")
patchno_spp_7<- aggregate(subdom_quad7$patchID, by = list(Species = subdom_quad7$spp_duplicate), FUN = max)
patchno_spp_7 <- as.data.frame(patchno_spp_7)
patchno_spp_7 <- cbind(quad_no = 7, patchno_spp_7)
rownames(patchno_spp_7) <- patchno_spp_7$Species
patchno_spp_7 <- data.frame(t(patchno_spp_7))
patchno_spp_7 <- patchno_spp_7[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_7)

subdom_quad8 <- subset(sub_abund, quad_no == "8")
patchno_spp_8<- aggregate(subdom_quad8$patchID, by = list(Species = subdom_quad8$spp_duplicate), FUN = max)
patchno_spp_8 <- as.data.frame(patchno_spp_8)
patchno_spp_8 <- cbind(quad_no = 8, patchno_spp_8)
rownames(patchno_spp_8) <- patchno_spp_8$Species
patchno_spp_8 <- data.frame(t(patchno_spp_8))
patchno_spp_8 <- patchno_spp_8[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_8)

subdom_quad9 <- subset(sub_abund, quad_no == "9")
patchno_spp_9<- aggregate(subdom_quad9$patchID, by = list(Species = subdom_quad9$spp_duplicate), FUN = max)
patchno_spp_9 <- as.data.frame(patchno_spp_9)
patchno_spp_9 <- cbind(quad_no = 9, patchno_spp_9)
rownames(patchno_spp_9) <- patchno_spp_9$Species
patchno_spp_9 <- data.frame(t(patchno_spp_9))
patchno_spp_9 <- patchno_spp_9[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_9)

subdom_quad10 <- subset(sub_abund, quad_no == "10")
patchno_spp_10<- aggregate(subdom_quad10$patchID, by = list(Species = subdom_quad10$spp_duplicate), FUN = max)
patchno_spp_10 <- as.data.frame(patchno_spp_10)
patchno_spp_10 <- cbind(quad_no = 10, patchno_spp_10)
rownames(patchno_spp_10) <- patchno_spp_10$Species
patchno_spp_10 <- data.frame(t(patchno_spp_10))
patchno_spp_10 <- patchno_spp_10[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_10)

subdom_quad11 <- subset(sub_abund, quad_no == "11")
patchno_spp_11<- aggregate(subdom_quad11$patchID, by = list(Species = subdom_quad11$spp_duplicate), FUN = max)
patchno_spp_11 <- as.data.frame(patchno_spp_11)
patchno_spp_11 <- cbind(quad_no = 11, patchno_spp_11)
rownames(patchno_spp_11) <- patchno_spp_11$Species
patchno_spp_11 <- data.frame(t(patchno_spp_11))
patchno_spp_11 <- patchno_spp_11[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_11)

subdom_quad12 <- subset(sub_abund, quad_no == "12")
patchno_spp_12<- aggregate(subdom_quad12$patchID, by = list(Species = subdom_quad12$spp_duplicate), FUN = max)
patchno_spp_12 <- as.data.frame(patchno_spp_12)
patchno_spp_12 <- cbind(quad_no = 12, patchno_spp_12)
rownames(patchno_spp_12) <- patchno_spp_12$Species
patchno_spp_12 <- data.frame(t(patchno_spp_12))
patchno_spp_12 <- patchno_spp_12[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_12)

subdom_quad13 <- subset(sub_abund, quad_no == "13")
patchno_spp_13<- aggregate(subdom_quad13$patchID, by = list(Species = subdom_quad13$spp_duplicate), FUN = max)
patchno_spp_13 <- as.data.frame(patchno_spp_13)
patchno_spp_13 <- cbind(quad_no = 13, patchno_spp_13)
rownames(patchno_spp_13) <- patchno_spp_13$Species
patchno_spp_13 <- data.frame(t(patchno_spp_13))
patchno_spp_13 <- patchno_spp_13[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_13)

subdom_quad14 <- subset(sub_abund, quad_no == "14")
patchno_spp_14<- aggregate(subdom_quad14$patchID, by = list(Species = subdom_quad14$spp_duplicate), FUN = max)
patchno_spp_14 <- as.data.frame(patchno_spp_14)
patchno_spp_14 <- cbind(quad_no = 14, patchno_spp_14)
rownames(patchno_spp_14) <- patchno_spp_14$Species
patchno_spp_14 <- data.frame(t(patchno_spp_14))
patchno_spp_14 <- patchno_spp_14[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_14)

subdom_quad15 <- subset(sub_abund, quad_no == "15")
patchno_spp_15<- aggregate(subdom_quad15$patchID, by = list(Species = subdom_quad15$spp_duplicate), FUN = max)
patchno_spp_15 <- as.data.frame(patchno_spp_15)
patchno_spp_15 <- cbind(quad_no = 15, patchno_spp_15)
rownames(patchno_spp_15) <- patchno_spp_15$Species
patchno_spp_15 <- data.frame(t(patchno_spp_15))
patchno_spp_15 <- patchno_spp_15[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_15)

subdom_quad16 <- subset(sub_abund, quad_no == "16")
patchno_spp_16<- aggregate(subdom_quad16$patchID, by = list(Species = subdom_quad16$spp_duplicate), FUN = max)
patchno_spp_16 <- as.data.frame(patchno_spp_16)
patchno_spp_16 <- cbind(quad_no = 16, patchno_spp_16)
rownames(patchno_spp_16) <- patchno_spp_16$Species
patchno_spp_16 <- data.frame(t(patchno_spp_16))
patchno_spp_16 <- patchno_spp_16[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_16)

subdom_quad17 <- subset(sub_abund, quad_no == "17")
patchno_spp_17<- aggregate(subdom_quad17$patchID, by = list(Species = subdom_quad17$spp_duplicate), FUN = max)
patchno_spp_17 <- as.data.frame(patchno_spp_17)
patchno_spp_17 <- cbind(quad_no = 17, patchno_spp_17)
rownames(patchno_spp_17) <- patchno_spp_17$Species
patchno_spp_17 <- data.frame(t(patchno_spp_17))
patchno_spp_17 <- patchno_spp_17[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_17)

subdom_quad18 <- subset(sub_abund, quad_no == "18")
patchno_spp_18<- aggregate(subdom_quad18$patchID, by = list(Species = subdom_quad18$spp_duplicate), FUN = max)
patchno_spp_18 <- as.data.frame(patchno_spp_18)
patchno_spp_18 <- cbind(quad_no = 18, patchno_spp_18)
rownames(patchno_spp_18) <- patchno_spp_18$Species
patchno_spp_18 <- data.frame(t(patchno_spp_18))
patchno_spp_18 <- patchno_spp_18[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_18)

subdom_quad19 <- subset(sub_abund, quad_no == "19")
patchno_spp_19<- aggregate(subdom_quad19$patchID, by = list(Species = subdom_quad19$spp_duplicate), FUN = max)
patchno_spp_19 <- as.data.frame(patchno_spp_19)
patchno_spp_19 <- cbind(quad_no = 19, patchno_spp_19)
rownames(patchno_spp_19) <- patchno_spp_19$Species
patchno_spp_19 <- data.frame(t(patchno_spp_19))
patchno_spp_19 <- patchno_spp_19[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_19)

subdom_quad20 <- subset(sub_abund, quad_no == "20")
patchno_spp_20<- aggregate(subdom_quad20$patchID, by = list(Species = subdom_quad20$spp_duplicate), FUN = max)
patchno_spp_20 <- as.data.frame(patchno_spp_20)
patchno_spp_20 <- cbind(quad_no = 20, patchno_spp_20)
rownames(patchno_spp_20) <- patchno_spp_20$Species
patchno_spp_20 <- data.frame(t(patchno_spp_20))
patchno_spp_20 <- patchno_spp_20[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_20)

subdom_quad21 <- subset(sub_abund, quad_no == "21")
patchno_spp_21<- aggregate(subdom_quad21$patchID, by = list(Species = subdom_quad21$spp_duplicate), FUN = max)
patchno_spp_21 <- as.data.frame(patchno_spp_21)
patchno_spp_21 <- cbind(quad_no = 21, patchno_spp_21)
rownames(patchno_spp_21) <- patchno_spp_21$Species
patchno_spp_21 <- data.frame(t(patchno_spp_21))
patchno_spp_21 <- patchno_spp_21[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_21)

subdom_quad22 <- subset(sub_abund, quad_no == "22")
patchno_spp_22<- aggregate(subdom_quad22$patchID, by = list(Species = subdom_quad22$spp_duplicate), FUN = max)
patchno_spp_22 <- as.data.frame(patchno_spp_22)
patchno_spp_22 <- cbind(quad_no = 22, patchno_spp_22)
rownames(patchno_spp_22) <- patchno_spp_22$Species
patchno_spp_22 <- data.frame(t(patchno_spp_22))
patchno_spp_22 <- patchno_spp_22[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_22)

subdom_quad23 <- subset(sub_abund, quad_no == "23")
patchno_spp_23<- aggregate(subdom_quad23$patchID, by = list(Species = subdom_quad23$spp_duplicate), FUN = max)
patchno_spp_23 <- as.data.frame(patchno_spp_23)
patchno_spp_23 <- cbind(quad_no = 23, patchno_spp_23)
rownames(patchno_spp_23) <- patchno_spp_23$Species
patchno_spp_23 <- data.frame(t(patchno_spp_23))
patchno_spp_23 <- patchno_spp_23[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_23)

subdom_quad24 <- subset(sub_abund, quad_no == "24")
patchno_spp_24<- aggregate(subdom_quad24$patchID, by = list(Species = subdom_quad24$spp_duplicate), FUN = max)
patchno_spp_24 <- as.data.frame(patchno_spp_24)
patchno_spp_24 <- cbind(quad_no = 24, patchno_spp_24)
rownames(patchno_spp_24) <- patchno_spp_24$Species
patchno_spp_24 <- data.frame(t(patchno_spp_24))
patchno_spp_24 <- patchno_spp_24[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_24)

subdom_quad25 <- subset(sub_abund, quad_no == "25")
patchno_spp_25<- aggregate(subdom_quad25$patchID, by = list(Species = subdom_quad25$spp_duplicate), FUN = max)
patchno_spp_25 <- as.data.frame(patchno_spp_25)
patchno_spp_25 <- cbind(quad_no = 25, patchno_spp_25)
rownames(patchno_spp_25) <- patchno_spp_25$Species
patchno_spp_25 <- data.frame(t(patchno_spp_25))
patchno_spp_25 <- patchno_spp_25[-1,]                           
patchno_spp <- rbind(patchno_spp,  patchno_spp_25)

subdom_quad26 <- subset(sub_abund, quad_no == "26")
patchno_spp_26<- aggregate(subdom_quad26$patchID, by = list(Species = subdom_quad26$spp_duplicate), FUN = max)
patchno_spp_26 <- as.data.frame(patchno_spp_26)
patchno_spp_26 <- cbind(quad_no = 26, patchno_spp_26)
rownames(patchno_spp_26) <- patchno_spp_26$Species
patchno_spp_26 <- data.frame(t(patchno_spp_26))
patchno_spp_26 <- patchno_spp_26[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_26)

subdom_quad27 <- subset(sub_abund, quad_no == "27")
patchno_spp_27<- aggregate(subdom_quad27$patchID, by = list(Species = subdom_quad27$spp_duplicate), FUN = max)
patchno_spp_27 <- as.data.frame(patchno_spp_27)
patchno_spp_27 <- cbind(quad_no = 27, patchno_spp_27)
rownames(patchno_spp_27) <- patchno_spp_27$Species
patchno_spp_27 <- data.frame(t(patchno_spp_27))
patchno_spp_27 <- patchno_spp_27[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_27)

subdom_quad_28 <- subset(sub_abund, quad_no == "28")
patchno_spp_28<- aggregate(subdom_quad_28$patchID, by = list(Species = subdom_quad_28$spp_duplicate), FUN = max)
patchno_spp_28 <- as.data.frame(patchno_spp_28)
patchno_spp_28 <- cbind(quad_no = 28, patchno_spp_28)
rownames(patchno_spp_28) <- patchno_spp_28$Species
patchno_spp_28 <- data.frame(t(patchno_spp_28))
patchno_spp_28 <- patchno_spp_28[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_28)

subdom_quad29 <- subset(sub_abund, quad_no == "29")
patchno_spp_29<- aggregate(subdom_quad29$patchID, by = list(Species = subdom_quad29$spp_duplicate), FUN = max)
patchno_spp_29 <- as.data.frame(patchno_spp_29)
patchno_spp_29 <- cbind(quad_no = 29, patchno_spp_29)
rownames(patchno_spp_29) <- patchno_spp_29$Species
patchno_spp_29 <- data.frame(t(patchno_spp_29))
patchno_spp_29 <- patchno_spp_29[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_29)

subdom_quad30 <- subset(sub_abund, quad_no == "30")
patchno_spp_30<- aggregate(subdom_quad30$patchID, by = list(Species = subdom_quad30$spp_duplicate), FUN = max)
patchno_spp_30 <- as.data.frame(patchno_spp_30)
patchno_spp_30 <- cbind(quad_no = 30, patchno_spp_30)
rownames(patchno_spp_30) <- patchno_spp_30$Species
patchno_spp_30 <- data.frame(t(patchno_spp_30))
patchno_spp_30 <- patchno_spp_30[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_30)

subdom_quad31 <- subset(sub_abund, quad_no == "31")
patchno_spp_31<- aggregate(subdom_quad31$patchID, by = list(Species = subdom_quad31$spp_duplicate), FUN = max)
patchno_spp_31 <- as.data.frame(patchno_spp_31)
patchno_spp_31 <- cbind(quad_no = 31, patchno_spp_31)
rownames(patchno_spp_31) <- patchno_spp_31$Species
patchno_spp_31 <- data.frame(t(patchno_spp_31))
patchno_spp_31 <- patchno_spp_31[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_31)

subdom_quad32 <- subset(sub_abund, quad_no == "32")
patchno_spp_32<- aggregate(subdom_quad32$patchID, by = list(Species = subdom_quad32$spp_duplicate), FUN = max)
patchno_spp_32 <- as.data.frame(patchno_spp_32)
patchno_spp_32 <- cbind(quad_no = 32, patchno_spp_32)
rownames(patchno_spp_32) <- patchno_spp_32$Species
patchno_spp_32 <- data.frame(t(patchno_spp_32))
patchno_spp_32 <- patchno_spp_32[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_32)

subdom_quad33 <- subset(sub_abund, quad_no == "33")
patchno_spp_33<- aggregate(subdom_quad33$patchID, by = list(Species = subdom_quad33$spp_duplicate), FUN = max)
patchno_spp_33 <- as.data.frame(patchno_spp_33)
patchno_spp_33 <- cbind(quad_no = 33, patchno_spp_33)
rownames(patchno_spp_33) <- patchno_spp_33$Species
patchno_spp_33 <- data.frame(t(patchno_spp_33))
patchno_spp_33 <- patchno_spp_33[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_33)

subdom_quad34 <- subset(sub_abund, quad_no == "34")
patchno_spp_34<- aggregate(subdom_quad34$patchID, by = list(Species = subdom_quad34$spp_duplicate), FUN = max)
patchno_spp_34 <- as.data.frame(patchno_spp_34)
patchno_spp_34 <- cbind(quad_no = 34, patchno_spp_34)
rownames(patchno_spp_34) <- patchno_spp_34$Species
patchno_spp_34 <- data.frame(t(patchno_spp_34))
patchno_spp_34 <- patchno_spp_34[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_34)

subdom_quad35 <- subset(sub_abund, quad_no == "35")
patchno_spp_35<- aggregate(subdom_quad35$patchID, by = list(Species = subdom_quad35$spp_duplicate), FUN = max)
patchno_spp_35 <- as.data.frame(patchno_spp_35)
patchno_spp_35 <- cbind(quad_no = 35, patchno_spp_35)
rownames(patchno_spp_35) <- patchno_spp_35$Species
patchno_spp_35 <- data.frame(t(patchno_spp_35))
patchno_spp_35 <- patchno_spp_35[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_35)

subdom_quad36 <- subset(sub_abund, quad_no == "36")
patchno_spp_36<- aggregate(subdom_quad36$patchID, by = list(Species = subdom_quad36$spp_duplicate), FUN = max)
patchno_spp_36 <- as.data.frame(patchno_spp_36)
patchno_spp_36 <- cbind(quad_no = 36, patchno_spp_36)
rownames(patchno_spp_36) <- patchno_spp_36$Species
patchno_spp_36 <- patchno_spp_36[-1,]
patchno_spp_36 <- data.frame(t(patchno_spp_36))
patchno_spp <- rbind(patchno_spp,  patchno_spp_36)

subdom_quad37 <- subset(sub_abund, quad_no == "37")
patchno_spp_37<- aggregate(subdom_quad37$patchID, by = list(Species = subdom_quad37$spp_duplicate), FUN = max)
patchno_spp_37 <- as.data.frame(patchno_spp_37)
patchno_spp_37 <- cbind(quad_no = 37, patchno_spp_37)
rownames(patchno_spp_37) <- patchno_spp_37$Species
patchno_spp_37 <- data.frame(t(patchno_spp_37))
patchno_spp_37 <- patchno_spp_37[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_37)

subdom_quad38 <- subset(sub_abund, quad_no == "38")
patchno_spp_38<- aggregate(subdom_quad38$patchID, by = list(Species = subdom_quad38$spp_duplicate), FUN = max)
patchno_spp_38 <- as.data.frame(patchno_spp_38)
patchno_spp_38 <- cbind(quad_no = 38, patchno_spp_38)
rownames(patchno_spp_38) <- patchno_spp_38$Species
patchno_spp_38 <- data.frame(t(patchno_spp_38))
patchno_spp_38 <- patchno_spp_38[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_38)

subdom_quad39 <- subset(sub_abund, quad_no == "39")
patchno_spp_39<- aggregate(subdom_quad39$patchID, by = list(Species = subdom_quad39$spp_duplicate), FUN = max)
patchno_spp_39 <- as.data.frame(patchno_spp_39)
patchno_spp_39 <- cbind(quad_no = 39, patchno_spp_39)
rownames(patchno_spp_39) <- patchno_spp_39$Species
patchno_spp_39 <- data.frame(t(patchno_spp_39))
patchno_spp_39 <- patchno_spp_39[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_39)

subdom_quad40 <- subset(sub_abund, quad_no == "40")
patchno_spp_40<- aggregate(subdom_quad40$patchID, by = list(Species = subdom_quad40$spp_duplicate), FUN = max)
patchno_spp_40 <- as.data.frame(patchno_spp_40)
patchno_spp_40 <- cbind(quad_no = 40, patchno_spp_40)
rownames(patchno_spp_40) <- patchno_spp_40$Species
patchno_spp_40 <- data.frame(t(patchno_spp_40))
patchno_spp_40 <- patchno_spp_40[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_40)

subdom_quad41 <- subset(sub_abund, quad_no == "41")
patchno_spp_41<- aggregate(subdom_quad41$patchID, by = list(Species = subdom_quad41$spp_duplicate), FUN = max)
patchno_spp_41 <- as.data.frame(patchno_spp_41)
patchno_spp_41 <- cbind(quad_no = 41, patchno_spp_41)
rownames(patchno_spp_41) <- patchno_spp_41$Species
patchno_spp_41 <- data.frame(t(patchno_spp_41))
patchno_spp <- rbind(patchno_spp,  patchno_spp_41)

subdom_quad42 <- subset(sub_abund, quad_no == "42")
patchno_spp_42<- aggregate(subdom_quad42$patchID, by = list(Species = subdom_quad42$spp_duplicate), FUN = max)
patchno_spp_42 <- as.data.frame(patchno_spp_42)
patchno_spp_42 <- cbind(quad_no = 42, patchno_spp_42)
rownames(patchno_spp_42) <- patchno_spp_42$Species
patchno_spp_42 <- data.frame(t(patchno_spp_42))
patchno_spp_41 <- patchno_spp_41[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_42)

subdom_quad43 <- subset(sub_abund, quad_no == "43")
patchno_spp_43<- aggregate(subdom_quad43$patchID, by = list(Species = subdom_quad43$spp_duplicate), FUN = max)
patchno_spp_43 <- as.data.frame(patchno_spp_43)
patchno_spp_43 <- cbind(quad_no = 43, patchno_spp_43)
rownames(patchno_spp_43) <- patchno_spp_43$Species
patchno_spp_43 <- data.frame(t(patchno_spp_43))
patchno_spp_43 <- patchno_spp_43[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_43)

subdom_quad44 <- subset(sub_abund, quad_no == "44")
patchno_spp_44<- aggregate(subdom_quad44$patchID, by = list(Species = subdom_quad44$spp_duplicate), FUN = max)
patchno_spp_44 <- as.data.frame(patchno_spp_44)
patchno_spp_44 <- cbind(quad_no = 44, patchno_spp_44)
rownames(patchno_spp_44) <- patchno_spp_44$Species
patchno_spp_44 <- data.frame(t(patchno_spp_44))
patchno_spp_44 <- patchno_spp_44[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_44)

subdom_quad45 <- subset(sub_abund, quad_no == "45")
patchno_spp_45<- aggregate(subdom_quad45$patchID, by = list(Species = subdom_quad45$spp_duplicate), FUN = max)
patchno_spp_45 <- as.data.frame(patchno_spp_45)
patchno_spp_45 <- cbind(quad_no = 45, patchno_spp_45)
rownames(patchno_spp_45) <- patchno_spp_45$Species
patchno_spp_45 <- data.frame(t(patchno_spp_45))
patchno_spp_45 <- patchno_spp_45[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_45)

subdom_quad46 <- subset(sub_abund, quad_no == "46")
patchno_spp_46<- aggregate(subdom_quad46$patchID, by = list(Species = subdom_quad46$spp_duplicate), FUN = max)
patchno_spp_46 <- as.data.frame(patchno_spp_46)
patchno_spp_46 <- cbind(quad_no = 46, patchno_spp_46)
rownames(patchno_spp_46) <- patchno_spp_46$Species
patchno_spp_46 <- data.frame(t(patchno_spp_46))
patchno_spp_46 <- patchno_spp_46[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_46)

subdom_quad47 <- subset(sub_abund, quad_no == "47")
patchno_spp_47<- aggregate(subdom_quad47$patchID, by = list(Species = subdom_quad47$spp_duplicate), FUN = max)
patchno_spp_47 <- as.data.frame(patchno_spp_47)
patchno_spp_47 <- cbind(quad_no = 47, patchno_spp_47)
rownames(patchno_spp_47) <- patchno_spp_47$Species
patchno_spp_47 <- data.frame(t(patchno_spp_47))
patchno_spp_47 <- patchno_spp_47[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_47)

subdom_quad48 <- subset(sub_abund, quad_no == "48")
patchno_spp_48<- aggregate(subdom_quad48$patchID, by = list(Species = subdom_quad48$spp_duplicate), FUN = max)
patchno_spp_48 <- as.data.frame(patchno_spp_48)
patchno_spp_48 <- cbind(quad_no = 48, patchno_spp_48)
rownames(patchno_spp_48) <- patchno_spp_48$Species
patchno_spp_48 <- data.frame(t(patchno_spp_48))
patchno_spp_48 <- patchno_spp_48[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_48)

subdom_quad49 <- subset(sub_abund, quad_no == "49")
patchno_spp_49<- aggregate(subdom_quad49$patchID, by = list(Species = subdom_quad49$spp_duplicate), FUN = max)
patchno_spp_49 <- as.data.frame(patchno_spp_49)
patchno_spp_49 <- cbind(quad_no = 49, patchno_spp_49)
rownames(patchno_spp_49) <- patchno_spp_49$Species
patchno_spp_49 <- data.frame(t(patchno_spp_49))
patchno_spp_49 <- patchno_spp_49[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_49)

subdom_quad50 <- subset(sub_abund, quad_no == "50")
patchno_spp_50<- aggregate(subdom_quad50$patchID, by = list(Species = subdom_quad50$spp_duplicate), FUN = max)
patchno_spp_50 <- as.data.frame(patchno_spp_50)
patchno_spp_50 <- cbind(quad_no = 50, patchno_spp_50)
rownames(patchno_spp_50) <- patchno_spp_50$Species
patchno_spp_50 <- data.frame(t(patchno_spp_50))
patchno_spp_50 <- patchno_spp_50[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_50)

subdom_quad51 <- subset(sub_abund, quad_no == "51")
patchno_spp_51<- aggregate(subdom_quad51$patchID, by = list(Species = subdom_quad51$spp_duplicate), FUN = max)
patchno_spp_51 <- as.data.frame(patchno_spp_51)
patchno_spp_51 <- cbind(quad_no = 51, patchno_spp_51)
rownames(patchno_spp_51) <- patchno_spp_51$Species
patchno_spp_51 <- data.frame(t(patchno_spp_51))
patchno_spp_51 <- patchno_spp_51[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_51)

subdom_quad52 <- subset(sub_abund, quad_no == "52")
patchno_spp_52<- aggregate(subdom_quad52$patchID, by = list(Species = subdom_quad52$spp_duplicate), FUN = max)
patchno_spp_52 <- as.data.frame(patchno_spp_52)
patchno_spp_52 <- cbind(quad_no = 52, patchno_spp_52)
rownames(patchno_spp_52) <- patchno_spp_52$Species
patchno_spp_52 <- data.frame(t(patchno_spp_52))
patchno_spp_52 <- patchno_spp_52[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_52)

subdom_quad53 <- subset(sub_abund, quad_no == "53")
patchno_spp_53<- aggregate(subdom_quad53$patchID, by = list(Species = subdom_quad53$spp_duplicate), FUN = max)
patchno_spp_53 <- as.data.frame(patchno_spp_53)
patchno_spp_53 <- cbind(quad_no = 53, patchno_spp_53)
rownames(patchno_spp_53) <- patchno_spp_53$Species
patchno_spp_53 <- data.frame(t(patchno_spp_53))
patchno_spp_53 <- patchno_spp_53[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_53)

subdom_quad54 <- subset(sub_abund, quad_no == "54")
patchno_spp_54<- aggregate(subdom_quad54$patchID, by = list(Species = subdom_quad54$spp_duplicate), FUN = max)
patchno_spp_54 <- as.data.frame(patchno_spp_54)
patchno_spp_54 <- cbind(quad_no = 54, patchno_spp_54)
rownames(patchno_spp_54) <- patchno_spp_54$Species
patchno_spp_54 <- data.frame(t(patchno_spp_54))
patchno_spp_54 <- patchno_spp_54[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_54)

subdom_quad55 <- subset(sub_abund, quad_no == "55")
patchno_spp_55<- aggregate(subdom_quad55$patchID, by = list(Species = subdom_quad55$spp_duplicate), FUN = max)
patchno_spp_55 <- as.data.frame(patchno_spp_55)
patchno_spp_55 <- cbind(quad_no = 55, patchno_spp_55)
rownames(patchno_spp_55) <- patchno_spp_55$Species
patchno_spp_55 <- data.frame(t(patchno_spp_55))
patchno_spp_55 <- patchno_spp_55[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_55)

subdom_quad56 <- subset(sub_abund, quad_no == "56")
patchno_spp_56<- aggregate(subdom_quad56$patchID, by = list(Species = subdom_quad56$spp_duplicate), FUN = max)
patchno_spp_56 <- as.data.frame(patchno_spp_56)
patchno_spp_56 <- cbind(quad_no = 56, patchno_spp_56)
rownames(patchno_spp_56) <- patchno_spp_56$Species
patchno_spp_56 <- data.frame(t(patchno_spp_56))
patchno_spp_56 <- patchno_spp_56[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_56)

subdom_quad57 <- subset(sub_abund, quad_no == "57")
patchno_spp_57<- aggregate(subdom_quad57$patchID, by = list(Species = subdom_quad57$spp_duplicate), FUN = max)
patchno_spp_57 <- as.data.frame(patchno_spp_57)
patchno_spp_57 <- cbind(quad_no = 57, patchno_spp_57)
rownames(patchno_spp_57) <- patchno_spp_57$Species
patchno_spp_57 <- data.frame(t(patchno_spp_57))
patchno_spp_57 <- patchno_spp_57[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_57)

subdom_quad58 <- subset(sub_abund, quad_no == "58")
patchno_spp_58<- aggregate(subdom_quad58$patchID, by = list(Species = subdom_quad58$spp_duplicate), FUN = max)
patchno_spp_58 <- as.data.frame(patchno_spp_58)
patchno_spp_58 <- cbind(quad_no = 58, patchno_spp_58)
rownames(patchno_spp_58) <- patchno_spp_58$Species
patchno_spp_58 <- data.frame(t(patchno_spp_58))
patchno_spp_58 <- patchno_spp_58[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_58)

subdom_quad59 <- subset(sub_abund, quad_no == "59")
patchno_spp_59<- aggregate(subdom_quad59$patchID, by = list(Species = subdom_quad59$spp_duplicate), FUN = max)
patchno_spp_59 <- as.data.frame(patchno_spp_59)
patchno_spp_59 <- cbind(quad_no = 59, patchno_spp_59)
rownames(patchno_spp_59) <- patchno_spp_59$Species
patchno_spp_59 <- data.frame(t(patchno_spp_59))
patchno_spp_59 <- patchno_spp_59[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_59)

subdom_quad60 <- subset(sub_abund, quad_no == "60")
patchno_spp_60<- aggregate(subdom_quad60$patchID, by = list(Species = subdom_quad60$spp_duplicate), FUN = max)
patchno_spp_60 <- as.data.frame(patchno_spp_60)
patchno_spp_60 <- cbind(quad_no = 60, patchno_spp_60)
rownames(patchno_spp_60) <- patchno_spp_60$Species
patchno_spp_60 <- data.frame(t(patchno_spp_60))
patchno_spp_60 <- patchno_spp_60[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_60)

subdom_quad61 <- subset(sub_abund, quad_no == "61")
patchno_spp_61<- aggregate(subdom_quad61$patchID, by = list(Species = subdom_quad61$spp_duplicate), FUN = max)
patchno_spp_61 <- as.data.frame(patchno_spp_61)
patchno_spp_61 <- cbind(quad_no = 61, patchno_spp_61)
rownames(patchno_spp_61) <- patchno_spp_61$Species
patchno_spp_61 <- data.frame(t(patchno_spp_61))
patchno_spp_61 <- patchno_spp_61[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_61)

subdom_quad62 <- subset(sub_abund, quad_no == "62")
patchno_spp_62<- aggregate(subdom_quad62$patchID, by = list(Species = subdom_quad62$spp_duplicate), FUN = max)
patchno_spp_62 <- as.data.frame(patchno_spp_62)
patchno_spp_62 <- cbind(quad_no = 62, patchno_spp_62)
rownames(patchno_spp_62) <- patchno_spp_62$Species
patchno_spp_62 <- data.frame(t(patchno_spp_62))
patchno_spp_62 <- patchno_spp_62[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_62)

subdom_quad63 <- subset(sub_abund, quad_no == "63")
patchno_spp_63<- aggregate(subdom_quad63$patchID, by = list(Species = subdom_quad63$spp_duplicate), FUN = max)
patchno_spp_63 <- as.data.frame(patchno_spp_63)
patchno_spp_63 <- cbind(quad_no = 63, patchno_spp_63)
rownames(patchno_spp_63) <- patchno_spp_63$Species
patchno_spp_63 <- data.frame(t(patchno_spp_63))
patchno_spp_63 <- patchno_spp_63[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_63)

subdom_quad64 <- subset(sub_abund, quad_no == "64")
patchno_spp_64<- aggregate(subdom_quad64$patchID, by = list(Species = subdom_quad64$spp_duplicate), FUN = max)
patchno_spp_64 <- as.data.frame(patchno_spp_64)
patchno_spp_64 <- cbind(quad_no = 64, patchno_spp_64)
rownames(patchno_spp_64) <- patchno_spp_64$Species
patchno_spp_64 <- data.frame(t(patchno_spp_64))
patchno_spp_64 <- patchno_spp_64[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_64)

subdom_quad65 <- subset(sub_abund, quad_no == "65")
patchno_spp_65<- aggregate(subdom_quad65$patchID, by = list(Species = subdom_quad65$spp_duplicate), FUN = max)
patchno_spp_65 <- as.data.frame(patchno_spp_65)
patchno_spp_65 <- cbind(quad_no = 65, patchno_spp_65)
rownames(patchno_spp_65) <- patchno_spp_65$Species
patchno_spp_65 <- data.frame(t(patchno_spp_65))
patchno_spp_65 <- patchno_spp_65[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_65)

subdom_quad66 <- subset(sub_abund, quad_no == "66")
patchno_spp_66<- aggregate(subdom_quad66$patchID, by = list(Species = subdom_quad66$spp_duplicate), FUN = max)
patchno_spp_66 <- as.data.frame(patchno_spp_66)
patchno_spp_66 <- cbind(quad_no = 66, patchno_spp_66)
rownames(patchno_spp_66) <- patchno_spp_66$Species
patchno_spp_66 <- data.frame(t(patchno_spp_66))
patchno_spp_66 <- patchno_spp_66[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_66)

subdom_quad67 <- subset(sub_abund, quad_no == "67")
patchno_spp_67<- aggregate(subdom_quad67$patchID, by = list(Species = subdom_quad67$spp_duplicate), FUN = max)
patchno_spp_67 <- as.data.frame(patchno_spp_67)
patchno_spp_67 <- cbind(quad_no = 67, patchno_spp_67)
rownames(patchno_spp_67) <- patchno_spp_67$Species
patchno_spp_67 <- data.frame(t(patchno_spp_67))
patchno_spp_67 <- patchno_spp_67[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_67)

subdom_quad68 <- subset(sub_abund, quad_no == "68")
patchno_spp_68<- aggregate(subdom_quad68$patchID, by = list(Species = subdom_quad68$spp_duplicate), FUN = max)
patchno_spp_68 <- as.data.frame(patchno_spp_68)
patchno_spp_68 <- cbind(quad_no = 68, patchno_spp_68)
rownames(patchno_spp_68) <- patchno_spp_68$Species
patchno_spp_68 <- data.frame(t(patchno_spp_68))
patchno_spp_68 <- patchno_spp_68[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_68)

subdom_quad69 <- subset(sub_abund, quad_no == "69")
patchno_spp_69<- aggregate(subdom_quad69$patchID, by = list(Species = subdom_quad69$spp_duplicate), FUN = max)
patchno_spp_69 <- as.data.frame(patchno_spp_69)
patchno_spp_69 <- cbind(quad_no = 69, patchno_spp_69)
rownames(patchno_spp_69) <- patchno_spp_69$Species
patchno_spp_69 <- data.frame(t(patchno_spp_69))
patchno_spp_69 <- patchno_spp_69[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_69)

subdom_quad70 <- subset(sub_abund, quad_no == "70")
patchno_spp_70<- aggregate(subdom_quad70$patchID, by = list(Species = subdom_quad70$spp_duplicate), FUN = max)
patchno_spp_70 <- as.data.frame(patchno_spp_70)
patchno_spp_70 <- cbind(quad_no = 70, patchno_spp_70)
rownames(patchno_spp_70) <- patchno_spp_70$Species
patchno_spp_70 <- data.frame(t(patchno_spp_70))
patchno_spp_70 <- patchno_spp_70[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_70)

subdom_quad71 <- subset(sub_abund, quad_no == "71")
patchno_spp_71<- aggregate(subdom_quad71$patchID, by = list(Species = subdom_quad71$spp_duplicate), FUN = max)
patchno_spp_71 <- as.data.frame(patchno_spp_71)
patchno_spp_71 <- cbind(quad_no = 71, patchno_spp_71)
rownames(patchno_spp_71) <- patchno_spp_71$Species
patchno_spp_71 <- data.frame(t(patchno_spp_71))
patchno_spp_71 <- patchno_spp_71[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_71)

subdom_quad72 <- subset(sub_abund, quad_no == "72")
patchno_spp_72<- aggregate(subdom_quad72$patchID, by = list(Species = subdom_quad72$spp_duplicate), FUN = max)
patchno_spp_72 <- as.data.frame(patchno_spp_72)
patchno_spp_72 <- cbind(quad_no = 72, patchno_spp_72)
rownames(patchno_spp_72) <- patchno_spp_72$Species
patchno_spp_72 <- data.frame(t(patchno_spp_72))
patchno_spp_72 <- patchno_spp_72[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_72)

subdom_quad73 <- subset(sub_abund, quad_no == "73")
patchno_spp_73<- aggregate(subdom_quad73$patchID, by = list(Species = subdom_quad73$spp_duplicate), FUN = max)
patchno_spp_73 <- as.data.frame(patchno_spp_73)
patchno_spp_73 <- cbind(quad_no = 73, patchno_spp_73)
rownames(patchno_spp_73) <- patchno_spp_73$Species
patchno_spp_73 <- data.frame(t(patchno_spp_73))
patchno_spp_73 <- patchno_spp_73[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_73)

subdom_quad74 <- subset(sub_abund, quad_no == "74")
patchno_spp_74<- aggregate(subdom_quad74$patchID, by = list(Species = subdom_quad74$spp_duplicate), FUN = max)
patchno_spp_74 <- as.data.frame(patchno_spp_74)
patchno_spp_74 <- cbind(quad_no = 74, patchno_spp_74)
rownames(patchno_spp_74) <- patchno_spp_74$Species
patchno_spp_74 <- data.frame(t(patchno_spp_74))
patchno_spp_74 <- patchno_spp_74[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_74)

subdom_quad75 <- subset(sub_abund, quad_no == "75")
patchno_spp_75<- aggregate(subdom_quad75$patchID, by = list(Species = subdom_quad75$spp_duplicate), FUN = max)
patchno_spp_75 <- as.data.frame(patchno_spp_75)
patchno_spp_75 <- cbind(quad_no = 75, patchno_spp_75)
rownames(patchno_spp_75) <- patchno_spp_75$Species
patchno_spp_75 <- data.frame(t(patchno_spp_75))
patchno_spp_75 <- patchno_spp_75[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_75)

subdom_quad76 <- subset(sub_abund, quad_no == "76")
patchno_spp_76<- aggregate(subdom_quad76$patchID, by = list(Species = subdom_quad76$spp_duplicate), FUN = max)
patchno_spp_76 <- as.data.frame(patchno_spp_76)
patchno_spp_76 <- cbind(quad_no = 76, patchno_spp_76)
rownames(patchno_spp_76) <- patchno_spp_76$Species
patchno_spp_76 <- data.frame(t(patchno_spp_76))
patchno_spp_76 <- patchno_spp_76[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_76)

subdom_quad77 <- subset(sub_abund, quad_no == "77")
patchno_spp_77<- aggregate(subdom_quad77$patchID, by = list(Species = subdom_quad77$spp_duplicate), FUN = max)
patchno_spp_77 <- as.data.frame(patchno_spp_77)
patchno_spp_77 <- cbind(quad_no = 77, patchno_spp_77)
rownames(patchno_spp_77) <- patchno_spp_77$Species
patchno_spp_77 <- data.frame(t(patchno_spp_77))
patchno_spp_77 <- patchno_spp_77[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_77)

subdom_quad78 <- subset(sub_abund, quad_no == "78")
patchno_spp_78<- aggregate(subdom_quad78$patchID, by = list(Species = subdom_quad78$spp_duplicate), FUN = max)
patchno_spp_78 <- as.data.frame(patchno_spp_78)
patchno_spp_78 <- cbind(quad_no = 78, patchno_spp_78)
rownames(patchno_spp_78) <- patchno_spp_78$Species
patchno_spp_78 <- data.frame(t(patchno_spp_78))
patchno_spp_78 <- patchno_spp_78[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_78)

subdom_quad79 <- subset(sub_abund, quad_no == "79")
patchno_spp_79<- aggregate(subdom_quad79$patchID, by = list(Species = subdom_quad79$spp_duplicate), FUN = max)
patchno_spp_79 <- as.data.frame(patchno_spp_79)
patchno_spp_79 <- cbind(quad_no = 79, patchno_spp_79)
rownames(patchno_spp_79) <- patchno_spp_79$Species
patchno_spp_79 <- data.frame(t(patchno_spp_79))
patchno_spp_79 <- patchno_spp_79[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_79)

subdom_quad80 <- subset(sub_abund, quad_no == "80")
patchno_spp_80<- aggregate(subdom_quad80$patchID, by = list(Species = subdom_quad80$spp_duplicate), FUN = max)
patchno_spp_80 <- as.data.frame(patchno_spp_80)
patchno_spp_80 <- cbind(quad_no = 80, patchno_spp_80)
rownames(patchno_spp_80) <- patchno_spp_80$Species
patchno_spp_80 <- data.frame(t(patchno_spp_80))
patchno_spp_80 <- patchno_spp_80[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_80)

subdom_quad81 <- subset(sub_abund, quad_no == "81")
patchno_spp_81<- aggregate(subdom_quad81$patchID, by = list(Species = subdom_quad81$spp_duplicate), FUN = max)
patchno_spp_81 <- as.data.frame(patchno_spp_81)
patchno_spp_81 <- cbind(quad_no = 81, patchno_spp_81)
rownames(patchno_spp_81) <- patchno_spp_81$Species
patchno_spp_81 <- data.frame(t(patchno_spp_81))
patchno_spp_81 <- patchno_spp_81[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_81)

subdom_quad82 <- subset(sub_abund, quad_no == "82")
patchno_spp_82<- aggregate(subdom_quad82$patchID, by = list(Species = subdom_quad82$spp_duplicate), FUN = max)
patchno_spp_82 <- as.data.frame(patchno_spp_82)
patchno_spp_82 <- cbind(quad_no = 82, patchno_spp_82)
rownames(patchno_spp_82) <- patchno_spp_82$Species
patchno_spp_82 <- data.frame(t(patchno_spp_82))
patchno_spp_82 <- patchno_spp_82[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_82)

subdom_quad83 <- subset(sub_abund, quad_no == "83")
patchno_spp_83<- aggregate(subdom_quad83$patchID, by = list(Species = subdom_quad83$spp_duplicate), FUN = max)
patchno_spp_83 <- as.data.frame(patchno_spp_83)
patchno_spp_83 <- cbind(quad_no = 83, patchno_spp_83)
rownames(patchno_spp_83) <- patchno_spp_83$Species
patchno_spp_83 <- data.frame(t(patchno_spp_83))
patchno_spp_83 <- patchno_spp_83[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_83)

subdom_quad84 <- subset(sub_abund, quad_no == "84")
patchno_spp_84<- aggregate(subdom_quad84$patchID, by = list(Species = subdom_quad84$spp_duplicate), FUN = max)
patchno_spp_84 <- as.data.frame(patchno_spp_84)
patchno_spp_84 <- cbind(quad_no = 84, patchno_spp_84)
rownames(patchno_spp_84) <- patchno_spp_84$Species
patchno_spp_84 <- data.frame(t(patchno_spp_84))
patchno_spp_84 <- patchno_spp_84[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_84)

subdom_quad85 <- subset(sub_abund, quad_no == "85")
patchno_spp_85<- aggregate(subdom_quad85$patchID, by = list(Species = subdom_quad85$spp_duplicate), FUN = max)
patchno_spp_85 <- as.data.frame(patchno_spp_85)
patchno_spp_85 <- cbind(quad_no = 85, patchno_spp_85)
rownames(patchno_spp_85) <- patchno_spp_85$Species
patchno_spp_85 <- data.frame(t(patchno_spp_85))
patchno_spp_85 <- patchno_spp_85[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_85)

subdom_quad86 <- subset(sub_abund, quad_no == "86")
patchno_spp_86<- aggregate(subdom_quad86$patchID, by = list(Species = subdom_quad86$spp_duplicate), FUN = max)
patchno_spp_86 <- as.data.frame(patchno_spp_86)
patchno_spp_86 <- cbind(quad_no = 86, patchno_spp_86)
rownames(patchno_spp_86) <- patchno_spp_86$Species
patchno_spp_86 <- data.frame(t(patchno_spp_86))
patchno_spp_86 <- patchno_spp_86[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_86)

subdom_quad87 <- subset(sub_abund, quad_no == "87")
patchno_spp_87<- aggregate(subdom_quad87$patchID, by = list(Species = subdom_quad87$spp_duplicate), FUN = max)
patchno_spp_87 <- as.data.frame(patchno_spp_87)
patchno_spp_87 <- cbind(quad_no = 87, patchno_spp_87)
rownames(patchno_spp_87) <- patchno_spp_87$Species
patchno_spp_87 <- data.frame(t(patchno_spp_87))
patchno_spp_87 <- patchno_spp_87[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_87)

subdom_quad88 <- subset(sub_abund, quad_no == "88")
patchno_spp_88<- aggregate(subdom_quad88$patchID, by = list(Species = subdom_quad88$spp_duplicate), FUN = max)
patchno_spp_88 <- as.data.frame(patchno_spp_88)
patchno_spp_88 <- cbind(quad_no = 88, patchno_spp_88)
rownames(patchno_spp_88) <- patchno_spp_88$Species
patchno_spp_88 <- data.frame(t(patchno_spp_88))
patchno_spp_88 <- patchno_spp_88[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_88)

subdom_quad89 <- subset(sub_abund, quad_no == "89")
patchno_spp_89<- aggregate(subdom_quad89$patchID, by = list(Species = subdom_quad89$spp_duplicate), FUN = max)
patchno_spp_89 <- as.data.frame(patchno_spp_89)
patchno_spp_89 <- cbind(quad_no = 89, patchno_spp_89)
rownames(patchno_spp_89) <- patchno_spp_89$Species
patchno_spp_89 <- data.frame(t(patchno_spp_89))
patchno_spp_89 <- patchno_spp_89[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_89)

subdom_quad90 <- subset(sub_abund, quad_no == "90")
patchno_spp_90<- aggregate(subdom_quad90$patchID, by = list(Species = subdom_quad90$spp_duplicate), FUN = max)
patchno_spp_90 <- as.data.frame(patchno_spp_90)
patchno_spp_90 <- cbind(quad_no = 90, patchno_spp_90)
rownames(patchno_spp_90) <- patchno_spp_90$Species
patchno_spp_90 <- data.frame(t(patchno_spp_90))
patchno_spp_90 <- patchno_spp_90[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_90)

subdom_quad91 <- subset(sub_abund, quad_no == "91")
patchno_spp_91<- aggregate(subdom_quad91$patchID, by = list(Species = subdom_quad91$spp_duplicate), FUN = max)
patchno_spp_91 <- as.data.frame(patchno_spp_91)
patchno_spp_91 <- cbind(quad_no = 91, patchno_spp_91)
rownames(patchno_spp_91) <- patchno_spp_91$Species
patchno_spp_91 <- data.frame(t(patchno_spp_91))
patchno_spp_91 <- patchno_spp_91[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_91)

subdom_quad92 <- subset(sub_abund, quad_no == "92")
patchno_spp_92<- aggregate(subdom_quad92$patchID, by = list(Species = subdom_quad92$spp_duplicate), FUN = max)
patchno_spp_92 <- as.data.frame(patchno_spp_92)
patchno_spp_92 <- cbind(quad_no = 92, patchno_spp_92)
rownames(patchno_spp_92) <- patchno_spp_92$Species
patchno_spp_92 <- data.frame(t(patchno_spp_92))
patchno_spp_92 <- patchno_spp_92[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_92)

subdom_quad93 <- subset(sub_abund, quad_no == "93")
patchno_spp_93<- aggregate(subdom_quad93$patchID, by = list(Species = subdom_quad93$spp_duplicate), FUN = max)
patchno_spp_93 <- as.data.frame(patchno_spp_93)
patchno_spp_93 <- cbind(quad_no = 93, patchno_spp_93)
rownames(patchno_spp_93) <- patchno_spp_93$Species
patchno_spp_93 <- data.frame(t(patchno_spp_93))
patchno_spp_93 <- patchno_spp_93[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_93)

subdom_quad94 <- subset(sub_abund, quad_no == "94")
patchno_spp_94<- aggregate(subdom_quad94$patchID, by = list(Species = subdom_quad94$spp_duplicate), FUN = max)
patchno_spp_94 <- as.data.frame(patchno_spp_94)
patchno_spp_94 <- cbind(quad_no = 94, patchno_spp_94)
rownames(patchno_spp_94) <- patchno_spp_94$Species
patchno_spp_94 <- data.frame(t(patchno_spp_94))
patchno_spp_94 <- patchno_spp_94[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_94)

subdom_quad95 <- subset(sub_abund, quad_no == "95")
patchno_spp_95<- aggregate(subdom_quad95$patchID, by = list(Species = subdom_quad95$spp_duplicate), FUN = max)
patchno_spp_95 <- as.data.frame(patchno_spp_95)
patchno_spp_95 <- cbind(quad_no = 95, patchno_spp_95)
rownames(patchno_spp_95) <- patchno_spp_95$Species
patchno_spp_95 <- data.frame(t(patchno_spp_95))
patchno_spp_95 <- patchno_spp_95[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_95)

subdom_quad96 <- subset(sub_abund, quad_no == "96")
patchno_spp_96<- aggregate(subdom_quad96$patchID, by = list(Species = subdom_quad96$spp_duplicate), FUN = max)
patchno_spp_96 <- as.data.frame(patchno_spp_96)
patchno_spp_96 <- cbind(quad_no = 96, patchno_spp_96)
rownames(patchno_spp_96) <- patchno_spp_96$Species
patchno_spp_96 <- data.frame(t(patchno_spp_96))
patchno_spp_96 <- patchno_spp_96[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_96)

subdom_quad97 <- subset(sub_abund, quad_no == "97")
patchno_spp_97<- aggregate(subdom_quad97$patchID, by = list(Species = subdom_quad97$spp_duplicate), FUN = max)
patchno_spp_97 <- as.data.frame(patchno_spp_97)
patchno_spp_97 <- cbind(quad_no = 97, patchno_spp_97)
rownames(patchno_spp_97) <- patchno_spp_97$Species
patchno_spp_97 <- data.frame(t(patchno_spp_97))
patchno_spp_97 <- patchno_spp_97[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_97)

subdom_quad98 <- subset(sub_abund, quad_no == "98")
patchno_spp_98<- aggregate(subdom_quad98$patchID, by = list(Species = subdom_quad98$spp_duplicate), FUN = max)
patchno_spp_98 <- as.data.frame(patchno_spp_98)
patchno_spp_98 <- cbind(quad_no = 98, patchno_spp_98)
rownames(patchno_spp_98) <- patchno_spp_98$Species
patchno_spp_98 <- data.frame(t(patchno_spp_98))
patchno_spp_98 <- patchno_spp_98[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_98)

subdom_quad99 <- subset(sub_abund, quad_no == "99")
patchno_spp_99<- aggregate(subdom_quad99$patchID, by = list(Species = subdom_quad99$spp_duplicate), FUN = max)
patchno_spp_99 <- as.data.frame(patchno_spp_99)
patchno_spp_99 <- cbind(quad_no = 99, patchno_spp_99)
rownames(patchno_spp_99) <- patchno_spp_99$Species
patchno_spp_99 <- data.frame(t(patchno_spp_99))
patchno_spp_99 <- patchno_spp_99[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_99)

subdom_quad100 <- subset(sub_abund, quad_no == "100")
patchno_spp_100<- aggregate(subdom_quad100$patchID, by = list(Species = subdom_quad100$spp_duplicate), FUN = max)
patchno_spp_100 <- as.data.frame(patchno_spp_100)
patchno_spp_100 <- cbind(quad_no = 100, patchno_spp_100)
rownames(patchno_spp_100) <- patchno_spp_100$Species
patchno_spp_100 <- data.frame(t(patchno_spp_100))
patchno_spp_100 <- patchno_spp_100[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_100)

subdom_quad101 <- subset(sub_abund, quad_no == "101")
patchno_spp_101<- aggregate(subdom_quad101$patchID, by = list(Species = subdom_quad101$spp_duplicate), FUN = max)
patchno_spp_101 <- as.data.frame(patchno_spp_101)
patchno_spp_101 <- cbind(quad_no = 101, patchno_spp_101)
rownames(patchno_spp_101) <- patchno_spp_101$Species
patchno_spp_101 <- data.frame(t(patchno_spp_101))
patchno_spp_101 <- patchno_spp_101[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_101)

subdom_quad102 <- subset(sub_abund, quad_no == "102")
patchno_spp_102<- aggregate(subdom_quad102$patchID, by = list(Species = subdom_quad102$spp_duplicate), FUN = max)
patchno_spp_102 <- as.data.frame(patchno_spp_102)
patchno_spp_102 <- cbind(quad_no = 102, patchno_spp_102)
rownames(patchno_spp_102) <- patchno_spp_102$Species
patchno_spp_102 <- data.frame(t(patchno_spp_102))
patchno_spp_102 <- patchno_spp_102[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_102)

subdom_quad103 <- subset(sub_abund, quad_no == "103")
patchno_spp_103<- aggregate(subdom_quad103$patchID, by = list(Species = subdom_quad103$spp_duplicate), FUN = max)
patchno_spp_103 <- as.data.frame(patchno_spp_103)
patchno_spp_103 <- cbind(quad_no = 103, patchno_spp_103)
rownames(patchno_spp_103) <- patchno_spp_103$Species
patchno_spp_103 <- data.frame(t(patchno_spp_103))
patchno_spp_103 <- patchno_spp_103[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_103)

subdom_quad104 <- subset(sub_abund, quad_no == "104")
patchno_spp_104<- aggregate(subdom_quad104$patchID, by = list(Species = subdom_quad104$spp_duplicate), FUN = max)
patchno_spp_104 <- as.data.frame(patchno_spp_104)
patchno_spp_104 <- cbind(quad_no = 104, patchno_spp_104)
rownames(patchno_spp_104) <- patchno_spp_104$Species
patchno_spp_104 <- data.frame(t(patchno_spp_104))
patchno_spp_104 <- patchno_spp_104[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_104)

subdom_quad105 <- subset(sub_abund, quad_no == "105")
patchno_spp_105<- aggregate(subdom_quad105$patchID, by = list(Species = subdom_quad105$spp_duplicate), FUN = max)
patchno_spp_105 <- as.data.frame(patchno_spp_105)
patchno_spp_105 <- cbind(quad_no = 105, patchno_spp_105)
rownames(patchno_spp_105) <- patchno_spp_105$Species
patchno_spp_105 <- data.frame(t(patchno_spp_105))
patchno_spp_105 <- patchno_spp_105[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_105)

subdom_quad106 <- subset(sub_abund, quad_no == "106")
patchno_spp_106<- aggregate(subdom_quad106$patchID, by = list(Species = subdom_quad106$spp_duplicate), FUN = max)
patchno_spp_106 <- as.data.frame(patchno_spp_106)
patchno_spp_106 <- cbind(quad_no = 106, patchno_spp_106)
rownames(patchno_spp_106) <- patchno_spp_106$Species
patchno_spp_106 <- data.frame(t(patchno_spp_106))
patchno_spp_106 <- patchno_spp_106[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_106)

subdom_quad107 <- subset(sub_abund, quad_no == "107")
patchno_spp_107<- aggregate(subdom_quad107$patchID, by = list(Species = subdom_quad107$spp_duplicate), FUN = max)
patchno_spp_107 <- as.data.frame(patchno_spp_107)
patchno_spp_107 <- cbind(quad_no = 107, patchno_spp_107)
rownames(patchno_spp_107) <- patchno_spp_107$Species
patchno_spp_107 <- data.frame(t(patchno_spp_107))
patchno_spp_107 <- patchno_spp_107[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_107)

subdom_quad108 <- subset(sub_abund, quad_no == "108")
patchno_spp_108<- aggregate(subdom_quad108$patchID, by = list(Species = subdom_quad108$spp_duplicate), FUN = max)
patchno_spp_108 <- as.data.frame(patchno_spp_108)
patchno_spp_108 <- cbind(quad_no = 108, patchno_spp_108)
rownames(patchno_spp_108) <- patchno_spp_108$Species
patchno_spp_108 <- data.frame(t(patchno_spp_108))
patchno_spp_108 <- patchno_spp_108[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_108)

subdom_quad109 <- subset(sub_abund, quad_no == "109")
patchno_spp_109<- aggregate(subdom_quad109$patchID, by = list(Species = subdom_quad109$spp_duplicate), FUN = max)
patchno_spp_109 <- as.data.frame(patchno_spp_109)
patchno_spp_109 <- cbind(quad_no = 109, patchno_spp_109)
rownames(patchno_spp_109) <- patchno_spp_109$Species
patchno_spp_109 <- data.frame(t(patchno_spp_109))
patchno_spp_109 <- patchno_spp_109[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_109)

subdom_quad110 <- subset(sub_abund, quad_no == "110")
patchno_spp_110<- aggregate(subdom_quad110$patchID, by = list(Species = subdom_quad110$spp_duplicate), FUN = max)
patchno_spp_110 <- as.data.frame(patchno_spp_110)
patchno_spp_110 <- cbind(quad_no = 110, patchno_spp_110)
rownames(patchno_spp_110) <- patchno_spp_110$Species
patchno_spp_110 <- data.frame(t(patchno_spp_110))
patchno_spp_110 <- patchno_spp_110[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_110)

subdom_quad111 <- subset(sub_abund, quad_no == "111")
patchno_spp_111<- aggregate(subdom_quad111$patchID, by = list(Species = subdom_quad111$spp_duplicate), FUN = max)
patchno_spp_111 <- as.data.frame(patchno_spp_111)
patchno_spp_111 <- cbind(quad_no = 111, patchno_spp_111)
rownames(patchno_spp_111) <- patchno_spp_111$Species
patchno_spp_111 <- data.frame(t(patchno_spp_111))
patchno_spp_111 <- patchno_spp_111[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_111)

subdom_quad112 <- subset(sub_abund, quad_no == "112")
patchno_spp_112<- aggregate(subdom_quad112$patchID, by = list(Species = subdom_quad112$spp_duplicate), FUN = max)
patchno_spp_112 <- as.data.frame(patchno_spp_112)
patchno_spp_112 <- cbind(quad_no = 112, patchno_spp_112)
rownames(patchno_spp_112) <- patchno_spp_112$Species
patchno_spp_112 <- data.frame(t(patchno_spp_112))
patchno_spp_112 <- patchno_spp_112[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_112)

subdom_quad113 <- subset(sub_abund, quad_no == "113")
patchno_spp_113<- aggregate(subdom_quad113$patchID, by = list(Species = subdom_quad113$spp_duplicate), FUN = max)
patchno_spp_113 <- as.data.frame(patchno_spp_113)
patchno_spp_113 <- cbind(quad_no = 113, patchno_spp_113)
rownames(patchno_spp_113) <- patchno_spp_113$Species
patchno_spp_113 <- data.frame(t(patchno_spp_113))
patchno_spp_113 <- patchno_spp_113[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_113)

subdom_quad114 <- subset(sub_abund, quad_no == "114")
patchno_spp_114<- aggregate(subdom_quad114$patchID, by = list(Species = subdom_quad114$spp_duplicate), FUN = max)
patchno_spp_114 <- as.data.frame(patchno_spp_114)
patchno_spp_114 <- cbind(quad_no = 114, patchno_spp_114)
rownames(patchno_spp_114) <- patchno_spp_114$Species
patchno_spp_114 <- data.frame(t(patchno_spp_114))
patchno_spp_114 <- patchno_spp_114[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_114)

subdom_quad115 <- subset(sub_abund, quad_no == "115")
patchno_spp_115<- aggregate(subdom_quad115$patchID, by = list(Species = subdom_quad115$spp_duplicate), FUN = max)
patchno_spp_115 <- as.data.frame(patchno_spp_115)
patchno_spp_115 <- cbind(quad_no = 115, patchno_spp_115)
rownames(patchno_spp_115) <- patchno_spp_115$Species
patchno_spp_115 <- data.frame(t(patchno_spp_115))
patchno_spp_115 <- patchno_spp_115[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_115)

subdom_quad116 <- subset(sub_abund, quad_no == "116")
patchno_spp_116<- aggregate(subdom_quad116$patchID, by = list(Species = subdom_quad116$spp_duplicate), FUN = max)
patchno_spp_116 <- as.data.frame(patchno_spp_116)
patchno_spp_116 <- cbind(quad_no = 116, patchno_spp_116)
rownames(patchno_spp_116) <- patchno_spp_116$Species
patchno_spp_116 <- data.frame(t(patchno_spp_116))
patchno_spp_116 <- patchno_spp_116[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_116)

subdom_quad117 <- subset(sub_abund, quad_no == "117")
patchno_spp_117<- aggregate(subdom_quad117$patchID, by = list(Species = subdom_quad117$spp_duplicate), FUN = max)
patchno_spp_117 <- as.data.frame(patchno_spp_117)
patchno_spp_117 <- cbind(quad_no = 117, patchno_spp_117)
rownames(patchno_spp_117) <- patchno_spp_117$Species
patchno_spp_117 <- data.frame(t(patchno_spp_117))
patchno_spp_117 <- patchno_spp_117[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_117)

subdom_quad118 <- subset(sub_abund, quad_no == "118")
patchno_spp_118<- aggregate(subdom_quad118$patchID, by = list(Species = subdom_quad118$spp_duplicate), FUN = max)
patchno_spp_118 <- as.data.frame(patchno_spp_118)
patchno_spp_118 <- cbind(quad_no = 118, patchno_spp_118)
rownames(patchno_spp_118) <- patchno_spp_118$Species
patchno_spp_118 <- data.frame(t(patchno_spp_118))
patchno_spp_118 <- patchno_spp_118[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_118)

subdom_quad119 <- subset(sub_abund, quad_no == "119")
patchno_spp_119<- aggregate(subdom_quad119$patchID, by = list(Species = subdom_quad119$spp_duplicate), FUN = max)
patchno_spp_119 <- as.data.frame(patchno_spp_119)
patchno_spp_119 <- cbind(quad_no = 119, patchno_spp_119)
rownames(patchno_spp_119) <- patchno_spp_119$Species
patchno_spp_119 <- data.frame(t(patchno_spp_119))
patchno_spp_119 <- patchno_spp_119[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_119)

subdom_quad120 <- subset(sub_abund, quad_no == "120")
patchno_spp_120<- aggregate(subdom_quad120$patchID, by = list(Species = subdom_quad120$spp_duplicate), FUN = max)
patchno_spp_120 <- as.data.frame(patchno_spp_120)
patchno_spp_120 <- cbind(quad_no = 120, patchno_spp_120)
rownames(patchno_spp_120) <- patchno_spp_120$Species
patchno_spp_120 <- data.frame(t(patchno_spp_120))
patchno_spp_120 <- patchno_spp_120[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_120)

subdom_quad121 <- subset(sub_abund, quad_no == "121")
patchno_spp_121<- aggregate(subdom_quad121$patchID, by = list(Species = subdom_quad121$spp_duplicate), FUN = max)
patchno_spp_121 <- as.data.frame(patchno_spp_121)
patchno_spp_121 <- cbind(quad_no = 121, patchno_spp_121)
rownames(patchno_spp_121) <- patchno_spp_121$Species
patchno_spp_121 <- data.frame(t(patchno_spp_121))
patchno_spp_121 <- patchno_spp_121[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_121)

subdom_quad122 <- subset(sub_abund, quad_no == "122")
patchno_spp_122<- aggregate(subdom_quad122$patchID, by = list(Species = subdom_quad122$spp_duplicate), FUN = max)
patchno_spp_122 <- as.data.frame(patchno_spp_122)
patchno_spp_122 <- cbind(quad_no = 122, patchno_spp_122)
rownames(patchno_spp_122) <- patchno_spp_122$Species
patchno_spp_122 <- data.frame(t(patchno_spp_122))
patchno_spp_122 <- patchno_spp_122[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_122)

subdom_quad123 <- subset(sub_abund, quad_no == "123")
patchno_spp_123<- aggregate(subdom_quad123$patchID, by = list(Species = subdom_quad123$spp_duplicate), FUN = max)
patchno_spp_123 <- as.data.frame(patchno_spp_123)
patchno_spp_123 <- cbind(quad_no = 123, patchno_spp_123)
rownames(patchno_spp_123) <- patchno_spp_123$Species
patchno_spp_123 <- data.frame(t(patchno_spp_123))
patchno_spp_123 <- patchno_spp_123[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_123)

subdom_quad124 <- subset(sub_abund, quad_no == "124")
patchno_spp_124<- aggregate(subdom_quad124$patchID, by = list(Species = subdom_quad124$spp_duplicate), FUN = max)
patchno_spp_124 <- as.data.frame(patchno_spp_124)
patchno_spp_124 <- cbind(quad_no = 124, patchno_spp_124)
rownames(patchno_spp_124) <- patchno_spp_124$Species
patchno_spp_124 <- data.frame(t(patchno_spp_124))
patchno_spp_124 <- patchno_spp_124[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_124)

subdom_quad125 <- subset(sub_abund, quad_no == "125")
patchno_spp_125<- aggregate(subdom_quad125$patchID, by = list(Species = subdom_quad125$spp_duplicate), FUN = max)
patchno_spp_125 <- as.data.frame(patchno_spp_125)
patchno_spp_125 <- cbind(quad_no = 125, patchno_spp_125)
rownames(patchno_spp_125) <- patchno_spp_125$Species
patchno_spp_125 <- data.frame(t(patchno_spp_125))
patchno_spp_125 <- patchno_spp_125[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_125)

subdom_quad126 <- subset(sub_abund, quad_no == "126")
patchno_spp_126<- aggregate(subdom_quad126$patchID, by = list(Species = subdom_quad126$spp_duplicate), FUN = max)
patchno_spp_126 <- as.data.frame(patchno_spp_126)
patchno_spp_126 <- cbind(quad_no = 126, patchno_spp_126)
rownames(patchno_spp_126) <- patchno_spp_126$Species
patchno_spp_126 <- data.frame(t(patchno_spp_126))
patchno_spp_126 <- patchno_spp_126[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_126)

subdom_quad127 <- subset(sub_abund, quad_no == "127")
patchno_spp_127<- aggregate(subdom_quad127$patchID, by = list(Species = subdom_quad127$spp_duplicate), FUN = max)
patchno_spp_127 <- as.data.frame(patchno_spp_127)
patchno_spp_127 <- cbind(quad_no = 127, patchno_spp_127)
rownames(patchno_spp_127) <- patchno_spp_127$Species
patchno_spp_127 <- data.frame(t(patchno_spp_127))
patchno_spp_127 <- patchno_spp_127[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_127)

subdom_quad128 <- subset(sub_abund, quad_no == "128")
patchno_spp_128<- aggregate(subdom_quad128$patchID, by = list(Species = subdom_quad128$spp_duplicate), FUN = max)
patchno_spp_128 <- as.data.frame(patchno_spp_128)
patchno_spp_128 <- cbind(quad_no = 128, patchno_spp_128)
rownames(patchno_spp_128) <- patchno_spp_128$Species
patchno_spp_128 <- data.frame(t(patchno_spp_128))
patchno_spp_128 <- patchno_spp_128[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_128)

subdom_quad129 <- subset(sub_abund, quad_no == "129")
patchno_spp_129<- aggregate(subdom_quad129$patchID, by = list(Species = subdom_quad129$spp_duplicate), FUN = max)
patchno_spp_129 <- as.data.frame(patchno_spp_129)
patchno_spp_129 <- cbind(quad_no = 129, patchno_spp_129)
rownames(patchno_spp_129) <- patchno_spp_129$Species
patchno_spp_129 <- data.frame(t(patchno_spp_129))
patchno_spp_129 <- patchno_spp_129[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_129)

subdom_quad130 <- subset(sub_abund, quad_no == "130")
patchno_spp_130<- aggregate(subdom_quad130$patchID, by = list(Species = subdom_quad130$spp_duplicate), FUN = max)
patchno_spp_130 <- as.data.frame(patchno_spp_130)
patchno_spp_130 <- cbind(quad_no = 130, patchno_spp_130)
rownames(patchno_spp_130) <- patchno_spp_130$Species
patchno_spp_130 <- data.frame(t(patchno_spp_130))
patchno_spp_130 <- patchno_spp_130[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_130)

subdom_quad131 <- subset(sub_abund, quad_no == "131")
patchno_spp_131<- aggregate(subdom_quad131$patchID, by = list(Species = subdom_quad131$spp_duplicate), FUN = max)
patchno_spp_131 <- as.data.frame(patchno_spp_131)
patchno_spp_131 <- cbind(quad_no = 131, patchno_spp_131)
rownames(patchno_spp_131) <- patchno_spp_131$Species
patchno_spp_131 <- data.frame(t(patchno_spp_131))
patchno_spp_131 <- patchno_spp_131[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_131)

subdom_quad132 <- subset(sub_abund, quad_no == "132")
patchno_spp_132<- aggregate(subdom_quad132$patchID, by = list(Species = subdom_quad132$spp_duplicate), FUN = max)
patchno_spp_132 <- as.data.frame(patchno_spp_132)
patchno_spp_132 <- cbind(quad_no = 132, patchno_spp_132)
rownames(patchno_spp_132) <- patchno_spp_132$Species
patchno_spp_132 <- data.frame(t(patchno_spp_132))
patchno_spp_132 <- patchno_spp_132[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_132)

subdom_quad133 <- subset(sub_abund, quad_no == "133")
patchno_spp_133<- aggregate(subdom_quad133$patchID, by = list(Species = subdom_quad133$spp_duplicate), FUN = max)
patchno_spp_133 <- as.data.frame(patchno_spp_133)
patchno_spp_133 <- cbind(quad_no = 133, patchno_spp_133)
rownames(patchno_spp_133) <- patchno_spp_133$Species
patchno_spp_133 <- data.frame(t(patchno_spp_133))
patchno_spp_133 <- patchno_spp_133[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_133)

subdom_quad134 <- subset(sub_abund, quad_no == "134")
patchno_spp_134<- aggregate(subdom_quad134$patchID, by = list(Species = subdom_quad134$spp_duplicate), FUN = max)
patchno_spp_134 <- as.data.frame(patchno_spp_134)
patchno_spp_134 <- cbind(quad_no = 134, patchno_spp_134)
rownames(patchno_spp_134) <- patchno_spp_134$Species
patchno_spp_134 <- data.frame(t(patchno_spp_134))
patchno_spp_134 <- patchno_spp_134[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_134)

subdom_quad135 <- subset(sub_abund, quad_no == "135")
patchno_spp_135<- aggregate(subdom_quad135$patchID, by = list(Species = subdom_quad135$spp_duplicate), FUN = max)
patchno_spp_135 <- as.data.frame(patchno_spp_135)
patchno_spp_135 <- cbind(quad_no = 135, patchno_spp_135)
rownames(patchno_spp_135) <- patchno_spp_135$Species
patchno_spp_135 <- data.frame(t(patchno_spp_135))
patchno_spp_135 <- patchno_spp_135[-1,]
patchno_spp <- rbind(patchno_spp, patchno_spp_135)

subdom_quad136 <- subset(sub_abund, quad_no == "136")
patchno_spp_136<- aggregate(subdom_quad136$patchID, by = list(Species = subdom_quad136$spp_duplicate), FUN = max)
patchno_spp_136 <- as.data.frame(patchno_spp_136)
patchno_spp_136 <- cbind(quad_no = 136, patchno_spp_136)
rownames(patchno_spp_136) <- patchno_spp_136$Species
patchno_spp_136 <- data.frame(t(patchno_spp_136))
patchno_spp_136 <- patchno_spp_136[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_136)

subdom_quad137 <- subset(sub_abund, quad_no == "137")
patchno_spp_137<- aggregate(subdom_quad137$patchID, by = list(Species = subdom_quad137$spp_duplicate), FUN = max)
patchno_spp_137 <- as.data.frame(patchno_spp_137)
patchno_spp_137 <- cbind(quad_no = 137, patchno_spp_137)
rownames(patchno_spp_137) <- patchno_spp_137$Species
patchno_spp_137 <- data.frame(t(patchno_spp_137))
patchno_spp_137 <- patchno_spp_137[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_137)

subdom_quad138 <- subset(sub_abund, quad_no == "138")
patchno_spp_138<- aggregate(subdom_quad138$patchID, by = list(Species = subdom_quad138$spp_duplicate), FUN = max)
patchno_spp_138 <- as.data.frame(patchno_spp_138)
patchno_spp_138 <- cbind(quad_no = 138, patchno_spp_138)
rownames(patchno_spp_138) <- patchno_spp_138$Species
patchno_spp_138 <- data.frame(t(patchno_spp_138))
patchno_spp_138 <- patchno_spp_138[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_138)

subdom_quad139 <- subset(sub_abund, quad_no == "139")
patchno_spp_139<- aggregate(subdom_quad139$patchID, by = list(Species = subdom_quad139$spp_duplicate), FUN = max)
patchno_spp_139 <- as.data.frame(patchno_spp_139)
patchno_spp_139 <- cbind(quad_no = 139, patchno_spp_139)
rownames(patchno_spp_139) <- patchno_spp_139$Species
patchno_spp_139 <- data.frame(t(patchno_spp_139))
patchno_spp_139 <- patchno_spp_139[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_139)

subdom_quad140 <- subset(sub_abund, quad_no == "140")
patchno_spp_140<- aggregate(subdom_quad140$patchID, by = list(Species = subdom_quad140$spp_duplicate), FUN = max)
patchno_spp_140 <- as.data.frame(patchno_spp_140)
patchno_spp_140 <- cbind(quad_no = 140, patchno_spp_140)
rownames(patchno_spp_140) <- patchno_spp_140$Species
patchno_spp_140 <- data.frame(t(patchno_spp_140))
patchno_spp_140 <- patchno_spp_140[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_140)

subdom_quad141 <- subset(sub_abund, quad_no == "141")
patchno_spp_141<- aggregate(subdom_quad141$patchID, by = list(Species = subdom_quad141$spp_duplicate), FUN = max)
patchno_spp_141 <- as.data.frame(patchno_spp_141)
patchno_spp_141 <- cbind(quad_no = 141, patchno_spp_141)
rownames(patchno_spp_141) <- patchno_spp_141$Species
patchno_spp_141 <- data.frame(t(patchno_spp_141))
patchno_spp_141 <- patchno_spp_141[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_141)

subdom_quad142 <- subset(sub_abund, quad_no == "142")
patchno_spp_142<- aggregate(subdom_quad142$patchID, by = list(Species = subdom_quad142$spp_duplicate), FUN = max)
patchno_spp_142 <- as.data.frame(patchno_spp_142)
patchno_spp_142 <- cbind(quad_no = 142, patchno_spp_142)
rownames(patchno_spp_142) <- patchno_spp_142$Species
patchno_spp_142 <- data.frame(t(patchno_spp_142))
patchno_spp_142 <- patchno_spp_142[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_142)

subdom_quad143 <- subset(sub_abund, quad_no == "143")
patchno_spp_143<- aggregate(subdom_quad143$patchID, by = list(Species = subdom_quad143$spp_duplicate), FUN = max)
patchno_spp_143 <- as.data.frame(patchno_spp_143)
patchno_spp_143 <- cbind(quad_no = 143, patchno_spp_143)
rownames(patchno_spp_143) <- patchno_spp_143$Species
patchno_spp_143 <- data.frame(t(patchno_spp_143))
patchno_spp_143 <- patchno_spp_143[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_143)

subdom_quad144 <- subset(sub_abund, quad_no == "144")
patchno_spp_144<- aggregate(subdom_quad144$patchID, by = list(Species = subdom_quad144$spp_duplicate), FUN = max)
patchno_spp_144 <- as.data.frame(patchno_spp_144)
patchno_spp_144 <- cbind(quad_no = 144, patchno_spp_144)
rownames(patchno_spp_144) <- patchno_spp_144$Species
patchno_spp_144 <- data.frame(t(patchno_spp_144))
patchno_spp_144 <- patchno_spp_144[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_144)

subdom_quad145 <- subset(sub_abund, quad_no == "145")
patchno_spp_145<- aggregate(subdom_quad145$patchID, by = list(Species = subdom_quad145$spp_duplicate), FUN = max)
patchno_spp_145 <- as.data.frame(patchno_spp_145)
patchno_spp_145 <- cbind(quad_no = 145, patchno_spp_145)
rownames(patchno_spp_145) <- patchno_spp_145$Species
patchno_spp_145 <- data.frame(t(patchno_spp_145))
patchno_spp_145 <- patchno_spp_145[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_145)

subdom_quad146 <- subset(sub_abund, quad_no == "146")
patchno_spp_146<- aggregate(subdom_quad146$patchID, by = list(Species = subdom_quad146$spp_duplicate), FUN = max)
patchno_spp_146 <- as.data.frame(patchno_spp_146)
patchno_spp_146 <- cbind(quad_no = 146, patchno_spp_146)
rownames(patchno_spp_146) <- patchno_spp_146$Species
patchno_spp_146 <- data.frame(t(patchno_spp_146))
patchno_spp_146 <- patchno_spp_146[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_146)

subdom_quad147 <- subset(sub_abund, quad_no == "147")
patchno_spp_147<- aggregate(subdom_quad147$patchID, by = list(Species = subdom_quad147$spp_duplicate), FUN = max)
patchno_spp_147 <- as.data.frame(patchno_spp_147)
patchno_spp_147 <- cbind(quad_no = 147, patchno_spp_147)
rownames(patchno_spp_147) <- patchno_spp_147$Species
patchno_spp_147 <- data.frame(t(patchno_spp_147))
patchno_spp_147 <- patchno_spp_147[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_147)

subdom_quad148 <- subset(sub_abund, quad_no == "148")
patchno_spp_148<- aggregate(subdom_quad148$patchID, by = list(Species = subdom_quad148$spp_duplicate), FUN = max)
patchno_spp_148 <- as.data.frame(patchno_spp_148)
patchno_spp_148 <- cbind(quad_no = 148, patchno_spp_148)
rownames(patchno_spp_148) <- patchno_spp_148$Species
patchno_spp_148 <- data.frame(t(patchno_spp_148))
patchno_spp_148 <- patchno_spp_148[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_148)

subdom_quad149 <- subset(sub_abund, quad_no == "149")
patchno_spp_149<- aggregate(subdom_quad149$patchID, by = list(Species = subdom_quad149$spp_duplicate), FUN = max)
patchno_spp_149 <- as.data.frame(patchno_spp_149)
patchno_spp_149 <- cbind(quad_no = 149, patchno_spp_149)
rownames(patchno_spp_149) <- patchno_spp_149$Species
patchno_spp_149 <- data.frame(t(patchno_spp_149))
patchno_spp_149 <- patchno_spp_149[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_149)

subdom_quad150 <- subset(sub_abund, quad_no == "150")
patchno_spp_150<- aggregate(subdom_quad150$patchID, by = list(Species = subdom_quad150$spp_duplicate), FUN = max)
patchno_spp_150 <- as.data.frame(patchno_spp_150)
patchno_spp_150 <- cbind(quad_no = 150, patchno_spp_150)
rownames(patchno_spp_150) <- patchno_spp_150$Species
patchno_spp_150 <- data.frame(t(patchno_spp_150))
patchno_spp_150 <- patchno_spp_150[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_150)

subdom_quad151 <- subset(sub_abund, quad_no == "151")
patchno_spp_151<- aggregate(subdom_quad151$patchID, by = list(Species = subdom_quad151$spp_duplicate), FUN = max)
patchno_spp_151 <- as.data.frame(patchno_spp_151)
patchno_spp_151 <- cbind(quad_no = 151, patchno_spp_151)
rownames(patchno_spp_151) <- patchno_spp_151$Species
patchno_spp_151 <- data.frame(t(patchno_spp_151))
patchno_spp_151 <- patchno_spp_151[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_151)

subdom_quad152 <- subset(sub_abund, quad_no == "152")
patchno_spp_152<- aggregate(subdom_quad152$patchID, by = list(Species = subdom_quad152$spp_duplicate), FUN = max)
patchno_spp_152 <- as.data.frame(patchno_spp_152)
patchno_spp_152 <- cbind(quad_no = 152, patchno_spp_152)
rownames(patchno_spp_152) <- patchno_spp_152$Species
patchno_spp_152 <- data.frame(t(patchno_spp_152))
patchno_spp_152 <- patchno_spp_152[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_152)

subdom_quad153 <- subset(sub_abund, quad_no == "153")
patchno_spp_153<- aggregate(subdom_quad153$patchID, by = list(Species = subdom_quad153$spp_duplicate), FUN = max)
patchno_spp_153 <- as.data.frame(patchno_spp_153)
patchno_spp_153 <- cbind(quad_no = 153, patchno_spp_153)
rownames(patchno_spp_153) <- patchno_spp_153$Species
patchno_spp_153 <- data.frame(t(patchno_spp_153))
patchno_spp_153 <- patchno_spp_153[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_153)

subdom_quad154 <- subset(sub_abund, quad_no == "154")
patchno_spp_154<- aggregate(subdom_quad154$patchID, by = list(Species = subdom_quad154$spp_duplicate), FUN = max)
patchno_spp_154 <- as.data.frame(patchno_spp_154)
patchno_spp_154 <- cbind(quad_no = 154, patchno_spp_154)
rownames(patchno_spp_154) <- patchno_spp_154$Species
patchno_spp_154 <- data.frame(t(patchno_spp_154))
patchno_spp_154 <- patchno_spp_154[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_154)

subdom_quad155 <- subset(sub_abund, quad_no == "155")
patchno_spp_155<- aggregate(subdom_quad155$patchID, by = list(Species = subdom_quad155$spp_duplicate), FUN = max)
patchno_spp_155 <- as.data.frame(patchno_spp_155)
patchno_spp_155 <- cbind(quad_no = 155, patchno_spp_155)
rownames(patchno_spp_155) <- patchno_spp_155$Species
patchno_spp_155 <- data.frame(t(patchno_spp_155))
patchno_spp_155 <- patchno_spp_155[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_155)

subdom_quad156 <- subset(sub_abund, quad_no == "156")
patchno_spp_156<- aggregate(subdom_quad156$patchID, by = list(Species = subdom_quad156$spp_duplicate), FUN = max)
patchno_spp_156 <- as.data.frame(patchno_spp_156)
patchno_spp_156 <- cbind(quad_no = 156, patchno_spp_156)
rownames(patchno_spp_156) <- patchno_spp_156$Species
patchno_spp_156 <- data.frame(t(patchno_spp_156))
patchno_spp_156 <- patchno_spp_156[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_156)

subdom_quad157 <- subset(sub_abund, quad_no == "157")
patchno_spp_157<- aggregate(subdom_quad157$patchID, by = list(Species = subdom_quad157$spp_duplicate), FUN = max)
patchno_spp_157 <- as.data.frame(patchno_spp_157)
patchno_spp_157 <- cbind(quad_no = 157, patchno_spp_157)
rownames(patchno_spp_157) <- patchno_spp_157$Species
patchno_spp_157 <- data.frame(t(patchno_spp_157))
patchno_spp_157 <- patchno_spp_157[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_157)

subdom_quad158 <- subset(sub_abund, quad_no == "158")
patchno_spp_158<- aggregate(subdom_quad158$patchID, by = list(Species = subdom_quad158$spp_duplicate), FUN = max)
patchno_spp_158 <- as.data.frame(patchno_spp_158)
patchno_spp_158 <- cbind(quad_no = 158, patchno_spp_158)
rownames(patchno_spp_158) <- patchno_spp_158$Species
patchno_spp_158 <- data.frame(t(patchno_spp_158))
patchno_spp_158 <- patchno_spp_158[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_158)

subdom_quad159 <- subset(sub_abund, quad_no == "159")
patchno_spp_159<- aggregate(subdom_quad159$patchID, by = list(Species = subdom_quad159$spp_duplicate), FUN = max)
patchno_spp_159 <- as.data.frame(patchno_spp_159)
patchno_spp_159 <- cbind(quad_no = 159, patchno_spp_159)
rownames(patchno_spp_159) <- patchno_spp_159$Species
patchno_spp_159 <- data.frame(t(patchno_spp_159))
patchno_spp_159 <- patchno_spp_159[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_159)

subdom_quad160 <- subset(sub_abund, quad_no == "160")
patchno_spp_160<- aggregate(subdom_quad160$patchID, by = list(Species = subdom_quad160$spp_duplicate), FUN = max)
patchno_spp_160 <- as.data.frame(patchno_spp_160)
patchno_spp_160 <- cbind(quad_no = 160, patchno_spp_160)
rownames(patchno_spp_160) <- patchno_spp_160$Species
patchno_spp_160 <- data.frame(t(patchno_spp_160))
patchno_spp_160 <- patchno_spp_160[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_160)

subdom_quad161 <- subset(sub_abund, quad_no == "161")
patchno_spp_161<- aggregate(subdom_quad161$patchID, by = list(Species = subdom_quad161$spp_duplicate), FUN = max)
patchno_spp_161 <- as.data.frame(patchno_spp_161)
patchno_spp_161 <- cbind(quad_no = 161, patchno_spp_161)
rownames(patchno_spp_161) <- patchno_spp_161$Species
patchno_spp_161 <- data.frame(t(patchno_spp_161))
patchno_spp_161 <- patchno_spp_161[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_161)

subdom_quad162 <- subset(sub_abund, quad_no == "162")
patchno_spp_162<- aggregate(subdom_quad162$patchID, by = list(Species = subdom_quad162$spp_duplicate), FUN = max)
patchno_spp_162 <- as.data.frame(patchno_spp_162)
patchno_spp_162 <- cbind(quad_no = 162, patchno_spp_162)
rownames(patchno_spp_162) <- patchno_spp_162$Species
patchno_spp_162 <- data.frame(t(patchno_spp_162))
patchno_spp_162 <- patchno_spp_162[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_162)

subdom_quad163 <- subset(sub_abund, quad_no == "163")
patchno_spp_163<- aggregate(subdom_quad163$patchID, by = list(Species = subdom_quad163$spp_duplicate), FUN = max)
patchno_spp_163 <- as.data.frame(patchno_spp_163)
patchno_spp_163 <- cbind(quad_no = 163, patchno_spp_163)
rownames(patchno_spp_163) <- patchno_spp_163$Species
patchno_spp_163 <- data.frame(t(patchno_spp_163))
patchno_spp_163 <- patchno_spp_163[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_163)

subdom_quad164 <- subset(sub_abund, quad_no == "164")
patchno_spp_164<- aggregate(subdom_quad164$patchID, by = list(Species = subdom_quad164$spp_duplicate), FUN = max)
patchno_spp_164 <- as.data.frame(patchno_spp_164)
patchno_spp_164 <- cbind(quad_no = 164, patchno_spp_164)
rownames(patchno_spp_164) <- patchno_spp_164$Species
patchno_spp_164 <- data.frame(t(patchno_spp_164))
patchno_spp_164 <- patchno_spp_164[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_164)

subdom_quad165 <- subset(sub_abund, quad_no == "165")
patchno_spp_165<- aggregate(subdom_quad165$patchID, by = list(Species = subdom_quad165$spp_duplicate), FUN = max)
patchno_spp_165 <- as.data.frame(patchno_spp_165)
patchno_spp_165 <- cbind(quad_no = 165, patchno_spp_165)
rownames(patchno_spp_165) <- patchno_spp_165$Species
patchno_spp_165 <- data.frame(t(patchno_spp_165))
patchno_spp_165 <- patchno_spp_165[-1,]
patchno_spp <- rbind(patchno_spp,  patchno_spp_165)

subdom_quad166 <- subset(sub_abund, quad_no == "166")
patchno_spp_166<- aggregate(subdom_quad166$patchID, by = list(Species = subdom_quad166$spp_duplicate), FUN = max)
patchno_spp_166 <- as.data.frame(patchno_spp_166)
patchno_spp_166 <- cbind(quad_no = 166, patchno_spp_166)
rownames(patchno_spp_166) <- patchno_spp_166$Species
patchno_spp_166 <- data.frame(t(patchno_spp_166))
patchno_spp_166 <- patchno_spp_166[-1,]
patchno_spp <- rbind(patchno_spp, patchno_spp_166)

subdom_quad167 <- subset(sub_abund, quad_no == "167")
patchno_spp_167<- aggregate(subdom_quad167$patchID, by = list(Species = subdom_quad167$spp_duplicate), FUN = max)
patchno_spp_167 <- as.data.frame(patchno_spp_167)
patchno_spp_167 <- cbind(quad_no = 167, patchno_spp_167)
rownames(patchno_spp_167) <- patchno_spp_167$Species
patchno_spp_167 <- data.frame(t(patchno_spp_167))
patchno_spp_167 <- patchno_spp_167[-1,]
patchno_spp <- rbind(patchno_spp, patchno_spp_167)


write.csv(patchno_spp, file = "Results/Patch_Stats/Ordination/patchno_SUBDOM_12-08-18.csv")

################################################################################### 
##  PATCH STATS: NUMBER OF PATCHES for chosen species per quadrats               ##
###################################################################################

patch_stats_subdom <- read.csv("Results/Patch_Stats/Individual_quadrats/04-08-18_SUB.csv")
patch_stats_subdom <- patch_stats_subdom[,-1]

################################################################################### 
##                                  BLOCK FORMERS                                ##
###################################################################################

## CALLUNA VULGARIS
callvulg_subdom <- subset(patch_stats_subdom, spp_duplicate == "Callvulg")
callvulg_subdom <- subset(callvulg_subdom, patchID > 0)

patchno_callvulg_subdom<- aggregate(callvulg_subdom$patchID, by = list(quad_no = callvulg_subdom$quad_no), 
                               FUN = max) # number of patches per species
write.csv(patchno_callvulg_subdom, file = "Results/Patch_Stats/Subdominant/patchno/callvulg.csv")

## NARDUS STRICTA
nardstri_subdom <- subset(patch_stats_subdom, spp_duplicate == "Nardstri")
nardstri_subdom <- subset(nardstri_subdom, patchID > 0)

patchno_nardstri_subdom<- aggregate(nardstri_subdom$patchID, by = list(quad_no = nardstri_subdom$quad_no), 
                               FUN = max) # number of patches per species
write.csv(patchno_nardstri_subdom, file = "Results/Patch_Stats/Subdominant/patchno/nardstri.csv")

## MOLINIA CAERULEA
molicaer_subdom <- subset(patch_stats_subdom, spp_duplicate == "Molicaer")
molicaer_subdom <- subset(molicaer_subdom, patchID > 0)

patchno_molicaer_subdom<- aggregate(molicaer_subdom$patchID, by = list(quad_no = molicaer_subdom$quad_no), 
                               FUN = max) # number of patches per species
write.csv(patchno_molicaer_subdom, file = "Results/Patch_Stats/Subdominant/patchno/molicaer.csv")

## ERIOPHORUM VAGINATUM
eriovagi_subdom <- subset(patch_stats_subdom, spp_duplicate == "Eriovagi")
eriovagi_subdom <- subset(eriovagi_subdom, patchID > 0)

patchno_eriovagi_subdom<- aggregate(eriovagi_subdom$patchID, by = list(quad_no = eriovagi_subdom$quad_no), 
                               FUN = max) # number of patches per species
write.csv(patchno_eriovagi_subdom, file = "Results/Patch_Stats/Subdominant/patchno/eriovagi.csv")

## JUNCUS SQUARROSUS
juncsqua_subdom <- subset(patch_stats_subdom, spp_duplicate == "Juncsqua")
juncsqua_subdom <- subset(juncsqua_subdom, patchID > 0)

patchno_juncsqua_subdom<- aggregate(juncsqua_subdom$patchID, by = list(quad_no = juncsqua_subdom$quad_no), 
                               FUN = max) # number of patches per species
write.csv(patchno_juncsqua_subdom, file = "Results/Patch_Stats/Subdominant/patchno/juncsqua.csv")


## JUNCUS EFFUSUS
junceffu_subdom <- subset(patch_stats_subdom, spp_duplicate == "Junceffu")
junceffu_subdom <- subset(junceffu_subdom, patchID > 0)

patchno_junceffu_subdom<- aggregate(junceffu_subdom$patchID, by = list(quad_no = junceffu_subdom$quad_no), 
                               FUN = max) # number of patches per species
write.csv(patchno_junceffu_subdom, file = "Results/Patch_Stats/Subdominant/patchno/junceffu.csv")


################################################################################### 
##                                  OPPORTUNISTS                                 ##
###################################################################################
## CAREX NIGRA
carenigr_subdom <- subset(patch_stats_subdom, spp_duplicate == "Carenigr")
carenigr_subdom <- subset(carenigr_subdom, patchID > 0)

patchno_carenigr_subdom<- aggregate(carenigr_subdom$patchID, by = list(quad_no = carenigr_subdom$quad_no), 
                               FUN = max) # number of patches per species
write.csv(patchno_carenigr_subdom, file = "Results/Patch_Stats/Subdominant/patchno/carenigr.csv")


## GALIUM SAXATILE
galisaxa_subdom <- subset(patch_stats_subdom, spp_duplicate == "Galisaxa")
galisaxa_subdom <- subset(galisaxa_subdom, patchID > 0)

patchno_galisaxa_subdom<- aggregate(galisaxa_subdom$patchID, by = list(quad_no = galisaxa_subdom$quad_no), 
                               FUN = max) # number of patches per species
write.csv(patchno_galisaxa_subdom, file = "Results/Patch_Stats/Subdominant/patchno/galisaxa.csv")


## POTENTILLA ERECTA
poteerec_subdom <- subset(patch_stats_subdom, spp_duplicate == "Poteerec")
poteerec_subdom <- subset(poteerec_subdom, patchID > 0)

patchno_poteerec_subdom<- aggregate(poteerec_subdom$patchID, by = list(quad_no = poteerec_subdom$quad_no), 
                               FUN = max) # number of patches per species
write.csv(patchno_poteerec_subdom, file = "Results/Patch_Stats/Subdominant/patchno/poteerec.csv")


## DESCHAMPSIA FLEXUOSA
descflex_subdom <- subset(patch_stats_subdom, spp_duplicate == "Descflex")
descflex_subdom <- subset(descflex_subdom, patchID > 0)

patchno_descflex_subdom<- aggregate(descflex_subdom$patchID, by = list(quad_no = descflex_subdom$quad_no), 
                               FUN = max) # number of patches per species
write.csv(patchno_descflex_subdom, file = "Results/Patch_Stats/Subdominant/patchno/descflex.csv")


## VACCINIUM MYRTILLUS
vaccmyrt_subdom <- subset(patch_stats_subdom, spp_duplicate == "Vaccmyrt")
vaccmyrt_subdom <- subset(vaccmyrt_subdom, patchID > 0)

patchno_vaccmyrt_subdom<- aggregate(vaccmyrt_subdom$patchID, by = list(quad_no = vaccmyrt_subdom$quad_no), 
                               FUN = max) # number of patches per species
write.csv(patchno_vaccmyrt_subdom, file = "Results/Patch_Stats/Subdominant/patchno/vaccmyrt.csv")



################################################################################### 
##  PATCH STATS: AREA OF PATCHES for chosen species per quadrats                 ##
###################################################################################
################################################################################### 
##                                  BLOCK FORMERS                                ##
###################################################################################

## CALLUNA VULGARIS
callvulg_subdom <- subset(patch_stats_subdom, spp_duplicate == "Callvulg")
callvulg_subdom <- subset(callvulg_subdom, patchID > 0)

area_callvulg_subdom<- aggregate(callvulg_subdom$area, by = list(quad_no = callvulg_subdom$quad_no), 
                               FUN = max) # number of patches per species

## NARDUS STRICTA
nardstri_subdom <- subset(patch_stats_subdom, spp_duplicate == "Nardstri")
nardstri_subdom <- subset(nardstri_subdom, patchID > 0)

area_nardstri_subdom<- aggregate(nardstri_subdom$area, by = list(quad_no = nardstri_subdom$quad_no), 
                               FUN = max) # number of patches per species

## MOLINIA CAERULEA
molicaer_subdom <- subset(patch_stats_subdom, spp_duplicate == "Molicaer")
molicaer_subdom <- subset(molicaer_subdom, patchID > 0)

area_molicaer_subdom<- aggregate(molicaer_subdom$area, by = list(quad_no = molicaer_subdom$quad_no), 
                               FUN = max) # number of patches per species

## ERIOPHORUM VAGINATUM
eriovagi_subdom <- subset(patch_stats_subdom, spp_duplicate == "Eriovagi")
eriovagi_subdom <- subset(eriovagi_subdom, patchID > 0)

area_eriovagi_subdom<- aggregate(eriovagi_subdom$area, by = list(quad_no = eriovagi_subdom$quad_no), 
                               FUN = max) # number of patches per species

## JUNCUS SQUARROSUS
juncsqua_subdom <- subset(patch_stats_subdom, spp_duplicate == "Juncsqua")
juncsqua_subdom <- subset(juncsqua_subdom, patchID > 0)

area_juncsqua_subdom<- aggregate(juncsqua_subdom$area, by = list(quad_no = juncsqua_subdom$quad_no), 
                               FUN = max) # number of patches per species

## JUNCUS EFFUSUS
junceffu_subdom <- subset(patch_stats_subdom, spp_duplicate == "Junceffu")
junceffu_subdom <- subset(junceffu_subdom, patchID > 0)

area_junceffu_subdom<- aggregate(junceffu_subdom$area, by = list(quad_no = junceffu_subdom$quad_no), 
                               FUN = max) # number of patches per species

################################################################################### 
##                                  OPPORTUNISTS                                 ##
###################################################################################
## CAREX NIGRA
carenigr_subdom <- subset(patch_stats_subdom, spp_duplicate == "Carenigr")
carenigr_subdom <- subset(carenigr_subdom, patchID > 0)

area_carenigr_subdom<- aggregate(carenigr_subdom$area, by = list(quad_no = carenigr_subdom$quad_no), 
                               FUN = max) # number of patches per species

## GALIUM SAXATILE
galisaxa_subdom <- subset(patch_stats_subdom, spp_duplicate == "Galisaxa")
galisaxa_subdom <- subset(galisaxa_subdom, patchID > 0)

area_galisaxa_subdom<- aggregate(galisaxa_subdom$area, by = list(quad_no = galisaxa_subdom$quad_no), 
                               FUN = max) # number of patches per species

## POTENTILLA ERECTA
poteerec_subdom <- subset(patch_stats_subdom, spp_duplicate == "Poteerec")
poteerec_subdom <- subset(poteerec_subdom, patchID > 0)

area_poteerec_subdom<- aggregate(poteerec_subdom$area, by = list(quad_no = poteerec_subdom$quad_no), 
                               FUN = max) # number of patches per species

## DESCHAMPSIA FLEXUOSA
descflex_subdom <- subset(patch_stats_subdom, spp_duplicate == "Descflex")
descflex_subdom <- subset(descflex_subdom, patchID > 0)

area_descflex_subdom<- aggregate(descflex_subdom$area, by = list(quad_no = descflex_subdom$quad_no), 
                               FUN = max) # number of patches per species

## VACCINIUM MYRTILLUS
vaccmyrt_subdom <- subset(patch_stats_subdom, spp_duplicate == "Vaccmyrt")
vaccmyrt_subdom <- subset(vaccmyrt_subdom, patchID > 0)

area_vaccmyrt_subdom<- aggregate(vaccmyrt_subdom$area, by = list(quad_no = vaccmyrt_subdom$quad_no), 
                               FUN = max) # number of patches per species


################################################################################### 
##  PATCH STATS: PER:AREA RATIO for chosen species per quadrats                  ##
###################################################################################



################################################################################### 
##  PATCH STATS: SHAPE INDEX for chosen species per quadrats                     ##
###################################################################################

################################################################################### 
##              READING SPECIES CSV WITH SEQUENCE QUADRAT NO                     ##
###################################################################################
##### PATCH NUMBER
## CALLUNA VULGARIS
patchno_callvulg_subdom <- read.csv("Results/Patch_Stats/Subdominant/patchno/callvulg.csv")
patchno_callvulg_subdom[is.na(patchno_callvulg_subdom)] <- 0
patchno_callvulg_subdom <- patchno_callvulg_subdom[,-1]
barplot(patchno_callvulg_subdom$x, names.arg = patchno_callvulg_subdom$quad_no, cex.axis = 0.7,
        xlab = "Quadrat number", ylab = "Number of Patches", main = "No. of patches C. vulgaris")
hist_patchno_callvulg <- hist(patchno_callvulg_subdom$x, xlab = "Number of patches")
xfit<-seq(min(patchno_callvulg_subdom$x),max(patchno_callvulg_subdom$x),length=8) 
yfit<-dnorm(xfit,mean=mean(patchno_callvulg_subdom$x),sd=sd(patchno_callvulg_subdom$x)) 
yfit <- yfit * diff(hist_patchno_callvulg$mids[1:2])*length(patchno_callvulg_subdom$x)
plot(hist_patchno_callvulg, main = "Number of Patches C. vulgaris", xlab = "Number of Patches")
lines(xfit, yfit, col = "black", lwd = 2)

density_callvulg <- density(patchno_callvulg_subdom$x)
plot(density_callvulg, main = "No. of Patches C. vulgaris", xlim = c(0, max(patchno_callvulg_subdom$x)))

pdf("Results/Plots/Frequency-Distribution/Subdominant/Patchno/Callvulg.pdf")
dev.off()


## NARDUS STRICTA
patchno_nardstri_subdom <- read.csv("Results/Patch_Stats/Subdominant/patchno/nardstri.csv")
patchno_nardstri_subdom[is.na(patchno_nardstri_subdom)] <- 0
patchno_nardstri_subdom <- patchno_nardstri_subdom[,-1]
barplot(patchno_nardstri_subdom$x, names.arg = patchno_nardstri_subdom$quad_no, cex.axis = 0.7,
        xlab = "Quadrat number", ylab = "Number of Patches", main = "No. of patches N. stricta")
hist_patchno_nardstri <- hist(patchno_nardstri_subdom$x, xlab = "Number of patches")
xfit<-seq(min(patchno_nardstri_subdom$x),max(patchno_nardstri_subdom$x),length=8) 
yfit<-dnorm(xfit,mean=mean(patchno_nardstri_subdom$x),sd=sd(patchno_nardstri_subdom$x)) 
yfit <- yfit * diff(hist_patchno_nardstri$mids[1:2])*length(patchno_nardstri_subdom$x)
plot(hist_patchno_nardstri, main = "Number of Patches N. stricta", xlab = "Number of Patches")
lines(xfit, yfit, col = "black", lwd = 2)

density_nardstri <- density(patchno_nardstri_subdom$x)
plot(density_nardstri, main = "No. of Patches N. stricta", xlim = c(0, max(patchno_nardstri_subdom$x)))

pdf("Results/Plots/Frequency-Distribution/Subdominant/Patchno/Nardstri.pdf")
dev.off()

### MOLINIA CAERULEA
patchno_molicaer_subdom <- read.csv("Results/Patch_Stats/Subdominant/patchno/molicaer.csv")
patchno_molicaer_subdom <- read.csv("Results/Patch_Stats/Subdominant/patchno/molicaer.csv")
patchno_molicaer_subdom[is.na(patchno_molicaer_subdom)] <- 0
patchno_molicaer_subdom <- patchno_molicaer_subdom[,-1]
barplot(patchno_molicaer_subdom$x, names.arg = patchno_molicaer_subdom$quad_no, cex.axis = 0.7,
        xlab = "Quadrat number", ylab = "Number of Patches", main = "No. of patches M. caerulea")
hist_patchno_molicaer <- hist(patchno_molicaer_subdom$x, xlab = "Number of patches")
xfit<-seq(min(patchno_molicaer_subdom$x),max(patchno_molicaer_subdom$x),length=8) 
yfit<-dnorm(xfit,mean=mean(patchno_molicaer_subdom$x),sd=sd(patchno_molicaer_subdom$x)) 
yfit <- yfit * diff(hist_patchno_molicaer$mids[1:2])*length(patchno_molicaer_subdom$x)
plot(hist_patchno_molicaer, main = "Number of Patches M. caerulea", xlab = "Number of Patches")
lines(xfit, yfit, col = "black", lwd = 2)

density_molicaer <- density(patchno_molicaer_subdom$x)
plot(density_molicaer, main = "No. of Patches M. caerulea", xlim = c(0, max(patchno_molicaer_subdom$x)))

pdf("Results/Plots/Frequency-Distribution/Subdominant/Patchno/Molicaer.pdf")
dev.off()

## ERIOPHORUM VAGINATUM
patchno_eriovagi_subdom <- read.csv("Results/Patch_Stats/Subdominant/patchno/eriovagi.csv")
patchno_eriovagi_subdom[is.na(patchno_eriovagi_subdom)] <- 0
patchno_eriovagi_subdom <- patchno_eriovagi_subdom[,-1]
barplot(patchno_eriovagi_subdom$x, names.arg = patchno_eriovagi_subdom$quad_no, cex.axis = 0.7,
        xlab = "Quadrat number", ylab = "Number of Patches", main = "No. of patches E. vaginatum")
hist_patchno_eriovagi <- hist(patchno_eriovagi_subdom$x, xlab = "Number of patches")
xfit<-seq(min(patchno_eriovagi_subdom$x),max(patchno_eriovagi_subdom$x),length=8) 
yfit<-dnorm(xfit,mean=mean(patchno_eriovagi_subdom$x),sd=sd(patchno_eriovagi_subdom$x)) 
yfit <- yfit * diff(hist_patchno_eriovagi$mids[1:2])*length(patchno_eriovagi_subdom$x)
plot(hist_patchno_eriovagi, main = "Number of Patches E. vaginatum", xlab = "Number of Patches")
lines(xfit, yfit, col = "black", lwd = 2)

density_eriovagi <- density(patchno_eriovagi_subdom$x)
plot(density_eriovagi, main = "No. of Patches E. vaginatum", xlim = c(0, max(patchno_eriovagi_subdom$x)))

pdf("Results/Plots/Frequency-Distribution/Subdominant/Patchno/Eriovagi.pdf")
dev.off()

## JUNCUS SQUARROSUS
patchno_juncsqua_subdom <- read.csv("Results/Patch_Stats/Subdominant/patchno/juncsqua.csv")
patchno_juncsqua_subdom[is.na(patchno_juncsqua_subdom)] <- 0
patchno_juncsqua_subdom <- patchno_juncsqua_subdom[,-1]
barplot(patchno_juncsqua_subdom$x, names.arg = patchno_juncsqua_subdom$quad_no, cex.axis = 0.7,
        xlab = "Quadrat number", ylab = "Number of Patches", main = "No. of patches J. squarrosus")
hist_patchno_juncsqua <- hist(patchno_juncsqua_subdom$x, xlab = "Number of patches")
xfit<-seq(min(patchno_juncsqua_subdom$x),max(patchno_juncsqua_subdom$x),length=8) 
yfit<-dnorm(xfit,mean=mean(patchno_juncsqua_subdom$x),sd=sd(patchno_juncsqua_subdom$x)) 
yfit <- yfit * diff(hist_patchno_juncsqua$mids[1:2])*length(patchno_juncsqua_subdom$x)
plot(hist_patchno_juncsqua, main = "Number of Patches J. squarrosus", xlab = "Number of Patches")
lines(xfit, yfit, col = "black", lwd = 2)

density_juncsqua <- density(patchno_juncsqua_subdom$x)
plot(density_juncsqua, main = "No. of Patches J. squarrosus", xlim = c(0, max(patchno_juncsqua_subdom$x)))

pdf("Results/Plots/Frequency-Distribution/Subdominant/Patchno/Juncsqua.pdf")
dev.off()

## JUNCUS EFFUSUS
patchno_junceffu_subdom <- read.csv("Results/Patch_Stats/Subdominant/patchno/junceffu.csv")
patchno_junceffu_subdom[is.na(patchno_junceffu_subdom)] <- 0
patchno_junceffu_subdom <- patchno_junceffu_subdom[,-1]
barplot(patchno_junceffu_subdom$x, names.arg = patchno_junceffu_subdom$quad_no, cex.axis = 0.7,
        xlab = "Quadrat number", ylab = "Number of Patches", main = "No. of patches J. effusus")
hist_patchno_junceffu <- hist(patchno_junceffu_subdom$x, xlab = "Number of patches")
xfit<-seq(min(patchno_junceffu_subdom$x),max(patchno_junceffu_subdom$x),length=8) 
yfit<-dnorm(xfit,mean=mean(patchno_junceffu_subdom$x),sd=sd(patchno_junceffu_subdom$x)) 
yfit <- yfit * diff(hist_patchno_junceffu$mids[1:2])*length(patchno_junceffu_subdom$x)
plot(hist_patchno_junceffu, main = "Number of Patches J. effusus", xlab = "Number of Patches")
lines(xfit, yfit, col = "black", lwd = 2)

density_junceffu <- density(patchno_junceffu_subdom$x)
plot(density_junceffu, main = "No. of Patches J. effusus", xlim = c(0, max(patchno_junceffu_subdom$x)))

pdf("Results/Plots/Frequency-Distribution/Subdominant/Patchno/Junceffu.pdf")
dev.off()

## CAREX NIGRA
patchno_carenigr_subdom <- read.csv("Results/Patch_Stats/Subdominant/patchno/carenigr.csv")
patchno_carenigr_subdom[is.na(patchno_carenigr_subdom)] <- 0
patchno_carenigr_subdom <- patchno_carenigr_subdom[,-1]
barplot(patchno_carenigr_subdom$x, names.arg = patchno_carenigr_subdom$quad_no, cex.axis = 0.7,
        xlab = "Quadrat number", ylab = "Number of Patches", main = "No. of patches C. nigra")
hist_patchno_carenigr <- hist(patchno_carenigr_subdom$x, xlab = "Number of patches")
xfit<-seq(min(patchno_carenigr_subdom$x),max(patchno_carenigr_subdom$x),length=8) 
yfit<-dnorm(xfit,mean=mean(patchno_carenigr_subdom$x),sd=sd(patchno_carenigr_subdom$x)) 
yfit <- yfit * diff(hist_patchno_carenigr$mids[1:2])*length(patchno_carenigr_subdom$x)
plot(hist_patchno_carenigr, main = "Number of Patches C. nigra", xlab = "Number of Patches")
lines(xfit, yfit, col = "black", lwd = 2)

density_carenigr <- density(patchno_carenigr_subdom$x)
plot(density_carenigr, main = "No. of Patches C. nigra", xlim = c(0, max(patchno_carenigr_subdom$x)))

pdf("Results/Plots/Frequency-Distribution/Subdominant/Patchno/Carenigr.pdf")
dev.off()

## GALIUM SAXATILE 
patchno_galisaxa_subdom <- read.csv("Results/Patch_Stats/Subdominant/patchno/galisaxa.csv")
patchno_galisaxa_subdom[is.na(patchno_galisaxa_subdom)] <- 0
patchno_galisaxa_subdom <- patchno_galisaxa_subdom[,-1]
barplot(patchno_galisaxa_subdom$x, names.arg = patchno_galisaxa_subdom$quad_no, cex.axis = 0.7,
        xlab = "Quadrat number", ylab = "Number of Patches", main = "No. of patches G. saxatile")
hist_patchno_galisaxa <- hist(patchno_galisaxa_subdom$x, xlab = "Number of patches")
xfit<-seq(min(patchno_galisaxa_subdom$x),max(patchno_galisaxa_subdom$x),length=8) 
yfit<-dnorm(xfit,mean=mean(patchno_galisaxa_subdom$x),sd=sd(patchno_galisaxa_subdom$x)) 
yfit <- yfit * diff(hist_patchno_galisaxa$mids[1:2])*length(patchno_galisaxa_subdom$x)
plot(hist_patchno_galisaxa, main = "Number of Patches G. saxatile", xlab = "Number of Patches")
lines(xfit, yfit, col = "black", lwd = 2)

density_galisaxa <- density(patchno_galisaxa_subdom$x)
plot(density_galisaxa, main = "No. of Patches N. stricta", xlim = c(0, max(patchno_galisaxa_subdom$x)))

pdf("Results/Plots/Frequency-Distribution/Subdominant/Patchno/Galisaxa.pdf")
dev.off()

## POTENTILLA ERECTA
patchno_poteerec_subdom <- read.csv("Results/Patch_Stats/Subdominant/patchno/poteerec.csv")
patchno_poteerec_subdom[is.na(patchno_poteerec_subdom)] <- 0
patchno_poteerec_subdom <- patchno_poteerec_subdom[,-1]
barplot(patchno_poteerec_subdom$x, names.arg = patchno_poteerec_subdom$quad_no, cex.axis = 0.7,
        xlab = "Quadrat number", ylab = "Number of Patches", main = "No. of patches P. erecta")
hist_patchno_poteerec <- hist(patchno_poteerec_subdom$x, xlab = "Number of patches")
xfit<-seq(min(patchno_poteerec_subdom$x),max(patchno_poteerec_subdom$x),length=8) 
yfit<-dnorm(xfit,mean=mean(patchno_poteerec_subdom$x),sd=sd(patchno_poteerec_subdom$x)) 
yfit <- yfit * diff(hist_patchno_poteerec$mids[1:2])*length(patchno_poteerec_subdom$x)
plot(hist_patchno_poteerec, main = "Number of Patches P. erecta", xlab = "Number of Patches")
lines(xfit, yfit, col = "black", lwd = 2)

density_poteerec <- density(patchno_poteerec_subdom$x)
plot(density_poteerec, main = "No. of Patches P. erecta", xlim = c(0, max(patchno_poteerec_subdom$x)))

pdf("Results/Plots/Frequency-Distribution/Subdominant/Patchno/Perecta.pdf")
dev.off()


## DESCHAMPSIA FLEXUOSA
patchno_descflex_subdom <- read.csv("Results/Patch_Stats/Subdominant/patchno/descflex.csv")
patchno_descflex_subdom[is.na(patchno_descflex_subdom)] <- 0
patchno_descflex_subdom <- patchno_descflex_subdom[,-1]
barplot(patchno_descflex_subdom$x, names.arg = patchno_descflex_subdom$quad_no, cex.axis = 0.7,
        xlab = "Quadrat number", ylab = "Number of Patches", main = "No. of patches D. flexuosa")
hist_patchno_descflex <- hist(patchno_descflex_subdom$x, xlab = "Number of patches")
xfit<-seq(min(patchno_descflex_subdom$x),max(patchno_descflex_subdom$x),length=8) 
yfit<-dnorm(xfit,mean=mean(patchno_descflex_subdom$x),sd=sd(patchno_descflex_subdom$x)) 
yfit <- yfit * diff(hist_patchno_descflex$mids[1:2])*length(patchno_descflex_subdom$x)
plot(hist_patchno_descflex, main = "Number of Patches D. flexuosa", xlab = "Number of Patches")
lines(xfit, yfit, col = "black", lwd = 2)
density_descflex <- density(patchno_descflex_subdom$x)
plot(density_descflex, main = "No. of Patches D. flexuosa", xlim = c(0, max(patchno_descflex_subdom$x)))

pdf("Results/Plots/Frequency-Distribution/Subdominant/Patchno/Descflex.pdf")
dev.off()

## VACCINIUM MYRTILLUS
patchno_vaccmyrt_subdom <- read.csv("Results/Patch_Stats/Subdominant/patchno/vaccmyrt.csv")
patchno_vaccmyrt_subdom[is.na(patchno_vaccmyrt_subdom)] <- 0
patchno_vaccmyrt_subdom <- patchno_vaccmyrt_subdom[,-1]
barplot(patchno_vaccmyrt_subdom$x, names.arg = patchno_vaccmyrt_subdom$quad_no, cex.axis = 0.7,
        xlab = "Quadrat number", ylab = "Number of Patches", main = "No. of patches V. myrtillus")
hist_patchno_vaccmyrt <- hist(patchno_vaccmyrt_subdom$x, xlab = "Number of patches")
xfit<-seq(min(patchno_vaccmyrt_subdom$x),max(patchno_vaccmyrt_subdom$x),length=8) 
yfit<-dnorm(xfit,mean=mean(patchno_vaccmyrt_subdom$x),sd=sd(patchno_vaccmyrt_subdom$x)) 
yfit <- yfit * diff(hist_patchno_vaccmyrt$mids[1:2])*length(patchno_vaccmyrt_subdom$x)
plot(hist_patchno_vaccmyrt, main = "Number of Patches V. myrtillus", xlab = "Number of Patches")
lines(xfit, yfit, col = "black", lwd = 2)

density_vaccmyrt <- density(patchno_vaccmyrt_subdom$x)
plot(density_vaccmyrt, main = "No. of Patches V. myrtillus", xlim = c(0, max(patchno_vaccmyrt_subdom$x)))

pdf("Results/Plots/Frequency-Distribution/Subdominant/Patchno/Vaccmyrt.pdf")
dev.off()


################################################################################### 
##      LINEAR ANALYSIS: % COVER AND PATCH NUMBER PER CHOSEN SPECIES             ##
###################################################################################


##################################################################################### 
## SUBSETTING SPECIES: MEANS PER PARCH PER SPECIES SPECIES IRRISPECTIVE OF QUADRAT ##
#####################################################################################
spat_all_sub <- data.frame(read.csv("Results/Patch_Stats/Individual_quadrats/04-08-18_SUB.csv"))
quad_sub_mat_all <- as.matrix(spat_all_sub[,-1])
quad_sub_tab_all <- table(quad_sub_mat_all[,2]) # gives count of each species in the quadrat
quad_sub_tab_all

agrocapi_sub <- subset(spat_all_sub, spp_duplicate == "Agrocapi")
agrocapi_subset <- subset(agrocapi_sub, patchID > 0)
agrocapi_sum <- colSums(agrocapi_subset[,-1:-3])
agrocapi_mean <- colMeans(agrocapi_subset[,-1:-3])
agrocapi_mean_patch <- mean(agrocapi_subset$patchID)

agrocapi_unique_patch_no <- 1:nrow(agrocapi_subset)
agrocapi_patch_unique <- cbind(agrocapi_unique_patch_no, agrocapi_subset)
agrocapi_max_patch <- max(agrocapi_patch_unique$agrocapi_unique_patch_no)
agrocapi_all <- c(agrocapi_sum, max_patch = agrocapi_max_patch)
#write.csv(agrocapi_sub, file = "Results/Patch_Stats/Species_results/agrocapi_sub.csv")

agrostolo_sub <- subset(spat_all_sub, spp_duplicate == "Agrostolo")
agrostolo_subset <- subset(agrostolo_sub, patchID > 0)
agrostolo_sum <- colSums(agrostolo_subset[,-1:-3])
agrostolo_mean <- colMeans(agrostolo_subset[,-1:-3])
agrostolo_mean_patch <- mean(agrostolo_subset$patchID)

agrostolo_unique_patch_no <- 1:nrow(agrostolo_subset)
agrostolo_patch_unique <- cbind(agrostolo_unique_patch_no, agrostolo_subset)
agrostolo_max_patch <- max(agrostolo_patch_unique$agrostolo_unique_patch_no)
agrostolo_all <- c(agrostolo_sum, max_patch = agrostolo_max_patch)
#write.csv(agrostolo_sub, file = "Results/Patch_Stats/Species_results/agrostolo_sub.csv")

anthodor_sub <- subset(spat_all_sub, spp_duplicate == "Anthodor")
anthodor_subset <- subset(anthodor_sub, patchID > 0)
anthodor_sum <- colSums(anthodor_subset[,-1:-3])
anthodor_mean <- colMeans(anthodor_subset[,-1:-3])
anthodor_mean_patch <- mean(anthodor_subset$patchID)

anthodor_unique_patch_no <- 1:nrow(anthodor_subset)
anthodor_patch_unique <- cbind(anthodor_unique_patch_no, anthodor_subset)
anthodor_max_patch <- max(anthodor_patch_unique$anthodor_unique_patch_no)
anthodor_all <- c(anthodor_sum, max_patch = anthodor_max_patch)
#write.csv(anthodor_sub, file = "Results/Patch_Stats/Species_results/anthodor_sub.csv")

aulopalu_sub <- subset(spat_all_sub, spp_duplicate == "Aulopalu")
aulopalu_subset <- subset(aulopalu_sub, patchID > 0)
aulopalu_sum <- colSums(aulopalu_subset[,-1:-3])
aulopalu_mean <- colMeans(aulopalu_subset[,-1:-3])
aulopalu_mean_patch <- mean(aulopalu_subset$patchID)

aulopalu_unique_patch_no <- 1:nrow(aulopalu_subset)
aulopalu_patch_unique <- cbind(aulopalu_unique_patch_no, aulopalu_subset)
aulopalu_max_patch <- max(aulopalu_patch_unique$aulopalu_unique_patch_no)
aulopalu_all <- c(aulopalu_sum, max_patch = aulopalu_max_patch)
#write.csv(aulopalu_sub, file = "Results/Patch_Stats/Species_results/aulopalu_sub.csv")

BARE_sub <- subset(spat_all_sub, spp_duplicate == "BARE")
BARE_subset <- subset(BARE_sub, patchID > 0)
BARE_sum <- colSums(BARE_subset[,-1:-3])
BARE_mean <- colMeans(BARE_subset[,-1:-3])
BARE_mean_patch <- mean(BARE_subset$patchID)

BARE_unique_patch_no <- 1:nrow(BARE_subset)
BARE_patch_unique <- cbind(BARE_unique_patch_no, BARE_subset)
BARE_max_patch <- max(BARE_patch_unique$BARE_unique_patch_no)
BARE_all <- c(BARE_sum, max_patch = BARE_max_patch)
#write.csv(BARE_sub, file = "Results/Patch_Stats/Species_results/BARE_sub.csv")

callvulg_sub <- subset(spat_all_sub, spp_duplicate == "Callvulg")
callvulg_subset <- subset(callvulg_sub, patchID > 0)
callvulg_sum <- colSums(callvulg_subset[,-1:-3])
callvulg_mean <- colMeans(callvulg_subset[,-1:-3])
callvulg_mean_patch <- mean(callvulg_subset$patchID)

callvulg_unique_patch_no <- 1:nrow(callvulg_subset)
callvulg_patch_unique <- cbind(callvulg_unique_patch_no, callvulg_subset)
callvulg_max_patch <- max(callvulg_patch_unique$callvulg_unique_patch_no)
callvulg_all <- c(callvulg_sum, max_patch = callvulg_max_patch)
#write.csv(callvulg_sub, file = "Results/Patch_Stats/Species_results/callvulg_sub.csv")

cardprat_sub <- subset(spat_all_sub, spp_duplicate == "Cardprat")
cardprat_subset <- subset(cardprat_sub, patchID > 0)
cardprat_sum <- colSums(cardprat_subset[,-1:-3])
cardprat_mean <- colMeans(cardprat_subset[,-1:-3])
cardprat_mean_patch <- mean(cardprat_subset$patchID)

cardprat_unique_patch_no <- 1:nrow(cardprat_subset)
cardprat_patch_unique <- cbind(cardprat_unique_patch_no, cardprat_subset)
cardprat_max_patch <- max(cardprat_patch_unique$cardprat_unique_patch_no)
cardprat_all <- c(cardprat_sum, max_patch = cardprat_max_patch)
#write.csv(cardprat_sub, file = "Results/Patch_Stats/Species_results/cardprat_sub.csv")

careechi_sub <- subset(spat_all_sub, spp_duplicate == "Careechi")
careechi_subset <- subset(careechi_sub, patchID > 0)
careechi_sum <- colSums(careechi_subset[,-1:-3])
careechi_mean <- colMeans(careechi_subset[,-1:-3])
careechi_mean_patch <- mean(careechi_subset$patchID)

careechi_unique_patch_no <- 1:nrow(careechi_subset)
careechi_patch_unique <- cbind(careechi_unique_patch_no, careechi_subset)
careechi_max_patch <- max(careechi_patch_unique$careechi_unique_patch_no)
careechi_all <- c(careechi_sum, max_patch = careechi_max_patch)
#write.csv(careechi_sub, file = "Results/Patch_Stats/Species_results/careechi_sub.csv")

carenigr_sub <- subset(spat_all_sub, spp_duplicate == "Carenigr")
carenigr_subset <- subset(carenigr_sub, patchID > 0)
carenigr_sum <- colSums(carenigr_subset[,-1:-3])
carenigr_mean <- colMeans(carenigr_subset[,-1:-3])
carenigr_mean_patch <- mean(carenigr_subset$patchID)

carenigr_unique_patch_no <- 1:nrow(carenigr_subset)
carenigr_patch_unique <- cbind(carenigr_unique_patch_no, carenigr_subset)
carenigr_max_patch <- max(carenigr_patch_unique$carenigr_unique_patch_no)
carenigr_all <- c(carenigr_sum, max_patch = carenigr_max_patch)
#write.csv(carenigr_sub, file = "Results/Patch_Stats/Species_results/carenigr_sub.csv")

carepani_sub <- subset(spat_all_sub, spp_duplicate == "Carepani")
carepani_subset <- subset(carepani_sub, patchID > 0)
carepani_sum <- colSums(carepani_subset[,-1:-3])
carepani_mean <- colMeans(carepani_subset[,-1:-3])
carepani_mean_patch <- mean(carepani_subset$patchID)

carepani_unique_patch_no <- 1:nrow(carepani_subset)
carepani_patch_unique <- cbind(carepani_unique_patch_no, carepani_subset)
carepani_max_patch <- max(carepani_patch_unique$carepani_unique_patch_no)
carepani_all <- c(carepani_sum, max_patch = carepani_max_patch)
#write.csv(carepani_sub, file = "Results/Patch_Stats/Species_results/carepani_sub.csv")

carepilu_sub <- subset(spat_all_sub, spp_duplicate == "Carepilu")
carepilu_subset <- subset(carepilu_sub, patchID > 0)
carepilu_sum <- colSums(carepilu_subset[,-1:-3])
carepilu_mean <- colMeans(carepilu_subset[,-1:-3])
carepilu_mean_patch <- mean(carepilu_subset$patchID)

carepilu_unique_patch_no <- 1:nrow(carepilu_subset)
carepilu_patch_unique <- cbind(carepilu_unique_patch_no, carepilu_subset)
carepilu_max_patch <- max(carepilu_patch_unique$carepilu_unique_patch_no)
carepilu_all <- c(carepilu_sum, max_patch = carepilu_max_patch)
#write.csv(carepilu_sub, file = "Results/Patch_Stats/Species_results/carepilu_sub.csv")

ceraarve_sub <- subset(spat_all_sub, spp_duplicate == "Ceraarve")
ceraarve_subset <- subset(ceraarve_sub, patchID > 0)
ceraarve_sum <- colSums(ceraarve_subset[,-1:-3])
ceraarve_mean <- colMeans(ceraarve_subset[,-1:-3])
ceraarve_mean_patch <- mean(ceraarve_subset$patchID)

ceraarve_unique_patch_no <- 1:nrow(ceraarve_subset)
ceraarve_patch_unique <- cbind(ceraarve_unique_patch_no, ceraarve_subset)
ceraarve_max_patch <- max(ceraarve_patch_unique$ceraarve_unique_patch_no)
ceraarve_all <- c(ceraarve_sum, max_patch = ceraarve_max_patch)
#write.csv(ceraarve_sub, file = "Results/Patch_Stats/Species_results/ceraarve_sub.csv")

cirsarve_sub <- subset(spat_all_sub, spp_duplicate == "Cirsarve")
cirsarve_subset <- subset(cirsarve_sub, patchID > 0)
cirsarve_sum <- colSums(cirsarve_subset[,-1:-3])
cirsarve_mean <- colMeans(cirsarve_subset[,-1:-3])
cirsarve_mean_patch <- mean(cirsarve_subset$patchID)

cirsarve_unique_patch_no <- 1:nrow(cirsarve_subset)
cirsarve_patch_unique <- cbind(cirsarve_unique_patch_no, cirsarve_subset)
cirsarve_max_patch <- max(cirsarve_patch_unique$cirsarve_unique_patch_no)
cirsarve_all <- c(cirsarve_sum, max_patch = cirsarve_max_patch)
#write.csv(cirsarve_sub, file = "Results/Patch_Stats/Species_results/cirsarve_sub.csv")

cirspalu_sub <- subset(spat_all_sub, spp_duplicate == "Cirspalu")
cirspalu_subset <- subset(cirspalu_sub, patchID > 0)
cirspalu_sum <- colSums(cirspalu_subset[,-1:-3])
cirspalu_mean <- colMeans(cirspalu_subset[,-1:-3])
cirspalu_mean_patch <- mean(cirspalu_subset$patchID)

cirspalu_unique_patch_no <- 1:nrow(cirspalu_subset)
cirspalu_patch_unique <- cbind(cirspalu_unique_patch_no, cirspalu_subset)
cirspalu_max_patch <- max(cirspalu_patch_unique$cirspalu_unique_patch_no)
cirspalu_all <- c(cirspalu_sum, max_patch = cirspalu_max_patch)
#write.csv(cirspalu_sub, file = "Results/Patch_Stats/Species_results/cirspalu_sub.csv")

cynocris_sub <- subset(spat_all_sub, spp_duplicate == "Cynocris")
cynocris_subset <- subset(cynocris_sub, patchID > 0)
cynocris_sum <- colSums(cynocris_subset[,-1:-3])
cynocris_mean <- colMeans(cynocris_subset[,-1:-3])
cynocris_mean_patch <- mean(cynocris_subset$patchID)

cynocris_unique_patch_no <- 1:nrow(cynocris_subset)
cynocris_patch_unique <- cbind(cynocris_unique_patch_no, cynocris_subset)
cynocris_max_patch <- max(cynocris_patch_unique$cynocris_unique_patch_no)
cynocris_all <- c(cynocris_sum, max_patch = cynocris_max_patch)
#write.csv(cynocris_sub, file = "Results/Patch_Stats/Species_results/cynocris_sub.csv")

DEAD_sub <- subset(spat_all_sub, spp_duplicate == "DEAD")
DEAD_subset <- subset(DEAD_sub, patchID > 0)
DEAD_sum <- colSums(DEAD_subset[,-1:-3])
DEAD_mean <- colMeans(DEAD_subset[,-1:-3])
DEAD_mean_patch <- mean(DEAD_subset$patchID)

DEAD_unique_patch_no <- 1:nrow(DEAD_subset)
DEAD_patch_unique <- cbind(DEAD_unique_patch_no, DEAD_subset)
DEAD_max_patch <- max(DEAD_patch_unique$DEAD_unique_patch_no)
DEAD_all <- c(DEAD_sum, max_patch = DEAD_max_patch)
#write.csv(DEAD_sub, file = "Results/Patch_Stats/Species_results/DEAD_sub.csv")

desccesp_sub <- subset(spat_all_sub, spp_duplicate == "Desccesp")
desccesp_subset <- subset(desccesp_sub, patchID > 0)
desccesp_sum <- colSums(desccesp_subset[,-1:-3])
desccesp_mean <- colMeans(desccesp_subset[,-1:-3])
desccesp_mean_patch <- mean(desccesp_subset$patchID)

desccesp_unique_patch_no <- 1:nrow(desccesp_subset)
desccesp_patch_unique <- cbind(desccesp_unique_patch_no, desccesp_subset)
desccesp_max_patch <- max(desccesp_patch_unique$desccesp_unique_patch_no)
desccesp_all <- c(desccesp_sum, max_patch = desccesp_max_patch)
#write.csv(desccesp_sub, file = "Results/Patch_Stats/Species_results/desccesp_sub.csv")

descflex_sub <- subset(spat_all_sub, spp_duplicate == "Descflex")
descflex_subset <- subset(descflex_sub, patchID > 0)
descflex_sum <- colSums(descflex_subset[,-1:-3])
descflex_mean <- colMeans(descflex_subset[,-1:-3])
descflex_mean_patch <- mean(descflex_subset$patchID)

descflex_unique_patch_no <- 1:nrow(descflex_subset)
descflex_patch_unique <- cbind(descflex_unique_patch_no, descflex_subset)
descflex_max_patch <- max(descflex_patch_unique$descflex_unique_patch_no)
descflex_all <- c(descflex_sum, max_patch = descflex_max_patch)
#write.csv(descflex_sub, file = "Results/Patch_Stats/Species_results/descflex_sub.csv")

dew_sub <- subset(spat_all_sub, spp_duplicate == "DEW")
dew_subset <- subset(dew_sub, patchID > 0)
dew_sum <- colSums(dew_subset[,-1:-3])
dew_mean <- colMeans(dew_subset[,-1:-3])
dew_mean_patch <- mean(dew_subset$patchID)

dew_unique_patch_no <- 1:nrow(dew_subset)
dew_patch_unique <- cbind(dew_unique_patch_no, dew_subset)
dew_max_patch <- max(dew_patch_unique$dew_unique_patch_no)
dew_all <- c(dew_sum, max_patch = dew_max_patch)
#write.csv(dew_sub, file = "Results/Patch_Stats/Species_results/dew_sub.csv")

empenigr_sub <- subset(spat_all_sub, spp_duplicate == "Empenigr")
empenigr_subset <- subset(empenigr_sub, patchID > 0)
empenigr_sum <- colSums(empenigr_subset[,-1:-3])
empenigr_mean <- colMeans(empenigr_subset[,-1:-3])
empenigr_mean_patch <- mean(empenigr_subset$patchID)

empenigr_unique_patch_no <- 1:nrow(empenigr_subset)
empenigr_patch_unique <- cbind(empenigr_unique_patch_no, empenigr_subset)
empenigr_max_patch <- max(empenigr_patch_unique$empenigr_unique_patch_no)
empenigr_all <- c(empenigr_sum, max_patch = empenigr_max_patch)
#write.csv(empenigr_sub, file = "Results/Patch_Stats/Species_results/empenigr_sub.csv")

erictetr_sub <- subset(spat_all_sub, spp_duplicate == "Erictetr")
erictetr_subset <- subset(erictetr_sub, patchID > 0)
erictetr_sum <- colSums(erictetr_subset[,-1:-3])
erictetr_mean <- colMeans(erictetr_subset[,-1:-3])
erictetr_mean_patch <- mean(erictetr_subset$patchID)

erictetr_unique_patch_no <- 1:nrow(erictetr_subset)
erictetr_patch_unique <- cbind(erictetr_unique_patch_no, erictetr_subset)
erictetr_max_patch <- max(erictetr_patch_unique$erictetr_unique_patch_no)
erictetr_all <- c(erictetr_sum, max_patch = erictetr_max_patch)
#write.csv(erictetr_sub, file = "Results/Patch_Stats/Species_results/erictetr_sub.csv")

erioangu_sub <- subset(spat_all_sub, spp_duplicate == "Erioangu")
erioangu_subset <- subset(erioangu_sub, patchID > 0)
erioangu_sum <- colSums(erioangu_subset[,-1:-3])
erioangu_mean <- colMeans(erioangu_subset[,-1:-3])
erioangu_mean_patch <- mean(erioangu_subset$patchID)

erioangu_unique_patch_no <- 1:nrow(erioangu_subset)
erioangu_patch_unique <- cbind(erioangu_unique_patch_no, erioangu_subset)
erioangu_max_patch <- max(erioangu_patch_unique$erioangu_unique_patch_no)
erioangu_all <- c(erioangu_sum, max_patch = erioangu_max_patch)
#write.csv(erioangu_sub, file = "Results/Patch_Stats/Species_results/erioangu_sub.csv")

eriovagi_sub <- subset(spat_all_sub, spp_duplicate == "Eriovagi")
eriovagi_subset <- subset(eriovagi_sub, patchID > 0)
eriovagi_sum <- colSums(eriovagi_subset[,-1:-3])
eriovagi_mean <- colMeans(eriovagi_subset[,-1:-3])
eriovagi_mean_patch <- mean(eriovagi_subset$patchID)

eriovagi_unique_patch_no <- 1:nrow(eriovagi_subset)
eriovagi_patch_unique <- cbind(eriovagi_unique_patch_no, eriovagi_subset)
eriovagi_max_patch <- max(eriovagi_patch_unique$eriovagi_unique_patch_no)
eriovagi_all <- c(eriovagi_sum, max_patch = eriovagi_max_patch)
#write.csv(eriovagi_sub, file = "Results/Patch_Stats/Species_results/eriovagi_sub.csv")

festovin_sub <- subset(spat_all_sub, spp_duplicate == "Festovin")
festovin_subset <- subset(festovin_sub, patchID > 0)
festovin_sum <- colSums(festovin_subset[,-1:-3])
festovin_mean <- colMeans(festovin_subset[,-1:-3])
festovin_mean_patch <- mean(festovin_subset$patchID)

festovin_unique_patch_no <- 1:nrow(festovin_subset)
festovin_patch_unique <- cbind(festovin_unique_patch_no, festovin_subset)
festovin_max_patch <- max(festovin_patch_unique$festovin_unique_patch_no)
festovin_all <- c(festovin_sum, max_patch = festovin_max_patch)
#write.csv(festovin_sub, file = "Results/Patch_Stats/Species_results/festovin_sub.csv")

festprat_sub <- subset(spat_all_sub, spp_duplicate == "Festprat")
festprat_subset <- subset(festprat_sub, patchID > 0)
festprat_sum <- colSums(festprat_subset[,-1:-3])
festprat_mean <- colMeans(festprat_subset[,-1:-3])
festprat_mean_patch <- mean(festprat_subset$patchID)

festprat_unique_patch_no <- 1:nrow(festprat_subset)
festprat_patch_unique <- cbind(festprat_unique_patch_no, festprat_subset)
festprat_max_patch <- max(festprat_patch_unique$festprat_unique_patch_no)
festprat_all <- c(festprat_sum, max_patch = festprat_max_patch)
#write.csv(festprat_sub, file = "Results/Patch_Stats/Species_results/festprat_sub.csv")

festrubr_sub <- subset(spat_all_sub, spp_duplicate == "Festrubr")
festrubr_subset <- subset(festrubr_sub, patchID > 0)
festrubr_sum <- colSums(festrubr_subset[,-1:-3])
festrubr_mean <- colMeans(festrubr_subset[,-1:-3])
festrubr_mean_patch <- mean(festrubr_subset$patchID)

festrubr_unique_patch_no <- 1:nrow(festrubr_subset)
festrubr_patch_unique <- cbind(festrubr_unique_patch_no, festrubr_subset)
festrubr_max_patch <- max(festrubr_patch_unique$festrubr_unique_patch_no)
festrubr_all <- c(festrubr_sum, max_patch = festrubr_max_patch)
#write.csv(festrubr_sub, file = "Results/Patch_Stats/Species_results/festrubr_sub.csv")

galisaxa_sub <- subset(spat_all_sub, spp_duplicate == "Galisaxa")
galisaxa_subset <- subset(galisaxa_sub, patchID > 0)
galisaxa_sum <- colSums(galisaxa_subset[,-1:-3])
galisaxa_mean <- colMeans(galisaxa_subset[,-1:-3])
galisaxa_mean_patch <- mean(galisaxa_subset$patchID)

galisaxa_unique_patch_no <- 1:nrow(galisaxa_subset)
galisaxa_patch_unique <- cbind(galisaxa_unique_patch_no, galisaxa_subset)
galisaxa_max_patch <- max(galisaxa_patch_unique$galisaxa_unique_patch_no)
galisaxa_all <- c(galisaxa_sum, max_patch = galisaxa_max_patch)
#write.csv(galisaxa_sub, file = "Results/Patch_Stats/Species_results/galisaxa_sub.csv")

holclana_sub <- subset(spat_all_sub, spp_duplicate == "Holclana")
holclana_subset <- subset(holclana_sub, patchID > 0)
holclana_sum <- colSums(holclana_subset[,-1:-3])
holclana_mean <- colMeans(holclana_subset[,-1:-3])
holclana_mean_patch <- mean(holclana_subset$patchID)

holclana_unique_patch_no <- 1:nrow(holclana_subset)
holclana_patch_unique <- cbind(holclana_unique_patch_no, holclana_subset)
holclana_max_patch <- max(holclana_patch_unique$holclana_unique_patch_no)
holclana_all <- c(holclana_sum, max_patch = holclana_max_patch)
#write.csv(holclana_sub, file = "Results/Patch_Stats/Species_results/holclana_sub.csv")

holcmolli_sub <- subset(spat_all_sub, spp_duplicate == "Holcmolli")
holcmolli_subset <- subset(holcmolli_sub, patchID > 0)
holcmolli_sum <- colSums(holcmolli_subset[,-1:-3])
holcmolli_mean <- colMeans(holcmolli_subset[,-1:-3])
holcmolli_mean_patch <- mean(holcmolli_subset$patchID)

holcmolli_unique_patch_no <- 1:nrow(holcmolli_subset)
holcmolli_patch_unique <- cbind(holcmolli_unique_patch_no, holcmolli_subset)
holcmolli_max_patch <- max(holcmolli_patch_unique$holcmolli_unique_patch_no)
holcmolli_all <- c(holcmolli_sum, max_patch = holcmolli_max_patch)
#write.csv(holcmolli_sub, file = "Results/Patch_Stats/Species_results/holcmolli_sub.csv")

juncacut_sub <- subset(spat_all_sub, spp_duplicate == "Juncacut")
juncacut_subset <- subset(juncacut_sub, patchID > 0)
juncacut_sum <- colSums(juncacut_subset[,-1:-3])
juncacut_mean <- colMeans(juncacut_subset[,-1:-3])
juncacut_mean_patch <- mean(juncacut_subset$patchID)

juncacut_unique_patch_no <- 1:nrow(juncacut_subset)
juncacut_patch_unique <- cbind(juncacut_unique_patch_no, juncacut_subset)
juncacut_max_patch <- max(juncacut_patch_unique$juncacut_unique_patch_no)
juncacut_all <- c(juncacut_sum, max_patch = juncacut_max_patch)
#write.csv(juncacut_sub, file = "Results/Patch_Stats/Species_results/juncacut_sub.csv")

juncarti_sub <- subset(spat_all_sub, spp_duplicate == "Juncarti")
juncarti_subset <- subset(juncarti_sub, patchID > 0)
juncarti_sum <- colSums(juncarti_subset[,-1:-3])
juncarti_mean <- colMeans(juncarti_subset[,-1:-3])
juncarti_mean_patch <- mean(juncarti_subset$patchID)

juncarti_unique_patch_no <- 1:nrow(juncarti_subset)
juncarti_patch_unique <- cbind(juncarti_unique_patch_no, juncarti_subset)
juncarti_max_patch <- max(juncarti_patch_unique$juncarti_unique_patch_no)
juncarti_all <- c(juncarti_sum, max_patch = juncarti_max_patch)
#write.csv(juncarti_sub, file = "Results/Patch_Stats/Species_results/juncarti_sub.csv")

junccong_sub <- subset(spat_all_sub, spp_duplicate == "Junccong")
junccong_subset <- subset(junccong_sub, patchID > 0)
junccong_sum <- colSums(junccong_subset[,-1:-3])
junccong_mean <- colMeans(junccong_subset[,-1:-3])
junccong_mean_patch <- mean(junccong_subset$patchID)

junccong_unique_patch_no <- 1:nrow(junccong_subset)
junccong_patch_unique <- cbind(junccong_unique_patch_no, junccong_subset)
junccong_max_patch <- max(junccong_patch_unique$junccong_unique_patch_no)
junccong_all <- c(junccong_sum, max_patch = junccong_max_patch)
#write.csv(junccong_sub, file = "Results/Patch_Stats/Species_results/junccong_sub.csv")

junceffu_sub <- subset(spat_all_sub, spp_duplicate == "Junceffu")
junceffu_subset <- subset(junceffu_sub, patchID > 0)
junceffu_sum <- colSums(junceffu_subset[,-1:-3])
junceffu_mean <- colMeans(junceffu_subset[,-1:-3])
junceffu_mean_patch <- mean(junceffu_subset$patchID)

junceffu_unique_patch_no <- 1:nrow(junceffu_subset)
junceffu_patch_unique <- cbind(junceffu_unique_patch_no, junceffu_subset)
junceffu_max_patch <- max(junceffu_patch_unique$junceffu_unique_patch_no)
junceffu_all <- c(junceffu_sum, max_patch = junceffu_max_patch)
#write.csv(junceffu_sub, file = "Results/Patch_Stats/Species_results/junceffu_sub.csv")

juncsqua_sub <- subset(spat_all_sub, spp_duplicate == "Juncsqua")
juncsqua_subset <- subset(juncsqua_sub, patchID > 0)
juncsqua_sum <- colSums(juncsqua_subset[,-1:-3])
juncsqua_mean <- colMeans(juncsqua_subset[,-1:-3])
juncsqua_mean_patch <- mean(juncsqua_subset$patchID)

juncsqua_unique_patch_no <- 1:nrow(juncsqua_subset)
juncsqua_patch_unique <- cbind(juncsqua_unique_patch_no, juncsqua_subset)
juncsqua_max_patch <- max(juncsqua_patch_unique$juncsqua_unique_patch_no)
juncsqua_all <- c(juncsqua_sum, max_patch = juncsqua_max_patch)
#write.csv(juncsqua_sub, file = "Results/Patch_Stats/Species_results/juncsqua_sub.csv")

LITTER_sub <- subset(spat_all_sub, spp_duplicate == "LITTER")
LITTER_subset <- subset(LITTER_sub, patchID > 0)
LITTER_sum <- colSums(LITTER_subset[,-1:-3])
LITTER_mean <- colMeans(LITTER_subset[,-1:-3])
LITTER_mean_patch <- mean(LITTER_subset$patchID)

LITTER_unique_patch_no <- 1:nrow(LITTER_subset)
LITTER_patch_unique <- cbind(LITTER_unique_patch_no, LITTER_subset)
LITTER_max_patch <- max(LITTER_patch_unique$LITTER_unique_patch_no)
LITTER_all <- c(LITTER_sum, max_patch = LITTER_max_patch)
#write.csv(LITTER_sub, file = "Results/Patch_Stats/Species_results/LITTER_sub.csv")

liverwort_sub <- subset(spat_all_sub, spp_duplicate == "LIVERWORT")
liverwort_subset <- subset(liverwort_sub, patchID > 0)
liverwort_sum <- colSums(liverwort_subset[,-1:-3])
liverwort_mean <- colMeans(liverwort_subset[,-1:-3])
liverwort_mean_patch <- mean(liverwort_subset$patchID)

liverwort_unique_patch_no <- 1:nrow(liverwort_subset)
liverwort_patch_unique <- cbind(liverwort_unique_patch_no, liverwort_subset)
liverwort_max_patch <- max(liverwort_patch_unique$liverwort_unique_patch_no)
liverwort_all <- c(liverwort_sum, max_patch = liverwort_max_patch)
#write.csv(liverwort_sub, file = "Results/Patch_Stats/Species_results/liverwort_sub.csv")

lolipere_sub <- subset(spat_all_sub, spp_duplicate == "Lolipere")
lolipere_subset <- subset(lolipere_sub, patchID > 0)
lolipere_sum <- colSums(lolipere_subset[,-1:-3])
lolipere_mean <- colMeans(lolipere_subset[,-1:-3])
lolipere_mean_patch <- mean(lolipere_subset$patchID)

lolipere_unique_patch_no <- 1:nrow(lolipere_subset)
lolipere_patch_unique <- cbind(lolipere_unique_patch_no, lolipere_subset)
lolipere_max_patch <- max(lolipere_patch_unique$lolipere_unique_patch_no)
lolipere_all <- c(lolipere_sum, max_patch = lolipere_max_patch)
#write.csv(lolipere_sub, file = "Results/Patch_Stats/Species_results/lolipere_sub.csv")

luzucamp_sub <- subset(spat_all_sub, spp_duplicate == "Luzucamp")
luzucamp_subset <- subset(luzucamp_sub, patchID > 0)
luzucamp_sum <- colSums(luzucamp_subset[,-1:-3])
luzucamp_mean <- colMeans(luzucamp_subset[,-1:-3])
luzucamp_mean_patch <- mean(luzucamp_subset$patchID)

luzucamp_unique_patch_no <- 1:nrow(luzucamp_subset)
luzucamp_patch_unique <- cbind(luzucamp_unique_patch_no, luzucamp_subset)
luzucamp_max_patch <- max(luzucamp_patch_unique$luzucamp_unique_patch_no)
luzucamp_all <- c(luzucamp_sum, max_patch = luzucamp_max_patch)
#write.csv(luzucamp_sub, file = "Results/Patch_Stats/Species_results/luzucamp_sub.csv")

luzumult_sub <- subset(spat_all_sub, spp_duplicate == "Luzumult")
luzumult_subset <- subset(luzumult_sub, patchID > 0)
luzumult_sum <- colSums(luzumult_subset[,-1:-3])
luzumult_mean <- colMeans(luzumult_subset[,-1:-3])
luzumult_mean_patch <- mean(luzumult_subset$patchID)

luzumult_unique_patch_no <- 1:nrow(luzumult_subset)
luzumult_patch_unique <- cbind(luzumult_unique_patch_no, luzumult_subset)
luzumult_max_patch <- max(luzumult_patch_unique$luzumult_unique_patch_no)
luzumult_all <- c(luzumult_sum, max_patch = luzumult_max_patch)
#write.csv(luzumult_sub, file = "Results/Patch_Stats/Species_results/luzumult_sub.csv")

luzupilo_sub <- subset(spat_all_sub, spp_duplicate == "Luzupilo")
luzupilo_subset <- subset(luzupilo_sub, patchID > 0)
luzupilo_sum <- colSums(luzupilo_subset[,-1:-3])
luzupilo_mean <- colMeans(luzupilo_subset[,-1:-3])
luzupilo_mean_patch <- mean(luzupilo_subset$patchID)

luzupilo_unique_patch_no <- 1:nrow(luzupilo_subset)
luzupilo_patch_unique <- cbind(luzupilo_unique_patch_no, luzupilo_subset)
luzupilo_max_patch <- max(luzupilo_patch_unique$luzupilo_unique_patch_no)
luzupilo_all <- c(luzupilo_sum, max_patch = luzupilo_max_patch)
#write.csv(luzupilo_sub, file = "Results/Patch_Stats/Species_results/luzupilo_sub.csv")

molicaer_sub <- subset(spat_all_sub, spp_duplicate == "Molicaer")
molicaer_subset <- subset(molicaer_sub, patchID > 0)
molicaer_sum <- colSums(molicaer_subset[,-1:-3])
molicaer_mean <- colMeans(molicaer_subset[,-1:-3])
molicaer_mean_patch <- mean(molicaer_subset$patchID)

molicaer_unique_patch_no <- 1:nrow(molicaer_subset)
molicaer_patch_unique <- cbind(molicaer_unique_patch_no, molicaer_subset)
molicaer_max_patch <- max(molicaer_patch_unique$molicaer_unique_patch_no)
molicaer_all <- c(molicaer_sum, max_patch = molicaer_max_patch)
#write.csv(molicaer_sub, file = "Results/Patch_Stats/Species_results/molicaer_sub.csv")

nardstri_sub <- subset(spat_all_sub, spp_duplicate == "Nardstri")
nardstri_subset <- subset(nardstri_sub, patchID > 0)
nardstri_sum <- colSums(nardstri_subset[,-1:-3])
nardstri_mean <- colMeans(nardstri_subset[,-1:-3])
nardstri_mean_patch <- mean(nardstri_subset$patchID)

nardstri_unique_patch_no <- 1:nrow(nardstri_subset)
nardstri_patch_unique <- cbind(nardstri_unique_patch_no, nardstri_subset)
nardstri_max_patch <- max(nardstri_patch_unique$nardstri_unique_patch_no)
nardstri_all <- c(nardstri_sum, max_patch = nardstri_max_patch)
#write.csv(nardstri_sub, file = "Results/Patch_Stats/Species_results/nardstri_sub.csv")

plagundu_sub <- subset(spat_all_sub, spp_duplicate == "Plagundu")
plagundu_subset <- subset(plagundu_sub, patchID > 0)
plagundu_sum <- colSums(plagundu_subset[,-1:-3])
plagundu_mean <- colMeans(plagundu_subset[,-1:-3])
plagundu_mean_patch <- mean(plagundu_subset$patchID)

plagundu_unique_patch_no <- 1:nrow(plagundu_subset)
plagundu_patch_unique <- cbind(plagundu_unique_patch_no, plagundu_subset)
plagundu_max_patch <- max(plagundu_patch_unique$plagundu_unique_patch_no)
plagundu_all <- c(plagundu_sum, max_patch = plagundu_max_patch)
#write.csv(plagundu_sub, file = "Results/Patch_Stats/Species_results/plagundu_sub.csv")

pleuschr_sub <- subset(spat_all_sub, spp_duplicate == "Pleuschr")
pleuschr_subset <- subset(pleuschr_sub, patchID > 0)
pleuschr_sum <- colSums(pleuschr_subset[,-1:-3])
pleuschr_mean <- colMeans(pleuschr_subset[,-1:-3])
pleuschr_mean_patch <- mean(pleuschr_subset$patchID)

pleuschr_unique_patch_no <- 1:nrow(pleuschr_subset)
pleuschr_patch_unique <- cbind(pleuschr_unique_patch_no, pleuschr_subset)
pleuschr_max_patch <- max(pleuschr_patch_unique$pleuschr_unique_patch_no)
pleuschr_all <- c(pleuschr_sum, max_patch = pleuschr_max_patch)
#write.csv(pleuschr_sub, file = "Results/Patch_Stats/Species_results/pleuschr_sub.csv")

poaannu_sub <- subset(spat_all_sub, spp_duplicate == "Poaannu")
poaannu_subset <- subset(poaannu_sub, patchID > 0)
poaannu_sum <- colSums(poaannu_subset[,-1:-3])
poaannu_mean <- colMeans(poaannu_subset[,-1:-3])
poaannu_mean_patch <- mean(poaannu_subset$patchID)

poaannu_unique_patch_no <- 1:nrow(poaannu_subset)
poaannu_patch_unique <- cbind(poaannu_unique_patch_no, poaannu_subset)
poaannu_max_patch <- max(poaannu_patch_unique$poaannu_unique_patch_no)
poaannu_all <- c(poaannu_sum, max_patch = poaannu_max_patch)
#write.csv(poaannu_sub, file = "Results/Patch_Stats/Species_results/poaannu_sub.csv")

poaprat_sub <- subset(spat_all_sub, spp_duplicate == "Poaprat")
poaprat_subset <- subset(poaprat_sub, patchID > 0)
poaprat_sum <- colSums(poaprat_subset[,-1:-3])
poaprat_mean <- colMeans(poaprat_subset[,-1:-3])
poaprat_mean_patch <- mean(poaprat_subset$patchID)

poaprat_unique_patch_no <- 1:nrow(poaprat_subset)
poaprat_patch_unique <- cbind(poaprat_unique_patch_no, poaprat_subset)
poaprat_max_patch <- max(poaprat_patch_unique$poaprat_unique_patch_no)
poaprat_all <- c(poaprat_sum, max_patch = poaprat_max_patch)
#write.csv(poaprat_sub, file = "Results/Patch_Stats/Species_results/poaprat_sub.csv")

poatriv_sub <- subset(spat_all_sub, spp_duplicate == "Poatriv")
poatriv_subset <- subset(poatriv_sub, patchID > 0)
poatriv_sum <- colSums(poatriv_subset[,-1:-3])
poatriv_mean <- colMeans(poatriv_subset[,-1:-3])
poatriv_mean_patch <- mean(poatriv_subset$patchID)

poatriv_unique_patch_no <- 1:nrow(poatriv_subset)
poatriv_patch_unique <- cbind(poatriv_unique_patch_no, poatriv_subset)
poatriv_max_patch <- max(poatriv_patch_unique$poatriv_unique_patch_no)
poatriv_all <- c(poatriv_sum, max_patch = poatriv_max_patch)
#write.csv(poatriv_sub, file = "Results/Patch_Stats/Species_results/poatriv_sub.csv")

polycomm_sub <- subset(spat_all_sub, spp_duplicate == "Polycomm")
polycomm_subset <- subset(polycomm_sub, patchID > 0)
polycomm_sum <- colSums(polycomm_subset[,-1:-3])
polycomm_mean <- colMeans(polycomm_subset[,-1:-3])
polycomm_mean_patch <- mean(polycomm_subset$patchID)

polycomm_unique_patch_no <- 1:nrow(polycomm_subset)
polycomm_patch_unique <- cbind(polycomm_unique_patch_no, polycomm_subset)
polycomm_max_patch <- max(polycomm_patch_unique$polycomm_unique_patch_no)
polycomm_all <- c(polycomm_sum, max_patch = polycomm_max_patch)
#write.csv(polycomm_sub, file = "Results/Patch_Stats/Species_results/polycomm_sub.csv")

poteerec_sub <- subset(spat_all_sub, spp_duplicate == "Poteerec")
poteerec_subset <- subset(poteerec_sub, patchID > 0)
poteerec_sum <- colSums(poteerec_subset[,-1:-3])
poteerec_mean <- colMeans(poteerec_subset[,-1:-3])
poteerec_mean_patch <- mean(poteerec_subset$patchID)

poteerec_unique_patch_no <- 1:nrow(poteerec_subset)
poteerec_patch_unique <- cbind(poteerec_unique_patch_no, poteerec_subset)
poteerec_max_patch <- max(poteerec_patch_unique$poteerec_unique_patch_no)
poteerec_all <- c(poteerec_sum, max_patch = poteerec_max_patch)
#write.csv(poteerec_sub, file = "Results/Patch_Stats/Species_results/poteerec_sub.csv")

rhytsqua_sub <- subset(spat_all_sub, spp_duplicate == "Rhytsqua")
rhytsqua_subset <- subset(rhytsqua_sub, patchID > 0)
rhytsqua_sum <- colSums(rhytsqua_subset[,-1:-3])
rhytsqua_mean <- colMeans(rhytsqua_subset[,-1:-3])
rhytsqua_mean_patch <- mean(rhytsqua_subset$patchID)

rhytsqua_unique_patch_no <- 1:nrow(rhytsqua_subset)
rhytsqua_patch_unique <- cbind(rhytsqua_unique_patch_no, rhytsqua_subset)
rhytsqua_max_patch <- max(rhytsqua_patch_unique$rhytsqua_unique_patch_no)
rhytsqua_all <- c(rhytsqua_sum, max_patch = rhytsqua_max_patch)
#write.csv(rhytsqua_sub, file = "Results/Patch_Stats/Species_results/rhytsqua_sub.csv")

ROCK_sub <- subset(spat_all_sub, spp_duplicate == "ROCK")
ROCK_subset <- subset(ROCK_sub, patchID > 0)
ROCK_sum <- colSums(ROCK_subset[,-1:-3])
ROCK_mean <- colMeans(ROCK_subset[,-1:-3])
ROCK_mean_patch <- mean(ROCK_subset$patchID)

ROCK_unique_patch_no <- 1:nrow(ROCK_subset)
ROCK_patch_unique <- cbind(ROCK_unique_patch_no, ROCK_subset)
ROCK_max_patch <- max(ROCK_patch_unique$ROCK_unique_patch_no)
ROCK_all <- c(ROCK_sum, max_patch = ROCK_max_patch)
#write.csv(ROCK_sub, file = "Results/Patch_Stats/Species_results/ROCK_sub.csv")

rumeacet_sub <- subset(spat_all_sub, spp_duplicate == "Rumeacet")
rumeacet_subset <- subset(rumeacet_sub, patchID > 0)
rumeacet_sum <- colSums(rumeacet_subset[,-1:-3])
rumeacet_mean <- colMeans(rumeacet_subset[,-1:-3])
rumeacet_mean_patch <- mean(rumeacet_subset$patchID)

rumeacet_unique_patch_no <- 1:nrow(rumeacet_subset)
rumeacet_patch_unique <- cbind(rumeacet_unique_patch_no, rumeacet_subset)
rumeacet_max_patch <- max(rumeacet_patch_unique$rumeacet_unique_patch_no)
rumeacet_all <- c(rumeacet_sum, max_patch = rumeacet_max_patch)
#write.csv(rumeacet_sub, file = "Results/Patch_Stats/Species_results/rumeacet_sub.csv")

rumeacetosa_sub <- subset(spat_all_sub, spp_duplicate == "Rumeacetosa")
rumeacetosa_subset <- subset(rumeacetosa_sub, patchID > 0)
rumeacetosa_sum <- colSums(rumeacetosa_subset[,-1:-3])
rumeacetosa_mean <- colMeans(rumeacetosa_subset[,-1:-3])
rumeacetosa_mean_patch <- mean(rumeacetosa_subset$patchID)

rumeacetosa_unique_patch_no <- 1:nrow(rumeacetosa_subset)
rumeacetosa_patch_unique <- cbind(rumeacetosa_unique_patch_no, rumeacetosa_subset)
rumeacetosa_max_patch <- max(rumeacetosa_patch_unique$rumeacetosa_unique_patch_no)
rumeacetosa_all <- c(rumeacetosa_sum, max_patch = rumeacetosa_max_patch)
#write.csv(rumeacetosa_sub, file = "Results/Patch_Stats/Species_results/rumeacetosa_sub.csv")

SHEEP_sub <- subset(spat_all_sub, spp_duplicate == "SHEEP")
SHEEP_subset <- subset(SHEEP_sub, patchID > 0)
SHEEP_sum <- colSums(SHEEP_subset[,-1:-3])
SHEEP_mean <- colMeans(SHEEP_subset[,-1:-3])
SHEEP_mean_patch <- mean(SHEEP_subset$patchID)

SHEEP_unique_patch_no <- 1:nrow(SHEEP_subset)
SHEEP_patch_unique <- cbind(SHEEP_unique_patch_no, SHEEP_subset)
SHEEP_max_patch <- max(SHEEP_patch_unique$SHEEP_unique_patch_no)
SHEEP_all <- c(SHEEP_sum, max_patch = SHEEP_max_patch)
#write.csv(SHEEP_sub, file = "Results/Patch_Stats/Species_results/SHEEP_sub.csv")

sphacapi_sub <- subset(spat_all_sub, spp_duplicate == "Sphacapi")
sphacapi_subset <- subset(sphacapi_sub, patchID > 0)
sphacapi_sum <- colSums(sphacapi_subset[,-1:-3])
sphacapi_mean <- colMeans(sphacapi_subset[,-1:-3])
sphacapi_mean_patch <- mean(sphacapi_subset$patchID)

sphacapi_unique_patch_no <- 1:nrow(sphacapi_subset)
sphacapi_patch_unique <- cbind(sphacapi_unique_patch_no, sphacapi_subset)
sphacapi_max_patch <- max(sphacapi_patch_unique$sphacapi_unique_patch_no)
sphacapi_all <- c(sphacapi_sum, max_patch = sphacapi_max_patch)
#write.csv(sphacapi_sub, file = "Results/Patch_Stats/Species_results/sphacapi_sub.csv")

sphafall_sub <- subset(spat_all_sub, spp_duplicate == "Sphafall")
sphafall_subset <- subset(sphafall_sub, patchID > 0)
sphafall_sum <- colSums(sphafall_subset[,-1:-3])
sphafall_mean <- colMeans(sphafall_subset[,-1:-3])
sphafall_mean_patch <- mean(sphafall_subset$patchID)

sphafall_unique_patch_no <- 1:nrow(sphafall_subset)
sphafall_patch_unique <- cbind(sphafall_unique_patch_no, sphafall_subset)
sphafall_max_patch <- max(sphafall_patch_unique$sphafall_unique_patch_no)
sphafall_all <- c(sphafall_sum, max_patch = sphafall_max_patch)
#write.csv(sphafall_sub, file = "Results/Patch_Stats/Species_results/sphafall_sub.csv")

sphapalu_sub <- subset(spat_all_sub, spp_duplicate == "Sphapalu")
sphapalu_subset <- subset(sphapalu_sub, patchID > 0)
sphapalu_sum <- colSums(sphapalu_subset[,-1:-3])
sphapalu_mean <- colMeans(sphapalu_subset[,-1:-3])
sphapalu_mean_patch <- mean(sphapalu_subset$patchID)

sphapalu_unique_patch_no <- 1:nrow(sphapalu_subset)
sphapalu_patch_unique <- cbind(sphapalu_unique_patch_no, sphapalu_subset)
sphapalu_max_patch <- max(sphapalu_patch_unique$sphapalu_unique_patch_no)
sphapalu_all <- c(sphapalu_sum, max_patch = sphapalu_max_patch)
#write.csv(sphapalu_sub, file = "Results/Patch_Stats/Species_results/sphapalu_sub.csv")

spharubr_sub <- subset(spat_all_sub, spp_duplicate == "Spharubr")
spharubr_subset <- subset(spharubr_sub, patchID > 0)
spharubr_sum <- colSums(spharubr_subset[,-1:-3])
spharubr_mean <- colMeans(spharubr_subset[,-1:-3])
spharubr_mean_patch <- mean(spharubr_subset$patchID)

spharubr_unique_patch_no <- 1:nrow(spharubr_subset)
spharubr_patch_unique <- cbind(spharubr_unique_patch_no, spharubr_subset)
spharubr_max_patch <- max(spharubr_patch_unique$spharubr_unique_patch_no)
spharubr_all <- c(spharubr_sum, max_patch = spharubr_max_patch)
#write.csv(spharubr_sub, file = "Results/Patch_Stats/Species_results/spharubr_sub.csv")

triccesp_sub <- subset(spat_all_sub, spp_duplicate == "Triccesp")
triccesp_subset <- subset(triccesp_sub, patchID > 0)
triccesp_sum <- colSums(triccesp_subset[,-1:-3])
triccesp_mean <- colMeans(triccesp_subset[,-1:-3])
triccesp_mean_patch <- mean(triccesp_subset$patchID)

triccesp_unique_patch_no <- 1:nrow(triccesp_subset)
triccesp_patch_unique <- cbind(triccesp_unique_patch_no, triccesp_subset)
triccesp_max_patch <- max(triccesp_patch_unique$triccesp_unique_patch_no)
triccesp_all <- c(triccesp_sum, max_patch = triccesp_max_patch)
#write.csv(triccesp_sub, file = "Results/Patch_Stats/Species_results/triccesp_sub.csv")

vaccmyrt_sub <- subset(spat_all_sub, spp_duplicate == "Vaccmyrt")
vaccmyrt_subset <- subset(vaccmyrt_sub, patchID > 0)
vaccmyrt_sum <- colSums(vaccmyrt_subset[,-1:-3])
vaccmyrt_mean <- colMeans(vaccmyrt_subset[,-1:-3])
vaccmyrt_mean_patch <- mean(vaccmyrt_subset$patchID)

vaccmyrt_unique_patch_no <- 1:nrow(vaccmyrt_subset)
vaccmyrt_patch_unique <- cbind(vaccmyrt_unique_patch_no, vaccmyrt_subset)
vaccmyrt_max_patch <- max(vaccmyrt_patch_unique$vaccmyrt_unique_patch_no)
vaccmyrt_all <- c(vaccmyrt_sum, max_patch = vaccmyrt_max_patch)
#write.csv(vaccmyrt_sub, file = "Results/Patch_Stats/Species_results/vaccmyrt_sub.csv")

vaccoxyc_sub <- subset(spat_all_sub, spp_duplicate == "Vaccoxyc")
vaccoxyc_subset <- subset(vaccoxyc_sub, patchID > 0)
vaccoxyc_sum <- colSums(vaccoxyc_subset[,-1:-3])
vaccoxyc_mean <- colMeans(vaccoxyc_subset[,-1:-3])
vaccoxyc_mean_patch <- mean(vaccoxyc_subset$patchID)

vaccoxyc_unique_patch_no <- 1:nrow(vaccoxyc_subset)
vaccoxyc_patch_unique <- cbind(vaccoxyc_unique_patch_no, vaccoxyc_subset)
vaccoxyc_max_patch <- max(vaccoxyc_patch_unique$vaccoxyc_unique_patch_no)
vaccoxyc_all <- c(vaccoxyc_sum, max_patch = vaccoxyc_max_patch)
#write.csv(vaccoxyc_sub, file = "Results/Patch_Stats/Species_results/vaccoxyc_sub.csv")

means_patch_stats <- rbind(agrocapi_mean, agrostolo_mean, anthodor_mean, aulopalu_mean, callvulg_mean, cardprat_mean, careechi_mean,
                           carenigr_mean, carepani_mean, carepilu_mean, ceraarve_mean, cirsarve_mean, cirspalu_mean, cynocris_mean, 
                           desccesp_mean, descflex_mean, dew_mean, empenigr_mean, erictetr_mean, 
                           erioangu_mean, eriovagi_mean, festovin_mean, festprat_mean, festrubr_mean, galisaxa_mean, holclana_mean, holcmolli_mean, 
                           juncacut_mean, juncarti_mean, junccong_mean, junceffu_mean, juncsqua_mean, liverwort_mean, lolipere_mean, luzucamp_mean, luzumult_mean, luzupilo_mean,
                           molicaer_mean, nardstri_mean, plagundu_mean, pleuschr_mean, poaannu_mean, poaprat_mean, poatriv_mean, polycomm_mean, poteerec_mean, 
                           rhytsqua_mean, rumeacet_mean, rumeacetosa_mean, sphacapi_mean, sphafall_mean, sphapalu_mean, spharubr_mean, 
                           triccesp_mean, vaccmyrt_mean, vaccoxyc_mean)
write.csv(means_patch_stats, file = "Results/Patch_Stats/Mean_sum_total/means_sub.csv")

sums_patch_stats <- rbind(agrocapi_sum, agrostolo_sum, anthodor_sum, aulopalu_sum, callvulg_sum, cardprat_sum, careechi_sum,
                           carenigr_sum, carepani_sum, carepilu_sum, ceraarve_sum, cirsarve_sum, cirspalu_sum, cynocris_sum, 
                           desccesp_sum, descflex_sum, dew_sum, empenigr_sum, erictetr_sum, 
                           erioangu_sum, eriovagi_sum, festovin_sum, festprat_sum, festrubr_sum, galisaxa_sum, holclana_sum, holcmolli_sum, 
                           juncacut_sum, juncarti_sum, junccong_sum, junceffu_sum, juncsqua_sum, liverwort_sum, lolipere_sum, luzucamp_sum, luzumult_sum, luzupilo_sum,
                           molicaer_sum, nardstri_sum, plagundu_sum, pleuschr_sum, poaannu_sum, poaprat_sum, poatriv_sum, polycomm_sum, poteerec_sum, 
                           rhytsqua_sum, rumeacet_sum, rumeacetosa_sum, sphacapi_sum, sphafall_sum, sphapalu_sum, spharubr_sum, 
                           triccesp_sum, vaccmyrt_sum, vaccoxyc_sum)
write.csv(sums_patch_stats, file = "Results/Patch_Stats/Mean_sum_total/sums_sub.csv")

means_patchnumber_stats <- rbind(agrocapi_mean_patch, agrostolo_mean_patch, anthodor_mean_patch, aulopalu_mean_patch, callvulg_mean_patch, cardprat_mean_patch, careechi_mean_patch,
                           carenigr_mean_patch, carepani_mean_patch, carepilu_mean_patch, ceraarve_mean_patch, cirsarve_mean_patch, cirspalu_mean_patch, cynocris_mean_patch, 
                           desccesp_mean_patch, descflex_mean_patch, dew_mean_patch, empenigr_mean_patch, erictetr_mean_patch, 
                           erioangu_mean_patch, eriovagi_mean_patch, festovin_mean_patch, festprat_mean_patch, festrubr_mean_patch, galisaxa_mean_patch, holclana_mean_patch, holcmolli_mean_patch, 
                           juncacut_mean_patch, juncarti_mean_patch, junccong_mean_patch, junceffu_mean_patch, juncsqua_mean_patch, liverwort_mean_patch, lolipere_mean_patch, luzucamp_mean_patch, luzumult_mean_patch, luzupilo_mean_patch,
                           molicaer_mean_patch, nardstri_mean_patch, plagundu_mean_patch, pleuschr_mean_patch, poaannu_mean_patch, poaprat_mean_patch, poatriv_mean_patch, polycomm_mean_patch, poteerec_mean_patch, 
                           rhytsqua_mean_patch, rumeacet_mean_patch, rumeacetosa_mean_patch, sphacapi_mean_patch, sphafall_mean_patch, sphapalu_mean_patch, spharubr_mean_patch, 
                           triccesp_mean_patch, vaccmyrt_mean_patch, vaccoxyc_mean_patch)
write.csv(means_patchnumber_stats, file = "Results/Patch_Stats/Mean_sum_total/means_patchnumber_sub.csv")

all_patch_stats <- rbind(agrocapi_all, agrostolo_all, anthodor_all, aulopalu_all, callvulg_all, cardprat_all, careechi_all,
                                    carenigr_all, carepani_all, carepilu_all, ceraarve_all, cirsarve_all, cirspalu_all, cynocris_all, 
                                    desccesp_all, descflex_all, empenigr_all, erictetr_all, 
                                    erioangu_all, eriovagi_all, festovin_all, festrubr_all, galisaxa_all, holclana_all, holcmolli_all,
                                    juncacut_all, juncarti_all, junccong_all, junceffu_all, juncsqua_all, luzumult_all, liverwort_all, lolipere_all, 
                                    luzucamp_all, luzumult_all, luzupilo_all, molicaer_all, nardstri_all, plagundu_all, pleuschr_all,
                                    poaannu_all, poaprat_all, poatriv_all, polycomm_all, poteerec_all, 
                                    rhytsqua_all, rumeacet_all, rumeacetosa_all, sphacapi_all, sphafall_all, sphapalu_all, spharubr_all, 
                                    triccesp_all, vaccmyrt_all, vaccoxyc_all)
all_patch_stats <- as.data.frame(all_patch_stats)

################################################################################### 
##     START OF PATCH STATISTICS USING CSV FILES FOR MEAN AND SUMS / SPECIES     ##
###################################################################################

mean_patch_stats <- read.csv("Results/Patch_Stats/Mean_sum_total/means_sub_21-08-18.csv")
sums_patch_stats <- read.csv("Results/Patch_Stats/Mean_sum_total/sums_sub_21-08-18.csv")

head(mean_patch_stats)
mean_patch_stats$Rank <- NA
mean_patch_stats$Rank <- rank(-mean_patch_stats$patchID, na.last = FALSE, ties.method = "first")
head(sums_patch_stats)
all_patch_stats$Rank <- NA
all_patch_stats$Rank <- rank(-all_patch_stats$max_patch, na.last = FALSE, ties.method = "first") # rank by area

# sort my number of patches
mean_sorted <- mean_patch_stats[order(-mean_patch_stats$patchID),] # mean patch number per species
sum_sorted <- all_patch_stats[order(-all_patch_stats$max_patch),] # total patch number per species

species_name <- data.frame(row.names(sum_sorted))
species_rank <- data.frame(sum_sorted$Rank)
species_rank_subdom <- data.frame(c(species_name, species_rank))

write.csv(species_rank_subdom, file = "Results/SUBDOM/species_rank_subdom.csv")

## plot histogram of mean patch number (y-axis) vs/ speies (x-axis)
hist(mean_patch_stats$patchID)
barplot(mean_sorted$patchID, names.arg = mean_sorted$Rank)     
barplot(sum_sorted$max_patch, names.arg = sum_sorted$Rank, xlab = "Species Rank", ylab = "Total Number of patches/species", 
        main = "Total number of patches vs. species rank (Subdominant)")

## Linear Models 
fit_area <- lm(area ~ max_patch, data = sum_sorted)
plot(sum_sorted$area ~ sum_sorted$max_patch, xlab = "Total number of patches/spp", 
     ylab = "Total area/spp", pch = 16)
abline(fit_area)
fit_perimeter <- lm(perimeter ~ max_patch, data = sum_sorted)
plot(sum_sorted$perimeter ~ sum_sorted$max_patch, xlab = "Total number of patches/spp", 
     ylab = "Total perimeter/spp", pch = 16)
abline(fit_perimeter)
fit_per_area_ratio <- lm(perim.area.ratio ~ max_patch, data = sum_sorted)
plot(sum_sorted$perim.area.ratio ~ sum_sorted$max_patch, xlab = "Total number of patches/spp",
     ylab = "Perimter:Area", pch = 16)
abline(fit_per_area_ratio)
fit_per_vs_area <- lm(perimeter ~ area, data = sum_sorted)
plot(perimeter ~ area, data = sum_sorted, xlab = "Total area/spp", ylab = "Total peimeter/spp", pch = 16)
abline(fit_per_vs_area)
fit_shape_patch <- lm(shape.index ~ max_patch, data = sum_sorted)
plot(shape.index ~ max_patch, data = sum_sorted, xlab = "Total number of patches/spp", 
     ylab = "Shape Index", pch = 16)
abline(fit_shape_patch)


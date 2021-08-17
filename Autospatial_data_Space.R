# Patch statistics

rm(list=ls())
library(vegan)
library(dplyr)
library(tidyverse)
library(SDMTools) # now withdrawn from CRAN
#library(tidyr)
library(mvabund)

getOption("max.print")
options(max.print = 1000000000)

x = seq(from = 10, to = 100, by = 10) #set x axis 10 to 100 by 10 for image
y = seq(from = 10, to = 120, by = 10) #set y axis 10 to 100 by 10 for image
nx = 10 # number of grid cells on x axis
ny = 12 # number of grid cells on y axis

###################################################################################
##                            COLUMN 1                                           ##
################################################################################### 
##               PATCH STATS ON ALL 167 QUADRATS: DOMINANT                       ##
###################################################################################
Dominant <- data.frame(read.csv("Data/Dominant-sub_New_Versions/Dominant_23-7-18_spaced.csv", header = FALSE)) # main datafram (all inputs)
colnames(Dominant) <- c("quad_no", "Row", 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
Dominant[1,] = "" ## add empty line to streamline with other quadrat rows
Dominant[1838,] = "" ## add line after last quadrat to streamline with other quadrat rows

quad1_to_167 <- Dominant
quad_dom_mat_all <- as.matrix(quad1_to_167[,-1])
quad_dom_tab_all <- table(quad_dom_mat_all[,-1]) # gives count of each species in the quadrat
quad_dom_tab_all
df_dom <- data.frame(quad_dom_tab_all)
quad_lng <- quad1_to_167 %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad_lng)
quad_lng <- mutate(quad_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

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

quad1_dom <- subset(Dominant, quad_no == "1")
quad1_dom_mat <- as.matrix(quad1_dom[,-1])
quad1_dom_tab <- table(quad1_dom_mat[,-1]) # gives count of each species in the quadrat
quad1_dom_tab
quad1_lng <- quad1_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad1_lng)
quad1_lng <- mutate(quad1_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad1_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad1_dom_tab 
quad_lng     <- quad1_lng

## QUADRAT 2
quad2_dom <- subset(Dominant, quad_no == "2")
quad2_dom_mat <- as.matrix(quad2_dom[,-1])
quad2_dom_tab <- table(quad2_dom_mat[,-1]) # gives count of each species in the quadrat
quad2_dom_tab
quad2_lng <- quad2_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad2_lng)
quad2_lng <- mutate(quad2_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad2_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad2_dom_tab 
quad_lng     <- quad2_lng

## QUADRAT 3
quad3_dom <- subset(Dominant, quad_no == "3")
quad3_dom_mat <- as.matrix(quad3_dom[,-1])
quad3_dom_tab <- table(quad3_dom_mat[,-1]) # gives count of each species in the quadrat
quad3_dom_tab
quad3_lng <- quad3_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad3_lng)
quad3_lng <- mutate(quad3_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad3_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad3_dom_tab 
quad_lng     <- quad3_lng

## QUADRAT 4
quad4_dom <- subset(Dominant, quad_no == "4")
quad4_dom_mat <- as.matrix(quad4_dom[,-1])
quad4_dom_tab <- table(quad4_dom_mat[,-1]) # gives count of each species in the quadrat
quad4_dom_tab
quad4_lng <- quad4_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad4_lng)
quad4_lng <- mutate(quad4_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad4_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad4_dom_tab 
quad_lng     <- quad4_lng

## QUADRAT 5
quad5_dom <- subset(Dominant, quad_no == "5")
quad5_dom_mat <- as.matrix(quad5_dom[,-1])
quad5_dom_tab <- table(quad5_dom_mat[,-1]) # gives count of each species in the quadrat
quad5_dom_tab
quad5_lng <- quad5_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad5_lng)
quad5_lng <- mutate(quad5_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad5_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad5_dom_tab 
quad_lng     <- quad5_lng

## QUAD 6
quad6_dom <- subset(Dominant, quad_no == "6")
quad6_dom_mat <- as.matrix(quad6_dom[,-1])
quad6_dom_tab <- table(quad6_dom_mat[,-1]) # gives count of each species in the quadrat
quad6_dom_tab
quad6_lng <- quad6_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad6_lng)
quad6_lng <- mutate(quad6_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad6_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad6_dom_tab 
quad_lng     <- quad6_lng

## QUAD 7
quad7_dom <- subset(Dominant, quad_no == "7")
quad7_dom_mat <- as.matrix(quad7_dom[,-1])
quad7_dom_tab <- table(quad7_dom_mat[,-1]) # gives count of each species in the quadrat
quad7_dom_tab
quad7_lng <- quad7_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad7_lng)
quad7_lng <- mutate(quad7_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad7_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad7_dom_tab 
quad_lng     <- quad7_lng

## QUAD 8
quad8_dom <- subset(Dominant, quad_no == "8")
quad8_dom_mat <- as.matrix(quad8_dom[,-1])
quad8_dom_tab <- table(quad8_dom_mat[,-1]) # gives count of each species in the quadrat
quad8_dom_tab
quad8_lng <- quad8_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad8_lng)
quad8_lng <- mutate(quad8_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad8_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad8_dom_tab 
quad_lng     <- quad8_lng

## QUAD 9
quad9_dom <- subset(Dominant, quad_no == "9")
quad9_dom_mat <- as.matrix(quad9_dom[,-1])
quad9_dom_tab <- table(quad9_dom_mat[,-1]) # gives count of each species in the quadrat
quad9_dom_tab
quad9_lng <- quad9_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad9_lng)
quad9_lng <- mutate(quad9_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad9_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad9_dom_tab 
quad_lng     <- quad9_lng

## quad10
quad10_dom <- subset(Dominant, quad_no == "10")
quad10_dom_mat <- as.matrix(quad10_dom[,-1])
quad10_dom_tab <- table(quad10_dom_mat[,-1]) # gives count of each species in the quadrat
quad10_dom_tab
quad10_lng <- quad10_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad10_lng)
quad10_lng <- mutate(quad10_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad10_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad10_dom_tab 
quad_lng     <- quad10_lng

## quad11
quad11_dom <- subset(Dominant, quad_no == "11")
quad11_dom_mat <- as.matrix(quad11_dom[,-1])
quad11_dom_tab <- table(quad11_dom_mat[,-1]) # gives count of each species in the quadrat
quad11_dom_tab
quad11_lng <- quad11_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad11_lng)
quad11_lng <- mutate(quad11_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad11_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad11_dom_tab 
quad_lng     <- quad11_lng

## QUAD 12
quad12_dom <- subset(Dominant, quad_no == "12")
quad12_dom_mat <- as.matrix(quad12_dom[,-1])
quad12_dom_tab <- table(quad12_dom_mat[,-1]) # gives count of each species in the quadrat
quad12_dom_tab
quad12_lng <- quad12_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad12_lng)
quad12_lng <- mutate(quad12_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad12_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad12_dom_tab 
quad_lng     <- quad12_lng

## QUAD 13
quad13_dom <- subset(Dominant, quad_no == "13")
quad13_dom_mat <- as.matrix(quad13_dom[,-1])
quad13_dom_tab <- table(quad13_dom_mat[,-1]) # gives count of each species in the quadrat
quad13_dom_tab
quad13_lng <- quad13_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad13_lng)
quad13_lng <- mutate(quad13_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad13_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad13_dom_tab 
quad_lng     <- quad13_lng

## QUAD 14
quad14_dom <- subset(Dominant, quad_no == "14")
quad14_dom_mat <- as.matrix(quad14_dom[,-1])
quad14_dom_tab <- table(quad14_dom_mat[,-1]) # gives count of each species in the quadrat
quad14_dom_tab
quad14_lng <- quad14_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad14_lng)
quad14_lng <- mutate(quad14_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad14_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad14_dom_tab 
quad_lng     <- quad14_lng

## QUAD 15
quad15_dom <- subset(Dominant, quad_no == "15")
quad15_dom_mat <- as.matrix(quad15_dom[,-1])
quad15_dom_tab <- table(quad15_dom_mat[,-1]) # gives count of each species in the quadrat
quad15_dom_tab
quad15_lng <- quad15_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad15_lng)
quad15_lng <- mutate(quad15_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad15_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad15_dom_tab 
quad_lng     <- quad15_lng

## QUAD 16
quad16_dom <- subset(Dominant, quad_no == "16")
quad16_dom_mat <- as.matrix(quad16_dom[,-1])
quad16_dom_tab <- table(quad16_dom_mat[,-1]) # gives count of each species in the quadrat
quad16_dom_tab
quad16_lng <- quad16_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad16_lng)
quad16_lng <- mutate(quad16_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad16_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad16_dom_tab 
quad_lng     <- quad16_lng

## QUAD 17
quad17_dom <- subset(Dominant, quad_no == "17")
quad17_dom_mat <- as.matrix(quad17_dom[,-1])
quad17_dom_tab <- table(quad17_dom_mat[,-1]) # gives count of each species in the quadrat
quad17_dom_tab
quad17_lng <- quad17_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad17_lng)
quad17_lng <- mutate(quad17_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad17_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad17_dom_tab 
quad_lng     <- quad17_lng

## QUAD 18
quad18_dom <- subset(Dominant, quad_no == "18")
quad18_dom_mat <- as.matrix(quad18_dom[,-1])
quad18_dom_tab <- table(quad18_dom_mat[,-1]) # gives count of each species in the quadrat
quad18_dom_tab
quad18_lng <- quad18_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad18_lng)
quad18_lng <- mutate(quad18_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad18_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad18_dom_tab 
quad_lng     <- quad18_lng

## QUAD 19
quad19_dom <- subset(Dominant, quad_no == "19")
quad19_dom_mat <- as.matrix(quad19_dom[,-1])
quad19_dom_tab <- table(quad19_dom_mat[,-1]) # gives count of each species in the quadrat
quad19_dom_tab
quad19_lng <- quad19_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad19_lng)
quad19_lng <- mutate(quad19_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad19_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad19_dom_tab 
quad_lng     <- quad19_lng

## QUAD 20
quad20_dom <- subset(Dominant, quad_no == "20")
quad20_dom_mat <- as.matrix(quad20_dom[,-1])
quad20_dom_tab <- table(quad20_dom_mat[,-1]) # gives count of each species in the quadrat
quad20_dom_tab
quad20_lng <- quad20_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad20_lng)
quad20_lng <- mutate(quad20_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad20_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad20_dom_tab 
quad_lng     <- quad20_lng

## QUAD 21
quad21_dom <- subset(Dominant, quad_no == "21")
quad21_dom_mat <- as.matrix(quad21_dom[,-1])
quad21_dom_tab <- table(quad21_dom_mat[,-1]) # gives count of each species in the quadrat
quad21_dom_tab
quad21_lng <- quad21_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad21_lng)
quad21_lng <- mutate(quad21_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad21_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad21_dom_tab 
quad_lng     <- quad21_lng

## QUAD 22
quad22_dom <- subset(Dominant, quad_no == "22")
quad22_dom_mat <- as.matrix(quad22_dom[,-1])
quad22_dom_tab <- table(quad22_dom_mat[,-1]) # gives count of each species in the quadrat
quad22_dom_tab
quad22_lng <- quad22_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad22_lng)
quad22_lng <- mutate(quad22_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad22_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad22_dom_tab 
quad_lng     <- quad22_lng

## QUAD 23
quad23_dom <- subset(Dominant, quad_no == "23")
quad23_dom_mat <- as.matrix(quad23_dom[,-1])
quad23_dom_tab <- table(quad23_dom_mat[,-1]) # gives count of each species in the quadrat
quad23_dom_tab
quad23_lng <- quad23_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad23_lng)
quad23_lng <- mutate(quad23_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad23_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad23_dom_tab 
quad_lng     <- quad23_lng

## QUAD 24
quad24_dom <- subset(Dominant, quad_no == "24")
quad24_dom_mat <- as.matrix(quad24_dom[,-1])
quad24_dom_tab <- table(quad24_dom_mat[,-1]) # gives count of each species in the quadrat
quad24_dom_tab
quad24_lng <- quad24_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad24_lng)
quad24_lng <- mutate(quad24_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad24_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad24_dom_tab 
quad_lng     <- quad24_lng

## QUAD 25
quad25_dom <- subset(Dominant, quad_no == "25")
quad25_dom_mat <- as.matrix(quad25_dom[,-1])
quad25_dom_tab <- table(quad25_dom_mat[,-1]) # gives count of each species in the quadrat
quad25_dom_tab
quad25_lng <- quad25_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad25_lng)
quad25_lng <- mutate(quad25_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad25_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad25_dom_tab 
quad_lng     <- quad25_lng

## QUAD 26
quad26_dom <- subset(Dominant, quad_no == "26")
quad26_dom_mat <- as.matrix(quad26_dom[,-1])
quad26_dom_tab <- table(quad26_dom_mat[,-1]) # gives count of each species in the quadrat
quad26_dom_tab
quad26_lng <- quad26_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad26_lng)
quad26_lng <- mutate(quad26_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad26_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad26_dom_tab 
quad_lng     <- quad26_lng

## QUAD 27
quad27_dom <- subset(Dominant, quad_no == "27")
quad27_dom_mat <- as.matrix(quad27_dom[,-1])
quad27_dom_tab <- table(quad27_dom_mat[,-1]) # gives count of each species in the quadrat
quad27_dom_tab
quad27_lng <- quad27_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad27_lng)
quad27_lng <- mutate(quad27_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad27_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad27_dom_tab 
quad_lng     <- quad27_lng

## QUAD 28
quad28_dom <- subset(Dominant, quad_no == "28")
quad28_dom_mat <- as.matrix(quad28_dom[,-1])
quad28_dom_tab <- table(quad28_dom_mat[,-1]) # gives count of each species in the quadrat
quad28_dom_tab
quad28_lng <- quad28_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad28_lng)
quad28_lng <- mutate(quad28_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad28_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad28_dom_tab 
quad_lng     <- quad28_lng

## QUAD 30
quad29_dom <- subset(Dominant, quad_no == "29")
quad29_dom_mat <- as.matrix(quad29_dom[,-1])
quad29_dom_tab <- table(quad29_dom_mat[,-1]) # gives count of each species in the quadrat
quad29_dom_tab
quad29_lng <- quad29_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad29_lng)
quad29_lng <- mutate(quad29_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad29_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad29_dom_tab 
quad_lng     <- quad29_lng

## QUAD 30
quad30_dom <- subset(Dominant, quad_no == "30")
quad30_dom_mat <- as.matrix(quad30_dom[,-1])
quad30_dom_tab <- table(quad30_dom_mat[,-1]) # gives count of each species in the quadrat
quad30_dom_tab
quad30_lng <- quad30_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad30_lng)
quad30_lng <- mutate(quad30_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad30_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad30_dom_tab 
quad_lng     <- quad30_lng

## QUAD 31
quad31_dom <- subset(Dominant, quad_no == "31")
quad31_dom_mat <- as.matrix(quad31_dom[,-1])
quad31_dom_tab <- table(quad31_dom_mat[,-1]) # gives count of each species in the quadrat
quad31_dom_tab
quad31_lng <- quad31_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad31_lng)
quad31_lng <- mutate(quad31_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad31_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad31_dom_tab 
quad_lng     <- quad31_lng

## QUAD 32
quad32_dom <- subset(Dominant, quad_no == "32")
quad32_dom_mat <- as.matrix(quad32_dom[,-1])
quad32_dom_tab <- table(quad32_dom_mat[,-1]) # gives count of each species in the quadrat
quad32_dom_tab
quad32_lng <- quad32_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad32_lng)
quad32_lng <- mutate(quad32_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad32_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad32_dom_tab 
quad_lng     <- quad32_lng

## QUAD 33
quad33_dom <- subset(Dominant, quad_no == "33")
quad33_dom_mat <- as.matrix(quad33_dom[,-1])
quad33_dom_tab <- table(quad33_dom_mat[,-1]) # gives count of each species in the quadrat
quad33_dom_tab
quad33_lng <- quad33_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad33_lng)
quad33_lng <- mutate(quad33_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad33_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad33_dom_tab 
quad_lng     <- quad33_lng

## QUAD 34
quad34_dom <- subset(Dominant, quad_no == "34")
quad34_dom_mat <- as.matrix(quad34_dom[,-1])
quad34_dom_tab <- table(quad34_dom_mat[,-1]) # gives count of each species in the quadrat
quad34_dom_tab
quad34_lng <- quad34_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad34_lng)
quad34_lng <- mutate(quad34_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad34_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad34_dom_tab 
quad_lng     <- quad34_lng

## QUAD 35
quad35_dom <- subset(Dominant, quad_no == "35")
quad35_dom_mat <- as.matrix(quad35_dom[,-1])
quad35_dom_tab <- table(quad35_dom_mat[,-1]) # gives count of each species in the quadrat
quad35_dom_tab
quad35_lng <- quad35_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad35_lng)
quad35_lng <- mutate(quad35_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad35_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad35_dom_tab 
quad_lng     <- quad35_lng

## QUAD 36
quad36_dom <- subset(Dominant, quad_no == "36")
quad36_dom_mat <- as.matrix(quad36_dom[,-1])
quad36_dom_tab <- table(quad36_dom_mat[,-1]) # gives count of each species in the quadrat
quad36_dom_tab
quad36_lng <- quad36_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad36_lng)
quad36_lng <- mutate(quad36_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad36_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad36_dom_tab 
quad_lng     <- quad36_lng

## QUAD 37
quad37_dom <- subset(Dominant, quad_no == "37")
quad37_dom_mat <- as.matrix(quad37_dom[,-1])
quad37_dom_tab <- table(quad37_dom_mat[,-1]) # gives count of each species in the quadrat
quad37_dom_tab
quad37_lng <- quad37_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad37_lng)
quad37_lng <- mutate(quad37_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad37_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad37_dom_tab 
quad_lng     <- quad37_lng

## QUAD 38
quad38_dom <- subset(Dominant, quad_no == "38")
quad38_dom_mat <- as.matrix(quad38_dom[,-1])
quad38_dom_tab <- table(quad38_dom_mat[,-1]) # gives count of each species in the quadrat
quad38_dom_tab
quad38_lng <- quad38_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad38_lng)
quad38_lng <- mutate(quad38_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad38_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad38_dom_tab 
quad_lng     <- quad38_lng

## QUAD 40
quad39_dom <- subset(Dominant, quad_no == "39")
quad39_dom_mat <- as.matrix(quad39_dom[,-1])
quad39_dom_tab <- table(quad39_dom_mat[,-1]) # gives count of each species in the quadrat
quad39_dom_tab
quad39_lng <- quad39_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad39_lng)
quad39_lng <- mutate(quad39_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad39_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad39_dom_tab 
quad_lng     <- quad39_lng

## QUAD 40
quad40_dom <- subset(Dominant, quad_no == "40")
quad40_dom_mat <- as.matrix(quad40_dom[,-1])
quad40_dom_tab <- table(quad40_dom_mat[,-1]) # gives count of each species in the quadrat
quad40_dom_tab
quad40_lng <- quad40_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad40_lng)
quad40_lng <- mutate(quad40_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad40_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad40_dom_tab 
quad_lng     <- quad40_lng

## QUAD 41
quad41_dom <- subset(Dominant, quad_no == "41")
quad41_dom_mat <- as.matrix(quad41_dom[,-1])
quad41_dom_tab <- table(quad41_dom_mat[,-1]) # gives count of each species in the quadrat
quad41_dom_tab
quad41_lng <- quad41_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad41_lng)
quad41_lng <- mutate(quad41_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad41_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad40_dom_tab 
quad_lng     <- quad40_lng

## QUAD 42
quad42_dom <- subset(Dominant, quad_no == "42")
quad42_dom_mat <- as.matrix(quad42_dom[,-1])
quad42_dom_tab <- table(quad42_dom_mat[,-1]) # gives count of each species in the quadrat
quad42_dom_tab
quad42_lng <- quad42_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad42_lng)
quad42_lng <- mutate(quad42_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad42_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad42_dom_tab 
quad_lng     <- quad42_lng

## QUAD 43
quad43_dom <- subset(Dominant, quad_no == "43")
quad43_dom_mat <- as.matrix(quad43_dom[,-1])
quad43_dom_tab <- table(quad43_dom_mat[,-1]) # gives count of each species in the quadrat
quad43_dom_tab
quad43_lng <- quad43_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad43_lng)
quad43_lng <- mutate(quad43_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad43_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad43_dom_tab 
quad_lng     <- quad43_lng

## QUAD 44
quad44_dom <- subset(Dominant, quad_no == "44")
quad44_dom_mat <- as.matrix(quad44_dom[,-1])
quad44_dom_tab <- table(quad44_dom_mat[,-1]) # gives count of each species in the quadrat
quad44_dom_tab
quad44_lng <- quad44_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad44_lng)
quad44_lng <- mutate(quad44_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad44_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad44_dom_tab 
quad_lng     <- quad44_lng

## QUAD 45
quad45_dom <- subset(Dominant, quad_no == "45")
quad45_dom_mat <- as.matrix(quad45_dom[,-1])
quad45_dom_tab <- table(quad45_dom_mat[,-1]) # gives count of each species in the quadrat
quad45_dom_tab
quad45_lng <- quad45_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad45_lng)
quad45_lng <- mutate(quad45_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad45_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad45_dom_tab 
quad_lng     <- quad45_lng

## QUAD 46
quad46_dom <- subset(Dominant, quad_no == "46")
quad46_dom_mat <- as.matrix(quad46_dom[,-1])
quad46_dom_tab <- table(quad46_dom_mat[,-1]) # gives count of each species in the quadrat
quad46_dom_tab
quad46_lng <- quad46_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad46_lng)
quad46_lng <- mutate(quad46_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad46_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad46_dom_tab 
quad_lng     <- quad46_lng

## QUAD 47
quad47_dom <- subset(Dominant, quad_no == "47")
quad47_dom_mat <- as.matrix(quad47_dom[,-1])
quad47_dom_tab <- table(quad47_dom_mat[,-1]) # gives count of each species in the quadrat
quad47_dom_tab
quad47_lng <- quad47_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad47_lng)
quad47_lng <- mutate(quad47_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad47_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad47_dom_tab 
quad_lng     <- quad47_lng

## QUAD 48
quad48_dom <- subset(Dominant, quad_no == "48")
quad48_dom_mat <- as.matrix(quad48_dom[,-1])
quad48_dom_tab <- table(quad48_dom_mat[,-1]) # gives count of each species in the quadrat
quad48_dom_tab
quad48_lng <- quad48_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad48_lng)
quad48_lng <- mutate(quad48_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad48_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad48_dom_tab 
quad_lng     <- quad48_lng

## QUAD 49
quad49_dom <- subset(Dominant, quad_no == "49")
quad49_dom_mat <- as.matrix(quad49_dom[,-1])
quad49_dom_tab <- table(quad49_dom_mat[,-1]) # gives count of each species in the quadrat
quad49_dom_tab
quad49_lng <- quad49_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad49_lng)
quad49_lng <- mutate(quad49_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad49_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad49_dom_tab 
quad_lng     <- quad49_lng

## QUAD 50
quad50_dom <- subset(Dominant, quad_no == "50")
quad50_dom_mat <- as.matrix(quad50_dom[,-1])
quad50_dom_tab <- table(quad50_dom_mat[,-1]) # gives count of each species in the quadrat
quad50_dom_tab
quad50_lng <- quad50_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad50_lng)
quad50_lng <- mutate(quad50_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad50_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad50_dom_tab 
quad_lng     <- quad50_lng

## QUAD 51
quad51_dom <- subset(Dominant, quad_no == "51")
quad51_dom_mat <- as.matrix(quad51_dom[,-1])
quad51_dom_tab <- table(quad51_dom_mat[,-1]) # gives count of each species in the quadrat
quad51_dom_tab
quad51_lng <- quad51_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad51_lng)
quad51_lng <- mutate(quad51_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad51_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad51_dom_tab 
quad_lng     <- quad51_lng

## QUAD 52
quad52_dom <- subset(Dominant, quad_no == "52")
quad52_dom_mat <- as.matrix(quad52_dom[,-1])
quad52_dom_tab <- table(quad52_dom_mat[,-1]) # gives count of each species in the quadrat
quad52_dom_tab
quad52_lng <- quad52_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad52_lng)
quad52_lng <- mutate(quad52_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad52_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad52_dom_tab 
quad_lng     <- quad52_lng


## QUAD 53
quad53_dom <- subset(Dominant, quad_no == "53")
quad53_dom_mat <- as.matrix(quad53_dom[,-1])
quad53_dom_tab <- table(quad53_dom_mat[,-1]) # gives count of each species in the quadrat
quad53_dom_tab
quad53_lng <- quad53_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad53_lng)
quad53_lng <- mutate(quad53_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad53_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad53_dom_tab 
quad_lng     <- quad53_lng

## QUAD 54
quad54_dom <- subset(Dominant, quad_no == "54")
quad54_dom_mat <- as.matrix(quad54_dom[,-1])
quad54_dom_tab <- table(quad54_dom_mat[,-1]) # gives count of each species in the quadrat
quad54_dom_tab
quad54_lng <- quad54_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad54_lng)
quad54_lng <- mutate(quad54_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad54_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad54_dom_tab 
quad_lng     <- quad54_lng

## QUAD 55
quad55_dom <- subset(Dominant, quad_no == "55")
quad55_dom_mat <- as.matrix(quad55_dom[,-1])
quad55_dom_tab <- table(quad55_dom_mat[,-1]) # gives count of each species in the quadrat
quad55_dom_tab
quad55_lng <- quad55_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad55_lng)
quad55_lng <- mutate(quad55_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad55_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad55_dom_tab 
quad_lng     <- quad55_lng

## QUAD 56
quad56_dom <- subset(Dominant, quad_no == "56")
quad56_dom_mat <- as.matrix(quad56_dom[,-1])
quad56_dom_tab <- table(quad56_dom_mat[,-1]) # gives count of each species in the quadrat
quad56_dom_tab
quad56_lng <- quad56_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad56_lng)
quad56_lng <- mutate(quad56_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad56_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad56_dom_tab 
quad_lng     <- quad56_lng

## QUAD 57
quad57_dom <- subset(Dominant, quad_no == "57")
quad57_dom_mat <- as.matrix(quad57_dom[,-1])
quad57_dom_tab <- table(quad57_dom_mat[,-1]) # gives count of each species in the quadrat
quad57_dom_tab
quad57_lng <- quad57_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad57_lng)
quad57_lng <- mutate(quad57_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad57_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad57_dom_tab 
quad_lng     <- quad57_lng

## QUAD 58
quad58_dom <- subset(Dominant, quad_no == "58")
quad58_dom_mat <- as.matrix(quad58_dom[,-1])
quad58_dom_tab <- table(quad58_dom_mat[,-1]) # gives count of each species in the quadrat
quad58_dom_tab
quad58_lng <- quad58_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad58_lng)
quad58_lng <- mutate(quad58_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad58_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad58_dom_tab 
quad_lng     <- quad58_lng

## QUAD 59
quad59_dom <- subset(Dominant, quad_no == "59")
quad59_dom_mat <- as.matrix(quad59_dom[,-1])
quad59_dom_tab <- table(quad59_dom_mat[,-1]) # gives count of each species in the quadrat
quad59_dom_tab
quad59_lng <- quad59_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad59_lng)
quad59_lng <- mutate(quad59_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad59_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad59_dom_tab 
quad_lng     <- quad59_lng

## QUAD 60
quad60_dom <- subset(Dominant, quad_no == "60")
quad60_dom_mat <- as.matrix(quad60_dom[,-1])
quad60_dom_tab <- table(quad60_dom_mat[,-1]) # gives count of each species in the quadrat
quad60_dom_tab
quad60_lng <- quad60_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad60_lng)
quad60_lng <- mutate(quad60_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad60_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad60_dom_tab 
quad_lng     <- quad60_lng

## QUAD 61
quad61_dom <- subset(Dominant, quad_no == "61")
quad61_dom_mat <- as.matrix(quad61_dom[,-1])
quad61_dom_tab <- table(quad61_dom_mat[,-1]) # gives count of each species in the quadrat
quad61_dom_tab
quad61_lng <- quad61_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad61_lng)
quad61_lng <- mutate(quad61_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad61_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad61_dom_tab 
quad_lng     <- quad61_lng

## QUAD 62
quad62_dom <- subset(Dominant, quad_no == "62")
quad62_dom_mat <- as.matrix(quad62_dom[,-1])
quad62_dom_tab <- table(quad62_dom_mat[,-1]) # gives count of each species in the quadrat
quad62_dom_tab
quad62_lng <- quad62_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad62_lng)
quad62_lng <- mutate(quad62_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad62_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad62_dom_tab 
quad_lng     <- quad62_lng

## QUAD 63
quad63_dom <- subset(Dominant, quad_no == "63")
quad63_dom_mat <- as.matrix(quad63_dom[,-1])
quad63_dom_tab <- table(quad63_dom_mat[,-1]) # gives count of each species in the quadrat
quad63_dom_tab
quad63_lng <- quad63_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad63_lng)
quad63_lng <- mutate(quad63_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad63_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad63_dom_tab 
quad_lng     <- quad63_lng

## QUAD 64
quad64_dom <- subset(Dominant, quad_no == "64")
quad64_dom_mat <- as.matrix(quad64_dom[,-1])
quad64_dom_tab <- table(quad64_dom_mat[,-1]) # gives count of each species in the quadrat
quad64_dom_tab
quad64_lng <- quad64_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad64_lng)
quad64_lng <- mutate(quad64_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad64_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad64_dom_tab 
quad_lng     <- quad64_lng

## QUAD 65
quad65_dom <- subset(Dominant, quad_no == "65")
quad65_dom_mat <- as.matrix(quad65_dom[,-1])
quad65_dom_tab <- table(quad65_dom_mat[,-1]) # gives count of each species in the quadrat
quad65_dom_tab
quad65_lng <- quad65_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad65_lng)
quad65_lng <- mutate(quad65_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad65_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad65_dom_tab 
quad_lng     <- quad65_lng

## QUAD 66
quad66_dom <- subset(Dominant, quad_no == "66")
quad66_dom_mat <- as.matrix(quad66_dom[,-1])
quad66_dom_tab <- table(quad66_dom_mat[,-1]) # gives count of each species in the quadrat
quad66_dom_tab
quad66_lng <- quad66_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad66_lng)
quad66_lng <- mutate(quad66_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad66_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad66_dom_tab 
quad_lng     <- quad66_lng

## QUAD 67
quad67_dom <- subset(Dominant, quad_no == "67")
quad67_dom_mat <- as.matrix(quad67_dom[,-1])
quad67_dom_tab <- table(quad67_dom_mat[,-1]) # gives count of each species in the quadrat
quad67_dom_tab
quad67_lng <- quad67_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad67_lng)
quad67_lng <- mutate(quad67_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad67_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad67_dom_tab 
quad_lng     <- quad67_lng

## QUAD 68
quad68_dom <- subset(Dominant, quad_no == "68")
quad68_dom_mat <- as.matrix(quad68_dom[,-1])
quad68_dom_tab <- table(quad68_dom_mat[,-1]) # gives count of each species in the quadrat
quad68_dom_tab
quad68_lng <- quad68_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad68_lng)
quad68_lng <- mutate(quad68_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad68_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad68_dom_tab 
quad_lng     <- quad68_lng

## QUAD 69
quad69_dom <- subset(Dominant, quad_no == "69")
quad69_dom_mat <- as.matrix(quad69_dom[,-1])
quad69_dom_tab <- table(quad69_dom_mat[,-1]) # gives count of each species in the quadrat
quad69_dom_tab
quad69_lng <- quad69_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad69_lng)
quad69_lng <- mutate(quad69_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad69_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad69_dom_tab 
quad_lng     <- quad69_lng

## QUAD 70
quad70_dom <- subset(Dominant, quad_no == "70")
quad70_dom_mat <- as.matrix(quad70_dom[,-1])
quad70_dom_tab <- table(quad70_dom_mat[,-1]) # gives count of each species in the quadrat
quad70_dom_tab
quad70_lng <- quad70_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad70_lng)
quad70_lng <- mutate(quad70_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad70_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad70_dom_tab 
quad_lng     <- quad70_lng

## QUAD 71
quad71_dom <- subset(Dominant, quad_no == "71")
quad71_dom_mat <- as.matrix(quad71_dom[,-1])
quad71_dom_tab <- table(quad71_dom_mat[,-1]) # gives count of each species in the quadrat
quad71_dom_tab
quad71_lng <- quad71_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad71_lng)
quad71_lng <- mutate(quad71_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad71_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad71_dom_tab 
quad_lng     <- quad71_lng

## QUAD 72
quad72_dom <- subset(Dominant, quad_no == "72")
quad72_dom_mat <- as.matrix(quad72_dom[,-1])
quad72_dom_tab <- table(quad72_dom_mat[,-1]) # gives count of each species in the quadrat
quad72_dom_tab
quad72_lng <- quad72_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad72_lng)
quad72_lng <- mutate(quad72_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad72_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad72_dom_tab 
quad_lng     <- quad72_lng

## QUAD 73
quad73_dom <- subset(Dominant, quad_no == "73")
quad73_dom_mat <- as.matrix(quad73_dom[,-1])
quad73_dom_tab <- table(quad73_dom_mat[,-1]) # gives count of each species in the quadrat
quad73_dom_tab
quad73_lng <- quad73_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad73_lng)
quad73_lng <- mutate(quad73_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad73_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad73_dom_tab 
quad_lng     <- quad73_lng

## QUAD 74
quad74_dom <- subset(Dominant, quad_no == "74")
quad74_dom_mat <- as.matrix(quad74_dom[,-1])
quad74_dom_tab <- table(quad74_dom_mat[,-1]) # gives count of each species in the quadrat
quad74_dom_tab
quad74_lng <- quad74_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad74_lng)
quad74_lng <- mutate(quad74_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad74_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad74_dom_tab 
quad_lng     <- quad74_lng

## QUAD 75
quad75_dom <- subset(Dominant, quad_no == "75")
quad75_dom_mat <- as.matrix(quad75_dom[,-1])
quad75_dom_tab <- table(quad75_dom_mat[,-1]) # gives count of each species in the quadrat
quad75_dom_tab
quad75_lng <- quad75_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad75_lng)
quad75_lng <- mutate(quad75_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad75_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad75_dom_tab 
quad_lng     <- quad75_lng

## QUAD 76
quad76_dom <- subset(Dominant, quad_no == "1")
quad76_dom_mat <- as.matrix(quad76_dom[,-1])
quad76_dom_tab <- table(quad76_dom_mat[,-1]) # gives count of each species in the quadrat
quad76_dom_tab
quad76_lng <- quad76_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad76_lng)
quad76_lng <- mutate(quad76_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad76_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad76_dom_tab 
quad_lng     <- quad76_lng

## QUAD 77
quad77_dom <- subset(Dominant, quad_no == "77")
quad77_dom_mat <- as.matrix(quad77_dom[,-1])
quad77_dom_tab <- table(quad77_dom_mat[,-1]) # gives count of each species in the quadrat
quad77_dom_tab
quad77_lng <- quad77_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad77_lng)
quad77_lng <- mutate(quad77_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad77_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad77_dom_tab 
quad_lng     <- quad77_lng

## QUAD 78
quad78_dom <- subset(Dominant, quad_no == "78")
quad78_dom_mat <- as.matrix(quad78_dom[,-1])
quad78_dom_tab <- table(quad78_dom_mat[,-1]) # gives count of each species in the quadrat
quad78_dom_tab
quad78_lng <- quad78_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad78_lng)
quad78_lng <- mutate(quad78_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad78_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad78_dom_tab 
quad_lng     <- quad78_lng

## QUAD 79
quad79_dom <- subset(Dominant, quad_no == "79")
quad79_dom_mat <- as.matrix(quad79_dom[,-1])
quad79_dom_tab <- table(quad79_dom_mat[,-1]) # gives count of each species in the quadrat
quad79_dom_tab
quad79_lng <- quad79_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad79_lng)
quad79_lng <- mutate(quad79_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad79_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad79_dom_tab 
quad_lng     <- quad79_lng

## QUAD 80
quad80_dom <- subset(Dominant, quad_no == "80")
quad80_dom_mat <- as.matrix(quad80_dom[,-1])
quad80_dom_tab <- table(quad80_dom_mat[,-1]) # gives count of each species in the quadrat
quad80_dom_tab
quad80_lng <- quad80_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad80_lng)
quad80_lng <- mutate(quad80_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad80_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad80_dom_tab 
quad_lng     <- quad80_lng

## QUAD 81
quad81_dom <- subset(Dominant, quad_no == "81")
quad81_dom_mat <- as.matrix(quad81_dom[,-1])
quad81_dom_tab <- table(quad81_dom_mat[,-1]) # gives count of each species in the quadrat
quad81_dom_tab
quad81_lng <- quad81_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad81_lng)
quad81_lng <- mutate(quad81_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad81_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad81_dom_tab 
quad_lng     <- quad81_lng

## QUAD 82
quad82_dom <- subset(Dominant, quad_no == "82")
quad82_dom_mat <- as.matrix(quad82_dom[,-1])
quad82_dom_tab <- table(quad82_dom_mat[,-1]) # gives count of each species in the quadrat
quad82_dom_tab
quad82_lng <- quad82_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad82_lng)
quad82_lng <- mutate(quad82_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad82_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad82_dom_tab 
quad_lng     <- quad82_lng

## QUAD 83
quad83_dom <- subset(Dominant, quad_no == "83")
quad83_dom_mat <- as.matrix(quad83_dom[,-1])
quad83_dom_tab <- table(quad83_dom_mat[,-1]) # gives count of each species in the quadrat
quad83_dom_tab
quad83_lng <- quad83_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad83_lng)
quad83_lng <- mutate(quad83_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad83_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad83_dom_tab <- quad83_dom_tab 
quad_lng     <- quad83_lng

## QUAD 84
quad84_dom <- subset(Dominant, quad_no == "84")
quad84_dom_mat <- as.matrix(quad84_dom[,-1])
quad84_dom_tab <- table(quad84_dom_mat[,-1]) # gives count of each species in the quadrat
quad84_dom_tab
quad84_lng <- quad84_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad84_lng)
quad84_lng <- mutate(quad84_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad84_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad84_dom_tab 
quad_lng     <- quad84_lng

## QUAD 85
quad85_dom <- subset(Dominant, quad_no == "85")
quad85_dom_mat <- as.matrix(quad85_dom[,-1])
quad85_dom_tab <- table(quad85_dom_mat[,-1]) # gives count of each species in the quadrat
quad85_dom_tab
quad85_lng <- quad85_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad85_lng)
quad85_lng <- mutate(quad85_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad85_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad85_dom_tab 
quad_lng     <- quad85_lng

## QUAD 86
quad86_dom <- subset(Dominant, quad_no == "86")
quad86_dom_mat <- as.matrix(quad86_dom[,-1])
quad86_dom_tab <- table(quad86_dom_mat[,-1]) # gives count of each species in the quadrat
quad86_dom_tab
quad86_lng <- quad86_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad86_lng)
quad86_lng <- mutate(quad86_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad86_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad86_dom_tab 
quad_lng     <- quad86_lng

## QUAD 87
quad87_dom <- subset(Dominant, quad_no == "87")
quad87_dom_mat <- as.matrix(quad87_dom[,-1])
quad87_dom_tab <- table(quad87_dom_mat[,-1]) # gives count of each species in the quadrat
quad87_dom_tab
quad87_lng <- quad87_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad87_lng)
quad87_lng <- mutate(quad87_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad87_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad87_dom_tab <- quad87_dom_tab 
quad_lng     <- quad87_lng

## QUAD 88
quad88_dom <- subset(Dominant, quad_no == "88")
quad88_dom_mat <- as.matrix(quad88_dom[,-1])
quad88_dom_tab <- table(quad88_dom_mat[,-1]) # gives count of each species in the quadrat
quad88_dom_tab
quad88_lng <- quad88_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad88_lng)
quad88_lng <- mutate(quad88_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad88_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad88_dom_tab 
quad_lng     <- quad88_lng

## QUAD 89
quad89_dom <- subset(Dominant, quad_no == "89")
quad89_dom_mat <- as.matrix(quad89_dom[,-1])
quad89_dom_tab <- table(quad89_dom_mat[,-1]) # gives count of each species in the quadrat
quad89_dom_tab
quad89_lng <- quad89_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad89_lng)
quad89_lng <- mutate(quad89_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad89_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad89_dom_tab 
quad_lng     <- quad89_lng

## QUAD 90
quad90_dom <- subset(Dominant, quad_no == "90")
quad90_dom_mat <- as.matrix(quad90_dom[,-1])
quad90_dom_tab <- table(quad90_dom_mat[,-1]) # gives count of each species in the quadrat
quad90_dom_tab
quad90_lng <- quad90_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad90_lng)
quad90_lng <- mutate(quad90_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad90_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad90_dom_tab 
quad_lng     <- quad90_lng


## QUAD 91
quad91_dom<- subset(Dominant, quad_no == "91")
quad91_dom_mat <- as.matrix(quad91_dom[,-1])
quad91_dom_tab <- table(quad91_dom_mat[,-1]) # gives count of each species in the quadrat
quad91_dom_tab
quad91_lng <- quad91_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad91_lng)
quad91_lng <- mutate(quad91_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad91_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad91_dom_tab 
quad_lng     <- quad91_lng

## QUAD 92
quad92_dom<- subset(Dominant, quad_no == "92")
quad92_dom_mat <- as.matrix(quad92_dom[,-1])
quad92_dom_tab <- table(quad92_dom_mat[,-1]) # gives count of each species in the quadrat
quad92_dom_tab
quad92_lng <- quad92_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad92_lng)
quad92_lng <- mutate(quad92_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad92_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad92_dom_tab 
quad_lng     <- quad92_lng

## QUAD 93
quad93_dom<- subset(Dominant, quad_no == "93")
quad93_dom_mat <- as.matrix(quad93_dom[,-1])
quad93_dom_tab <- table(quad93_dom_mat[,-1]) # gives count of each species in the quadrat
quad93_dom_tab
quad93_lng <- quad93_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad93_lng)
quad93_lng <- mutate(quad93_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad93_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad93_dom_tab 
quad_lng     <- quad93_lng

## QUAD 94
quad94_dom <- subset(Dominant, quad_no == "94")
quad94_dom_mat <- as.matrix(quad94_dom[,-1])
quad94_dom_tab <- table(quad94_dom_mat[,-1]) # gives count of each species in the quadrat
quad94_dom_tab
quad94_lng <- quad94_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad94_lng)
quad94_lng <- mutate(quad94_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad94_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad94_dom_tab 
quad_lng     <- quad94_lng

## QUAD 95
quad95_dom <- subset(Dominant, quad_no == "95")
quad95_dom_mat <- as.matrix(quad95_dom[,-1])
quad95_dom_tab <- table(quad95_dom_mat[,-1]) # gives count of each species in the quadrat
quad95_dom_tab
quad95_lng <- quad95_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad95_lng)
quad95_lng <- mutate(quad95_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad95_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad95_dom_tab 
quad_lng     <- quad95_lng

## QUAD 96
quad96_dom <- subset(Dominant, quad_no == "96")
quad96_dom_mat <- as.matrix(quad96_dom[,-1])
quad96_dom_tab <- table(quad96_dom_mat[,-1]) # gives count of each species in the quadrat
quad96_dom_tab
quad96_lng <- quad96_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad96_lng)
quad96_lng <- mutate(quad96_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad96_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad96_dom_tab 
quad_lng     <- quad96_lng

## QUAD 97
quad97_dom <- subset(Dominant, quad_no == "97")
quad97_dom_mat <- as.matrix(quad97_dom[,-1])
quad97_dom_tab <- table(quad97_dom_mat[,-1]) # gives count of each species in the quadrat
quad97_dom_tab
quad97_lng <- quad97_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad97_lng)
quad97_lng <- mutate(quad97_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad97_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad97_dom_tab 
quad_lng     <- quad97_lng

## QUAD 98
quad98_dom <- subset(Dominant, quad_no == "98")
quad98_dom_mat <- as.matrix(quad98_dom[,-1])
quad98_dom_tab <- table(quad98_dom_mat[,-1]) # gives count of each species in the quadrat
quad98_dom_tab
quad98_lng <- quad98_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad98_lng)
quad98_lng <- mutate(quad98_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad98_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad98_dom_tab 
quad_lng     <- quad98_lng

## QUAD 99
quad99_dom <- subset(Dominant, quad_no == "99")
quad99_dom_mat <- as.matrix(quad99_dom[,-1])
quad99_dom_tab <- table(quad99_dom_mat[,-1]) # gives count of each species in the quadrat
quad99_dom_tab
quad99_lng <- quad99_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad99_lng)
quad99_lng <- mutate(quad99_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad99_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad99_dom_tab 
quad_lng     <- quad99_lng

## QUAD 100
quad100_dom <- subset(Dominant, quad_no == "100")
quad100_dom_mat <- as.matrix(quad100_dom[,-1])
quad100_dom_tab <- table(quad100_dom_mat[,-1]) # gives count of each species in the quadrat
quad100_dom_tab
quad100_lng <- quad100_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad100_lng)
quad100_lng <- mutate(quad100_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad100_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad100_dom_tab 
quad_lng     <- quad100_lng

## QUAD 101
quad101_dom <- subset(Dominant, quad_no == "101")
quad101_dom_mat <- as.matrix(quad101_dom[,-1])
quad101_dom_tab <- table(quad101_dom_mat[,-1]) # gives count of each species in the quadrat
quad101_dom_tab
quad101_lng <- quad101_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad101_lng)
quad101_lng <- mutate(quad101_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad101_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad101_dom_tab 
quad_lng     <- quad101_lng


## QUAD 102
quad102_dom <- subset(Dominant, quad_no == "102")
quad102_dom_mat <- as.matrix(quad102_dom[,-1])
quad102_dom_tab <- table(quad102_dom_mat[,-1]) # gives count of each species in the quadrat
quad102_dom_tab
quad102_lng <- quad102_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad102_lng)
quad102_lng <- mutate(quad102_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad102_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad102_dom_tab 
quad_lng     <- quad102_lng

## QUAD 103
quad103_dom <- subset(Dominant, quad_no == "103")
quad103_dom_mat <- as.matrix(quad103_dom[,-1])
quad103_dom_tab <- table(quad103_dom_mat[,-1]) # gives count of each species in the quadrat
quad103_dom_tab
quad103_lng <- quad103_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad103_lng)
quad103_lng <- mutate(quad103_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad103_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad103_dom_tab 
quad_lng     <- quad103_lng

## QUAD 104
quad104_dom <- subset(Dominant, quad_no == "104")
quad104_dom_mat <- as.matrix(quad104_dom[,-1])
quad104_dom_tab <- table(quad104_dom_mat[,-1]) # gives count of each species in the quadrat
quad104_dom_tab
quad104_lng <- quad104_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad104_lng)
quad104_lng <- mutate(quad104_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad104_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad104_dom_tab 
quad_lng     <- quad104_lng

## QUAD 105
quad105_dom <- subset(Dominant, quad_no == "105")
quad105_dom_mat <- as.matrix(quad105_dom[,-1])
quad105_dom_tab <- table(quad105_dom_mat[,-1]) # gives count of each species in the quadrat
quad105_dom_tab
quad105_lng <- quad105_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad105_lng)
quad105_lng <- mutate(quad105_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad105_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad105_dom_tab 
quad_lng     <- quad105_lng

## QUAD 106
quad106_dom <- subset(Dominant, quad_no == "106")
quad106_dom_mat <- as.matrix(quad106_dom[,-1])
quad106_dom_tab <- table(quad106_dom_mat[,-1]) # gives count of each species in the quadrat
quad106_dom_tab
quad106_lng <- quad106_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad106_lng)
quad106_lng <- mutate(quad106_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad106_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad106_dom_tab 
quad_lng     <- quad106_lng

## QUAD 107
quad107_dom <- subset(Dominant, quad_no == "107")
quad107_dom_mat <- as.matrix(quad107_dom[,-1])
quad107_dom_tab <- table(quad107_dom_mat[,-1]) # gives count of each species in the quadrat
quad107_dom_tab
quad107_lng <- quad107_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad107_lng)
quad107_lng <- mutate(quad107_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad107_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad107_dom_tab 
quad_lng     <- quad107_lng

## QUAD 108
quad108_dom <- subset(Dominant, quad_no == "108")
quad108_dom_mat <- as.matrix(quad108_dom[,-1])
quad108_dom_tab <- table(quad108_dom_mat[,-1]) # gives count of each species in the quadrat
quad108_dom_tab
quad108_lng <- quad108_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad108_lng)
quad108_lng <- mutate(quad108_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad108_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad108_dom_tab 
quad_lng     <- quad108_lng

## QUAD 109
quad109_dom <- subset(Dominant, quad_no == "109")
quad109_dom_mat <- as.matrix(quad109_dom[,-1])
quad109_dom_tab <- table(quad109_dom_mat[,-1]) # gives count of each species in the quadrat
quad109_dom_tab
quad109_lng <- quad109_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad109_lng)
quad109_lng <- mutate(quad109_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad109_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad109_dom_tab 
quad_lng     <- quad109_lng

## QUAD 110
quad110_dom <- subset(Dominant, quad_no == "110")
quad110_dom_mat <- as.matrix(quad110_dom[,-1])
quad110_dom_tab <- table(quad110_dom_mat[,-1]) # gives count of each species in the quadrat
quad110_dom_tab
quad110_lng <- quad110_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad110_lng)
quad110_lng <- mutate(quad110_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad110_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad110_dom_tab 
quad_lng     <- quad110_lng

## QUAD 111
quad111_dom <- subset(Dominant, quad_no == "111")
quad111_dom_mat <- as.matrix(quad111_dom[,-1])
quad111_dom_tab <- table(quad111_dom_mat[,-1]) # gives count of each species in the quadrat
quad111_dom_tab
quad111_lng <- quad111_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad111_lng)
quad111_lng <- mutate(quad111_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad111_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad111_dom_tab 
quad_lng     <- quad111_lng

## QUAD 112
quad112_dom <- subset(Dominant, quad_no == "112")
quad112_dom_mat <- as.matrix(quad112_dom[,-1])
quad112_dom_tab <- table(quad112_dom_mat[,-1]) # gives count of each species in the quadrat
quad112_dom_tab
quad112_lng <- quad112_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad112_lng)
quad112_lng <- mutate(quad112_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad112_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad112_dom_tab 
quad_lng     <- quad112_lng

## QUAD 113
quad113_dom <- subset(Dominant, quad_no == "113")
quad113_dom_mat <- as.matrix(quad113_dom[,-1])
quad113_dom_tab <- table(quad113_dom_mat[,-1]) # gives count of each species in the quadrat
quad113_dom_tab
quad113_lng <- quad113_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad113_lng)
quad113_lng <- mutate(quad113_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad113_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad113_dom_tab 
quad_lng     <- quad113_lng

## QUAD 114
quad114_dom <- subset(Dominant, quad_no == "114")
quad114_dom_mat <- as.matrix(quad114_dom[,-1])
quad114_dom_tab <- table(quad114_dom_mat[,-1]) # gives count of each species in the quadrat
quad114_dom_tab
quad114_lng <- quad114_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad114_lng)
quad114_lng <- mutate(quad114_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad114_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad114_dom_tab 
quad_lng     <- quad114_lng

## QUAD 115
quad115_dom <- subset(Dominant, quad_no == "115")
quad115_dom_mat <- as.matrix(quad115_dom[,-1])
quad115_dom_tab <- table(quad115_dom_mat[,-1]) # gives count of each species in the quadrat
quad115_dom_tab
quad115_lng <- quad115_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad115_lng)
quad115_lng <- mutate(quad115_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad115_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad115_dom_tab 
quad_lng     <- quad115_lng

## QUAD 116
quad116_dom <- subset(Dominant, quad_no == "116")
quad116_dom_mat <- as.matrix(quad116_dom[,-1])
quad116_dom_tab <- table(quad116_dom_mat[,-1]) # gives count of each species in the quadrat
quad116_dom_tab
quad116_lng <- quad116_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad116_lng)
quad116_lng <- mutate(quad116_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad116_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad116_dom_tab 
quad_lng     <- quad116_lng

## QUAD 117
quad117_dom <- subset(Dominant, quad_no == "117")
quad117_dom_mat <- as.matrix(quad117_dom[,-1])
quad117_dom_tab <- table(quad117_dom_mat[,-1]) # gives count of each species in the quadrat
quad117_dom_tab
quad117_lng <- quad117_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad117_lng)
quad117_lng <- mutate(quad117_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad117_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad117_dom_tab 
quad_lng     <- quad117_lng

## QUAD 118
quad118_dom <- subset(Dominant, quad_no == "118")
quad118_dom_mat <- as.matrix(quad118_dom[,-1])
quad118_dom_tab <- table(quad118_dom_mat[,-1]) # gives count of each species in the quadrat
quad118_dom_tab
quad118_lng <- quad118_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad118_lng)
quad118_lng <- mutate(quad118_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad118_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad118_dom_tab 
quad_lng     <- quad118_lng

## QUAD 119
quad119_dom <- subset(Dominant, quad_no == "119")
quad119_dom_mat <- as.matrix(quad119_dom[,-1])
quad119_dom_tab <- table(quad119_dom_mat[,-1]) # gives count of each species in the quadrat
quad119_dom_tab
quad119_lng <- quad119_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad119_lng)
quad119_lng <- mutate(quad119_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad119_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad119_dom_tab 
quad_lng     <- quad119_lng

## QUAD 120
quad120_dom <- subset(Dominant, quad_no == "120")
quad120_dom_mat <- as.matrix(quad120_dom[,-1])
quad120_dom_tab <- table(quad120_dom_mat[,-1]) # gives count of each species in the quadrat
quad120_dom_tab
quad120_lng <- quad120_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad120_lng)
quad120_lng <- mutate(quad120_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad120_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad120_dom_tab 
quad_lng     <- quad120_lng

## QUAD 121
quad121_dom <- subset(Dominant, quad_no == "121")
quad121_dom_mat <- as.matrix(quad121_dom[,-1])
quad121_dom_tab <- table(quad121_dom_mat[,-1]) # gives count of each species in the quadrat
quad121_dom_tab
quad121_lng <- quad121_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad121_lng)
quad121_lng <- mutate(quad121_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad121_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad121_dom_tab 
quad_lng     <- quad121_lng

## QUAD 122
quad122_dom <- subset(Dominant, quad_no == "122")
quad122_dom_mat <- as.matrix(quad122_dom[,-1])
quad122_dom_tab <- table(quad122_dom_mat[,-1]) # gives count of each species in the quadrat
quad122_dom_tab
quad122_lng <- quad122_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad122_lng)
quad122_lng <- mutate(quad122_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad122_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad122_dom_tab 
quad_lng     <- quad122_lng

## QUAD 123
quad123_dom <- subset(Dominant, quad_no == "123")
quad123_dom_mat <- as.matrix(quad123_dom[,-1])
quad123_dom_tab <- table(quad123_dom_mat[,-1]) # gives count of each species in the quadrat
quad123_dom_tab
quad123_lng <- quad123_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad123_lng)
quad123_lng <- mutate(quad123_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad123_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad123_dom_tab 
quad_lng     <- quad123_lng

## QUAD 124
quad124_dom <- subset(Dominant, quad_no == "124")
quad124_dom_mat <- as.matrix(quad124_dom[,-1])
quad124_dom_tab <- table(quad124_dom_mat[,-1]) # gives count of each species in the quadrat
quad124_dom_tab
quad124_lng <- quad124_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad124_lng)
quad124_lng <- mutate(quad124_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad124_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad124_dom_tab 
quad_lng     <- quad124_lng

## QUAD 125
quad125_dom <- subset(Dominant, quad_no == "125")
quad125_dom_mat <- as.matrix(quad125_dom[,-1])
quad125_dom_tab <- table(quad125_dom_mat[,-1]) # gives count of each species in the quadrat
quad125_dom_tab
quad125_lng <- quad125_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad125_lng)
quad125_lng <- mutate(quad125_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad125_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad125_dom_tab 
quad_lng     <- quad125_lng

## QUAD 126
quad126_dom <- subset(Dominant, quad_no == "126")
quad126_dom_mat <- as.matrix(quad126_dom[,-1])
quad126_dom_tab <- table(quad126_dom_mat[,-1]) # gives count of each species in the quadrat
quad126_dom_tab
quad126_lng <- quad126_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad126_lng)
quad126_lng <- mutate(quad126_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad126_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad126_dom_tab 
quad_lng     <- quad126_lng

## QUAD 127
quad127_dom <- subset(Dominant, quad_no == "127")
quad127_dom_mat <- as.matrix(quad127_dom[,-1])
quad127_dom_tab <- table(quad127_dom_mat[,-1]) # gives count of each species in the quadrat
quad127_dom_tab
quad127_lng <- quad127_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad127_lng)
quad127_lng <- mutate(quad127_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad127_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad127_dom_tab 
quad_lng     <- quad127_lng

## QUAD 128
quad128_dom <- subset(Dominant, quad_no == "128")
quad128_dom_mat <- as.matrix(quad128_dom[,-1])
quad128_dom_tab <- table(quad128_dom_mat[,-1]) # gives count of each species in the quadrat
quad128_dom_tab
quad128_lng <- quad128_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad128_lng)
quad128_lng <- mutate(quad128_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad128_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad128_dom_tab 
quad_lng     <- quad128_lng

## QUAD 129
quad129_dom <- subset(Dominant, quad_no == "129")
quad129_dom_mat <- as.matrix(quad129_dom[,-1])
quad129_dom_tab <- table(quad129_dom_mat[,-1]) # gives count of each species in the quadrat
quad129_dom_tab
quad129_lng <- quad129_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad129_lng)
quad129_lng <- mutate(quad129_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad129_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad129_dom_tab 
quad_lng     <- quad129_lng

## QUAD 130
quad130_dom <- subset(Dominant, quad_no == "130")
quad130_dom_mat <- as.matrix(quad130_dom[,-1])
quad130_dom_tab <- table(quad130_dom_mat[,-1]) # gives count of each species in the quadrat
quad130_dom_tab
quad130_lng <- quad130_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad130_lng)
quad130_lng <- mutate(quad130_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad130_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad130_dom_tab 
quad_lng     <- quad130_lng

## QUAD 131
quad131_dom <- subset(Dominant, quad_no == "131")
quad131_dom_mat <- as.matrix(quad131_dom[,-1])
quad131_dom_tab <- table(quad131_dom_mat[,-1]) # gives count of each species in the quadrat
quad131_dom_tab
quad131_lng <- quad131_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad131_lng)
quad131_lng <- mutate(quad131_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad131_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad131_dom_tab 
quad_lng     <- quad131_lng

## QUAD 132
quad132_dom <- subset(Dominant, quad_no == "132")
quad132_dom_mat <- as.matrix(quad132_dom[,-1])
quad132_dom_tab <- table(quad132_dom_mat[,-1]) # gives count of each species in the quadrat
quad132_dom_tab
quad132_lng <- quad132_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad132_lng)
quad132_lng <- mutate(quad132_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad132_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad132_dom_tab 
quad_lng     <- quad132_lng

## QUAD 133
quad133_dom <- subset(Dominant, quad_no == "133")
quad133_dom_mat <- as.matrix(quad133_dom[,-1])
quad133_dom_tab <- table(quad133_dom_mat[,-1]) # gives count of each species in the quadrat
quad133_dom_tab
quad133_lng <- quad133_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad133_lng)
quad133_lng <- mutate(quad133_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad133_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad133_dom_tab 
quad_lng     <- quad133_lng

## QUAD 134
quad134_dom <- subset(Dominant, quad_no == "134")
quad134_dom_mat <- as.matrix(quad134_dom[,-1])
quad134_dom_tab <- table(quad134_dom_mat[,-1]) # gives count of each species in the quadrat
quad134_dom_tab
quad134_lng <- quad134_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad134_lng)
quad134_lng <- mutate(quad134_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad134_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad134_dom_tab 
quad_lng     <- quad134_lng

## QUAD 135
quad135_dom <- subset(Dominant, quad_no == "135")
quad135_dom_mat <- as.matrix(quad135_dom[,-1])
quad135_dom_tab <- table(quad135_dom_mat[,-1]) # gives count of each species in the quadrat
quad135_dom_tab
quad135_lng <- quad135_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad135_lng)
quad135_lng <- mutate(quad135_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad135_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad135_dom_tab 
quad_lng     <- quad135_lng

## QUAD 136
quad136_dom <- subset(Dominant, quad_no == "136")
quad136_dom_mat <- as.matrix(quad136_dom[,-1])
quad136_dom_tab <- table(quad136_dom_mat[,-1]) # gives count of each species in the quadrat
quad136_dom_tab
quad136_lng <- quad136_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad136_lng)
quad136_lng <- mutate(quad136_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad136_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad136_dom_tab 
quad_lng     <- quad136_lng

## QUAD 137
quad137_dom <- subset(Dominant, quad_no == "137")
quad137_dom_mat <- as.matrix(quad137_dom[,-1])
quad137_dom_tab <- table(quad137_dom_mat[,-1]) # gives count of each species in the quadrat
quad137_dom_tab
quad137_lng <- quad137_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad137_lng)
quad137_lng <- mutate(quad137_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad137_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad137_dom_tab 
quad_lng     <- quad137_lng

## QUAD 138
quad138_dom <- subset(Dominant, quad_no == "138")
quad138_dom_mat <- as.matrix(quad138_dom[,-1])
quad138_dom_tab <- table(quad138_dom_mat[,-1]) # gives count of each species in the quadrat
quad138_dom_tab
quad138_lng <- quad138_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad138_lng)
quad138_lng <- mutate(quad138_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad138_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad138_dom_tab 
quad_lng     <- quad138_lng

## QUAD 139
quad139_dom <- subset(Dominant, quad_no == "139")
quad139_dom_mat <- as.matrix(quad139_dom[,-1])
quad139_dom_tab <- table(quad139_dom_mat[,-1]) # gives count of each species in the quadrat
quad139_dom_tab
quad139_lng <- quad139_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad139_lng)
quad139_lng <- mutate(quad139_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad139_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad139_dom_tab 
quad_lng     <- quad139_lng

## QUAD 140
quad140_dom <- subset(Dominant, quad_no == "140")
quad140_dom_mat <- as.matrix(quad140_dom[,-1])
quad140_dom_tab <- table(quad140_dom_mat[,-1]) # gives count of each species in the quadrat
quad140_dom_tab
quad140_lng <- quad140_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad140_lng)
quad140_lng <- mutate(quad140_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad140_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad140_dom_tab 
quad_lng     <- quad140_lng

## QUAD 141
quad141_dom <- subset(Dominant, quad_no == "140")
quad141_dom_mat <- as.matrix(quad141_dom[,-1])
quad141_dom_tab <- table(quad141_dom_mat[,-1]) # gives count of each species in the quadrat
quad141_dom_tab
quad141_lng <- quad141_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad141_lng)
quad141_lng <- mutate(quad141_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad141_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad141_dom_tab 
quad_lng     <- quad141_lng

## QUAD 142
quad142_dom <- subset(Dominant, quad_no == "142")
quad142_dom_mat <- as.matrix(quad142_dom[,-1])
quad142_dom_tab <- table(quad142_dom_mat[,-1]) # gives count of each species in the quadrat
quad142_dom_tab
quad142_lng <- quad142_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad142_lng)
quad142_lng <- mutate(quad142_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad142_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad142_dom_tab 
quad_lng     <- quad142_lng

## QUAD 143
quad143_dom <- subset(Dominant, quad_no == "143")
quad143_dom_mat <- as.matrix(quad143_dom[,-1])
quad143_dom_tab <- table(quad143_dom_mat[,-1]) # gives count of each species in the quadrat
quad143_dom_tab
quad143_lng <- quad143_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad143_lng)
quad143_lng <- mutate(quad143_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad143_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad143_dom_tab 
quad_lng     <- quad143_lng

## QUAD 144
quad144_dom <- subset(Dominant, quad_no == "144")
quad144_dom_mat <- as.matrix(quad144_dom[,-1])
quad144_dom_tab <- table(quad144_dom_mat[,-1]) # gives count of each species in the quadrat
quad144_dom_tab
quad144_lng <- quad144_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad144_lng)
quad144_lng <- mutate(quad144_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad144_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad144_dom_tab 
quad_lng     <- quad144_lng

## QUAD 145
quad145_dom <- subset(Dominant, quad_no == "145")
quad145_dom_mat <- as.matrix(quad145_dom[,-1])
quad145_dom_tab <- table(quad145_dom_mat[,-1]) # gives count of each species in the quadrat
quad145_dom_tab
quad145_lng <- quad145_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad145_lng)
quad145_lng <- mutate(quad145_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad145_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad145_dom_tab 
quad_lng     <- quad145_lng

## QUAD 146
quad146_dom <- subset(Dominant, quad_no == "146")
quad146_dom_mat <- as.matrix(quad146_dom[,-1])
quad146_dom_tab <- table(quad146_dom_mat[,-1]) # gives count of each species in the quadrat
quad146_dom_tab
quad146_lng <- quad146_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad146_lng)
quad146_lng <- mutate(quad146_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad146_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad146_dom_tab 
quad_lng     <- quad146_lng

## QUAD 147
quad147_dom <- subset(Dominant, quad_no == "147")
quad147_dom_mat <- as.matrix(quad147_dom[,-1])
quad147_dom_tab <- table(quad147_dom_mat[,-1]) # gives count of each species in the quadrat
quad147_dom_tab
quad147_lng <- quad147_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad147_lng)
quad147_lng <- mutate(quad147_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad147_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad147_dom_tab <- quad147_dom_tab 
quad_lng     <- quad147_lng

## QUAD 148
quad148_dom <- subset(Dominant, quad_no == "148")
quad148_dom_mat <- as.matrix(quad148_dom[,-1])
quad148_dom_tab <- table(quad148_dom_mat[,-1]) # gives count of each species in the quadrat
quad148_dom_tab
quad148_lng <- quad148_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad148_lng)
quad148_lng <- mutate(quad148_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad148_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad148_dom_tab 
quad_lng     <- quad148_lng

## QUAD 149
quad149_dom <- subset(Dominant, quad_no == "149")
quad149_dom_mat <- as.matrix(quad149_dom[,-1])
quad149_dom_tab <- table(quad149_dom_mat[,-1]) # gives count of each species in the quadrat
quad149_dom_tab
quad149_lng <- quad149_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad149_lng)
quad149_lng <- mutate(quad149_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad149_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad149_dom_tab 
quad_lng     <- quad149_lng

## QUAD 150
quad150_dom <- subset(Dominant, quad_no == "150")
quad150_dom_mat <- as.matrix(quad150_dom[,-1])
quad150_dom_tab <- table(quad150_dom_mat[,-1]) # gives count of each species in the quadrat
quad150_dom_tab
quad150_lng <- quad150_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad150_lng)
quad150_lng <- mutate(quad150_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad150_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad150_dom_tab 
quad_lng     <- quad150_lng

## QUAD 151
quad151_dom <- subset(Dominant, quad_no == "151")
quad151_dom_mat <- as.matrix(quad151_dom[,-1])
quad151_dom_tab <- table(quad151_dom_mat[,-1]) # gives count of each species in the quadrat
quad151_dom_tab
quad151_lng <- quad151_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad151_lng)
quad151_lng <- mutate(quad151_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad151_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad151_dom_tab 
quad_lng     <- quad151_lng

## QUAD 152
quad152_dom <- subset(Dominant, quad_no == "152")
quad152_dom_mat <- as.matrix(quad152_dom[,-1])
quad152_dom_tab <- table(quad152_dom_mat[,-1]) # gives count of each species in the quadrat
quad152_dom_tab
quad152_lng <- quad152_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad152_lng)
quad152_lng <- mutate(quad152_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad152_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad152_dom_tab 
quad_lng     <- quad152_lng

## QUAD 153
quad153_dom <- subset(Dominant, quad_no == "153")
quad153_dom_mat <- as.matrix(quad153_dom[,-1])
quad153_dom_tab <- table(quad153_dom_mat[,-1]) # gives count of each species in the quadrat
quad153_dom_tab
quad153_lng <- quad153_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad153_lng)
quad153_lng <- mutate(quad153_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad153_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad153_dom_tab 
quad_lng     <- quad153_lng

## QUAD 154
quad154_dom <- subset(Dominant, quad_no == "154")
quad154_dom_mat <- as.matrix(quad154_dom[,-1])
quad154_dom_tab <- table(quad154_dom_mat[,-1]) # gives count of each species in the quadrat
quad154_dom_tab
quad154_lng <- quad154_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad154_lng)
quad154_lng <- mutate(quad154_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad154_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad154_dom_tab 
quad_lng     <- quad154_lng

## QUAD 155
quad155_dom <- subset(Dominant, quad_no == "155")
quad155_dom_mat <- as.matrix(quad155_dom[,-1])
quad155_dom_tab <- table(quad155_dom_mat[,-1]) # gives count of each species in the quadrat
quad155_dom_tab
quad155_lng <- quad155_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad155_lng)
quad155_lng <- mutate(quad155_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad155_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad155_dom_tab 
quad_lng     <- quad155_lng

## QUAD 156
quad156_dom <- subset(Dominant, quad_no == "156")
quad156_dom_mat <- as.matrix(quad156_dom[,-1])
quad156_dom_tab <- table(quad156_dom_mat[,-1]) # gives count of each species in the quadrat
quad156_dom_tab
quad156_lng <- quad156_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad156_lng)
quad156_lng <- mutate(quad156_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad156_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad156_dom_tab 
quad_lng     <- quad156_lng

## QUAD 157
quad157_dom <- subset(Dominant, quad_no == "157")
quad157_dom_mat <- as.matrix(quad157_dom[,-1])
quad157_dom_tab <- table(quad157_dom_mat[,-1]) # gives count of each species in the quadrat
quad157_dom_tab
quad157_lng <- quad157_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad157_lng)
quad157_lng <- mutate(quad157_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad157_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad157_dom_tab 
quad_lng     <- quad157_lng

## QUAD 158
quad158_dom <- subset(Dominant, quad_no == "158")
quad158_dom_mat <- as.matrix(quad158_dom[,-1])
quad158_dom_tab <- table(quad158_dom_mat[,-1]) # gives count of each species in the quadrat
quad158_dom_tab
quad158_lng <- quad158_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad158_lng)
quad158_lng <- mutate(quad158_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad158_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad158_dom_tab 
quad_lng     <- quad158_lng

## QUAD 159
quad159_dom <- subset(Dominant, quad_no == "159")
quad159_dom_mat <- as.matrix(quad159_dom[,-1])
quad159_dom_tab <- table(quad159_dom_mat[,-1]) # gives count of each species in the quadrat
quad159_dom_tab
quad159_lng <- quad159_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad159_lng)
quad159_lng <- mutate(quad159_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad159_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad159_dom_tab 
quad_lng     <- quad159_lng

## QUAD 160
quad160_dom <- subset(Dominant, quad_no == "160")
quad160_dom_mat <- as.matrix(quad160_dom[,-1])
quad160_dom_tab <- table(quad160_dom_mat[,-1]) # gives count of each species in the quadrat
quad160_dom_tab
quad160_lng <- quad160_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad160_lng)
quad160_lng <- mutate(quad160_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad160_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad160_dom_tab 
quad_lng     <- quad160_lng

## QUAD 161
quad161_dom <- subset(Dominant, quad_no == "1")
quad161_dom_mat <- as.matrix(quad161_dom[,-1])
quad161_dom_tab <- table(quad161_dom_mat[,-1]) # gives count of each species in the quadrat
quad161_dom_tab
quad161_lng <- quad161_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad161_lng)
quad161_lng <- mutate(quad161_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad161_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad161_dom_tab 
quad_lng     <- quad161_lng

## QUAD 162
quad162_dom <- subset(Dominant, quad_no == "162")
quad162_dom_mat <- as.matrix(quad162_dom[,-1])
quad162_dom_tab <- table(quad162_dom_mat[,-1]) # gives count of each species in the quadrat
quad162_dom_tab
quad162_lng <- quad162_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad162_lng)
quad162_lng <- mutate(quad162_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad162_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad162_dom_tab 
quad_lng     <- quad162_lng

## QUAD 163
quad163_dom <- subset(Dominant, quad_no == "163")
quad163_dom_mat <- as.matrix(quad163_dom[,-1])
quad163_dom_tab <- table(quad163_dom_mat[,-1]) # gives count of each species in the quadrat
quad163_dom_tab
quad163_lng <- quad163_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad163_lng)
quad163_lng <- mutate(quad163_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad163_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad163_dom_tab 
quad_lng     <- quad163_lng

## QUAD 164
quad164_dom <- subset(Dominant, quad_no == "164")
quad164_dom_mat <- as.matrix(quad164_dom[,-1])
quad164_dom_tab <- table(quad164_dom_mat[,-1]) # gives count of each species in the quadrat
quad164_dom_tab
quad164_lng <- quad164_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad164_lng)
quad164_lng <- mutate(quad164_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad164_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad164_dom_tab 
quad_lng     <- quad164_lng

## QUAD 165
quad165_dom <- subset(Dominant, quad_no == "165")
quad165_dom_mat <- as.matrix(quad165_dom[,-1])
quad165_dom_tab <- table(quad165_dom_mat[,-1]) # gives count of each species in the quadrat
quad165_dom_tab
quad165_lng <- quad165_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad165_lng)
quad165_lng <- mutate(quad165_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad165_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad165_dom_tab 
quad_lng     <- quad165_lng

## QUAD 166
quad166_dom <- subset(Dominant, quad_no == "166")
quad166_dom_mat <- as.matrix(quad166_dom[,-1])
quad166_dom_tab <- table(quad166_dom_mat[,-1]) # gives count of each species in the quadrat
quad166_dom_tab
quad166_lng <- quad166_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad166_lng)
quad166_lng <- mutate(quad166_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad166_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad166_dom_tab 
quad_lng     <- quad166_lng

## QUAD 167
quad167_dom <- subset(Dominant, quad_no == "167")
quad167_dom_mat <- as.matrix(quad167_dom[,-1])
quad167_dom_tab <- table(quad167_dom_mat[,-1]) # gives count of each species in the quadrat
quad167_dom_tab
quad167_lng <- quad167_dom %>% gather(`10`:`100`, -quad_no, key="Col", value="Species") # drops variables since they are not identical acrioss mewasures/This is always used before filtering by species name
summary(quad167_lng)
quad167_lng <- mutate(quad167_lng, Row = as.factor(Row), Col = as.factor(Col)) # This is always used before filtering by species name
levels(quad167_lng$Col) <- c("10", "20", "30", "40", "50", # This is always used before filtering by species name
                          "60", "70", "80", "90", "100") # This is always used before filtering by species name

quad_dom_tab <- quad167_dom_tab 
quad_lng     <- quad167_lng

################################################################################### 
##                       RUN BEFORE LOOP, DEPENDING ON QUADRART NUMBER           ##
###################################################################################

quad_dom_tab <- quad22_dom_tab ## number to be replaced by quadrat number required before running loop 
quad_lng     <- quad22_lng

################################################################################### 
##                         NEEDED - BEGINNING OF LOOP                            ##
###################################################################################

## CURRENT LOOP READS ALL OF THE FIRST 5 QUADRATS TOGETHER AND SUMMARISES PATCHES ACROSS ALL 5 QUADRATS RATHER THAN
## EACH QUADRAT ON ITS OWN

#for (quad_no in 1:167) {
quad_dom_tab <- quad1_dom_tab ## change according to quadrat number needed for patch stats
quad_lng     <- quad1_lng

spp_in_quad  <- names(quad_dom_tab)
spat_all     <- NULL
for (spp_name in seq_along(spp_in_quad)){
  quad_no <- 138
  spp_text <- names(quad_dom_tab)[spp_name]
  quad_spp_lng <- quad_lng %>% filter(Species == spp_text)
  quad_spp_wde <- quad_spp_lng %>% reshape2::dcast(Row ~ Col, drop = FALSE, fill = 0)
  # Recode text to number
  quad_spp_wde[,-1] <- ifelse(quad_spp_wde[,-1]!="0",1,0)
  quad_spp_mat <- as.matrix(quad_spp_wde[,-1])
  
  quad_ccl = ConnCompLabel(quad_spp_mat)
  print(spp_text)
  print(quad_ccl)
  image(t(quad_ccl[12:1,]), col=c('white',rainbow(length(unique(quad_ccl))-1)), main = print(spp_text)) #due to spaces matrix is 12 (y-axis)x10 (x-axis)
  grid(nx=10, ny=12, col = "grey", lty = 1) # added post roy
  print(PatchStat(quad_ccl))
  
  # Store the spatial stats
  # Need to duplicate spp text names in final output table
  spp_duplicate <- rep(spp_text, max(quad_ccl)+1)
  patch_stats <- PatchStat(quad_ccl)
  patch_stats <- cbind(quad_no, spp_duplicate, patch_stats)
  spat_all <- rbind(spat_all, patch_stats)
  readline()
  }
write.csv(spat_all, file = "Results/Patch_Stats/Individual_quadrats/Q138_DOMINANT.csv")

# cbind in the quadrat no. into spat_all
#}

################################################################################### 
##  PATCH STATS: NUMBER OF PATCHES for chosen species per quadrats               ##
###################################################################################

patch_stats_dom <- read.csv("Results/Patch_Stats/Individual_quadrats/31-07-18_DOMINANT.csv")
patch_stats_dom <- patch_stats_dom[,-1]

################################################################################### 
##                                  BLOCK FORMERS                                ##
###################################################################################

## CALLUNA VULGARIS
callvulg_dom <- subset(patch_stats_dom, spp_duplicate == "Callvulg")
callvulg_dom <- subset(callvulg_dom, patchID > 0)

patchno_callvulg_dom<- aggregate(callvulg_dom$patchID, by = list(quad_no = callvulg_dom$quad_no), 
                                 FUN = max) # number of patches per species
write.csv(patchno_callvulg_dom, file = "Results/Patch_Stats/patchno/callvulg.csv")

## NARDUS STRICTA
nardstri_dom <- subset(patch_stats_dom, spp_duplicate == "Nardstri")
nardstri_dom <- subset(nardstri_dom, patchID > 0)

patchno_nardstri_dom<- aggregate(nardstri_dom$patchID, by = list(quad_no = nardstri_dom$quad_no), 
                                 FUN = max) # number of patches per species
write.csv(patchno_nardstri_dom, file = "Results/Patch_Stats/patchno/nardstri.csv")

## MOLINIA CAERULEA
molicaer_dom <- subset(patch_stats_dom, spp_duplicate == "Molicaer")
molicaer_dom <- subset(molicaer_dom, patchID > 0)

patchno_molicaer_dom<- aggregate(molicaer_dom$patchID, by = list(quad_no = molicaer_dom$quad_no), 
                                 FUN = max) # number of patches per species
write.csv(patchno_molicaer_dom, file = "Results/Patch_Stats/patchno/molicaer.csv")

## ERIOPHORUM VAGINATUM
eriovagi_dom <- subset(patch_stats_dom, spp_duplicate == "Eriovagi")
eriovagi_dom <- subset(eriovagi_dom, patchID > 0)

patchno_eriovagi_dom<- aggregate(eriovagi_dom$patchID, by = list(quad_no = eriovagi_dom$quad_no), 
                                 FUN = max) # number of patches per species
write.csv(patchno_eriovagi_dom, file = "Results/Patch_Stats/patchno/eriovagi.csv")

## JUNCUS SQUARROSUS
juncsqua_dom <- subset(patch_stats_dom, spp_duplicate == "Juncsqua")
juncsqua_dom <- subset(juncsqua_dom, patchID > 0)

patchno_juncsqua_dom<- aggregate(juncsqua_dom$patchID, by = list(quad_no = juncsqua_dom$quad_no), 
                                 FUN = max) # number of patches per species
write.csv(patchno_juncsqua_dom, file = "Results/Patch_Stats/patchno/juncsqua.csv")

## JUNCUS EFFUSUS
junceffu_dom <- subset(patch_stats_dom, spp_duplicate == "Junceffu")
junceffu_dom <- subset(junceffu_dom, patchID > 0)

patchno_junceffu_dom<- aggregate(junceffu_dom$patchID, by = list(quad_no = junceffu_dom$quad_no), 
                                 FUN = max) # number of patches per species
write.csv(patchno_junceffu_dom, file = "Results/Patch_Stats/patchno/junceffu.csv")

################################################################################### 
##                                  OPPORTUNISTS                                 ##
###################################################################################
## CAREX NIGRA
carenigr_dom <- subset(patch_stats_dom, spp_duplicate == "Carenigr")
carenigr_dom <- subset(carenigr_dom, patchID > 0)

patchno_carenigr_dom<- aggregate(carenigr_dom$patchID, by = list(quad_no = carenigr_dom$quad_no), 
                                 FUN = max) # number of patches per species
write.csv(patchno_carenigr_dom, file = "Results/Patch_Stats/patchno/carenigr.csv")

## GALIUM SAXATILE
galisaxa_dom <- subset(patch_stats_dom, spp_duplicate == "Galisaxa")
galisaxa_dom <- subset(galisaxa_dom, patchID > 0)

patchno_galisaxa_dom<- aggregate(galisaxa_dom$patchID, by = list(quad_no = galisaxa_dom$quad_no), 
                                 FUN = max) # number of patches per species
write.csv(patchno_galisaxa_dom, file = "Results/Patch_Stats/patchno/galisaxa.csv")

## POTENTILLA ERECTA
poteerec_dom <- subset(patch_stats_dom, spp_duplicate == "Poteerec")
poteerec_dom <- subset(poteerec_dom, patchID > 0)

patchno_poteerec_dom<- aggregate(poteerec_dom$patchID, by = list(quad_no = poteerec_dom$quad_no), 
                                 FUN = max) # number of patches per species
write.csv(patchno_poteerec_dom, file = "Results/Patch_Stats/patchno/poteerec.csv")

## DESCHAMPSIA FLEXUOSA
descflex_dom <- subset(patch_stats_dom, spp_duplicate == "Descflex")
descflex_dom <- subset(descflex_dom, patchID > 0)

patchno_descflex_dom<- aggregate(descflex_dom$patchID, by = list(quad_no = descflex_dom$quad_no), 
                                 FUN = max) # number of patches per species
write.csv(patchno_descflex_dom, file = "Results/Patch_Stats/patchno/descflex.csv")

## VACCINIUM MYRTILLUS
vaccmyrt_dom <- subset(patch_stats_dom, spp_duplicate == "Vaccmyrt")
vaccmyrt_dom <- subset(vaccmyrt_dom, patchID > 0)

patchno_vaccmyrt_dom<- aggregate(vaccmyrt_dom$patchID, by = list(quad_no = vaccmyrt_dom$quad_no), 
                                 FUN = max) # number of patches per species
write.csv(patchno_vaccmyrt_dom, file = "Results/Patch_Stats/patchno/vaccmyrt.csv")


################################################################################### 
##  PATCH STATS: AREA OF PATCHES for chosen species per quadrats                 ##
###################################################################################
################################################################################### 
##                                  BLOCK FORMERS                                ##
###################################################################################

## CALLUNA VULGARIS
callvulg_dom <- subset(patch_stats_dom, spp_duplicate == "Callvulg")
callvulg_dom <- subset(callvulg_dom, patchID > 0)

area_callvulg_dom<- aggregate(callvulg_dom$area, by = list(quad_no = callvulg_dom$quad_no), 
                              FUN = sum) # number of patches per species
write.csv(area_callvulg_dom, file = "Results/Patch_Stats/Area/callvulg.csv")

## NARDUS STRICTA
nardstri_dom <- subset(patch_stats_dom, spp_duplicate == "Nardstri")
nardstri_dom <- subset(nardstri_dom, patchID > 0)

area_nardstri_dom<- aggregate(nardstri_dom$area, by = list(quad_no = nardstri_dom$quad_no), 
                              FUN = sum) # number of patches per species
write.csv(area_nardstri_dom, file = "Results/Patch_Stats/Area/nardstri.csv")

## MOLINIA CAERULEA
molicaer_dom <- subset(patch_stats_dom, spp_duplicate == "Molicaer")
molicaer_dom <- subset(molicaer_dom, patchID > 0)

area_molicaer_dom<- aggregate(molicaer_dom$area, by = list(quad_no = molicaer_dom$quad_no), 
                              FUN = sum) # number of patches per species
write.csv(area_molicaer_dom, file = "Results/Patch_Stats/Area/molicaer.csv")

## ERIOPHORUM VAGINATUM
eriovagi_dom <- subset(patch_stats_dom, spp_duplicate == "Eriovagi")
eriovagi_dom <- subset(eriovagi_dom, patchID > 0)

area_eriovagi_dom<- aggregate(eriovagi_dom$area, by = list(quad_no = eriovagi_dom$quad_no), 
                              FUN = sum) # number of patches per species
write.csv(area_eriovagi_dom, file = "Results/Patch_Stats/Area/eriovagi.csv")

## JUNCUS SQUARROSUS
juncsqua_dom <- subset(patch_stats_dom, spp_duplicate == "Juncsqua")
juncsqua_dom <- subset(juncsqua_dom, patchID > 0)

area_juncsqua_dom<- aggregate(juncsqua_dom$area, by = list(quad_no = juncsqua_dom$quad_no), 
                              FUN = sum) # number of patches per species
write.csv(area_juncsqua_dom, file = "Results/Patch_Stats/Area/juncsqua.csv")

## JUNCUS EFFUSUS
junceffu_dom <- subset(patch_stats_dom, spp_duplicate == "Junceffu")
junceffu_dom <- subset(junceffu_dom, patchID > 0)

area_junceffu_dom<- aggregate(junceffu_dom$area, by = list(quad_no = junceffu_dom$quad_no), 
                              FUN = sum) # number of patches per species
write.csv(area_junceffu_dom, file = "Results/Patch_Stats/Area/junceffu.csv")

################################################################################### 
##                                  OPPORTUNISTS                                 ##
###################################################################################
## CAREX NIGRA
carenigr_dom <- subset(patch_stats_dom, spp_duplicate == "Carenigr")
carenigr_dom <- subset(carenigr_dom, patchID > 0)

area_carenigr_dom<- aggregate(carenigr_dom$area, by = list(quad_no = carenigr_dom$quad_no), 
                              FUN = sum) # number of patches per species
write.csv(area_carenigr_dom, file = "Results/Patch_Stats/Area/carenigr.csv")

## GALIUM SAXATILE
galisaxa_dom <- subset(patch_stats_dom, spp_duplicate == "Galisaxa")
galisaxa_dom <- subset(galisaxa_dom, patchID > 0)

area_galisaxa_dom<- aggregate(galisaxa_dom$area, by = list(quad_no = galisaxa_dom$quad_no), 
                              FUN = sum) # number of patches per species
write.csv(area_galisaxa_dom, file = "Results/Patch_Stats/Area/galisaxa.csv")

## POTENTILLA ERECTA
poteerec_dom <- subset(patch_stats_dom, spp_duplicate == "Poteerec")
poteerec_dom <- subset(poteerec_dom, patchID > 0)

area_poteerec_dom<- aggregate(poteerec_dom$area, by = list(quad_no = poteerec_dom$quad_no), 
                              FUN = sum) # number of patches per species
write.csv(area_poteerec_dom, file = "Results/Patch_Stats/Area/poteerec.csv")

## DESCHAMPSIA FLEXUOSA
descflex_dom <- subset(patch_stats_dom, spp_duplicate == "Descflex")
descflex_dom <- subset(descflex_dom, patchID > 0)

area_descflex_dom<- aggregate(descflex_dom$area, by = list(quad_no = descflex_dom$quad_no), 
                              FUN = sum) # number of patches per species
write.csv(area_descflex_dom, file = "Results/Patch_Stats/Area/descflex.csv")

## VACCINIUM MYRTILLUS
vaccmyrt_dom <- subset(patch_stats_dom, spp_duplicate == "Vaccmyrt")
vaccmyrt_dom <- subset(vaccmyrt_dom, patchID > 0)

area_vaccmyrt_dom<- aggregate(vaccmyrt_dom$area, by = list(quad_no = vaccmyrt_dom$quad_no), 
                              FUN = sum) # number of patches per species
write.csv(area_vaccmyrt_dom, file = "Results/Patch_Stats/Area/vaccmyrt.csv")

################################################################################### 
##  PATCH STATS: PERIMETER OF PATCHES for chosen species per quadrats                 ##
###################################################################################
################################################################################### 
##                                  BLOCK FORMERS                                ##
###################################################################################

## CALLUNA VULGARIS
callvulg_dom <- subset(patch_stats_dom, spp_duplicate == "Callvulg")
callvulg_dom <- subset(callvulg_dom, patchID > 0)

perim_callvulg_dom<- aggregate(callvulg_dom$perimeter, by = list(quad_no = callvulg_dom$quad_no), 
                              FUN = sum) # number of patches per species
write.csv(perim_callvulg_dom, file = "Results/Patch_Stats/Perimeter/callvulg.csv")

## NARDUS STRICTA
nardstri_dom <- subset(patch_stats_dom, spp_duplicate == "Nardstri")
nardstri_dom <- subset(nardstri_dom, patchID > 0)

perim_nardstri_dom<- aggregate(nardstri_dom$perimeter, by = list(quad_no = nardstri_dom$quad_no), 
                              FUN = sum) # number of patches per species
write.csv(perim_nardstri_dom, file = "Results/Patch_Stats/Perimeter/nardstri.csv")

## MOLINIA CAERULEA
molicaer_dom <- subset(patch_stats_dom, spp_duplicate == "Molicaer")
molicaer_dom <- subset(molicaer_dom, patchID > 0)

perim_molicaer_dom<- aggregate(molicaer_dom$perimeter, by = list(quad_no = molicaer_dom$quad_no), 
                              FUN = sum) # number of patches per species
write.csv(perim_molicaer_dom, file = "Results/Patch_Stats/Perimeter/molicaer.csv")

## ERIOPHORUM VAGINATUM
eriovagi_dom <- subset(patch_stats_dom, spp_duplicate == "Eriovagi")
eriovagi_dom <- subset(eriovagi_dom, patchID > 0)

perim_eriovagi_dom<- aggregate(eriovagi_dom$perimeter, by = list(quad_no = eriovagi_dom$quad_no), 
                              FUN = sum) # number of patches per species
write.csv(perim_eriovagi_dom, file = "Results/Patch_Stats/Perimeter/eriovagi.csv")

## JUNCUS SQUARROSUS
juncsqua_dom <- subset(patch_stats_dom, spp_duplicate == "Juncsqua")
juncsqua_dom <- subset(juncsqua_dom, patchID > 0)

perim_juncsqua_dom<- aggregate(juncsqua_dom$perimeter, by = list(quad_no = juncsqua_dom$quad_no), 
                              FUN = sum) # number of patches per species
write.csv(perim_juncsqua_dom, file = "Results/Patch_Stats/Perimeter/juncsqua.csv")

## JUNCUS EFFUSUS
junceffu_dom <- subset(patch_stats_dom, spp_duplicate == "Junceffu")
junceffu_dom <- subset(junceffu_dom, patchID > 0)

perim_junceffu_dom<- aggregate(junceffu_dom$perimeter, by = list(quad_no = junceffu_dom$quad_no), 
                              FUN = sum) # number of patches per species
write.csv(perim_junceffu_dom, file = "Results/Patch_Stats/Perimeter/junceffu.csv")

################################################################################### 
##                                  OPPORTUNISTS                                 ##
###################################################################################
## CAREX NIGRA
carenigr_dom <- subset(patch_stats_dom, spp_duplicate == "Carenigr")
carenigr_dom <- subset(carenigr_dom, patchID > 0)

perim_carenigr_dom<- aggregate(carenigr_dom$perimeter, by = list(quad_no = carenigr_dom$quad_no), 
                              FUN = sum) # number of patches per species
write.csv(perim_carenigr_dom, file = "Results/Patch_Stats/Perimeter/carenigr.csv")

## GALIUM SAXATILE
galisaxa_dom <- subset(patch_stats_dom, spp_duplicate == "Galisaxa")
galisaxa_dom <- subset(galisaxa_dom, patchID > 0)

perim_galisaxa_dom<- aggregate(galisaxa_dom$perimeter, by = list(quad_no = galisaxa_dom$quad_no), 
                              FUN = sum) # number of patches per species
write.csv(perim_galisaxa_dom, file = "Results/Patch_Stats/Perimeter/galisaxa.csv")

## POTENTILLA ERECTA
poteerec_dom <- subset(patch_stats_dom, spp_duplicate == "Poteerec")
poteerec_dom <- subset(poteerec_dom, patchID > 0)

perim_poteerec_dom<- aggregate(poteerec_dom$perimeter, by = list(quad_no = poteerec_dom$quad_no), 
                              FUN = sum) # number of patches per species
write.csv(perim_poteerec_dom, file = "Results/Patch_Stats/Perimeter/poteerec.csv")

## DESCHAMPSIA FLEXUOSA
descflex_dom <- subset(patch_stats_dom, spp_duplicate == "Descflex")
descflex_dom <- subset(descflex_dom, patchID > 0)

perim_descflex_dom<- aggregate(descflex_dom$perimeter, by = list(quad_no = descflex_dom$quad_no), 
                              FUN = sum) # number of patches per species
write.csv(perim_descflex_dom, file = "Results/Patch_Stats/Perimeter/descflex.csv")

## VACCINIUM MYRTILLUS
vaccmyrt_dom <- subset(patch_stats_dom, spp_duplicate == "Vaccmyrt")
vaccmyrt_dom <- subset(vaccmyrt_dom, patchID > 0)

perim_vaccmyrt_dom<- aggregate(vaccmyrt_dom$perimeter, by = list(quad_no = vaccmyrt_dom$quad_no), 
                              FUN = sum) # number of patches per species
write.csv(perim_vaccmyrt_dom, file = "Results/Patch_Stats/Perimeter/vaccmyrt.csv")

################################################################################### 
##  PATCH STATS: PERIM:AREA OF PATCHES for chosen species per quadrats                 ##
###################################################################################
################################################################################### 
##                                  BLOCK FORMERS                                ##
###################################################################################

## CALLUNA VULGARIS
callvulg_dom <- subset(patch_stats_dom, spp_duplicate == "Callvulg")
callvulg_dom <- subset(callvulg_dom, patchID > 0)

perim.area_callvulg_dom<- aggregate(callvulg_dom$perim.area.ratio, by = list(quad_no = callvulg_dom$quad_no), 
                              FUN = sum) # number of patches per species
write.csv(perim.area_callvulg_dom, file = "Results/Patch_Stats/Per-Area-Ratio/callvulg.csv")

## NARDUS STRICTA
nardstri_dom <- subset(patch_stats_dom, spp_duplicate == "Nardstri")
nardstri_dom <- subset(nardstri_dom, patchID > 0)

perim.area_nardstri_dom<- aggregate(nardstri_dom$perim.area.ratio, by = list(quad_no = nardstri_dom$quad_no), 
                              FUN = sum) # number of patches per species
write.csv(perim.area_nardstri_dom, file = "Results/Patch_Stats/Per-Area-Ratio/nardstri.csv")

## MOLINIA CAERULEA
molicaer_dom <- subset(patch_stats_dom, spp_duplicate == "Molicaer")
molicaer_dom <- subset(molicaer_dom, patchID > 0)

perim.area_molicaer_dom<- aggregate(molicaer_dom$perim.area.ratio, by = list(quad_no = molicaer_dom$quad_no), 
                              FUN = sum) # number of patches per species
write.csv(perim.area_molicaer_dom, file = "Results/Patch_Stats/Per-Area-Ratio/molicaer.csv")

## ERIOPHORUM VAGINATUM
eriovagi_dom <- subset(patch_stats_dom, spp_duplicate == "Eriovagi")
eriovagi_dom <- subset(eriovagi_dom, patchID > 0)

perim.area_eriovagi_dom<- aggregate(eriovagi_dom$perim.area.ratio, by = list(quad_no = eriovagi_dom$quad_no), 
                              FUN = sum) # number of patches per species
write.csv(perim.area_eriovagi_dom, file = "Results/Patch_Stats/Per-Area-Ratio/eriovagi.csv")

## JUNCUS SQUARROSUS
juncsqua_dom <- subset(patch_stats_dom, spp_duplicate == "Juncsqua")
juncsqua_dom <- subset(juncsqua_dom, patchID > 0)

perim.area_juncsqua_dom<- aggregate(juncsqua_dom$perim.area.ratio, by = list(quad_no = juncsqua_dom$quad_no), 
                              FUN = sum) # number of patches per species
write.csv(perim.area_juncsqua_dom, file = "Results/Patch_Stats/Per-Area-Ratio/juncsqua.csv")

## JUNCUS EFFUSUS
junceffu_dom <- subset(patch_stats_dom, spp_duplicate == "Junceffu")
junceffu_dom <- subset(junceffu_dom, patchID > 0)

perim.area_junceffu_dom<- aggregate(junceffu_dom$perim.area.ratio, by = list(quad_no = junceffu_dom$quad_no), 
                              FUN = sum) # number of patches per species
write.csv(perim.area_junceffu_dom, file = "Results/Patch_Stats/Per-Area-Ratio/junceffu.csv")

################################################################################### 
##                                  OPPORTUNISTS                                 ##
###################################################################################
## CAREX NIGRA
carenigr_dom <- subset(patch_stats_dom, spp_duplicate == "Carenigr")
carenigr_dom <- subset(carenigr_dom, patchID > 0)

perim.area_carenigr_dom<- aggregate(carenigr_dom$perim.area.ratio, by = list(quad_no = carenigr_dom$quad_no), 
                              FUN = sum) # number of patches per species
write.csv(perim.area_carenigr_dom, file = "Results/Patch_Stats/Per-Area-Ratio/carenigr.csv")

## GALIUM SAXATILE
galisaxa_dom <- subset(patch_stats_dom, spp_duplicate == "Galisaxa")
galisaxa_dom <- subset(galisaxa_dom, patchID > 0)

perim.area_galisaxa_dom<- aggregate(galisaxa_dom$perim.area.ratio, by = list(quad_no = galisaxa_dom$quad_no), 
                              FUN = sum) # number of patches per species
write.csv(perim.area_galisaxa_dom, file = "Results/Patch_Stats/Per-Area-Ratio/galisaxa.csv")

## POTENTILLA ERECTA
poteerec_dom <- subset(patch_stats_dom, spp_duplicate == "Poteerec")
poteerec_dom <- subset(poteerec_dom, patchID > 0)

perim.area_poteerec_dom<- aggregate(poteerec_dom$perim.area.ratio, by = list(quad_no = poteerec_dom$quad_no), 
                              FUN = sum) # number of patches per species
write.csv(perim.area_poteerec_dom, file = "Results/Patch_Stats/Per-Area-Ratio/poteerec.csv")

## DESCHAMPSIA FLEXUOSA
descflex_dom <- subset(patch_stats_dom, spp_duplicate == "Descflex")
descflex_dom <- subset(descflex_dom, patchID > 0)

perim.area_descflex_dom<- aggregate(descflex_dom$perim.area.ratio, by = list(quad_no = descflex_dom$quad_no), 
                              FUN = sum) # number of patches per species
write.csv(perim.area_descflex_dom, file = "Results/Patch_Stats/Per-Area-Ratio/descflex.csv")

## VACCINIUM MYRTILLUS
vaccmyrt_dom <- subset(patch_stats_dom, spp_duplicate == "Vaccmyrt")
vaccmyrt_dom <- subset(vaccmyrt_dom, patchID > 0)

perim.area_vaccmyrt_dom<- aggregate(vaccmyrt_dom$perim.area.ratio, by = list(quad_no = vaccmyrt_dom$quad_no), 
                              FUN = sum) # number of patches per species
write.csv(perim.area_vaccmyrt_dom, file = "Results/Patch_Stats/Per-Area-Ratio/vaccmyrt.csv")

################################################################################### 
##  PATCH STATS: SHAPE INDEX for chosen species per quadrats                 ##
###################################################################################
################################################################################### 
##                                  BLOCK FORMERS                                ##
###################################################################################

## CALLUNA VULGARIS
callvulg_dom <- subset(patch_stats_dom, spp_duplicate == "Callvulg")
callvulg_dom <- subset(callvulg_dom, patchID > 0)

shape.ind_callvulg_dom<- aggregate(callvulg_dom$area, by = list(quad_no = callvulg_dom$quad_no), 
                              FUN = sum) # number of patches per species
write.csv(shape.ind_callvulg_dom, file = "Results/Patch_Stats/Shape_Index/callvulg.csv")

## NARDUS STRICTA
nardstri_dom <- subset(patch_stats_dom, spp_duplicate == "Nardstri")
nardstri_dom <- subset(nardstri_dom, patchID > 0)

shape.ind_nardstri_dom<- aggregate(nardstri_dom$area, by = list(quad_no = nardstri_dom$quad_no), 
                              FUN = sum) # number of patches per species
write.csv(shape.ind_nardstri_dom, file = "Results/Patch_Stats/Shape_Index/nardstri.csv")

## MOLINIA CAERULEA
molicaer_dom <- subset(patch_stats_dom, spp_duplicate == "Molicaer")
molicaer_dom <- subset(molicaer_dom, patchID > 0)

shape.ind_molicaer_dom<- aggregate(molicaer_dom$area, by = list(quad_no = molicaer_dom$quad_no), 
                              FUN = sum) # number of patches per species
write.csv(shape.ind_molicaer_dom, file = "Results/Patch_Stats/Shape_Index/molicaer.csv")

## ERIOPHORUM VAGINATUM
eriovagi_dom <- subset(patch_stats_dom, spp_duplicate == "Eriovagi")
eriovagi_dom <- subset(eriovagi_dom, patchID > 0)

shape.ind_eriovagi_dom<- aggregate(eriovagi_dom$area, by = list(quad_no = eriovagi_dom$quad_no), 
                              FUN = sum) # number of patches per species
write.csv(shape.ind_eriovagi_dom, file = "Results/Patch_Stats/Shape_Index/eriovagi.csv")

## JUNCUS SQUARROSUS
juncsqua_dom <- subset(patch_stats_dom, spp_duplicate == "Juncsqua")
juncsqua_dom <- subset(juncsqua_dom, patchID > 0)

shape.ind_juncsqua_dom<- aggregate(juncsqua_dom$area, by = list(quad_no = juncsqua_dom$quad_no), 
                              FUN = sum) # number of patches per species
write.csv(shape.ind_juncsqua_dom, file = "Results/Patch_Stats/Shape_Index/juncsqua.csv")

## JUNCUS EFFUSUS
junceffu_dom <- subset(patch_stats_dom, spp_duplicate == "Junceffu")
junceffu_dom <- subset(junceffu_dom, patchID > 0)

shape.ind_junceffu_dom<- aggregate(junceffu_dom$area, by = list(quad_no = junceffu_dom$quad_no), 
                              FUN = sum) # number of patches per species
write.csv(shape.ind_junceffu_dom, file = "Results/Patch_Stats/Shape_Index/junceffu.csv")

################################################################################### 
##                                  OPPORTUNISTS                                 ##
###################################################################################
## CAREX NIGRA
carenigr_dom <- subset(patch_stats_dom, spp_duplicate == "Carenigr")
carenigr_dom <- subset(carenigr_dom, patchID > 0)

shape.ind_carenigr_dom<- aggregate(carenigr_dom$area, by = list(quad_no = carenigr_dom$quad_no), 
                              FUN = sum) # number of patches per species
write.csv(shape.ind_carenigr_dom, file = "Results/Patch_Stats/Shape_Index/carenigr.csv")

## GALIUM SAXATILE
galisaxa_dom <- subset(patch_stats_dom, spp_duplicate == "Galisaxa")
galisaxa_dom <- subset(galisaxa_dom, patchID > 0)

shape.ind_galisaxa_dom<- aggregate(galisaxa_dom$area, by = list(quad_no = galisaxa_dom$quad_no), 
                              FUN = sum) # number of patches per species
write.csv(shape.ind_galisaxa_dom, file = "Results/Patch_Stats/Shape_Index/galisaxa.csv")

## POTENTILLA ERECTA
poteerec_dom <- subset(patch_stats_dom, spp_duplicate == "Poteerec")
poteerec_dom <- subset(poteerec_dom, patchID > 0)

shape.ind_poteerec_dom<- aggregate(poteerec_dom$area, by = list(quad_no = poteerec_dom$quad_no), 
                              FUN = sum) # number of patches per species
write.csv(shape.ind_poteerec_dom, file = "Results/Patch_Stats/Shape_Index/poteerec.csv")

## DESCHAMPSIA FLEXUOSA
descflex_dom <- subset(patch_stats_dom, spp_duplicate == "Descflex")
descflex_dom <- subset(descflex_dom, patchID > 0)

shape.ind_descflex_dom<- aggregate(descflex_dom$area, by = list(quad_no = descflex_dom$quad_no), 
                              FUN = sum) # number of patches per species
write.csv(shape.ind_descflex_dom, file = "Results/Patch_Stats/Shape_Index/descflex.csv")

## VACCINIUM MYRTILLUS
vaccmyrt_dom <- subset(patch_stats_dom, spp_duplicate == "Vaccmyrt")
vaccmyrt_dom <- subset(vaccmyrt_dom, patchID > 0)

shape.ind_vaccmyrt_dom<- aggregate(vaccmyrt_dom$area, by = list(quad_no = vaccmyrt_dom$quad_no), 
                              FUN = sum) # number of patches per species#
write.csv(shape.ind_vaccmyrt_dom, file = "Results/Patch_Stats/Shape_Index/vaccmyrt.csv")

################################################################################### 
##              READING SPECIES CSV WITH SEQUENCE QUADRAT NO                     ##
###################################################################################
##### PATCH NUMBER
## CALLUNA VULGARIS
patchno_callvulg_dom <- read.csv("Results/Patch_Stats/patchno/patchno/callvulg.csv")
patchno_callvulg_dom[is.na(patchno_callvulg_dom)] <- 0
patchno_callvulg_dom <- patchno_callvulg_dom[,-1]
barplot(patchno_callvulg_dom$x, names.arg = patchno_callvulg_dom$quad_no, cex.axis = 0.7,
        xlab = "Quadrat number", ylab = "Number of Patches", main = "No. of patches C. vulgaris")
hist_patchno_callvulg <- hist(patchno_callvulg_dom$x, xlab = "Number of patches")
xfit<-seq(min(patchno_callvulg_dom$x),max(patchno_callvulg_dom$x),length=8) 
yfit<-dnorm(xfit,mean=mean(patchno_callvulg_dom$x),sd=sd(patchno_callvulg_dom$x)) 
yfit <- yfit * diff(hist_patchno_callvulg$mids[1:2])*length(patchno_callvulg_dom$x)
plot(hist_patchno_callvulg, main = "Number of Patches C. vulgaris", xlab = "Number of Patches")
lines(xfit, yfit, col = "black", lwd = 2)

density_callvulg <- density(patchno_callvulg_dom$x)
plot(density_callvulg, main = "No. of Patches C. vulgaris", xlim = c(0, max(patchno_callvulg_dom$x)))
pdf("Results/Plots/Frequency-Distribution/Patch_no/Callvulg.pdf")
dev.off()


## NARDUS STRICTA
patchno_nardstri_dom <- read.csv("Results/Patch_Stats/patchno/patchno/nardstri.csv")
patchno_nardstri_dom[is.na(patchno_nardstri_dom)] <- 0
patchno_nardstri_dom <- patchno_nardstri_dom[,-1]
barplot(patchno_nardstri_dom$x, names.arg = patchno_nardstri_dom$quad_no, cex.axis = 0.7,
        xlab = "Quadrat number", ylab = "Number of Patches", main = "No. of patches N. stricta")
hist_patchno_nardstri <- hist(patchno_nardstri_dom$x, xlab = "Number of patches")
xfit<-seq(min(patchno_nardstri_dom$x),max(patchno_nardstri_dom$x),length=8) 
yfit<-dnorm(xfit,mean=mean(patchno_nardstri_dom$x),sd=sd(patchno_nardstri_dom$x)) 
yfit <- yfit * diff(hist_patchno_nardstri$mids[1:2])*length(patchno_nardstri_dom$x)
plot(hist_patchno_nardstri, main = "Number of Patches N. stricta", xlab = "Number of Patches")
lines(xfit, yfit, col = "black", lwd = 2)

density_nardstri <- density(patchno_nardstri_dom$x)
plot(density_nardstri, main = "No. of Patches N. stricta", xlim = c(0, max(patchno_nardstri_dom$x)))
pdf("Results/Plots/Frequency-Distribution/Patch_no/Nardstri.pdf")
dev.off()

### MOLINIA CAERULEA
patchno_molicaer_dom <- read.csv("Results/Patch_Stats/patchno/patchno/molicaer.csv")
patchno_molicaer_dom <- read.csv("Results/Patch_Stats/patchno/patchno/molicaer.csv")
patchno_molicaer_dom[is.na(patchno_molicaer_dom)] <- 0
patchno_molicaer_dom <- patchno_molicaer_dom[,-1]
barplot(patchno_molicaer_dom$x, names.arg = patchno_molicaer_dom$quad_no, cex.axis = 0.7,
        xlab = "Quadrat number", ylab = "Number of Patches", main = "No. of patches M. caerulea")
hist_patchno_molicaer <- hist(patchno_molicaer_dom$x, xlab = "Number of patches")
xfit<-seq(min(patchno_molicaer_dom$x),max(patchno_molicaer_dom$x),length=8) 
yfit<-dnorm(xfit,mean=mean(patchno_molicaer_dom$x),sd=sd(patchno_molicaer_dom$x)) 
yfit <- yfit * diff(hist_patchno_molicaer$mids[1:2])*length(patchno_molicaer_dom$x)
plot(hist_patchno_molicaer, main = "Number of Patches M. caerulea", xlab = "Number of Patches")
lines(xfit, yfit, col = "black", lwd = 2)

density_molicaer <- density(patchno_molicaer_dom$x)
plot(density_molicaer, main = "No. of Patches M. caerulea", xlim = c(0, max(patchno_molicaer_dom$x)))
pdf("Results/Plots/Frequency-Distribution/Patch_no/Molicaer.pdf")
dev.off()

## ERIOPHORUM VAGINATUM
patchno_eriovagi_dom <- read.csv("Results/Patch_Stats/patchno/patchno/eriovagi.csv")
patchno_eriovagi_dom[is.na(patchno_eriovagi_dom)] <- 0
patchno_eriovagi_dom <- patchno_eriovagi_dom[,-1]
barplot(patchno_eriovagi_dom$x, names.arg = patchno_eriovagi_dom$quad_no, cex.axis = 0.7,
        xlab = "Quadrat number", ylab = "Number of Patches", main = "No. of patches E. vaginatum")
hist_patchno_eriovagi <- hist(patchno_eriovagi_dom$x, xlab = "Number of patches")
xfit<-seq(min(patchno_eriovagi_dom$x),max(patchno_eriovagi_dom$x),length=8) 
yfit<-dnorm(xfit,mean=mean(patchno_eriovagi_dom$x),sd=sd(patchno_eriovagi_dom$x)) 
yfit <- yfit * diff(hist_patchno_eriovagi$mids[1:2])*length(patchno_eriovagi_dom$x)
plot(hist_patchno_eriovagi, main = "Number of Patches E. vaginatum", xlab = "Number of Patches")
lines(xfit, yfit, col = "black", lwd = 2)

density_eriovagi <- density(patchno_eriovagi_dom$x)
plot(density_eriovagi, main = "No. of Patches E. vaginatum", xlim = c(0, max(patchno_eriovagi_dom$x)))

pdf("Results/Plots/Frequency-Distribution/Patch_no/Eriovagi.pdf")
dev.off()

## JUNCUS SQUARROSUS
patchno_juncsqua_dom <- read.csv("Results/Patch_Stats/patchno/patchno/juncsqua.csv")
patchno_juncsqua_dom[is.na(patchno_juncsqua_dom)] <- 0
patchno_juncsqua_dom <- patchno_juncsqua_dom[,-1]
barplot(patchno_juncsqua_dom$x, names.arg = patchno_juncsqua_dom$quad_no, cex.axis = 0.7,
        xlab = "Quadrat number", ylab = "Number of Patches", main = "No. of patches J. squarrosus")
hist_patchno_juncsqua <- hist(patchno_juncsqua_dom$x, xlab = "Number of patches")
xfit<-seq(min(patchno_juncsqua_dom$x),max(patchno_juncsqua_dom$x),length=8) 
yfit<-dnorm(xfit,mean=mean(patchno_juncsqua_dom$x),sd=sd(patchno_juncsqua_dom$x)) 
yfit <- yfit * diff(hist_patchno_juncsqua$mids[1:2])*length(patchno_juncsqua_dom$x)
plot(hist_patchno_juncsqua, main = "Number of Patches J. squarrosus", xlab = "Number of Patches")
lines(xfit, yfit, col = "black", lwd = 2)

density_juncsqua <- density(patchno_juncsqua_dom$x)
plot(density_juncsqua, main = "No. of Patches J. squarrosus", xlim = c(0, max(patchno_juncsqua_dom$x)))

pdf("Results/Plots/Frequency-Distribution/Patch_no/Juncsqua.pdf")
dev.off()

## JUNCUS EFFUSUS
patchno_junceffu_dom <- read.csv("Results/Patch_Stats/patchno/patchno/junceffu.csv")
patchno_junceffu_dom[is.na(patchno_junceffu_dom)] <- 0
patchno_junceffu_dom <- patchno_junceffu_dom[,-1]
barplot(patchno_junceffu_dom$x, names.arg = patchno_junceffu_dom$quad_no, cex.axis = 0.7,
        xlab = "Quadrat number", ylab = "Number of Patches", main = "No. of patches J. effusus")
hist_patchno_junceffu <- hist(patchno_junceffu_dom$x, xlab = "Number of patches")
xfit<-seq(min(patchno_junceffu_dom$x),max(patchno_junceffu_dom$x),length=8) 
yfit<-dnorm(xfit,mean=mean(patchno_junceffu_dom$x),sd=sd(patchno_junceffu_dom$x)) 
yfit <- yfit * diff(hist_patchno_junceffu$mids[1:2])*length(patchno_junceffu_dom$x)
plot(hist_patchno_junceffu, main = "Number of Patches J. effusus", xlab = "Number of Patches")
lines(xfit, yfit, col = "black", lwd = 2)

density_junceffu <- density(patchno_junceffu_dom$x)
plot(density_junceffu, main = "No. of Patches J. effusus", xlim = c(0, max(patchno_junceffu_dom$x)))

pdf("Results/Plots/Frequency-Distribution/Patch_no/Junceffu.pdf")
dev.off()

## CAREX NIGRA
patchno_carenigr_dom <- read.csv("Results/Patch_Stats/patchno/patchno/carenigr.csv")
patchno_carenigr_dom[is.na(patchno_carenigr_dom)] <- 0
patchno_carenigr_dom <- patchno_carenigr_dom[,-1]
barplot(patchno_carenigr_dom$x, names.arg = patchno_carenigr_dom$quad_no, cex.axis = 0.7,
        xlab = "Quadrat number", ylab = "Number of Patches", main = "No. of patches C. nigra")
hist_patchno_carenigr <- hist(patchno_carenigr_dom$x, xlab = "Number of patches")
xfit<-seq(min(patchno_carenigr_dom$x),max(patchno_carenigr_dom$x),length=8) 
yfit<-dnorm(xfit,mean=mean(patchno_carenigr_dom$x),sd=sd(patchno_carenigr_dom$x)) 
yfit <- yfit * diff(hist_patchno_carenigr$mids[1:2])*length(patchno_carenigr_dom$x)
plot(hist_patchno_carenigr, main = "Number of Patches C. nigra", xlab = "Number of Patches")
lines(xfit, yfit, col = "black", lwd = 2)

density_carenigr <- density(patchno_carenigr_dom$x)
plot(density_carenigr, main = "No. of Patches C. nigra", xlim = c(0, max(patchno_carenigr_dom$x)))

pdf("Results/Plots/Frequency-Distribution/Patch_no/Carenigr.pdf")
dev.off()

## GALIUM SAXATILE 
patchno_galisaxa_dom <- read.csv("Results/Patch_Stats/patchno/patchno/galisaxa.csv")
patchno_galisaxa_dom[is.na(patchno_galisaxa_dom)] <- 0
patchno_galisaxa_dom <- patchno_galisaxa_dom[,-1]
barplot(patchno_galisaxa_dom$x, names.arg = patchno_galisaxa_dom$quad_no, cex.axis = 0.7,
        xlab = "Quadrat number", ylab = "Number of Patches", main = "No. of patches G. saxatile")
hist_patchno_galisaxa <- hist(patchno_galisaxa_dom$x, xlab = "Number of patches")
xfit<-seq(min(patchno_galisaxa_dom$x),max(patchno_galisaxa_dom$x),length=8) 
yfit<-dnorm(xfit,mean=mean(patchno_galisaxa_dom$x),sd=sd(patchno_galisaxa_dom$x)) 
yfit <- yfit * diff(hist_patchno_galisaxa$mids[1:2])*length(patchno_galisaxa_dom$x)
plot(hist_patchno_galisaxa, main = "Number of Patches G. saxatile", xlab = "Number of Patches")
lines(xfit, yfit, col = "black", lwd = 2)

density_galisaxa <- density(patchno_galisaxa_dom$x)
plot(density_galisaxa, main = "No. of Patches N. stricta", xlim = c(0, max(patchno_galisaxa_dom$x)))

pdf("Results/Plots/Frequency-Distribution/Patch_no/Galisaxa.pdf")
dev.off()

## POTENTILLA ERECTA
patchno_poteerec_dom <- read.csv("Results/Patch_Stats/patchno/patchno/poteerec.csv")
patchno_poteerec_dom[is.na(patchno_poteerec_dom)] <- 0
patchno_poteerec_dom <- patchno_poteerec_dom[,-1]
barplot(patchno_poteerec_dom$x, names.arg = patchno_poteerec_dom$quad_no, cex.axis = 0.7,
        xlab = "Quadrat number", ylab = "Number of Patches", main = "No. of patches P. erecta")
hist_patchno_poteerec <- hist(patchno_poteerec_dom$x, xlab = "Number of patches")
xfit<-seq(min(patchno_poteerec_dom$x),max(patchno_poteerec_dom$x),length=8) 
yfit<-dnorm(xfit,mean=mean(patchno_poteerec_dom$x),sd=sd(patchno_poteerec_dom$x)) 
yfit <- yfit * diff(hist_patchno_poteerec$mids[1:2])*length(patchno_poteerec_dom$x)
plot(hist_patchno_poteerec, main = "Number of Patches P. erecta", xlab = "Number of Patches")
lines(xfit, yfit, col = "black", lwd = 2)

density_poteerec <- density(patchno_poteerec_dom$x)
plot(density_poteerec, main = "No. of Patches P. erecta", xlim = c(0, max(patchno_poteerec_dom$x)))
pdf("Results/Plots/Frequency-Distribution/Patch_no/Perecta.pdf")
dev.off()


## DESCHAMPSIA FLEXUOSA
patchno_descflex_dom <- read.csv("Results/Patch_Stats/patchno/patchno/descflex.csv")
patchno_descflex_dom[is.na(patchno_descflex_dom)] <- 0
patchno_descflex_dom <- patchno_descflex_dom[,-1]
barplot(patchno_descflex_dom$x, name.arg = patchno_descflex_dom$quad_no, cex.axis = 0.7,
        xlab = "Quadrat number", ylab = "Number of Patches", main = "No. of patches D. flexuosa")
hist_patchno_descflex <- hist(patchno_descflex_dom$x, xlab = "Number of patches")
xfit<-seq(min(patchno_descflex_dom$x),max(patchno_descflex_dom$x),length=8) 
yfit<-dnorm(xfit,mean=mean(patchno_descflex_dom$x),sd=sd(patchno_descflex_dom$x)) 
yfit <- yfit * diff(hist_patchno_descflex$mids[1:2])*length(patchno_descflex_dom$x)
plot(hist_patchno_descflex, main = "Number of Patches D. flexuosa", xlab = "Number of Patches")
lines(xfit, yfit, col = "black", lwd = 2)

density_descflex <- density(patchno_descflex_dom$x)
plot(density_descflex, main = "No. of Patches D. flexuosa", xlim = c(0, max(patchno_descflex_dom$x)))
pdf("Results/Plots/Frequency-Distribution/Patch_no/Descflex.pdf")
dev.off()

## VACCINIUM MYRTILLUS
patchno_vaccmyrt_dom <- read.csv("Results/Patch_Stats/patchno/patchno/vaccmyrt.csv")
patchno_vaccmyrt_dom[is.na(patchno_vaccmyrt_dom)] <- 0
patchno_vaccmyrt_dom <- patchno_vaccmyrt_dom[,-1]
barplot(patchno_vaccmyrt_dom$x, names.arg = patchno_vaccmyrt_dom$quad_no, cex.axis = 0.7,
        xlab = "Quadrat number", ylab = "Number of Patches", main = "No. of patches V. myrtillus")
hist_patchno_vaccmyrt <- hist(patchno_vaccmyrt_dom$x, xlab = "Number of patches")
xfit<-seq(min(patchno_vaccmyrt_dom$x),max(patchno_vaccmyrt_dom$x),length=8) 
yfit<-dnorm(xfit,mean=mean(patchno_vaccmyrt_dom$x),sd=sd(patchno_vaccmyrt_dom$x)) 
yfit <- yfit * diff(hist_patchno_vaccmyrt$mids[1:2])*length(patchno_vaccmyrt_dom$x)
plot(hist_patchno_vaccmyrt, main = "Number of Patches V. myrtillus", xlab = "Number of Patches")
lines(xfit, yfit, col = "black", lwd = 2)

density_vaccmyrt <- density(patchno_vaccmyrt_dom$x)
plot(density_vaccmyrt, main = "No. of Patches V. myrtillus", xlim = c(0, max(patchno_vaccmyrt_dom$x)))

pdf("Results/Plots/Frequency-Distribution/Patch_no/Vaccmyrt.pdf")
dev.off()

##### AREA
area_callvulg_dom <- read.csv("Results/Patch_Stats/Area/callvulg.csv")

area_nardstri_dom <- read.csv("Results/Patch_Stats/Area/nardstri.csv")
area_molicaer_dom <- read.csv("Results/Patch_Stats/Area/molicaer.csv")
area_eriovagi_dom <- read.csv("Results/Patch_Stats/Area/eriovagi.csv")
area_juncsqua_dom <- read.csv("Results/Patch_Stats/Area/juncsqua.csv")
area_junceffu_dom <- read.csv("Results/Patch_Stats/Area/junceffu.csv")
area_carenigr_dom <- read.csv("Results/Patch_Stats/Area/carenigr.csv")
area_galisaxa_dom <- read.csv("Results/Patch_Stats/Area/galisaxa.csv")
area_poteerec_dom <- read.csv("Results/Patch_Stats/Area/poteerec.csv")
area_descflex_dom <- read.csv("Results/Patch_Stats/Area/descflex.csv")
area_vaccmyrt_dom <- read.csv("Results/Patch_Stats/Area/vaccmyrt.csv")

##### PERIMETER
perim_callvulg_dom <- read.csv("Results/Patch_Stats/Perimeter/callvulg.csv")
perim_nardstri_dom <- read.csv("Results/Patch_Stats/Perimeter/nardstri.csv")
perim_molicaer_dom <- read.csv("Results/Patch_Stats/Perimeter/molicaer.csv")
perim_eriovagi_dom <- read.csv("Results/Patch_Stats/Perimeter/eriovagi.csv")
perim_juncsqua_dom <- read.csv("Results/Patch_Stats/Perimeter/juncsqua.csv")
perim_junceffu_dom <- read.csv("Results/Patch_Stats/Perimeter/junceffu.csv")
perim_carenigr_dom <- read.csv("Results/Patch_Stats/Perimeter/carenigr.csv")
perim_galisaxa_dom <- read.csv("Results/Patch_Stats/Perimeter/galisaxa.csv")
perim_poteerec_dom <- read.csv("Results/Patch_Stats/Perimeter/poteerec.csv")
perim_descflex_dom <- read.csv("Results/Patch_Stats/Perimeter/descflex.csv")
perim_vaccmyrt_dom <- read.csv("Results/Patch_Stats/Perimeter/vaccmyrt.csv")

##### PER:AREA RATIO
per.area_callvulg_dom <- read.csv("Results/Patch_Stats/Per-Area-Ratio/callvulg.csv")
per.area_nardstri_dom <- read.csv("Results/Patch_Stats/Per-Area-Ratio/nardstri.csv")
per.area_molicaer_dom <- read.csv("Results/Patch_Stats/Per-Area-Ratio/molicaer.csv")
per.area_eriovagi_dom <- read.csv("Results/Patch_Stats/Per-Area-Ratio/eriovagi.csv")
per.area_juncsqua_dom <- read.csv("Results/Patch_Stats/Per-Area-Ratio/juncsqua.csv")
per.area_junceffu_dom <- read.csv("Results/Patch_Stats/Per-Area-Ratio/junceffu.csv")
per.area_carenigr_dom <- read.csv("Results/Patch_Stats/Per-Area-Ratio/carenigr.csv")
per.area_galisaxa_dom <- read.csv("Results/Patch_Stats/Per-Area-Ratio/galisaxa.csv")
per.area_poteerec_dom <- read.csv("Results/Patch_Stats/Per-Area-Ratio/poteerec.csv")
per.area_descflex_dom <- read.csv("Results/Patch_Stats/Per-Area-Ratio/descflex.csv")
per.area_vaccmyrt_dom <- read.csv("Results/Patch_Stats/Per-Area-Ratio/vaccmyrt.csv")

##### SHAPE INDEX
shape.ind_callvulg_dom <- read.csv("Results/Patch_Stats/Shape_Index/callvulg.csv")
shape.ind_nardstri_dom <- read.csv("Results/Patch_Stats/Shape_Index/nardstri.csv")
shape.ind_molicaer_dom <- read.csv("Results/Patch_Stats/Shape_Index/molicaer.csv")
shape.ind_eriovagi_dom <- read.csv("Results/Patch_Stats/Shape_Index/eriovagi.csv")
shape.ind_juncsqua_dom <- read.csv("Results/Patch_Stats/Shape_Index/juncsqua.csv")
shape.ind_junceffu_dom <- read.csv("Results/Patch_Stats/Shape_Index/junceffu.csv")
shape.ind_carenigr_dom <- read.csv("Results/Patch_Stats/Shape_Index/carenigr.csv")
shape.ind_galisaxa_dom <- read.csv("Results/Patch_Stats/Shape_Index/galisaxa.csv")
shape.ind_poteerec_dom <- read.csv("Results/Patch_Stats/Shape_Index/poteerec.csv")
shape.ind_descflex_dom <- read.csv("Results/Patch_Stats/Shape_Index/descflex.csv")
shape.ind_vaccmyrt_dom <- read.csv("Results/Patch_Stats/Shape_Index/vaccmyrt.csv")


################################################################################### 
##      LINEAR ANALYSIS: % COVER AND PATCH NUMBER PER CHOSEN SPECIES             ##
###################################################################################
spp_abund <- read.csv("Data/Abundance_2-08-18.csv")
callvulg_abund <- data.frame(spp_abund$Callvulg)
patchno_callvulg_dom <- read.csv("Results/Patch_Stats/Dominant/patchno/patchno/callvulg.csv")
patchno_callvulg_dom[is.na(patchno_callvulg_dom)] <- 0
patchno_callvulg_dom <- patchno_callvulg_dom[,-1:-2]
callvulg_abund_patchno <- data.frame(cbind(callvulg_abund, patchno_callvulg_dom))
colnames(callvulg_abund_patchno) <- c("perc_cover", "No_patches")
lm.callvulg_abund_patchno <- lm(No_patches ~ perc_cover, data = callvulg_abund_patchno)
summary(lm.callvulg_abund_patchno)
plot(perc_cover ~ No_patches, data = callvulg_abund_patchno, pch = 20)
abline(lm.callvulg_abund_patchno)
plot(lm.callvulg_abund_patchno)

area_callvulg_dom <- read.csv("Results/Patch_Stats/Dominant/Area/Area/callvulg.csv")
area_callvulg_dom[is.na(area_callvulg_dom)] <- 0
area_callvulg_dom <- area_callvulg_dom[,-1:-2]
callvulg_abund_area <- data.frame(cbind(callvulg_abund, area_callvulg_dom))
colnames(callvulg_abund_area) <- c("perc_cover", "Area")
lm.callvulg_abund_area <- lm(Area ~ perc_cover, data = callvulg_abund_area)
summary(lm.callvulg_abund_area)
plot(perc_cover ~ Area, data = callvulg_abund_area, pch = 20)
abline(lm.callvulg_abund_area)
plot(lm.callvulg_abund_area)


################################################################################### 
##     MULTIVARIATE CCA/RDA ANALYSIS: RELATIONSHIP WITH ENVIRON. FACTORS         ##
###################################################################################
##            TO COMPARE WITH RAW % COVER FROM ASHTREES DATA                     ##
###################################################################################

## 1. ##
## Constrained ordination (RDA or CCA) of the Dominant and Subdominants vs 
##the same predictors you used for the Ashtrees % cover data to get an overview and 
##feel for what’s going on.

########## DOMINANT ########## 

area_dom_matrix <- read.csv("Results/Patch_Stats/Ordination/Area_spp/Area_spp_dominant_4-08-19.csv")
area_dom_matrix <- area_dom_matrix[,-1]
area_dom_matrix[is.na(area_dom_matrix)] <- 0
area_dom_matrix <- area_dom_matrix[,-54] ## taraxacu
area_dom_matrix <- area_dom_matrix[,-48]
area_dom_matrix <- area_dom_matrix[,-46]
area_dom_matrix <- area_dom_matrix[,-34]
area_dom_matrix <- area_dom_matrix[,-16]
area_dom_matrix <- area_dom_matrix[,-12] ## cerarve
area_dom_matrix <- area_dom_matrix[,-4]

env_factors <- data.frame(read.csv("Data/Biotic_data/ashtrees_env_variables_14-01-19.csv"))
env_factors[is.na(env_factors)] <- 0

area_dom_stand <- decostand(area_dom_matrix, method = "hellinger")
area_dom_cca <- cca(area_dom_stand)
area_dom_rda <- rda(area_dom_stand)
plot(area_dom_cca, display = "sites")
plot(area_dom_rda, display = "sites")
area_dom_rda.env <- rda(area_dom_stand ~ soil_pH + slope + Altitude + pct_water + length_10m + distance_10m + 
                          length_25m + distance_25m + length_35m + distance_35m + ditch_distance, 
                data = env_factors, scale = FALSE) # Scaled = FALSE
plot(area_dom_rda.env, display = c("sites", "bp"), main = "RDA Dominant")
plot(area_dom_rda.env, display = c("species", "bp"))

########## SUBDOMINANT ########## 

area_sub_matrix <- read.csv("Results/Patch_Stats/Ordination/Area_spp/Area_matrix/Area_spp_sub_4-08-19.csv")
area_sub_matrix <- area_sub_matrix[,-1]
area_sub_matrix[is.na(area_sub_matrix)] <- 0
area_sub_matrix <- area_sub_matrix[,-61] # trifrepe
area_sub_matrix <- area_sub_matrix[,-54] # SHEEP
area_sub_matrix <- area_sub_matrix[,-51] # ROCK
area_sub_matrix <- area_sub_matrix[,-35] # LITTER
area_sub_matrix <- area_sub_matrix[,-16] # DEAD
area_sub_matrix <- area_sub_matrix[,-5] # BARE

env_factors <- data.frame(read.csv("Data/Biotic_data/ashtrees_env_variables_14-01-19.csv"))
env_factors[is.na(env_factors)] <- 0

area_sub_stand <- decostand(area_sub_matrix, method = "hellinger")
area_sub_cca <- cca(area_sub_stand)
area_sub_rda <- rda(area_sub_stand)
plot(area_sub_cca, display = "sites")
plot(area_sub_rda, display = "sites")
area_sub_rda.env <- rda(area_sub_stand ~ soil_pH + slope + Altitude + pct_water + length_10m + distance_10m + 
                          length_25m + distance_25m + length_35m + distance_35m + ditch_distance, 
                        data = env_factors, scale = FALSE) # Scaled = FALSE
plot(area_sub_rda.env, display = c("sites", "bp"), main = "RDA Sub-dominant")
plot(area_dom_rda.env, display = c("species", "bp"))

plot(area_sub_rda.env, type = "n", xlim = c(-0.6,0.6), ylim = c(-0.5, 0.5))
text(area_sub_rda.env, display = "species", col = 1, cex = 0.7)
text(area_sub_rda.env, display = "bp", col = 2, cex = 0.7)
plot(area_sub_rda.env, type = "n", xlim = c(-0.6,0.6), ylim = c(-0.5, 0.5))
text(area_sub_rda.env, display = "species", col = 1, cex = 0.8)
text(area_sub_rda.env, display = "bp", col = 2, cex = 0.7)


plot(area_sub_rda.env, type = "n")
points(area_sub_rda.env, display = "sites", col = 1, cex = 0.7, pch = 16)
text(area_sub_rda.env, display = "bp", col = 2, cex = 0.7)
text(area_dom_rda.env, display = "species", col = 1, cex = 0.7)
pdf("Results/Plots/Ordination/Dom_RDA.pdf")
dev.off()
pdf("Results/Plots/Ordination/SUBDOM/Sub_RDA.pdf")
dev.off()

capture.output(anova(area_dom_rda.env), file = "Results/DOMINANT/ANOVA_Overall_DOM.doc")  # Overall significance
capture.output(anova(area_dom_rda.env, by = "axis"), file = "Results/DOMINANT/ANOVA_axis_DOM.doc") # separate tests on RDA1 and RDA2
capture.output(anova(area_dom_rda.env, by= "terms"), file = "Results/DOMINANT/ANOVA_terms_DOM.doc") # each predictor separately
capture.output(anova(area_dom_rda.env, by= "margin"), file = "Results/DOMINANT/ANOVA_margin_DOM.doc") # each predictor taking into account collinearities; usually less significant

capture.output(anova(area_sub_rda.env), file = "Results/SUBDOM/ANOVA_Overall_DOM.doc")  # Overall significance
capture.output(anova(area_sub_rda.env, by = "axis"), file = "Results/SUBDOM/ANOVA_axis_DOM.doc") # separate tests on RDA1 and RDA2
capture.output(anova(area_sub_rda.env, by= "terms"), file = "Results/SUBDOM/ANOVA_terms_DOM.doc") # each predictor separately
capture.output(anova(area_sub_rda.env, by= "margin"), file = "Results/SUBDOM/ANOVA_margin_DOM.doc") # each predictor taking into account collinearities; usually less significant

## 2. ## 
## Then you could try a GLM with e.g. number of patches as the response, and altitude, 
## pct_water, pH and slope as predictors (possibly include some interactions)
## lm(area_of_calluna ~ pH + altitude + slope, data=....)
## glm(no_patches_calluna ~ pH + altitude + slope, data=...., family="poisson")

################################################################################### 
##         AREA: GLM ANALYSIS: RELATIONSHIP WITH ENVIRON. FACTORS                ##
###################################################################################
###################
###  DOMINANT   ###
###################
area_dom_spp <- read.csv("Results/Patch_Stats/Ordination/Area_spp/Area_matrix/Area_spp_dominant_4-08-19.csv")
area_dom_spp[is.na(area_dom_spp)] <- 0
area_dom_spp <- area_dom_spp[,-55]
area_dom_spp <- area_dom_spp[,-49] # sheep
area_dom_spp <- area_dom_spp[,-47] # ROCK
area_dom_spp <- area_dom_spp[,-35] # LITTER
area_dom_spp <- area_dom_spp[,-17] # DEAD
area_dom_spp <- area_dom_spp[,-8] # cerarve = 0
area_dom_spp <- area_dom_spp[,-5] # BARE
area_dom_spp <- area_dom_spp[,-1] # quad_no
env_var <- data.frame(read.csv("Data/Biotic_data/ashtrees_env_variables_14-01-19.csv"))
env_var[is.na(env_var)] <- 0

area_dom_mvabund <- mvabund(area_dom_spp)
plot.mvformula(log(area_dom_mvabund+1) ~ exp(env_var$soil_pH), main="Dominant Area vs Soil pH",
               xlab="% Soil pH - Log Scale ", ylab="AREA[log scale]",
               overall.main="Species area DOM vs soil pH",
               fg="grey", las=1, scale.lab="ss",t.lab="o", mfrow=c(4,3),log="x") # Produces a range of plots for 
                      #visualising multivariate abundance data and its relationship to environmental
                      #variables

#boxplot(area_dom_mvabund, horizontal = TRUE, las = 2, main = "Area Cover") # boxplot
meanvar.plot(area_dom_mvabund) # check mean variance
#area_dom_glm_poisson <- manyglm(area_dom_mvabund ~ soil_pH + slope + pct_water + Altitude, data = env_var, family = "poisson") # gives fan shape; 
## This could mean that one of our assumptions was wrong: either our mean-variance relationship was wrongly specified, 
## or our assumed relationship between our response and predictors was incorrect.
#plot(area_dom_glm_poisson)

#area_dom_glm_binomial <- manyglm(area_dom_mvabund ~ soil_pH + slope + pct_water + Altitude, 
#                                 data = env_var, family = "binomial") # gives a curve
#plot(area_dom_glm_binomial)  
area_dom_glm_negbinomial <- manyglm(area_dom_mvabund ~ soil_pH + slope + pct_water + Altitude, 
                                  data = env_var, family = "negative.binomial", show.coef = TRUE) # does not give a particular shape; will use this
plot(area_dom_glm_negbinomial)
plot(area_dom_glm_negbinomial, which=1) #note the marked fan-shape on the plot

summary(area_dom_glm_negbinomial, resamp="monte.carlo", test="wald", nBoot=300) # summary method for class "manyglm"
## NEED TO CHECK RE. SUMMARY FOR RESAMPLING AND BOOTSTRAPPING?

area_dom_lm_gauss <- manylm(area_dom_mvabund ~ soil_pH + slope + pct_water + Altitude, 
                            data = env_var, family = "Gaussian") # cannot fit glm with Gaussian
plot(area_dom_glm_gauss)

anova_glm_dom_area <- anova(area_dom_glm_negbinomial)
capture.output(anova_glm_dom_area, file = "Results/DOMINANT/anova_glm_area.doc")
#capture.output(anova(area_dom_glm_negbinomial, by = "axis"), file = "Results/DOMINANT/anova_axis_area.doc") # separate tests on RDA1 and RDA2
#capture.output(anova(area_dom_glm_negbinomial, by= "terms"), file = "Results/DOMINANT/anova_terms_area.doc") # each predictor separately
#capture.output(anova(area_dom_glm_negbinomial, by= "margin"), file = "Results/DOMINANT/anova_margin_area.doc") # each predictor taking into account collinearities; usually less significant

resid_glm_area <- residuals(area_dom_glm_negbinomial)
plot(resid_glm_area)
summary(resid_glm_area)


### try traitglm ###
trait_dom_area <- traitglm(area_dom_spp, temp, Q = NULL, family="negative.binomial", formula = NULL, method = "manyglm",
         composition = FALSE, col.intercepts = TRUE, show.time = "all")
plot(trait_dom_area)

anova_ind_dom_area <- anova(area_dom_glm_negbinomial, p.uni="adjusted", show.time = "all") # get ANOVA results for each species vs environment variables
capture.output(anova_ind_dom_area, file = "Results/DOMINANT/anova_glm_area_ind.doc")
###################
### SUBDOMINANT ###
###################
area_sub_spp <- read.csv("Results/Patch_Stats/Ordination/Area_spp/Area_matrix/Area_spp_sub_4-08-19.csv")
area_sub_spp[is.na(area_sub_spp)] <- 0
area_sub_spp <- area_sub_spp[,-61] # Trifrepe
area_sub_spp <- area_sub_spp[,-55] # SHEEP
area_sub_spp <- area_sub_spp[,-52] # ROCK
area_sub_spp <- area_sub_spp[,-36] # LITTER
area_sub_spp <- area_sub_spp[,-17] # BARE
area_sub_spp <- area_sub_spp[,-6]
area_sub_spp <- area_sub_spp[,-1] # quad_no
env_var <- data.frame(read.csv("Data/Biotic_data/ashtrees_env_variables_14-01-19.csv"))
env_var[is.na(env_var)] <- 0

area_sub_mvabund <- mvabund(area_sub_spp)
plot.mvformula(log(area_sub_mvabund+1) ~ exp(env_var$soil_pH), main="SUB Area vs Soil pH",
               xlab="% Soil pH - Log Scale ", ylab="Abundance [log scale]",
               overall.main="Species area SUB vs soil pH",
               fg="grey", las=1, scale.lab="ss",t.lab="o", mfrow=c(4,3),log="x") # Produces a range of plots for 
#visualising multivariate abundance data and its relationship to environmental
#variables

boxplot(area_sub_mvabund, horizontal = TRUE, las = 2, main = "Area Cover") # boxplot
meanvar.plot(area_sub_mvabund) # check mean variance
#area_sub_glm_poisson <- manyglm(area_sub_mvabund ~ soil_pH + slope + pct_water + Altitude, data = env_var, family = "poisson") # gives fan shape; 
## This could mean that one of our assumptions was wrong: either our mean-variance relationship was wrongly specified, 
## or our assumed relationship between our response and predictors was incorrect.
#plot(area_sub_glm_poisson)

#area_sub_glm_binomial <- manyglm(area_sub_mvabund ~ soil_pH + slope + pct_water + Altitude, 
#                                 data = env_var, family = "binomial") # gives a curve
#plot(area_sub_glm_binomial)  
area_sub_glm_negbinomial <- manyglm(area_sub_mvabund ~ soil_pH + slope + pct_water + Altitude, 
                                    data = env_var, family = "negative.binomial") # does not give a particular shape; will use this
plot(area_sub_glm_negbinomial)
abline(area_sub_glm_negbinomial)
summary(area_sub_glm_negbinomial, resamp="monte.carlo", test="wald", nBoot=300) # summary method for class "manyglm"
## NEED TO CHECK RE. SUMMARY FOR RESAMPLING AND BOOTSTRAPPING?

#area_sub_lm_gauss <- manylm(area_sub_mvabund ~ soil_pH + slope + pct_water + Altitude, 
#                            data = env_var, family = "Gaussian") # cannot fit glm with Gaussian
#plot(area_sub_lm_gauss)

anova_glm_sub_area <- anova.manyglm(area_sub_glm_negbinomial)
capture.output(anova_glm_sub_area, file = "Results/SUBDOM/GLM/AREA/anova_glm_sub_area.doc")

resid_glm_sub_area <- residuals(area_sub_glm_negbinomial)
plot(resid_glm_sub_area)
summary(resid_glm_sub_area)

anova_ind_sub_area <- anova(area_sub_glm_negbinomial, p.uni="adjusted", show.time = "all")
capture.output(anova_ind_sub_area, file = "Results/SUBDOM/anova_glm_area_sub_ind.doc")

################################################################################### 
##    NUMBER OF PATCHES: GLM ANALYSIS: RELATIONSHIP WITH ENVIRON. FACTORS        ##
###################################################################################

###################
###  DOMINANT   ###
###################

patchno_dom_spp <- read.csv("Results/Patch_Stats/Ordination/Patchno_spp/Patchno_matrix/Patchno_matrix_dom_12-08-19.csv")
patchno_dom_spp[is.na(patchno_dom_spp)] <- 0
patchno_dom_spp <- patchno_dom_spp[,-55] # trifrepe
patchno_dom_spp <- patchno_dom_spp[,-49] # sheep
patchno_dom_spp <- patchno_dom_spp[,-47] # ROCK
patchno_dom_spp <- patchno_dom_spp[,-35] # LITTER
patchno_dom_spp <- patchno_dom_spp[,-17] # DEAD
patchno_dom_spp <- patchno_dom_spp[,-8] # cerarve = 0
patchno_dom_spp <- patchno_dom_spp[,-5] # BARE
patchno_dom_spp <- patchno_dom_spp[,-1] # quad_no
env_var <- data.frame(read.csv("Data/Biotic_data/ashtrees_env_variables_03-08-18.csv"))
env_var[is.na(env_var)] <- 0

patchno_dom_mvabund <- mvabund(patchno_dom_spp)
plot.mvformula(log(patchno_dom_mvabund+1) ~ exp(env_var$soil_pH), main="Dominant Patchno vs Soil pH",
               xlab="% Soil pH - Log Scale ", ylab="Abundance [log scale]",
               overall.main="Species Abundance vs soil pH",
               fg="grey", las=1, scale.lab="ss",t.lab="o", mfrow=c(4,3),log="x") # Produces a range of plots for 
#visualising multivariate abundance data and its relationship to environmental
#variables

boxplot(patchno_dom_mvabund, horizontal = TRUE, las = 2, main = "Number of Patches") # boxplot
meanvar.plot(patchno_dom_mvabund) # check mean variance

patchno_dom_glm_poisson <- manyglm(patchno_dom_mvabund ~ soil_pH + slope + pct_water 
                                   + Altitude, data = env_var, family = "poisson") # gives fan shape; 
## This could mean that one of our assumptions was wrong: either our mean-variance relationship was wrongly specified, 
## or our assumed relationship between our response and predictors was incorrect.
plot(patchno_dom_glm_poisson)

patchno_dom_glm_spp <- manyglm(patchno_dom_mvabund ~ agrocapi + agrostolo + anthodor + callvulg + cardpalu + careechi + carenigr + carepani + carepilu + ceraarve +
                                     cirsarve + cirspalu + cynocris + desccesp + descflex + empenigr + erictetr + erioangu + eriovagi + festovin + festrubr + galisaxa + 
                                     holclana + holcmoll + hypncupr + juncacut + juncarti + junccong + junceffu +  juncsqua + luzumult + molicaer + nardstri + plagundu + 
                                     planlanc + pleuschr + poaannu + poaprat + polycomm + poteerec + rhytsqua + rumeacet + sphacapi + sphafall + sphapalu + spharubr + 
                                     sphastolo + triccesp, data = patchno_dom_spp, family = "negative.binomial") # using species as variables

# what happens if we use quadrat numbers as variables?

plot(patchno_dom_glm_spp)

anova_patchno_dom_spp <- anova.manyglm(patchno_dom_glm_spp, show.time = "all") # summary of species vs species (i.e. species used as variables)
capture.output(anova_patchno_dom_spp, file = "Results/DOMINANT/anova_glm_dom_spp.doc")

anova_ind_dom_spp <- anova(patchno_dom_glm_spp, p.uni="adjusted", show.time = "all") # try to get anova results for each species with each species in each quadrat
capture.output(anova_ind_dom_spp, file = "Results/DOMINANT/anova_ind_dom_spp.doc")

#area_sub_glm_binomial <- manyglm(area_sub_mvabund ~ soil_pH + slope + pct_water + Altitude, 
#                                 data = env_var, family = "binomial") # gives a curve
#plot(area_sub_glm_binomial)  
patchno_dom_glm_negbinomial <- manyglm(patchno_dom_mvabund ~ soil_pH + slope + pct_water + Altitude, 
                                    data = env_var, family = "negative.binomial") # does not give a particular shape; will use this
plot(patchno_dom_glm_negbinomial)

summary(patchno_sub_glm_negbinomial, resamp="monte.carlo", test="wald", nBoot=300) # summary method for class "manyglm"
## NEED TO CHECK RE. SUMMARY FOR RESAMPLING AND BOOTSTRAPPING?

anova_glm_dom_patchno <- anova.manyglm(patchno_dom_glm_negbinomial, show.time = "all")
capture.output(anova_glm_dom_patchno, file = "Results/DOMINANT/anova_glm_dom_patchno.doc")

resid_glm_dom_patchno <- residuals(patchno_dom_glm_negbinomial)
plot(resid_glm_dom_patchno)
summary(resid_glm_dom_patchno)

anova_ind_dom_patchno <- anova(patchno_dom_glm_negbinomial, p.uni="adjusted", show.time = "all")
capture.output(anova_ind_dom_patchno, file = "Results/DOMINANT/patchno_anova_glm_dom_ind.doc")
#######################
## testing if we transpose dominant vegetation data so that quadrats are a variable rather than species
# using patchno_dom_spp
# using patchno_dom_mvabund
getOption("max.print")
options(max.print = 1000000000)

#patchno_dom_mvabund <- mvabund(patchno_dom_spp)

patchno_dom_transposed <- as.data.frame(t(patchno_dom_spp))
features <- c(sprintf("q%d", seq(1,167)))
colnames(patchno_dom_transposed) <- features
# use mvabund on transposed patchno_dom_spp to relate it with the species where quadrat is the variable
patchno_dom_spp_mva_t <- mvabund(patchno_dom_transposed) # transpose so that quadrat number is on top and use as mvabund class
#rownames(patchno_dom_spp_mva_t) <- rownames(patchno_dom_transposed)

patchno_dom_glm_quads <- manyglm(patchno_dom_spp_mva_t ~ q1 +q2 +q3 +q4 +q5 +q6 +q7 + q8 + q9 
                                 +q10 +q11 +q12 +q13 +q14 +q15 +q16 +q17 +q18 +q19 
                                 +q20 +q21 +q22 +q23 +q24 +q25 +q26 +q27 +q28 +q29 +q30 +q31 +q32 
                                 +q33 +q34 +q35 +q36 +q37 +q38 +q39 +q40 +q41 +q42 +q43 +q44 +q45 
                                 +q46 +q47 +q48 +q49 +q50 +q51 +q52 +q53 +q54 +q55 +q56 +q57 
                                 +q58 +q59 +q60 +q61 +q62 +q63 +q64 +q65 +q66 +q67 +q68 +q69 +q70 
                                 +q71 +q72 +q73 +q74 +q75 +q76 +q77 +q78 +q79 +q80 +q81 +q82 +q83 
                                 +q84 +q85 +q86 +q87 +q88 +q89 +q90 +q91 +q92 +q93 +q94 +q95 
                                 +q96 +q97 +q98 +q99 +q100 +q101 +q102 +q103 +q104 +q105 +q106 +q107 
                                 +q108 +q109 +q110 +q111 +q112 +q113 +q114+q115 +q116 +q117 +q118 +q119 
                                 +q120 +q121 +q122 +q123 +q124 +q125 +q126 +q127 +q128 +q129 +q130 +q131 
                                 +q132 +q133+q134 +q135 +q136 +q137 +q138 +q139 +q140 +q141 +q142 
                                 +q143 +q144 +q145 +q146 +q147 +q148 +q149 +q150 +q151 +q152+q153 
                                 +q154 +q155 +q156 +q157 +q158 +q159 +q160 +q161 +q162 +q163 +q164 
                                 +q165 +q166 +q167, data = patchno_dom_transposed, family = "negative.binomial")
plot(patchno_dom_glm_quads)
anova_ind_dom_quadrats_2 <- anova(patchno_dom_glm_quads, p.uni = "adjusted", show.time = "all") # try to get anova results using quadrat number as the variable
capture.output(anova_ind_dom_quadrats, file = "Results/DOMINANT/anova_ind_dom_quads_2.csv")

###################
### SUBDOMINANT ###
###################
patchno_sub_spp <- data.frame(read.csv("Results/Patch_Stats/Ordination/Patchno_spp/Patchno_matrix/Patchno_matrix_sub_12-08-19.csv"))
patchno_sub_spp[is.na(patchno_sub_spp)] <- 0
patchno_sub_spp <- patchno_sub_spp[,-61] # trifrepe = 0
patchno_sub_spp <- patchno_sub_spp[,-55] # sheep
patchno_sub_spp <- patchno_sub_spp[,-54] # rumeacetosa = 0
patchno_sub_spp <- patchno_sub_spp[,-52] # ROCK
patchno_sub_spp <- patchno_sub_spp[,-36] # LITTER
patchno_sub_spp <- patchno_sub_spp[,-6] # BARE
patchno_sub_spp <- patchno_sub_spp[,-1] # quad_no
env_var <- data.frame(read.csv("Data/Biotic_data/ashtrees_env_variables_03-08-18.csv"))
env_var[is.na(env_var)] <- 0

patchno_sub_mvabund <- mvabund(patchno_sub_spp)
plot.mvformula(log(patchno_sub_mvabund+1) ~ exp(env_var$soil_pH), main="Dominant Patchno vs Soil pH",
               xlab="% Soil pH - Log Scale ", ylab="Abundance [log scale]",
               overall.main="Species Abundance vs soil pH",
               fg="grey", las=1, scale.lab="ss",t.lab="o", mfrow=c(4,3),log="x") # Produces a range of plots for 
#visualising multivariate abundance data and its relationship to environmental
#variables

boxplot(patchno_sub_mvabund, horizontal = TRUE, las = 2, main = "Number of Patches: SUB") # boxplot
meanvar.plot(patchno_sub_mvabund) # check mean variance

patchno_sub_glm_poisson <- manyglm(patchno_sub_mvabund ~ soil_pH + slope + pct_water 
                                   + Altitude, data = env_var, family = "poisson") # gives fan shape although not bad fit
## This could mean that one of our assumptions was wrong: either our mean-variance relationship was wrongly specified, 
## or our assumed relationship between our response and predictors was incorrect.
plot(patchno_sub_glm_poisson)

#patchno_sub_glm_binomial <- manyglm(patchno_sub_mvabund ~ soil_pH + slope + pct_water + Altitude, 
#                                 data = env_var, family = "binomial") # gives a curve
#plot(patchno_sub_glm_binomial)  
patchno_sub_glm_negbinomial <- manyglm(patchno_sub_mvabund ~ soil_pH + slope + pct_water + Altitude, 
                                       data = env_var, family = "negative.binomial") # does not give a particular shape; will use this
plot(patchno_sub_glm_negbinomial)

summary(patchno_sub_glm_negbinomial, resamp="monte.carlo", test="wald", nBoot=300) # summary method for class "manyglm"
## NEED TO CHECK RE. SUMMARY FOR RESAMPLING AND BOOTSTRAPPING?

anova_glm_sub_patchno_negbin <- anova.manyglm(patchno_sub_glm_negbinomial, show.time = "all")
capture.output(anova_glm_sub_patchno_negbin, file = "Results/DOMINANT/anova_glm_sub_patchno.doc")
anova_glm_sub_patchno_pois <- anova.manyglm(patchno_sub_glm_poisson, show.time = "all")


resid_glm_sub_patchno <- residuals(patchno_sub_glm_negbinomial)
plot(resid_glm_sub_patchno)
summary(resid_glm_sub_patchno)

anova_ind_sub_patchno <- anova(patchno_sub_glm_negbinomial, p.uni="adjusted", show.time = "all")
capture.output(anova_ind_sub_patchno, file = "Results/SUBDOM/patchno_anova_glm_sub_ind.doc")

################################################################################### 
##        SHAPE INDEX: GLM ANALYSIS: RELATIONSHIP WITH ENVIRON. FACTORS          ##
###################################################################################
## 3. ## 
## Similarly for “within-patch calculated % cover area” (not the Ashtrees % cover; 
## a bit confusing I know!) and the same predictors.

###################
###  DOMINANT   ###
###################
shape_dom_spp <- data.frame(read.csv("Results/Patch_Stats/Ordination/Shape_Index/shape_matrix/Shape_dom_matrix_12-08-19.csv"))
shape_dom_spp[is.na(shape_dom_spp)] <- 0
shape_dom_spp <- shape_dom_spp[,-55] # taraxacu = 0
shape_dom_spp <- shape_dom_spp[,-49] # SHEEP
shape_dom_spp <- shape_dom_spp[,-47] # ROCK
shape_dom_spp <- shape_dom_spp[,-35] # LITTER
shape_dom_spp <- shape_dom_spp[,-17] # DEAD
shape_dom_spp <- shape_dom_spp[,-5] # BARE
shape_dom_spp <- shape_dom_spp[,-1] # quad_no
env_var <- data.frame(read.csv("Data/Biotic_data/ashtrees_env_variables_03-08-18.csv"))
env_var[is.na(env_var)] <- 0

shape_dom_mvabund <- mvabund(shape_dom_spp)
plot.mvformula(log(shape_dom_mvabund+1) ~ exp(env_var$soil_pH), main="Dominant Patchno vs Soil pH",
               xlab="% Soil pH - Log Scale ", ylab="Abundance [log scale]",
               overall.main="Species Abundance vs soil pH",
               fg="grey", las=1, scale.lab="ss",t.lab="o", mfrow=c(4,3),log="x") # Produces a range of plots for 
#visualising multivariate abundance data and its relationship to environmental
#variables

boxplot(shape_dom_mvabund, horizontal = TRUE, las = 2, main = "Shape Index: DOM") # boxplot
meanvar.plot(shape_dom_mvabund) # check mean variance


shape_dom_glm_binomial <- manyglm(shape_dom_mvabund ~ soil_pH + slope + pct_water + Altitude, 
                                 data = env_var, family = "binomial") # might be good but data exceeds range of 1
plot(shape_dom_glm_binomial)  
#shape_dom_glm_negbinomial <- manyglm(shape_dom_mvabund ~ soil_pH + slope + pct_water + Altitude, 
#                                       data = env_var, family = "negative.binomial") # does not give a particular shape; will use this
#plot(shape_dom_glm_negbinomial)

summary(shape_dom_glm_binomial, resamp="monte.carlo", test="wald", nBoot=300) # summary method for class "manyglm"
## NEED TO CHECK RE. SUMMARY FOR RESAMPLING AND BOOTSTRAPPING?

anova_glm_dom_shape_bin <- anova.manyglm(shape_dom_glm_binomial, show.time = "all")
capture.output(anova_glm_dom_shape_bin, file = "Results/DOMINANT/anova_glm_dom_shape.doc")

resid_glm_dom_shape <- residuals(shape_dom_glm_binomial)
plot(resid_glm_dom_shape)
summary(resid_glm_dom_shape)

anova_ind_dom_shape <- anova(shape_dom_glm_binomial, p.uni="adjusted", show.time = "all")
capture.output(anova_ind_dom_shape, file = "Results/DOMINANT/shape_anova_glm_dom_ind.doc")

###################
###  DOMINANT   ###
###################

shape_dom_spp <- data.frame(read.csv("Results/Patch_Stats/Ordination/Shape_Index/shape_matrix/Shape_dom_matrix_12-08-19.csv"))
shape_dom_spp[is.na(shape_dom_spp)] <- 0
shape_dom_spp <- shape_dom_spp[,-55] # taraxacu = 0
shape_dom_spp <- shape_dom_spp[,-49] # SHEEP
shape_dom_spp <- shape_dom_spp[,-47] # ROCK
shape_dom_spp <- shape_dom_spp[,-35] # LITTER
shape_dom_spp <- shape_dom_spp[,-17] # DEAD
shape_dom_spp <- shape_dom_spp[,-5] # BARE
shape_dom_spp <- shape_dom_spp[,-1] # quad_no
env_var <- data.frame(read.csv("Data/Biotic_data/ashtrees_env_variables_03-08-18.csv"))
env_var[is.na(env_var)] <- 0

shape_dom_mvabund <- mvabund(shape_dom_spp)
plot.mvformula(log(shape_dom_mvabund+1) ~ exp(env_var$soil_pH), main="Dominant Patchno vs Soil pH",
               xlab="% Soil pH - Log Scale ", ylab="Abundance [log scale]",
               overall.main="Species Abundance vs soil pH",
               fg="grey", las=1, scale.lab="ss",t.lab="o", mfrow=c(4,3),log="x") # Produces a range of plots for 
#visualising multivariate abundance data and its relationship to environmental
#variables

boxplot(shape_dom_mvabund, horizontal = TRUE, las = 2, main = "Shape Index: DOM") # boxplot
meanvar.plot(shape_dom_mvabund) # check mean variance


shape_dom_glm_binomial <- manyglm(shape_dom_mvabund ~ soil_pH + slope + pct_water + Altitude, 
                                  data = env_var, family = "binomial") # might be good but data exceeds range of 1
plot(shape_dom_glm_binomial)  
#shape_dom_glm_negbinomial <- manyglm(shape_dom_mvabund ~ soil_pH + slope + pct_water + Altitude, 
#                                       data = env_var, family = "negative.binomial") # does not give a particular shape; will use this
#plot(shape_dom_glm_negbinomial)

summary(shape_dom_glm_binomial, resamp="monte.carlo", test="wald", nBoot=300) # summary method for class "manyglm"
## NEED TO CHECK RE. SUMMARY FOR RESAMPLING AND BOOTSTRAPPING?

anova_glm_dom_shape_bin <- anova.manyglm(shape_dom_glm_binomial, show.time = "all")
capture.output(anova_glm_dom_shape_bin, file = "Results/DOMINANT/anova_glm_dom_shape.doc")

resid_glm_dom_shape <- residuals(shape_dom_glm_binomial)
plot(resid_glm_dom_shape)
summary(resid_glm_dom_shape)

anova_ind_dom_shape <- anova(shape_dom_glm_binomial, p.uni="adjusted", show.time = "all")
capture.output(anova_ind_dom_shape, file = "Results/DOMINANT/shape_anova_glm_dom_ind.doc")

###################
### SUBOMINANT  ###
###################

shape_sub_spp <- data.frame(read.csv("Results/Patch_Stats/Ordination/Shape_Index/shape_matrix/Shape_sub_matrix_12-08-19.csv"))
shape_sub_spp[is.na(shape_sub_spp)] <- 0
shape_sub_spp <- shape_sub_spp[,-61] # taraxacu = 0
shape_sub_spp <- shape_sub_spp[,-55] # SHEEP
shape_sub_spp <- shape_sub_spp[,-54] # rumexacetosa = 0
shape_sub_spp <- shape_sub_spp[,-52] # ROCK
shape_sub_spp <- shape_sub_spp[,-36] # LITTER
shape_sub_spp <- shape_sub_spp[,-17] # DEAD
shape_sub_spp <- shape_sub_spp[,-6] # BARE
shape_sub_spp <- shape_sub_spp[,-1] # quad_no
env_var <- data.frame(read.csv("Data/Biotic_data/ashtrees_env_variables_03-08-18.csv"))
env_var[is.na(env_var)] <- 0

shape_sub_mvabund <- mvabund(shape_sub_spp)
plot.mvformula(log(shape_sub_mvabund+1) ~ exp(env_var$soil_pH), main="Dominant Patchno vs Soil pH",
               xlab="% Soil pH - Log Scale ", ylab="Abundance [log scale]",
               overall.main="Species Abundance vs soil pH",
               fg="grey", las=1, scale.lab="ss",t.lab="o", mfrow=c(4,3),log="x") # Produces a range of plots for 
#visualising multivariate abundance data and its relationship to environmental
#variables

boxplot(shape_sub_mvabund, horizontal = TRUE, las = 2, main = "Shape Index: DOM") # boxplot
meanvar.plot(shape_sub_mvabund) # check mean variance


shape_sub_glm_binomial <- manyglm(shape_sub_mvabund ~ soil_pH + slope + pct_water + Altitude, 
                                  data = env_var, family = "binomial") # might be good but data exceeds range of 1
plot(shape_sub_glm_binomial)  
#shape_sub_glm_negbinomial <- manyglm(shape_sub_mvabund ~ soil_pH + slope + pct_water + Altitude, 
#                                       data = env_var, family = "negative.binomial") # does not give a particular shape; will use this
#plot(shape_sub_glm_negbinomial)

summary(shape_sub_glm_binomial, resamp="monte.carlo", test="wald", nBoot=300) # summary method for class "manyglm"
## NEED TO CHECK RE. SUMMARY FOR RESAMPLING AND BOOTSTRAPPING?

anova_glm_sub_shape_bin <- anova.manyglm(shape_sub_glm_binomial, show.time = "all")
capture.output(anova_glm_sub_shape_bin, file = "Results/SUBDOM/anova_glm_sub_shape.doc")

resid_glm_sub_shape <- residuals(shape_sub_glm_binomial)
plot(resid_glm_sub_shape)
summary(resid_glm_sub_shape)

anova_ind_sub_shape <- anova(shape_sub_glm_binomial, p.uni="adjusted", show.time = "all")
capture.output(anova_ind_sub_shape, file = "Results/SUBDOM/shape_anova_glm_sub_ind.doc")

################################################################################### 
##    GLM MVABUND FOR COMMUNITIES PREDICTED FROM ASH VS ASH PSEUDOQUADRATS       ##
###################################################################################

prob_pred <- read.csv("Data/Pseudoquad_predic/Probabilities_20-08-18.csv")
prob_pred <- prob_pred[,-1]
env_var <- data.frame(read.csv("Data/Biotic_data/ashtrees_env_variables_03-08-18.csv"))

prob_mvabund <- mvabund(prob_pred)

boxplot(prob_pred, horizontal = TRUE, las = 2, main = "Probability per community") # boxplot
meanvar.plot(prob_mvabund) # check mean variance

prob_lm <- manylm(prob_mvabund ~ soil_pH + slope + pct_water + Altitude, 
                                  data = env_var, method = "qr") # might be good but data exceeds range of 1
prob_glm <- manyglm(prob_mvabund ~ soil_pH + slope + pct_water + Altitude + soil_pH:Altitude, data = env_var,
                    method = "negative_binomial")
prob_lm_interaction <- manylm(prob_mvabund ~ soil_pH + slope + pct_water + Altitude + soil_pH:Altitude, 
                              data = env_var, method = "qr")
plot(prob_glm)  

anova_NVC_interaction <- anova(prob_glm, p.uni="adjusted", show.time = "all")
capture.output(anova_NVC_interaction, file = "Results/anova_glm_probability_GLM_24-08-18.doc")

anova_lm_prob <- anova.manylm(prob_lm) # all communities and all environment variables together
capture.output(anova_lm_prob, file = "Results/anova_lm_probability_all.doc")
summary.manylm(prob_lm)

################################################################################### 
##              Univariate GLM using TOTALS AND MEANS per quadrat                ##
###################################################################################
###################
###  DOMINANT   ###
###################
## number of patches ##
dom_quad_patch <- read.csv("Results/Patch_Stats/Mean_sum_total/dominant_sum_area_shape_quad_21-08-18.csv")
dom_quad_patch <- dom_quad_patch[,-1]
dom_quad_patch <- dom_quad_patch[-168,]

dom_species_stats <- read.csv("Results/Patch_Stats/Mean_sum_total/mean_species_dom_21-08-18.csv")
rownames(dom_species_stats) <- dom_species_stats$species
dom_species_stats <- dom_species_stats[,-1]
dom_species_stats <- dom_species_stats[-168,]

patchno_dom_spp <- read.csv("Results/Patch_Stats/Ordination/Patchno_spp/Patchno_matrix/Patchno_matrix_dom_12-08-19.csv")
patchno_dom_spp[is.na(patchno_dom_spp)] <- 0
patchno_dom_spp <- patchno_dom_spp[,-55] # trifrepe
patchno_dom_spp <- patchno_dom_spp[,-49] # sheep
patchno_dom_spp <- patchno_dom_spp[,-47] # ROCK
patchno_dom_spp <- patchno_dom_spp[,-35] # LITTER
patchno_dom_spp <- patchno_dom_spp[,-17] # DEAD
patchno_dom_spp <- patchno_dom_spp[,-8] # cerarve = 0
patchno_dom_spp <- patchno_dom_spp[,-5] # BARE
patchno_dom_spp <- patchno_dom_spp[,-1] # quad_no

patchno_dom_mvabund <- mvabund(patchno_dom_spp)

patchno_dom_glm_negbin <- manyglm(patchno_dom_mvabund ~ total + mean_area + mean_shape, 
                                   data = dom_quad_patch, family = "negative_binomial") # gives fan shape; 
## This could mean that one of our assumptions was wrong: either our mean-variance relationship was wrongly specified, 
## or our assumed relationship between our response and predictors was incorrect.
plot(patchno_dom_glm_negbin)

#anova_glm_dom_total_mean_patch <- anova.manyglm(patchno_dom_glm_negbin, show.time = "all")
#capture.output(anova_glm_dom_total_mean_patch, file = "Results/DOMINANT/anova_glm_dom_patchno.doc")

resid_glm_dom_patchno <- residuals(patchno_dom_glm_negbin)
plot(resid_glm_dom_patchno)
summary(resid_glm_dom_patchno)

anova_ind_dom_total_patchno <- anova(patchno_dom_glm_negbin, p.uni="adjusted", show.time = "all")
capture.output(anova_ind_dom_total_patchno, file = "Results/DOMINANT/patchno_anova_glm_dom_ind_TOTAL_21-08-18.doc")

######################
## area per quadrat ##

dom_quad_patch <- read.csv("Results/Patch_Stats/Mean_sum_total/dominant_sum_area_shape_quad_21-08-18.csv")
dom_quad_patch <- dom_quad_patch[,-1]
dom_quad_patch <- dom_quad_patch[-168,]

area_dom_spp <- read.csv("Results/Patch_Stats/Ordination/Area_spp/Area_matrix/Area_spp_dominant_4-08-19.csv")
area_dom_spp[is.na(area_dom_spp)] <- 0
area_dom_spp <- area_dom_spp[,-55]
area_dom_spp <- area_dom_spp[,-49] # sheep
area_dom_spp <- area_dom_spp[,-47] # ROCK
area_dom_spp <- area_dom_spp[,-35] # LITTER
area_dom_spp <- area_dom_spp[,-17] # DEAD
area_dom_spp <- area_dom_spp[,-8] # cerarve = 0
area_dom_spp <- area_dom_spp[,-5] # BARE
area_dom_spp <- area_dom_spp[,-1] # quad_no

area_dom_mvabund <- mvabund(area_dom_spp)

#boxplot(area_dom_mvabund, horizontal = TRUE, las = 2, main = "Area Cover") # boxplot
#meanvar.plot(area_dom_mvabund) # check mean variance
area_dom_glm_negbin <- manyglm(area_dom_mvabund ~ total + mean_area + mean_shape, 
                                 data = dom_quad_patch, family = "negative_binomial") # gives a curve
plot(area_dom_glm_negbin)  

anova_glm_dom_area <- anova(area_dom_glm_negbin)
capture.output(anova_glm_dom_area, file = "Results/DOMINANT/anova_glm_area_TOTAL_21-08-18.doc")

resid_glm_area <- residuals(area_dom_glm_negbin)
plot(resid_glm_area)
summary(resid_glm_area)

anova_ind_dom_total_area <- anova(area_dom_glm_negbin, p.uni="adjusted", show.time = "all") # get ANOVA results for each species vs environment variables
capture.output(anova_ind_dom_total_area, file = "Results/DOMINANT/anova_glm_area_ind_TOTAL_21-08-18.doc")

#############################
## shape index per quadrat ##

dom_quad_patch <- read.csv("Results/Patch_Stats/Mean_sum_total/dominant_sum_area_shape_quad_21-08-18.csv")
dom_quad_patch <- dom_quad_patch[,-1]
dom_quad_patch <- dom_quad_patch[-168,]

shape_dom_spp <- data.frame(read.csv("Results/Patch_Stats/Ordination/Shape_Index/shape_matrix/Shape_dom_matrix_12-08-19.csv"))
shape_dom_spp[is.na(shape_dom_spp)] <- 0
shape_dom_spp <- shape_dom_spp[,-55] # taraxacu = 0
shape_dom_spp <- shape_dom_spp[,-49] # SHEEP
shape_dom_spp <- shape_dom_spp[,-47] # ROCK
shape_dom_spp <- shape_dom_spp[,-35] # LITTER
shape_dom_spp <- shape_dom_spp[,-17] # DEAD
shape_dom_spp <- shape_dom_spp[,-5] # BARE
shape_dom_spp <- shape_dom_spp[,-1] # quad_no

shape_dom_mvabund <- mvabund(shape_dom_spp)

shape_dom_glm_binomial <- manyglm(shape_dom_mvabund ~ total + mean_area + mean_shape, 
                                  data = dom_quad_patch, family = "binomial") # might be good but data exceeds range of 1
plot(shape_dom_glm_binomial)  

anova_glm_dom_shape_bin <- anova.manyglm(shape_dom_glm_binomial, show.time = "all")
capture.output(anova_glm_dom_shape_bin, file = "Results/DOMINANT/anova_glm_dom_shape.doc")

resid_glm_dom_shape <- residuals(shape_dom_glm_binomial)
plot(resid_glm_dom_shape)
summary(resid_glm_dom_shape)

anova_ind_dom_total_shape <- anova(shape_dom_glm_binomial, p.uni="adjusted", show.time = "all")
capture.output(anova_ind_dom_total_shape, file = "Results/DOMINANT/shape_anova_glm_dom_ind_TOTAL_21-08-18.doc")

###################
### SUBDOMINANT ###
###################
###################################
## number of patches per quadrat ##
sub_quad_patch <- read.csv("Results/Patch_Stats/Mean_sum_total/subdominant_sum_area_shape_quad_21-08-18.csv")
sub_quad_patch <- sub_quad_patch[,-1]
sub_quad_patch <- sub_quad_patch[-168,]

patchno_sub_spp <- data.frame(read.csv("Results/Patch_Stats/Ordination/Patchno_spp/Patchno_matrix/Patchno_matrix_sub_12-08-19.csv"))
patchno_sub_spp[is.na(patchno_sub_spp)] <- 0
patchno_sub_spp <- patchno_sub_spp[,-61] # trifrepe = 0
patchno_sub_spp <- patchno_sub_spp[,-55] # sheep
patchno_sub_spp <- patchno_sub_spp[,-54] # rumeacetosa = 0
patchno_sub_spp <- patchno_sub_spp[,-52] # ROCK
patchno_sub_spp <- patchno_sub_spp[,-36] # LITTER
patchno_sub_spp <- patchno_sub_spp[,-6] # BARE
patchno_sub_spp <- patchno_sub_spp[,-1] # quad_no

patchno_sub_mvabund <- mvabund(patchno_sub_spp)

patchno_sub_glm_negbin <- manyglm(patchno_sub_mvabund ~ total_patch + mean_area + mean_shape, 
                                   data = sub_quad_patch, family = "negative_binomial") # gives fan shape although not bad fit
plot(patchno_sub_glm_negbin)

anova_glm_sub_patchno_negbin <- anova.manyglm(patchno_sub_glm_negbinomial, show.time = "all")
capture.output(anova_glm_sub_patchno_negbin, file = "Results/DOMINANT/anova_glm_sub_TOTAL_patchno_21-08-18.doc")
anova_glm_sub_patchno_pois <- anova.manyglm(patchno_sub_glm_poisson, show.time = "all")

resid_glm_sub_patchno <- residuals(patchno_sub_glm_negbin)
plot(resid_glm_sub_patchno)
summary(resid_glm_sub_patchno)

anova_ind_sub_total_patchno <- anova(patchno_sub_glm_negbin, p.uni="adjusted", show.time = "all")
capture.output(anova_ind_sub_total_patchno, file = "Results/SUBDOM/patchno_anova_glm_sub_ind_TOTAL_21-08-18.doc")

######################
## area per quadrat ## 
sub_quad_patch <- read.csv("Results/Patch_Stats/Mean_sum_total/subdominant_sum_area_shape_quad_21-08-18.csv")
sub_quad_patch <- sub_quad_patch[,-1]
sub_quad_patch <- sub_quad_patch[-168,]

area_sub_spp <- read.csv("Results/Patch_Stats/Ordination/Area_spp/Area_matrix/Area_spp_sub_4-08-19.csv")
area_sub_spp[is.na(area_sub_spp)] <- 0
area_sub_spp <- area_sub_spp[,-61] # Trifrepe
area_sub_spp <- area_sub_spp[,-55] # SHEEP
area_sub_spp <- area_sub_spp[,-52] # ROCK
area_sub_spp <- area_sub_spp[,-36] # LITTER
area_sub_spp <- area_sub_spp[,-17] # BARE
area_sub_spp <- area_sub_spp[,-6]
area_sub_spp <- area_sub_spp[,-1] # quad_no

area_sub_mvabund <- mvabund(area_sub_spp)

area_sub_glm_negbin <- manyglm(area_sub_mvabund ~ total_patch + mean_area + mean_shape, 
                                 data = sub_quad_patch, family = "negative_binomial") # gives a curve
plot(area_sub_glm_negbin)  

anova_glm_sub_area <- anova.manyglm(area_sub_glm_negbin)
capture.output(anova_glm_sub_area, file = "Results/SUBDOM/GLM/AREA/anova_glm_sub_area_TOTAL_21-08-18.doc")

resid_glm_sub_area <- residuals(area_sub_glm_negbin)
plot(resid_glm_sub_area)
summary(resid_glm_sub_area)

anova_ind_sub_total_area <- anova(area_sub_glm_negbin, p.uni="adjusted", show.time = "all")
capture.output(anova_ind_sub_total_area, file = "Results/SUBDOM/anova_glm_area_sub_ind_TOTAL_21-08-18.doc")

#############################
## shape index per quadrat ##

sub_quad_patch <- read.csv("Results/Patch_Stats/Mean_sum_total/subdominant_sum_area_shape_quad_21-08-18.csv")
sub_quad_patch <- sub_quad_patch[,-1]
sub_quad_patch <- sub_quad_patch[-168,]

shape_sub_spp <- data.frame(read.csv("Results/Patch_Stats/Ordination/Shape_Index/shape_matrix/Shape_sub_matrix_12-08-19.csv"))
shape_sub_spp[is.na(shape_sub_spp)] <- 0
shape_sub_spp <- shape_sub_spp[,-61] # taraxacu = 0
shape_sub_spp <- shape_sub_spp[,-55] # SHEEP
shape_sub_spp <- shape_sub_spp[,-54] # rumexacetosa = 0
shape_sub_spp <- shape_sub_spp[,-52] # ROCK
shape_sub_spp <- shape_sub_spp[,-36] # LITTER
shape_sub_spp <- shape_sub_spp[,-17] # DEAD
shape_sub_spp <- shape_sub_spp[,-6] # BARE
shape_sub_spp <- shape_sub_spp[,-1] # quad_no

shape_sub_mvabund <- mvabund(shape_sub_spp)

shape_sub_glm_binomial <- manyglm(shape_sub_mvabund ~ total_patch + mean_area + mean_shape, 
                                  data = sub_quad_patch, family = "binomial") # might be good but data exceeds range of 1
plot(shape_sub_glm_binomial)  

anova_glm_sub_shape_bin <- anova.manyglm(shape_sub_glm_binomial, show.time = "all")
capture.output(anova_glm_sub_shape_bin, file = "Results/SUBDOM/anova_glm_sub_shape.doc")

resid_glm_sub_shape <- residuals(shape_sub_glm_binomial)
plot(resid_glm_sub_shape)
summary(resid_glm_sub_shape)

anova_ind_sub_total_shape <- anova(shape_sub_glm_binomial, p.uni="adjusted", show.time = "all")
capture.output(anova_ind_sub_total_shape, file = "Results/SUBDOM/shape_anova_glm_sub_ind_total_21-08-18.doc")

################################################################################### 
##                 Univariate GLM using MEANS PER SPECIES                        ##
###################################################################################
###################
###  DOMINANT   ###
###################

#######################
## number of patches ##

dom_species_stats <- read.csv("Results/Patch_Stats/Mean_sum_total/mean_species_dom_21-08-18.csv")
rownames(dom_species_stats) <- dom_species_stats$species
dom_species_stats <- dom_species_stats[,-1]
dom_species_stats <- dom_species_stats[-49,] # should have 50 rows

patchno_dom_spp <- read.csv("Results/Patch_Stats/Ordination/Patchno_spp/Patchno_matrix/Patchno_matrix_dom_12-08-19.csv")
patchno_dom_spp[is.na(patchno_dom_spp)] <- 0
patchno_dom_spp <- patchno_dom_spp[,-55] # trifrepe
patchno_dom_spp <- patchno_dom_spp[,-49] # sheep
patchno_dom_spp <- patchno_dom_spp[,-47] # ROCK
patchno_dom_spp <- patchno_dom_spp[,-35] # LITTER
patchno_dom_spp <- patchno_dom_spp[,-17] # DEAD
patchno_dom_spp <- patchno_dom_spp[,-5] # BARE
patchno_dom_spp <- patchno_dom_spp[,-1] # quad_no
patchno_dom_spp <- t(patchno_dom_spp) # should have 50 rows

patchno_dom_mvabund <- mvabund(patchno_dom_spp)

patchno_dom_glm_negbin <- manyglm(patchno_dom_mvabund ~ patchID + area + shape.index, 
                                  data = dom_species_stats, family = "negative_binomial") 
plot(patchno_dom_glm_negbin)

#anova_glm_dom_mean_patch <- anova.manyglm(patchno_dom_glm_negbin, show.time = "all")
#capture.output(anova_glm_dom_mean_patch, file = "Results/DOMINANT/anova_glm_dom_MEAN_patchno.doc")

resid_glm_dom_patchno <- residuals(patchno_dom_glm_negbin)
plot(resid_glm_dom_patchno)
summary(resid_glm_dom_patchno)

anova_ind_dom_mean_patchno <- anova(patchno_dom_glm_negbin, p.uni="adjusted", show.time = "all")
capture.output(anova_ind_dom_mean_patchno, file = "Results/DOMINANT/patchno_anova_glm_dom_ind_MEAN_21-08-18.doc")

######################
## area per quadrat ##

dom_species_stats <- read.csv("Results/Patch_Stats/Mean_sum_total/mean_species_dom_21-08-18.csv")
rownames(dom_species_stats) <- dom_species_stats$species
dom_species_stats <- dom_species_stats[,-1]
dom_species_stats <- dom_species_stats[-49,] # should have 50 rows

area_dom_spp <- read.csv("Results/Patch_Stats/Ordination/Area_spp/Area_matrix/Area_spp_dominant_4-08-19.csv")
area_dom_spp[is.na(area_dom_spp)] <- 0
area_dom_spp <- area_dom_spp[,-55] # taraxacu
area_dom_spp <- area_dom_spp[,-49] # sheep
area_dom_spp <- area_dom_spp[,-47] # ROCK
area_dom_spp <- area_dom_spp[,-35] # LITTER
area_dom_spp <- area_dom_spp[,-17] # DEAD
area_dom_spp <- area_dom_spp[,-5] # BARE
area_dom_spp <- area_dom_spp[,-1] # quad_no
area_dom_spp <- t(area_dom_spp)

area_dom_mvabund <- mvabund(area_dom_spp)

area_dom_glm_negbin <- manyglm(area_dom_mvabund ~ patchID + area + shape.index, 
                                  data = dom_species_stats, family = "negative_binomial") 
plot(area_dom_glm_negbin)

#anova_glm_dom_mean_patch <- anova.manyglm(patchno_dom_glm_negbin, show.time = "all")
#capture.output(anova_glm_dom_mean_patch, file = "Results/DOMINANT/anova_glm_dom_MEAN_patchno.doc")

resid_glm_dom_area <- residuals(area_dom_glm_negbin)
plot(resid_glm_dom_area)
summary(resid_glm_dom_patchno)

anova_ind_dom_mean_area <- anova(area_dom_glm_negbin, p.uni="adjusted", show.time = "all")
capture.output(anova_ind_dom_mean_area, file = "Results/DOMINANT/area_anova_glm_dom_ind_MEAN_21-08-18.doc")

#############################
## shape index per quadrat ##

dom_quad_patch <- read.csv("Results/Patch_Stats/Mean_sum_total/dominant_sum_area_shape_quad_21-08-18.csv")
dom_quad_patch <- dom_quad_patch[,-1]
dom_quad_patch <- dom_quad_patch[-49,]

shape_dom_spp <- data.frame(read.csv("Results/Patch_Stats/Ordination/Shape_Index/shape_matrix/Shape_dom_matrix_12-08-19.csv"))
shape_dom_spp[is.na(shape_dom_spp)] <- 0
shape_dom_spp <- shape_dom_spp[,-55] # taraxacu = 0
shape_dom_spp <- shape_dom_spp[,-49] # SHEEP
shape_dom_spp <- shape_dom_spp[,-47] # ROCK
shape_dom_spp <- shape_dom_spp[,-35] # LITTER
shape_dom_spp <- shape_dom_spp[,-17] # DEAD
shape_dom_spp <- shape_dom_spp[,-5] # BARE
shape_dom_spp <- shape_dom_spp[,-1] # quad_no
shape_dom_spp <- t(shape_dom_spp)

shape_dom_mvabund <- mvabund(shape_dom_spp)

shape_dom_glm_bin <- manyglm(shape_dom_mvabund ~ patchID + area + shape.index, 
                               data = dom_species_stats, family = "binomial") 
plot(shape_dom_glm_negbin)

#anova_glm_dom_mean_patch <- anova.manyglm(patchno_dom_glm_negbin, show.time = "all")
#capture.output(anova_glm_dom_mean_patch, file = "Results/DOMINANT/anova_glm_dom_MEAN_patchno.doc")

resid_glm_dom_shape <- residuals(shape_dom_glm_negbin)
plot(resid_glm_dom_shape)
summary(resid_glm_dom_shape)

anova_ind_dom_mean_shape <- anova(shape_dom_glm_bin, p.uni="adjusted", show.time = "all")
capture.output(anova_ind_dom_mean_shape, file = "Results/DOMINANT/shape_anova_glm_dom_ind_MEAN_21-08-18.doc")

###################
### SUBDOMINANT ###
###################

###################################
## mean number of patches per species ##

sub_species_stats <- read.csv("Results/Patch_Stats/Mean_sum_total/mean_species_subdom_21-08-18.csv")
rownames(sub_species_stats) <- sub_species_stats$species
sub_species_stats <- sub_species_stats[,-1]

patchno_sub_spp <- data.frame(read.csv("Results/Patch_Stats/Ordination/Patchno_spp/Patchno_matrix/Patchno_matrix_sub_12-08-19.csv"))
patchno_sub_spp[is.na(patchno_sub_spp)] <- 0
patchno_sub_spp <- patchno_sub_spp[,-64] # TOTAL - added after the initial ANOVA analysis
patchno_sub_spp <- patchno_sub_spp[,-61] # trifrepe = 0
patchno_sub_spp <- patchno_sub_spp[,-55] # sheep
patchno_sub_spp <- patchno_sub_spp[,-54] # rumeacetosa = 0
patchno_sub_spp <- patchno_sub_spp[,-52] # ROCK
patchno_sub_spp <- patchno_sub_spp[,-36] # LITTER
patchno_sub_spp <- patchno_sub_spp[,-6] # BARE
patchno_sub_spp <- patchno_sub_spp[,-1] # quad_no
patchno_sub_spp <- t(patchno_sub_spp)

patchno_sub_mvabund <- mvabund(patchno_sub_spp)

patchno_sub_glm_negbin <- manyglm(patchno_sub_mvabund ~ patchID + area + shape.index, 
                             data = sub_species_stats, family = "negative_binomial") 
plot(patchno_sub_glm_negbin)

#anova_glm_sub_mean_patch <- anova.manyglm(patchno_sub_glm_negbin, show.time = "all")
#capture.output(anova_glm_sub_mean_patch, file = "Results/DOMINANT/anova_glm_sub_MEAN_patchno.doc")

resid_glm_sub_patchno <- residuals(patchno_sub_glm_negbin)
plot(resid_glm_sub_patchno)
summary(resid_glm_sub_patchno)

anova_ind_sub_mean_patchno <- anova(patchno_sub_glm_negbin, p.uni="adjusted", show.time = "all")
capture.output(anova_ind_sub_mean_patchno, file = "Results/DOMINANT/patchno_anova_glm_sub_ind_MEAN_21-08-18.doc")

#####################################
## mean area per species per patch ##
sub_species_stats <- read.csv("Results/Patch_Stats/Mean_sum_total/mean_species_subdom_21-08-18.csv")
rownames(sub_species_stats) <- sub_species_stats$species
sub_species_stats <- sub_species_stats[,-1]

area_sub_spp <- read.csv("Results/Patch_Stats/Ordination/Area_spp/Area_matrix/Area_spp_sub_4-08-19.csv")
area_sub_spp[is.na(area_sub_spp)] <- 0
area_sub_spp <- area_sub_spp[,-61] # Trifrepe
area_sub_spp <- area_sub_spp[,-55] # SHEEP
area_sub_spp <- area_sub_spp[,-52] # ROCK
area_sub_spp <- area_sub_spp[,-36] # LITTER
area_sub_spp <- area_sub_spp[,-17] # BARE
area_sub_spp <- area_sub_spp[,-6]
area_sub_spp <- area_sub_spp[,-1] # quad_no
area_sub_spp <- t(area_sub_spp)

area_sub_mvabund <- mvabund(area_sub_spp)

area_sub_glm_negbin <- manyglm(area_sub_mvabund ~ patchID + area + shape.index, 
                               data = sub_species_stats, family = "negative_binomial") 
plot(area_sub_glm_negbin)

resid_glm_sub_area <- residuals(area_sub_glm_negbin)
plot(resid_glm_sub_area)
summary(resid_glm_sub_patchno)

anova_ind_sub_mean_area <- anova(area_sub_glm_negbin, p.uni="adjusted", show.time = "all")
capture.output(anova_ind_sub_mean_area, file = "Results/SUBDOM/area_anova_glm_sub_ind_MEAN_22-08-18.doc")


############################################
## mean shape index per species per patch ##
sub_species_stats <- read.csv("Results/Patch_Stats/Mean_sum_total/mean_species_subdom_21-08-18.csv")
rownames(sub_species_stats) <- sub_species_stats$species
sub_species_stats <- sub_species_stats[,-1]

shape_sub_spp <- data.frame(read.csv("Results/Patch_Stats/Ordination/Shape_Index/shape_matrix/Shape_sub_matrix_12-08-19.csv"))
shape_sub_spp[is.na(shape_sub_spp)] <- 0
shape_sub_spp <- shape_sub_spp[,-61] # taraxacu = 0
shape_sub_spp <- shape_sub_spp[,-55] # SHEEP
shape_sub_spp <- shape_sub_spp[,-52] # ROCK
shape_sub_spp <- shape_sub_spp[,-36] # LITTER
shape_sub_spp <- shape_sub_spp[,-17] # DEAD
shape_sub_spp <- shape_sub_spp[,-6] # BARE
shape_sub_spp <- shape_sub_spp[,-1] # quad_no
shape_sub_spp <- t(shape_sub_spp)

shape_sub_mvabund <- mvabund(shape_sub_spp)

shape_sub_glm_bin <- manyglm(shape_sub_mvabund ~ patchID + area + shape.index, 
                               data = sub_species_stats, family = "binomial") 
plot(shape_sub_glm_bin)

resid_glm_sub_shape <- residuals(shape_sub_glm_bin)
plot(resid_glm_sub_shape)
summary(resid_glm_sub_patchno)

anova_ind_sub_mean_shape <- anova(shape_sub_glm_bin, p.uni="adjusted", show.time = "all")
capture.output(anova_ind_sub_mean_shape, file = "Results/SUBDOM/shape_anova_glm_sub_ind_MEAN_22-08-18.doc")

################################################################################### 
##            Is there a “predict” function in manyglm, so can you use           ##
##        the output from the model to predict the number of patches of each     ##
##                spp given a set of environmental data?                         ##
###################################################################################


################################################################################### 
##         Univariate GLM comparing total patch number, mean area                ##
##            and mean shape index per quadrat to environmental variables        ##
###################################################################################
#######
## data sets to use for total and mean for each quadrat ##
#######
dom_quad_patch <- read.csv("Results/Patch_Stats/Mean_sum_total/dominant_sum_area_shape_quad_21-08-18.csv") # per quadrat
dom_quad_patch <- dom_quad_patch[,-1]
dom_quad_patch <- dom_quad_patch[-168,]

sub_quad_patch <- read.csv("Results/Patch_Stats/Mean_sum_total/subdominant_sum_area_shape_quad_21-08-18.csv") # per quadrat
sub_quad_patch <- sub_quad_patch[,-1]
sub_quad_patch <- sub_quad_patch[-168,]

env_var <- data.frame(read.csv("Data/Biotic_data/ashtrees_env_variables_03-08-18.csv")) # environmental variables at ashtrees
env_var[is.na(env_var)] <- 0

#### dominant ####
dom_glm_quads_total <- glm(dom_quad_patch$total ~ soil_pH + slope + pct_water + Altitude, data = env_var, 
                     family = "poisson", model = TRUE)
summary(dom_glm_quads_total)
capture.output(summary(dom_glm_quads_total), file = "Results/DOMINANT/dom_glm_quads_total.doc")

dom_glm_quads_total_interaction <- glm(dom_quad_patch$total ~ soil_pH + slope + pct_water + Altitude + 
                                         soil_pH:Altitude, data = env_var, family = "poisson")
summary(dom_glm_quads_total_interaction)

dom_glm_quad_mean_area <- glm(dom_quad_patch$mean_area ~ soil_pH + slope + pct_water + Altitude, data = env_var,
                              family = "gaussian", model = TRUE)
dom_glm_quad_mean_area
summary(dom_glm_quad_mean_area)
capture.output(summary(dom_glm_quad_mean_area), file = "Results/DOMINANT/dom_glm_quads_mean_area.doc")
dom_glm_quads_area_interaction <- glm(dom_quad_patch$mean_area ~ soil_pH + slope + pct_water + Altitude + 
                                         soil_pH:Altitude, data = env_var, family = "gaussian")
summary(dom_glm_quads_area_interaction)

dom_glm_quad_mean_shape <- glm(dom_quad_patch$mean_shape ~ soil_pH + slope + pct_water + Altitude, data = env_var,
                               family = "gaussian", model = TRUE)
summary(dom_glm_quad_mean_shape)
capture.output(summary(dom_glm_quad_mean_shape), file = "Results/DOMINANT/dom_glm_quads_mean_shape.doc")
dom_glm_quads_shape_interaction <- glm(dom_quad_patch$mean_shape ~ soil_pH + slope + pct_water + Altitude + 
                                         soil_pH:Altitude, data = env_var, family = "gaussian")
summary(dom_glm_quads_shape_interaction)

#### subdominant ####
sub_glm_quads_total <- glm(sub_quad_patch$total_patch ~ soil_pH + slope + pct_water + Altitude, data = env_var, 
                           family = "poisson", model = TRUE)
summary(sub_glm_quads_total)
capture.output(summary(sub_glm_quads_total), file = "Results/SUBDOM/sub_glm_quads_total.doc")
sub_glm_quads_total_interaction <- glm(sub_quad_patch$total_patch ~ soil_pH + slope + pct_water + Altitude + 
                                         soil_pH:Altitude, data = env_var, family = "poisson")
summary(sub_glm_quads_total_interaction)

sub_glm_quad_mean_area <- glm(sub_quad_patch$mean_area ~ soil_pH + slope + pct_water + Altitude, data = env_var,
                              family = "gaussian", model = TRUE)
summary(sub_glm_quad_mean_area)
capture.output(summary(sub_glm_quad_mean_area), file = "Results/SUBDOM/sub_glm_quads_mean_area.doc")
sub_glm_quads_area_interaction <- glm(sub_quad_patch$mean_area ~ soil_pH + slope + pct_water + Altitude + 
                                         soil_pH:Altitude, data = env_var, family = "gaussian")
summary(sub_glm_quads_area_interaction)

sub_glm_quad_mean_shape <- glm(sub_quad_patch$mean_shape ~ soil_pH + slope + pct_water + Altitude, data = env_var,
                               family = "gaussian", model = TRUE)
summary(sub_glm_quad_mean_shape)
capture.output(summary(sub_glm_quad_mean_shape), file = "Results/SUBDOM/sub_glm_quads_mean_shape.doc")
sub_glm_quads_shape_interaction <- glm(sub_quad_patch$mean_shape ~ soil_pH + slope + pct_water + Altitude + 
                                        soil_pH:Altitude, data = env_var, family = "gaussian")
summary(sub_glm_quads_shape_interaction)

##############################################
#### Univariate analysis for sheep tracks ####
##############################################

## SHEEP TRACKS ENVIRONMENTAL DATA ##
sheep_data <- read.csv("Data/Biotic_data/ashtrees_env_variables_14-01-19.csv")
sheep_data$length_10m[is.na(sheep_data$length_10m)] <- 0
sheep_data$length_25m[is.na(sheep_data$length_25m)] <- 0
sheep_data$length_35m[is.na(sheep_data$length_35m)] <- 0

### DOMINANT univariate analysis ####
## DOM: NUMBER OF PATCHES for sheep and ditch##

dom_quad_patch <- read.csv("Results/Patch_Stats/Mean_sum_total/dominant_sum_area_shape_quad_21-08-18.csv") # total number of patches, mean area and mean shape index per quadrat
dom_quad_patch <- dom_quad_patch[,-1]
dom_quad_patch <- dom_quad_patch[-168,]

dom_sheep_ditch_patch <- glm(dom_quad_patch$total ~ length_35m + distance_35m + ditch_distance + 
                             length_10m:distance_10m + length_25m:distance_25m + length_35m:distance_35m , data = sheep_data, 
                           family = "poisson", model = TRUE) ## NUMBER OF PATCHES
summary(dom_sheep_ditch_patch)
capture.output(summary(dom_sheep_ditch_patch), file = "Results/DOMINANT/dom_sheep_ditch_patch.csv") 


## DOM: AREA for sheep and ditch ####
dom_10m_sheep_area <- glm(dom_quad_patch$mean_area ~ length_10m + distance_10m + length_25m + distance_25m +length_35m 
                          + distance_35m + ditch_distance + length_10m:distance_10m + length_25m:distance_25m + length_35m:distance_35m, data = sheep_data,
                          family = "gaussian", model = TRUE) ## MEAN AREA

dom_10m_sheep_area
summary(dom_10m_sheep_area)
capture.output(summary(dom_10m_sheep_area), file = "Results/DOMINANT/dom_sheep_ditch_area.csv") 

## SHAPE INDEX for sheep and ditch ####

dom_sheep_ditch_shape <- anova.glm(dom_quad_patch$mean_shape ~ length_10m + distance_10m + length_25m + distance_25m +length_35m 
                             + distance_35m + ditch_distance + length_10m:distance_10m + length_25m:distance_25m + length_35m:distance_35m, data = sheep_data,
                               family = "gaussian", model = TRUE) ## MEAN SHAPE INDEX
summary(dom_sheep_ditch_shape)
capture.output(summary(dom_sheep_ditch_shape), file = "Results/DOMINANT/dom_sheep_ditch_shape.csv")


#### SUBDOMINANT univariate analysis ####
sub_quad_patch <- read.csv("Results/Patch_Stats/Mean_sum_total/subdominant_sum_area_shape_quad_21-08-18.csv") # per quadrat
sub_quad_patch <- sub_quad_patch[,-1]
sub_quad_patch <- sub_quad_patch[-168,]

sub_sheep_ditch_patch <- glm(sub_quad_patch$total_patch ~ length_10m + distance_10m + length_25m + distance_25m +length_35m 
                             + distance_35m + ditch_distance + length_10m:distance_10m + length_25m:distance_25m + length_35m:distance_35m, data = sheep_data, 
                           family = "poisson", model = TRUE) ## NUMBER OF PATCHES
summary(sub_sheep_ditch_patch)
capture.output(summary(sub_sheep_ditch_patch), file = "Results/SUBDOM/sub_sheep_ditch_patch.csv") 

# 25m buffer
sub_25m_sheep_patch <- glm(sub_quad_patch$total_patch ~ length_25m + distance_25m, data = sheep_data, 
                           family = "poisson", model = TRUE) ## NUMBER OF PATCHES
summary(sub_25m_sheep_patch)
capture.output(summary(dom_25m_sheep_patch), file = "Results/SUBDOM/sub_25m_sheep_patch.doc") 

#### SUB AREA sheep and ditch ####

sub_sheep_ditch_area <- glm(sub_quad_patch$mean_area ~ length_10m + distance_10m + length_25m + distance_25m +length_35m 
                            + distance_35m + ditch_distance + length_10m:distance_10m + length_25m:distance_25m + length_35m:distance_35m, data = sheep_data,
                          family = "gaussian", model = TRUE) ## MEAN AREA
sub_sheep_ditch_area
summary(sub_sheep_ditch_area)
capture.output(summary(sub_sheep_ditch_area), file = "Results/SUBDOM/sub_sheep_ditch_area.csv") 

#### SUB SHAPE INDEX for sheep and ditches####

sub_sheep_ditch_shape <- glm(sub_quad_patch$mean_shape ~ length_10m + distance_10m + length_25m + distance_25m +length_35m 
                           + distance_35m + ditch_distance + length_10m:distance_10m + length_25m:distance_25m + length_35m:distance_35m, data = sheep_data,
                           family = "gaussian", model = TRUE) ## MEAN SHAPE INDEX
summary(sub_sheep_ditch_shape)
capture.output(summary(sub_sheep_ditch_shape), file = "Results/SUBDOM/sub_sheep_ditch_shape.csv")

temp <- anova(sub_sheep_ditch_shape)
/summary(sub_25m_sheep_shape)
capture.output(summary(sub_25m_sheep_shape), file = "Results/SUBDOM/sub_25m_sheep_shape.doc")


##############################################
## multivariate analysis for sheep tracks ####
##############################################

#########################################################
###  DOMINANT AREA multivariate sheep + ditches ########
#########################################################
## AREA ##

area_dom_spp <- read.csv("Results/Patch_Stats/Ordination/Area_spp/Area_matrix/Area_spp_dominant_4-08-19.csv")
area_dom_spp[is.na(area_dom_spp)] <- 0
area_dom_spp <- area_dom_spp[,-55]
area_dom_spp <- area_dom_spp[,-49] # sheep
area_dom_spp <- area_dom_spp[,-47] # ROCK
area_dom_spp <- area_dom_spp[,-35] # LITTER
area_dom_spp <- area_dom_spp[,-17] # DEAD
area_dom_spp <- area_dom_spp[,-8] # cerarve = 0
area_dom_spp <- area_dom_spp[,-5] # BARE
area_dom_spp <- area_dom_spp[,-1] # quad_no
env_var <- data.frame(read.csv("Data/Biotic_data/ashtrees_env_variables_14-01-19.csv"))
env_var$length_10m[is.na(env_var$length_10m)] <- 0
env_var$length_25m[is.na(env_var$length_25m)] <- 0
env_var$length_25m[is.na(env_var$length_35m)] <- 0

area_dom_mvabund <- mvabund(area_dom_spp)
plot.mvformula(log(area_dom_mvabund+1) ~ exp(env_var$length_10m), main="Dominant Area vs Soil pH",
               xlab="% Soil pH - Log Scale ", ylab="AREA[log scale]",
               overall.main="Species area DOM vs soil pH",
               fg="grey", las=1, scale.lab="ss",t.lab="o", mfrow=c(4,3),log="x") # Produces a range of plots for 
#visualising multivariate abundance data and its relationship to environmental
#variables

#boxplot(area_dom_mvabund, horizontal = TRUE, las = 2, main = "Area Cover") # boxplot
meanvar.plot(area_dom_mvabund) # check mean variance
area_dom_sheep <- manyglm(area_dom_mvabund ~ length_10m + length_25m + distance_10m + 
                            distance_25m + length_35m + distance_35m + 
                            length_10m:distance_10m + length_25m:distance_25m + 
                            length_35m:distance_35m + ditch_distance, 
                                    data = env_var, family = "negative.binomial") # does not give a particular shape; will use this
area_dom_sheep <- manyglm(area_dom_mvabund ~ length_35m + distance_35m + 
                            length_35m:distance_35m + ditch_distance, 
                          data = env_var, family = "negative.binomial") # does not give a particular shape; will use this

plot(area_dom_sheep)

# summary(area_dom_glm_negbinomial, resamp="monte.carlo", test="wald", nBoot=300) # summary method for class "manyglm"
## NEED TO CHECK RE. SUMMARY FOR RESAMPLING AND BOOTSTRAPPING?

#area_dom_lm_sheep <- manylm(area_dom_mvabund ~ length_10m + length_25m + distance_10m + distance_25m, 
#                            data = env_var, family = "Gaussian") # cannot fit glm with Gaussian
#plot(area_dom_glm_sheep)

anova_dom_sheep <- anova(area_dom_sheep, show.time = "all")
capture.output(anova_dom_10m_sheep, file = "Results/DOMINANT/GLM/AREA/anova_glm_area_sheep.doc")
#capture.output(anova(area_dom_glm_negbinomial, by = "axis"), file = "Results/DOMINANT/anova_axis_area.doc") # separate tests on RDA1 and RDA2
#capture.output(anova(area_dom_glm_negbinomial, by= "terms"), file = "Results/DOMINANT/anova_terms_area.doc") # each predictor separately
#capture.output(anova(area_dom_glm_negbinomial, by= "margin"), file = "Results/DOMINANT/anova_margin_area.doc") # each predictor taking into account collinearities; usually less significant

resid_glm_area <- residuals(area_dom_glm_negbinomial)
plot(resid_glm_area)
summary(resid_glm_area)

ind_dom_sheep_area <- anova(area_dom_sheep, p.uni="adjusted", show.time = "all")
capture.output(ind_dom_sheep_area, file = "Results/DOMINANT/anova_area_sheep_ditches_ind.csv")

##### Plot linear models for environmental variables against dominant area #####

plot(area_dom_spp, type = "n", main = "% cover nMDS")

#########################################################
### SUBDOMINANT AREA multivariate sheep + ditches #####
#########################################################
## AREA ##
area_sub_spp <- read.csv("Results/Patch_Stats/Ordination/Area_spp/Area_matrix/Area_spp_sub_4-08-19.csv")
area_sub_spp[is.na(area_sub_spp)] <- 0
area_sub_spp <- area_sub_spp[,-61] # Trifrepe
area_sub_spp <- area_sub_spp[,-55] # SHEEP
area_sub_spp <- area_sub_spp[,-52] # ROCK
area_sub_spp <- area_sub_spp[,-36] # LITTER
area_sub_spp <- area_sub_spp[,-17] # BARE
area_sub_spp <- area_sub_spp[,-6]
area_sub_spp <- area_sub_spp[,-1] # quad_no
#env_var <- data.frame(read.csv("Data/Biotic_data/ashtrees_env_variables_03-08-18.csv"))
env_var$length_10m[is.na(env_var$length_10m)] <- 0
env_var$length_25m[is.na(env_var$length_25m)] <- 0
env_var$length_35m[is.na(env_var$length_35m)] <- 0
area_sub_mvabund <- mvabund(area_sub_spp)
plot.mvformula(log(area_sub_mvabund+1) ~ exp(env_var$soil_pH), main="SUB Area vs Soil pH",
               xlab="% Soil pH - Log Scale ", ylab="Abundance [log scale]",
               overall.main="Species area SUB vs soil pH",
               fg="grey", las=1, scale.lab="ss",t.lab="o", mfrow=c(4,3),log="x") # Produces a range of plots for 
#visualising multivariate abundance data and its relationship to environmental
#variables

boxplot(area_sub_mvabund, horizontal = TRUE, las = 2, main = "Area Cover") # boxplot
meanvar.plot(area_sub_mvabund) # check mean variance
#area_sub_glm_poisson <- manyglm(area_sub_mvabund ~ soil_pH + slope + pct_water + Altitude, data = env_var, family = "poisson") # gives fan shape; 
## This could mean that one of our assumptions was wrong: either our mean-variance relationship was wrongly specified, 
## or our assumed relationship between our response and predictors was incorrect.
#plot(area_sub_glm_poisson)

#area_sub_glm_binomial <- manyglm(area_sub_mvabund ~ soil_pH + slope + pct_water + Altitude, 
#                                 data = env_var, family = "binomial") # gives a curve
#plot(area_sub_glm_binomial)  
area_sub_sheep <- manyglm(area_sub_mvabund ~ length_10m + length_25m + distance_10m + 
                            distance_25m +  + length_35 + distance_35m + 
                            length_10m:distance_10m + length_25m:distance_25m + 
                            length_35m:distance_35m + ditch_distance, 
                                    data = env_var, family = "negative.binomial") # does not give a particular shape; will use this
area_sub_sheep <- manyglm(area_sub_mvabund ~ length_35m + distance_35m + 
                            length_35m:distance_35m + ditch_distance, 
                          data = env_var, family = "negative.binomial") # does not give a particular shape; will use this

plot(area_sub_sheep)
#abline(area_sub_sheep)
#summary(area_sub_sheep, resamp="monte.carlo", test="wald", nBoot=300) # summary method for class "manyglm"
## NEED TO CHECK RE. SUMMARY FOR RESAMPLING AND BOOTSTRAPPING?

anova_glm_sub_area <- anova.manyglm(area_sub_sheep)
capture.output(anova_glm_sub_area, file = "Results/SUBDOM/GLM/AREA/anova_area_sheep_ditches.csv")

resid_glm_sub_area <- residuals(anova_glm_sub_area)
plot(resid_glm_sub_area)
summary(resid_glm_sub_area)

anova_ind_sub_area <- anova(area_sub_sheep, p.uni="adjusted", show.time = "all")
capture.output(anova_ind_sub_area, file = "Results/SUBDOM/anova_area_sheep_ditches_ind.csv")

################################################################################### 
####    DOMINANT PATCHES: multivariate analysis sheep + ditches      ####
###################################################################################

patchno_dom_spp <- read.csv("Results/Patch_Stats/Ordination/Patchno_spp/Patchno_matrix/Patchno_matrix_dom_12-08-19.csv")
patchno_dom_spp[is.na(patchno_dom_spp)] <- 0
patchno_dom_spp <- patchno_dom_spp[,-55] # trifrepe
patchno_dom_spp <- patchno_dom_spp[,-49] # sheep
patchno_dom_spp <- patchno_dom_spp[,-47] # ROCK
patchno_dom_spp <- patchno_dom_spp[,-35] # LITTER
patchno_dom_spp <- patchno_dom_spp[,-17] # DEAD
patchno_dom_spp <- patchno_dom_spp[,-8] # cerarve = 0
patchno_dom_spp <- patchno_dom_spp[,-5] # BARE
patchno_dom_spp <- patchno_dom_spp[,-1] # quad_no
env_var <- data.frame(read.csv("Data/Biotic_data/ashtrees_env_variables_14-01-19.csv"))
env_var$length_10m[is.na(env_var$length_10m)] <- 0
env_var$length_25m[is.na(env_var$length_25m)] <- 0
env_var$length_35m[is.na(env_var$length_35m)] <- 0
patchno_dom_mvabund <- mvabund(patchno_dom_spp)
plot.mvformula(log(patchno_dom_mvabund+1) ~ exp(env_var$soil_pH), main="Dominant Patchno vs Soil pH",
               xlab="% Soil pH - Log Scale ", ylab="Abundance [log scale]",
               overall.main="Species Abundance vs soil pH",
               fg="grey", las=1, scale.lab="ss",t.lab="o", mfrow=c(4,3),log="x") # Produces a range of plots for 
#visualising multivariate abundance data and its relationship to environmental
#variables

boxplot(patchno_dom_mvabund, horizontal = TRUE, las = 2, main = "Number of Patches") # boxplot
meanvar.plot(patchno_dom_mvabund) # check mean variance

patchno_dom_sheep<- manyglm(patchno_dom_mvabund ~ length_10m + length_25m + distance_10m + 
                              distance_25m + length_35m + distance_35m +  
                              length_10m:distance_10m + length_25m:distance_25m + 
                              length_35m:distance_35m + ditch_distance,
                            data = env_var, family = "poisson") # gives fan shape; 
## This could mean that one of our assumptions was wrong: either our mean-variance relationship was wrongly specified, 
## or our assumed relationship between our response and predictors was incorrect.
patchno_dom_sheep <- manyglm(patchno_dom_mvabund ~ length_35m + distance_35m + 
                            length_35m:distance_35m + ditch_distance, 
                          data = env_var, family = "negative.binomial") # does not give a particular shape; will use this

plot(patchno_dom_sheep)

anova_patchno_dom_sheep <- anova.manyglm(patchno_dom_sheep, show.time = "all") # summary of species vs species (i.e. species used as variables)
capture.output(anova_patchno_dom_spp, file = "Results/DOMINANT/GLM/patchno/anova_patchno_sheep.doc")

ind_dom_patchno_sheep <- anova(patchno_dom_sheep, p.uni="adjusted", show.time = "all") # try to get anova results for each species with each species in each quadrat
capture.output(ind_dom_patchno_sheep, file = "Results/DOMINANT/anova_patchno_sheep_ditches.csv")

#area_sub_glm_binomial <- manyglm(area_sub_mvabund ~ soil_pH + slope + pct_water + Altitude, 
#                                 data = env_var, family = "binomial") # gives a curve
#plot(area_sub_glm_binomial)  

#######################

getOption("max.print")
options(max.print = 1000000000)

##################################################
### SUBDOMINANT PATCHES multivariate analysis ####
##################################################
patchno_sub_spp <- data.frame(read.csv("Results/Patch_Stats/Ordination/Patchno_spp/Patchno_matrix/Patchno_matrix_sub_12-08-19.csv"))
patchno_sub_spp[is.na(patchno_sub_spp)] <- 0
patchno_sub_spp <- patchno_sub_spp[,-61] # trifrepe = 0
patchno_sub_spp <- patchno_sub_spp[,-55] # sheep
patchno_sub_spp <- patchno_sub_spp[,-54] # rumeacetosa = 0
patchno_sub_spp <- patchno_sub_spp[,-52] # ROCK
patchno_sub_spp <- patchno_sub_spp[,-36] # LITTER
patchno_sub_spp <- patchno_sub_spp[,-6] # BARE
patchno_sub_spp <- patchno_sub_spp[,-1] # quad_no
env_var <- data.frame(read.csv("Data/Biotic_data/ashtrees_env_variables_03-08-18.csv"))
env_var$length_10m[is.na(env_var$length_10m)] <- 0
env_var$length_25m[is.na(env_var$length_25m)] <- 0

patchno_sub_mvabund <- mvabund(patchno_sub_spp)
plot.mvformula(log(patchno_sub_mvabund+1) ~ exp(env_var$soil_pH), main="Dominant Patchno vs Soil pH",
               xlab="% Soil pH - Log Scale ", ylab="Abundance [log scale]",
               overall.main="Species Abundance vs soil pH",
               fg="grey", las=1, scale.lab="ss",t.lab="o", mfrow=c(4,3),log="x") # Produces a range of plots for 
#visualising multivariate abundance data and its relationship to environmental
#variables

boxplot(patchno_sub_mvabund, horizontal = TRUE, las = 2, main = "Number of Patches: SUB") # boxplot
meanvar.plot(patchno_sub_mvabund) # check mean variance

patchno_sub_sheep <- manyglm(patchno_sub_mvabund ~ length_10m + length_25m + distance_10m + 
                               distance_25m + length_35m + distance_35m + 
                               length_10m:distance_10m + length_25m:distance_25m + 
                               length_35m:distance_35m + ditch_distance,
                             data = env_var, family = "negative.binomial")
## This could mean that one of our assumptions was wrong: either our mean-variance relationship was wrongly specified, 
## or our assumed relationship between our response and predictors was incorrect.
patchno_sub_sheep <- manyglm(patchno_sub_mvabund ~ length_35m + distance_35m + 
                               length_35m:distance_35m + ditch_distance,
                             data = env_var, family = "negative.binomial")

plot(patchno_sub_sheep)

anova_patchno_sub_sheep <- anova.manyglm(patchno_sub_sheep, show.time = "all")
capture.output(anova_patchno_sub_sheep, file = "Results/SUBDOM/patchno_sub_sheep.doc")

resid_glm_sub_patchno <- residuals(patchno_sub_sheep)
plot(resid_glm_sub_patchno)
summary(resid_glm_sub_patchno)

ind_sub_patchno <- anova(patchno_sub_sheep, p.uni="adjusted", show.time = "all")
capture.output(ind_sub_patchno, file = "Results/SUBDOM/anova_patchno_sub_sheep_ditches.csv")

################################################################################### 
##        DOMINANT SHAPE INDEX multivariate analysis sheep + ditches    ####
###################################################################################
## 3. ## 
## Similarly for “within-patch calculated % cover area” (not the Ashtrees % cover; 
## a bit confusing I know!) and the same predictors.

shape_dom_spp <- data.frame(read.csv("Results/Patch_Stats/Ordination/Shape_Index/shape_matrix/Shape_dom_matrix_12-08-19.csv"))
shape_dom_spp[is.na(shape_dom_spp)] <- 0
shape_dom_spp <- shape_dom_spp[,-55] # taraxacu = 0
shape_dom_spp <- shape_dom_spp[,-49] # SHEEP
shape_dom_spp <- shape_dom_spp[,-47] # ROCK
shape_dom_spp <- shape_dom_spp[,-35] # LITTER
shape_dom_spp <- shape_dom_spp[,-17] # DEAD
shape_dom_spp <- shape_dom_spp[,-5] # BARE
shape_dom_spp <- shape_dom_spp[,-1] # quad_no
#env_var <- data.frame(read.csv("Data/Biotic_data/ashtrees_env_variables_03-08-18.csv"))
env_var <- read.csv("Data/Biotic_data/ashtrees_env_variables_14-01-19.csv")
env_var[is.na(env_var)] <- 0
env_var$length_35m[is.na(env_var$length_35m)] <- 0
shape_dom_mvabund <- mvabund(shape_dom_spp)
plot.mvformula(log(shape_dom_mvabund+1) ~ exp(env_var$soil_pH), main="Dominant Patchno vs Soil pH",
               xlab="% Soil pH - Log Scale ", ylab="Abundance [log scale]",
               overall.main="Species Abundance vs soil pH",
               fg="grey", las=1, scale.lab="ss",t.lab="o", mfrow=c(4,3),log="x") # Produces a range of plots for 
#visualising multivariate abundance data and its relationship to environmental
#variables

boxplot(shape_dom_mvabund, horizontal = TRUE, las = 2, main = "Shape Index: DOM") # boxplot
meanvar.plot(shape_dom_mvabund) # check mean variance


shape_dom_sheep <- manyglm(shape_dom_mvabund ~ length_10m + distance_10m + length_25m + 
                             distance_25m + length_35m + distance_35m + 
                             length_10m:distance_10m + length_25m:distance_25m + 
                             length_35m:distance_35m + ditch_distance, 
                                  data = env_var, family = "negative.binomial") # might be good but data exceeds range of 1
shape_dom_sheep <- manyglm(shape_dom_mvabund ~ length_35m + distance_35m + 
                             length_35m:distance_35m + ditch_distance, 
                           data = env_var, family = "negative.binomial") # might be good but data exceeds range of 1

plot(shape_dom_sheep)  

anova_shape_sheep <- anova.manyglm(shape_dom_sheep, show.time = "all")
capture.output(anova_glm_dom_shape_bin, file = "Results/DOMINANT/GLM/Shape_Index/anova_glm_dom_shape.doc")

resid_glm_dom_shape <- residuals(shape_dom_glm_binomial)
plot(resid_glm_dom_shape)
summary(resid_glm_dom_shape)

ind_dom_shape_sheep <- anova(shape_dom_sheep, p.uni="adjusted", show.time = "all")
capture.output(ind_dom_shape_sheep, file = "Results/DOMINANT/shape_dom_sheep_ditches.csv")

############################################################################
### SUBOMINANT SHAPE INDEX multivariate analysis sheep + ditches ####
###########################################################################

shape_sub_spp <- data.frame(read.csv("Results/Patch_Stats/Ordination/Shape_Index/shape_matrix/Shape_sub_matrix_12-08-19.csv"))
shape_sub_spp[is.na(shape_sub_spp)] <- 0
shape_sub_spp <- shape_sub_spp[,-61] # taraxacu = 0
shape_sub_spp <- shape_sub_spp[,-55] # SHEEP
shape_sub_spp <- shape_sub_spp[,-54] # rumexacetosa = 0
shape_sub_spp <- shape_sub_spp[,-52] # ROCK
shape_sub_spp <- shape_sub_spp[,-36] # LITTER
shape_sub_spp <- shape_sub_spp[,-17] # DEAD
shape_sub_spp <- shape_sub_spp[,-6] # BARE
shape_sub_spp <- shape_sub_spp[,-1] # quad_no
#env_var <- data.frame(read.csv("Data/Biotic_data/ashtrees_env_variables_03-08-18.csv"))
#env_var[is.na(env_var)] <- 0
env_var <- read.csv("Data/Biotic_data/ashtrees_env_variables_14-01-19.csv")
env_var[is.na(env_var)] <- 0
env_var$length_35m[is.na(env_var$length_35m)] <- 0
shape_sub_mvabund <- mvabund(shape_sub_spp)
plot.mvformula(log(shape_sub_mvabund+1) ~ exp(env_var$soil_pH), main="Dominant Patchno vs Soil pH",
               xlab="% Soil pH - Log Scale ", ylab="Abundance [log scale]",
               overall.main="Species Abundance vs soil pH",
               fg="grey", las=1, scale.lab="ss",t.lab="o", mfrow=c(4,3),log="x") # Produces a range of plots for 
#visualising multivariate abundance data and its relationship to environmental
#variables

boxplot(shape_sub_mvabund, horizontal = TRUE, las = 2, main = "Shape Index: DOM") # boxplot
meanvar.plot(shape_sub_mvabund) # check mean variance


shape_sub_sheep <- manyglm(shape_sub_mvabund ~ length_10m + distance_10m + length_25m + 
                             distance_25m + length_35m + distance_35m + 
                             length_10m:distance_10m + length_25m:distance_25m + 
                             length_35m:distance_35m + ditch_distance, 
                                  data = env_var, family = "negative.binomial") # might be good but data exceeds range of 1
shape_sub_sheep <- manyglm(shape_sub_mvabund ~ length_35m + distance_35m + 
                             length_35m:distance_35m + ditch_distance, 
                           data = env_var, family = "negative.binomial") # might be good but data exceeds range of 1
plot(shape_sub_sheep)  

anova_sub_shape_sheep <- anova.manyglm(shape_sub_sheep, show.time = "all")
capture.output(anova_glm_sub_shape_bin, file = "Results/SUBDOM/GLM/Shape_Index/anova_sub_shape_sheep.doc")

resid_glm_sub_shape <- residuals(shape_sub_glm_binomial)
plot(resid_glm_sub_shape)
summary(resid_glm_sub_shape)

ind_sub_shape <- anova(shape_sub_sheep, p.uni="adjusted", show.time = "all")
capture.output(ind_sub_shape, file = "Results/SUBDOM/shape_sub_sheep_ditches.csv")


##########################################################################
#### Plot linear models for environmental variables + sheep + ditches ####
#########################################################################
## Dominant - univariate ##
dom_quad_patch <- read.csv("Results/Patch_Stats/Mean_sum_total/dominant_sum_area_shape_quad_21-08-18.csv") # per quadrat
dom_quad_patch <- dom_quad_patch[,-1]
dom_quad_patch <- dom_quad_patch[-168,]
env_var <- read.csv("Data/Biotic_data/ashtrees_env_variables_14-01-19.csv")

#### DOM total number of patches vs env. var ####
dom_total_length_25 <- lm(dom_quad_patch$total ~ env_var$length_25m)
dom_total_length_35 <- lm(dom_quad_patch$total ~ env_var$length_35m)
plot(dom_quad_patch$total ~ env_var$length_35m, type = "n", xlab = "Sheep track length", ylab = "Number of patches per quadrat")
points(dom_quad_patch$total ~ env_var$length_25m, pch = 5, col = "red", cex = 0.5)
abline(dom_total_length_25, col = "red")
points(dom_quad_patch$total ~ env_var$length_35, pch = 16, col = "black", cex = 0.5)
abline(dom_total_length_35)

dom_total_distance_35 <- lm(dom_quad_patch$total ~ env_var$distance_35m)

## sheep track distance ##
plot(dom_quad_patch$total ~ env_var$distance_35m, type = "n", xlab = "Distance to sheep track", ylab = "Number of patches/quadrat")
points(dom_quad_patch$total ~ env_var$distance_35m, pch = 16, col = "black", cex = 0.5)
points(sub_quad_patch$total_patch ~ env_var$distance_35m, pch = 16, col = "red", cex = 0.5)
abline(dom_total_distance_35, col = "black")
abline(sub_distance_35, col = "red")


dom_soil_p <- lm(dom_quad_patch$total ~ env_var$soil_pH)
dom_alt_p <- lm(dom_quad_patch$total ~ env_var$Altitude)
dom_slope_p <- lm(dom_quad_patch$total ~ env_var$slope)
dom_slope_p <- lm(dom_quad_patch$total ~ env_var$slope)
dom_water_p <- lm(dom_quad_patch$total ~ env_var$pct_water)
dom_ditch_p <- lm(dom_quad_patch$total ~ env_var$ditch_distance)

## Patches ####
plot(dom_quad_patch$total ~ env_var$soil_pH, type = "n", xlab = "Soil pH", ylab = "Number of patches/quadrat")
points(dom_quad_patch$total ~ env_var$soil_pH, pch = 16, col = "black", cex = 0.5)
points(sub_quad_patch$total_patch ~ env_var$soil_pH, pch = 16, col = "red", cex = 0.5)
abline(dom_soil_p, col = "black")
abline(sub_soil_p, col = "red")

## Altitude_total_p ####
plot(dom_quad_patch$total ~ env_var$Altitude, type = "n", xlab = "Altitude", ylab = "Number of patches/quadrat")
points(dom_quad_patch$total ~ env_var$Altitude, pch = 16, col = "black", cex = 0.5)
points(sub_quad_patch$total_patch ~ env_var$Altitude, pch = 16, col = "red", cex = 0.5)
abline(dom_alt_p, col = "black")
abline(sub_alt_p, col = "red")

## Slope_total_p ####
plot(dom_quad_patch$total ~ env_var$slope, type = "n", xlab = "slope", ylab = "Number of patches/quadrat")
points(dom_quad_patch$total ~ env_var$slope, pch = 16, col = "black", cex = 0.5)
points(sub_quad_patch$total_patch ~ env_var$slope, pch = 16, col = "red", cex = 0.5)
abline(dom_slope_p, col = "black")
abline(sub_slope_p, col = "red")

## % water_total_p ####
plot(dom_quad_patch$total ~ env_var$pct_water, type = "n", xlab = "% water content", ylab = "Number of patches/quadrat")
points(dom_quad_patch$total ~ env_var$pct_water, pch = 16, col = "black", cex = 0.5)
points(sub_quad_patch$total_patch ~ env_var$pct_water, pch = 16, col = "red", cex = 0.5)
abline(dom_ditch_p, col = "black")
abline(sub_ditch_p, col = "red")

## Distance to ditches_total_p ####
plot(dom_quad_patch$total ~ env_var$ditch_distance, type = "n", xlab = "Distance to ditch", ylab = "Number of patches/quadrat")
points(dom_quad_patch$total ~ env_var$ditch_distance, pch = 16, col = "black", cex = 0.5)
points(sub_quad_patch$total_patch ~ env_var$pct_water, pch = 16, col = "red", cex = 0.5)
abline(dom_water_p, col = "black")
abline(sub_water_p, col = "red")


#### DOM Mean area vs. env var. ####
dom_area_length_25 <- lm(dom_quad_patch$mean_area ~ env_var$length_25m)
dom_area_length_35 <- lm(dom_quad_patch$mean_area ~ env_var$length_35m)
plot(dom_quad_patch$mean_area ~ env_var$length_35m, type = "n", xlab = "Sheep track length", ylab = "Mean area per quadrat")
points(dom_quad_patch$mean_area ~ env_var$length_25m, pch = 5, col = "black", cex = 0.5)
abline(dom_area_length_25, col = "red")
points(dom_quad_patch$mean_area ~ env_var$length_35, pch = 16, col = "black", cex = 0.5)
abline(dom_area_length_35, col = "black")

## distance to track
dom_distance_35_a <- lm(dom_quad_patch$mean_area ~ env_var$distance_35m)
sub_distance_35_a <- lm(sub_quad_patch$mean_area ~ env_var$distance_35m)

plot(dom_quad_patch$mean_area ~ env_var$distance_35m, type = "n", xlab = "Sheep track length", ylab = "Mean area per quadrat")
points(dom_quad_patch$mean_area ~ env_var$distance_35m, pch = 16, col = "black", cex = 0.5)
points(sub_quad_patch$mean_area ~ env_var$distance_35m, pch = 16, col = "red", cex = 0.5)
abline(sub_distance_35_a, col = "black")
abline(sub_distance_35_a, col = "red")


dom_soil_a <- lm(dom_quad_patch$mean_area ~ env_var$soil_pH)
dom_alt_a <- lm(dom_quad_patch$mean_area ~ env_var$Altitude)
dom_slope_a <- lm(dom_quad_patch$mean_area ~ env_var$slope)
dom_slope_a <- lm(dom_quad_patch$mean_area ~ env_var$slope)
dom_water_a <- lm(dom_quad_patch$mean_area ~ env_var$pct_water)
dom_ditch_a <- lm(dom_quad_patch$mean_area ~ env_var$ditch_distance)

# Soil_mean_a ####
plot(dom_quad_patch$mean_area ~ env_var$soil_pH, type = "n", xlab = "Soil pH", ylab = "Mean area per patch")
points(dom_quad_patch$mean_area ~ env_var$soil_pH, pch = 16, col = "black", cex = 0.5)
points(sub_quad_patch$mean_area ~ env_var$soil_pH, pch = 16, col = "red", cex = 0.5)
abline(dom_soil_a, col = "black")
abline(sub_soil_a, col = "red")

# slope_mean_a ####
plot(dom_quad_patch$mean_area ~ env_var$slope, type = "n", xlab = "Slope", ylab = "Number of patches/quadrat")
points(dom_quad_patch$mean_area ~ env_var$slope, pch = 16, col = "black", cex = 0.5)
points(sub_quad_patch$mean_area ~ env_var$slope, pch = 16, col = "red", cex = 0.5)
abline(dom_slope_a, col = "black")
abline(sub_slope_a, col = "red")

# Altitude_mean_a ####

plot(dom_quad_patch$mean_area ~ env_var$Altitude, type = "n", xlab = "Soil pH", ylab = "Mean area per patch")
points(dom_quad_patch$mean_area ~ env_var$Altitude, pch = 16, col = "black", cex = 0.5)
points(sub_quad_patch$mean_area ~ env_var$Altitude, pch = 16, col = "red", cex = 0.5)
abline(dom_alt_a, col = "black")
abline(sub_alt_a, col = "red")

# Pct water_mean_a ####
plot(dom_quad_patch$mean_area ~ env_var$pct_water, type = "n", xlab = "% water", ylab = "Mean area per patch")
points(dom_quad_patch$mean_area ~ env_var$pct_water, pch = 16, col = "black", cex = 0.5)
points(sub_quad_patch$mean_area ~ env_var$pct_water, pch = 16, col = "red", cex = 0.5)
abline(dom_water_a, col = "black")
abline(sub_water_a, col = "red")

# ditch_mean_a ####
plot(dom_quad_patch$mean_area ~ env_var$ditch_distance, type = "n", xlab = "Distance to ditch", ylab = "Mean area per patch")
points(dom_quad_patch$mean_area ~ env_var$ditch_distance, pch = 16, col = "black", cex = 0.5)
points(sub_quad_patch$mean_area ~ env_var$ditch_distance, pch = 16, col = "red", cex = 0.5)
abline(dom_ditch_a, col = "black")
abline(sub_ditch_a, col = "red")

#### DOM Mean shape vs. env var ####

# Distance to track_mean_s ####
plot(dom_quad_patch$mean_shape ~ env_var$distance_35m, type = "n", xlab = "Soil pH", ylab = "Mean shape index per patch")
points(dom_quad_patch$mean_shape ~ env_var$distance_35m, pch = 16, col = "black", cex = 0.5)
points(sub_quad_patch$mean_shape ~ env_var$distance_35m, pch = 16, col = "red", cex = 0.5)
abline(dom_soil_s, col = "black")
abline(sub_soil_s, col = "red")


dom_soil_s <- lm(dom_quad_patch$mean_shape ~ env_var$soil_pH)
dom_alt_s <- lm(dom_quad_patch$mean_shape ~ env_var$Altitude)
dom_slope_s <- lm(dom_quad_patch$mean_shape ~ env_var$slope)
dom_slope_s <- lm(dom_quad_patch$mean_shape ~ env_var$slope)
dom_water_s <- lm(dom_quad_patch$mean_shape ~ env_var$pct_water)
dom_ditch_s <- lm(dom_quad_patch$mean_shape ~ env_var$ditch_distance)

# Soil pH_mean_s ####
plot(dom_quad_patch$mean_shape ~ env_var$soil_pH, type = "n", xlab = "Soil pH", ylab = "Mean shape index per patch")
points(dom_quad_patch$mean_shape ~ env_var$soil_pH, pch = 16, col = "black", cex = 0.5)
points(sub_quad_patch$mean_shape ~ env_var$soil_pH, pch = 16, col = "red", cex = 0.5)
abline(dom_soil_s, col = "black")
abline(sub_soil_s, col = "red")

# Altitude_mean_s ####
plot(dom_quad_patch$mean_shape ~ env_var$Altitude, type = "n", xlab = "Altitude", ylab = "Mean shape index per patch")
points(dom_quad_patch$mean_shape ~ env_var$Altitude, pch = 16, col = "black", cex = 0.5)
points(sub_quad_patch$mean_shape ~ env_var$Altitude, pch = 16, col = "red", cex = 0.5)
abline(dom_alt_s, col = "black")
abline(sub_alt_s, col = "red")

# Slope_mean_s #####
plot(dom_quad_patch$mean_shape ~ env_var$slope, type = "n", xlab = "Slope", ylab = "Mean shape index per patch")
points(dom_quad_patch$mean_shape ~ env_var$slope, pch = 16, col = "black", cex = 0.5)
points(sub_quad_patch$mean_shape ~ env_var$slope, pch = 16, col = "red", cex = 0.5)
abline(dom_slope_s, col = "black")
abline(sub_slope_s, col = "red")

# Pct water_mean_s ####
plot(dom_quad_patch$mean_shape ~ env_var$pct_water, type = "n", xlab = "% water", ylab = "Mean shape index per patch")
points(dom_quad_patch$mean_shape ~ env_var$pct_water, pch = 16, col = "black", cex = 0.5)
points(sub_quad_patch$mean_shape ~ env_var$pct_water, pch = 16, col = "red", cex = 0.5)
abline(dom_water_s, col = "black")
abline(sub_water_s, col = "red")

# distance to ditch_mean_s ####
plot(dom_quad_patch$mean_shape ~ env_var$ditch_distance, type = "n", xlab = "% water", ylab = "Mean shape index per patch")
points(dom_quad_patch$mean_shape ~ env_var$ditch_distance, pch = 16, col = "black", cex = 0.5)
points(sub_quad_patch$mean_shape ~ env_var$ditch_distance, pch = 16, col = "red", cex = 0.5)
abline(dom_ditch_s, col = "black")
abline(sub_ditch_s, col = "red")

# mean shape per patch vs. sheep tracks ####
dom_shape_length_25 <- lm(dom_quad_patch$mean_shape ~ env_var$length_25m)
dom_shape_length_35 <- lm(dom_quad_patch$mean_shape ~ env_var$length_35m)
plot(dom_quad_patch$mean_shape ~ env_var$length_35m, type = "n", xlab = "Sheep track length", ylab = "Mean shape index per quadrat")
points(dom_quad_patch$mean_shape ~ env_var$length_25m, pch = 5, col = "red", cex = 0.5)
abline(dom_shape_length_25, col = "red")
points(dom_quad_patch$mean_shape ~ env_var$length_35, pch = 16, col = "black", cex = 0.5)
abline(dom_shape_length_35, col = "black")
points(sub_quad_patch$mean_shape ~ env_var$length_35m, pch = 16, col = "red", cex = 0.5)
abline(sub_shape_length_35, col = "red")

dom_distance_35_s <- lm(dom_quad_patch$mean_shape ~ env_var$distance_35m)
plot(dom_quad_patch$mean_shape ~ env_var$distance_35m, type = "n", xlab = "Distance to sheep track", ylab = "Mean shape index per patch")
points(dom_quad_patch$mean_shape ~ env_var$distance_35m, pch = 16, col = "black", cex = 0.5)
points(sub_quad_patch$mean_shape ~ env_var$distance_35m, pch = 16, col = "red", cex = 0.5)
abline(dom_distance_35_s, col = "black")
abline(sub_distance_35_s, col = "red")


##### SUBDOMINANT - univariate env_var #####
sub_quad_patch <- read.csv("Results/Patch_Stats/Mean_sum_total/subdominant_sum_area_shape_quad_21-08-18.csv") # per quadrat
sub_quad_patch <- sub_quad_patch[,-1]
sub_quad_patch <- sub_quad_patch[-168,]

# total area per quadrat
sub_total_length_25 <- lm(sub_quad_patch$total_patch ~ env_var$length_25m)
sub_total_length_35 <- lm(sub_quad_patch$total_patch ~ env_var$length_35m)
plot(sub_quad_patch$total_patch ~ env_var$length_35m, type = "n", xlab = "Sheep track length", ylab = "Number of patches per quadrat")
points(sub_quad_patch$total_patch ~ env_var$length_25m, pch = 5, col = "red", cex = 0.5)
abline(sub_total_length_25, col = "red")
points(sub_quad_patch$total_patch ~ env_var$length_35, pch = 16, col = "red", cex = 0.5)
abline(sub_total_length_35, col = "red")

sub_distance_35 <- lm(sub_quad_patch$total_patch ~ env_var$distance_35m)

sub_soil_p <- lm(sub_quad_patch$total_patch ~ env_var$soil_pH)
sub_alt_p <- lm(sub_quad_patch$total_patch ~ env_var$Altitude)
sub_slope_p <- lm(sub_quad_patch$total_patch ~ env_var$slope)
sub_slope_p <- lm(sub_quad_patch$total_patch ~ env_var$slope)
sub_water_p <- lm(sub_quad_patch$total_patch ~ env_var$pct_water)
sub_ditch_p <- lm(sub_quad_patch$total_patch ~ env_var$ditch_distance)

# mean area per quadrat
sub_area_length_25 <- lm(sub_quad_patch$mean_area ~ env_var$length_25m)
sub_area_length_35 <- lm(sub_quad_patch$mean_area ~ env_var$length_35m)
plot(sub_quad_patch$mean_area ~ env_var$length_35m, type = "n", xlab = "Sheep track length", ylab = "Mean area of patch per quadrat")
points(sub_quad_patch$mean_area ~ env_var$length_25m, pch = 5, col = "red", cex = 0.5)
abline(sub_area_length_25, col = "red")
points(sub_quad_patch$mean_area ~ env_var$length_35, pch = 16, col = "black", cex = 0.5)
abline(sub_area_length_35)

sub_soil_a <- lm(sub_quad_patch$mean_area ~ env_var$soil_pH)
sub_alt_a <- lm(sub_quad_patch$mean_area ~ env_var$Altitude)
sub_slope_a <- lm(sub_quad_patch$mean_area ~ env_var$slope)
sub_slope_a <- lm(sub_quad_patch$mean_area ~ env_var$slope)
sub_water_a <- lm(sub_quad_patch$mean_area ~ env_var$pct_water)
sub_ditch_a <- lm(sub_quad_patch$mean_area ~ env_var$ditch_distance)

#mean shape per quadrat
sub_shape_length_25 <- lm(sub_quad_patch$mean_shape ~ env_var$length_25m)
sub_shape_length_35 <- lm(sub_quad_patch$mean_shape ~ env_var$length_35m)
plot(sub_quad_patch$mean_shape ~ env_var$length_35m, type = "n", xlab = "Sheep track length", ylab = "Mean shape of patch per quadrat")
points(sub_quad_patch$mean_shape ~ env_var$length_25m, pch = 5, col = "red", cex = 0.5)
abline(sub_shape_length_25, col = "red")
points(sub_quad_patch$mean_shape ~ env_var$length_35, pch = 16, col = "black", cex = 0.5)
abline(sub_shape_length_35)

sub_distance_35_s <- lm(sub_quad_patch$mean_shape ~ env_var$distance_35m)

sub_soil_s <- lm(sub_quad_patch$mean_shape ~ env_var$soil_pH)
sub_alt_s <- lm(sub_quad_patch$mean_shape ~ env_var$Altitude)
sub_slope_s <- lm(sub_quad_patch$mean_shape ~ env_var$slope)
sub_slope_s <- lm(sub_quad_patch$mean_shape ~ env_var$slope)
sub_water_s <- lm(sub_quad_patch$mean_shape ~ env_var$pct_water)
sub_ditch_s <- lm(sub_quad_patch$mean_shape ~ env_var$ditch_distance)

# linear models for dominant/subdominant with each plot having the chosen species, i.e. each line will be a species
#### DOMINANT LMs for sheep track ####
# Patch number #

patchno_dom_spp <- read.csv("Results/Patch_Stats/Ordination/Patchno_spp/Patchno_matrix/Patchno_matrix_dom_12-08-19.csv")
patchno_dom_spp[is.na(patchno_dom_spp)] <- 0
callvulg_35 <- lm(patchno_dom_spp$callvulg ~ env_var$length_35m)
eriovagi_35 <- lm(patchno_dom_spp$eriovagi ~ env_var$length_35m)
junceffu_35 <- lm(patchno_dom_spp$junceffu ~ env_var$length_35m)
juncsqua_35 <- lm(patchno_dom_spp$juncsqua ~ env_var$length_35m)
molicaer_35 <- lm(patchno_dom_spp$molicaer ~ env_var$length_35m)
nardstri_35 <- lm(patchno_dom_spp$nardstri ~ env_var$length_35m)
carenigr_35 <- lm(patchno_dom_spp$carenigr ~ env_var$length_35m)
descflex_35 <- lm(patchno_dom_spp$descflex ~ env_var$length_35m)
galisaxa_35 <- lm(patchno_dom_spp$galisaxa ~ env_var$length_35m)
poteerec_35 <- lm(patchno_dom_spp$poteerec ~ env_var$length_35m)
vaccmyrt_35 <- lm(patchno_dom_spp$vaccmyrt ~ env_var$length_35m)

#dom_patch_callvulg_35 <- lm(patchno_dom_spp$callvulg ~ env_var$length_35m)
plot(patchno_dom_spp$callvulg + patchno_dom_spp$eriovagi + patchno_dom_spp$junceffu + 
       patchno_dom_spp$juncsqua + patchno_dom_spp$molicaer + patchno_dom_spp$nardstri
     + patchno_dom_spp$carenigr + patchno_dom_spp$descflex + patchno_dom_spp$galisaxa + 
       patchno_dom_spp$poteerec + patchno_dom_spp$vaccmyrt 
     ~ env_var$length_35m, ylim = c(0,5), type = "n", xlab = "Sheep track length", ylab = "Number of patches")
#points(patchno_dom_spp$callvulg + patchno_dom_spp$eriovagi + patchno_dom_spp$junceffu + 
#        patchno_dom_spp$juncsqua + patchno_dom_spp$molicaer + patchno_dom_spp$nardstri
#       + patchno_dom_spp$carenigr + patchno_dom_spp$descflex + patchno_dom_spp$galisaxa + patchno_dom_spp$poteerec + 
#         patchno_dom_spp$vaccmyrt ~ env_var$length_35m, pch = 1:11, col = 1:11, cex = 0.5)
#abline(c(callvulg_35, eriovagi_35, junceffu_35, juncsqua_35, molicaer_35, nardstri_35,
#       carenigr_35, descflex_35, galisaxa_35, poteerec_35, vaccmyrt_35), col = 1:11)
abline(callvulg_35, col = "red")
abline(eriovagi_35, col = "green")
abline(junceffu_35, col = "blue")
abline(juncsqua_35, col = "yellow")
abline(molicaer_35, col = "black")
abline(nardstri_35, col = "orange")
abline(carenigr_35, col = "brown")
abline(descflex_35, col = "pink")
abline(galisaxa_35, col = "aquamarine")
abline(poteerec_35, col = "violet")
abline(vaccmyrt_35, col = "gold")
legend("topright", 
       legend = c("callvulg", "eriovagi", "junceffu", "juncsqua", "molicaer", "nardstri", 
                  "carenigr", "descflex", "galisaxa", "poteerec", "vaccmyrt"), 
       col = c("red", "green", "blue", "yellow", "black", "orange", "brown", "pink", "aquamarine",
               "violet", "gold"), 
       lty = 1,
       bty = "n", 
       cex = 0.5, 
       text.col = "black", 
       horiz = F, ncol = 5)

## Area ## 
area_dom_spp <- read.csv("Results/Patch_Stats/Ordination/Area_spp/Area_matrix/Area_spp_dominant_4-08-19.csv")

callvulg_35_a <- lm(area_dom_spp$callvulg ~ env_var$length_35m)
eriovagi_35_a <- lm(area_dom_spp$eriovagi ~ env_var$length_35m)
junceffu_35_a <- lm(area_dom_spp$junceffu ~ env_var$length_35m)
juncsqua_35_a <- lm(area_dom_spp$juncsqua ~ env_var$length_35m)
molicaer_35_a <- lm(area_dom_spp$molicaer ~ env_var$length_35m)
nardstri_35_a <- lm(area_dom_spp$nardstri ~ env_var$length_35m)
carenigr_35_a <- lm(area_dom_spp$carenigr ~ env_var$length_35m)
descflex_35_a <- lm(area_dom_spp$descflex ~ env_var$length_35m)
galisaxa_35_a <- lm(area_dom_spp$galisaxa ~ env_var$length_35m)
poteerec_35_a <- lm(area_dom_spp$poteerec ~ env_var$length_35m)
vaccmyrt_35_a <- lm(area_dom_spp$vaccmyrt ~ env_var$length_35m)
plot(area_dom_spp$callvulg + area_dom_spp$eriovagi + area_dom_spp$junceffu + 
       area_dom_spp$juncsqua + area_dom_spp$molicaer + area_dom_spp$nardstri
     + area_dom_spp$carenigr + area_dom_spp$descflex + area_dom_spp$galisaxa + 
       area_dom_spp$poteerec + area_dom_spp$vaccmyrt 
     ~ env_var$length_35m, ylim = c(0,50), type = "n", xlab = "Sheep track length", ylab = "Area per patch")
abline(callvulg_35_a, col = "red")
abline(eriovagi_35_a, col = "green")
abline(junceffu_35_a, col = "blue")
abline(juncsqua_35_a, col = "yellow")
abline(molicaer_35_a, col = "black")
abline(nardstri_35_a, col = "orange")
abline(carenigr_35_a, col = "brown")
abline(descflex_35_a, col = "pink")
abline(galisaxa_35_a, col = "aquamarine")
abline(poteerec_35_a, col = "violet")
abline(vaccmyrt_35_a, col = "gold")
legend("top", 
       legend = c("callvulg", "eriovagi", "junceffu", "juncsqua", "molicaer", "nardstri", 
                  "carenigr", "descflex", "galisaxa", "poteerec", "vaccmyrt"), 
       col = c("red", "green", "blue", "yellow", "black", "orange", "brown", "pink", "aquamarine",
               "violet", "gold"), 
       lty = 1,
       bty = "n", 
       cex = 0.5, 
       text.col = "black", 
       horiz = F, ncol = 5)

## Shape Index ##
shape_dom_spp <- data.frame(read.csv("Results/Patch_Stats/Ordination/Shape_Index/shape_matrix/Shape_dom_matrix_12-08-19.csv"))
callvulg_35_s <- lm(shape_dom_spp$callvulg ~ env_var$length_35m)
eriovagi_35_s <- lm(shape_dom_spp$eriovagi ~ env_var$length_35m)
junceffu_35_s <- lm(shape_dom_spp$junceffu ~ env_var$length_35m)
juncsqua_35_s <- lm(shape_dom_spp$juncsqua ~ env_var$length_35m)
molicaer_35_s <- lm(shape_dom_spp$molicaer ~ env_var$length_35m)
nardstri_35_s <- lm(shape_dom_spp$nardstri ~ env_var$length_35m)
carenigr_35_s <- lm(shape_dom_spp$carenigr ~ env_var$length_35m)
descflex_35_s <- lm(shape_dom_spp$descflex ~ env_var$length_35m)
galisaxa_35_s <- lm(shape_dom_spp$galisaxa ~ env_var$length_35m)
poteerec_35_s <- lm(shape_dom_spp$poteerec ~ env_var$length_35m)
vaccmyrt_35_s <- lm(shape_dom_spp$vaccmyrt ~ env_var$length_35m)
plot(shape_dom_spp$callvulg + shape_dom_spp$eriovagi + shape_dom_spp$junceffu + 
       shape_dom_spp$juncsqua + shape_dom_spp$molicaer + shape_dom_spp$nardstri
     + shape_dom_spp$carenigr + shape_dom_spp$descflex + shape_dom_spp$galisaxa + 
       shape_dom_spp$poteerec + shape_dom_spp$vaccmyrt 
     ~ env_var$length_35m, ylim = c(1, 2), type = "n", xlab = "Sheep track length/m", ylab = "Shape Index", cex = 0.5)
abline(callvulg_35_s, col = "red")
abline(eriovagi_35_s, col = "green")
abline(junceffu_35_s, col = "blue")
abline(juncsqua_35_s, col = "yellow")
abline(molicaer_35_s, col = "black")
abline(nardstri_35_s, col = "orange")
abline(carenigr_35_s, col = "brown")
abline(descflex_35_s, col = "pink")
abline(galisaxa_35_s, col = "aquamarine")
abline(poteerec_35_s, col = "violet")
abline(vaccmyrt_35_s, col = "gold")
legend("top", 
       legend = c("callvulg", "eriovagi", "junceffu", "juncsqua", "molicaer", "nardstri", 
                  "carenigr", "descflex", "galisaxa", "poteerec", "vaccmyrt"), 
       col = c("red", "green", "blue", "yellow", "black", "orange", "brown", "pink", "aquamarine",
               "violet", "gold"), 
       lty = 1,
       bty = "n", 
       cex = 0.5, 
       text.col = "black", 
       horiz = F, ncol = 5)

##### SUBDOMINANT LMs for sheep track #####
## LENGTH OF TRACK ##
# Patches # 
patchno_sub_spp <- data.frame(read.csv("Results/Patch_Stats/Ordination/Patchno_spp/Patchno_matrix/Patchno_matrix_sub_12-08-19.csv"))
patchno_sub_spp[is.na(patchno_sub_spp)] <- 0
sub_callvulg_35 <- lm(patchno_sub_spp$Callvulg ~ env_var$length_35m)
sub_eriovagi_35 <- lm(patchno_sub_spp$Eriovagi ~ env_var$length_35m)
sub_junceffu_35 <- lm(patchno_sub_spp$Junceffu ~ env_var$length_35m)
sub_juncsqua_35 <- lm(patchno_sub_spp$Juncsqua ~ env_var$length_35m)
sub_molicaer_35 <- lm(patchno_sub_spp$Molicaer ~ env_var$length_35m)
sub_nardstri_35 <- lm(patchno_sub_spp$Nardstri ~ env_var$length_35m)
sub_carenigr_35 <- lm(patchno_sub_spp$Carenigr ~ env_var$length_35m)
sub_descflex_35 <- lm(patchno_sub_spp$Descflex ~ env_var$length_35m)
sub_galisaxa_35 <- lm(patchno_sub_spp$Galisaxa ~ env_var$length_35m)
sub_poteerec_35 <- lm(patchno_sub_spp$Poteerec ~ env_var$length_35m)
sub_vaccmyrt_35 <- lm(patchno_sub_spp$Vaccmyrt ~ env_var$length_35m)

#sub_patch_callvulg_35 <- lm(patchno_sub_spp$callvulg ~ env_var$length_35m)
plot(patchno_sub_spp$Callvulg + patchno_sub_spp$Eriovagi + patchno_sub_spp$Junceffu + 
       patchno_sub_spp$Juncsqua + patchno_sub_spp$Molicaer + patchno_sub_spp$Nardstri
     + patchno_sub_spp$Carenigr + patchno_sub_spp$Descflex + patchno_sub_spp$Galisaxa + 
       patchno_sub_spp$Poteerec + patchno_sub_spp$Vaccmyrt 
     ~ env_var$distance_35m, ylim = c(0, 5), type = "n", xlab = "Length of track", ylab = "Number of patches")
#points(patchno_sub_spp$callvulg + patchno_sub_spp$eriovagi + patchno_sub_spp$junceffu + 
#patchno_sub_spp$juncsqua + patchno_sub_spp$molicaer + patchno_sub_spp$nardstri
#+ patchno_sub_spp$carenigr + patchno_sub_spp$descflex + patchno_sub_spp$galisaxa + patchno_sub_spp$poteerec + 
#  patchno_sub_spp$vaccmyrt ~ env_var$distance_35m, pch = 1:11, col = 1:11, cex = 0.5)
#abline(c(callvulg_35, eriovagi_35, junceffu_35, juncsqua_35, molicaer_35, nardstri_35,
#carenigr_35, descflex_35, galisaxa_35, poteerec_35, vaccmyrt_35), col = 1:11)
abline(callvulg_35, col = "red")
abline(eriovagi_35, col = "green")
abline(junceffu_35, col = "blue")
abline(juncsqua_35, col = "yellow")
abline(molicaer_35, col = "black")
abline(nardstri_35, col = "orange")
abline(carenigr_35, col = "brown")
abline(descflex_35, col = "pink")
abline(galisaxa_35, col = "aquamarine")
abline(poteerec_35, col = "violet")
abline(vaccmyrt_35, col = "gold")
legend("topright", 
       legend = c("callvulg", "eriovagi", "junceffu", "juncsqua", "molicaer", "nardstri", 
                  "carenigr", "descflex", "galisaxa", "poteerec", "vaccmyrt"), 
       col = c("red", "green", "blue", "yellow", "black", "orange", "brown", "pink", "aquamarine",
               "violet", "gold"), 
       lty = 1,
       bty = "n", 
       cex = 0.5, 
       text.col = "black", 
       horiz = F, ncol = 5)

## AREA ##
area_sub_spp <- read.csv("Results/Patch_Stats/Ordination/Area_spp/Area_matrix/Area_spp_sub_4-08-19.csv")
area_sub_spp[is.na(area_sub_spp)] <- 0
sub_callvulg_35_a <- lm(area_sub_spp$Callvulg ~ env_var$length_35m)
sub_eriovagi_35_a <- lm(area_sub_spp$Eriovagi ~ env_var$length_35m)
sub_junceffu_35_a <- lm(area_sub_spp$Junceffu ~ env_var$length_35m)
sub_juncsqua_35_a <- lm(area_sub_spp$Juncsqua ~ env_var$length_35m)
sub_molicaer_35_a <- lm(area_sub_spp$Molicaer ~ env_var$length_35m)
sub_nardstri_35_a <- lm(area_sub_spp$Nardstri ~ env_var$length_35m)
sub_carenigr_35_a <- lm(area_sub_spp$Carenigr ~ env_var$length_35m)
sub_descflex_35_a <- lm(area_sub_spp$Descflex ~ env_var$length_35m)
sub_galisaxa_35_a <- lm(area_sub_spp$Galisaxa ~ env_var$length_35m)
sub_poteerec_35_a <- lm(area_sub_spp$Poteerec ~ env_var$length_35m)
sub_vaccmyrt_35_a <- lm(area_sub_spp$Vaccmyrt ~ env_var$length_35m)

#sub_patch_callvulg_35 <- lm(area_sub_spp$callvulg ~ env_var$length_35m)
plot(area_sub_spp$Callvulg + area_sub_spp$Eriovagi + area_sub_spp$Junceffu + 
       area_sub_spp$Juncsqua + area_sub_spp$Molicaer + area_sub_spp$Nardstri
     + area_sub_spp$Carenigr + area_sub_spp$Descflex + area_sub_spp$Galisaxa + 
       area_sub_spp$Poteerec + area_sub_spp$Vaccmyrt 
     ~ env_var$distance_35m, ylim = c(0, 60), type = "n", xlab = "Length of track", ylab = "Area per patch")
#points(area_sub_spp$callvulg + area_sub_spp$eriovagi + area_sub_spp$junceffu + 
#area_sub_spp$juncsqua + area_sub_spp$molicaer + area_sub_spp$nardstri
#+ area_sub_spp$carenigr + area_sub_spp$descflex + area_sub_spp$galisaxa + area_sub_spp$poteerec + 
#  area_sub_spp$vaccmyrt ~ env_var$distance_35m, pch = 1:11, col = 1:11, cex = 0.5)
#abline(c(callvulg_35, eriovagi_35, junceffu_35, juncsqua_35, molicaer_35, nardstri_35,
#carenigr_35, descflex_35, galisaxa_35, poteerec_35, vaccmyrt_35), col = 1:11)
abline(callvulg_35_a, col = "red")
abline(eriovagi_35_a, col = "green")
abline(junceffu_35_a, col = "blue")
abline(juncsqua_35_a, col = "yellow")
abline(molicaer_35_a, col = "black")
abline(nardstri_35_a, col = "orange")
abline(carenigr_35_a, col = "brown")
abline(descflex_35_a, col = "pink")
abline(galisaxa_35_a, col = "aquamarine")
abline(poteerec_35_a, col = "violet")
abline(vaccmyrt_35_a, col = "gold")
legend("topright", 
       legend = c("callvulg", "eriovagi", "junceffu", "juncsqua", "molicaer", "nardstri", 
                  "carenigr", "descflex", "galisaxa", "poteerec", "vaccmyrt"), 
       col = c("red", "green", "blue", "yellow", "black", "orange", "brown", "pink", "aquamarine",
               "violet", "gold"), 
       lty = 1,
       bty = "n", 
       cex = 0.5, 
       text.col = "black", 
       horiz = F, ncol = 5)

## SHAPE INDEX ##
shape_sub_spp <- data.frame(read.csv("Results/Patch_Stats/Ordination/Shape_Index/shape_matrix/Shape_sub_matrix_12-08-19.csv"))
sub_callvulg_35_s <- lm(shape_sub_spp$Callvulg ~ env_var$length_35m)
sub_eriovagi_35_s <- lm(shape_sub_spp$Eriovagi ~ env_var$length_35m)
sub_junceffu_35_s <- lm(shape_sub_spp$Junceffu ~ env_var$length_35m)
sub_juncsqua_35_s <- lm(shape_sub_spp$Juncsqua ~ env_var$length_35m)
sub_molicaer_35_s <- lm(shape_sub_spp$Molicaer ~ env_var$length_35m)
sub_nardstri_35_s <- lm(shape_sub_spp$Nardstri ~ env_var$length_35m)
sub_carenigr_35_s <- lm(shape_sub_spp$Carenigr ~ env_var$length_35m)
sub_descflex_35_s <- lm(shape_sub_spp$Descflex ~ env_var$length_35m)
sub_galisaxa_35_s <- lm(shape_sub_spp$Galisaxa ~ env_var$length_35m)
sub_poteerec_35_s <- lm(shape_sub_spp$Poteerec ~ env_var$length_35m)
sub_vaccmyrt_35_s <- lm(shape_sub_spp$Vaccmyrt ~ env_var$length_35m)
plot(shape_sub_spp$Callvulg + shape_sub_spp$Eriovagi + shape_sub_spp$Junceffu + 
       shape_sub_spp$Juncsqua + shape_sub_spp$Molicaer + shape_sub_spp$Nardstri
     + shape_sub_spp$Carenigr + shape_sub_spp$Descflex + shape_sub_spp$Galisaxa + 
       shape_sub_spp$Poteerec + shape_sub_spp$Vaccmyrt 
     ~ env_var$length_35m, ylim = c(1, 1.5), type = "n", xlab = "Sheep track length", ylab = "Shape per patch")
abline(sub_callvulg_35_s, col = "red")
abline(sub_eriovagi_35_s, col = "green")
abline(sub_junceffu_35_s, col = "blue")
abline(sub_juncsqua_35_s, col = "yellow")
abline(sub_molicaer_35_s, col = "black")
abline(sub_nardstri_35_s, col = "orange")
abline(sub_carenigr_35_s, col = "brown")
abline(sub_descflex_35_s, col = "pink")
abline(sub_galisaxa_35_s, col = "aquamarine")
abline(sub_poteerec_35_s, col = "violet")
abline(sub_vaccmyrt_35_s, col = "gold")
legend("top", 
       legend = c("callvulg", "eriovagi", "junceffu", "juncsqua", "molicaer", "nardstri", 
                  "carenigr", "descflex", "galisaxa", "poteerec", "vaccmyrt"), 
       col = c("red", "green", "blue", "yellow", "black", "orange", "brown", "pink", "aquamarine",
               "violet", "gold"), 
       lty = 1,
       bty = "n", 
       cex = 0.5, 
       text.col = "black", 
       horiz = F, ncol = 5)

## distance to track ##
#### DOMINANT for distance to track ####
patchno_dom_spp <- read.csv("Results/Patch_Stats/Ordination/Patchno_spp/Patchno_matrix/Patchno_matrix_dom_12-08-19.csv")
patchno_dom_spp[is.na(patchno_dom_spp)] <- 0
callvulg_35 <- lm(patchno_dom_spp$callvulg ~ env_var$distance_35m)
eriovagi_35 <- lm(patchno_dom_spp$eriovagi ~ env_var$distance_35m)
junceffu_35 <- lm(patchno_dom_spp$junceffu ~ env_var$distance_35m)
juncsqua_35 <- lm(patchno_dom_spp$juncsqua ~ env_var$distance_35m)
molicaer_35 <- lm(patchno_dom_spp$molicaer ~ env_var$distance_35m)
nardstri_35 <- lm(patchno_dom_spp$nardstri ~ env_var$distance_35m)
carenigr_35 <- lm(patchno_dom_spp$carenigr ~ env_var$distance_35m)
descflex_35 <- lm(patchno_dom_spp$descflex ~ env_var$distance_35m)
galisaxa_35 <- lm(patchno_dom_spp$galisaxa ~ env_var$distance_35m)
poteerec_35 <- lm(patchno_dom_spp$poteerec ~ env_var$distance_35m)
vaccmyrt_35 <- lm(patchno_dom_spp$vaccmyrt ~ env_var$distance_35m)

#dom_patch_callvulg_35 <- lm(patchno_dom_spp$callvulg ~ env_var$length_35m)
plot(patchno_dom_spp$callvulg + patchno_dom_spp$eriovagi + patchno_dom_spp$junceffu + 
       patchno_dom_spp$juncsqua + patchno_dom_spp$molicaer + patchno_dom_spp$nardstri
     + patchno_dom_spp$carenigr + patchno_dom_spp$descflex + patchno_dom_spp$galisaxa + 
       patchno_dom_spp$poteerec + patchno_dom_spp$vaccmyrt 
     ~ env_var$distance_35m, ylim = c(0, 5), type = "n", xlab = "Distance from quadrat", ylab = "Number of patches")
#points(patchno_dom_spp$callvulg + patchno_dom_spp$eriovagi + patchno_dom_spp$junceffu + 
#patchno_dom_spp$juncsqua + patchno_dom_spp$molicaer + patchno_dom_spp$nardstri
#+ patchno_dom_spp$carenigr + patchno_dom_spp$descflex + patchno_dom_spp$galisaxa + patchno_dom_spp$poteerec + 
#  patchno_dom_spp$vaccmyrt ~ env_var$distance_35m, pch = 1:11, col = 1:11, cex = 0.5)
#abline(c(callvulg_35, eriovagi_35, junceffu_35, juncsqua_35, molicaer_35, nardstri_35,
#carenigr_35, descflex_35, galisaxa_35, poteerec_35, vaccmyrt_35), col = 1:11)
abline(callvulg_35, col = "red")
abline(eriovagi_35, col = "green")
abline(junceffu_35, col = "blue")
abline(juncsqua_35, col = "yellow")
abline(molicaer_35, col = "black")
abline(nardstri_35, col = "orange")
abline(carenigr_35, col = "brown")
abline(descflex_35, col = "pink")
abline(galisaxa_35, col = "aquamarine")
abline(poteerec_35, col = "violet")
abline(vaccmyrt_35, col = "gold")
legend("topright", 
       legend = c("callvulg", "eriovagi", "junceffu", "juncsqua", "molicaer", "nardstri", 
                  "carenigr", "descflex", "galisaxa", "poteerec", "vaccmyrt"), 
       col = c("red", "green", "blue", "yellow", "black", "orange", "brown", "pink", "aquamarine",
               "violet", "gold"), 
       lty = 1,
       bty = "n", 
       cex = 0.5, 
       text.col = "black", 
       horiz = F, ncol = 5)

## Area ##
area_dom_spp <- read.csv("Results/Patch_Stats/Ordination/Area_spp/Area_matrix/Area_spp_dominant_4-08-19.csv")

callvulg_35_a <- lm(area_dom_spp$callvulg ~ env_var$distance_35m)
eriovagi_35_a <- lm(area_dom_spp$eriovagi ~ env_var$distance_35m)
junceffu_35_a <- lm(area_dom_spp$junceffu ~ env_var$distance_35m)
juncsqua_35_a <- lm(area_dom_spp$juncsqua ~ env_var$distance_35m)
molicaer_35_a <- lm(area_dom_spp$molicaer ~ env_var$distance_35m)
nardstri_35_a <- lm(area_dom_spp$nardstri ~ env_var$distance_35m)
carenigr_35_a <- lm(area_dom_spp$carenigr ~ env_var$distance_35m)
descflex_35_a <- lm(area_dom_spp$descflex ~ env_var$distance_35m)
galisaxa_35_a <- lm(area_dom_spp$galisaxa ~ env_var$distance_35m)
poteerec_35_a <- lm(area_dom_spp$poteerec ~ env_var$distance_35m)
vaccmyrt_35_a <- lm(area_dom_spp$vaccmyrt ~ env_var$distance_35m)
plot(area_dom_spp$callvulg + area_dom_spp$eriovagi + area_dom_spp$junceffu + 
       area_dom_spp$juncsqua + area_dom_spp$molicaer + area_dom_spp$nardstri
     + area_dom_spp$carenigr + area_dom_spp$descflex + area_dom_spp$galisaxa + 
       area_dom_spp$poteerec + area_dom_spp$vaccmyrt 
     ~ env_var$distance_35m, ylim = c(0,50), type = "n", xlab = "Distance from quadrat", ylab = "Area per patch")
abline(callvulg_35_a, col = "red")
abline(eriovagi_35_a, col = "green")
abline(junceffu_35_a, col = "blue")
abline(juncsqua_35_a, col = "yellow")
abline(molicaer_35_a, col = "black")
abline(nardstri_35_a, col = "orange")
abline(carenigr_35_a, col = "brown")
abline(descflex_35_a, col = "pink")
abline(galisaxa_35_a, col = "aquamarine")
abline(poteerec_35_a, col = "violet")
abline(vaccmyrt_35_a, col = "gold")
legend("top", 
       legend = c("callvulg", "eriovagi", "junceffu", "juncsqua", "molicaer", "nardstri", 
                  "carenigr", "descflex", "galisaxa", "poteerec", "vaccmyrt"), 
       col = c("red", "green", "blue", "yellow", "black", "orange", "brown", "pink", "aquamarine",
               "violet", "gold"), 
       lty = 1,
       bty = "n", 
       cex = 0.5, 
       text.col = "black", 
       horiz = F, ncol = 5)

## SHAPE INDEX ##
shape_dom_spp <- data.frame(read.csv("Results/Patch_Stats/Ordination/Shape_Index/shape_matrix/Shape_dom_matrix_12-08-19.csv"))
callvulg_35_s <- lm(shape_dom_spp$callvulg ~ env_var$distance_35m)
eriovagi_35_s <- lm(shape_dom_spp$eriovagi ~ env_var$distance_35m)
junceffu_35_s <- lm(shape_dom_spp$junceffu ~ env_var$distance_35m)
juncsqua_35_s <- lm(shape_dom_spp$juncsqua ~ env_var$distance_35m)
molicaer_35_s <- lm(shape_dom_spp$molicaer ~ env_var$distance_35m)
nardstri_35_s <- lm(shape_dom_spp$nardstri ~ env_var$distance_35m)
carenigr_35_s <- lm(shape_dom_spp$carenigr ~ env_var$distance_35m)
descflex_35_s <- lm(shape_dom_spp$descflex ~ env_var$distance_35m)
galisaxa_35_s <- lm(shape_dom_spp$galisaxa ~ env_var$distance_35m)
poteerec_35_s <- lm(shape_dom_spp$poteerec ~ env_var$distance_35m)
vaccmyrt_35_s <- lm(shape_dom_spp$vaccmyrt ~ env_var$distance_35m)
plot(shape_dom_spp$callvulg + shape_dom_spp$eriovagi + shape_dom_spp$junceffu + 
       shape_dom_spp$juncsqua + shape_dom_spp$molicaer + shape_dom_spp$nardstri
     + shape_dom_spp$carenigr + shape_dom_spp$descflex + shape_dom_spp$galisaxa + 
       shape_dom_spp$poteerec + shape_dom_spp$vaccmyrt 
     ~ env_var$distance_35m, ylim = c(1,2), type = "n", xlab = "Distance from quadrat/m", ylab = "Shape Index")
abline(callvulg_35_s, col = "red")
abline(eriovagi_35_s, col = "green")
abline(junceffu_35_s, col = "blue")
abline(juncsqua_35_s, col = "yellow")
abline(molicaer_35_s, col = "black")
abline(nardstri_35_s, col = "orange")
abline(carenigr_35_s, col = "brown")
abline(descflex_35_s, col = "pink")
abline(galisaxa_35_s, col = "aquamarine")
abline(poteerec_35_s, col = "violet")
abline(vaccmyrt_35_s, col = "gold")
legend("top", 
       legend = c("callvulg", "eriovagi", "junceffu", "juncsqua", "molicaer", "nardstri", 
                  "carenigr", "descflex", "galisaxa", "poteerec", "vaccmyrt"), 
       col = c("red", "green", "blue", "yellow", "black", "orange", "brown", "pink", "aquamarine",
               "violet", "gold"), 
       lty = 1,
       bty = "n", 
       cex = 0.5, 
       text.col = "black", 
       horiz = F, ncol = 5)

##### SUBDOMINANT for distance to track #####
# Patches # 
patchno_sub_spp <- data.frame(read.csv("Results/Patch_Stats/Ordination/Patchno_spp/Patchno_matrix/Patchno_matrix_sub_12-08-19.csv"))
sub_callvulg_35 <- lm(patchno_sub_spp$Callvulg ~ env_var$distance_35m)
sub_eriovagi_35 <- lm(patchno_sub_spp$Eriovagi ~ env_var$distance_35m)
sub_junceffu_35 <- lm(patchno_sub_spp$Junceffu ~ env_var$distance_35m)
sub_juncsqua_35 <- lm(patchno_sub_spp$Juncsqua ~ env_var$distance_35m)
sub_molicaer_35 <- lm(patchno_sub_spp$Molicaer ~ env_var$distance_35m)
sub_nardstri_35 <- lm(patchno_sub_spp$Nardstri ~ env_var$distance_35m)
sub_carenigr_35 <- lm(patchno_sub_spp$Carenigr ~ env_var$distance_35m)
sub_descflex_35 <- lm(patchno_sub_spp$Descflex ~ env_var$distance_35m)
sub_galisaxa_35 <- lm(patchno_sub_spp$Galisaxa ~ env_var$distance_35m)
sub_poteerec_35 <- lm(patchno_sub_spp$Poteerec ~ env_var$distance_35m)
sub_vaccmyrt_35 <- lm(patchno_sub_spp$Vaccmyrt ~ env_var$distance_35m)

#sub_patch_callvulg_35 <- lm(patchno_sub_spp$callvulg ~ env_var$length_35m)
plot(patchno_sub_spp$Callvulg + patchno_sub_spp$Eriovagi + patchno_sub_spp$Junceffu + 
       patchno_sub_spp$Juncsqua + patchno_sub_spp$Molicaer + patchno_sub_spp$Nardstri
     + patchno_sub_spp$Carenigr + patchno_sub_spp$Descflex + patchno_sub_spp$Galisaxa + 
       patchno_sub_spp$Poteerec + patchno_sub_spp$Vaccmyrt 
     ~ env_var$distance_35m, ylim = c(0, 8), type = "n", xlab = "Distance from quadrat", ylab = "Number of patches")
#points(patchno_sub_spp$callvulg + patchno_sub_spp$eriovagi + patchno_sub_spp$junceffu + 
#patchno_sub_spp$juncsqua + patchno_sub_spp$molicaer + patchno_sub_spp$nardstri
#+ patchno_sub_spp$carenigr + patchno_sub_spp$descflex + patchno_sub_spp$galisaxa + patchno_sub_spp$poteerec + 
#  patchno_sub_spp$vaccmyrt ~ env_var$distance_35m, pch = 1:11, col = 1:11, cex = 0.5)
#abline(c(callvulg_35, eriovagi_35, junceffu_35, juncsqua_35, molicaer_35, nardstri_35,
#carenigr_35, descflex_35, galisaxa_35, poteerec_35, vaccmyrt_35), col = 1:11)
abline(sub_callvulg_35, col = "red")
abline(sub_eriovagi_35, col = "green")
abline(sub_junceffu_35, col = "blue")
abline(sub_juncsqua_35, col = "yellow")
abline(sub_molicaer_35, col = "black")
abline(sub_nardstri_35, col = "orange")
abline(sub_carenigr_35, col = "brown")
abline(sub_descflex_35, col = "pink")
abline(sub_galisaxa_35, col = "aquamarine")
abline(sub_poteerec_35, col = "violet")
abline(sub_vaccmyrt_35, col = "gold")
legend("topright", 
       legend = c("callvulg", "eriovagi", "junceffu", "juncsqua", "molicaer", "nardstri", 
                  "carenigr", "descflex", "galisaxa", "poteerec", "vaccmyrt"), 
       col = c("red", "green", "blue", "yellow", "black", "orange", "brown", "pink", "aquamarine",
               "violet", "gold"), 
       lty = 1,
       bty = "n", 
       cex = 0.5, 
       text.col = "black", 
       horiz = F, ncol = 5)

## AREA ##
area_sub_spp <- read.csv("Results/Patch_Stats/Ordination/Area_spp/Area_matrix/Area_spp_sub_4-08-19.csv")
area_sub_spp[is.na(patchno_sub_spp)] <- 0
sub_callvulg_35_a <- lm(area_sub_spp$Callvulg ~ env_var$distance_35m)
sub_eriovagi_35_a <- lm(area_sub_spp$Eriovagi ~ env_var$distance_35m)
sub_junceffu_35_a <- lm(area_sub_spp$Junceffu ~ env_var$distance_35m)
sub_juncsqua_35_a <- lm(area_sub_spp$Juncsqua ~ env_var$distance_35m)
sub_molicaer_35_a <- lm(area_sub_spp$Molicaer ~ env_var$distance_35m)
sub_nardstri_35_a <- lm(area_sub_spp$Nardstri ~ env_var$distance_35m)
sub_carenigr_35_a <- lm(area_sub_spp$Carenigr ~ env_var$distance_35m)
sub_descflex_35_a <- lm(area_sub_spp$Descflex ~ env_var$distance_35m)
sub_galisaxa_35_a <- lm(area_sub_spp$Galisaxa ~ env_var$distance_35m)
sub_poteerec_35_a <- lm(area_sub_spp$Poteerec ~ env_var$distance_35m)
sub_vaccmyrt_35_a <- lm(area_sub_spp$Vaccmyrt ~ env_var$distance_35m)

#dom_patch_callvulg_35 <- lm(area_sub_spp$callvulg ~ env_var$length_35m)
plot(area_sub_spp$Callvulg + area_sub_spp$Eriovagi + area_sub_spp$Junceffu + 
       area_sub_spp$Juncsqua + area_sub_spp$Molicaer + area_sub_spp$Nardstri
     + area_sub_spp$Carenigr + area_sub_spp$Descflex + area_sub_spp$Galisaxa + 
       area_sub_spp$Poteerec + area_sub_spp$Vaccmyrt 
     ~ env_var$distance_35m, ylim = c(0, 25), type = "n", xlab = "Distance from quadrat", ylab = "Area of patches")
#points(patchno_dom_spp$callvulg + patchno_dom_spp$eriovagi + patchno_dom_spp$junceffu + 
#patchno_dom_spp$juncsqua + patchno_dom_spp$molicaer + patchno_dom_spp$nardstri
#+ patchno_dom_spp$carenigr + patchno_dom_spp$descflex + patchno_dom_spp$galisaxa + patchno_dom_spp$poteerec + 
#  patchno_dom_spp$vaccmyrt ~ env_var$distance_35m, pch = 1:11, col = 1:11, cex = 0.5)
#abline(c(callvulg_35, eriovagi_35, junceffu_35, juncsqua_35, molicaer_35, nardstri_35,
#carenigr_35, descflex_35, galisaxa_35, poteerec_35, vaccmyrt_35), col = 1:11)
abline(sub_callvulg_35_a, col = "red")
abline(sub_eriovagi_35_a, col = "green")
abline(sub_junceffu_35_a, col = "blue")
abline(sub_juncsqua_35_a, col = "yellow")
abline(sub_molicaer_35_a, col = "black")
abline(sub_nardstri_35_a, col = "orange")
abline(sub_carenigr_35_a, col = "brown")
abline(sub_descflex_35_a, col = "pink")
abline(sub_galisaxa_35_a, col = "aquamarine")
abline(sub_poteerec_35_a, col = "violet")
abline(sub_vaccmyrt_35_a, col = "gold")
legend("topright", 
       legend = c("callvulg", "eriovagi", "junceffu", "juncsqua", "molicaer", "nardstri", 
                  "carenigr", "descflex", "galisaxa", "poteerec", "vaccmyrt"), 
       col = c("red", "green", "blue", "yellow", "black", "orange", "brown", "pink", "aquamarine",
               "violet", "gold"), 
       lty = 1,
       bty = "n", 
       cex = 0.5, 
       text.col = "black", 
       horiz = F, ncol = 5)

## SHAPE INDEX ## 
shape_sub_spp <- data.frame(read.csv("Results/Patch_Stats/Ordination/Shape_Index/shape_matrix/Shape_sub_matrix_12-08-19.csv"))
sub_callvulg_35_s <- lm(shape_sub_spp$Callvulg ~ env_var$distance_35m)
sub_eriovagi_35_s <- lm(shape_sub_spp$Eriovagi ~ env_var$distance_35m)
sub_junceffu_35_s <- lm(shape_sub_spp$Junceffu ~ env_var$distance_35m)
sub_juncsqua_35_s <- lm(shape_sub_spp$Juncsqua ~ env_var$distance_35m)
sub_molicaer_35_s <- lm(shape_sub_spp$Molicaer ~ env_var$distance_35m)
sub_nardstri_35_s <- lm(shape_sub_spp$Nardstri ~ env_var$distance_35m)
sub_carenigr_35_s <- lm(shape_sub_spp$Carenigr ~ env_var$distance_35m)
sub_descflex_35_s <- lm(shape_sub_spp$Descflex ~ env_var$distance_35m)
sub_galisaxa_35_s <- lm(shape_sub_spp$Galisaxa ~ env_var$distance_35m)
sub_poteerec_35_s <- lm(shape_sub_spp$Poteerec ~ env_var$distance_35m)
sub_vaccmyrt_35_s <- lm(shape_sub_spp$Vaccmyrt ~ env_var$distance_35m)

#dom_patch_callvulg_35 <- lm(shape_sub_spp$callvulg ~ env_var$length_35m)
plot(shape_sub_spp$Callvulg + shape_sub_spp$Eriovagi + shape_sub_spp$Junceffu + 
       shape_sub_spp$Juncsqua + shape_sub_spp$Molicaer + shape_sub_spp$Nardstri
     + shape_sub_spp$Carenigr + shape_sub_spp$Descflex + shape_sub_spp$Galisaxa + 
       shape_sub_spp$Poteerec + shape_sub_spp$Vaccmyrt 
     ~ env_var$distance_35m, ylim = c(1, 1.5), type = "n", xlab = "Distance from quadrat", ylab = "Shape of patches")
#points(patchno_dom_spp$callvulg + patchno_dom_spp$eriovagi + patchno_dom_spp$junceffu + 
#patchno_dom_spp$juncsqua + patchno_dom_spp$molicaer + patchno_dom_spp$nardstri
#+ patchno_dom_spp$carenigr + patchno_dom_spp$descflex + patchno_dom_spp$galisaxa + patchno_dom_spp$poteerec + 
#  patchno_dom_spp$vaccmyrt ~ env_var$distance_35m, pch = 1:11, col = 1:11, cex = 0.5)
#abline(c(callvulg_35, eriovagi_35, junceffu_35, juncsqua_35, molicaer_35, nardstri_35,
#carenigr_35, descflex_35, galisaxa_35, poteerec_35, vaccmyrt_35), col = 1:11)
abline(sub_callvulg_35_s, col = "red")
abline(sub_eriovagi_35_s, col = "green")
abline(sub_junceffu_35_s, col = "blue")
abline(sub_juncsqua_35_s, col = "yellow")
abline(sub_molicaer_35_s, col = "black")
abline(sub_nardstri_35_s, col = "orange")
abline(sub_carenigr_35_s, col = "brown")
abline(sub_descflex_35_s, col = "pink")
abline(sub_galisaxa_35_s, col = "aquamarine")
abline(sub_poteerec_35_s, col = "violet")
abline(sub_vaccmyrt_35_s, col = "gold")
legend("topright", 
       legend = c("callvulg", "eriovagi", "junceffu", "juncsqua", "molicaer", "nardstri", 
                  "carenigr", "descflex", "galisaxa", "poteerec", "vaccmyrt"), 
       col = c("red", "green", "blue", "yellow", "black", "orange", "brown", "pink", "aquamarine",
               "violet", "gold"), 
       lty = 1,
       bty = "n", 
       cex = 0.5, 
       text.col = "black", 
       horiz = F, ncol = 5)

## DOMINANT: Soil pH ####
patchno_dom_spp <- read.csv("Results/Patch_Stats/Ordination/Patchno_spp/Patchno_matrix/Patchno_matrix_dom_12-08-19.csv")
patchno_dom_spp[is.na(patchno_dom_spp)] <- 0
callvulg_ph <- lm(patchno_dom_spp$callvulg ~ env_var$soil_pH)
eriovagi_ph <- lm(patchno_dom_spp$eriovagi ~ env_var$soil_pH)
junceffu_ph <- lm(patchno_dom_spp$junceffu ~ env_var$soil_pH)
juncsqua_ph <- lm(patchno_dom_spp$juncsqua ~ env_var$soil_pH)
molicaer_ph <- lm(patchno_dom_spp$molicaer ~ env_var$soil_pH)
nardstri_ph <- lm(patchno_dom_spp$nardstri ~ env_var$soil_pH)
carenigr_ph <- lm(patchno_dom_spp$carenigr ~ env_var$soil_pH)
descflex_ph <- lm(patchno_dom_spp$descflex ~ env_var$soil_pH)
galisaxa_ph <- lm(patchno_dom_spp$galisaxa ~ env_var$soil_pH)
poteerec_ph <- lm(patchno_dom_spp$poteerec ~ env_var$soil_pH)
vaccmyrt_ph <- lm(patchno_dom_spp$vaccmyrt ~ env_var$soil_pH)

plot(patchno_dom_spp$callvulg + patchno_dom_spp$eriovagi + patchno_dom_spp$junceffu + 
            patchno_dom_spp$juncsqua + patchno_dom_spp$molicaer + patchno_dom_spp$nardstri
          + patchno_dom_spp$carenigr + patchno_dom_spp$descflex + patchno_dom_spp$galisaxa + 
            patchno_dom_spp$poteerec + patchno_dom_spp$vaccmyrt 
          ~ env_var$soil_pH, ylim = c(0, 5), type = "n", xlab = "Soil pH", ylab = "Number of patches")
     #points(patchno_dom_spp$callvulg + patchno_dom_spp$eriovagi + patchno_dom_spp$junceffu + 
     #patchno_dom_spp$juncsqua + patchno_dom_spp$molicaer + patchno_dom_spp$nardstri
     #+ patchno_dom_spp$carenigr + patchno_dom_spp$descflex + patchno_dom_spp$galisaxa + patchno_dom_spp$poteerec + 
     #  patchno_dom_spp$vaccmyrt ~ env_var$distance_phm, pch = 1:11, col = 1:11, cex = 0.5)
     #abline(c(callvulg_ph, eriovagi_ph, junceffu_ph, juncsqua_ph, molicaer_ph, nardstri_ph,
     #carenigr_ph, descflex_ph, galisaxa_ph, poteerec_ph, vaccmyrt_ph), col = 1:11)
     abline(callvulg_ph, col = "red")
     abline(eriovagi_ph, col = "green")
     abline(junceffu_ph, col = "blue")
     abline(juncsqua_ph, col = "yellow")
     abline(molicaer_ph, col = "black")
     abline(nardstri_ph, col = "orange")
     abline(carenigr_ph, col = "brown")
     abline(descflex_ph, col = "pink")
     abline(galisaxa_ph, col = "aquamarine")
     abline(poteerec_ph, col = "violet")
     abline(vaccmyrt_ph, col = "gold")
     legend("topright", 
            legend = c("callvulg", "eriovagi", "junceffu", "juncsqua", "molicaer", "nardstri", 
                       "carenigr", "descflex", "galisaxa", "poteerec", "vaccmyrt"), 
            col = c("red", "green", "blue", "yellow", "black", "orange", "brown", "pink", "aquamarine",
                    "violet", "gold"), 
            lty = 1,
            bty = "n", 
            cex = 0.5, 
            text.col = "black", 
            horiz = F, ncol = 5)

## Area ##
area_dom_spp <- read.csv("Results/Patch_Stats/Ordination/Area_spp/Area_matrix/Area_spp_dominant_4-08-19.csv")
     
callvulg_ph_a <- lm(area_dom_spp$callvulg ~ env_var$soil_pH)
eriovagi_ph_a <- lm(area_dom_spp$eriovagi ~ env_var$soil_pH)
junceffu_ph_a <- lm(area_dom_spp$junceffu ~ env_var$soil_pH)
juncsqua_ph_a <- lm(area_dom_spp$juncsqua ~ env_var$soil_pH)
molicaer_ph_a <- lm(area_dom_spp$molicaer ~ env_var$soil_pH)
nardstri_ph_a <- lm(area_dom_spp$nardstri ~ env_var$soil_pH)
carenigr_ph_a <- lm(area_dom_spp$carenigr ~ env_var$soil_pH)
descflex_ph_a <- lm(area_dom_spp$descflex ~ env_var$soil_pH)
galisaxa_ph_a <- lm(area_dom_spp$galisaxa ~ env_var$soil_pH)
poteerec_ph_a <- lm(area_dom_spp$poteerec ~ env_var$soil_pH)
vaccmyrt_ph_a <- lm(area_dom_spp$vaccmyrt ~ env_var$soil_pH)

plot(area_dom_spp$callvulg + area_dom_spp$eriovagi + area_dom_spp$junceffu + 
            area_dom_spp$juncsqua + area_dom_spp$molicaer + area_dom_spp$nardstri
          + area_dom_spp$carenigr + area_dom_spp$descflex + area_dom_spp$galisaxa + 
            area_dom_spp$poteerec + area_dom_spp$vaccmyrt 
          ~ env_var$soil_pH, ylim = c(0,50), type = "n", xlab = "Soil pH", ylab = "Area per patch")
abline(callvulg_ph_a, col = "red")
abline(eriovagi_ph_a, col = "green")
abline(junceffu_ph_a, col = "blue")
abline(juncsqua_ph_a, col = "yellow")
abline(molicaer_ph_a, col = "black")
abline(nardstri_ph_a, col = "orange")
abline(carenigr_ph_a, col = "brown")
abline(descflex_ph_a, col = "pink")
abline(galisaxa_ph_a, col = "aquamarine")
abline(poteerec_ph_a, col = "violet")
abline(vaccmyrt_ph_a, col = "gold")

legend("top", 
            legend = c("callvulg", "eriovagi", "junceffu", "juncsqua", "molicaer", "nardstri", 
                       "carenigr", "descflex", "galisaxa", "poteerec", "vaccmyrt"), 
            col = c("red", "green", "blue", "yellow", "black", "orange", "brown", "pink", "aquamarine",
                    "violet", "gold"), 
            lty = 1,
            bty = "n", 
            cex = 0.5, 
            text.col = "black", 
            horiz = F, ncol = 5)
     
## SHAPE INDEX ##
shape_dom_spp <- data.frame(read.csv("Results/Patch_Stats/Ordination/Shape_Index/shape_matrix/Shape_dom_matrix_12-08-19.csv"))
shape_dom_spp[is.na(shape_dom_spp)] <- 0
callvulg_ph_s <- lm(shape_dom_spp$callvulg ~ env_var$soil_pH)
eriovagi_ph_s <- lm(shape_dom_spp$eriovagi ~ env_var$soil_pH)
junceffu_ph_s <- lm(shape_dom_spp$junceffu ~ env_var$soil_pH)
juncsqua_ph_s <- lm(shape_dom_spp$juncsqua ~ env_var$soil_pH)
molicaer_ph_s <- lm(shape_dom_spp$molicaer ~ env_var$soil_pH)
nardstri_ph_s <- lm(shape_dom_spp$nardstri ~ env_var$soil_pH)
carenigr_ph_s <- lm(shape_dom_spp$carenigr ~ env_var$soil_pH)
descflex_ph_s <- lm(shape_dom_spp$descflex ~ env_var$soil_pH)
galisaxa_ph_s <- lm(shape_dom_spp$galisaxa ~ env_var$soil_pH)
poteerec_ph_s <- lm(shape_dom_spp$poteerec ~ env_var$soil_pH)
vaccmyrt_ph_s <- lm(shape_dom_spp$vaccmyrt ~ env_var$soil_pH)

plot(shape_dom_spp$callvulg + shape_dom_spp$eriovagi + shape_dom_spp$junceffu + 
            shape_dom_spp$juncsqua + shape_dom_spp$molicaer + shape_dom_spp$nardstri
          + shape_dom_spp$carenigr + shape_dom_spp$descflex + shape_dom_spp$galisaxa + 
            shape_dom_spp$poteerec + shape_dom_spp$vaccmyrt 
          ~ env_var$soil_pH, ylim = c(1,2), type = "n", xlab = "Soil pH", ylab = "Shape per patch")
abline(callvulg_ph_s, col = "red")
abline(eriovagi_ph_s, col = "green")
abline(junceffu_ph_s, col = "blue")
abline(juncsqua_ph_s, col = "yellow")
abline(molicaer_ph_s, col = "black")
abline(nardstri_ph_s, col = "orange")
abline(carenigr_ph_s, col = "brown")
abline(descflex_ph_s, col = "pink")
abline(galisaxa_ph_s, col = "aquamarine")
abline(poteerec_ph_s, col = "violet")
abline(vaccmyrt_ph_s, col = "gold")

legend("top", 
            legend = c("callvulg", "eriovagi", "junceffu", "juncsqua", "molicaer", "nardstri", 
                       "carenigr", "descflex", "galisaxa", "poteerec", "vaccmyrt"), 
            col = c("red", "green", "blue", "yellow", "black", "orange", "brown", "pink", "aquamarine",
                    "violet", "gold"), 
            lty = 1,
            bty = "n", 
            cex = 0.5,  
            text.col = "black", 
            horiz = F, ncol = 5)

#### SUBDOMINANT: Soil pH #####
## Patch ##
# Patches # 
patchno_sub_spp <- data.frame(read.csv("Results/Patch_Stats/Ordination/Patchno_spp/Patchno_matrix/Patchno_matrix_sub_12-08-19.csv"))
sub_callvulg_ph <- lm(patchno_sub_spp$Callvulg ~ env_var$soil_pH)
sub_eriovagi_ph <- lm(patchno_sub_spp$Eriovagi ~ env_var$soil_pH)
sub_junceffu_ph <- lm(patchno_sub_spp$Junceffu ~ env_var$soil_pH)
sub_juncsqua_ph <- lm(patchno_sub_spp$Juncsqua ~ env_var$soil_pH)
sub_molicaer_ph <- lm(patchno_sub_spp$Molicaer ~ env_var$soil_pH)
sub_nardstri_ph <- lm(patchno_sub_spp$Nardstri ~ env_var$soil_pH)
sub_carenigr_ph <- lm(patchno_sub_spp$Carenigr ~ env_var$soil_pH)
sub_descflex_ph <- lm(patchno_sub_spp$Descflex ~ env_var$soil_pH)
sub_galisaxa_ph <- lm(patchno_sub_spp$Galisaxa ~ env_var$soil_pH)
sub_poteerec_ph <- lm(patchno_sub_spp$Poteerec ~ env_var$soil_pH)
sub_vaccmyrt_ph <- lm(patchno_sub_spp$Vaccmyrt ~ env_var$soil_pH)

#sub_patch_callvulg_ph <- lm(patchno_sub_spp$callvulg ~ env_var$length_phm)
plot(patchno_sub_spp$Callvulg + patchno_sub_spp$Eriovagi + patchno_sub_spp$Junceffu + 
       patchno_sub_spp$Juncsqua + patchno_sub_spp$Molicaer + patchno_sub_spp$Nardstri
     + patchno_sub_spp$Carenigr + patchno_sub_spp$Descflex + patchno_sub_spp$Galisaxa + 
       patchno_sub_spp$Poteerec + patchno_sub_spp$Vaccmyrt 
     ~ env_var$soil_pH, ylim = c(0, 8), type = "n", xlab = "Soil pH", ylab = "Number of patches")
#points(patchno_sub_spp$callvulg + patchno_sub_spp$eriovagi + patchno_sub_spp$junceffu + 
#patchno_sub_spp$juncsqua + patchno_sub_spp$molicaer + patchno_sub_spp$nardstri
#+ patchno_sub_spp$carenigr + patchno_sub_spp$descflex + patchno_sub_spp$galisaxa + patchno_sub_spp$poteerec + 
#  patchno_sub_spp$vaccmyrt ~ env_var$soil_pH, pch = 1:11, col = 1:11, cex = 0.5)
#abline(c(callvulg_ph, eriovagi_ph, junceffu_ph, juncsqua_ph, molicaer_ph, nardstri_ph,
#carenigr_ph, descflex_ph, galisaxa_ph, poteerec_ph, vaccmyrt_ph), col = 1:11)
abline(sub_callvulg_ph, col = "red")
abline(sub_eriovagi_ph, col = "green")
abline(sub_junceffu_ph, col = "blue")
abline(sub_juncsqua_ph, col = "yellow")
abline(sub_molicaer_ph, col = "black")
abline(sub_nardstri_ph, col = "orange")
abline(sub_carenigr_ph, col = "brown")
abline(sub_descflex_ph, col = "pink")
abline(sub_galisaxa_ph, col = "aquamarine")
abline(sub_poteerec_ph, col = "violet")
abline(sub_vaccmyrt_ph, col = "gold")
legend("topright", 
       legend = c("callvulg", "eriovagi", "junceffu", "juncsqua", "molicaer", "nardstri", 
                  "carenigr", "descflex", "galisaxa", "poteerec", "vaccmyrt"), 
       col = c("red", "green", "blue", "yellow", "black", "orange", "brown", "pink", "aquamarine",
               "violet", "gold"), 
       lty = 1,
       bty = "n", 
       cex = 0.5, 
       text.col = "black", 
       horiz = F, ncol = 5)

## AREA ##
area_sub_spp <- read.csv("Results/Patch_Stats/Ordination/Area_spp/Area_matrix/Area_spp_sub_4-08-19.csv")
area_sub_spp[is.na(area_sub_spp)] <- 0
sub_callvulg_ph_a <- lm(area_sub_spp$Callvulg ~ env_var$soil_pH)
sub_eriovagi_ph_a <- lm(area_sub_spp$Eriovagi ~ env_var$soil_pH)
sub_junceffu_ph_a <- lm(area_sub_spp$Junceffu ~ env_var$soil_pH)
sub_juncsqua_ph_a <- lm(area_sub_spp$Juncsqua ~ env_var$soil_pH)
sub_molicaer_ph_a <- lm(area_sub_spp$Molicaer ~ env_var$soil_pH)
sub_nardstri_ph_a <- lm(area_sub_spp$Nardstri ~ env_var$soil_pH)
sub_carenigr_ph_a <- lm(area_sub_spp$Carenigr ~ env_var$soil_pH)
sub_descflex_ph_a <- lm(area_sub_spp$Descflex ~ env_var$soil_pH)
sub_galisaxa_ph_a <- lm(area_sub_spp$Galisaxa ~ env_var$soil_pH)
sub_poteerec_ph_a <- lm(area_sub_spp$Poteerec ~ env_var$soil_pH)
sub_vaccmyrt_ph_a <- lm(area_sub_spp$Vaccmyrt ~ env_var$soil_pH)

#dom_patch_callvulg_ph <- lm(area_sub_spp$callvulg ~ env_var$length_phm)
plot(area_sub_spp$Callvulg + area_sub_spp$Eriovagi + area_sub_spp$Junceffu + 
       area_sub_spp$Juncsqua + area_sub_spp$Molicaer + area_sub_spp$Nardstri
     + area_sub_spp$Carenigr + area_sub_spp$Descflex + area_sub_spp$Galisaxa + 
       area_sub_spp$Poteerec + area_sub_spp$Vaccmyrt 
     ~ env_var$soil_pH, ylim = c(0, 25), type = "n", xlab = "Soil pH", ylab = "Area of patches")
#points(patchno_dom_spp$callvulg + patchno_dom_spp$eriovagi + patchno_dom_spp$junceffu + 
#patchno_dom_spp$juncsqua + patchno_dom_spp$molicaer + patchno_dom_spp$nardstri
#+ patchno_dom_spp$carenigr + patchno_dom_spp$descflex + patchno_dom_spp$galisaxa + patchno_dom_spp$poteerec + 
#  patchno_dom_spp$vaccmyrt ~ env_var$soil_pH, pch = 1:11, col = 1:11, cex = 0.5)
#abline(c(callvulg_ph, eriovagi_ph, junceffu_ph, juncsqua_ph, molicaer_ph, nardstri_ph,
#carenigr_ph, descflex_ph, galisaxa_ph, poteerec_ph, vaccmyrt_ph), col = 1:11)
abline(sub_callvulg_ph_a, col = "red")
abline(sub_eriovagi_ph_a, col = "green")
abline(sub_junceffu_ph_a, col = "blue")
abline(sub_juncsqua_ph_a, col = "yellow")
abline(sub_molicaer_ph_a, col = "black")
abline(sub_nardstri_ph_a, col = "orange")
abline(sub_carenigr_ph_a, col = "brown")
abline(sub_descflex_ph_a, col = "pink")
abline(sub_galisaxa_ph_a, col = "aquamarine")
abline(sub_poteerec_ph_a, col = "violet")
abline(sub_vaccmyrt_ph_a, col = "gold")
legend("topright", 
       legend = c("callvulg", "eriovagi", "junceffu", "juncsqua", "molicaer", "nardstri", 
                  "carenigr", "descflex", "galisaxa", "poteerec", "vaccmyrt"), 
       col = c("red", "green", "blue", "yellow", "black", "orange", "brown", "pink", "aquamarine",
               "violet", "gold"), 
       lty = 1,
       bty = "n", 
       cex = 0.5, 
       text.col = "black", 
       horiz = F, ncol = 5)

## SHAPE INDEX ## 
shape_sub_spp <- data.frame(read.csv("Results/Patch_Stats/Ordination/Shape_Index/shape_matrix/Shape_sub_matrix_12-08-19.csv"))
sub_callvulg_ph_s <- lm(shape_sub_spp$Callvulg ~ env_var$soil_pH)
sub_eriovagi_ph_s <- lm(shape_sub_spp$Eriovagi ~ env_var$soil_pH)
sub_junceffu_ph_s <- lm(shape_sub_spp$Junceffu ~ env_var$soil_pH)
sub_juncsqua_ph_s <- lm(shape_sub_spp$Juncsqua ~ env_var$soil_pH)
sub_molicaer_ph_s <- lm(shape_sub_spp$Molicaer ~ env_var$soil_pH)
sub_nardstri_ph_s <- lm(shape_sub_spp$Nardstri ~ env_var$soil_pH)
sub_carenigr_ph_s <- lm(shape_sub_spp$Carenigr ~ env_var$soil_pH)
sub_descflex_ph_s <- lm(shape_sub_spp$Descflex ~ env_var$soil_pH)
sub_galisaxa_ph_s <- lm(shape_sub_spp$Galisaxa ~ env_var$soil_pH)
sub_poteerec_ph_s <- lm(shape_sub_spp$Poteerec ~ env_var$soil_pH)
sub_vaccmyrt_ph_s <- lm(shape_sub_spp$Vaccmyrt ~ env_var$soil_pH)

#dom_patch_callvulg_ph <- lm(shape_sub_spp$callvulg ~ env_var$length_phm)
plot(shape_sub_spp$Callvulg + shape_sub_spp$Eriovagi + shape_sub_spp$Junceffu + 
       shape_sub_spp$Juncsqua + shape_sub_spp$Molicaer + shape_sub_spp$Nardstri
     + shape_sub_spp$Carenigr + shape_sub_spp$Descflex + shape_sub_spp$Galisaxa + 
       shape_sub_spp$Poteerec + shape_sub_spp$Vaccmyrt 
     ~ env_var$soil_pH, ylim = c(1, 1.5), type = "n", xlab = "Soil pH", ylab = "Shape of patches")
#points(patchno_dom_spp$callvulg + patchno_dom_spp$eriovagi + patchno_dom_spp$junceffu + 
#patchno_dom_spp$juncsqua + patchno_dom_spp$molicaer + patchno_dom_spp$nardstri
#+ patchno_dom_spp$carenigr + patchno_dom_spp$descflex + patchno_dom_spp$galisaxa + patchno_dom_spp$poteerec + 
#  patchno_dom_spp$vaccmyrt ~ env_var$soil_pH, pch = 1:11, col = 1:11, cex = 0.5)
#abline(c(callvulg_ph, eriovagi_ph, junceffu_ph, juncsqua_ph, molicaer_ph, nardstri_ph,
#carenigr_ph, descflex_ph, galisaxa_ph, poteerec_ph, vaccmyrt_ph), col = 1:11)
abline(sub_callvulg_ph_s, col = "red")
abline(sub_eriovagi_ph_s, col = "green")
abline(sub_junceffu_ph_s, col = "blue")
abline(sub_juncsqua_ph_s, col = "yellow")
abline(sub_molicaer_ph_s, col = "black")
abline(sub_nardstri_ph_s, col = "orange")
abline(sub_carenigr_ph_s, col = "brown")
abline(sub_descflex_ph_s, col = "pink")
abline(sub_galisaxa_ph_s, col = "aquamarine")
abline(sub_poteerec_ph_s, col = "violet")
abline(sub_vaccmyrt_ph_s, col = "gold")
legend("topright", 
       legend = c("callvulg", "eriovagi", "junceffu", "juncsqua", "molicaer", "nardstri", 
                  "carenigr", "descflex", "galisaxa", "poteerec", "vaccmyrt"), 
       col = c("red", "green", "blue", "yellow", "black", "orange", "brown", "pink", "aquamarine",
               "violet", "gold"), 
       lty = 1,
       bty = "n", 
       cex = 0.5, 
       text.col = "black", 
       horiz = F, ncol = 5)

#### DOMINANT: Altitude ####
## Patches ###
patchno_dom_spp <- read.csv("Results/Patch_Stats/Ordination/Patchno_spp/Patchno_matrix/Patchno_matrix_dom_12-08-19.csv")
patchno_dom_spp[is.na(patchno_dom_spp)] <- 0
callvulg_alt <- lm(patchno_dom_spp$callvulg ~ env_var$Altitude)
eriovagi_alt <- lm(patchno_dom_spp$eriovagi ~ env_var$Altitude)
junceffu_alt <- lm(patchno_dom_spp$junceffu ~ env_var$Altitude)
juncsqua_alt <- lm(patchno_dom_spp$juncsqua ~ env_var$Altitude)
molicaer_alt <- lm(patchno_dom_spp$molicaer ~ env_var$Altitude)
nardstri_alt <- lm(patchno_dom_spp$nardstri ~ env_var$Altitude)
carenigr_alt <- lm(patchno_dom_spp$carenigr ~ env_var$Altitude)
descflex_alt <- lm(patchno_dom_spp$descflex ~ env_var$Altitude)
galisaxa_alt <- lm(patchno_dom_spp$galisaxa ~ env_var$Altitude)
poteerec_alt <- lm(patchno_dom_spp$poteerec ~ env_var$Altitude)
vaccmyrt_alt <- lm(patchno_dom_spp$vaccmyrt ~ env_var$Altitude)

plot(patchno_dom_spp$callvulg + patchno_dom_spp$eriovagi + patchno_dom_spp$junceffu + 
       patchno_dom_spp$juncsqua + patchno_dom_spp$molicaer + patchno_dom_spp$nardstri
     + patchno_dom_spp$carenigr + patchno_dom_spp$descflex + patchno_dom_spp$galisaxa + 
       patchno_dom_spp$poteerec + patchno_dom_spp$vaccmyrt 
     ~ env_var$Altitude, ylim = c(0, 4), type = "n", xlab = "Altitude", ylab = "Number of patches")
#points(patchno_dom_spp$callvulg + patchno_dom_spp$eriovagi + patchno_dom_spp$junceffu + 
#patchno_dom_spp$juncsqua + patchno_dom_spp$molicaer + patchno_dom_spp$nardstri
#+ patchno_dom_spp$carenigr + patchno_dom_spp$descflex + patchno_dom_spp$galisaxa + patchno_dom_spp$poteerec + 
#  patchno_dom_spp$vaccmyrt ~ env_var$distance_altm, pch = 1:11, col = 1:11, cex = 0.5)
#abline(c(callvulg_alt, eriovagi_alt, junceffu_alt, juncsqua_alt, molicaer_alt, nardstri_alt,
#carenigr_alt, descflex_alt, galisaxa_alt, poteerec_alt, vaccmyrt_alt), col = 1:11)
abline(callvulg_alt, col = "red")
abline(eriovagi_alt, col = "green")
abline(junceffu_alt, col = "blue")
abline(juncsqua_alt, col = "yellow")
abline(molicaer_alt, col = "black")
abline(nardstri_alt, col = "orange")
abline(carenigr_alt, col = "brown")
abline(descflex_alt, col = "pink")
abline(galisaxa_alt, col = "aquamarine")
abline(poteerec_alt, col = "violet")
abline(vaccmyrt_alt, col = "gold")
legend("topright", 
       legend = c("callvulg", "eriovagi", "junceffu", "juncsqua", "molicaer", "nardstri", 
                  "carenigr", "descflex", "galisaxa", "poteerec", "vaccmyrt"), 
       col = c("red", "green", "blue", "yellow", "black", "orange", "brown", "pink", "aquamarine",
               "violet", "gold"), 
       lty = 1,
       bty = "n", 
       cex = 0.5, 
       text.col = "black", 
       horiz = F, ncol = 5)

## Area ##
area_dom_spp <- read.csv("Results/Patch_Stats/Ordination/Area_spp/Area_matrix/Area_spp_dominant_4-08-19.csv")

callvulg_alt_a <- lm(area_dom_spp$callvulg ~ env_var$Altitude)
eriovagi_alt_a <- lm(area_dom_spp$eriovagi ~ env_var$Altitude)
junceffu_alt_a <- lm(area_dom_spp$junceffu ~ env_var$Altitude)
juncsqua_alt_a <- lm(area_dom_spp$juncsqua ~ env_var$Altitude)
molicaer_alt_a <- lm(area_dom_spp$molicaer ~ env_var$Altitude)
nardstri_alt_a <- lm(area_dom_spp$nardstri ~ env_var$Altitude)
carenigr_alt_a <- lm(area_dom_spp$carenigr ~ env_var$Altitude)
descflex_alt_a <- lm(area_dom_spp$descflex ~ env_var$Altitude)
galisaxa_alt_a <- lm(area_dom_spp$galisaxa ~ env_var$Altitude)
poteerec_alt_a <- lm(area_dom_spp$poteerec ~ env_var$Altitude)
vaccmyrt_alt_a <- lm(area_dom_spp$vaccmyrt ~ env_var$Altitude)

plot(area_dom_spp$callvulg + area_dom_spp$eriovagi + area_dom_spp$junceffu + 
       area_dom_spp$juncsqua + area_dom_spp$molicaer + area_dom_spp$nardstri
     + area_dom_spp$carenigr + area_dom_spp$descflex + area_dom_spp$galisaxa + 
       area_dom_spp$poteerec + area_dom_spp$vaccmyrt 
     ~ env_var$Altitude, ylim = c(0,50), type = "n", xlab = "Altitude", ylab = "Area per patch")
abline(callvulg_alt_a, col = "red")
abline(eriovagi_alt_a, col = "green")
abline(junceffu_alt_a, col = "blue")
abline(juncsqua_alt_a, col = "yellow")
abline(molicaer_alt_a, col = "black")
abline(nardstri_alt_a, col = "orange")
abline(carenigr_alt_a, col = "brown")
abline(descflex_alt_a, col = "pink")
abline(galisaxa_alt_a, col = "aquamarine")
abline(poteerec_alt_a, col = "violet")
abline(vaccmyrt_alt_a, col = "gold")

legend("top", 
       legend = c("callvulg", "eriovagi", "junceffu", "juncsqua", "molicaer", "nardstri", 
                  "carenigr", "descflex", "galisaxa", "poteerec", "vaccmyrt"), 
       col = c("red", "green", "blue", "yellow", "black", "orange", "brown", "pink", "aquamarine",
               "violet", "gold"), 
       lty = 1,
       bty = "n", 
       cex = 0.5, 
       text.col = "black", 
       horiz = F, ncol = 5)

## SHAPE INDEX ##
shape_dom_spp <- data.frame(read.csv("Results/Patch_Stats/Ordination/Shape_Index/shape_matrix/Shape_dom_matrix_12-08-19.csv"))
shape_dom_spp[is.na(shape_dom_spp)] <- 0
callvulg_alt_s <- lm(shape_dom_spp$callvulg ~ env_var$Altitude)
eriovagi_alt_s <- lm(shape_dom_spp$eriovagi ~ env_var$Altitude)
junceffu_alt_s <- lm(shape_dom_spp$junceffu ~ env_var$Altitude)
juncsqua_alt_s <- lm(shape_dom_spp$juncsqua ~ env_var$Altitude)
molicaer_alt_s <- lm(shape_dom_spp$molicaer ~ env_var$Altitude)
nardstri_alt_s <- lm(shape_dom_spp$nardstri ~ env_var$Altitude)
carenigr_alt_s <- lm(shape_dom_spp$carenigr ~ env_var$Altitude)
descflex_alt_s <- lm(shape_dom_spp$descflex ~ env_var$Altitude)
galisaxa_alt_s <- lm(shape_dom_spp$galisaxa ~ env_var$Altitude)
poteerec_alt_s <- lm(shape_dom_spp$poteerec ~ env_var$Altitude)
vaccmyrt_alt_s <- lm(shape_dom_spp$vaccmyrt ~ env_var$Altitude)

plot(shape_dom_spp$callvulg + shape_dom_spp$eriovagi + shape_dom_spp$junceffu + 
       shape_dom_spp$juncsqua + shape_dom_spp$molicaer + shape_dom_spp$nardstri
     + shape_dom_spp$carenigr + shape_dom_spp$descflex + shape_dom_spp$galisaxa + 
       shape_dom_spp$poteerec + shape_dom_spp$vaccmyrt 
     ~ env_var$Altitude, ylim = c(0.5,2), type = "n", xlab = "Altitude", ylab = "Shape per patch")
abline(callvulg_alt_s, col = "red")
abline(eriovagi_alt_s, col = "green")
abline(junceffu_alt_s, col = "blue")
abline(juncsqua_alt_s, col = "yellow")
abline(molicaer_alt_s, col = "black")
abline(nardstri_alt_s, col = "orange")
abline(carenigr_alt_s, col = "brown")
abline(descflex_alt_s, col = "pink")
abline(galisaxa_alt_s, col = "aquamarine")
abline(poteerec_alt_s, col = "violet")
abline(vaccmyrt_alt_s, col = "gold")

legend("top", 
       legend = c("callvulg", "eriovagi", "junceffu", "juncsqua", "molicaer", "nardstri", 
                  "carenigr", "descflex", "galisaxa", "poteerec", "vaccmyrt"), 
       col = c("red", "green", "blue", "yellow", "black", "orange", "brown", "pink", "aquamarine",
               "violet", "gold"), 
       lty = 1,
       bty = "n", 
       cex = 0.5,  
       text.col = "black", 
       horiz = F, ncol = 5)

#### SUBDOMINANT: Altitude ####
## Patches ##
patchno_sub_spp <- data.frame(read.csv("Results/Patch_Stats/Ordination/Patchno_spp/Patchno_matrix/Patchno_matrix_sub_12-08-19.csv"))
patchno_sub_spp[is.na(patchno_sub_spp)] <- 0
sub_callvulg_alt <- lm(patchno_sub_spp$Callvulg ~ env_var$Altitude)
sub_eriovagi_alt <- lm(patchno_sub_spp$Eriovagi ~ env_var$Altitude)
sub_junceffu_alt <- lm(patchno_sub_spp$Junceffu ~ env_var$Altitude)
sub_juncsqua_alt <- lm(patchno_sub_spp$Juncsqua ~ env_var$Altitude)
sub_molicaer_alt <- lm(patchno_sub_spp$Molicaer ~ env_var$Altitude)
sub_nardstri_alt <- lm(patchno_sub_spp$Nardstri ~ env_var$Altitude)
sub_carenigr_alt <- lm(patchno_sub_spp$Carenigr ~ env_var$Altitude)
sub_descflex_alt <- lm(patchno_sub_spp$Descflex ~ env_var$Altitude)
sub_galisaxa_alt <- lm(patchno_sub_spp$Galisaxa ~ env_var$Altitude)
sub_poteerec_alt <- lm(patchno_sub_spp$Poteerec ~ env_var$Altitude)
sub_vaccmyrt_alt <- lm(patchno_sub_spp$Vaccmyrt ~ env_var$Altitude)

#sub_patch_callvulg_alt <- lm(patchno_sub_spp$callvulg ~ env_var$length_altm)
plot(patchno_sub_spp$Callvulg + patchno_sub_spp$Eriovagi + patchno_sub_spp$Junceffu + 
       patchno_sub_spp$Juncsqua + patchno_sub_spp$Molicaer + patchno_sub_spp$Nardstri
     + patchno_sub_spp$Carenigr + patchno_sub_spp$Descflex + patchno_sub_spp$Galisaxa + 
       patchno_sub_spp$Poteerec + patchno_sub_spp$Vaccmyrt 
     ~ env_var$Altitude, ylim = c(0, 8), type = "n", xlab = "Altitude", ylab = "Number of patches")
#points(patchno_sub_spp$callvulg + patchno_sub_spp$eriovagi + patchno_sub_spp$junceffu + 
#patchno_sub_spp$juncsqua + patchno_sub_spp$molicaer + patchno_sub_spp$nardstri
#+ patchno_sub_spp$carenigr + patchno_sub_spp$descflex + patchno_sub_spp$galisaxa + patchno_sub_spp$poteerec + 
#  patchno_sub_spp$vaccmyrt ~ env_var$Altitude, pch = 1:11, col = 1:11, cex = 0.5)
#abline(c(callvulg_alt, eriovagi_alt, junceffu_alt, juncsqua_alt, molicaer_alt, nardstri_alt,
#carenigr_alt, descflex_alt, galisaxa_alt, poteerec_alt, vaccmyrt_alt), col = 1:11)
abline(sub_callvulg_alt, col = "red")
abline(sub_eriovagi_alt, col = "green")
abline(sub_junceffu_alt, col = "blue")
abline(sub_juncsqua_alt, col = "yellow")
abline(sub_molicaer_alt, col = "black")
abline(sub_nardstri_alt, col = "orange")
abline(sub_carenigr_alt, col = "brown")
abline(sub_descflex_alt, col = "pink")
abline(sub_galisaxa_alt, col = "aquamarine")
abline(sub_poteerec_alt, col = "violet")
abline(sub_vaccmyrt_alt, col = "gold")
legend("topright", 
       legend = c("callvulg", "eriovagi", "junceffu", "juncsqua", "molicaer", "nardstri", 
                  "carenigr", "descflex", "galisaxa", "poteerec", "vaccmyrt"), 
       col = c("red", "green", "blue", "yellow", "black", "orange", "brown", "pink", "aquamarine",
               "violet", "gold"), 
       lty = 1,
       bty = "n", 
       cex = 0.5, 
       text.col = "black", 
       horiz = F, ncol = 5)

## AREA ##
area_sub_spp <- read.csv("Results/Patch_Stats/Ordination/Area_spp/Area_matrix/Area_spp_sub_4-08-19.csv")
area_sub_spp[is.na(area_sub_spp)] <- 0
sub_callvulg_alt_a <- lm(area_sub_spp$Callvulg ~ env_var$Altitude)
sub_eriovagi_alt_a <- lm(area_sub_spp$Eriovagi ~ env_var$Altitude)
sub_junceffu_alt_a <- lm(area_sub_spp$Junceffu ~ env_var$Altitude)
sub_juncsqua_alt_a <- lm(area_sub_spp$Juncsqua ~ env_var$Altitude)
sub_molicaer_alt_a <- lm(area_sub_spp$Molicaer ~ env_var$Altitude)
sub_nardstri_alt_a <- lm(area_sub_spp$Nardstri ~ env_var$Altitude)
sub_carenigr_alt_a <- lm(area_sub_spp$Carenigr ~ env_var$Altitude)
sub_descflex_alt_a <- lm(area_sub_spp$Descflex ~ env_var$Altitude)
sub_galisaxa_alt_a <- lm(area_sub_spp$Galisaxa ~ env_var$Altitude)
sub_poteerec_alt_a <- lm(area_sub_spp$Poteerec ~ env_var$Altitude)
sub_vaccmyrt_alt_a <- lm(area_sub_spp$Vaccmyrt ~ env_var$Altitude)

#dom_patch_callvulg_alt <- lm(area_sub_spp$callvulg ~ env_var$length_altm)
plot(area_sub_spp$Callvulg + area_sub_spp$Eriovagi + area_sub_spp$Junceffu + 
       area_sub_spp$Juncsqua + area_sub_spp$Molicaer + area_sub_spp$Nardstri
     + area_sub_spp$Carenigr + area_sub_spp$Descflex + area_sub_spp$Galisaxa + 
       area_sub_spp$Poteerec + area_sub_spp$Vaccmyrt 
     ~ env_var$Altitude, ylim = c(0, 25), type = "n", xlab = "Altitude", ylab = "Area of patches")
#points(patchno_dom_spp$callvulg + patchno_dom_spp$eriovagi + patchno_dom_spp$junceffu + 
#patchno_dom_spp$juncsqua + patchno_dom_spp$molicaer + patchno_dom_spp$nardstri
#+ patchno_dom_spp$carenigr + patchno_dom_spp$descflex + patchno_dom_spp$galisaxa + patchno_dom_spp$poteerec + 
#  patchno_dom_spp$vaccmyrt ~ env_var$Altitude, pch = 1:11, col = 1:11, cex = 0.5)
#abline(c(callvulg_alt, eriovagi_alt, junceffu_alt, juncsqua_alt, molicaer_alt, nardstri_alt,
#carenigr_alt, descflex_alt, galisaxa_alt, poteerec_alt, vaccmyrt_alt), col = 1:11)
abline(sub_callvulg_alt_a, col = "red")
abline(sub_eriovagi_alt_a, col = "green")
abline(sub_junceffu_alt_a, col = "blue")
abline(sub_juncsqua_alt_a, col = "yellow")
abline(sub_molicaer_alt_a, col = "black")
abline(sub_nardstri_alt_a, col = "orange")
abline(sub_carenigr_alt_a, col = "brown")
abline(sub_descflex_alt_a, col = "pink")
abline(sub_galisaxa_alt_a, col = "aquamarine")
abline(sub_poteerec_alt_a, col = "violet")
abline(sub_vaccmyrt_alt_a, col = "gold")
legend("topright", 
       legend = c("callvulg", "eriovagi", "junceffu", "juncsqua", "molicaer", "nardstri", 
                  "carenigr", "descflex", "galisaxa", "poteerec", "vaccmyrt"), 
       col = c("red", "green", "blue", "yellow", "black", "orange", "brown", "pink", "aquamarine",
               "violet", "gold"), 
       lty = 1,
       bty = "n", 
       cex = 0.5, 
       text.col = "black", 
       horiz = F, ncol = 5)

## SHAPE INDEX ## 
shape_sub_spp <- data.frame(read.csv("Results/Patch_Stats/Ordination/Shape_Index/shape_matrix/Shape_sub_matrix_12-08-19.csv"))
sub_callvulg_alt_s <- lm(shape_sub_spp$Callvulg ~ env_var$Altitude)
sub_eriovagi_alt_s <- lm(shape_sub_spp$Eriovagi ~ env_var$Altitude)
sub_junceffu_alt_s <- lm(shape_sub_spp$Junceffu ~ env_var$Altitude)
sub_juncsqua_alt_s <- lm(shape_sub_spp$Juncsqua ~ env_var$Altitude)
sub_molicaer_alt_s <- lm(shape_sub_spp$Molicaer ~ env_var$Altitude)
sub_nardstri_alt_s <- lm(shape_sub_spp$Nardstri ~ env_var$Altitude)
sub_carenigr_alt_s <- lm(shape_sub_spp$Carenigr ~ env_var$Altitude)
sub_descflex_alt_s <- lm(shape_sub_spp$Descflex ~ env_var$Altitude)
sub_galisaxa_alt_s <- lm(shape_sub_spp$Galisaxa ~ env_var$Altitude)
sub_poteerec_alt_s <- lm(shape_sub_spp$Poteerec ~ env_var$Altitude)
sub_vaccmyrt_alt_s <- lm(shape_sub_spp$Vaccmyrt ~ env_var$Altitude)

#dom_patch_callvulg_alt <- lm(shape_sub_spp$callvulg ~ env_var$length_altm)
plot(shape_sub_spp$Callvulg + shape_sub_spp$Eriovagi + shape_sub_spp$Junceffu + 
       shape_sub_spp$Juncsqua + shape_sub_spp$Molicaer + shape_sub_spp$Nardstri
     + shape_sub_spp$Carenigr + shape_sub_spp$Descflex + shape_sub_spp$Galisaxa + 
       shape_sub_spp$Poteerec + shape_sub_spp$Vaccmyrt 
     ~ env_var$Altitude, ylim = c(1, 1.5), type = "n", xlab = "Altitude", ylab = "Shape of patches")
#points(patchno_dom_spp$callvulg + patchno_dom_spp$eriovagi + patchno_dom_spp$junceffu + 
#patchno_dom_spp$juncsqua + patchno_dom_spp$molicaer + patchno_dom_spp$nardstri
#+ patchno_dom_spp$carenigr + patchno_dom_spp$descflex + patchno_dom_spp$galisaxa + patchno_dom_spp$poteerec + 
#  patchno_dom_spp$vaccmyrt ~ env_var$Altitude, pch = 1:11, col = 1:11, cex = 0.5)
#abline(c(callvulg_alt, eriovagi_alt, junceffu_alt, juncsqua_alt, molicaer_alt, nardstri_alt,
#carenigr_alt, descflex_alt, galisaxa_alt, poteerec_alt, vaccmyrt_alt), col = 1:11)
abline(sub_callvulg_alt_s, col = "red")
abline(sub_eriovagi_alt_s, col = "green")
abline(sub_junceffu_alt_s, col = "blue")
abline(sub_juncsqua_alt_s, col = "yellow")
abline(sub_molicaer_alt_s, col = "black")
abline(sub_nardstri_alt_s, col = "orange")
abline(sub_carenigr_alt_s, col = "brown")
abline(sub_descflex_alt_s, col = "pink")
abline(sub_galisaxa_alt_s, col = "aquamarine")
abline(sub_poteerec_alt_s, col = "violet")
abline(sub_vaccmyrt_alt_s, col = "gold")
legend("topright", 
       legend = c("callvulg", "eriovagi", "junceffu", "juncsqua", "molicaer", "nardstri", 
                  "carenigr", "descflex", "galisaxa", "poteerec", "vaccmyrt"), 
       col = c("red", "green", "blue", "yellow", "black", "orange", "brown", "pink", "aquamarine",
               "violet", "gold"), 
       lty = 1,
       bty = "n", 
       cex = 0.5, 
       text.col = "black", 
       horiz = F, ncol = 5)

#### DOMINANT: Slope ####
## Patches ##
patchno_dom_spp <- read.csv("Results/Patch_Stats/Ordination/Patchno_spp/Patchno_matrix/Patchno_matrix_dom_12-08-19.csv")
patchno_dom_spp[is.na(patchno_dom_spp)] <- 0
callvulg_slope <- lm(patchno_dom_spp$callvulg ~ env_var$slope)
eriovagi_slope <- lm(patchno_dom_spp$eriovagi ~ env_var$slope)
junceffu_slope <- lm(patchno_dom_spp$junceffu ~ env_var$slope)
juncsqua_slope <- lm(patchno_dom_spp$juncsqua ~ env_var$slope)
molicaer_slope <- lm(patchno_dom_spp$molicaer ~ env_var$slope)
nardstri_slope <- lm(patchno_dom_spp$nardstri ~ env_var$slope)
carenigr_slope <- lm(patchno_dom_spp$carenigr ~ env_var$slope)
descflex_slope <- lm(patchno_dom_spp$descflex ~ env_var$slope)
galisaxa_slope <- lm(patchno_dom_spp$galisaxa ~ env_var$slope)
poteerec_slope <- lm(patchno_dom_spp$poteerec ~ env_var$slope)
vaccmyrt_slope <- lm(patchno_dom_spp$vaccmyrt ~ env_var$slope)

plot(patchno_dom_spp$callvulg + patchno_dom_spp$eriovagi + patchno_dom_spp$junceffu + 
       patchno_dom_spp$juncsqua + patchno_dom_spp$molicaer + patchno_dom_spp$nardstri
     + patchno_dom_spp$carenigr + patchno_dom_spp$descflex + patchno_dom_spp$galisaxa + 
       patchno_dom_spp$poteerec + patchno_dom_spp$vaccmyrt 
     ~ env_var$slope, ylim = c(0, 5), type = "n", xlab = "Slope", ylab = "Number of patches")
#points(patchno_dom_spp$callvulg + patchno_dom_spp$eriovagi + patchno_dom_spp$junceffu + 
#patchno_dom_spp$juncsqua + patchno_dom_spp$molicaer + patchno_dom_spp$nardstri
#+ patchno_dom_spp$carenigr + patchno_dom_spp$descflex + patchno_dom_spp$galisaxa + patchno_dom_spp$poteerec + 
#  patchno_dom_spp$vaccmyrt ~ env_var$distance_slopem, pch = 1:11, col = 1:11, cex = 0.5)
#abline(c(callvulg_slope, eriovagi_slope, junceffu_slope, juncsqua_slope, molicaer_slope, nardstri_slope,
#carenigr_slope, descflex_slope, galisaxa_slope, poteerec_slope, vaccmyrt_slope), col = 1:11)
abline(callvulg_slope, col = "red")
abline(eriovagi_slope, col = "green")
abline(junceffu_slope, col = "blue")
abline(juncsqua_slope, col = "yellow")
abline(molicaer_slope, col = "black")
abline(nardstri_slope, col = "orange")
abline(carenigr_slope, col = "brown")
abline(descflex_slope, col = "pink")
abline(galisaxa_slope, col = "aquamarine")
abline(poteerec_slope, col = "violet")
abline(vaccmyrt_slope, col = "gold")
legend("topright", 
       legend = c("callvulg", "eriovagi", "junceffu", "juncsqua", "molicaer", "nardstri", 
                  "carenigr", "descflex", "galisaxa", "poteerec", "vaccmyrt"), 
       col = c("red", "green", "blue", "yellow", "black", "orange", "brown", "pink", "aquamarine",
               "violet", "gold"), 
       lty = 1,
       bty = "n", 
       cex = 0.5, 
       text.col = "black", 
       horiz = F, ncol = 5)

## Area ##
area_dom_spp <- read.csv("Results/Patch_Stats/Ordination/Area_spp/Area_matrix/Area_spp_dominant_4-08-19.csv")
area_dom_spp[is.na(area_dom_spp)] <- 0
callvulg_slope_a <- lm(area_dom_spp$callvulg ~ env_var$slope)
eriovagi_slope_a <- lm(area_dom_spp$eriovagi ~ env_var$slope)
junceffu_slope_a <- lm(area_dom_spp$junceffu ~ env_var$slope)
juncsqua_slope_a <- lm(area_dom_spp$juncsqua ~ env_var$slope)
molicaer_slope_a <- lm(area_dom_spp$molicaer ~ env_var$slope)
nardstri_slope_a <- lm(area_dom_spp$nardstri ~ env_var$slope)
carenigr_slope_a <- lm(area_dom_spp$carenigr ~ env_var$slope)
descflex_slope_a <- lm(area_dom_spp$descflex ~ env_var$slope)
galisaxa_slope_a <- lm(area_dom_spp$galisaxa ~ env_var$slope)
poteerec_slope_a <- lm(area_dom_spp$poteerec ~ env_var$slope)
vaccmyrt_slope_a <- lm(area_dom_spp$vaccmyrt ~ env_var$slope)

plot(area_dom_spp$callvulg + area_dom_spp$eriovagi + area_dom_spp$junceffu + 
       area_dom_spp$juncsqua + area_dom_spp$molicaer + area_dom_spp$nardstri
     + area_dom_spp$carenigr + area_dom_spp$descflex + area_dom_spp$galisaxa + 
       area_dom_spp$poteerec + area_dom_spp$vaccmyrt 
     ~ env_var$slope, ylim = c(0,50), type = "n", xlab = "Slope", ylab = "Area per patch")
abline(callvulg_slope_a, col = "red")
abline(eriovagi_slope_a, col = "green")
abline(junceffu_slope_a, col = "blue")
abline(juncsqua_slope_a, col = "yellow")
abline(molicaer_slope_a, col = "black")
abline(nardstri_slope_a, col = "orange")
abline(carenigr_slope_a, col = "brown")
abline(descflex_slope_a, col = "pink")
abline(galisaxa_slope_a, col = "aquamarine")
abline(poteerec_slope_a, col = "violet")
abline(vaccmyrt_slope_a, col = "gold")

legend("top", 
       legend = c("callvulg", "eriovagi", "junceffu", "juncsqua", "molicaer", "nardstri", 
                  "carenigr", "descflex", "galisaxa", "poteerec", "vaccmyrt"), 
       col = c("red", "green", "blue", "yellow", "black", "orange", "brown", "pink", "aquamarine",
               "violet", "gold"), 
       lty = 1,
       bty = "n", 
       cex = 0.5, 
       text.col = "black", 
       horiz = F, ncol = 5)

## SHAPE INDEX ##
shape_dom_spp <- data.frame(read.csv("Results/Patch_Stats/Ordination/Shape_Index/shape_matrix/Shape_dom_matrix_12-08-19.csv"))
shape_dom_spp[is.na(shape_dom_spp)] <- 0
callvulg_slope_s <- lm(shape_dom_spp$callvulg ~ env_var$slope)
eriovagi_slope_s <- lm(shape_dom_spp$eriovagi ~ env_var$slope)
junceffu_slope_s <- lm(shape_dom_spp$junceffu ~ env_var$slope)
juncsqua_slope_s <- lm(shape_dom_spp$juncsqua ~ env_var$slope)
molicaer_slope_s <- lm(shape_dom_spp$molicaer ~ env_var$slope)
nardstri_slope_s <- lm(shape_dom_spp$nardstri ~ env_var$slope)
carenigr_slope_s <- lm(shape_dom_spp$carenigr ~ env_var$slope)
descflex_slope_s <- lm(shape_dom_spp$descflex ~ env_var$slope)
galisaxa_slope_s <- lm(shape_dom_spp$galisaxa ~ env_var$slope)
poteerec_slope_s <- lm(shape_dom_spp$poteerec ~ env_var$slope)
vaccmyrt_slope_s <- lm(shape_dom_spp$vaccmyrt ~ env_var$slope)

plot(shape_dom_spp$callvulg + shape_dom_spp$eriovagi + shape_dom_spp$junceffu + 
       shape_dom_spp$juncsqua + shape_dom_spp$molicaer + shape_dom_spp$nardstri
     + shape_dom_spp$carenigr + shape_dom_spp$descflex + shape_dom_spp$galisaxa + 
       shape_dom_spp$poteerec + shape_dom_spp$vaccmyrt 
     ~ env_var$slope, ylim = c(1,2), type = "n", xlab = "Slope", ylab = "Shape per patch")
abline(callvulg_slope_s, col = "red")
abline(eriovagi_slope_s, col = "green")
abline(junceffu_slope_s, col = "blue")
abline(juncsqua_slope_s, col = "yellow")
abline(molicaer_slope_s, col = "black")
abline(nardstri_slope_s, col = "orange")
abline(carenigr_slope_s, col = "brown")
abline(descflex_slope_s, col = "pink")
abline(galisaxa_slope_s, col = "aquamarine")
abline(poteerec_slope_s, col = "violet")
abline(vaccmyrt_slope_s, col = "gold")

legend("top", 
       legend = c("callvulg", "eriovagi", "junceffu", "juncsqua", "molicaer", "nardstri", 
                  "carenigr", "descflex", "galisaxa", "poteerec", "vaccmyrt"), 
       col = c("red", "green", "blue", "yellow", "black", "orange", "brown", "pink", "aquamarine",
               "violet", "gold"), 
       lty = 1,
       bty = "n", 
       cex = 0.5,  
       text.col = "black", 
       horiz = F, ncol = 5)

#### SUBDOMINANT: Slope ####
## Patches ##
patchno_sub_spp <- data.frame(read.csv("Results/Patch_Stats/Ordination/Patchno_spp/Patchno_matrix/Patchno_matrix_sub_12-08-19.csv"))
patchno_sub_spp[is.na(patchno_sub_spp)] <- 0
sub_callvulg_slope <- lm(patchno_sub_spp$Callvulg ~ env_var$slope)
sub_eriovagi_slope <- lm(patchno_sub_spp$Eriovagi ~ env_var$slope)
sub_junceffu_slope <- lm(patchno_sub_spp$Junceffu ~ env_var$slope)
sub_juncsqua_slope <- lm(patchno_sub_spp$Juncsqua ~ env_var$slope)
sub_molicaer_slope <- lm(patchno_sub_spp$Molicaer ~ env_var$slope)
sub_nardstri_slope <- lm(patchno_sub_spp$Nardstri ~ env_var$slope)
sub_carenigr_slope <- lm(patchno_sub_spp$Carenigr ~ env_var$slope)
sub_descflex_slope <- lm(patchno_sub_spp$Descflex ~ env_var$slope)
sub_galisaxa_slope <- lm(patchno_sub_spp$Galisaxa ~ env_var$slope)
sub_poteerec_slope <- lm(patchno_sub_spp$Poteerec ~ env_var$slope)
sub_vaccmyrt_slope <- lm(patchno_sub_spp$Vaccmyrt ~ env_var$slope)

#sub_patch_callvulg_slope <- lm(patchno_sub_spp$callvulg ~ env_var$length_slopem)
plot(patchno_sub_spp$Callvulg + patchno_sub_spp$Eriovagi + patchno_sub_spp$Junceffu + 
       patchno_sub_spp$Juncsqua + patchno_sub_spp$Molicaer + patchno_sub_spp$Nardstri
     + patchno_sub_spp$Carenigr + patchno_sub_spp$Descflex + patchno_sub_spp$Galisaxa + 
       patchno_sub_spp$Poteerec + patchno_sub_spp$Vaccmyrt 
     ~ env_var$slope, ylim = c(0, 8), type = "n", xlab = "Slope", ylab = "Number of patches")
#points(patchno_sub_spp$callvulg + patchno_sub_spp$eriovagi + patchno_sub_spp$junceffu + 
#patchno_sub_spp$juncsqua + patchno_sub_spp$molicaer + patchno_sub_spp$nardstri
#+ patchno_sub_spp$carenigr + patchno_sub_spp$descflex + patchno_sub_spp$galisaxa + patchno_sub_spp$poteerec + 
#  patchno_sub_spp$vaccmyrt ~ env_var$slope, pch = 1:11, col = 1:11, cex = 0.5)
#abline(c(callvulg_slope, eriovagi_slope, junceffu_slope, juncsqua_slope, molicaer_slope, nardstri_slope,
#carenigr_slope, descflex_slope, galisaxa_slope, poteerec_slope, vaccmyrt_slope), col = 1:11)
abline(sub_callvulg_slope, col = "red")
abline(sub_eriovagi_slope, col = "green")
abline(sub_junceffu_slope, col = "blue")
abline(sub_juncsqua_slope, col = "yellow")
abline(sub_molicaer_slope, col = "black")
abline(sub_nardstri_slope, col = "orange")
abline(sub_carenigr_slope, col = "brown")
abline(sub_descflex_slope, col = "pink")
abline(sub_galisaxa_slope, col = "aquamarine")
abline(sub_poteerec_slope, col = "violet")
abline(sub_vaccmyrt_slope, col = "gold")
legend("topright", 
       legend = c("callvulg", "eriovagi", "junceffu", "juncsqua", "molicaer", "nardstri", 
                  "carenigr", "descflex", "galisaxa", "poteerec", "vaccmyrt"), 
       col = c("red", "green", "blue", "yellow", "black", "orange", "brown", "pink", "aquamarine",
               "violet", "gold"), 
       lty = 1,
       bty = "n", 
       cex = 0.5, 
       text.col = "black", 
       horiz = F, ncol = 5)

## AREA ##
area_sub_spp <- read.csv("Results/Patch_Stats/Ordination/Area_spp/Area_matrix/Area_spp_sub_4-08-19.csv")
area_sub_spp[is.na(area_sub_spp)] <- 0
sub_callvulg_slope_a <- lm(area_sub_spp$Callvulg ~ env_var$slope)
sub_eriovagi_slope_a <- lm(area_sub_spp$Eriovagi ~ env_var$slope)
sub_junceffu_slope_a <- lm(area_sub_spp$Junceffu ~ env_var$slope)
sub_juncsqua_slope_a <- lm(area_sub_spp$Juncsqua ~ env_var$slope)
sub_molicaer_slope_a <- lm(area_sub_spp$Molicaer ~ env_var$slope)
sub_nardstri_slope_a <- lm(area_sub_spp$Nardstri ~ env_var$slope)
sub_carenigr_slope_a <- lm(area_sub_spp$Carenigr ~ env_var$slope)
sub_descflex_slope_a <- lm(area_sub_spp$Descflex ~ env_var$slope)
sub_galisaxa_slope_a <- lm(area_sub_spp$Galisaxa ~ env_var$slope)
sub_poteerec_slope_a <- lm(area_sub_spp$Poteerec ~ env_var$slope)
sub_vaccmyrt_slope_a <- lm(area_sub_spp$Vaccmyrt ~ env_var$slope)

#dom_patch_callvulg_slope <- lm(area_sub_spp$callvulg ~ env_var$length_slopem)
plot(area_sub_spp$Callvulg + area_sub_spp$Eriovagi + area_sub_spp$Junceffu + 
       area_sub_spp$Juncsqua + area_sub_spp$Molicaer + area_sub_spp$Nardstri
     + area_sub_spp$Carenigr + area_sub_spp$Descflex + area_sub_spp$Galisaxa + 
       area_sub_spp$Poteerec + area_sub_spp$Vaccmyrt 
     ~ env_var$slope, ylim = c(0, 25), type = "n", xlab = "Slope", ylab = "Area of patches")
#points(patchno_dom_spp$callvulg + patchno_dom_spp$eriovagi + patchno_dom_spp$junceffu + 
#patchno_dom_spp$juncsqua + patchno_dom_spp$molicaer + patchno_dom_spp$nardstri
#+ patchno_dom_spp$carenigr + patchno_dom_spp$descflex + patchno_dom_spp$galisaxa + patchno_dom_spp$poteerec + 
#  patchno_dom_spp$vaccmyrt ~ env_var$slope, pch = 1:11, col = 1:11, cex = 0.5)
#abline(c(callvulg_slope, eriovagi_slope, junceffu_slope, juncsqua_slope, molicaer_slope, nardstri_slope,
#carenigr_slope, descflex_slope, galisaxa_slope, poteerec_slope, vaccmyrt_slope), col = 1:11)
abline(sub_callvulg_slope_a, col = "red")
abline(sub_eriovagi_slope_a, col = "green")
abline(sub_junceffu_slope_a, col = "blue")
abline(sub_juncsqua_slope_a, col = "yellow")
abline(sub_molicaer_slope_a, col = "black")
abline(sub_nardstri_slope_a, col = "orange")
abline(sub_carenigr_slope_a, col = "brown")
abline(sub_descflex_slope_a, col = "pink")
abline(sub_galisaxa_slope_a, col = "aquamarine")
abline(sub_poteerec_slope_a, col = "violet")
abline(sub_vaccmyrt_slope_a, col = "gold")
legend("topright", 
       legend = c("callvulg", "eriovagi", "junceffu", "juncsqua", "molicaer", "nardstri", 
                  "carenigr", "descflex", "galisaxa", "poteerec", "vaccmyrt"), 
       col = c("red", "green", "blue", "yellow", "black", "orange", "brown", "pink", "aquamarine",
               "violet", "gold"), 
       lty = 1,
       bty = "n", 
       cex = 0.5, 
       text.col = "black", 
       horiz = F, ncol = 5)

## SHAPE INDEX ## 
shape_sub_spp <- data.frame(read.csv("Results/Patch_Stats/Ordination/Shape_Index/shape_matrix/Shape_sub_matrix_12-08-19.csv"))
sub_callvulg_slope_s <- lm(shape_sub_spp$Callvulg ~ env_var$slope)
sub_eriovagi_slope_s <- lm(shape_sub_spp$Eriovagi ~ env_var$slope)
sub_junceffu_slope_s <- lm(shape_sub_spp$Junceffu ~ env_var$slope)
sub_juncsqua_slope_s <- lm(shape_sub_spp$Juncsqua ~ env_var$slope)
sub_molicaer_slope_s <- lm(shape_sub_spp$Molicaer ~ env_var$slope)
sub_nardstri_slope_s <- lm(shape_sub_spp$Nardstri ~ env_var$slope)
sub_carenigr_slope_s <- lm(shape_sub_spp$Carenigr ~ env_var$slope)
sub_descflex_slope_s <- lm(shape_sub_spp$Descflex ~ env_var$slope)
sub_galisaxa_slope_s <- lm(shape_sub_spp$Galisaxa ~ env_var$slope)
sub_poteerec_slope_s <- lm(shape_sub_spp$Poteerec ~ env_var$slope)
sub_vaccmyrt_slope_s <- lm(shape_sub_spp$Vaccmyrt ~ env_var$slope)

#dom_patch_callvulg_slope <- lm(shape_sub_spp$callvulg ~ env_var$length_slopem)
plot(shape_sub_spp$Callvulg + shape_sub_spp$Eriovagi + shape_sub_spp$Junceffu + 
       shape_sub_spp$Juncsqua + shape_sub_spp$Molicaer + shape_sub_spp$Nardstri
     + shape_sub_spp$Carenigr + shape_sub_spp$Descflex + shape_sub_spp$Galisaxa + 
       shape_sub_spp$Poteerec + shape_sub_spp$Vaccmyrt 
     ~ env_var$slope, ylim = c(1, 1.5), type = "n", xlab = "Slope", ylab = "Shape of patches")
#points(patchno_dom_spp$callvulg + patchno_dom_spp$eriovagi + patchno_dom_spp$junceffu + 
#patchno_dom_spp$juncsqua + patchno_dom_spp$molicaer + patchno_dom_spp$nardstri
#+ patchno_dom_spp$carenigr + patchno_dom_spp$descflex + patchno_dom_spp$galisaxa + patchno_dom_spp$poteerec + 
#  patchno_dom_spp$vaccmyrt ~ env_var$slope, pch = 1:11, col = 1:11, cex = 0.5)
#abline(c(callvulg_slope, eriovagi_slope, junceffu_slope, juncsqua_slope, molicaer_slope, nardstri_slope,
#carenigr_slope, descflex_slope, galisaxa_slope, poteerec_slope, vaccmyrt_slope), col = 1:11)
abline(sub_callvulg_slope_s, col = "red")
abline(sub_eriovagi_slope_s, col = "green")
abline(sub_junceffu_slope_s, col = "blue")
abline(sub_juncsqua_slope_s, col = "yellow")
abline(sub_molicaer_slope_s, col = "black")
abline(sub_nardstri_slope_s, col = "orange")
abline(sub_carenigr_slope_s, col = "brown")
abline(sub_descflex_slope_s, col = "pink")
abline(sub_galisaxa_slope_s, col = "aquamarine")
abline(sub_poteerec_slope_s, col = "violet")
abline(sub_vaccmyrt_slope_s, col = "gold")
legend("topright", 
       legend = c("callvulg", "eriovagi", "junceffu", "juncsqua", "molicaer", "nardstri", 
                  "carenigr", "descflex", "galisaxa", "poteerec", "vaccmyrt"), 
       col = c("red", "green", "blue", "yellow", "black", "orange", "brown", "pink", "aquamarine",
               "violet", "gold"), 
       lty = 1,
       bty = "n", 
       cex = 0.5, 
       text.col = "black", 
       horiz = F, ncol = 5)

#### DOMINANT: % water content #####
## Patches ##
patchno_dom_spp <- read.csv("Results/Patch_Stats/Ordination/Patchno_spp/Patchno_matrix/Patchno_matrix_dom_12-08-19.csv")
patchno_dom_spp[is.na(patchno_dom_spp)] <- 0
callvulg_pct_water <- lm(patchno_dom_spp$callvulg ~ env_var$pct_water)
eriovagi_pct_water <- lm(patchno_dom_spp$eriovagi ~ env_var$pct_water)
junceffu_pct_water <- lm(patchno_dom_spp$junceffu ~ env_var$pct_water)
juncsqua_pct_water <- lm(patchno_dom_spp$juncsqua ~ env_var$pct_water)
molicaer_pct_water <- lm(patchno_dom_spp$molicaer ~ env_var$pct_water)
nardstri_pct_water <- lm(patchno_dom_spp$nardstri ~ env_var$pct_water)
carenigr_pct_water <- lm(patchno_dom_spp$carenigr ~ env_var$pct_water)
descflex_pct_water <- lm(patchno_dom_spp$descflex ~ env_var$pct_water)
galisaxa_pct_water <- lm(patchno_dom_spp$galisaxa ~ env_var$pct_water)
poteerec_pct_water <- lm(patchno_dom_spp$poteerec ~ env_var$pct_water)
vaccmyrt_pct_water <- lm(patchno_dom_spp$vaccmyrt ~ env_var$pct_water)

plot(patchno_dom_spp$callvulg + patchno_dom_spp$eriovagi + patchno_dom_spp$junceffu + 
       patchno_dom_spp$juncsqua + patchno_dom_spp$molicaer + patchno_dom_spp$nardstri
     + patchno_dom_spp$carenigr + patchno_dom_spp$descflex + patchno_dom_spp$galisaxa + 
       patchno_dom_spp$poteerec + patchno_dom_spp$vaccmyrt 
     ~ env_var$pct_water, ylim = c(0, 4), type = "n", xlab = "pct_water", ylab = "Number of patches")
#points(patchno_dom_spp$callvulg + patchno_dom_spp$eriovagi + patchno_dom_spp$junceffu + 
#patchno_dom_spp$juncsqua + patchno_dom_spp$molicaer + patchno_dom_spp$nardstri
#+ patchno_dom_spp$carenigr + patchno_dom_spp$descflex + patchno_dom_spp$galisaxa + patchno_dom_spp$poteerec + 
#  patchno_dom_spp$vaccmyrt ~ env_var$distance_pct_waterm, pch = 1:11, col = 1:11, cex = 0.5)
#abline(c(callvulg_pct_water, eriovagi_pct_water, junceffu_pct_water, juncsqua_pct_water, molicaer_pct_water, nardstri_pct_water,
#carenigr_pct_water, descflex_pct_water, galisaxa_pct_water, poteerec_pct_water, vaccmyrt_pct_water), col = 1:11)
abline(callvulg_pct_water, col = "red")
abline(eriovagi_pct_water, col = "green")
abline(junceffu_pct_water, col = "blue")
abline(juncsqua_pct_water, col = "yellow")
abline(molicaer_pct_water, col = "black")
abline(nardstri_pct_water, col = "orange")
abline(carenigr_pct_water, col = "brown")
abline(descflex_pct_water, col = "pink")
abline(galisaxa_pct_water, col = "aquamarine")
abline(poteerec_pct_water, col = "violet")
abline(vaccmyrt_pct_water, col = "gold")
legend("topright", 
       legend = c("callvulg", "eriovagi", "junceffu", "juncsqua", "molicaer", "nardstri", 
                  "carenigr", "descflex", "galisaxa", "poteerec", "vaccmyrt"), 
       col = c("red", "green", "blue", "yellow", "black", "orange", "brown", "pink", "aquamarine",
               "violet", "gold"), 
       lty = 1,
       bty = "n", 
       cex = 0.5, 
       text.col = "black", 
       horiz = F, ncol = 5)

## Area ##
area_dom_spp <- read.csv("Results/Patch_Stats/Ordination/Area_spp/Area_matrix/Area_spp_dominant_4-08-19.csv")

callvulg_pct_water_a <- lm(area_dom_spp$callvulg ~ env_var$pct_water)
eriovagi_pct_water_a <- lm(area_dom_spp$eriovagi ~ env_var$pct_water)
junceffu_pct_water_a <- lm(area_dom_spp$junceffu ~ env_var$pct_water)
juncsqua_pct_water_a <- lm(area_dom_spp$juncsqua ~ env_var$pct_water)
molicaer_pct_water_a <- lm(area_dom_spp$molicaer ~ env_var$pct_water)
nardstri_pct_water_a <- lm(area_dom_spp$nardstri ~ env_var$pct_water)
carenigr_pct_water_a <- lm(area_dom_spp$carenigr ~ env_var$pct_water)
descflex_pct_water_a <- lm(area_dom_spp$descflex ~ env_var$pct_water)
galisaxa_pct_water_a <- lm(area_dom_spp$galisaxa ~ env_var$pct_water)
poteerec_pct_water_a <- lm(area_dom_spp$poteerec ~ env_var$pct_water)
vaccmyrt_pct_water_a <- lm(area_dom_spp$vaccmyrt ~ env_var$pct_water)

plot(area_dom_spp$callvulg + area_dom_spp$eriovagi + area_dom_spp$junceffu + 
       area_dom_spp$juncsqua + area_dom_spp$molicaer + area_dom_spp$nardstri
     + area_dom_spp$carenigr + area_dom_spp$descflex + area_dom_spp$galisaxa + 
       area_dom_spp$poteerec + area_dom_spp$vaccmyrt 
     ~ env_var$pct_water, ylim = c(0, 60), type = "n", xlab = "pct_water", ylab = "Area per patch")
abline(callvulg_pct_water_a, col = "red")
abline(eriovagi_pct_water_a, col = "green")
abline(junceffu_pct_water_a, col = "blue")
abline(juncsqua_pct_water_a, col = "yellow")
abline(molicaer_pct_water_a, col = "black")
abline(nardstri_pct_water_a, col = "orange")
abline(carenigr_pct_water_a, col = "brown")
abline(descflex_pct_water_a, col = "pink")
abline(galisaxa_pct_water_a, col = "aquamarine")
abline(poteerec_pct_water_a, col = "violet")
abline(vaccmyrt_pct_water_a, col = "gold")

legend("top", 
       legend = c("callvulg", "eriovagi", "junceffu", "juncsqua", "molicaer", "nardstri", 
                  "carenigr", "descflex", "galisaxa", "poteerec", "vaccmyrt"), 
       col = c("red", "green", "blue", "yellow", "black", "orange", "brown", "pink", "aquamarine",
               "violet", "gold"), 
       lty = 1,
       bty = "n", 
       cex = 0.5, 
       text.col = "black", 
       horiz = F, ncol = 5)

## SHAPE INDEX ##
shape_dom_spp <- data.frame(read.csv("Results/Patch_Stats/Ordination/Shape_Index/shape_matrix/Shape_dom_matrix_12-08-19.csv"))
#shape_dom_spp[is.na(shape_dom_spp)] <- 0
callvulg_pct_water_s <- lm(shape_dom_spp$callvulg ~ env_var$pct_water)
eriovagi_pct_water_s <- lm(shape_dom_spp$eriovagi ~ env_var$pct_water)
junceffu_pct_water_s <- lm(shape_dom_spp$junceffu ~ env_var$pct_water)
juncsqua_pct_water_s <- lm(shape_dom_spp$juncsqua ~ env_var$pct_water)
molicaer_pct_water_s <- lm(shape_dom_spp$molicaer ~ env_var$pct_water)
nardstri_pct_water_s <- lm(shape_dom_spp$nardstri ~ env_var$pct_water)
carenigr_pct_water_s <- lm(shape_dom_spp$carenigr ~ env_var$pct_water)
descflex_pct_water_s <- lm(shape_dom_spp$descflex ~ env_var$pct_water)
galisaxa_pct_water_s <- lm(shape_dom_spp$galisaxa ~ env_var$pct_water)
poteerec_pct_water_s <- lm(shape_dom_spp$poteerec ~ env_var$pct_water)
vaccmyrt_pct_water_s <- lm(shape_dom_spp$vaccmyrt ~ env_var$pct_water)

plot(shape_dom_spp$callvulg + shape_dom_spp$eriovagi + shape_dom_spp$junceffu + 
       shape_dom_spp$juncsqua + shape_dom_spp$molicaer + shape_dom_spp$nardstri
     + shape_dom_spp$carenigr + shape_dom_spp$descflex + shape_dom_spp$galisaxa + 
       shape_dom_spp$poteerec + shape_dom_spp$vaccmyrt 
     ~ env_var$pct_water, ylim = c(0.5,2), type = "n", xlab = "pct_water", ylab = "Shape per patch")
abline(callvulg_pct_water_s, col = "red")
abline(eriovagi_pct_water_s, col = "green")
abline(junceffu_pct_water_s, col = "blue")
abline(juncsqua_pct_water_s, col = "yellow")
abline(molicaer_pct_water_s, col = "black")
abline(nardstri_pct_water_s, col = "orange")
abline(carenigr_pct_water_s, col = "brown")
abline(descflex_pct_water_s, col = "pink")
abline(galisaxa_pct_water_s, col = "aquamarine")
abline(poteerec_pct_water_s, col = "violet")
abline(vaccmyrt_pct_water_s, col = "gold")

legend("top", 
       legend = c("callvulg", "eriovagi", "junceffu", "juncsqua", "molicaer", "nardstri", 
                  "carenigr", "descflex", "galisaxa", "poteerec", "vaccmyrt"), 
       col = c("red", "green", "blue", "yellow", "black", "orange", "brown", "pink", "aquamarine",
               "violet", "gold"), 
       lty = 1,
       bty = "n", 
       cex = 0.5,  
       text.col = "black", 
       horiz = F, ncol = 5)

#### SUBDOMINANT: % water content ####
## Patches ##
patchno_sub_spp <- data.frame(read.csv("Results/Patch_Stats/Ordination/Patchno_spp/Patchno_matrix/Patchno_matrix_sub_12-08-19.csv"))
patchno_sub_spp[is.na(patchno_sub_spp)] <- 0
sub_callvulg_pct_water <- lm(patchno_sub_spp$Callvulg ~ env_var$pct_water)
sub_eriovagi_pct_water <- lm(patchno_sub_spp$Eriovagi ~ env_var$pct_water)
sub_junceffu_pct_water <- lm(patchno_sub_spp$Junceffu ~ env_var$pct_water)
sub_juncsqua_pct_water <- lm(patchno_sub_spp$Juncsqua ~ env_var$pct_water)
sub_molicaer_pct_water <- lm(patchno_sub_spp$Molicaer ~ env_var$pct_water)
sub_nardstri_pct_water <- lm(patchno_sub_spp$Nardstri ~ env_var$pct_water)
sub_carenigr_pct_water <- lm(patchno_sub_spp$Carenigr ~ env_var$pct_water)
sub_descflex_pct_water <- lm(patchno_sub_spp$Descflex ~ env_var$pct_water)
sub_galisaxa_pct_water <- lm(patchno_sub_spp$Galisaxa ~ env_var$pct_water)
sub_poteerec_pct_water <- lm(patchno_sub_spp$Poteerec ~ env_var$pct_water)
sub_vaccmyrt_pct_water <- lm(patchno_sub_spp$Vaccmyrt ~ env_var$pct_water)

#sub_patch_callvulg_pct_water <- lm(patchno_sub_spp$callvulg ~ env_var$length_pct_waterm)
plot(patchno_sub_spp$Callvulg + patchno_sub_spp$Eriovagi + patchno_sub_spp$Junceffu + 
       patchno_sub_spp$Juncsqua + patchno_sub_spp$Molicaer + patchno_sub_spp$Nardstri
     + patchno_sub_spp$Carenigr + patchno_sub_spp$Descflex + patchno_sub_spp$Galisaxa + 
       patchno_sub_spp$Poteerec + patchno_sub_spp$Vaccmyrt 
     ~ env_var$pct_water, ylim = c(0, 8), type = "n", xlab = "pct_water", ylab = "Number of patches")
#points(patchno_sub_spp$callvulg + patchno_sub_spp$eriovagi + patchno_sub_spp$junceffu + 
#patchno_sub_spp$juncsqua + patchno_sub_spp$molicaer + patchno_sub_spp$nardstri
#+ patchno_sub_spp$carenigr + patchno_sub_spp$descflex + patchno_sub_spp$galisaxa + patchno_sub_spp$poteerec + 
#  patchno_sub_spp$vaccmyrt ~ env_var$pct_water, pch = 1:11, col = 1:11, cex = 0.5)
#abline(c(callvulg_pct_water, eriovagi_pct_water, junceffu_pct_water, juncsqua_pct_water, molicaer_pct_water, nardstri_pct_water,
#carenigr_pct_water, descflex_pct_water, galisaxa_pct_water, poteerec_pct_water, vaccmyrt_pct_water), col = 1:11)
abline(sub_callvulg_pct_water, col = "red")
abline(sub_eriovagi_pct_water, col = "green")
abline(sub_junceffu_pct_water, col = "blue")
abline(sub_juncsqua_pct_water, col = "yellow")
abline(sub_molicaer_pct_water, col = "black")
abline(sub_nardstri_pct_water, col = "orange")
abline(sub_carenigr_pct_water, col = "brown")
abline(sub_descflex_pct_water, col = "pink")
abline(sub_galisaxa_pct_water, col = "aquamarine")
abline(sub_poteerec_pct_water, col = "violet")
abline(sub_vaccmyrt_pct_water, col = "gold")
legend("topright", 
       legend = c("callvulg", "eriovagi", "junceffu", "juncsqua", "molicaer", "nardstri", 
                  "carenigr", "descflex", "galisaxa", "poteerec", "vaccmyrt"), 
       col = c("red", "green", "blue", "yellow", "black", "orange", "brown", "pink", "aquamarine",
               "violet", "gold"), 
       lty = 1,
       bty = "n", 
       cex = 0.5, 
       text.col = "black", 
       horiz = F, ncol = 5)

## AREA ##
area_sub_spp <- read.csv("Results/Patch_Stats/Ordination/Area_spp/Area_matrix/Area_spp_sub_4-08-19.csv")
area_sub_spp[is.na(area_sub_spp)] <- 0
sub_callvulg_pct_water_a <- lm(area_sub_spp$Callvulg ~ env_var$pct_water)
sub_eriovagi_pct_water_a <- lm(area_sub_spp$Eriovagi ~ env_var$pct_water)
sub_junceffu_pct_water_a <- lm(area_sub_spp$Junceffu ~ env_var$pct_water)
sub_juncsqua_pct_water_a <- lm(area_sub_spp$Juncsqua ~ env_var$pct_water)
sub_molicaer_pct_water_a <- lm(area_sub_spp$Molicaer ~ env_var$pct_water)
sub_nardstri_pct_water_a <- lm(area_sub_spp$Nardstri ~ env_var$pct_water)
sub_carenigr_pct_water_a <- lm(area_sub_spp$Carenigr ~ env_var$pct_water)
sub_descflex_pct_water_a <- lm(area_sub_spp$Descflex ~ env_var$pct_water)
sub_galisaxa_pct_water_a <- lm(area_sub_spp$Galisaxa ~ env_var$pct_water)
sub_poteerec_pct_water_a <- lm(area_sub_spp$Poteerec ~ env_var$pct_water)
sub_vaccmyrt_pct_water_a <- lm(area_sub_spp$Vaccmyrt ~ env_var$pct_water)

#dom_patch_callvulg_pct_water <- lm(area_sub_spp$callvulg ~ env_var$length_pct_waterm)
plot(area_sub_spp$Callvulg + area_sub_spp$Eriovagi + area_sub_spp$Junceffu + 
       area_sub_spp$Juncsqua + area_sub_spp$Molicaer + area_sub_spp$Nardstri
     + area_sub_spp$Carenigr + area_sub_spp$Descflex + area_sub_spp$Galisaxa + 
       area_sub_spp$Poteerec + area_sub_spp$Vaccmyrt 
     ~ env_var$pct_water, ylim = c(0, 20), type = "n", xlab = "pct_water", ylab = "Area of patches")
#points(patchno_dom_spp$callvulg + patchno_dom_spp$eriovagi + patchno_dom_spp$junceffu + 
#patchno_dom_spp$juncsqua + patchno_dom_spp$molicaer + patchno_dom_spp$nardstri
#+ patchno_dom_spp$carenigr + patchno_dom_spp$descflex + patchno_dom_spp$galisaxa + patchno_dom_spp$poteerec + 
#  patchno_dom_spp$vaccmyrt ~ env_var$pct_water, pch = 1:11, col = 1:11, cex = 0.5)
#abline(c(callvulg_pct_water, eriovagi_pct_water, junceffu_pct_water, juncsqua_pct_water, molicaer_pct_water, nardstri_pct_water,
#carenigr_pct_water, descflex_pct_water, galisaxa_pct_water, poteerec_pct_water, vaccmyrt_pct_water), col = 1:11)
abline(sub_callvulg_pct_water_a, col = "red")
abline(sub_eriovagi_pct_water_a, col = "green")
abline(sub_junceffu_pct_water_a, col = "blue")
abline(sub_juncsqua_pct_water_a, col = "yellow")
abline(sub_molicaer_pct_water_a, col = "black")
abline(sub_nardstri_pct_water_a, col = "orange")
abline(sub_carenigr_pct_water_a, col = "brown")
abline(sub_descflex_pct_water_a, col = "pink")
abline(sub_galisaxa_pct_water_a, col = "aquamarine")
abline(sub_poteerec_pct_water_a, col = "violet")
abline(sub_vaccmyrt_pct_water_a, col = "gold")
legend("topright", 
       legend = c("callvulg", "eriovagi", "junceffu", "juncsqua", "molicaer", "nardstri", 
                  "carenigr", "descflex", "galisaxa", "poteerec", "vaccmyrt"), 
       col = c("red", "green", "blue", "yellow", "black", "orange", "brown", "pink", "aquamarine",
               "violet", "gold"), 
       lty = 1,
       bty = "n", 
       cex = 0.5, 
       text.col = "black", 
       horiz = F, ncol = 5)

## SHAPE INDEX ## 
shape_sub_spp <- data.frame(read.csv("Results/Patch_Stats/Ordination/Shape_Index/shape_matrix/Shape_sub_matrix_12-08-19.csv"))
sub_callvulg_pct_water_s <- lm(shape_sub_spp$Callvulg ~ env_var$pct_water)
sub_eriovagi_pct_water_s <- lm(shape_sub_spp$Eriovagi ~ env_var$pct_water)
sub_junceffu_pct_water_s <- lm(shape_sub_spp$Junceffu ~ env_var$pct_water)
sub_juncsqua_pct_water_s <- lm(shape_sub_spp$Juncsqua ~ env_var$pct_water)
sub_molicaer_pct_water_s <- lm(shape_sub_spp$Molicaer ~ env_var$pct_water)
sub_nardstri_pct_water_s <- lm(shape_sub_spp$Nardstri ~ env_var$pct_water)
sub_carenigr_pct_water_s <- lm(shape_sub_spp$Carenigr ~ env_var$pct_water)
sub_descflex_pct_water_s <- lm(shape_sub_spp$Descflex ~ env_var$pct_water)
sub_galisaxa_pct_water_s <- lm(shape_sub_spp$Galisaxa ~ env_var$pct_water)
sub_poteerec_pct_water_s <- lm(shape_sub_spp$Poteerec ~ env_var$pct_water)
sub_vaccmyrt_pct_water_s <- lm(shape_sub_spp$Vaccmyrt ~ env_var$pct_water)

#dom_patch_callvulg_pct_water <- lm(shape_sub_spp$callvulg ~ env_var$length_pct_waterm)
plot(shape_sub_spp$Callvulg + shape_sub_spp$Eriovagi + shape_sub_spp$Junceffu + 
       shape_sub_spp$Juncsqua + shape_sub_spp$Molicaer + shape_sub_spp$Nardstri
     + shape_sub_spp$Carenigr + shape_sub_spp$Descflex + shape_sub_spp$Galisaxa + 
       shape_sub_spp$Poteerec + shape_sub_spp$Vaccmyrt 
     ~ env_var$pct_water, ylim = c(1, 1.5), type = "n", xlab = "pct_water", ylab = "Shape of patches")
#points(patchno_dom_spp$callvulg + patchno_dom_spp$eriovagi + patchno_dom_spp$junceffu + 
#patchno_dom_spp$juncsqua + patchno_dom_spp$molicaer + patchno_dom_spp$nardstri
#+ patchno_dom_spp$carenigr + patchno_dom_spp$descflex + patchno_dom_spp$galisaxa + patchno_dom_spp$poteerec + 
#  patchno_dom_spp$vaccmyrt ~ env_var$pct_water, pch = 1:11, col = 1:11, cex = 0.5)
#abline(c(callvulg_pct_water, eriovagi_pct_water, junceffu_pct_water, juncsqua_pct_water, molicaer_pct_water, nardstri_pct_water,
#carenigr_pct_water, descflex_pct_water, galisaxa_pct_water, poteerec_pct_water, vaccmyrt_pct_water), col = 1:11)
abline(sub_callvulg_pct_water_s, col = "red")
abline(sub_eriovagi_pct_water_s, col = "green")
abline(sub_junceffu_pct_water_s, col = "blue")
abline(sub_juncsqua_pct_water_s, col = "yellow")
abline(sub_molicaer_pct_water_s, col = "black")
abline(sub_nardstri_pct_water_s, col = "orange")
abline(sub_carenigr_pct_water_s, col = "brown")
abline(sub_descflex_pct_water_s, col = "pink")
abline(sub_galisaxa_pct_water_s, col = "aquamarine")
abline(sub_poteerec_pct_water_s, col = "violet")
abline(sub_vaccmyrt_pct_water_s, col = "gold")
legend("topright", 
       legend = c("callvulg", "eriovagi", "junceffu", "juncsqua", "molicaer", "nardstri", 
                  "carenigr", "descflex", "galisaxa", "poteerec", "vaccmyrt"), 
       col = c("red", "green", "blue", "yellow", "black", "orange", "brown", "pink", "aquamarine",
               "violet", "gold"), 
       lty = 1,
       bty = "n", 
       cex = 0.5, 
       text.col = "black", 
       horiz = F, ncol = 5)

## DOMINANT: distance to ditch ####
patchno_dom_spp <- read.csv("Results/Patch_Stats/Ordination/Patchno_spp/Patchno_matrix/Patchno_matrix_dom_12-08-19.csv")
patchno_dom_spp[is.na(patchno_dom_spp)] <- 0
callvulg_ditch <- lm(patchno_dom_spp$callvulg ~ env_var$ditch_distance)
eriovagi_ditch <- lm(patchno_dom_spp$eriovagi ~ env_var$ditch_distance)
junceffu_ditch <- lm(patchno_dom_spp$junceffu ~ env_var$ditch_distance)
juncsqua_ditch <- lm(patchno_dom_spp$juncsqua ~ env_var$ditch_distance)
molicaer_ditch <- lm(patchno_dom_spp$molicaer ~ env_var$ditch_distance)
nardstri_ditch <- lm(patchno_dom_spp$nardstri ~ env_var$ditch_distance)
carenigr_ditch <- lm(patchno_dom_spp$carenigr ~ env_var$ditch_distance)
descflex_ditch <- lm(patchno_dom_spp$descflex ~ env_var$ditch_distance)
galisaxa_ditch <- lm(patchno_dom_spp$galisaxa ~ env_var$ditch_distance)
poteerec_ditch <- lm(patchno_dom_spp$poteerec ~ env_var$ditch_distance)
vaccmyrt_ditch <- lm(patchno_dom_spp$vaccmyrt ~ env_var$ditch_distance)

plot(patchno_dom_spp$callvulg + patchno_dom_spp$eriovagi + patchno_dom_spp$junceffu + 
       patchno_dom_spp$juncsqua + patchno_dom_spp$molicaer + patchno_dom_spp$nardstri
     + patchno_dom_spp$carenigr + patchno_dom_spp$descflex + patchno_dom_spp$galisaxa + 
       patchno_dom_spp$poteerec + patchno_dom_spp$vaccmyrt 
     ~ env_var$ditch_distance, ylim = c(0, 4), type = "n", xlab = "Distance to ditch", ylab = "Number of patches")
#points(patchno_dom_spp$callvulg + patchno_dom_spp$eriovagi + patchno_dom_spp$junceffu + 
#patchno_dom_spp$juncsqua + patchno_dom_spp$molicaer + patchno_dom_spp$nardstri
#+ patchno_dom_spp$carenigr + patchno_dom_spp$descflex + patchno_dom_spp$galisaxa + patchno_dom_spp$poteerec + 
#  patchno_dom_spp$vaccmyrt ~ env_var$distance_ditchm, pch = 1:11, col = 1:11, cex = 0.5)
#abline(c(callvulg_ditch, eriovagi_ditch, junceffu_ditch, juncsqua_ditch, molicaer_ditch, nardstri_ditch,
#carenigr_ditch, descflex_ditch, galisaxa_ditch, poteerec_ditch, vaccmyrt_ditch), col = 1:11)
abline(callvulg_ditch, col = "red")
abline(eriovagi_ditch, col = "green")
abline(junceffu_ditch, col = "blue")
abline(juncsqua_ditch, col = "yellow")
abline(molicaer_ditch, col = "black")
abline(nardstri_ditch, col = "orange")
abline(carenigr_ditch, col = "brown")
abline(descflex_ditch, col = "pink")
abline(galisaxa_ditch, col = "aquamarine")
abline(poteerec_ditch, col = "violet")
abline(vaccmyrt_ditch, col = "gold")
legend("topright", 
       legend = c("callvulg", "eriovagi", "junceffu", "juncsqua", "molicaer", "nardstri", 
                  "carenigr", "descflex", "galisaxa", "poteerec", "vaccmyrt"), 
       col = c("red", "green", "blue", "yellow", "black", "orange", "brown", "pink", "aquamarine",
               "violet", "gold"), 
       lty = 1,
       bty = "n", 
       cex = 0.5, 
       text.col = "black", 
       horiz = F, ncol = 5)

## Area ##
area_dom_spp <- read.csv("Results/Patch_Stats/Ordination/Area_spp/Area_matrix/Area_spp_dominant_4-08-19.csv")

callvulg_ditch_a <- lm(area_dom_spp$callvulg ~ env_var$ditch_distance)
eriovagi_ditch_a <- lm(area_dom_spp$eriovagi ~ env_var$ditch_distance)
junceffu_ditch_a <- lm(area_dom_spp$junceffu ~ env_var$ditch_distance)
juncsqua_ditch_a <- lm(area_dom_spp$juncsqua ~ env_var$ditch_distance)
molicaer_ditch_a <- lm(area_dom_spp$molicaer ~ env_var$ditch_distance)
nardstri_ditch_a <- lm(area_dom_spp$nardstri ~ env_var$ditch_distance)
carenigr_ditch_a <- lm(area_dom_spp$carenigr ~ env_var$ditch_distance)
descflex_ditch_a <- lm(area_dom_spp$descflex ~ env_var$ditch_distance)
galisaxa_ditch_a <- lm(area_dom_spp$galisaxa ~ env_var$ditch_distance)
poteerec_ditch_a <- lm(area_dom_spp$poteerec ~ env_var$ditch_distance)
vaccmyrt_ditch_a <- lm(area_dom_spp$vaccmyrt ~ env_var$ditch_distance)

plot(area_dom_spp$callvulg + area_dom_spp$eriovagi + area_dom_spp$junceffu + 
       area_dom_spp$juncsqua + area_dom_spp$molicaer + area_dom_spp$nardstri
     + area_dom_spp$carenigr + area_dom_spp$descflex + area_dom_spp$galisaxa + 
       area_dom_spp$poteerec + area_dom_spp$vaccmyrt 
     ~ env_var$ditch_distance, ylim = c(0, 60), type = "n", xlab = "Distance to ditch", ylab = "Area per patch")
abline(callvulg_ditch_a, col = "red")
abline(eriovagi_ditch_a, col = "green")
abline(junceffu_ditch_a, col = "blue")
abline(juncsqua_ditch_a, col = "yellow")
abline(molicaer_ditch_a, col = "black")
abline(nardstri_ditch_a, col = "orange")
abline(carenigr_ditch_a, col = "brown")
abline(descflex_ditch_a, col = "pink")
abline(galisaxa_ditch_a, col = "aquamarine")
abline(poteerec_ditch_a, col = "violet")
abline(vaccmyrt_ditch_a, col = "gold")

legend("top", 
       legend = c("callvulg", "eriovagi", "junceffu", "juncsqua", "molicaer", "nardstri", 
                  "carenigr", "descflex", "galisaxa", "poteerec", "vaccmyrt"), 
       col = c("red", "green", "blue", "yellow", "black", "orange", "brown", "pink", "aquamarine",
               "violet", "gold"), 
       lty = 1,
       bty = "n", 
       cex = 0.5, 
       text.col = "black", 
       horiz = F, ncol = 5)

## SHAPE INDEX ##
shape_dom_spp <- data.frame(read.csv("Results/Patch_Stats/Ordination/Shape_Index/shape_matrix/Shape_dom_matrix_12-08-19.csv"))
#shape_dom_spp[is.na(shape_dom_spp)] <- 0
callvulg_ditch_s <- lm(shape_dom_spp$callvulg ~ env_var$ditch_distance)
eriovagi_ditch_s <- lm(shape_dom_spp$eriovagi ~ env_var$ditch_distance)
junceffu_ditch_s <- lm(shape_dom_spp$junceffu ~ env_var$ditch_distance)
juncsqua_ditch_s <- lm(shape_dom_spp$juncsqua ~ env_var$ditch_distance)
molicaer_ditch_s <- lm(shape_dom_spp$molicaer ~ env_var$ditch_distance)
nardstri_ditch_s <- lm(shape_dom_spp$nardstri ~ env_var$ditch_distance)
carenigr_ditch_s <- lm(shape_dom_spp$carenigr ~ env_var$ditch_distance)
descflex_ditch_s <- lm(shape_dom_spp$descflex ~ env_var$ditch_distance)
galisaxa_ditch_s <- lm(shape_dom_spp$galisaxa ~ env_var$ditch_distance)
poteerec_ditch_s <- lm(shape_dom_spp$poteerec ~ env_var$ditch_distance)
vaccmyrt_ditch_s <- lm(shape_dom_spp$vaccmyrt ~ env_var$ditch_distance)

plot(shape_dom_spp$callvulg + shape_dom_spp$eriovagi + shape_dom_spp$junceffu + 
       shape_dom_spp$juncsqua + shape_dom_spp$molicaer + shape_dom_spp$nardstri
     + shape_dom_spp$carenigr + shape_dom_spp$descflex + shape_dom_spp$galisaxa + 
       shape_dom_spp$poteerec + shape_dom_spp$vaccmyrt 
     ~ env_var$ditch_distance, ylim = c(1,2), type = "n", xlab = "pct_ditch", ylab = "Shape per patch")
abline(callvulg_ditch_s, col = "red")
abline(eriovagi_ditch_s, col = "green")
abline(junceffu_ditch_s, col = "blue")
abline(juncsqua_ditch_s, col = "yellow")
abline(molicaer_ditch_s, col = "black")
abline(nardstri_ditch_s, col = "orange")
abline(carenigr_ditch_s, col = "brown")
abline(descflex_ditch_s, col = "pink")
abline(galisaxa_ditch_s, col = "aquamarine")
abline(poteerec_ditch_s, col = "violet")
abline(vaccmyrt_ditch_s, col = "gold")

legend("top", 
       legend = c("callvulg", "eriovagi", "junceffu", "juncsqua", "molicaer", "nardstri", 
                  "carenigr", "descflex", "galisaxa", "poteerec", "vaccmyrt"), 
       col = c("red", "green", "blue", "yellow", "black", "orange", "brown", "pink", "aquamarine",
               "violet", "gold"), 
       lty = 1,
       bty = "n", 
       cex = 0.5,  
       text.col = "black", 
       horiz = F, ncol = 5)

## SUBDOMINANT: distance to ditch ####
patchno_sub_spp <- data.frame(read.csv("Results/Patch_Stats/Ordination/Patchno_spp/Patchno_matrix/Patchno_matrix_sub_12-08-19.csv"))
patchno_sub_spp[is.na(patchno_sub_spp)] <- 0
sub_callvulg_ditch <- lm(patchno_sub_spp$Callvulg ~ env_var$ditch_distance)
sub_eriovagi_ditch <- lm(patchno_sub_spp$Eriovagi ~ env_var$ditch_distance)
sub_junceffu_ditch <- lm(patchno_sub_spp$Junceffu ~ env_var$ditch_distance)
sub_juncsqua_ditch <- lm(patchno_sub_spp$Juncsqua ~ env_var$ditch_distance)
sub_molicaer_ditch <- lm(patchno_sub_spp$Molicaer ~ env_var$ditch_distance)
sub_nardstri_ditch <- lm(patchno_sub_spp$Nardstri ~ env_var$ditch_distance)
sub_carenigr_ditch <- lm(patchno_sub_spp$Carenigr ~ env_var$ditch_distance)
sub_descflex_ditch <- lm(patchno_sub_spp$Descflex ~ env_var$ditch_distance)
sub_galisaxa_ditch <- lm(patchno_sub_spp$Galisaxa ~ env_var$ditch_distance)
sub_poteerec_ditch <- lm(patchno_sub_spp$Poteerec ~ env_var$ditch_distance)
sub_vaccmyrt_ditch <- lm(patchno_sub_spp$Vaccmyrt ~ env_var$ditch_distance)

#sub_patch_callvulg_ditch <- lm(patchno_sub_spp$callvulg ~ env_var$length_ditchm)
plot(patchno_sub_spp$Callvulg + patchno_sub_spp$Eriovagi + patchno_sub_spp$Junceffu + 
       patchno_sub_spp$Juncsqua + patchno_sub_spp$Molicaer + patchno_sub_spp$Nardstri
     + patchno_sub_spp$Carenigr + patchno_sub_spp$Descflex + patchno_sub_spp$Galisaxa + 
       patchno_sub_spp$Poteerec + patchno_sub_spp$Vaccmyrt 
     ~ env_var$ditch_distance, ylim = c(0, 6), type = "n", xlab = "Slope", ylab = "Number of patches")
#points(patchno_sub_spp$callvulg + patchno_sub_spp$eriovagi + patchno_sub_spp$junceffu + 
#patchno_sub_spp$juncsqua + patchno_sub_spp$molicaer + patchno_sub_spp$nardstri
#+ patchno_sub_spp$carenigr + patchno_sub_spp$descflex + patchno_sub_spp$galisaxa + patchno_sub_spp$poteerec + 
#  patchno_sub_spp$vaccmyrt ~ env_var$ditch_distance, pch = 1:11, col = 1:11, cex = 0.5)
#abline(c(callvulg_ditch, eriovagi_ditch, junceffu_ditch, juncsqua_ditch, molicaer_ditch, nardstri_ditch,
#carenigr_ditch, descflex_ditch, galisaxa_ditch, poteerec_ditch, vaccmyrt_ditch), col = 1:11)
abline(sub_callvulg_ditch, col = "red")
abline(sub_eriovagi_ditch, col = "green")
abline(sub_junceffu_ditch, col = "blue")
abline(sub_juncsqua_ditch, col = "yellow")
abline(sub_molicaer_ditch, col = "black")
abline(sub_nardstri_ditch, col = "orange")
abline(sub_carenigr_ditch, col = "brown")
abline(sub_descflex_ditch, col = "pink")
abline(sub_galisaxa_ditch, col = "aquamarine")
abline(sub_poteerec_ditch, col = "violet")
abline(sub_vaccmyrt_ditch, col = "gold")
legend("topright", 
       legend = c("callvulg", "eriovagi", "junceffu", "juncsqua", "molicaer", "nardstri", 
                  "carenigr", "descflex", "galisaxa", "poteerec", "vaccmyrt"), 
       col = c("red", "green", "blue", "yellow", "black", "orange", "brown", "pink", "aquamarine",
               "violet", "gold"), 
       lty = 1,
       bty = "n", 
       cex = 0.5, 
       text.col = "black", 
       horiz = F, ncol = 5)

## AREA ##
area_sub_spp <- read.csv("Results/Patch_Stats/Ordination/Area_spp/Area_matrix/Area_spp_sub_4-08-19.csv")
area_sub_spp[is.na(area_sub_spp)] <- 0
sub_callvulg_ditch_a <- lm(area_sub_spp$Callvulg ~ env_var$ditch_distance)
sub_eriovagi_ditch_a <- lm(area_sub_spp$Eriovagi ~ env_var$ditch_distance)
sub_junceffu_ditch_a <- lm(area_sub_spp$Junceffu ~ env_var$ditch_distance)
sub_juncsqua_ditch_a <- lm(area_sub_spp$Juncsqua ~ env_var$ditch_distance)
sub_molicaer_ditch_a <- lm(area_sub_spp$Molicaer ~ env_var$ditch_distance)
sub_nardstri_ditch_a <- lm(area_sub_spp$Nardstri ~ env_var$ditch_distance)
sub_carenigr_ditch_a <- lm(area_sub_spp$Carenigr ~ env_var$ditch_distance)
sub_descflex_ditch_a <- lm(area_sub_spp$Descflex ~ env_var$ditch_distance)
sub_galisaxa_ditch_a <- lm(area_sub_spp$Galisaxa ~ env_var$ditch_distance)
sub_poteerec_ditch_a <- lm(area_sub_spp$Poteerec ~ env_var$ditch_distance)
sub_vaccmyrt_ditch_a <- lm(area_sub_spp$Vaccmyrt ~ env_var$ditch_distance)

#dom_patch_callvulg_ditch <- lm(area_sub_spp$callvulg ~ env_var$length_ditchm)
plot(area_sub_spp$Callvulg + area_sub_spp$Eriovagi + area_sub_spp$Junceffu + 
       area_sub_spp$Juncsqua + area_sub_spp$Molicaer + area_sub_spp$Nardstri
     + area_sub_spp$Carenigr + area_sub_spp$Descflex + area_sub_spp$Galisaxa + 
       area_sub_spp$Poteerec + area_sub_spp$Vaccmyrt 
     ~ env_var$ditch_distance, ylim = c(0, 25), type = "n", xlab = "Slope", ylab = "Area of patches")
#points(patchno_dom_spp$callvulg + patchno_dom_spp$eriovagi + patchno_dom_spp$junceffu + 
#patchno_dom_spp$juncsqua + patchno_dom_spp$molicaer + patchno_dom_spp$nardstri
#+ patchno_dom_spp$carenigr + patchno_dom_spp$descflex + patchno_dom_spp$galisaxa + patchno_dom_spp$poteerec + 
#  patchno_dom_spp$vaccmyrt ~ env_var$ditch_distance, pch = 1:11, col = 1:11, cex = 0.5)
#abline(c(callvulg_ditch, eriovagi_ditch, junceffu_ditch, juncsqua_ditch, molicaer_ditch, nardstri_ditch,
#carenigr_ditch, descflex_ditch, galisaxa_ditch, poteerec_ditch, vaccmyrt_ditch), col = 1:11)
abline(sub_callvulg_ditch_a, col = "red")
abline(sub_eriovagi_ditch_a, col = "green")
abline(sub_junceffu_ditch_a, col = "blue")
abline(sub_juncsqua_ditch_a, col = "yellow")
abline(sub_molicaer_ditch_a, col = "black")
abline(sub_nardstri_ditch_a, col = "orange")
abline(sub_carenigr_ditch_a, col = "brown")
abline(sub_descflex_ditch_a, col = "pink")
abline(sub_galisaxa_ditch_a, col = "aquamarine")
abline(sub_poteerec_ditch_a, col = "violet")
abline(sub_vaccmyrt_ditch_a, col = "gold")
legend("topright", 
       legend = c("callvulg", "eriovagi", "junceffu", "juncsqua", "molicaer", "nardstri", 
                  "carenigr", "descflex", "galisaxa", "poteerec", "vaccmyrt"), 
       col = c("red", "green", "blue", "yellow", "black", "orange", "brown", "pink", "aquamarine",
               "violet", "gold"), 
       lty = 1,
       bty = "n", 
       cex = 0.5, 
       text.col = "black", 
       horiz = F, ncol = 5)

## SHAPE INDEX ## 
shape_sub_spp <- data.frame(read.csv("Results/Patch_Stats/Ordination/Shape_Index/shape_matrix/Shape_sub_matrix_12-08-19.csv"))
sub_callvulg_ditch_s <- lm(shape_sub_spp$Callvulg ~ env_var$ditch_distance)
sub_eriovagi_ditch_s <- lm(shape_sub_spp$Eriovagi ~ env_var$ditch_distance)
sub_junceffu_ditch_s <- lm(shape_sub_spp$Junceffu ~ env_var$ditch_distance)
sub_juncsqua_ditch_s <- lm(shape_sub_spp$Juncsqua ~ env_var$ditch_distance)
sub_molicaer_ditch_s <- lm(shape_sub_spp$Molicaer ~ env_var$ditch_distance)
sub_nardstri_ditch_s <- lm(shape_sub_spp$Nardstri ~ env_var$ditch_distance)
sub_carenigr_ditch_s <- lm(shape_sub_spp$Carenigr ~ env_var$ditch_distance)
sub_descflex_ditch_s <- lm(shape_sub_spp$Descflex ~ env_var$ditch_distance)
sub_galisaxa_ditch_s <- lm(shape_sub_spp$Galisaxa ~ env_var$ditch_distance)
sub_poteerec_ditch_s <- lm(shape_sub_spp$Poteerec ~ env_var$ditch_distance)
sub_vaccmyrt_ditch_s <- lm(shape_sub_spp$Vaccmyrt ~ env_var$ditch_distance)

#dom_patch_callvulg_ditch <- lm(shape_sub_spp$callvulg ~ env_var$length_ditchm)
plot(shape_sub_spp$Callvulg + shape_sub_spp$Eriovagi + shape_sub_spp$Junceffu + 
       shape_sub_spp$Juncsqua + shape_sub_spp$Molicaer + shape_sub_spp$Nardstri
     + shape_sub_spp$Carenigr + shape_sub_spp$Descflex + shape_sub_spp$Galisaxa + 
       shape_sub_spp$Poteerec + shape_sub_spp$Vaccmyrt 
     ~ env_var$ditch_distance, ylim = c(1, 1.5), type = "n", xlab = "Slope", ylab = "Shape of patches")
#points(patchno_dom_spp$callvulg + patchno_dom_spp$eriovagi + patchno_dom_spp$junceffu + 
#patchno_dom_spp$juncsqua + patchno_dom_spp$molicaer + patchno_dom_spp$nardstri
#+ patchno_dom_spp$carenigr + patchno_dom_spp$descflex + patchno_dom_spp$galisaxa + patchno_dom_spp$poteerec + 
#  patchno_dom_spp$vaccmyrt ~ env_var$ditch_distance, pch = 1:11, col = 1:11, cex = 0.5)
#abline(c(callvulg_ditch, eriovagi_ditch, junceffu_ditch, juncsqua_ditch, molicaer_ditch, nardstri_ditch,
#carenigr_ditch, descflex_ditch, galisaxa_ditch, poteerec_ditch, vaccmyrt_ditch), col = 1:11)
abline(sub_callvulg_ditch_s, col = "red")
abline(sub_eriovagi_ditch_s, col = "green")
abline(sub_junceffu_ditch_s, col = "blue")
abline(sub_juncsqua_ditch_s, col = "yellow")
abline(sub_molicaer_ditch_s, col = "black")
abline(sub_nardstri_ditch_s, col = "orange")
abline(sub_carenigr_ditch_s, col = "brown")
abline(sub_descflex_ditch_s, col = "pink")
abline(sub_galisaxa_ditch_s, col = "aquamarine")
abline(sub_poteerec_ditch_s, col = "violet")
abline(sub_vaccmyrt_ditch_s, col = "gold")
legend("topright", 
       legend = c("callvulg", "eriovagi", "junceffu", "juncsqua", "molicaer", "nardstri", 
                  "carenigr", "descflex", "galisaxa", "poteerec", "vaccmyrt"), 
       col = c("red", "green", "blue", "yellow", "black", "orange", "brown", "pink", "aquamarine",
               "violet", "gold"), 
       lty = 1,
       bty = "n", 
       cex = 0.5, 
       text.col = "black", 
       horiz = F, ncol = 5)

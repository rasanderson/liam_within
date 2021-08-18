#library(DescTools)
library(ineq) # more accurate than DescTools when comparing to Weiner and Solbrig
library(mvabund)
library(dplyr)
library(tidyverse)
library(SDMTools)

rm(list=ls())

# Example from 
# The meaning and measurement of size hierarchies
# in plant populations
# Jacob Weiner 1, and Otto T. Solbrig
# n = c(rep(1, 50), rep(2, 5) )
# Gini(n)
# ineq(n)
# 
# n = c(rep(1, 50), rep(10, 5) )
# Gini(n)
# ineq(n)
# 
# n = c(rep(101, 50), rep(102, 5) )
# Gini(n)
# ineq(n)

#ashtrees_env <- read.csv("Data/Biotic_data/ashtrees_env_variables_14-01-19.csv")
ashtrees_env <- read.csv("data/ashtrees_env_variables_14-01-19.csv")
ashtrees_env <- ashtrees_env[,-1:-2]


# This is how Liam did it earlier ####
###################
###  DOMINANT   ####
###################
# shape_dom_spp <- data.frame(read.csv("Results/Patch_Stats/Ordination/Shape_Index/shape_matrix/Shape_dom_matrix_12-08-19.csv"))
shape_dom_spp <- data.frame(read.csv("data/Shape_dom_12-08-19.csv"))
shape_dom_spp[is.na(shape_dom_spp)] <- 0
shape_dom_spp <- shape_dom_spp[,-55] # taraxacu = 0
shape_dom_spp <- shape_dom_spp[,-49] # SHEEP
shape_dom_spp <- shape_dom_spp[,-47] # ROCK
shape_dom_spp <- shape_dom_spp[,-35] # LITTER
shape_dom_spp <- shape_dom_spp[,-17] # DEAD
shape_dom_spp <- shape_dom_spp[,-5] # BARE
shape_dom_spp <- shape_dom_spp[,-1] # quad_no
#env_var <- data.frame(read.csv("Data/Biotic_data/ashtrees_env_variables_14-01-19.csv"))
env_var <- data.frame(read.csv("data/ashtrees_env_variables_14-01-19.csv"))
env_var[is.na(env_var)] <- 0

# shape_dom_mvabund <- mvabund(shape_dom_spp)
# plot.mvformula(log(shape_dom_mvabund+1) ~ exp(env_var$soil_pH), main="Dominant Patchno vs Soil pH",
#                xlab="% Soil pH - Log Scale ", ylab="Abundance [log scale]",
#                overall.main="Species Abundance vs soil pH",
#                fg="grey", las=1, scale.lab="ss",t.lab="o", mfrow=c(4,3),log="x") # Produces a range of plots for 
# #visualising multivariate abundance data and its relationship to environmental
# #variables
# 
# boxplot(shape_dom_mvabund, horizontal = TRUE, las = 2, main = "Shape Index: DOM") # boxplot
# meanvar.plot(shape_dom_mvabund) # check mean variance
# 
# 
# shape_dom_glm_binomial <- manyglm(shape_dom_mvabund ~ soil_pH + slope + pct_water + Altitude  + length_10m + distance_10m + 
#                                     length_25m + distance_25m + length_35m + distance_35m + ditch_distance, 
#                                   data = env_var, family = "negative.binomial") # might be good but data exceeds range of 1
# plot(shape_dom_glm_binomial)  
# capture.output(shape_dom_glm_binomial[["coefficients"]], file = "Results/DOMINANT/Coefficients/shape_dom_coefficients.csv")
# anova_ind_dom_shape <- anova(shape_dom_glm_binomial, p.uni="adjusted", show.time = "all")
# capture.output(anova_ind_dom_shape, file = "Results/DOMINANT/Statistics/shape_anova_glm_dom_ind.csv")

# Gini dominant
# Need to calculate values for each spp in each quadrat
# Dominant <- data.frame(read.csv("Data/Dominant-sub_New_Versions/Dominant_23-7-18_spaced.csv", header = FALSE)) # main datafram (all inputs)
Dominant <- data.frame(read.csv("data/Dominant_23-7-18_spaced.csv", header = FALSE)) # main datafram (all inputs)

colnames(Dominant) <- c("quad_no", "Row", 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
Dominant[1,] = "" ## add empty line to streamline with other quadrat rows
Dominant[1838,] = "" ## add line after last quadrat to streamline with other quadrat rows

spat_all_quads <- NULL
for(quad_no_code in 1:167){
print(quad_no_code)  
quad1_dom <- subset(Dominant, quad_no == as.character(quad_no_code))
#quad1_dom <- subset(Dominant, quad_no == "1")
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


quad_dom_tab <- quad1_dom_tab ## change according to quadrat number needed for patch stats
quad_lng     <- quad1_lng

spp_in_quad  <- names(quad_dom_tab)
spat_all     <- NULL
for (spp_name in seq_along(spp_in_quad)){
  quad_no <- quad_no_code
  spp_text <- names(quad_dom_tab)[spp_name]
  quad_spp_lng <- quad_lng %>% filter(Species == spp_text)
  quad_spp_wde <- quad_spp_lng %>% reshape2::dcast(Row ~ Col, drop = FALSE, fill = 0)
  # Recode text to number
  quad_spp_wde[,-1] <- ifelse(quad_spp_wde[,-1]!="0",1,0)
  quad_spp_mat <- as.matrix(quad_spp_wde[,-1])
  
  quad_ccl = ConnCompLabel(quad_spp_mat)
  #print(spp_text)
  #print(quad_ccl)
  #image(t(quad_ccl[12:1,]), col=c('white',rainbow(length(unique(quad_ccl))-1)), main = print(spp_text)) #due to spaces matrix is 12 (y-axis)x10 (x-axis)
  #grid(nx=10, ny=12, col = "grey", lty = 1) # added post roy
  #print(PatchStat(quad_ccl))
  
  # Store the spatial stats
  # Need to duplicate spp text names in final output table
  spp_duplicate <- rep(spp_text, max(quad_ccl)+1)
  patch_stats <- PatchStat(quad_ccl)
  patch_stats <- cbind(quad_no, spp_duplicate, patch_stats)
  spat_all <- rbind(spat_all, patch_stats)
  #readline()
}
spat_all_quads <- rbind(spat_all_quads, spat_all)
}
#write.csv(spat_all, file = "Results/Patch_Stats/Individual_quadrats/Q138_DOMINANT.csv")

# Filter out the background quadrats and calculate Gini
gini_lng <- spat_all_quads %>%
  filter(patchID != 0) %>% 
  group_by(quad_no, spp_duplicate) %>% 
  summarise( gini = ineq(n.cell))

# If only one patch of a spp in a quadrat not possible to calc Gini
#gini_lng$gini[is.nan(gini_lng$gini)] <- NA

# Make wide
gini_wde <- pivot_wider(gini_lng, names_from = spp_duplicate, values_from = gini)

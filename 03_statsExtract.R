
############################################
## CENSUS STAT EXTRACTION AND SUMMARIZING ##
############################################


## REMEMBER THE UNIVERSE YOU'RE DEALING WITH!! 
# Race alone - total pop
# Income - households*
# children - families SOMETIMES subset by race.

# * Vincent's code (lines 63 & 74 in utils.R) suggests
# that income-based tracts are defined by income percentiles
# and are then compared to individual (total pop) baselines.
# The first part checks out, the second part prob should have
# used a household-based baseline. But he only created an
# individ- and a family-based baseline. It's prob close enough.
# But when computing total numbers, it won't be possible to say
# X # of households are low income. Rather, it will be X # of ppl
# live in low-income tracts. (By contrast, we DO have # fams.)



##---------------------------------------------------------------------

## Create buffers around AOI and extract census tract data therein

m_in_mi <- 1609
# Select AOI and associated buffer sizes
# # --------------
# d <- cr ; s <- txnm; loc = "Castner Range" ; print("Castner Range selected.")
# b1 <- st_buffer(d, 10*m_in_mi) ; b1_size = "10mi"
# b2 <- st_buffer(d, 25*m_in_mi) ; b2_size = "25mi"
# b3 <- st_buffer(d, 50*m_in_mi) ; b3_size = "50mi"
# # -------------- 
d <- aka ; s <- nvca; loc = "AviKwaAme" ; print("Avi Kwa Ame selected.")
b1 <- st_buffer(d, 25*m_in_mi) ; b1_size = "25mi"
b2 <- st_buffer(d, 50*m_in_mi) ; b2_size = "50mi"
b3 <- st_buffer(d, 100*m_in_mi) ; b3_size = "100mi"
# # --------------

# plot(b3)
# plot(b2, add = TRUE)
# plot(b1, add = TRUE)
# plot(d, add = TRUE)


## Find census tracts that intersect w each buffer.
# 1st, ensure existing sqkm is correct to use to weight #ppl for partial tracts.
# area_sqkm <- terra::area(as_Spatial(s)) / 1000000
# mean(abs(area_sqkm - s$area_km2)) ; max(abs(area_sqkm - s$area_km2)) 
# identical(round(txnm$area_km2,4), round(area_sqkm,4)) # TRUE for CR, not quite AKA but close enough
# area_sqkm <- NULL

# Initial intersection threw this error
  # Error in s2_geography_from_wkb(x, oriented = oriented, check = check) : 
  #   Evaluation error: Found 1 feature with invalid spherical geometry.
  # [1] Loop 228 is not valid: Edge 8281 has duplicate vertex with edge 8285
# Advised on github: sf_use_s2(FALSE)
sf_use_s2(FALSE)

s_b1 <- st_intersection(b1, s)
s_b2 <- st_intersection(b2, s)
s_b3 <- st_intersection(b3, s)

# Compute new areas to account for partial census tracts.
s_b1$area_km2_new <- terra::area(as_Spatial(s_b1)) / 1000000
s_b2$area_km2_new <- terra::area(as_Spatial(s_b2)) / 1000000
s_b3$area_km2_new <- terra::area(as_Spatial(s_b3)) / 1000000

# Compute the proportion of the original tract area the new portion represents.
# This will be used to scale population numbers within each.
# Visual check to make sure many match. 
# round(s_b1$area_km2[1:30],2) ; round(s_b1$area_km2_new[1:30],2)
# round(s_b2$area_km2[125:150],2) ; round(s_b2$area_km2_new[125:150],2)
# round(s_b3$area_km2[125:150],2) ; round(s_b3$area_km2_new[125:150],2)
s_b1$prop_ttl_area <- s_b1$area_km2_new/s_b1$area_km2 ; Mode(round(s_b1$prop_ttl_area, 4))
s_b2$prop_ttl_area <- s_b2$area_km2_new/s_b2$area_km2 ; Mode(round(s_b2$prop_ttl_area, 4))
s_b3$prop_ttl_area <- s_b3$area_km2_new/s_b3$area_km2 ; Mode(round(s_b3$prop_ttl_area, 4))


## --------------------------------------------------------------------

# Which zone? Turn on/off.
zone <- s_b1 ; (buffer <- b1_size)
# zone <- s_b2 ; (buffer <- b2_size)
# zone <- s_b3 ; (buffer <- b3_size)

# Eliminate any rows that have zero ppl else get NA rows when subsetting. 
zone <- zone[zone$AHY2E001 > 0,]

# Pull state abbreviations for selecting PAs 
(states <- unique(zone$STATE))
(states_abb <- if("Texas" %in% states) c("NM", "TX") else c("CA", "NV"))

# Source code that calcs dist to nearest PA for each tract and adds to shapefile.
source(paste0(wd, "analyses/cap-nature-access/04_distPA.R"), echo = FALSE)

# set geom to null for faster looping
zone_df <- zone ; zone_df$geometry <- NULL



## --------------------------------------------------------------------------


## What prop of status grp tracts and what # of ppl/families have hm >avg?

# Create empty vectors for dumping values
stat_grp <- as.character()
unit_type <- as.character()

# Human mod (all & energy) - natl
prop_tracts_gt_hm_natl <- as.numeric()
num_gt_hm_natl <- as.numeric()
prop_tracts_gt_hme_natl <- as.numeric()
num_gt_hme_natl <- as.numeric()

# # Human mod (all & energy) - state
# prop_tracts_gt_hm_st <- as.numeric()
# num_gt_hm_st <- as.numeric()
# prop_tracts_gt_hme_st <- as.numeric()
# num_gt_hme_st <- as.numeric()

# Distance to nearest PA
pa_dist_mi_gt_hm <- as.numeric()
pa_dist_mi_gt_hme <- as.numeric()

# Grab list of status groups, nixing baseline grps (tracts_ind, tracts_fam)
(stat_grps <- c(unique(hm_st$status_group)))
(1:length(stat_grps))[-c(25,26)]

## LOOP ## -------------------------------------------------------
# Loop through status grps but exclude baselines (tracts_ind, tracts_fam)
for (i in (1:length(stat_grps))[-c(25,26)]){ 
# for (i in 2){ # test black
# for (i in 4){ # test non-white
# for (i in 19){ # test non-white w children in pov
  # Assign status grp name
  grp <- stat_grps[i]
  
  # Look up the corresponding col with # ppl/fams for that status grp
  code1 <- match(grp, lu$status_grp) %>% lu$code1[.]
  code1_sign <- match(grp, lu$status_grp) %>% lu$code1_sign[.]
  code2 <- match(grp, lu$status_grp) %>% lu$code2[.]
  code2_sign <- match(grp, lu$status_grp) %>% lu$code2_sign[.]
  code3 <- match(grp, lu$status_grp) %>% lu$code3[.]
  code3_sign <- match(grp, lu$status_grp) %>% lu$code3_sign[.]
  code4 <- match(grp, lu$status_grp) %>% lu$code4[.]
  code4_sign <- match(grp, lu$status_grp) %>% lu$code4_sign[.]
  code5 <- match(grp, lu$status_grp) %>% lu$code5[.]
  code5_sign <- match(grp, lu$status_grp) %>% lu$code5_sign[.]

  # Look up the unit -- ie # ppl or # families
  unit <- match(grp, lu$status_grp) %>% lu$unit[.]
  
  # Find the appropriate baseline based on unit (individ or fam)
  hm_base <- hm_natl$hm_mean[hm_natl$status_group == unit]
  hme_base <- hm_natl$hm_energy_mean[hm_natl$status_group == unit]
  
  # Pull tracts in status grp w hm/hme > avg; row of NAs may be returned b/c of other NAs in df
  all <- zone_df[zone_df[grp] == 1,] 
  sel_hm <- all[all$hm > hm_base,]
  sel_hme <- all[all$hm_energy > hme_base,]
  
  # Compute prop of grp tracts w hm/hme > avg
  prop_hm <- round(nrow(sel_hm)/nrow(all),4)
  prop_hme <- round(nrow(sel_hme)/nrow(all),4)
  
  # Compute ttl # ppl or fams w hm/hme > avg, correcting for partial tract areas.
  # Several grps use multiple codes (additive is +1 in sign, subtract is -1)
  # Deal with those special cases first...
  if(grp %in% c("non_white", "no_ch", "nwht_lowin")) {
    num_hm = round(sum(sel_hm[code1] * sel_hm$prop_ttl_area * code1_sign,
                       sel_hm[code2] * sel_hm$prop_ttl_area * code2_sign))
  } else if(grp %in% c("non_wht_ch", "nwht_chpov")){
    num_hm = round(sum(sel_hm[code1] * sel_hm$prop_ttl_area * code1_sign,
                       sel_hm[code2] * sel_hm$prop_ttl_area * code2_sign,
                       sel_hm[code3] * sel_hm$prop_ttl_area * code3_sign,
                       sel_hm[code4] * sel_hm$prop_ttl_area * code4_sign,
                       sel_hm[code5] * sel_hm$prop_ttl_area * code5_sign))
  } else {num_hm <- round(sum(sel_hm[code1] * sel_hm$prop_ttl_area * code1_sign))
  }
  
  if(grp %in% c("non_white", "no_ch", "nwht_lowin")) {
    num_hme = round(sum(sel_hme[code1] * sel_hme$prop_ttl_area * code1_sign,
                        sel_hme[code2] * sel_hme$prop_ttl_area * code2_sign))
  } else if(grp %in% c("non_wht_ch", "nwht_chpov")){
    num_hme = round(sum(sel_hme[code1] * sel_hme$prop_ttl_area * code1_sign,
                       sel_hme[code2] * sel_hme$prop_ttl_area * code2_sign,
                       sel_hme[code3] * sel_hme$prop_ttl_area * code3_sign,
                       sel_hme[code4] * sel_hme$prop_ttl_area * code4_sign,
                       sel_hme[code5] * sel_hme$prop_ttl_area * code5_sign))
  } else {num_hme <- round(sum(sel_hme[code1] * sel_hme$prop_ttl_area * code1_sign))
  }
  
  # Compute avg distance to nearest PA in miles for status grp tracts
  dist_hm <- round(mean(sel_hm$dist_pa_mi),2)
  dist_hme <- round(mean(sel_hme$dist_pa_mi),2)
  
  
    # Run loop thru each state if necessary, combining vals for 2 states into 1
    # for (i in 1:length(f)){ # Loop thru each state
    # }
  
stat_grp <- c(stat_grp, grp)
unit_type <- c(unit_type, unit)
prop_tracts_gt_hm_natl <- c(prop_tracts_gt_hm_natl, prop_hm)
prop_tracts_gt_hme_natl <- c(prop_tracts_gt_hme_natl, prop_hme)
num_gt_hm_natl <- c(num_gt_hm_natl, num_hm)
num_gt_hme_natl <- c(num_gt_hme_natl, num_hme)
pa_dist_mi_gt_hm <- c(pa_dist_mi_gt_hm, dist_hm)
pa_dist_mi_gt_hme <- c(pa_dist_mi_gt_hme, dist_hme)


print(paste0(grp, " tracts complete."))
}

foo <- cbind(loc, buffer, stat_grp, unit_type,
             prop_tracts_gt_hm_natl, prop_tracts_gt_hme_natl,
             num_gt_hm_natl, num_gt_hme_natl,
             pa_dist_mi_gt_hm, pa_dist_mi_gt_hme) %>% as.data.frame()

loc
buffer
write.csv(foo, paste0(out.dir,loc,"_",buffer,"_hm_stats_",today,".csv"))






## ------------------------------------------------------------------------------------------------
# ## Testing grounds
# stat_grps
# # test w black
# boo <- zone_df %>% dplyr::select(AHY2E003, black, hm, hm_energy, prop_ttl_area)
# boo <- boo %>% filter(black == 1)
# boo <- boo %>% filter(hm > hm_base)
# boo$n <- boo$AHY2E003
# boo$n <- round(boo$n * boo$prop_ttl_area)
# sum(boo$n)
# 
# # test w non-white
# boo <- zone_df %>% dplyr::select(AHY2E001, AHZAE003, non_white, hm, hm_energy, prop_ttl_area)
# boo <- boo %>% filter(non_white == 1, hm > hm_base)
# boo$n <- boo$AHY2E001 - boo$AHZAE003
# boo$n <- round(boo$n * boo$prop_ttl_area)
# sum(boo$n)
# # test w nonwhite ch pov
# boo <- zone_df %>% dplyr::select(AIF6CHPOV, AIF7CHPOV, AIF8CHPOV, AIOTHCHPOV, AIGDCHPOV, nwht_chpov, hm, hm_energy, prop_ttl_area)
# boo <- boo %>% filter(nwht_chpov == 1)
# boo <- boo %>% filter(hm > hm_base)
# attach(boo)
# boo$n <- round(AIF6CHPOV + AIF7CHPOV + AIF8CHPOV + AIOTHCHPOV + AIGDCHPOV)
# detach(boo)
# boo$n <- round(boo$n * boo$prop_ttl_area)
# sum(boo$n) 
# # test w low inc
# boo <- zone_df %>% dplyr::select(AHY2E001, low_inc, hm, hm_energy, prop_ttl_area)
# boo <- boo %>% filter(low_inc == 1)
# boo <- boo %>% filter(hm > hm_base)
# boo$n <- boo$AHY2E001
# boo$n <- round(boo$n * boo$prop_ttl_area)
# sum(boo$n)





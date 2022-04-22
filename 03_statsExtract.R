
############################################
## CENSUS STAT EXTRACTION AND SUMMARIZING ##
############################################


##--------------------------------------------

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

##--------------------------------------------


## Select AOI
d <- cr ; s <- txnm; loc = "Castner Range" ; print("Castner Range selected.")
# d <- aka ; s <- nvca; loc = "Avi Kwa Ame" ; print("Avi Kwa Ame selected.")


## Buffer proposed nat'l monument at multiple intervals. Currently m but set to latlong
m_in_mi <- 1609

b1 <- st_buffer(d, 10*m_in_mi) ; b1_size = "10 miles"
b2 <- st_buffer(d, 25*m_in_mi) ; b2_size = "25 miles"
b3 <- st_buffer(d, 50*m_in_mi) ; b3_size = "50 miles"

plot(b3)
plot(b2, add = TRUE)
plot(b1, add = TRUE)
plot(d, add = TRUE)



## Find census tracts that intersect w each buffer.

# 1st, ensure existing sqkm is correct to use to weight #ppl for partial tracts.
area_sqkm <- terra::area(as_Spatial(s)) / 1000000
s$area_km2[1:50] # already in shapefile
area_sqkm[1:50]
identical(round(txnm$area_km2,2), round(area_sqkm,2)) # TRUE
s$area_sqkm <- NULL

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

# Drop geometries to shrink size for looping
s_b1_df <- s_b1 ; s_b1_df$geometry <- NULL
s_b2_df <- s_b2 ; s_b2_df$geometry <- NULL
s_b3_df <- s_b3 ; s_b3_df$geometry <- NULL



####################### DELETE ALL THIS
# ## FOLD THIS ALL TOGETHER INTO JUST ONE TABLE OR DON'T EVEN WORRY AOBUT PULLING OUT VALS
# 
# ## Pull national and state-level hm and hme (human mod from energy) vals
# # Tracts_ind represent total pop (per functions in util.R); use fam for children-defined tracts. 
# attach(hm_natl)
# (hm_natl_ind_mean <- hm_mean[status_group == "tracts_ind"])
# (hme_natl_ind_mean <- hm_energy_mean[status_group == "tracts_ind"])
# (hm_natl_fam_mean <- hm_mean[status_group == "tracts_fam"])
# (hme_natl_fam_mean <- hm_energy_mean[status_group == "tracts_fam"])
# detach(hm_natl)
# 
# # Pull state names to use as filter.
# states <- unique(s$STATE)
# 
# attach(hm_st)
# hm_s1_ind_mean <- hm_mean[status_group == "tracts_ind" & state == f[1]]
# hm_s2_ind_mean <- hm_mean[status_group == "tracts_ind" & state == f[2]]
# hme_s1_ind_mean <- hm_energy_mean[status_group == "tracts_ind" & state == f[1]]
# hme_s2_ind_mean <- hm_energy_mean[status_group == "tracts_ind" & state == f[2]]
# hm_s1_fam_mean <- hm_mean[status_group == "tracts_fam" & state == f[1]]
# hm_s2_fam_mean <- hm_mean[status_group == "tracts_fam" & state == f[2]]
# hme_s1_fam_mean <- hm_energy_mean[status_group == "tracts_fam" & state == f[1]]
# hme_s2_fam_mean <- hm_energy_mean[status_group == "tracts_fam" & state == f[2]]
# detach(hm_st)






## What prop of status grp tracts and what # of ppl/families have hm >avg?

# Create empty vectors for dumping values
stat_grp <- as.character()

# Human mod - all
prop_tracts_gt_hm_natl <- as.numeric()
num_gt_hm_natl <- as.numeric()
prop_tracts_gt_hm_st <- as.numeric()
num_gt_hm_st <- as.numeric()

# Human mod by energy
prop_tracts_gt_hme_natl <- as.numeric()
num_gt_hme_natl <- as.numeric()
prop_tracts_gt_hme_st <- as.numeric()
num_gt_hme_st <- as.numeric()


# Which zone? Turn on/off
# zone <- s_b1_df ; buffer <- b1_size
zone <- s_b2_df ; (buffer <- b2_size)
# zone <- s_b3_df ; buffer <- b3_size

# Grab list of status groups
stat_grps <- c(unique(hm_st$status_group))

# test
i <- 2
boo <- s_b2_df %>% dplyr::select(AHY2E003, black, hm, hm_energy, prop_ttl_area)



## LOOP ## -------------------------------------------------------
for (i in 1:length(stat_grps)){ # Loop thru status grps
  # Assign status grp name
  grp <- stat_grps[i]
  
  # Look up the corresponding col with # ppl for that status grp
  code1 <- match(grp, lu$status_grp) %>% lu$code1[.]
  code1_sign <- match(grp, lu$status_grp) %>% lu$code1_sign[.]
  # ... maybe add in other relevant codes here or skip ... #
  
  # Look up the unit -- ie # ppl or # families
  unit <- match(grp, lu$status_grp) %>% lu$unit[.]
  
  # Find the appropriate baseline based on unit
  hm_base <- hm_natl$hm_mean[hm_natl$status_group == unit]
  hme_base <- hm_natl$hm_energy_mean[hm_natl$status_group == unit]
  
  # Compute ttl # tracts in status grp
  ttl <- sum(zone[grp], na.rm = TRUE)
  
  # Compute prop tracts in status grp w hm or hme > natl avg
  prop_hm <- sum(zone[grp][zone$hm > hm_base,], na.rm = TRUE) / sum(zone[grp], na.rm = TRUE) 
  prop_hme <- sum(zone[grp][zone$hm_energy > hme_base,], na.rm = TRUE) / sum(zone[grp], na.rm = TRUE) 
  
  
  
  
  
  
  
  
  # compute ttl tracts in status grp
  # select tracts in status grp > natl avg for hm
  # compute proportion of ttl tracts and stick in prop_tracts_gt_hm_natl
  # multiply selected pop col by tract weight (to address partial tracts)
  # add pop and stick in num_ppl_gt_hm_natl
  # select tracts in status grp > natl avg for hme
  # compute proportion of ttl tracts and stick in prop_tracts_gt_hme_natl
  # multiply selected pop col by tract weight (to address partial tracts)
  # add pop and stick in num_ppl_gt_hme_natl
  for (i in 1:length(f)){ # Loop thru each state
    # Assign state name
    # extract hm and hme for national
    # extract hm and hme for selected state
    # for status grp 
  } 
  # Combine vals for the 2 diff states before folding into 1
}

  
}  
}
hm_st$hm_mean[hm_st$status_group == "tracts_ind" & hm_st$state == f[1]]



## What prop non-white tracts within 50 miles of Caster Range have > natl median hm?
## What prop white tracts within 50 miles of Caster Range have > natl median hm?
(ttl_tracts_hisp <- sum(txnm_50mi$hispanic, na.rm = TRUE))
(ttl_tracts_wht_nohisp <- sum(txnm_50mi$wht_nohisp, na.rm = TRUE))

(num_tracts_hisp_gt_natl_hm_med <- sum(txnm_50mi$hispanic[txnm_50mi$hm > hm_natl_med], na.rm = TRUE))
(num_tracts_wht_nohisp_gt_natl_hm_med <- sum(txnm_50mi$wht_nohisp[txnm_50mi$hm > hm_natl_med], na.rm = TRUE))


(num_tracts_hisp_gt_tx_hm_med <- sum(txnm_50mi$hispanic[txnm_50mi$STATE == "Texas" &
                                                         txnm_50mi$hm > hm_tx_med], na.rm = TRUE))
(num_tracts_hisp_gt_nm_hm_med <- sum(txnm_50mi$hispanic[txnm_50mi$STATE == "New Mexico" &
                                                         txnm_50mi$hm > hm_nm_med], na.rm = TRUE))
(num_tracts_hisp_gt_state_hm_med <- num_tracts_hisp_gt_tx_hm_med + num_tracts_hisp_gt_nm_hm_med)



(num_tracts_wht_nohisp_gt_tx_hm_med <- sum(txnm_50mi$wht_nohisp[txnm_50mi$STATE == "Texas" &
                                                                 txnm_50mi$hm > hm_tx_med], na.rm = TRUE))
(num_tracts_wht_nohisp_gt_nm_hm_med <- sum(txnm_50mi$wht_nohisp[txnm_50mi$STATE == "New Mexico"
                                                               & txnm_50mi$hm > hm_nm_med], na.rm = TRUE))
(num_tracts_hisp_gt_state_hm_med <- num_tracts_wht_nohisp_gt_tx_hm_med + num_tracts_wht_nohisp_gt_nm_hm_med)


num_tracts_hisp_gt_natl_hm_med/ttl_tracts_hisp
num_tracts_wht_nohisp_gt_natl_hm_med/ttl_tracts_wht_nohisp

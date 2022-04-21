
############################################
## CENSUS STAT EXTRACTION AND SUMMARIZING ##
############################################

# Select AOI
d <- cr ; s <- txnm; loc = "Castner Range" ; print("Castner Range selected.")
# d <- aka ; s <- nvca; loc = "Avi Kwa Ame" ; print("Avi Kwa Ame selected.")


## Buffer proposed nat'l monument at multiple intervals. Currently m but set to latlong
m_in_mi <- 1609

b1 <- st_buffer(d, 10*m_in_mi) ; buff_1_size = "10 miles"
b2 <- st_buffer(d, 25*m_in_mi) ; buff_1_size = "25 miles"
b3 <- st_buffer(d, 50*m_in_mi) ; buff_1_size = "50 miles"

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

s_b1 <- st_intersection(buff_1, s)
s_b2 <- st_intersection(buff_2, s)
s_b3 <- st_intersection(buff_3, s)

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


## Compare to state-level and national hm and hm-energy values.
# Tracts_ind represent total pop (per functions in util.R) 
(hm_natl_mean <- hm_natl$hm_mean[hm_natl$status_group == "tracts_ind"])
(hme_natl_mean <- hm_natl$hm_energy_mean[hm_natl$status_group == "tracts_ind"])

# Pull state names to use as filter.
f <- unique(s$STATE)

hm_s1_mean <- hm_st$hm_mean[hm_st$status_group == "tracts_ind" & hm_st$state == f[1]]
hm_s2_mean <- hm_st$hm_mean[hm_st$status_group == "tracts_ind" & hm_st$state == f[2]]
hme_s1_mean <- hm_st$hm_energy_mean[hm_st$status_group == "tracts_ind" & hm_st$state == f[1]]
hme_s2_mean <- hm_st$hm_energy_mean[hm_st$status_group == "tracts_ind" & hm_st$state == f[2]]


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

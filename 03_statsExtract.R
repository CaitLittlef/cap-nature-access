
############################################
## CENSUS STAT EXTRACTION AND SUMMARIZING ##
############################################

## Buffer proposed nat'l monument at multiple intervals. Currently m but set to latlong
m_mi <- 1609

buff_10mi <- st_buffer(cr, 10*m_mi)
buff_25mi <- st_buffer(cr, 25*m_mi)
buff_50mi <- st_buffer(cr, 50*m_mi)

plot(buff_50mi)
plot(buff_25mi, add = TRUE)
plot(buff_10mi, add = TRUE)
plot(cr, add = TRUE)


## Extract census tracts within each buffer.
# First, ensure existing sqkm calcs are correct.
# B/c for partial census tracts within a given buffer, we'll scale # ppl.

area_sqkm <- terra::area(as_Spatial(txnm)) / 1000000
txnm$area_km2[1:50] # already in shapefile
area_sqkm[1:50]
identical(round(txnm$area_km2,2), round(area_sqkm,2))
# TRUE. Rounded to the 100th sqkm, that's close enough.
txnm$area_sqkm <- NULL

# Initial intersection threw this error
  # Error in s2_geography_from_wkb(x, oriented = oriented, check = check) : 
  #   Evaluation error: Found 1 feature with invalid spherical geometry.
  # [1] Loop 228 is not valid: Edge 8281 has duplicate vertex with edge 8285
# Advised on github: sf_use_s2(FALSE)
sf_use_s2(FALSE)

txnm_10mi <- st_intersection(buff_10mi, txnm)
txnm_25mi <- st_intersection(buff_25mi, txnm)
txnm_50mi <- st_intersection(buff_50mi, txnm)


# Compute new areas to account for partial census tracts
txnm_10mi$area_km2_new <- terra::area(as_Spatial(txnm_10mi)) / 1000000
txnm_25mi$area_km2_new <- terra::area(as_Spatial(txnm_25mi)) / 1000000
txnm_50mi$area_km2_new <- terra::area(as_Spatial(txnm_50mi)) / 1000000

txnm_10mi$area_km2[1:10]
txnm_10mi$area_km2_new[1:10]

txnm_25mi$area_km2[125:150]
txnm_25mi$area_km2_new[125:150]

txnm_50mi$area_km2[150:200]
txnm_50mi$area_km2_new[150:200]



## Compare to state-level and national hm values
(hm_natl_med <- hm_natl$hm_mean[hm_natl$status_group == "tracts_ind"])
(hm_tx_med <- hm_st$hm_mean[hm_st$status_group == "tracts_ind" & hm_st$state == "Texas"])
(hm_nm_med <- hm_st$hm_mean[hm_st$status_group == "tracts_ind" & hm_st$state == "New Mexico"])


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

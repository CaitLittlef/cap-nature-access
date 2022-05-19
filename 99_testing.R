## ------------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------------


# ## Testing grounds
# loc
# buffer_mi
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




# ## Compare to state-level and national hm values
# (hm_natl_med <- hm_natl$hm_mean[hm_natl$status_group == "tracts_ind"])
# (hm_tx_med <- hm_st$hm_mean[hm_st$status_group == "tracts_ind" & hm_st$state == "Texas"])
# (hm_nm_med <- hm_st$hm_mean[hm_st$status_group == "tracts_ind" & hm_st$state == "New Mexico"])
# (hm_az_med <- hm_st$hm_mean[hm_st$status_group == "tracts_ind" & hm_st$state == "Arizona"])
# (hm_ca_med <- hm_st$hm_mean[hm_st$status_group == "tracts_ind" & hm_st$state == "California"])
# (hm_nv_med <- hm_st$hm_mean[hm_st$status_group == "tracts_ind" & hm_st$state == "Nevada"])
# 
# 
# ## What prop status grp tracts within X miles of aoi have > natl or state median hm?
# (ttl_tracts <- sum(zone_df$hisp_chpov, na.rm = TRUE))
# (num_tracts_gt_natl_hm_med <- sum(zone_df$hisp_chpov[zone_df$hm > hm_natl_med], na.rm = TRUE))
# 
# num_tracts_gt_hm_st1 <- sum(zone_df$hisp_chpov[zone_df$STATE == "Texas" &
#                                                           zone_df$hm > hm_az_med], na.rm = TRUE)
# num_tracts_gt_hm_st2 <- sum(zone_df$hisp_chpov[zone_df$STATE == "New Mexico" &
#                                                           zone_df$hm > hm_nv_med], na.rm = TRUE)
# # num_tracts_gt_hm_st3 <- sum(zone_df$non_white[zone_df$STATE == "California" &
# #                                                         zone_df$hm > hm_ca_med], na.rm = TRUE)
# (num_tracts_gt_state_hm <- num_tracts_gt_hm_st1 + num_tracts_gt_hm_st2)
# # (num_tracts_gt_state_hm <- num_tracts_gt_hm_st1 + num_tracts_gt_hm_st2 + num_tracts_gt_hm_st3)
# 95/112
# 96/112
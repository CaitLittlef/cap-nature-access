
############################################
## CENSUS STAT EXTRACTION AND SUMMARIZING ##
############################################


##---------------------------------------------------------------------

## Chose AOI, select buffer_mi, and whether it's real or random sample.
# Random sample are of polys that are same size as selected aoi plus buffer_mi.
# For poly width, take sqrt of aoi area to get "one side", halve it, add buffer_mi.
n <- 500
# n <- 50

# loc = "CR" ; d <- cr ; n <- 1 # set n = 1 for division in loop
# loc = "CR_randSample" ; d <- cr
# loc = "AKA" ; d <- aka; n <- 1
loc = "AKA_randSample" ; d <- aka

# buffer_mi <- 10
buffer_mi <- 25
# buffer_mi <- 50


if(loc == "CR") b <- st_buffer(d, buffer_mi*m_in_mi)
if(loc == "CR_randSample") b <- sf::st_sample(domain, size = n) %>% as_Spatial() %>%
  gBuffer(width = sqrt(terra::area(as_Spatial(d)))/2 + buffer_mi*m_in_mi, byid = TRUE) %>%
  st_as_sf()
if(loc == "AKA") b <- st_buffer(d, buffer_mi*m_in_mi)
if(loc == "AKA_randSample") b <- sf::st_sample(domain, size = n) %>% as_Spatial() %>%
  gBuffer(width = sqrt(terra::area(as_Spatial(d)))/2 + buffer_mi*m_in_mi, byid = TRUE) %>%
  st_as_sf()

####

print(paste0(loc," selected with buffer of ", buffer_mi, " miles."))

plot(st_geometry(domain))
plot(b, add = TRUE)



## ----------------------------------------

## Find census tracts that intersect w each buffer.

# May need to make space for next calcs; can remove pas if not doing dist (below)
remove(all) ; remove(sel_hm) ; remove(sel_hme) ; remove(pa)
gc()


start <- Sys.time()
zone <- st_intersection(b, tracts) 
print(Sys.time() - start)

# remove(tracts)

# Eliminate any rows w NAs and rows w zero ppl else return NAs in loop.
zone <- zone[zone$AHY2E001 > 0,]
# which(is.na(zone), arr.ind = TRUE)
zone <- na.omit(zone)

# Compute new areas to account for partial census tracts.
zone$area_km2_new <- terra::area(as_Spatial(zone)) / 1000000

# Compute prop of orig tract area for scaling pop #s within all tracts (most should = 1)
zone$prop_ttl_area <- zone$area_km2_new/zone$area_km2 ; Mode(round(zone$prop_ttl_area, 4))



## --------------------------------------------------------------------

# Calc dist to nearest PAs.

# IF NOT RUNNING DIST (EG WITH LARGER SAMPLE), SET DUMMY VALUE...
# ...SO THAT LOOP WORKS AND THEN APPEND SMALLER SAMPLE VALUES (EG N=100)
zone$dist_pa_mi <- 0 ; dist <- "dist0ff" ; remove(pa)



# dist <- "distOn
# # What size cut-off? PAs must be at least...
# size <- 50 #sqkm
# 
# # Source code that calcs dist to nearest PA for each tract and adds to shapefile.
# start <- Sys.time()
# source(paste0(wd, "analyses/cap-nature-access/04_distPA.R"), echo = FALSE)
# print(Sys.time() - start)




# set geom to null for faster looping
zone_df <- zone ; zone_df$geometry <- NULL

# object.size(zone)
remove(zone) ; gc()
# object.size(zone_df)


## --------------------------------------------------------------------------


## What prop of status grp tracts and what # of ppl/families have hm >avg?

# Create empty vectors for dumping values
stat_grp <- as.character()
unit_type <- as.character()

# Status grp tract tallies
num_tracts <- as.numeric()
num_stat_grp_tracts <- as.numeric()

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
  prop_hm <- round(nrow(sel_hm)/nrow(all),2)
  prop_hme <- round(nrow(sel_hme)/nrow(all),2)
  
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
  
  # # Compute avg distance to nearest PA in miles for status grp tracts
  dist_hm <- round(mean(sel_hm$dist_pa_mi),2)
  dist_hme <- round(mean(sel_hme$dist_pa_mi),2)
  
  
    # Run loop thru each state if necessary, combining vals for 2 states into 1
    # for (i in 1:length(f)){ # Loop thru each state
    # }
  
stat_grp <- c(stat_grp, grp)
unit_type <- c(unit_type, unit)
num_tracts <- c(num_tracts, round(nrow(zone_df)/n,0))
num_stat_grp_tracts <- c(num_stat_grp_tracts, round(nrow(all)/n,0)) # to get avg per sample
prop_tracts_gt_hm_natl <- c(prop_tracts_gt_hm_natl, prop_hm)
prop_tracts_gt_hme_natl <- c(prop_tracts_gt_hme_natl, prop_hme)
num_gt_hm_natl <- c(num_gt_hm_natl, round(num_hm/n,0))
num_gt_hme_natl <- c(num_gt_hme_natl, round(num_hme/n,0))
pa_dist_mi_gt_hm <- c(pa_dist_mi_gt_hm, dist_hm)
pa_dist_mi_gt_hme <- c(pa_dist_mi_gt_hme, dist_hme)


print(paste0(grp, " tracts complete."))
}

foo <- cbind(loc, buffer_mi, num_tracts, stat_grp, unit_type, num_stat_grp_tracts,
             prop_tracts_gt_hm_natl, prop_tracts_gt_hme_natl,
             num_gt_hm_natl, num_gt_hme_natl,
             pa_dist_mi_gt_hm, pa_dist_mi_gt_hme) %>% as.data.frame()

loc
buffer_mi
write.csv(foo, paste0(out.dir,loc,"_",buffer_mi,"mi_n",n,"_",dist,"_hm_stats_",today,".csv"))

gc()

loc




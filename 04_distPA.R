##############################################
## DISTANCE TO NEAREST GAP 1/2 PA; POP CALC ##
##############################################

# Where are we working right now?
loc ; buffer 
# Prob fold this all into loop to give avg dist to nearest PA for each tract.
# plus maybe how close it will be to Castner Range
(states <- unique(zone$STATE))
states_abb <- c("NM", "TX")



# Load PADUS.
padus <- st_read("C:/Users/clitt/OneDrive/Desktop/data_gen/PADUS_2_1/PAD_US2_1.gdb",
              layer="PADUS2_1Combined_Proclamation_Marine_Fee_Designation_Easement")

# Filter by category, state, and GAP status
cats <- c("Proclamation", "Designation", "Easement", "Fee")
pa <- padus %>% filter(State_Nm %in% states_abb,
                       Category %in% cats,
                       GAP_Sts == "1" | GAP_Sts == "2")

# To pre-empt error related to multisurface geometries, here's fix...
# ref: https://gis.stackexchange.com/questions/389814/r-st-centroid-geos-error-unknown-wkb-type-12
# install.packages("gdalUtilities")
library(gdalUtilities)
ensure_multipolygons <- function(X) {
  tmp1 <- tempfile(fileext = ".gpkg")
  tmp2 <- tempfile(fileext = ".gpkg")
  st_write(X, tmp1)
  ogr2ogr(tmp1, tmp2, f = "GPKG", nlt = "MULTIPOLYGON")
  Y <- st_read(tmp2)
  st_sf(st_drop_geometry(X), geom = st_geometry(Y))
}
pa <- ensure_multipolygons(pa)

# Consider filtering by size. Confirm unit of pa$SHAPE_area and that it's accurate.
# area_sqm <- terra::area(as_Spatial(pa))
# mean(abs(area_sqm - pa$SHAPE_Area)) ; max(abs(area_sqm - pa$SHAPE_Area)) 
# # Close enough.
# # What are focal PA areas?
# terra::area(as_Spatial(cr)) / 1000000 # 27 sqkm
# terra::area(as_Spatial(aka)) / 1000000 # 1817 sqkm
# # Threshold of at least 100 sqkm
sqm_in_sqkm <- 1000000
pa <- pa %>% filter(SHAPE_Area >= 50*sqm_in_sqkm)


# Grab centroids of these PAs (distances won't be to border, but so it goes)
pa_cntr <- st_centroid(pa)
d_cntr <- st_centroid(d) ; loc # data for selected loc

# How close is nearest PA to centroid of CR or AKA?
dist <- as.numeric(c(st_distance(d_cntr, pa_cntr)/1000))
min(dist) # only 6.5 km. Could clip to certain sized PA??

# How close is each census tract to nearest PA?
dists <- as.numeric()
for (i in 1:nrow(zone_geom)){
  dist <- as.numeric(c(st_distance(zone_geom[i,], pa_cntr)/1000))
  min <- min(dist)
  dists <- cbind(dists, min)
}

# ^ ABOVE GIVES DISTANCE OF EACH CENSUS TRACT TO NEAREST PA.
# ADD THIS INTO SHAPEFILE AND THEN COMPUTE AVG DISTANCE FOR PPL & FAMS
# WEIGHTED BY THE # OF PPL & FAMS
# PUT THAT IN SUMMARY DOC FROM 03_statsExtract.R







## OLD STUFF -------------------------------------------------------
# 
# # The nearest PA had been 30mi away. For ppl w/in 10mi proposed NM, that's 30-10=20 mi closer on avg.
# # How many people are in there? Look at hispanic w > natl median HM.
# # sCale pop #s by proportion of original tract that's represneted w/in 10 mile buffer.
# 
# # Hispanic
# txnm_10mi$AHZAE012_new <- round(txnm_10mi$AHZAE012*txnm_10mi$prop_ttl_area,0)
# 
# txnm_10mi$AHZAE012[1:10]
# txnm_10mi$AHZAE012_new[1:10]
# 
# sum(txnm_10mi$AHZAE012_new[txnm_10mi$hm > hm_natl_med], na.rm = TRUE)
# 
# 
# 

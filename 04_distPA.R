##############################################
## DISTANCE TO NEAREST GAP 1/2 PA; POP CALC ##
##############################################

## ** IN US, DOESN'T INCLUDE CIUDAD JUAREZ ** ##

padus <- st_read("G:/My Drive/1Data/PADUS_2_1/PAD_US2_1.gdb",
              layer="PADUS2_1Combined_Proclamation_Marine_Fee_Designation_Easement")

states <- c("NM", "TX")
cats <- c("Proclamation", "Designation", "Easement", "Fee")

pa_txnm <- padus %>% filter(State_Nm == "NM" | State_Nm == "TX")
pa_txnm <- pa_txnm %>% filter(Category %in% keeps)
pa_txnm_gap12 <- pa_txnm %>% filter(GAP_Sts == "1" | GAP_Sts == "2")

plot(st_geometry(pa_txnm_gap12))


# get centroids of each PA that's GAP1/2 (or consider doing border)
pa_cntr <- st_centroid(pa_txnm_gap12)
# Error related to multisurface geometries. HEre's fix
# ref: https://gis.stackexchange.com/questions/389814/r-st-centroid-geos-error-unknown-wkb-type-12

install.packages("gdalUtilities")
library(gdalUtilities)

ensure_multipolygons <- function(X) {
  tmp1 <- tempfile(fileext = ".gpkg")
  tmp2 <- tempfile(fileext = ".gpkg")
  st_write(X, tmp1)
  ogr2ogr(tmp1, tmp2, f = "GPKG", nlt = "MULTIPOLYGON")
  Y <- st_read(tmp2)
  st_sf(st_drop_geometry(X), geom = st_geometry(Y))
}

pa_txnm_gap12 <- ensure_multipolygons(pa_txnm_gap12)

pa_cntr <- st_centroid(pa_txnm_gap12)
cr_cntr <- st_centroid(cr)

dist <- as.numeric(c(st_distance(cr_cntr, pa_cntr)/1000))
min(dist) # only 6.5 km. Could clip to certain sized PA??

(area_sqkm <- terra::area(as_Spatial(pa_txnm_gap12)) / 1000000)
pa_txnm_gap12$SHAPE_Area[1:50] # already in shapefile
# They match close enough. So just stick with what's in shapefile

# Retain only PAs that are at least 30 sq km, which is roughly size of Castner Range
pa_txnm_gap12 <- pa_txnm_gap12[pa_txnm_gap12$SHAPE_Area / 1000000 > 30,]
pa_cntr <- st_centroid(pa_txnm_gap12)

dist_km <- as.numeric(c(st_distance(cr_cntr, pa_cntr)/1000))
min(dist_km) # now 50km
dist_mi <- dist_km/(m_mi/1000)
min(dist_mi)

# The nearest PA had been 30mi away. For ppl w/in 10mi proposed NM, that's 30-10=20 mi closer on avg.
# How many people are in there? Look at hispanic w > natl median HM.
# sCale pop #s by proportion of original tract that's represneted w/in 10 mile buffer.

# Hispanic
txnm_10mi$AHZAE012_new <- round(txnm_10mi$AHZAE012*txnm_10mi$prop_ttl_area,0)

txnm_10mi$AHZAE012[1:10]
txnm_10mi$AHZAE012_new[1:10]

sum(txnm_10mi$AHZAE012_new[txnm_10mi$hm > hm_natl_med], na.rm = TRUE)




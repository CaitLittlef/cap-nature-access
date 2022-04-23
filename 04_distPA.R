##############################################
## DISTANCE TO NEAREST GAP 1/2 PA; POP CALC ##
##############################################

# Where are we working right now?
loc ; buffer ; states_abb
# Prob fold this all into loop to give avg dist to nearest PA for each tract.
# plus maybe how close it will be to Castner Range

# # Load PADUS only if it's not already loaded.
if(!exists("padus")) padus <- st_read("C:/Users/clitt/OneDrive/Desktop/data_gen/PADUS_2_1/PAD_US2_1.gdb",
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
# # Threshold of at least 50 sqkm
sqm_in_sqkm <- 1000000
pa <- pa %>% filter(SHAPE_Area >= 50*sqm_in_sqkm)
# plot(pa)

# Grab centroids of these PAs (distances won't be to border, but so it goes)
pa_cntr <- st_centroid(pa)
d_cntr <- st_centroid(d) ; loc # data for selected loc

# How close is nearest PA to centroid of CR or AKA?
dist <- as.numeric(c(st_distance(d_cntr, pa_cntr)))
min(dist) 

# How close is each census tract to nearest PA?
dists_pa <- as.numeric()
dists_d <- as.numeric()
for (i in 1:nrow(zone)){
  dist_pa <- as.numeric(c(st_distance(st_centroid(zone[i,]), pa_cntr)))
  min <- min(dist_pa)
  dists_pa <- rbind(dists_pa, min)
  dist_d <- as.numeric(c(st_distance(st_centroid(zone[i,]), d_cntr)))
  min <- min(dist_d)
  dists_d <- rbind(dists_d, min)
}

c <- cbind(dists_pa, dists_d)
dist_pa_mi <- c/m_in_mi

# Assign those to each census tract.
zone$dist_pa_mi <- dist_pa_mi




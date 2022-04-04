
######################################
## DATA COLLECTION & PRE-PROCESSING ##
######################################

## Function to load features, set common crs, and fix any invalid geometries
# CRS based on what PADUS came in.
load_f <- function(f) {
  # proj.crs <- "+proj=longlat +datum=WGS84 +no_defs"
  proj.crs <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
  read_sf(f) %>%
    # st_transform(proj.crs) %>%
    st_make_valid() %>%
    st_buffer(dist = 0)
}
# proj.crs <- "+proj=longlat +datum=WGS84 +no_defs"
proj.crs <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"


## Load data
# load tracts (function takes way too long")
# tracts <- load_f(paste0(data.dir,"2020 data - output/output_tract_hm_attr_2013_2017.shp"))
tracts <- read_sf(paste0(data.dir,"2020 data - output/output_tract_hm_attr_2013_2017.shp"))
txnm <- tracts %>% dplyr::filter(STATE == "Texas" | STATE == "New Mexico") ; remove(tracts)
# nvca <- tracts %>% dplyr::filter(STATE == "Nevada" | STATE == "California")
# Set to meter-based CRS now that it's smaller
txnm <- st_transform(txnm, proj.crs)
# plot(txnm)



# Load padus # Checked via ArcMap to see appropriate shapefile
padustx <- load_f(paste0(data.dir,"padus/PADUS2_1Proclamation_StateTX.shp"))
# Castner Range proposed NM within Fort Bliss
fb <- padustx %>% filter(Unit_Nm == "Fort Bliss") ; remove(padustx)
# Split fb into individual polygons to retain only westerly Caster area
fb <- fb %>% st_cast("POLYGON")
# Trial and error got me to the appropriate polygon (tried all obs 1-6)
# plot(st_geometry(fb[6,]))
cr <- fb[6,] ; remove(fb)
# Only retain geometry, which is column 42
cr <- cr[,42]
plot(cr)



# Load hm status by state and national from prior effort
hm_st <- read.csv(paste0(data.dir,"2020 data - output/output_hm_by_status_by_state.csv"))
hm_natl <- read.csv(paste0(data.dir,"2020 data - output/output_hm_by_status_natl.csv"))


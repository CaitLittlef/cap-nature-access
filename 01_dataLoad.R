
######################################
## DATA COLLECTION & PRE-PROCESSING ##
######################################



## Function to load features, set common crs (based on PADUS) and fix any invalid geometries

load_f <- function(f) {
  # proj.crs <- "+proj=longlat +datum=WGS84 +no_defs"
  proj.crs <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
  read_sf(f) %>%
    st_transform(proj.crs) %>%
    st_make_valid() %>%
    st_buffer(dist = 0)
}
# proj.crs <- "+proj=longlat +datum=WGS84 +no_defs"
proj.crs <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"



## Load data 

# Load tracts (function takes way too long on giant shapefile). 
tracts <- read_sf(paste0(data.dir,"2020 data - output/output_tract_hm_attr_2013_2017.shp"))

# Subset to relevant states; now that they're smaller, transform.
txnm <- tracts %>%
  dplyr::filter(STATE == "Texas" | STATE == "New Mexico") %>%
  st_transform(proj.crs)

nvca <- tracts %>%
  dplyr::filter(STATE == "Nevada" | STATE == "California") %>%
  st_transform(proj.crs)

remove(tracts)

# Load CR & AKA, just keeping geometry
cr <- load_f(paste0(data.dir,"CastnerRange/CastnerRange.shp"))
aka <- load_f(paste0(data.dir,"AviKwaAme/20220103_updatedAKAshp.shp"))
cr <- cr[,"geometry"] ; plot(cr)
aka <- aka[,"geometry"] ; plot(aka)


# Load hm status by state and national from prior effort
hm_st <- read.csv(paste0(data.dir,"2020 data - output/output_hm_by_status_by_state.csv"), stringsAsFactors = FALSE)
hm_natl <- read.csv(paste0(data.dir,"2020 data - output/output_hm_by_status_natl.csv"), stringsAsFactors = FALSE)


# Load in look-up for census codes to get raw #s of individs & fams
# install.packages("readxl")
library(readxl)
lu <- read_excel(paste0(data.dir,"lu_census_codes.xlsx"),sheet = "lu")

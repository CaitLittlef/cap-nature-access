
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


today <- paste0(mid(Sys.Date(),3,2),
                mid(Sys.Date(),6,2),
                mid(Sys.Date(),9,2))

m_in_mi <- 1609
sqm_in_sqkm <- 1000000


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



## !!!!!!!!!! --------------------------------------------
# If subsetting to Central Plains & Western US from CONUS, change in 3 places:
# line 70
# line 81
# line 135
## !!!!!!!!!! --------------------------------------------

##-------------------------------------------------------------

# Load CONUS and consider keeping only states in western US/Central Plains

domain <- load_f("C:/Users/clitt/OneDrive/Desktop/data_gen/political/tl_2012_us_state.shp")
nix <- c("Alaska",
         "American Samoa",
         "Commonwealth of the Northern Mariana Islands",
         "Guam",
         "Puerto Rico",
         "Hawaii",
         "United States Virgin Islands")
domain <- domain %>% filter(!NAME %in% nix)
# keeps <- c("Washington", "Oregon", "California", "Idaho", "Montana",
           # "Wyoming", "Nevada", "Utah", "Colorado", "Arizona",
           # "New Mexico", "North Dakota", "South Dakota", "Nebraska",
           # "Kansas", "Oklahoma", "Texas")
# domain <- domain %>% filter(NAME %in% keeps)
unique(domain$NAME)

##-------------------------------------------------------------

# Load tracts (function takes way too long on giant shapefile). 
tracts <- read_sf(paste0(data.dir,"2020 data - output/output_tract_hm_attr_2013_2017.shp"))

# Potentially subset to western and Central Plains states.
tracts <- tracts %>%
  # filter(STATE %in% keeps) %>%
  st_transform(proj.crs)


# Later intersections with tract data threw this error: 
# Error in s2_geography_from_wkb(x, oriented = oriented, check = check) : 
#   Evaluation error: Found 1 feature with invalid spherical geometry.
# [1] Loop 228 is not valid: Edge 8281 has duplicate vertex with edge 8285
# Advised on github: sf_use_s2(FALSE)
sf_use_s2(FALSE)

# # Confirm that area in tracts shapefile is accurate
# area_sqkm <- terra::area(as_Spatial(tracts)) / 1000000
# mean(abs(area_sqkm - tracts$area_km2)) ; max(abs(area_sqkm - tracts$area_km2))
# identical(round(tracts$area_km2), round(area_sqkm)) # rounded to zero points. close enough
# area_sqkm <- NULL


# Load in look-up for census codes to get raw #s of individs & fams
# install.packages("readxl")
library(readxl)
lu <- read_excel(paste0(data.dir,"lu_census_codes.xlsx"),sheet = "lu")



##-------------------------------------------------------------

# Load CR & AKA, just keeping geometry
cr <- load_f(paste0(data.dir,"CastnerRange/CastnerRange.shp"))
aka <- load_f(paste0(data.dir,"AviKwaAme/20220103_updatedAKAshp.shp"))
cr <- cr[,"geometry"] ; plot(cr)
aka <- aka[,"geometry"] ; plot(aka)



##-------------------------------------------------------------

# Load hm status by state and national from prior effort
hm_st <- read.csv(paste0(data.dir,"2020 data - output/output_hm_by_status_by_state.csv"), stringsAsFactors = FALSE)
hm_natl <- read.csv(paste0(data.dir,"2020 data - output/output_hm_by_status_natl.csv"), stringsAsFactors = FALSE)



##-------------------------------------------------------------

# Load PADUS only if it's not already loaded.
if(!exists("padus")) padus <- st_read("C:/Users/clitt/OneDrive/Desktop/data_gen/PADUS_2_1/PAD_US2_1.gdb",
                                      layer="PADUS2_1Combined_Proclamation_Marine_Fee_Designation_Easement")

# keeps <- c("WA", "OR", "CA", "ID", "MT", "WY", "NV", "UT", "CO", "AZ", "NM", "ND", "SD", "NE", "KS", "OK", "TX")
# Filter by category, state, and GAP status
cats <- c("Proclamation", "Designation", "Easement", "Fee")
pa <- padus %>% filter(Category %in% cats,
                       # State_Nm %in% keeps,
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

# # Confirm unit of pa$SHAPE_area and that it's accurate.
# area_sqm <- terra::area(as_Spatial(pa))
# mean(abs(area_sqm - pa$SHAPE_Area)) ; max(abs(area_sqm - pa$SHAPE_Area))
# # max is 1.3 sq km. Close enough.

remove(padus)
gc()

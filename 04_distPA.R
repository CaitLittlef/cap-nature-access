####################################
## DISTANCE TO NEAREST GAP 1/2 PA ##
####################################

# Nb subsetting PAs to Cr and AK states is faster...
# but not appreciably so, so just retain all PAs,
# which are needed for random samples anyways. 

# # Pull state abbreviations for selecting PAs from PADUS
# (states <- unique(zone$STATE))
# (states_abb <- if("Texas" %in% states) c("NM", "TX") else c("CA", "NV"))
# # Filter for states adjacent to AOI or 
# pa <- pa %>% filter(State_Nm %in% states_abb)


# Size threshold set in 03_statsExtract.R
pa <- pa %>% filter(SHAPE_Area >= size*sqm_in_sqkm)

# Grab centroids of these PAs (distances won't be to border, but so it goes)
pa_cntr <- st_centroid(pa)
d_cntr <- st_centroid(d) # data for selected AOI

# How close is nearest PA to centroid of CR or AKA?
# dist <- as.numeric(c(st_distance(d_cntr, pa_cntr)))
# min(dist) 

# How close is each census tract to nearest PA?
# Initializes the progress bar
pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = nrow(zone), # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=")   # Character used to create the bar

dists_pa <- as.numeric()
dists_d <- as.numeric()
for (i in 1:nrow(zone)){
  # Distance btwn each tract's centroid and each pa's centroid
  dist_pa <- as.numeric(c(st_distance(st_centroid(zone[i,]), pa_cntr)))
  min <- min(dist_pa)
  dists_pa <- rbind(dists_pa, min)
  # Distance btwn each tract's centroid and centroid of AOI
  dist_d <- as.numeric(c(st_distance(st_centroid(zone[i,]), d_cntr)))
  min <- min(dist_d)
  dists_d <- rbind(dists_d, min)
  
  setTxtProgressBar(pb, i)
  
}

close(pb) # Close the connection

c <- cbind(dists_pa, dists_d)
dist_pa_mi <- c/m_in_mi

# Assign those to each census tract.
zone$dist_pa_mi <- dist_pa_mi




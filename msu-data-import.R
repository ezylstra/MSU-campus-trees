# Creating files for importing MSU data to NPN database
# 23 June 2026

library(dplyr)
library(stringr)
library(terra)
library(tidyterra)
library(rnpn)

# Load files ------------------------------------------------------------------#

# Tree list (and remove any trees without lat/lon)
trees <- read.csv("data/MSUTreeList.csv")
trees <- trees %>%
  filter(!is.na(latitude))

# Load phenology data, 2017-2025
dat <- read.csv("data/msu-phenology-data-2017-2025.csv")

# Load sites and site names
sites <- vect("nn-sites/nn-sites-revised2.shp")
sitenames <- read.csv("nn-sites/nn-site-names-revised.csv")

# Site/station import ---------------------------------------------------------#

# Add sitenames to spatvector
sites <- sites %>%
  left_join(select(sitenames, -current_name), by = c("site_no" = "number")) %>%
  select(-name) %>%
  rename(sitename = new_name)

# Write to file (commented out for now to avoid overwriting accidentally)
# writeVector(sites, "nn-import/msu-sites.shp")

# Get centroid locations
sites_centroid <- terra::centroids(sites) %>%
  as.data.frame(geom = "XY") %>%
  rename(lat = y,
         lon = x)

# Get site areas (in sq meters)
sites_centroid$area_m2 <- round(terra::expanse(sites, unit = "m"))

# Create station table (make up 6-digit station IDs for now for easy matching)
stations <- sites_centroid %>%
  mutate(station_id = 100000 + 1:nrow(sites_centroid)) %>%
  mutate(lat_lon_datum = "WGS84",
         state = "MI",
         country = "USA",
         area_of_site_units_id = 313) %>%
  rename(latitude = lat,
         longitude = lon,
         station_name = sitename,
         area_of_site = area_m2) %>%
  select(station_id, station_name, latitude, longitude, lat_lon_datum,
         state, country, area_of_site, area_of_site_units_id)
  
# Write to file (commented out for now to avoid overwriting accidentally)
# write.csv(stations, "nn-import/stations.csv", row.names = FALSE)

# Trees import ----------------------------------------------------------------#

# Attach site to trees dataframe
treesv <- vect(trees, geom = c("longitude", "latitude"), crs = "epsg:4326")
trees_sites <- terra::extract(sites, treesv)
trees$station_name <- trees_sites$sitename
trees <- trees %>%
  left_join(select(stations, station_id, station_name), by = "station_name") %>%
  mutate(lat_lon_datum = "WGS84") %>%
  rename(individual_userstr = accession)

# Get NPN species ID numbers and common names
spp <- npn_species() %>%
  select(-species_type) %>%
  data.frame()

# Active (1/0): use info about whether monitored in 2025


  





# Determine which years each tree was monitored and attach to tree list
dat <- dat %>%
  group_by(tree) %>%
  summarize(minyr = min(year),
            maxyr = max(year),
            nyrs = n_distinct(year),
            y2025 = ifelse(2025 %in% year, 1, 0),
            nobs = n()) %>%
  mutate(yrs = paste0(minyr, "-", maxyr)) %>%
  mutate(obs = paste0(nyrs, " (", nobs, ")")) %>%
  data.frame() %>%
  select(tree, yrs, obs, y2025)
trees <- trees %>%
  left_join(dat, by = c("accession" = "tree"))



# Attach site to trees dataframe
treesv <- vect(trees, geom = c("longitude", "latitude"), crs = "epsg:4326")
trees_sites <- terra::extract(sites, treesv)
trees$site <- trees_sites$name
trees$site_no <- trees_sites$site_no
trees <- left_join(trees, select(sitenames, -current_name), 
                   by = c("site_no" = "number")) %>%
  rename(sitename = new_name)
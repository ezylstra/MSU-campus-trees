# Creating files for importing MSU data to NPN database
# 24 June 2026

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
# write.csv(stations, "nn-import/station.csv", row.names = FALSE)

# Trees import ----------------------------------------------------------------#

# Get NPN species ID numbers and common names
npn_spp <- npn_species() %>%
  select(species_id, genus, species, common_name) %>%
  mutate(scientific_name = paste(genus, species)) %>%
  data.frame()

# Correct spelling of coffeetree species and add NPN species information 
trees <- trees %>%
  mutate(scientific_name = ifelse(scientific_name == "Gymnocladus dioicius",
                                  "Gymnocladus dioicus", scientific_name)) %>%
  rename(msu_common_name = common_name) %>%
  left_join(select(npn_spp, species_id, common_name, scientific_name),
            by = "scientific_name") %>%
  select(-msu_common_name)

# Remove eastern white pine from tree list since MSU collected data on 
# phenophases that are not available for that species in the database
trees <- trees %>%
  filter(common_name != "eastern white pine")

# Attach site to trees dataframe and generate tree "nicknames" (combination
# of species common names and accession numbers)
treesv <- vect(trees, geom = c("longitude", "latitude"), crs = "epsg:4326")
trees_sites <- terra::extract(sites, treesv)
trees$station_name <- trees_sites$sitename
trees <- trees %>%
  left_join(select(stations, station_id, station_name), by = "station_name") %>%
  mutate(individual_userstr = paste0(common_name, "-", accession))

# If trees were monitored in 2025, list them as active (1).
active_trees <- dat %>%
  rename(accession = tree) %>%
  group_by(accession) %>%
  summarize(active = ifelse(2025 %in% year, 1, 0)) %>%
  data.frame()
trees <- trees %>%
  left_join(active_trees, by = "accession")

# Create station-species-indiviudal table (make up 8-digit indiviual IDs for now 
# for easy matching)
ssi <- trees %>%
  mutate(individual_id = 10000000 + 1:nrow(trees)) %>%
  mutate(lat_lon_datum = "WGS84") %>%
  select(individual_id, station_id, station_name, species_id, 
         individual_userstr, latitude, longitude, lat_lon_datum)

# Write to file (commented out for now to avoid overwriting accidentally)
# write.csv(ssi, "nn-import/station-species-individual.csv", row.names = FALSE)


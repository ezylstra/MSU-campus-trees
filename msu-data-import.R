# Creating files for importing MSU data to NPN database
# 24 June 2026

library(dplyr)
library(stringr)
library(terra)
library(tidyterra)
library(tidyr)

##### Need to create observers????? (each with note that observers are either
##### numbers [anonymized IDs] or when observer isn't known, then a generic 
##### observer created for MSU set?)

# Questions for Jeff:
# about Person Table: 
  # What field should anonymized 8-digit number go in?
  # For observer = unknown, I can create one new person: Anon MSUstudent
# about raw_abundance_value in observation table (is it exactly what it sounds like?)
# about observation_group_id having no additional information

##### In observation comments field: probably want to report the original color/
##### fall values. eg, Percent of leaves fallen = X; Percent of leaves that are 
##### colored = X

# Load files ------------------------------------------------------------------#

# Tree list (remove any trees without lat/lon)
trees <- read.csv("data/MSUTreeList.csv")
trees <- trees %>%
  filter(!is.na(latitude))

# Load phenology data, 2017-2025
dat <- read.csv("data/msu-phenology-data-2017-2025.csv")

# Load sites and site names
sites <- vect("nn-sites/nn-sites-revised2.shp")
sitenames <- read.csv("nn-sites/nn-site-names-revised.csv")

# Load phenophase, intensity information for MSU tree species (created file
# in msu-spp-php-info.R)
phpint <- read.csv("data/msu-species-phenophase-intensity-info.csv")

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

# Extract NPN species ID numbers and common names from phpint table
colnames(phpint) <- str_to_lower(colnames(phpint))
npn_spp <- phpint %>%
  distinct(species_id, common_name, genus, species) %>%
  mutate(scientific_name = paste(genus, species))

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
trees <- trees %>%
  mutate(individual_id = 10000000 + 1:nrow(trees)) %>%
  mutate(lat_lon_datum = "WGS84") 
ssi <- trees %>%
  select(individual_id, station_id, station_name, species_id, 
         individual_userstr, latitude, longitude, lat_lon_datum)

# Write to file (commented out for now to avoid overwriting accidentally)
# write.csv(ssi, "nn-import/station-species-individual.csv", row.names = FALSE)

# Status-intensity data import ------------------------------------------------#

# What accession numbers appear in dat that aren't in trees dataframe?
missing <- dat %>%
  filter(!tree %in% trees$accession) %>%
  mutate(location = ifelse(is.na(latitude), "none", "lat/lon")) %>%
  distinct(tree, scientific_name, location)
missing 
# 21 accession numbers (16 white pines, 5 with missing lat/lon)

# Replace species names/IDs in dat with info from trees list
colnames(dat) <- str_to_lower(colnames(dat))
dat <- dat %>%
  select(observerid, sectionid, tree, year, date, color, fallen, submission) %>%
  left_join(select(trees, accession, individual_userstr, individual_id, 
                   species_id, common_name),
            by = c("tree" = "accession"))

# Remove data for any trees that aren't in trees list (white pines, trees 
# without lat/lon)
dat <- dat %>%
  filter(!is.na(individual_userstr))

# Add in protocol ID
dat <- dat %>%
  left_join(distinct(phpint, species_id, protocol_id), by = "species_id")

# Delete any observations with negative fall or color values
dat <- dat %>%
  filter(color >= 0 & fallen >= 0)

# In MSU data:
# Color = percent of leaves that are colored
# Fallen = percent of canopy with fallen leaves

# For NPN, we want:
# Percent of canopy with leaves
# Percent of canopy with colored leaves

# Convert percents to proportions and calculate proportion of potential canopy 
# that has colored leaves
dat <- dat %>%
  rename(fall_percent = fallen,
         color_percent = color) %>%
  mutate(fall = fall_percent/100,
         color = color_percent/100,
         canopy = 1 - fall,
         color_canopy = color * canopy) %>%
  data.frame()

# Convert color columns to NA when canopy = 0
dat <- dat %>%
  mutate(color_percent = ifelse(canopy == 0, NA, color_percent),
         color = ifelse(canopy == 0, NA, color),
         color_canopy = ifelse(canopy == 0, NA, color_canopy))

# Add in NPN status columns
dat <- dat %>%
  mutate(status_leaves = ifelse(canopy > 0, 1, 0),
         status_coloredleaves = ifelse(color > 0, 1, 0)) %>%
  mutate(status_fallingleaves = case_when(
    canopy == 0 ~ NA,
    canopy < 1 ~ 1,
    canopy == 1 ~ 0)
  )

# Add in canopy fullness (intensity values for leaves phenophase)
dat <- dat %>%
  mutate(intensity_canopy = cut(canopy,
                                breaks = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1),
                                include.lowest = TRUE,
                                right = FALSE)) %>%
  mutate(intensity_canopy = addNA(intensity_canopy))
dat$intensity_canopy[dat$status_leaves == 0] <- NA

# Add in colored canopy (intensity values for colored leaves phenophase)
dat <- dat %>%
  mutate(intensity_coloredcanopy = cut(color_canopy,
                                       breaks = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1),
                                       include.lowest = TRUE,
                                       right = FALSE)) %>%
  mutate(intensity_coloredcanopy = addNA(intensity_coloredcanopy))
dat$intensity_coloredcanopy[dat$status_coloredleaves == 0] <- NA

# Keep just NPN-relevant columns
dat <- dat %>%
  select(observerid, date, individual_userstr, individual_id, species_id, 
         protocol_id, contains("status_"), contains("intensity_"), canopy,
         color_canopy)

#### First, put in observation_group_id?????

# Put in long form, with each phenophase observation in a separate row
leaves <- dat %>%
  select(-c(status_coloredleaves, status_fallingleaves, 
            intensity_coloredcanopy, color_canopy)) %>%
  mutate(pheno_class_id = 3,
         intensity_name = "Canopy fullness") %>%
  rename(phenophase_status = status_leaves,
         intensity_number = intensity_canopy,
         raw_abundance_value = canopy)
coloredleaves <- dat %>%
  select(-c(status_leaves, status_fallingleaves, 
            intensity_canopy, canopy)) %>%
  mutate(pheno_class_id= 4,
         intensity_name = "Canopy color") %>%
  rename(phenophase_status = status_coloredleaves,
         intensity_number = intensity_coloredcanopy,
         raw_abundance_value = color_canopy)
fallingleaves <- dat %>%
  select(-c(status_leaves, status_coloredleaves, 
            intensity_canopy, intensity_coloredcanopy, 
            canopy, color_canopy)) %>%
  mutate(pheno_class_id = 5,
         intensity_name = NA,
         intensity_number = NA,
         raw_abundance_value = NA) %>%
  rename(phenophase_status = status_fallingleaves)
datl <- bind_rows(leaves, coloredleaves, fallingleaves)

# Create text-based intensity value (called abundance_value for now)
datl <- datl %>%
  mutate(abundance_value = case_when(
    intensity_number == "[0,0.05)" ~ "Less than 5%",
    intensity_number == "[0.05,0.25)" ~ "5-24%",
    intensity_number == "[0.25,0.5)" ~ "25-49%",
    intensity_number == "[0.5,0.75)" ~ "50-74%",
    intensity_number == "[0.75,0.95)" ~ "75-94%",
    intensity_number == "[0.95,1]" ~ "95% or more",
    .default = NA
  )) %>%
  mutate(abundance_value = as.character(abundance_value))
# check:
# count(datl, intensity_number, abundance_value)

# Attach phenopohase_id abundance_category_id, abundance_value_id
phpint_subs <- phpint %>%
  distinct(species_id, pheno_class_id, phenophase_id, phenophase_description,
           abundance_category_id, abundance_name)

datl <- datl %>%
  left_join(phpint_subs, by = c("species_id", "pheno_class_id"))

# check:
# count(datl, species_id, phenophase_id, phenophase_description)

# Attach abundance_value_ids (when there is an abundance value)
datl <- datl %>%
  left_join(distinct(phpint, abundance_value, abundance_value_id),
            by = "abundance_value")

# Check data:
# count(datl, pheno_class_id, phenophase_description, phenophase_status, 
#       abundance_value_id, abundance_value)

# Clean up
observations <- datl %>%
  rename(observer_id = observerid,
         observation_date = date,
         abundance_category = abundance_category_id,
         abundance_category_value = abundance_value_id) %>%
  mutate(raw_abundance_value = raw_abundance_value * 100,
         comment = NA,
         observation_group_id = NA) %>% ##########
  select(observer_id, observation_date, phenophase_id, phenophase_status, 
         individual_userstr, individual_id, observation_group_id, protocol_id, 
         abundance_category, abundance_category_value, raw_abundance_value)

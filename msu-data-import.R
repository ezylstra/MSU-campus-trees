# Creating files for importing MSU data to NPN database
# 14 July 2026

library(dplyr)
library(stringr)
library(lubridate)
library(terra)
library(tidyterra)
library(tidyr)

# Load files ------------------------------------------------------------------#

# Tree list (remove any trees without lat/lon)
trees <- read.csv("data/MSUTreeList.csv")
trees <- trees %>%
  filter(!is.na(latitude))

# Load phenology data, 2017-2025
dat <- read.csv("data/msu-phenology-data-2017-2025.csv")
colnames(dat) <- str_to_lower(colnames(dat))

# Load sites and site names
sites <- vect("nn-sites/nn-sites-revised2.shp")
sitenames <- read.csv("nn-sites/nn-site-names-revised.csv")

# Load phenophase, intensity information for MSU tree species (created file
# in msu-spp-php-info.R)
phpint <- read.csv("data/msu-species-phenophase-intensity-info.csv")
colnames(phpint) <- str_to_lower(colnames(phpint))

# Station (site) table --------------------------------------------------------#

# Add sitenames to spatvector
sites <- sites %>%
  left_join(select(sitenames, -current_name), by = c("site_no" = "number")) %>%
  select(-name) %>%
  rename(sitename = new_name,
         msu_site_no = site_no)

# Write to file (commented out for now to avoid overwriting accidentally)
# writeVector(sites, "nn-import/msu-sites.shp", overwrite = TRUE)

# Get centroid locations
sites_centroid <- terra::centroids(sites) %>%
  as.data.frame(geom = "XY") %>%
  rename(lat = y,
         lon = x)

# Get site areas (in sq meters)
sites_centroid$area_m2 <- round(terra::expanse(sites, unit = "m"))

# Create station table (will use station_name for matching until station ID is
# created)
stations <- sites_centroid %>%
  # mutate(station_id = 100000 + 1:nrow(sites_centroid)) %>%
  mutate(lat_lon_datum = "WGS84",
         state = "MI",
         country = "USA",
         area_of_site_units_id = 313) %>%
  rename(latitude = lat,
         longitude = lon,
         station_name = sitename,
         area_of_site = area_m2) %>%
  select(station_name, latitude, longitude, lat_lon_datum,
         state, country, area_of_site, area_of_site_units_id)
  
# Write to file (commented out for now to avoid overwriting accidentally)
# write.csv(stations, "nn-import/station.csv", row.names = FALSE)

# Station-Species-Individual table --------------------------------------------#

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
  mutate(individual_userstr = paste0(common_name, "-", accession))

# If trees were monitored in 2025, list them as active (1).
active_trees <- dat %>%
  rename(accession = tree) %>%
  group_by(accession) %>%
  summarize(active = ifelse(2025 %in% year, 1, 0)) %>%
  data.frame()
trees <- trees %>%
  left_join(active_trees, by = "accession")

# Create station-species-individual table (will use plant nickname [individual_userstr]
# for matching until Individual IDs are created)
trees <- trees %>%
  mutate(lat_lon_datum = "WGS84") 
ssi <- trees %>%
  select(station_name, species_id, individual_userstr, latitude, longitude, 
         lat_lon_datum, active)

# Write to file (commented out for now to avoid overwriting accidentally)
# write.csv(ssi, "nn-import/station-species-individual.csv", row.names = FALSE)

# Person table ----------------------------------------------------------------#

# Extract unique observer IDs
anonobs <- sort(unique(dat$observerid))

# Will put unique 8-digit number identifying 2018-2025 observers into the 
# last_name field (this will be used for matching until Observer/Person IDs
# are created)
anon <- data.frame(first_name = NA,
                   middle_name = NA,
                   last_name = anonobs, 
                   email = NA,
                   active = 0,
                   comments = "MSU campus trees project student observer, pre 2026")

# Create fictional observer for 2017 (we don't have observer info this year)
obs17 <- data.frame(first_name = "MSU",
                    middle_name = NA,
                    last_name = "Student",
                    email = NA,
                    active = 0,
                    comments = "Fictional person for MSU campus trees project in 2017, when no observer listed")

# Combine and write to file (commented out for now to avoid overwriting accidentally)
persons <- rbind(obs17, anon)
# write.csv(persons, "nn-import/person.csv", row.names = FALSE)

# Observation group (site visit) table ----------------------------------------#

# Need to create this table, but no extra information will be conveyed here.
# Will just create a unique name for every student/tree/date combination

# First, want to remove any observations that we don't want to import. This 
# includes: 
  # 1) observations with negative fallen or color values or values that are
  #    between 0 and 1 (since it's unclear whether the student intended to
  #    report a very small value or erroneously reported value as a proportion 
  #    instead of a percent - see exploration in phenology-data-exploration.R)
  # 2) observations of white pine or trees that don't have lat/lons
  # Note: there are a few observations with non-integer color/fallen values that
  # are > 1, but these aren't necessarily problematic. Will leave as is.

dat <- dat %>%
  filter(color == 0 | color >= 1) %>%
  filter(fallen == 0 | fallen >= 1) %>%
  filter(tree %in% trees$accession)
  
# Then, need to remove any duplicate observations (same student, tree, day and
# same color/fallen values). We can remove the submission datetime column since
# this isn't useful for the NPN database. The date column indicates what day the 
# observations were made.
dat <- dat %>%
  select(-submission) %>%
  distinct()

# Are there times when a student made multiple observations on one tree on the
# same day? (Temporarily using "unk_#" for 2017 observations, where each row
# gets a different #, since we don't have Observer IDs for that year)
sttreeday <- dat %>%
  mutate(observerid = ifelse(is.na(observerid),
                             paste0("unk_", row_number()),
                             observerid)) %>%
  group_by(observerid, tree, date) %>%
  summarize(nobs = n(), .groups = "drop") %>%
  data.frame()
count(sttreeday, nobs) 
# Yes, so we'll need to include a number in the observation_group_ids that
# indicate which observation it was that day

# Creating unique observation_group_id
dat <- dat %>%
  mutate(observerid = ifelse(is.na(observerid),
                             paste0("unk_", row_number()),
                             observerid)) %>%
  group_by(observerid, tree, date) %>%
  mutate(obsnumber = 1:n()) %>%
  ungroup() %>%
  mutate(observation_group_name = paste0(str_remove_all(date, "-"), "_", tree, 
                                         "_", observerid, "_", obsnumber)) %>%
  data.frame()

# Attach site, tree information 
dat <- dat %>%
  left_join(select(trees, accession, individual_userstr,
                   species_id, station_name), 
            by = c("tree" = "accession")) %>%
  mutate(last_name = ifelse(str_detect(observerid, "unk"), 
                            "Student", observerid))

# Check that each observation has a site name
# count(dat, station_name)

# Create observation-group table
og <- dat %>%
  select(observation_group_name, date, last_name, station_name)

# Write to file (commented out for now to avoid overwriting accidentally)
# write.csv(og, "nn-import/observation-group.csv", row.names = FALSE)

# Observation table -----------------------------------------------------------#

# First remove unnecessary columns
dat <- dat %>%
  select(-c(observerid, sectionid, tree, scientific_name, year, common_name,
            latitude, longitude, obsnumber))

# Add in protocol ID
dat <- dat %>%
  left_join(distinct(phpint, species_id, protocol_id), by = "species_id")

# In MSU data:
# Color = percent of leaves that are colored
# Fallen = percent of canopy with fallen leaves

# For NPN intensity data, we want:
# Percent of canopy with leaves (leaves phenophase)
# Percent of canopy with colored leaves (colored leaves phenophase)

# Convert percents to proportions and calculate proportion of cannopy with
# leaves (canopy) and proportion of potential canopy that has colored leaves
# (color_canopy)
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

# Keep just NPN-relevant columns (or MSU data that we'll want to put in 
# comments field)
dat <- dat %>%
  select(observation_group_name, last_name, date, individual_userstr, 
         species_id, protocol_id, contains("status_"), 
         contains("intensity_"), canopy, color_canopy, color_percent, 
         fall_percent)

# Put in long form, with each phenophase observation in a separate row
leaves <- dat %>%
  mutate(pheno_class_id = 3,
         intensity_name = "Canopy fullness",
         comment = paste0("Percent fallen leaves: ", fall_percent)) %>%
  rename(phenophase_status = status_leaves,
         intensity_number = intensity_canopy,
         raw_abundance_value = canopy) %>%
  select(-c(status_coloredleaves, status_fallingleaves, 
            intensity_coloredcanopy, color_canopy, fall_percent, color_percent))
coloredleaves <- dat %>%
  mutate(pheno_class_id = 4,
         intensity_name = "Canopy color",
         comment = paste0("Percent fallen leaves: ", fall_percent, 
                          "; Percent of existing leaves that are colored: ",
                          color_percent)) %>%
  rename(phenophase_status = status_coloredleaves,
         intensity_number = intensity_coloredcanopy,
         raw_abundance_value = color_canopy) %>%
  select(-c(status_leaves, status_fallingleaves, 
            intensity_canopy, canopy, fall_percent, color_percent))
fallingleaves <- dat %>%
  mutate(pheno_class_id = 5,
         intensity_name = NA,
         intensity_number = NA,
         raw_abundance_value = NA,
         comment = paste0("Percent fallen leaves: ", fall_percent)) %>%
  rename(phenophase_status = status_fallingleaves) %>%
  select(-c(status_leaves, status_coloredleaves, 
            intensity_canopy, intensity_coloredcanopy, 
            canopy, color_canopy, fall_percent, color_percent))
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

# Attach phenophase_id abundance_category_id, abundance_value_id
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

# Checks:
count(datl, pheno_class_id, phenophase_description, phenophase_status,
      abundance_value_id, abundance_value)
count(datl, phenophase_description, phenophase_status)

# Remove rows for colored/falling leaves whern status = NA because there are
# no leaves left
datl <- datl %>%
  filter(!is.na(phenophase_status))

# Clean up
observations <- datl %>%
  rename(observation_date = date,
         abundance_category = abundance_category_id,
         abundance_category_value = abundance_value_id) %>%
  mutate(raw_abundance_value = raw_abundance_value * 100) %>%
  select(last_name, observation_date, phenophase_id, 
         phenophase_description, phenophase_status, individual_userstr, 
         observation_group_name, protocol_id, abundance_category, 
         abundance_category_value, raw_abundance_value, comment) %>%
  arrange(observation_group_name, phenophase_id)

# Write to file (commented out for now to avoid overwriting accidentally)
# write.csv(observations, "nn-import/observation.csv", row.names = FALSE)

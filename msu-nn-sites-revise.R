library(mapedit)
library(mapview)
library(sf)
library(terra)
library(dplyr)
library(maptiles)
library(ggplot2)
library(tidyterra)

# Load tree data --------------------------------------------------------------#
# Load tree list (and remove any trees without lat/lon)
trees <- read.csv("data/MSUTreeList.csv")
trees <- trees %>%
  mutate(common_name = factor(common_name)) %>%
  filter(!is.na(latitude))

# Load phenology data, 2017-2025
dat <- read.csv("data/msu-phenology-data-2017-2025.csv")

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

treesv <- vect(trees, geom = c("longitude", "latitude"), crs = "epsg:4326")

# Force mapedit to open in browser instead of the Viewer pane
options(viewer = NULL)

# Load original polygons shapefile --------------------------------------------#
sites_sf <- st_read("nn-sites/nn-sites-revised.shp") %>%
  st_transform(4326)

# Edit original polygons ------------------------------------------------------#
# Edit a few, hit Done, save immediately
batch1 <- editFeatures(sites_sf)
st_write(batch1, "nn-sites/nn-sites-batch1.shp")

# Load that saved file for the next batch
batch2 <- editFeatures(st_read("nn-sites/nn-sites-batch1.shp"))
st_write(batch2, "nn-sites/nn-sites-batch2.shp")

# Continue...
batch3 <- editFeatures(st_read("nn-sites/nn-sites-batch2.shp"))
st_write(batch3, "nn-sites/nn-sites-batch3.shp")

batch4 <- editFeatures(st_read("nn-sites/nn-sites-batch3.shp"))
st_write(batch4, "nn-sites/nn-sites-batch4.shp")

batch5 <- editFeatures(st_read("nn-sites/nn-sites-batch4.shp"))
st_write(batch5, "nn-sites/nn-sites-batch5.shp")

batch6 <- editFeatures(st_read("nn-sites/nn-sites-batch5.shp"))
st_write(batch6, "nn-sites/nn-sites-batch6.shp")

batch7 <- editFeatures(st_read("nn-sites/nn-sites-batch6.shp"))
st_write(batch7, "nn-sites/nn-sites-batch7.shp")

batch8 <- editFeatures(st_read("nn-sites/nn-sites-batch7.shp"))
st_write(batch8, "nn-sites/nn-sites-batch8.shp")

batch9 <- editFeatures(st_read("nn-sites/nn-sites-batch8.shp"))
st_write(batch9, "nn-sites/nn-sites-revised.shp")

sites <- vect("nn-sites/nn-sites-revised.shp") 
crs(sites) <- crs(treesv)
writeVector(sites, "nn-sites/nn-sites-revised2.shp")

# Remove batch files
batch_filelist <- list.files("nn-sites", pattern = "batch", full.names = TRUE)
file.remove(batch_filelist)

# Format and clean up ---------------------------------------------------------#

sites <- vect("nn-sites/nn-sites-revised2.shp")

# Number sites
sites_centroid <- terra::centroids(sites) %>%
  as.data.frame(geom = "XY") %>%
  arrange(desc(y)) %>%
  mutate(site_number = row_number())

# Add site number to SpatVector and remove other fields
sites <- sites %>%
  left_join(select(sites_centroid, name, site_number), by = "name")
sites[, "_lflt_d"] <- NULL
sites[, "layerId"] <- NULL

# Define area of interest (MSU campus approx bbox)
aoi <- st_bbox(c(xmin = -84.495, ymin = 42.72, xmax = -84.465, ymax = 42.7354),
               crs = st_crs(4326)) %>%
  st_as_sfc()

# Fetch street tiles (OpenStreetMap)
# tiles <- get_tiles(aoi, provider = "Esri.WorldStreetMap", zoom = 16)
tiles <- get_tiles(aoi, provider = "CartoDB.Positron", zoom = 16)

# Plot
ggplot() +
  geom_spatraster_rgb(data = tiles) +
  geom_spatvector(data = treesv,aes(color = common_name, shape = factor(y2025))) +
  geom_spatvector(data = sites, color = "black", fill = NA) +
  geom_text(data = sites_centroid, 
            aes(x = x, y = y, label = site_number, fontface = 2)) +
  scale_shape_manual(values = c(4, 19)) +
  labs(color = "Species", shape = "Obs in 2025") +
  theme_void()

# Check/edit site names
as.data.frame(sites) %>% arrange(site_no)

sites <- sites %>%
  mutate(name = case_when(
    site_number == 9 ~ "circle_auditorium_river_east",
    site_number == 8 ~ "east_of_physics",
    site_number == 10 ~ "farmlane_physics_auditorium_river",
    site_number == 11 ~ "breslin_stadium",
    site_number == 12 ~ "redcedar_shaw_farmlane_s_river",
    site_number == 14 ~ "farmlane_shaw_bogue_s_river",
    site_number == 15 ~ "redcedar_wilson_farmland_shaw",
    site_number == 17 ~ "east_neighborhood",
    .default = name
  ))

# Save sites file with names 
names(sites) <- c("name", "site_no")
writeVector(sites, "nn-sites/nn-sites-revised2.shp", overwrite = TRUE)


# Check tree assignments ------------------------------------------------------#
sites <- vect("nn-sites/nn-sites-revised.shp")

# Attach site to trees dataframe
trees_sites <- terra::extract(sites, treesv)
trees$site <- trees_sites$name
trees$site_no <- trees_sites$site_no

# Plot
ggplot() +
  geom_spatraster_rgb(data = tiles) +
  geom_spatvector(data = subset(sites, sites$site_no == 8),
                  color = "black", fill = "gray", alpha = 0.3) +
  geom_spatvector(data = treesv,
                  aes(color = common_name, shape = factor(y2025))) +
  geom_point(data = filter(trees, site_no == 8), 
             aes(x = longitude, y = latitude)) +
  geom_text(data = sites_centroid, 
            aes(x = x, y = y, label = site_number, fontface = 2)) +
  scale_shape_manual(values = c(4, 19)) +
  labs(color = "Species", shape = "Obs in 2025") +
  theme_void()

# Summarize trees at each site
sites_summary <- trees %>%
  group_by(site_no, site) %>%
  summarize(n_trees = n(),
            n_spp = n_distinct(common_name),
            n_trees_2025 = sum(y2025),
            n_spp_2025 = n_distinct(common_name[y2025 == 1]),
            .groups = "drop") %>%
  data.frame()
sites_summary


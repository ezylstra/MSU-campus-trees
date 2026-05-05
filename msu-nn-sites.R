library(dplyr)
library(maptiles)
library(sf)
library(terra)
library(ggplot2)
library(tidyterra)


# Set working directory (if working in a different R session)
# setwd("C:/Users/erin/Documents/NPN/MSU-campus-trees/")

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

# Create test set of Nature's Notebook sites ----------------------------------#

sites_file <- "nn-sites/nn-sites-18.shp"

# Define area of interest (MSU campus approx bbox)
aoi <- st_bbox(c(xmin = -84.495, ymin = 42.72, xmax = -84.465, ymax = 42.7354),
               crs = st_crs(4326)) %>%
  st_as_sfc()

# Fetch street tiles (OpenStreetMap)
# tiles <- get_tiles(aoi, provider = "Esri.WorldStreetMap", zoom = 16)
tiles <- get_tiles(aoi, provider = "CartoDB.Positron", zoom = 16)

if (!file.exists(sites_file)) {

  # Plot
  plot_tiles(tiles)
  points(treesv, col = "blue", pch = 19)

  # Draw polygons interactively (n = 18)
  sw_shaw_redcedar <- draw("polygon")
  n_kalamazoo <- draw("polygon")
  shaw_kalamazoo_w_stadium <- draw("polygon")
  s_michigan_w_abbot <- draw("polygon")
  shaw_redcedar_int <- draw("polygon")
  se_wilson_redcedar <- draw("polygon")
  shaw_wilson_e_farmlane <- draw("polygon")
  ne_shaw_bogue <- draw("polygon")
  shaw_to_river_e_farmlane <- draw("polygon")
  redcedar_farmlane_s_river <- draw("polygon")
  farmlane_bogue_s_grandriver <- draw("polygon")
  farmlane_bogue_n_river <- draw("polygon")
  circle_grandriver_e_abbot <- draw("polygon")
  circle_west <- draw("polygon")
  circle_east <- draw("polygon")
  circle_farmlane_auditorium <- draw("polygon")
  circle_river_west <- draw("polygon")
  circle_river_east <- draw("polygon")

  # Assign crs
  crs(sw_shaw_redcedar) <- crs(tiles)
  crs(n_kalamazoo) <- crs(tiles)
  crs(shaw_kalamazoo_w_stadium) <- crs(tiles)
  crs(s_michigan_w_abbot) <- crs(tiles)
  crs(shaw_redcedar_int) <- crs(tiles)
  crs(se_wilson_redcedar) <- crs(tiles)
  crs(shaw_wilson_e_farmlane) <- crs(tiles)
  crs(ne_shaw_bogue) <- crs(tiles)
  crs(shaw_to_river_e_farmlane) <- crs(tiles)
  crs(redcedar_farmlane_s_river) <- crs(tiles)
  crs(farmlane_bogue_s_grandriver) <- crs(tiles)
  crs(farmlane_bogue_n_river) <- crs(tiles)
  crs(circle_grandriver_e_abbot) <- crs(tiles)
  crs(circle_west) <- crs(tiles)
  crs(circle_east) <- crs(tiles)
  crs(circle_farmlane_auditorium) <- crs(tiles)
  crs(circle_river_west) <- crs(tiles)
  crs(circle_river_east) <- crs(tiles)
  
  # Find all spatvectors in environment
  spatvects <- ls()[!ls() %in% c("aoi", "dat", "tiles", "trees", "treesv")]
  
  # Add a name attribute to each polygon
  for (i in 1:length(spatvects)) {
    sv <- get(spatvects[i])
    sv$name <- spatvects[i]
    assign(spatvects[i], sv)
  }
  
  # Combine polygons into one spatvector
  site_list <- mget(spatvects)
  sites <- vect(site_list)

  # Save spatvector 
  # writeVector(sites, sites_file)
}

# Testing things out ----------------------------------------------------------#

sites <- vect(sites_file)

# Number sites
sites_centroid <- terra::centroids(sites) %>%
  as.data.frame(geom = "XY") %>%
  arrange(desc(y)) %>%
  mutate(site_number = row_number())

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

# Attach site to trees dataframe
trees_sites <- terra::extract(sites, treesv)
trees$site <- trees_sites$name
# Attach site number to trees dataframe
trees <- trees %>%
  left_join(select(sites_centroid, name, site_number),
            by = c("site" = "name"))


# Summarize trees at each site
sites_summary <- trees %>%
  group_by(site_number, site) %>%
  summarize(n_trees = n(),
            n_spp = n_distinct(common_name),
            n_trees_2025 = sum(y2025),
            n_spp_2025 = n_distinct(common_name[y2025 == 1]),
            .groups = "drop") %>%
  data.frame()




  









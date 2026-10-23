library(dplyr)
library(ggplot2)
library(ggspatial)
library(terra)
library(tidyterra)
library(stringr)
library(prettymapr)

sites <- vect("nn-import/msu-sites.shp")
site_centers <- terra::centroids(sites)

staticmap <- ggplot(sites) +
  annotation_map_tile(type = "osm", zoom = 15) +
  geom_spatvector(color = "blue", fill = NA, size = 0.5) +
  geom_spatvector_text(
    data = site_centers,
    aes(label = stringr::str_wrap(sitename, width = 20)),
    fontface = "bold", 
    size = 2,
    color = "blue") +
  scale_x_continuous(breaks = seq(-84.49, 84.47, by = 0.01)) +
  scale_y_continuous(breaks = seq(42.71, 42.73, by = 0.01)) +
  theme(axis.title = element_blank(),
        axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5))
# staticmap
ggsave("nn-import/site-map.png", staticmap, width = 6.5, height = 8, units = "in",
       dpi = 600)
  

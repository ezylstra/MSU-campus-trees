library(dplyr)
library(leaflet)
library(htmltools)

trees <- read.csv("data/MSUTreeList.csv")

trees <- trees %>%
  mutate(common_name = factor(common_name))

pal_leaf <- leaflet::colorFactor(palette = "turbo", 
                                 domain = unique(trees$common_name))

treespp <- sort(unique(trees$common_name))
cols <- pal_leaf(treespp)

leaflet(trees) %>%
  addProviderTiles(providers$OpenStreetMap,
                   options = providerTileOptions(opacity = 0.6)) %>% 
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    label = ~paste0(common_name, ": ", accession),
    fillColor = ~pal_leaf(common_name),
    fillOpacity = 1,
    color = "black",
    stroke = TRUE,
    weight = 2,
    radius = 5,
    popup = ~paste0(common_name, "<br>",
                    "ID: ", accession, "<br>")
  ) %>%
  addLegend(
    position = "bottomleft",
    pal = pal_leaf,
    values = ~ common_name,
    labels = ~common_name,
    title = "Tree Species"
  )

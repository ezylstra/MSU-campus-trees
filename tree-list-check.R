# Tree list check

library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)

# Load data -------------------------------------------------------------------#

# Phenology data
df <- read.csv("data/msu-phenology-data.csv")

# List of trees
trees <- read.csv("data/TreeList_All_updatedSpr26.csv")

# A little data formatting, clean up for phenology data -----------------------#

# Format date, create day-of-year variable (doy), and remove observations
# with negative values for fall or color (there are no missing values)
df <- df %>%
  mutate(date = ymd(date)) %>%
  mutate(doy = yday(date)) %>%
  filter(fall >= 0 & color >= 0)

# Delete observations that occurred before doy 239 or after doy 345
df <- df %>%
  filter(doy >= 239 & doy <= 345)

# Summarize info about each accession number in phenology dataset
ptrees <- df %>%
  group_by(tree, species) %>%
  summarize(nobs = n(),
            nyrs = n_distinct(year),
            yr_min = min(year),
            yr_max = max(year),
            .groups = "drop") %>%
  data.frame()
# Do any accession numbers appear twice?
ptrees %>%
  group_by(tree) %>%
  mutate(n = n()) %>%
  filter(n > 1)
# Yes, CC0190*01 appears as both F. grandifolia in 2018-2019 and F. sylvatica in 
# 2021-2025 in the phenology dataset. 
filter(trees, accession == "CC0190*01" | scientific_name == "Fagus sylvatica")
# Changing species for this tree in the phenology dataset
df <- df %>%
  mutate(species = ifelse(tree == "CC0190*01", "Fagus sylvatica", species))
# And recreating ptrees
ptrees <- df %>%
  group_by(tree, species) %>%
  summarize(nobs = n(),
            nyrs = n_distinct(year),
            yr_min = min(year),
            yr_max = max(year),
            .groups = "drop") %>%
  data.frame()

# Look at tree data -----------------------------------------------------------#

count(trees, scientific_name, common_name)
# At least name one inconsistency: Quercus alba / Red Oak
namecheck <- trees %>%
  filter(scientific_name == "Quercus alba" & common_name == "Red oak") %>%
  pull(accession)
ptrees %>%
  filter(tree == namecheck)
# This tree's common name should be White Oak. Changing now:
trees <- trees %>%
  mutate(common_name = ifelse(accession == namecheck, "White oak", common_name))

summary(trees) 
# lat/lon seem fine

# Does each accession number only appear once?
length(unique(trees$accession)) == nrow(trees)
# Yes

# Matching up accession numbers -----------------------------------------------#

# List of accession numbers in new tree list 
trees_a <- sort(unique(trees$accession))

# List of accession numbers in phenology dataset
df_a <- sort(unique(df$tree))

# Check for differences:
setdiff(trees_a, df_a) # All accession numbers in trees are in df
setdiff(df_a, trees_a) # 4 accession numbers in df that aren't in trees
  # 20100046-07
  # 20100048*06
  # 20100048*10
  # CC4955*04

# More info on missing trees (and trees with similar accession numbers)
df %>%
  group_by(tree, species) %>%
  summarize(nobs = n(),
            yr_min = min(year),
            yr_max = max(year)) %>%
  filter(str_detect(tree, "201000|CC4955")) %>%
  arrange(tree) %>%
  left_join(trees, by = c("tree" = "accession")) %>%
  data.frame()

# Looks like 20100046-07 should probably be 20100046*07 (same species, no
# overlap in years)
# All the others just missing? Other 3 trees only had observations in 2018-2019

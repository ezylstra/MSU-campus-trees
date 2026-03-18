# Prepping MSU data to incoporate in NN database
# 18 March 2026

library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)

# Test run with cut function
# x <- seq(0, 100, length = 1000)
# bins <- cut(x, 
#             breaks = c(0, 5, 25, 50, 75, 95, 100),
#             include.lowest = TRUE,
#             right = FALSE)
# Creates 6 bins: [0, 5), [5, 25), [25, 50), [50, 75), [75, 95), [95, 100]
# Can subseqently convert any values of 0 (when status = 0) to NA

# Create fake MSU dataset that encompasses range of values --------------------#

# Fall: % of leaf canopy that's fallen (0 = no fallen leaves; 100 = all fallen)
fall_percent <- c(0, 4:6, 24:26, 49:51, 74:76, 94:96, 99, 100)

# Color: % of leaves that are colored (0 = all leaves green; 100 = all colored)
# Were supposed to enter 0 if there are no leaves, but can ignore these
color_percent <- c(0, 4:6, 24:26, 49:51, 74:76, 94:96, 99, 100)

df <- expand_grid(fall_percent, color_percent) %>%
  mutate(fall = fall_percent/100,
         color = color_percent/100,
         canopy = 1 - fall,
         color_canopy = color * canopy) %>%
  data.frame()

# Convert color columns to NA when fall = 1, then remove duplicate rows
df <- df %>%
  mutate(color_percent = ifelse(fall == 1, NA, color_percent),
         color = ifelse(fall == 1, NA, color),
         color_canopy = ifelse(fall == 1, NA, color_canopy)) %>%
  distinct()

# Add in NN status columns
df <- df %>%
  mutate(nn_leaves = ifelse(canopy > 0, 1, 0),
         nn_coloredleaves = ifelse(color > 0, 1, 0))
# check
# count(df, fall_percent, nn_leaves)
# count(df, fall_percent == 100, color_percent, nn_coloredleaves)

# Add in NN canopy fullness (intensity values for leaf phenophase)
df <- df %>%
  mutate(nn_canopy = cut(canopy,
                         breaks = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1),
                         include.lowest = TRUE,
                         right = FALSE)) %>%
  mutate(nn_canopy = addNA(nn_canopy))
df$nn_canopy[df$nn_leaves == 0] <- NA

# Add in NN colored canopy (intensity values for colored leaves phenophase)
df <- df %>%
  mutate(nn_coloredcanopy = cut(color_canopy,
                                breaks = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1),
                                include.lowest = TRUE,
                                right = FALSE)) %>%
  mutate(nn_coloredcanopy = addNA(nn_coloredcanopy))
df$nn_coloredcanopy[df$nn_coloredleaves == 0] <- NA

# Converting MSU color/fall observations --------------------------------------#

# Load phenology data (2018-2025)
dat <- read.csv("https://raw.githubusercontent.com/ezylstra/MSU-campus-trees/refs/heads/main/data/msu-phenology-data.csv")
# dat <- read.csv("data/msu-phenology-data.csv")

# Convert percents to proportions and calculate % of potential canopy that has
# colored leaves
dat <- dat %>%
  rename(fall_percent = fall,
         color_percent = color) %>%
  mutate(fall = fall_percent/100,
         color = color_percent/100,
         canopy = 1 - fall,
         color_canopy = color * canopy) %>%
  data.frame()

# Convert color columns to NA when fall = 1
dat <- dat %>%
  mutate(color_percent = ifelse(fall == 1, NA, color_percent),
         color = ifelse(fall == 1, NA, color),
         color_canopy = ifelse(fall == 1, NA, color_canopy))

# Add in NN status columns
dat <- dat %>%
  mutate(nn_leaves = ifelse(canopy > 0, 1, 0),
         nn_coloredleaves = ifelse(color > 0, 1, 0))

# Add in NN canopy fullness (intensity values for leaf phenophase)
dat <- dat %>%
  mutate(nn_canopy = cut(canopy,
                         breaks = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1),
                         include.lowest = TRUE,
                         right = FALSE)) %>%
  mutate(nn_canopy = addNA(nn_canopy))
dat$nn_canopy[dat$nn_leaves == 0] <- NA

# Add in NN colored canopy (intensity values for colored leaves phenophase)
dat <- dat %>%
  mutate(nn_coloredcanopy = cut(color_canopy,
                                breaks = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1),
                                include.lowest = TRUE,
                                right = FALSE)) %>%
  mutate(nn_coloredcanopy = addNA(nn_coloredcanopy))
dat$nn_coloredcanopy[dat$nn_coloredleaves == 0] <- NA


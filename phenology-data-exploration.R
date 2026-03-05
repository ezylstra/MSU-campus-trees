# Create basic summaries of MSU Campus Trees phenology data

library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(ggplot2)

# If there are errors, make sure package is installed, eg:
# install.packages("tidyr")

# Load data -------------------------------------------------------------------#
# The code below will read the data from the GitHub website. What's commented 
# out is code to read data from a local file (in a data folder)

# Phenology data
df <- read.csv("https://raw.githubusercontent.com/ezylstra/MSU-campus-trees/refs/heads/main/data/msu-phenology-data.csv")
# df <- read.csv("data/msu-phenology-data.csv")

# List of tree species (*this will eventually be replaced with a list of
# individual trees and associated species info*)
tree_spp <- read.csv("https://raw.githubusercontent.com/ezylstra/MSU-campus-trees/refs/heads/main/data/tree-species.csv")
# tree_spp <- read.csv("data/tree-species.csv")

# Fixing a few section numbers ------------------------------------------------#

# Realized after the fact that some sections in 2024 were listed with slightly 
# different names in the original datafiles and thus were assigned different 
# section ID numbers. Will fix that here:

# Sections 2024-01, 2024-06, and 2024-11 are the same
# Sections 2024-03, 2024-07, and 2024-09 are the same
# Sections 2024-04, 2024-10, and 2024-12 are the same
# Sections 2024-05 and 2024-08 are the same

df <- df %>%
  mutate(section = case_when(
    section == "2024-06" ~ "2024-01",
    section == "2024-11" ~ "2024-01",
    section == "2024-07" ~ "2024-03",
    section == "2024-09" ~ "2024-03",
    section == "2024-10" ~ "2024-04",
    section == "2024-12" ~ "2024-04",
    section == "2024-08" ~ "2024-05",
    .default = section
  ))

# A little data formatting, clean up ------------------------------------------#

# Format date, create day-of-year variable (doy), and remove observations
# with negative values for fall or color (there are no missing values)
df <- df %>%
  mutate(date = ymd(date)) %>%
  mutate(doy = yday(date)) %>%
  filter(fall >= 0 & color >= 0)

# Look at structure and summary of data
str(df)
summary(df)

# Histogram of observation dates with blue vertical lines denoting proposed
# min/max dates
ggplot(df) +
  geom_histogram(aes(x = doy), binwidth = 1) +
  geom_vline(xintercept = c(239, 345), color = "blue") +
  labs(x = "Day of year", y = "Number of observations")

# Will delete observations that occurred before doy 239 (27 Aug) or
# after doy 345 (11 Dec)
df <- df %>%
  filter(doy >= 239 & doy <= 345)

# Add species codes, common names (and remove scientific name for now)
df <- df %>%
  left_join(tree_spp, by = "species") %>%
  select(-species)
  
# Remove comments column
df <- select(df, -comments)

# Remove any duplicate observations (same student, tree, date and same
# values for color and fall)
df <- df %>%
  distinct(date, student, tree, color, fall, .keep_all = TRUE)

# Number of observations by species, year -------------------------------------#

# By species, across years
ggplot(df, aes(x = spp)) +
  geom_bar() +
  labs(x = "Species", y = "Number of observations")

# By year, across species
ggplot(df, aes(x = year)) +
  geom_bar() +
  labs(x = "Year", y = "Number of observations")

# Species and year
ggplot(df, aes(x = year)) +
  geom_bar(aes(group = factor(year), fill = factor(year))) + 
  facet_wrap(~ common_name) +
  scale_fill_viridis_d(guide = "none") + 
  labs(x = "Year", y = "Number of observations")
ggplot(df, aes(x = spp)) +
  geom_bar() + 
  facet_wrap(~ year, ncol = 1) +
  labs(x = "Species", y = "Number of observations")

# Visualize phenology data ----------------------------------------------------#

# Seasonal patterns in leaf color by species (across years, trees, students)
ggplot(df) +
  geom_point(aes(x = doy, y = color)) +
  facet_wrap(~common_name) +
  labs(x = "Day of year", y = "Color estimate")
# Seasonal patterns in leaf fall by species (across years, trees, students)
ggplot(df) +
  geom_point(aes(x = doy, y = fall)) +
  facet_wrap(~common_name) +
  labs(x = "Day of year", y = "Fall estimate")

# What do yearly patterns look like for one species?

# Fall, in red maples 
# (Can change species or replace "fall" with "color" to look at other data)
ggplot(filter(df, common_name == "red maple")) +
  geom_point(aes(x = doy, y = fall)) +
  facet_wrap(~year) +
  labs(x = "Day of year", y = "Fall estimate")

# What do the data for individual trees look like for a given species, year? 

# Fall values in 2024, red maple trees
# (Can change species, year, or fall/color)
ggplot(filter(df, common_name == "red maple" & year == 2024),
       aes(x = doy, y = fall)) +
  geom_point() +
  facet_wrap(~tree) +
  labs(x = "Day of year", y = "Fall estimate")

# What do the data from each student look like for one tree?

# Fall values in 2024, red maple tree 20100046-07, by student
# (Can change species, year, tree, or fall/color)
ggplot(filter(df, common_name == "red maple" & year == 2024 & 
                tree == "20100046-07"),
       aes(x = doy, y = fall)) +
  geom_point() +
  facet_wrap(~student) +
  labs(x = "Day of year", y = "Fall estimate")

# Summarizing observer effort by year -----------------------------------------#

# Summarize effort for each tree each year
treeyr <- df %>%
  group_by(tree, common_name, spp, year) %>%
  summarize(n_observations = n(),
            n_dates = n_distinct(date),
            n_students = n_distinct(student),
            .groups = "drop") %>%
  data.frame() %>%
  mutate(obs_per_student = n_observations / n_students)
head(treeyr)

# How many students observed each tree in a given year?
summary(treeyr$n_students)
count(treeyr, n_students)

# How many observations did each student submit for a tree in a given year?
summary(treeyr$obs_per_student)

# How many days was each tree observed in a given year?
summary(treeyr$n_dates)
ggplot(treeyr, aes(x = n_dates)) + 
  geom_histogram(binwidth = 1) +
  labs(x = "Number of days a tree was observed in a year",
       y = "Number of tree-years")
# Looking into instances where a tree was observed < 5 times during the semester
treeyr %>%
  filter(n_dates < 5)
  
# Did the number of students or dates per tree vary among species?
treeyr %>%
  group_by(common_name) %>%
  summarize(n_treeyrs = n(),
            n_trees = n_distinct(tree),
            n_students_mean = round(mean(n_students), 1),
            n_students_median = median(n_students),
            n_dates_mean = round(mean(n_dates), 1),
            n_dates_median = median(n_dates)) %>%
  data.frame()

# Did the number of students or dates per tree vary among years?
treeyr %>%
  group_by(year) %>%
  summarize(n_trees = n(),
            n_species = n_distinct(common_name),
            n_students_mean = round(mean(n_students), 1),
            n_students_median = median(n_students),
            n_dates_mean = round(mean(n_dates), 1),
            n_dates_median = median(n_dates),
            .groups = "drop") %>%
  data.frame()

# Did students ever make >1 observation of the same tree on the same day? -----#

# Note that we already removed duplicates, so these are instances where the 
# student reported different values for leaf color and/or fall

# Summarize information for each student, tree, day
obs <- df %>%
  group_by(student, tree, common_name, year, date) %>%
  summarize(n_observations = n(),
            color_min = min(color),
            color_max = max(color),
            fall_min = min(fall),
            fall_max = max(fall),
            .groups = "drop") %>%
  data.frame() 

# How often/what proportion of the time did students submit > 1 observation?
count(obs, n_observations)
n_repeats <- sum(obs$n_observations > 1) # 1017 times
n_repeats/nrow(obs) # 1.8%

# Isolate instances when students submitted multiple observations of same tree
# on same day
repeatobs <- obs %>%
  filter(n_observations > 1) %>%
  mutate(color_diff = color_max - color_min,
         fall_diff = fall_max - fall_min)
head(repeatobs)

# How many times did a student report a different color value, a different fall
# value, or different values for both?
count(repeatobs, color_diff > 0, fall_diff > 0) 
  # Most of the time, both color and fall values are different

# Did this occur more/less often in some years?
count(repeatobs, year)
  # Many more instances of repeat observations in 2018-2019 than 2021-2025

# When color/fall values are different, how different are they?
repeatobs %>%
  pivot_longer(cols = color_diff:fall_diff,
               names_to = "variable",
               values_to = "diff") %>%
  mutate(var = ifelse(variable == "color_diff", 
                      "Difference in color estimates",
                      "Difference in fall estimates")) %>%
  filter(diff > 0) %>%
  ggplot() +
  geom_histogram(aes(x = diff), binwidth = 2) +
  facet_wrap(~var, ncol = 1) +
  labs(x = "Difference", y = "Count")

# Might want to investigate these a little more. Color/fall values can be very
# different, suggesting some kind of data entry issue. For now, we'll ignore 
# them, but it might be better to delete observations when things are in
# question.

# Summarizing observer effort by day ------------------------------------------#

# Summarize effort for each tree each day
treeday <- df %>%
  group_by(tree, common_name, spp, year, date, doy) %>%
  summarize(n_observations = n(),
            # Identify unique number of student observers
            n_students = n_distinct(student), 
            .groups = "drop") %>%
  data.frame() %>%
  # Create indicator to denote when a tree was observed by >1 student in a day
  mutate(mult_students = ifelse(n_students > 1, 1, 0))
head(treeday)

# How many times did 1, 2, 3, etc students observe one tree on the same day?
count(treeday, n_students) 

# How often were trees observed on the same day by multiple students?
sum(treeday$mult_students)/nrow(treeday)
# Overall, 29% of tree-days have multiple observers

# Did the frequency of multiple observer days vary among species?
treeday %>%
  group_by(common_name) %>%
  summarize(prop_mult_obs = round(sum(mult_students)/n(), 2)) %>%
  data.frame()
  # Ranges from 22-35%

# Did the frequency of multiple observer days vary among years?
treeday %>%
  group_by(year) %>%
  summarize(prop_mult_obs = round(sum(mult_students)/n(), 2)) %>%
  data.frame()
  # Lower rate in 2021

# Append information about the number of observations and number of students
# that made observations each tree-day to the main dataframe
df <- df %>%
  group_by(tree, common_name, spp, year, date) %>%
  mutate(n_observations = n(),
         n_students = n_distinct(student)) %>%
  ungroup() %>%
  data.frame()

# How different were observations made on the same day? 
sameday <- df %>%
  filter(n_students > 1) %>%
  group_by(tree, common_name, spp, year, date) %>%
  summarize(n_observations = n(),
            n_students = n_distinct(student),
            color_min = min(color),
            color_mean = mean(color),
            color_max = max(color),
            fall_min = min(fall),
            fall_mean = mean(fall),
            fall_max = max(fall),
            .groups = "drop") %>%
  data.frame() %>%
  mutate(color_range = color_max - color_min, .after = "color_max") %>%
  mutate(fall_range = fall_max - fall_min)
  # Note: Not calculating SDs right now given how few observations are made on
  # the same day (median = 2, mean = 2.3)

# Does consistency vary by species?
sameday %>%
  group_by(common_name) %>%
  summarize(n_treedays = n(),
            n_observations_mean = round(mean(n_observations), 2),
            color_range_mean = round(mean(color_range), 2),
            fall_range_mean = round(mean(fall_range), 2)) %>%
  data.frame()

# Does consistency vary by year?
sameday %>%
  group_by(year) %>%
  summarize(n_treedays = n(),
            n_observations_mean = round(mean(n_observations), 2),
            color_range_mean = round(mean(color_range), 2),
            fall_range_mean = round(mean(fall_range), 2)) %>%
  data.frame()

# Next step: evaluate consistency of observations made in the same 3-day or 
# 7-day period.

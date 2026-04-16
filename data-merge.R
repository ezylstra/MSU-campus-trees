# Merging MSU Campus Trees data, 2017-2025
# 16 Apr 2026
# E Zylstra

library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)

# Load tree list --------------------------------------------------------------#
trees <- read.csv("data/MSUTreeList.csv") 

# 2017-2019 data --------------------------------------------------------------#

# Load csvs with 2017-2019 data
for (yy in 2017:2019) {
  # Identify folder
  filename <- paste0("data/original/anonymized-data-", yy, ".csv")
  # Load csv
  assign(paste0("df", str_sub(yy, 3, 4)), read.csv(filename))
}

# Column names/types years match, so we can combine them
df1719 <- rbind(df17, df18, df19)

# Only retain columns we need for analyses and rename 
df1719 <- df1719 %>%
  select(ObserverID, SectionID, tree, percent_color, percent_fallen, 
         submission_time) %>%
  rename(color = percent_color,
         fallen = percent_fallen,
         datetime = submission_time) 

# Extract date from datetime and create year column
# Retaining submission time (for additional info on replicate observations)
df1719 <- df1719 %>%
  mutate(date = ymd(str_sub(datetime, 1, 10))) %>%
  mutate(year = year(date)) %>%
  mutate(submission = str_replace_all(datetime, "T", " ")) %>%
  mutate(submission = parse_date_time(submission, orders = "YmdhMS")) %>%
  select(-datetime)

# Merge tree info with phenology data
df1719 <- df1719 %>%
  left_join(trees, by = c("tree" = "accession"))

# 2021-2023 data --------------------------------------------------------------#

# Load csvs with 2021-2023 data
for (yy in 2021:2023) {
  # Identify folder
  filename <- paste0("data/original/anonymized-data-", yy, ".csv")
  # Load csv
  assign(paste0("df", str_sub(yy, 3, 4)), read.csv(filename))
}

# Column names/types years match, so we can combine them
df2123 <- rbind(df21, df22, df23)

# start_time and completion_time columns can be used to identify when students'
# observations were uploaded to the database. They're almost always very 
# similar. Will retain information from the completion_time column to note when
# a student submitted data.
df2123 <- df2123 %>%
  mutate(submission = parse_date_time(completion_time, orders = "YmdhMS")) %>%
  select(-c(start_time, completion_time))

# Create date and year column from photo_date
df2123 <- df2123 %>%
  mutate(date = ymd(photo_date)) %>%
  mutate(year = year(date)) %>%
  select(-photo_date)

# Right now, accession numbers (ie, tree IDs) appear in multiple columns.
# Will combine them into one column (including checks to make sure that there
# was always an accession number in the correct column based on tree species)
df2123 <- df2123 %>%
  pivot_longer(cols = contains("accession"),
               names_to = "species_check",
               values_to = "tree",
               values_drop_na = TRUE) %>%
  mutate(species_check = str_remove(species_check, "accession_")) %>%
  mutate(species_check = str_replace_all(species_check, "_", " ")) %>%
  data.frame()
# Check that the accession number species always matched the name in the
# species column
count(df2123, species, species_check)
# Check that those species match with what's in tree list
df2123 <- df2123 %>%
  left_join(trees, by = c("tree" = "accession"))
count(df2123, species, species_check, scientific_name)
# Everything matches, so we can delete the extra columns
df2123 <- df2123 %>%
  select(-c(species, species_check))

# Rename a couple columns 
df2123 <- df2123 %>%
  rename(color = percent_color,
         fallen = percent_fallen) 

# 2024-2025 data --------------------------------------------------------------#

# Load csvs with 2024-2025 data
for (yy in 2024:2025) {
  # Identify folder
  filename <- paste0("data/original/anonymized-data-", yy, ".csv")
  # Load csv
  assign(paste0("df", str_sub(yy, 3, 4)), read.csv(filename))
}

# Column names/types years match, so we can combine them
df2425 <- rbind(df24, df25)

# There is one red maple accession number in 2024-2025 that was listed 
# incorrectly in the original datasheets. Listed as 20100046-07 when it should 
# have been 20100046*07. Changing here.
df2425 <- df2425 %>%
  mutate(accession_Acer_rubrum = ifelse(accession_Acer_rubrum == "20100046-07",
                                        "20100046*07", accession_Acer_rubrum))

# start_time and completion_time columns can be used to identify when students'
# observations were uploaded to the database. They're almost always very 
# similar. Will retain information from the completion_time column to note when
# a student submitted data.
df2425 <- df2425 %>%
  mutate(submission = parse_date_time(completion_time, orders = "YmdhMS")) %>%
  select(-c(start_time, completion_time))

# Create date and year column from photo_date
df2425 <- df2425 %>%
  mutate(date = ymd(photo_date)) %>%
  mutate(year = year(date)) %>%
  select(-photo_date)

# Right now, accession numbers (ie, tree IDs) appear in multiple columns.
# Will combine them into one column (including checks to make sure that there
# was always an accession number in the correct column based on tree species)
df2425 <- df2425 %>%
  pivot_longer(cols = contains("accession"),
               names_to = "species_check",
               values_to = "tree",
               values_drop_na = TRUE) %>%
  mutate(species_check = str_remove(species_check, "accession_")) %>%
  mutate(species_check = str_replace_all(species_check, "_", " ")) %>%
  data.frame()
# Check that the accession number species always matched the name in the
# species column
count(df2425, species, species_check)
# Check that those species match with what's in tree list
df2425 <- df2425 %>%
  left_join(trees, by = c("tree" = "accession"))
count(df2425, species, species_check, scientific_name)
# Everything matches, so we can delete the extra columns
df2425 <- df2425 %>%
  select(-c(species, species_check))

# Rename a couple columns 
df2425 <- df2425 %>%
  rename(color = percent_color,
         fallen = percent_fallen) 

# Merge data from all years and clean up --------------------------------------#

dat <- bind_rows(df1719, df2123, df2425)

# Look for any problematic years/dates
filter(dat, !year %in% 2017:2025) 
# Three dates are incorrect (in 2004 or 2005). Will delete these observations
dat <- dat %>%
  filter(year %in% 2017:2025)

# Check that all section numbers match up with year
dat %>%
  filter(year != str_sub(SectionID, 1, 4))
# Couple instances where there's a mismatch between Section year, photo date, 
# and/or submission year. Will delete
dat <- dat %>%
  filter(year == str_sub(SectionID, 1, 4))

# Remove duplicate rows (information in all columns is the same)
dat <- distinct(dat, .keep_all = TRUE)

# Put columns in a logical order
dat <- dat %>%
  select(ObserverID, SectionID, tree, scientific_name, year, date, color, 
         fallen, common_name, latitude, longitude, submission)

# Write to file ---------------------------------------------------------------#
# Commenting out until needed so the file is not accidentally overwritten

# write.csv(dat, "data/msu-phenology-data-2017-2025.csv", row.names = FALSE)

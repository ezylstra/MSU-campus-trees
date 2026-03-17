# Merging MSU Campus Trees data, 2018-2025
# 17 Mar 2026
# E Zylstra

library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)

# 2018-2019 data --------------------------------------------------------------#

# Load csvs with 2018-2019 data
for (yy in 2018:2019) {
  # Identify folder
  folder <- paste0("data/original/data-", yy)
  
  # Identify filepath
  file <- list.files(path = folder, pattern = ".csv", full.names = TRUE)
  
  # Load csv
  assign(paste0("df", str_sub(yy, 3, 4)), read.csv(file))
}

# Column names/types years match, so we can combine them
df_early <- rbind(df18, df19)

# Only retain columns we need for analyses and rename 
# (there were no entries in the notes column, so didn't keep)
df_early <- df_early %>%
  select(section, group_num, student_id, X_tree_gps_latitude, 
         X_tree_gps_longitude, species, individual, color, fall, 
         X_submission_time) %>%
  rename(section_id = section, 
         lat = X_tree_gps_latitude,
         lon = X_tree_gps_longitude, 
         datetime = X_submission_time) 

# Extract date from datetime and create year column
# Retaining submission time (for additional info on replicate observations)
df_early <- df_early %>%
  mutate(date = ymd(str_sub(datetime, 1, 10))) %>%
  mutate(year = year(date)) %>%
  mutate(submission = str_replace_all(datetime, "T", " ")) %>%
  mutate(submission = parse_date_time(submission, orders = "YmdhMS")) %>%
  select(-datetime)

# Look at group numbers
count(df_early, group_num) # numeric group IDs 1:51; 2 entries with neg values
filter(df_early, group_num < 0) 

# Delete entries with negative group numbers and change to character, width 2
df_early <- df_early %>%
  filter(group_num > 0) %>%
  mutate(group_num = str_pad(group_num, width = 2, pad = "0"))

# Look at student labels
count(df_early, student_id) # A:E (but many fewer E's than A:D)

# Create unique group and student IDs
  # group = section_yr_groupnum
  # student = section_yr_groupnum_studentid
df_early <- df_early %>%
  mutate(section = paste0(section_id, "_", year)) %>%
  mutate(group = paste0(section, "_", group_num)) %>%
  mutate(student = paste0(group, "_", student_id))

# Look at number of students and observations per section/group
df_early %>% 
  group_by(section, group) %>%
  summarize(n_students = n_distinct(student),
            n_obs = n(),
            .groups = "keep") %>%
  data.frame() %>%
  summary()
# 202 groups over the 2 years, average 4 students, 57 observations

# What species, trees were observed?
df_early %>% 
  group_by(species) %>%
  summarize(n_trees = n_distinct(individual),
            n_obs = n()) %>%
  data.frame()
# 10 species (same as species in 2021, except not Ostrya virginiana [OSVI])
# 15-16 trees/spp
# 1000-1250 observations/spp

# Clean up and remove columns to match up better with 2021-2025 data
df_early <- df_early %>%
  select(year, date, section, group, student, 
         species, individual, color, fall, submission) %>%
  mutate(species = case_when(
    species == "ACRU" ~ "Acer rubrum",
    species == "ACSA" ~ "Acer saccharum",
    species == "COFL" ~ "Cornus florida",
    species == "FAGR" ~ "Fagus grandifolia",
    species == "MEGL" ~ "Metasequoia glyptostroboides",
    species == "PIST" ~ "Pinus strobus",
    species == "PLOC" ~ "Platanus occidentalis",
    species == "QUAL" ~ "Quercus alba",
    species == "QURU" ~ "Quercus rubra",
    species == "ULMA" ~ "Ulmus americana"
  )) %>%
  rename(tree = individual)
 
# 2021-2025 data --------------------------------------------------------------#

# Load csvs with 2021-2025 (folder for each year; separate csvs for each week)
for (yy in 2021:2025) {
  # Identify folder
  folder <- paste0("data/original/anonymized-", yy)
  
  # Identify filepaths
  files <- list.files(path = folder, pattern = ".csv", full.names = TRUE)
  
  # Load weekly data into a list
  dat_list <- lapply(files, read.csv, na.strings = c("NA", ""))
  
  # Bind data from each csv into a single dataframe for each year (eg, df21)
  assign(paste0("df", str_sub(yy, 3, 4)), bind_rows(dat_list))

}

# Datafiles from 2021-2023 have the same dimensions and column names, so can 
# combine them
df2123 <- rbind(df21, df22, df23)

# Start_time and Completion_time columns can be used to identify when students'
# observations were uploaded to the database. They're almost always very 
# similar. Will retain information from the Completion_time column to note when
# a student submitted data.
df2123 <- df2123 %>%
  mutate(submission = parse_date_time(Completion_time, orders = "YmdhMS")) %>%
  select(-c(Start_time, Completion_time))

# Right now, accession numbers (ie, tree IDs) appear in multiple columns.
# Will combine them into one column (including checks to make sure that there
# was always an accession number in the correct column based on tree species)
df2123 <- df2123 %>%
  pivot_longer(cols = contains("Accession"),
               names_to = "species_check",
               values_to = "tree",
               values_drop_na = TRUE) %>%
  mutate(species_check = str_remove(species_check, "Accession_")) %>%
  mutate(species_check = str_replace_all(species_check, "_", " ")) %>%
  data.frame()
# Check that the accession number species always matched the name in the
# Tree_species column
count(df2123, Tree_species, species_check)
# Yes, so we can delete the species_check column
df2123 <- df2123 %>%
  select(-species_check)

# Data from 2024-2025 have the same dimensions and column names, so can combine
# them
df2425 <- rbind(df24, df25)

# Create submission column and then remove original columns used to identify  
# when students' observations were uploaded to database
df2425 <- df2425 %>%
  mutate(submission = parse_date_time(Completion_time, orders = "YmdhMS")) %>%
  select(-c(Last_modified_time, Start_time, Completion_time))

# Combine accession numbers into one column (including species checks)
df2425 <- df2425 %>%
  pivot_longer(cols = contains("Accession"),
               names_to = "species_check",
               values_to = "tree",
               values_drop_na = TRUE) %>%
  mutate(species_check = str_remove(species_check, "Accession_")) %>%
  mutate(species_check = str_replace_all(species_check, "_", " ")) %>%
  data.frame()
# Check that the accession number species always matched the name in the
# Tree_species column
count(df2425, Tree_species, species_check)
# Yes, so we can delete the species_check column
df2425 <- df2425 %>%
  select(-species_check)

# Remove common name from Tree_species column
df2425 <- df2425 %>%
  mutate(Tree_species = str_split_i(Tree_species, " \\(", 1))

# Merge 2021-2023 and 2024-2025 data. Clean up columns to better match up
# with earlier data
df2125 <- rbind(df2123, df2425) %>%
  mutate(Photo_date = ymd(Photo_date)) %>%
  mutate(year = year(Photo_date)) %>%
  select(year, Photo_date, SectionID, StudentID, 
         Tree_species, tree, Percent_color, Percent_fallen, submission,
         Comments) %>%
  rename(section = SectionID,
         date = Photo_date,
         student = StudentID,
         species = Tree_species,
         color = Percent_color,
         fall = Percent_fallen,
         comments = Comments)

# Look for any problematic years/dates
filter(df2125, !year %in% 2021:2025) 
# Three dates are incorrect (in 2004 or 2005). Will delete these observations
df2125 <- df2125 %>%
  filter(year %in% 2021:2025)

# Fixing a few section numbers
# Realized after the fact that some sections in 2024 were listed with slightly 
# different names in the original datafiles and thus were assigned different 
# section ID numbers. Will fix that here:

# Sections 2024-01, 2024-06, and 2024-11 are the same
# Sections 2024-03, 2024-07, and 2024-09 are the same
# Sections 2024-04, 2024-10, and 2024-12 are the same
# Sections 2024-05 and 2024-08 are the same

df2125 <- df2125 %>%
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

# Check that all section numbers match up with year
df2125 %>%
  filter(year != str_sub(section, 1, 4))
# One instance where photo date is listed in 2022, but section and submission
# date listed as 2022. Will delete this observation
df2125 <- df2125 %>%
  filter(year == str_sub(section, 1, 4))

# Combine data from all years -------------------------------------------------#

# Before combining, need to add a group column to 2021-2025 data and a comments
# column to the earlier data
df2125 <- df2125 %>%
  mutate(group = NA, .before = "student")
df_early <- df_early %>%
  mutate(comments = NA)

# Combine
df <- rbind(df_early, df2125)

# Remove duplicate rows (information in all columns is the same)
df <- distinct(df, .keep_all = TRUE)

# Check accession numbers and species -----------------------------------------#

# Clean up Fagus tree species
# Tree #CC0190*01 was later determined to be Fagus sylvatica
# All other Fagus spp trees should be Fagus grandifolia
df <- df %>%
  mutate(species = case_when(
    tree == "CC0190*01" ~ "Fagus sylvatica",
    grepl("Fagus", species) ~ "Fagus grandifolia",
    .default = species
  ))

# Load tree list
treelist <- read.csv("data/MSUTreeList.csv")

# Does every tree in treelist appear in phenology dataset?
setdiff(treelist$accession, unique(df$tree)) # yes
# Does every tree in phenology dataset appear in treelist?
setdiff(unique(df$tree), treelist$accession) 
  # no. Looks like one accession has a dash instead of an asterisk

filter(df, grepl("20100046", tree)) %>%
  count(tree, species, year)
# Accession number with asterisk observed in 2022-2023, and with dash observed
# in 2024-2025. Will swap out dash with asterisk
df <- df %>%
  mutate(tree = ifelse(tree == "20100046-07", "20100046*07", tree))

# Last tree number/species check:
dftrees <- df %>%
  distinct(tree, species) %>%
  arrange(tree)
tltrees <- treelist %>%
  distinct(accession, scientific_name) %>%
  rename(tree = accession,
         species = scientific_name) %>%
  arrange(tree)
# Same list of tree accession numbers and assigned species in phenology dataset
# and treelist?
all.equal(dftrees, tltrees) # yes

# Write to file (commenting out until needed so the file is not accidentally
# overwritten)
# write.csv(df, "data/msu-phenology-data.csv", row.names = FALSE)

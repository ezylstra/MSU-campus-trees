# Creating protocol/phenophase information to import MSU data to NPN database
# 24 June 2026

library(DBI)
library(odbc)
library(dplyr)
library(dbplyr)
library(tidyr)

# Connect to database
con <- DBI::dbConnect(odbc::odbc(),
                      Driver   = "MySQL ODBC 9.4 ANSI Driver",
                      Server   = "usanpn-databases-prod.c0xzlo6s7duc.us-west-2.rds.amazonaws.com",
                      UID      = rstudioapi::askForPassword("Database user"),
                      PWD      = rstudioapi::askForPassword("Database password"),
                      database = "usanpn2",
                      Port     = 3306)

# Load list of species
trees <- read.csv("data/MSUTreeList.csv")

# Get unique species list and fix species for Kentucky coffeetree (spelled
# wrong in MSU datafiles)
trees <- trees %>%
  distinct(scientific_name) %>%
  mutate(scientific_name = ifelse(scientific_name == "Gymnocladus dioicius",
                                  "Gymnocladus dioicus", 
                                  scientific_name))
  
# Get species table from DB
species <- tbl(con, "Species") %>%
  mutate(scientific_name = paste(Genus, Species)) %>%
  filter(scientific_name %in% trees$scientific_name) %>%
  select(Species_ID, Common_Name, Genus, Species, scientific_name) %>%
  arrange(scientific_name) %>%
  collect()

# Get protocol ID for each species 
protocols <- tbl(con, "Species_Protocol") %>%
  filter(Species_ID %in% species$Species_ID) %>%
  filter(Active == 1) %>%
  filter(is.na(Dataset_ID)) %>%
  collect()
protocols
# Most use 233 or 234, which were started in 2012 EXCEPT:
# eastern white pine (53) uses 437, which started in Mar 2026
# dawn redwood (1356) uses 438, which started in Mar 2026

# Add protocol ID to species table
species <- species %>%
  left_join(select(protocols, Species_ID, Protocol_ID), by = "Species_ID") %>%
  data.frame()

# Get phenophases for each Protocol
pphps <- tbl(con, "Protocol_Phenophase") %>%
  filter(Protocol_ID %in% species$Protocol_ID) %>%
  filter(Active == 1) %>%
  collect() %>%
  data.frame() %>%
  select(Protocol_ID, Phenophase_ID)
php_ids <- unique(pphps$Phenophase_ID)

# Get phenophase information (description, class ID) and attach to pphps
phps <- tbl(con, "Phenophase") %>%
  filter(Phenophase_ID %in% php_ids) %>%
  collect() %>%
  data.frame() %>%
  select(Phenophase_ID, Description, Pheno_Class_ID)
pphps <- pphps %>%
  left_join(phps, by = "Phenophase_ID") %>%
  arrange(Protocol_ID, Pheno_Class_ID)
# Just keeping info for phenophases in Classes 3:5
pphps <- pphps %>%
  filter(Pheno_Class_ID %in% 3:5)

# Will use SSPI table to get abundance/intensity category for each phenophase
php_abunc <- tbl(con, "Species_Specific_Phenophase_Information") %>%
  filter(Phenophase_ID %in% pphps$Phenophase_ID) %>%
  filter(Active == 1) %>%
  distinct(Phenophase_ID, Abundance_Category) %>%
  data.frame()
# Attach name/description of Abundance Category
abund <- tbl(con, "Abundance_Category") %>%
  rename(Abundance_Category = Abundance_Category_ID,
         Abundance_Name = Name) %>%
  select(-Description) %>%
  data.frame()
php_abunc <- php_abunc %>%
  left_join(abund, by = "Abundance_Category")
# Create rows for each Abundance Value (in each Category)
abuncv <- tbl(con, "Abundance_Category_Abundance_Values") %>%
  select(-Seq_Num) %>%
  data.frame()
abunv <- tbl(con, "Abundance_Values") %>%
  select(-Abundance_Value) %>%
  data.frame()
abunv <- left_join(abuncv, abunv,
                   by = "Abundance_Value_ID") %>%
  rename(Abundance_Category = Abundance_Category_ID,
         Abundance_Value = Short_Name)
php_abund <- php_abunc %>%
  left_join(abunv, by = "Abundance_Category", relationship = "many-to-many") %>%
  data.frame()

# Attach abundance info to protocol-phenophase (pphps) table
pphps <- pphps %>%
  left_join(php_abund, by = "Phenophase_ID", relationship = "many-to-many")

# Attach everything to species table
species_php <- species %>%
  left_join(pphps, by = "Protocol_ID", relationship = "many-to-many") %>%
  arrange(Species_ID, Pheno_Class_ID)

# Note: there are no needles/colored needles/falling needles phenophases for
# eastern white pine (species ID = 53, protocol ID = 437)

species_php <- species_php %>%
  select(-scientific_name) %>%
  rename(Phenophase_Description = Description,
         Abundance_Category_ID = Abundance_Category)

# Write to file
# write.csv(species_php,
#           "data/msu-species-phenophase-intensity-info.csv",
#           row.names = FALSE)

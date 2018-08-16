# Import California Department of Education (CDE) English Leaners (EL) data files
# 
# Date files and file specification available here: https://www.cde.ca.gov/ds/sd/sd/fileselsch.asp
# 
# This script imports EL data files from individual school years and combines them into a single data frame

# Install/load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, tidyr, dplyr)

# Set working directory
setwd("C:/Github/SimilarSchoolComparison/Data/EnglishLearners")

# Create list to store data frames
l_dataframes <- list()

# List of files in the working folder
l_files <- dir()

# Import each file into a data frame
for (i in 1:length(l_files)) {
  
  l_dataframes[[i]] <- l_files[i] %>%
    
    # Import file
    read.csv(header = TRUE
             , sep = "\t"
             , colClasses = c(rep("character", 6)
                              , rep("integer", 15))) %>%
    
    # Parse school year from file name
    mutate(FILE_NAME = l_files[i])
}

# Create single data frame
df_el <- bind_rows(l_dataframes)

# Clean up
rm(i, l_dataframes, l_files)
gc()

# Add variables
df_el <- df_el %>%
  
            # Extract year from file name
            mutate(SCHOOL_YEAR = substring(FILE_NAME
                                           , 6
                                           , 7)) %>%
            
            # Translate school year
            mutate(SCHOOL_YEAR = case_when(
              SCHOOL_YEAR == "18" ~ "17-18"
              , SCHOOL_YEAR == "17" ~ "16-17"
              , SCHOOL_YEAR == "16" ~ "15-16"
            )) %>%
            
            # Drop file name variable
            dplyr::select(-FILE_NAME) %>%

            # Rename CDS code variable
            rename(CDS_CODE = CDS)
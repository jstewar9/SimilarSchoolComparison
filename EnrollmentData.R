# Import CDE enrollment data files from 2007-2008 school year and beyond into a data frame

# # Clear console
# cat("\014") 
# 
# # Clear memory
# rm(list=ls())
# gc()

# Install/load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, tidyr, dplyr)

# Set working directory
setwd("C:/Data/State Data Files/Enrollment")

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
                                                    , rep("integer", 17))) %>%
                          
                          # Parse school year from file name
                          mutate(FILE_NAME = l_files[i])
}

# Create single data frame
df_enr <- bind_rows(l_dataframes)

# Clean up
rm(i, l_dataframes, l_files)
gc()

# Add variables
df_enr <- df_enr %>%
            
            # Add school year column
            mutate(SCHOOL_YEAR = substring(FILE_NAME
                                           , 5
                                           , 8)) %>%
            
            # Drop file name
            dplyr::select(-FILE_NAME) %>%
            
            # Add ethnic text variable
            mutate(ETHNIC_TXT = case_when(
              ETHNIC == "0" ~ "Not Reported"
              , ETHNIC == "1" ~ "American Indian or Alaska Native"
              , ETHNIC == "2" ~ "Asian"
              , ETHNIC == "3" ~ "Pacific Islander"
              , ETHNIC == "4" ~ "Filipino"
              , ETHNIC == "5" ~ "Hispanic or Latino"
              , ETHNIC == "6" ~ "African American"
              , ETHNIC == "7" ~ "White"
              , ETHNIC == "9" ~ "Two or More Races"))

          
        


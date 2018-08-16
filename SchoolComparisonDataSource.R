# Create Tableau-friendly data sources from CDE data files

# Renaming and organizing CORE data files for submission to CORE FTP

# Clear console
cat("\014") 

# Clear memory
rm(list=ls())
gc()

# Install/load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, tidyr, dplyr, readxl)

# Set working directory
setwd("C:/Data/State Data Files")

#------------------------------------------------
# Public Schools Directory
#------------------------------------------------

# Import publschls.txt, tab-delimited file
df_pubschls <- "pubschls.txt" %>%
                
                  # Import file
                  read.csv(header = TRUE
                            , sep = "\t"
                            , colClasses = "character") %>%

                  # Keep active schools
                  filter(StatusType == "Active") %>%
  
                  # Keep relevant columns; rename CDSCode to CDS_CODE to match other file formats
                  dplyr::select(CDS_CODE = CDSCode
                                 , County
                                 , District
                                 , School
                                 , StreetAbr
                                 , City
                                 , Zip
                                 , State
                                 , Phone
                                 , WebSite
                                 , Charter
                                 , CharterNum
                                 , DOCType
                                 , SOCType
                                 , EdOpsName
                                 , EILName
                                 , GSoffered
                                 , GSserved
                                 , Latitude
                                 , Longitude
                                 , AdmFName1
                                 , AdmLName1
                                 , AdmEmail1
                                 , LastUpDate)

#------------------------------------------------
# Enrollment
#------------------------------------------------

source("C://Data/EnrollmentData.R")

# 
df_enr_tot <- df_enr %>%
  
  
  
  # Re-order columns
  select(FILE_NAME
         , CDS_CODE
         , ETHNIC_TXT
         , GENDER
         , ENR_TOTAL) %>%
  
  # Group by year, school, and ethnicity
  group_by(FILE_NAME
           , CDS_CODE
           , ETHNIC_TXT) %>%
  
  # Calculate total enrollment by ethnicity by school
  summarise_at(vars(ENR_TOTAL)
               , funs(sum)) %>%
  
  # Convert output to a data frame
  as.data.frame() %>%
  
  # ETHNIC_TXT variable is not needed
  select(FILE_NAME
         , CDS_CODE
         , ENR_TOTAL) %>%
  
  # Group by school
  group_by(FILE_NAME
           , CDS_CODE) %>%
  
  # Calculate total enrollment for each school
  summarise_at(vars(ENR_TOTAL)
               , funs(sum)) %>%
  
  # Conver output to data frame
  as.data.frame()



# Add total school enrollment as a column
df_enr_1718 <- df_enr_1718 %>%
  
  # Filter for "All Students" group and then join to ethnicity enrollment data
  left_join(df_enr_total
            , by = "CDS_CODE") %>%
  
  # Re-name and re-order columns
  select(CDS_CODE
         , ETHNIC_TXT
         , ENR_ETHNIC = ENR_TOTAL.x
         , ENR_TOTAL = ENR_TOTAL.y) %>%
  
  # Calculate % ethnicity and add variable
  mutate(PCT_ETHNIC = ENR_ETHNIC / ENR_TOTAL) %>%
  
  # Re-order columns
  select(CDS_CODE
         , ETHNIC_TXT
         , PCT_ETHNIC
         , ENR_TOTAL) %>%
  
  # Pivot rows to columns, if value is missing assign 0
  spread(ETHNIC_TXT
         , PCT_ETHNIC
         , fill = 0) %>%
  
  # Re-name Columns
  select(CDS_CODE
         , ENR_TOTAL
         , PCT_AfricanAmerican = "African American"
         , PCT_AmericanIndian = "American Indian or Alaska Native"
         , PCT_Asian = "Asian"
         , PCT_Filipino = "Filipino"
         , PCT_Latinx = "Hispanic or Latino"
         , PCT_NotReported = "Not Reported"
         , PCT_PacificIsland = "Pacific Islander"
         , PCT_MultiEthnic = "Two or More Races"
         , PCT_White = "White")



# Import 17-18 enrollment data file
df_enr_1718 <- "enr_1718.txt" %>%
                  
                  # Import file
                  read.csv(header = TRUE
                            , sep = "\t"
                            , colClasses = "character") %>%
  
                  # Keep relevant columns
                  select(CDS_CODE
                          , ETHNIC
                          , GENDER
                          , ENR_TOTAL) %>%

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
                                        , ETHNIC == "9" ~ "Two or More Races")) %>%

                  # Drop ethnic code column
                  select(-ETHNIC) %>%

                  # Re-order columns
                  select(CDS_CODE
                         , ETHNIC_TXT
                         , GENDER
                         , ENR_TOTAL) %>%
                  
                  # Cast enr_total as number
                  mutate_at(vars(ENR_TOTAL)
                            , funs(as.numeric)) %>%

                  # Group by school and ethnicity
                  group_by(CDS_CODE
                           , ETHNIC_TXT) %>%
                  
                  # Calculate enrollment for ethnicity by school
                  summarise_at(vars(ENR_TOTAL)
                               , funs(sum)) %>%
                  
                  # Convert output to a data frame
                  as.data.frame()

# Calculate total school enrollment
df_enr_total <- df_enr_1718 %>%
                
                # ETHNIC_TXT variable is not needed
                select(CDS_CODE
                       , ENR_TOTAL) %>%
                
                # Group by school
                group_by(CDS_CODE) %>%
                
                # Calculate total enrollment for each school
                summarise_at(vars(ENR_TOTAL)
                             , funs(sum))

# Add total school enrollment as a column
df_enr_1718 <- df_enr_1718 %>%
                
                # Filter for "All Students" group and then join to ethnicity enrollment data
                left_join(df_enr_total
                          , by = "CDS_CODE") %>%
                
                # Re-name and re-order columns
                select(CDS_CODE
                       , ETHNIC_TXT
                       , ENR_ETHNIC = ENR_TOTAL.x
                       , ENR_TOTAL = ENR_TOTAL.y) %>%

                # Calculate % ethnicity and add variable
                mutate(PCT_ETHNIC = ENR_ETHNIC / ENR_TOTAL) %>%
                
                # Re-order columns
                select(CDS_CODE
                       , ETHNIC_TXT
                       , PCT_ETHNIC
                       , ENR_TOTAL) %>%

                # Pivot rows to columns, if value is missing assign 0
                spread(ETHNIC_TXT
                       , PCT_ETHNIC
                       , fill = 0) %>%

                # Re-name Columns
                select(CDS_CODE
                       , ENR_TOTAL
                       , PCT_AfricanAmerican = "African American"
                       , PCT_AmericanIndian = "American Indian or Alaska Native"
                       , PCT_Asian = "Asian"
                       , PCT_Filipino = "Filipino"
                       , PCT_Latinx = "Hispanic or Latino"
                       , PCT_NotReported = "Not Reported"
                       , PCT_PacificIsland = "Pacific Islander"
                       , PCT_MultiEthnic = "Two or More Races"
                       , PCT_White = "White")

# Join with public school directory info
df_all_data <- df_pubschls %>%
                
                # Join data
                left_join(df_enr_1718
                          , by = "CDS_CODE")
#------------------------------------------------
# Diversity Index
#------------------------------------------------
# divesity_index <- function(AA, AI, AS, HI, FI, PI, WH, MR, NR) {
#   
#   fAA = 
#   
# }

#------------------------------------------------
# FRL
#------------------------------------------------
# Import CDS info
df_frl_1718 <- "frpm1718.xlsx" %>%
                
                # Import data, skip first line; column names don't import cleanly
                read_excel(sheet = "FRPM School-Level Data "
                           , skip = 1
                           , col_names = FALSE
                           , col_types = c("text")) %>%
  
                # Remove "header" row
                slice(2:length(X__1)) %>%

                # Keep relevant columns
                select(COUNTY_CODE = X__2
                       , DISTRICT_CODE = X__3
                       , SCHOOL_CODE = X__4
                       , PCT_FRL = X__22) %>%
                
                # Create cds code column
                mutate(CDS_CODE = paste0(COUNTY_CODE
                                         , DISTRICT_CODE
                                         , SCHOOL_CODE)) %>%
                
                # Keep relevant columns and re-order
                select(CDS_CODE
                       , PCT_FRL) %>%

                # Cast pct frl as number
                mutate_at(vars(PCT_FRL)
                          , funs(as.numeric))

# Join with public school directory info
df_all_data <- df_all_data %>%
  
                # Join data
                left_join(df_frl_1718, by = "CDS_CODE")

#------------------------------------------------
# ELL
#------------------------------------------------

# Import ell data file
df_ell_1718 <- "elsch1718.txt" %>%
                
                # Read file
                read.csv(header = TRUE
                          , sep = "\t"
                          , colClasses = "character") %>%
                
                # Keep relevant columns 
                select(CDS_CODE = CDS
                       , LANGUAGE
                       , TOTAL_EL) %>%
  
                # Cast total el count as number
                mutate_at(vars(TOTAL_EL)
                          , funs(as.numeric)) %>%

                # Group by school
                group_by(CDS_CODE) %>%
                
                # Calculate total el enrollment by school
                summarise_at(vars(TOTAL_EL)
                             , funs(sum)) %>%
                
                # Convert output to a data frame
                as.data.frame() %>%

                # Join school enrollment
                left_join(select(df_enr_1718
                                 , CDS_CODE
                                 , ENR_TOTAL)
                          , by = "CDS_CODE") %>%
                
                # Calculate ell pct
                mutate(PCT_ELL = TOTAL_EL / ENR_TOTAL) %>%
  
                # Keep relevent columns
                select(CDS_CODE
                       , PCT_ELL)

# Join to all_data
df_all_data <- df_all_data %>%
                
                left_join(df_ell_1718
                          , by = "CDS_CODE")

#------------------------------------------------
# Mahalanobis Distance
#------------------------------------------------

df_mahalanobis <- df_all_data %>%
                    
                    filter(EILName == "Elementary"
                           , ) %>%
                    
                    select(ENR_TOTAL
                           , starts_with("PCT")) %>%
  
                    na.omit %>%
  
                    cov()
  
df_distance <- mahalanobis(df_mahalanobis
                           , colMeans(df_mahalanobis)
                           , cov(df_mahalanobis))

# Elementary schools
df_enr_1718_elem <- df_enr_1718 %>%
                      
                      # Join public schools directory information
                      left_join(df_pubschls, by = c("CDS_CODE" = "CDSCode")) %>%
                      
                      # Keep enrollment data and school level (i.e. Elementary, Middle, etc.)
                      select(CDS_CODE
                             , EILName
                             , starts_with("ENR")
                             , starts_with("PCT")) %>%
                      
                      # Keep only elementary schools
                      filter(EILName == "Elementary")

# Calculate z-scores 
df_enr_1718_elem_z <- df_enr_1718_elem %>%
  
                        # Select numeric columns
                        select(ENR_TOTAL:PCT_White) %>%
                        
                        # 2 -> operate on columns, scale -> function to calculate z-scores
                        apply(2, scale) %>%
                        
                        # Convert to data frame
                        as.data.frame() %>%
                        
                        # Add CDS_CODE column
                        #mutate(CDS_CODE = "") %>%
                        
                        # Re-name columns
                        select(Z_TOTAL = ENR_TOTAL
                               , Z_AfricanAmerican = PCT_AfricanAmerican
                               , Z_AmericanIndian = PCT_AmericanIndian
                               , Z_Asian = PCT_Asian
                               , Z_Filipino = PCT_Filipino
                               , Z_Latinx = PCT_Latinx
                               , Z_NotReported = PCT_NotReported
                               , Z_PacificIsland = PCT_PacificIsland
                               , Z_MultiEthnic = PCT_MultiEthnic
                               , Z_White = PCT_White)

# Bind columns
df_enr_1718_elem_z <- bind_cols(df_enr_1718_elem, df_enr_1718_elem_z)

# Cluster schools
set.seed(50)
clusters_elem <- df_enr_1718_elem_z %>%
                  select(starts_with("Z")) %>%
                  kmeans(50)

# Join cluster data
df_test <- df_enr_1718_elem_z %>%
            bind_cols(as.data.frame(clusters_elem$cluster))
            
names(df_test)[23] <- "Cluster"  

df_test_sum <- df_test %>%
                group_by(Cluster) %>%
                summarise_all(funs(mean))

df_cluster1 <- df_test %>%
                filter(Cluster == 1)

#------------------------------------------------
# Calculate euclidean distance
#------------------------------------------------

# Clean up
rm(df_enr_total
   , df_enr_zscores
   , df_ethnic_lk)

gc()

# Distance
df_dist <- df_enr_1718 %>%
            select(Z_TOTAL:Z_White) %>%
            dist(method = "euclidean", diag = FALSE, upper = FALSE) %>%
            as.matrix() %>%
            as.data.frame()

# Calculate rank
df_dist_rank <- df_dist %>%
                  apply(2, dense_rank) %>%
                  as.data.frame()

# Rename columns
names(df_dist_rank) <- df_enr_1718$CDS_CODE[1:10]

# Rename rows
row.names(df_dist_rank) <- df_enr_1718$CDS_CODE[1:10]

# 
df_dist_sch <- data.frame(row.names(df_dist_rank)
                          , df_dist_rank[,1])
# Rename columns
names(df_dist_sch) <- c("CDS_CODE", "DIST_RANK")

# Sort
df_dist_sch <- df_dist_sch %>%
                arrange(DIST_RANK)

# Return top 5 closest schools
df_similiar_schools <- df_dist_sch %>%
                        top_n(-5, DIST_RANK)

# Return top 5 closest schools
df_test <-  sort(df_dist_rank[,1])


df_similar_schools <- df_dist_rank %>%
                        top_n()

df_test <- filter(df_all_data
                  , CDS_CODE == "01612596057087")


                  




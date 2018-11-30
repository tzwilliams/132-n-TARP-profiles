## ===================================================== ##
# Title:         ####
# Project:      
#               https://github.com/tzwilliams/
# 
# Copyright 2018 Taylor Williams
# 
#     Licensed under the Apache License, Version 2.0 (the "License");
#     you may not use this file except in compliance with the License.
#     You may obtain a copy of the License at
#     
#     http://www.apache.org/licenses/LICENSE-2.0
#     
#     Unless required by applicable law or agreed to in writing, software
#     distributed under the License is distributed on an "AS IS" BASIS,
#     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#     See the License for the specific language governing permissions and
#     limitations under the License.
#
#
#
# Authors:      Taylor Williams
# Affiliation:  Purdue University
#
# Description:  
# 
# Package dependancies: 
#
# Changelog:
#   2018.10.23.   Init code
#   2018.11.27.   continued init development
#   2018.11.30.   First complete working version
#                   
# Feature wishlist:  (*: planned but not complete)
#     
## ===================================================== ##



######### Clean the environment ########## 
# rm(list=ls())   


######### Internal functions ########## 



######### Setup ##########
#load required packages
require(tidyverse)
require(readxl)


######### Read Data ##########
# _import the LO mapping data ####
LO_mapping <- read_xlsx(#path = file.choose(), 
                        path = "C:\\Users\\Taylor Williams\\OneDrive\\Documents\\_Purdue (OD)\\__Milestones\\2_Prelim (OD)\\132 data + info\\ENGR132_Sp18_assessment_plan (update1).xlsx",
                        sheet = "CGtoLOtoActivityMapping", 
                        col_names = T)
LO_mapping <- as_tibble(LO_mapping)

#extract and add the numeric versions of the LO, LO_map, and CO
LO_mapping <- add_column(.data = LO_mapping, 
                         "LO_ID" = NA,
                         "LO_map_ID" = NA,
                         "CO_ID" = NA)
for (i in 1:nrow(LO_mapping)) {
  LO_mapping$LO_ID[i]     <- str_extract(string = LO_mapping$`LO# Learning Objective`[i],
                                    pattern = "\\d{2}\\.\\d{2}")
  LO_mapping$LO_map_ID[i] <- str_extract(string = LO_mapping$LO_Map[i],
                                    pattern = "\\d{2}\\.\\d{2}")
  LO_mapping$CO_ID[i]     <- str_extract(string = LO_mapping$`CO# Course Goal`[i],
                                    pattern = "CO\\d{2}")
}



# _import BlackBoard LO data ####
data_raw_assessment <- read_csv(#file = file.choose(),
                                file = "C:\\Users\\Taylor Williams\\OneDrive\\Documents\\_Purdue (OD)\\__Milestones\\2_Prelim (OD)\\132 data + info\\132 deID data (sp18)\\_132 deID data. complete. clean.csv")



######### Clean Data ##########
# extract the numeric LO IDs
LO_ID <- str_match(string = data_raw_assessment$`Rubric Row`, pattern = "\\d{2}\\.\\d{2}")[,1]

# build working dataframe and add LO_ID column
df <- as_tibble(data_raw_assessment)
df <- df %>% add_column("LO_ID" = LO_ID)


## comparing defined vs used LOs #### 
LOs_in_map <- as.tibble(sort(unique(LO_mapping$LO_ID)))
LOs_in_assessment <- as.tibble(sort(unique(df$LO_ID)))

LOs_unused <- anti_join(LOs_in_map, LOs_in_assessment)



# get list of student IDs
student_ids_all <- unique(df_subset$"User ID")



########## SUBSETS for initial development and testing #########
# filter on the 01.xx LOs
LO_ID_range <- str_detect(string = df$LO_ID, pattern = "^01\\.\\d{2}")
df_subset <- df[LO_ID_range, ]
LO_subset <- sort(unique(df_subset$LO_ID))

# only include first 100 students
student_ids <- student_ids_all[1:100]



######### Probability Matrix ##########
#Create a prob matrix which will record the fraction of an individual's
#   assessment score for a particular LO: prob(assessment score|LO) 
#   (e.g., if they earn "Proficient" to 2 of 3 items for LO 01.03 
#   then 2/3 will be recorded)
#   
## build LO level feature vectors (FV)
# construct column names for feature vectors
proficiency_levels <- tibble(value = c(4,3,2,1,0),
                             label = c("4_Proficient", "3_Developing", 
                                       "2_Emerging", "1_Insufficient Evidence", 
                                       "0_No Attempt"), 
                             rubric_column =  c("Proficient", "Developing", 
                                                "Emerging", "Insufficient Evidence", 
                                                "No Attempt"), 
                             rubric_column_alt =  c("Proficient", "Developing", 
                                                    "Emerging", "Insufficient Evidence", 
                                                    "Did Not Attempt"))

# make list of FV name
FV_names <- character()
for (LO in LO_subset) {
  FV_names <- c(FV_names, paste0(LO, " - ", proficiency_levels$label))
}


# construct student FV table
stu_LO_FV <- tibble(student_id = student_ids)

# append feature vector names to the main table (init. values to 0)
for (i in 1:length(FV_names)) {
  stu_LO_FV <- stu_LO_FV %>% add_column(!!FV_names[i] := 0)
  
}
stu_LO_FV <- column_to_rownames(stu_LO_FV, var = "student_id")
  




#For each student subset the LOs and calculate average LO proficiency 
#   (TW Note: for now I'm ignoring the point value and working with the proficiency level)
for (cur_stu in student_ids) {
  cur_stu_LOs <- df_subset[df_subset$"User ID" == cur_stu, ]

  for (LO in LO_subset){
    cur_assessments <- cur_stu_LOs[cur_stu_LOs$LO_ID == LO, "Rubric Column"]
    # LO_value_sum <- 0.0
    
    if(nrow(cur_assessments) > 0){
      # loop through the recorded proficencies for the current LO
      for (i in 1:length(cur_assessments$`Rubric Column`)){
        level <- cur_assessments$`Rubric Column`[[i]]
        
        # LO_value_sum <- LO_value_sum +
        #   proficiency_levels[proficiency_levels$rubric_column == level, "value"]
        
        cur_colName <- paste0(LO, " - ",
                              proficiency_levels$label[proficiency_levels$rubric_column == level])
        
        # increment the count for this LO's profiency level
        stu_LO_FV[cur_stu, cur_colName] <- stu_LO_FV[cur_stu, cur_colName] +1
      }
    
      
      # calc probability for these LOs
      stu_LO_FV[cur_stu, str_detect(string = FV_names, pattern = paste0("^", LO))] <- 
        stu_LO_FV[cur_stu, str_detect(string = FV_names, pattern = paste0("^", LO))]/i
    } # end IF
  } # end LO FOR
} # end stu FOR



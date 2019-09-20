## ===================================================== ##
# Title:        Building Feature Vectors ####
# Project:      132 n-TARP Profiles
#               https://github.com/tzwilliams/
# 
# Copyright 2018-19 Taylor Williams
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
#   2019.09.20.   Updates to combine LOs into higher level groupings
#                   
# Feature wishlist:  (*: planned but not complete)
#     * label the first column of the feature vector as "userID" (it's currently blank)
## ===================================================== ##



######### Clean the environment ########## 
rm(list=ls())


######### Internal functions ########## 



######### Setup ##########
#load required packages
require(tidyverse)
require(readxl)

#Load funtions 
source(paste0(getwd(), "/R/functions/DisplayPercentComplete.R"))


######### Read Data ##########
# _import the LO mapping data ####
LO_mapping <- read_xlsx(#path = file.choose(), 
                        path = "C:\\Users\\Taylor Williams\\Dropbox (Personal)\\_Purdue (DB)\\__Milestones\\2_Prelim (TW DB)\\132 data + info\\ENGR132_Sp18_assessment_plan (update1.01).xlsx",
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



# _import BlackBoard LO data in CSV format ####
data_raw_assessment <- read_csv(#file = file.choose(),
                                file = "C:\\Users\\Taylor Williams\\Dropbox (Personal)\\_Purdue (DB)\\__Milestones\\2_Prelim (TW DB)\\132 data + info\\_2 cleaned data\\_assessment data. 132 sp18 deID data. complete. cleaned 2019.09.12\\_132 deID data. complete. clean. 2019.09.12.csv")



######### Clean Data ##########
# extract the numeric LO IDs
LO_ID <- str_match(string = data_raw_assessment$`Rubric Row`, pattern = "\\d{2}\\.\\d{2}")[,1]

# build working dataframe and add LO_ID column
df <- as_tibble(data_raw_assessment)
df <- df %>% add_column("LO_ID" = LO_ID)

# lookup LO_map_ID and CO_ID from th LO_mapping data
df <- add_column(.data = df, 
                 "LO_map_ID" = NA,
                 "CO_ID" = NA)
for (i in 1:nrow(df)) {
  # df$LO_ID[i]     <- str_extract(string = LO_mapping$`LO# Learning Objective`[i],
  #                                        pattern = "\\d{2}\\.\\d{2}")
  df$LO_map_ID[i] <- LO_mapping$LO_map_ID[LO_mapping$LO_ID == df$LO_ID[i]][1]
  df$CO_ID[i]     <- LO_mapping$CO_ID[LO_mapping$LO_ID == df$LO_ID[i]][1]
  
  #| print completion progress to console   ####
  #durring first iteration, create progress status variables for main processing loop
  if(i==1)
  {
    iCount <- 0 #loop counter for completion updates
    pct <- 0  #percentage complete tracker
  }
  
  #print function
  updateVars <- DisplayPercentComplete(dataFrame = df, iCount, pct, displayText = "LO grouping: ")
  
  #update status variables (for next iteration)
  iCount <- updateVars$iCount
  pct <- updateVars$pct
  
  #print update
  cat(updateVars$toPrint)
  
}


## comparing defined vs used LOs #### 
LOs_in_map <- as.tibble(sort(unique(LO_mapping$LO_ID)))
LOs_in_assessment <- as.tibble(sort(unique(df$LO_ID)))

LOs_unused <- anti_join(LOs_in_map, LOs_in_assessment)



# get list of all student IDs in the dataset
student_ids_all <- unique(df$"User ID")



########## SUBSETS for initial development and testing #########
# # filter on the 01.xx LOs
# LO_ID_range <- str_detect(string = df$LO_ID, pattern = "^01\\.\\d{2}")
# df_subset <- df[LO_ID_range, ]
# LO_subset <- sort(unique(df_subset$LO_ID))

# filter on ALL LOs
# LO_ID_range <- str_detect(string = df$LO_ID, pattern = "^01\\.\\d{2}")
df_subset <- df
LO_subset <- sort(unique(df_subset$LO_ID))

# only include first n students
student_ids <- student_ids_all#[1:100]


########## COMBINE LOs into higher level groupings #########
# 
# LO_ID_range <- str_detect(string = df$LO_ID, pattern = "^01\\.\\d{2}")



######### Probability Matrix ##########
#Create a prob matrix which will record the fraction of an individual's
#   assessment score for a particular LO: prob(assessment score|LO) 
#   (e.g., if they earn "Proficient" to 2 of 3 items for LO 01.03 
#   then 2/3 will be recorded)
#   
## build LO level feature vectors (FV)
# construct column names for feature vectors
proficiency_levels <- tibble(value = c(4,3,2,1,0,0),
                             label = c("4_Proficient", "3_Developing", 
                                       "2_Emerging", "1_Insufficient Evidence", 
                                       "0_No Attempt", "0_Did Not Attempt"), 
                             rubric_column =  c("Proficient", "Developing", 
                                                "Emerging", "Insufficient Evidence", 
                                                "No Attempt", "Did Not Attempt"), 
                             rubric_column_alt =  c("", "", 
                                                    "", "", 
                                                    "", ""))

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
        
        # convert proficency level into the column name format for this specific LO
        cur_colName <- paste0(LO, " - ",
                              proficiency_levels$label[proficiency_levels$rubric_column == level])
        
        # increment the count for this LO's profiency level
        stu_LO_FV[cur_stu, cur_colName] <- stu_LO_FV[cur_stu, cur_colName] + 1
      }
    
      
      # calc probability for these LOs
      stu_LO_FV[cur_stu, str_detect(string = FV_names, pattern = paste0("^", LO))] <- 
        stu_LO_FV[cur_stu, str_detect(string = FV_names, pattern = paste0("^", LO))]/i
    } # end IF
  } # end LO FOR
  
  #| print completion progress to console   ####
    #durring first iteration, create progress status variables for main processing loop
    if(cur_stu==student_ids[1])
    {
      iCount <- 0 #loop counter for completion updates
      pct <- 0  #percentage complete tracker
    }
  
    #print function
    updateVars <- DisplayPercentComplete(dataFrame = student_ids, iCount, pct, displayText = "Student: ")
  
    #update status variables (for next iteration)
    iCount <- updateVars$iCount
    pct <- updateVars$pct
  
    #print update
    cat(updateVars$toPrint)
  
} # end stu FOR






######### Save data to CSV file #########
write.csv(x = stu_LO_FV, file.choose())

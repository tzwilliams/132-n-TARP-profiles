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
#   2020.02.19.   init code
#     * extracted code for building feature vectors to 110_buildFeatureVectors.R
#     
#                   
# Feature wishlist:  (*: planned but not complete)
#     * add functionality to the user selection of the threshold number of data points to include
#     * create a new feature associated with each LO to record the observed probability that the student submitted anything for that LO during the semester
#     * build feature vectors that do and don't include team assessments
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
load(file.path("output", paste0("100_assessmentData.RData")))





########## SUBSETS for initial development and testing #########
# # filter on the 01.xx LOs
# LO_ID_range <- str_detect(string = df$LO_ID, pattern = "^01\\.\\d{2}")
# df_subset <- df[LO_ID_range, ]
# LO_subset <- sort(unique(df_subset$LO_ID))


# user selection of CO filtering
repeat{
  userSelection_filter <- readline(prompt="\n Would you like to use all assessments or only those from one CO (course objective)?:
Enter '0' for ALL assessments
      '1' for only CO01
      '2' for only CO02
      '3' for only CO03
      '4' for only CO04
      '5' for only CO05");
  
  
  
  #exit loop and continue script if input valid
  if(userSelection_filter == 0){
    # no filter
    df_subset <- df
    break
  } else if(userSelection_filter == 1){
    # filter on the CO01 LOs
    LO_ID_range <- df$CO_ID == "CO01"
    df_subset <- df[LO_ID_range, ]
    LO_subset <- sort(unique(df_subset$LO_ID))
    break
  } else if(userSelection_filter == 2){
    # filter on the CO02 LOs
    LO_ID_range <- df$CO_ID == "CO02"
    df_subset <- df[LO_ID_range, ]
    break
  } else if(userSelection_filter == 3){
    # filter on the CO03 LOs
    LO_ID_range <- df$CO_ID == "CO03"
    df_subset <- df[LO_ID_range, ]
    break
  } else if(userSelection_filter == 4){
    # filter on the CO04 LOs
    LO_ID_range <- df$CO_ID == "CO04"
    df_subset <- df[LO_ID_range, ]
    break
  } else if(userSelection_filter == 5){
    # filter on the CO05 LOs
    LO_ID_range <- df$CO_ID == "CO05"
    df_subset <- df[LO_ID_range, ]
    break
  }
  
  beepr::beep(sound = 10)   #notify user to provide input
}   #repeat if none of the conditions were met (i.e., user input was invalid)

#identify the LOs that remain in the dataset
LO_subset <- sort(unique(df_subset$LO_ID))




########## COMBINE LOs into higher level groupings #########
# user selection of grouping level
repeat{
  userSelection_LO_Grouping <- readline(prompt="\n At what level would you like to group the learning objectives?:
Enter '1' for detailed level (LO level, not recommended),
      '2' for middle level (CC level),
      '3' for highest level (CO level)");
  
  
  
  #exit loop and continue script if input valid
  if(userSelection_LO_Grouping == 1){
    LO_lvl <- 'LO_ID'
    LO_subset <- sort(unique(df_subset$LO_ID))
    break
  } else if(userSelection_LO_Grouping == 2){
    LO_lvl <- 'CC_ID'
    LO_subset <- sort(unique(df_subset$CC_ID))
    break
  } else if(userSelection_LO_Grouping == 3){
    LO_lvl <- 'CO_ID'
    LO_subset <- sort(unique(df_subset$CO_ID))
    break
  }
  
  beepr::beep(sound = 10)   #notify user to provide input
}   #repeat if none of the conditions were met (i.e., user input was invalid)



# user selection of minimum number of observations
repeat{
  userSelection_minObs <- readline(prompt="\n What is the minimum number of observations required \n at the selected learning objective group level? :
Enter '1' to include all available assessments,
      '2'-'9' to only include groups with at least that number of assessments   ");
  
  userSelection_minObs <- as.integer(userSelection_minObs)
  
  #exit loop and continue script if input valid
  if(userSelection_minObs >= 1 &
     userSelection_minObs <= 9){
    break
  }
  beepr::beep(sound = 10)   #notify user to provide input
}   #repeat if none of the conditions were met (i.e., user input was invalid)










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
stu_LO_FV <- tibble(user_ID=student_IDs$student_id)

# append feature vector names to the main table (init. values to 0)
for (i in 1:length(FV_names)) {
  stu_LO_FV <- stu_LO_FV %>% add_column(!!FV_names[i] := 0)
}
# stu_LO_FV <- column_to_rownames(stu_LO_FV, var = "student_id")





#For each student subset the LOs and calculate average LO proficiency 
#   (TW Note: for now I'm ignoring the point value and working with the proficiency level)
for (j in 1:nrow(student_IDs)) {
  cur_stu <- as.character(student_IDs[j, 1])
  cur_stu_row <- stu_LO_FV[,1] == cur_stu
  cur_stu_LOs <- df_subset[df_subset$"User ID" == cur_stu, ]
  
  for (LO in LO_subset){
    cur_assessments <- cur_stu_LOs[cur_stu_LOs[[LO_lvl]] == LO, "Rubric Column"]
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
        stu_LO_FV[cur_stu_row, cur_colName] <- (stu_LO_FV[cur_stu_row, cur_colName] + 1)
      }
      
      
      # calc probability for these LOs
      stu_LO_FV[,2:length(stu_LO_FV)][cur_stu_row, str_detect(string = FV_names, pattern = paste0("^", LO))] <- 
        stu_LO_FV[,2:length(stu_LO_FV)][cur_stu_row, str_detect(string = FV_names, pattern = paste0("^", LO))]/i
    } # end IF
  } # end LO FOR
  
  #| print completion progress to console   ####
  #durring first iteration, create progress status variables for main processing loop
  if(cur_stu==student_IDs[1])
  {
    iCount <- 0 #loop counter for completion updates
    pct <- 0  #percentage complete tracker
  }
  
  #print function
  updateVars <- DisplayPercentComplete(dataFrame = as.data.frame(student_IDs), iCount, pct, displayText = "Probability matrix: ")
  
  #update status variables (for next iteration)
  iCount <- updateVars$iCount
  pct <- updateVars$pct
  
  #print update
  cat(updateVars$toPrint)
  
} # end stu FOR







######### Save data to file #########
##Save assessment data to file ####
message("\nSaving Feature vector files.\n")

#write to CSV file
write_csv(path = file.path("output", paste0("110_stuFeatureVector-", LO_lvl ,"_grouping.csv")), 
          x = stu_LO_FV, col_names = T) 
#write to RData file
save(stu_LO_FV, LO_lvl, 
     file = file.path("output", paste0("110_stuFeatureVector-", LO_lvl ,"_grouping.RData")),
     precheck = TRUE, compress = TRUE)


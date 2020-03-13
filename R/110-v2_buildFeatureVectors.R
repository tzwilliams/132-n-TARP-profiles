## ===================================================== ##
# Title:        Building Feature Vectors ####
# Project:      132 n-TARP Profiles
#               https://github.com/tzwilliams/
#
# Copyright 2018-20 Taylor Williams
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
# Package dependencies:
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



######### Read Data ##########
load(file.path("output", paste0("100_assessmentData.RData")))



######### Setup ##########
#set run constants
DEV_MODE_DEFAULTS_TOGGLE <- F  #set true to use defaults (skip user options)
USER_IDS <- stu_sections$`User ID`[stu_sections$training_set==T]


#load required packages
require(tidyverse)
require(readxl)

#Load funtions
source(paste0(getwd(), "/R/functions/DisplayPercentComplete.R"))







########## user options and customization #########
##########
## dev mode option to skip user option selection durint initial development and testing

if(DEV_MODE_DEFAULTS_TOGGLE){
  # no CO filtering
  df_subset <- data_raw100_assessment_training
  
  # CO01 filtering
  # LO_ID_range <- data_raw100_assessment_training$CO_ID == "CO01"
  # df_subset <- data_raw100_assessment_training[LO_ID_range, ]

  LO_subset <- sort(unique(df_subset$LO_ID))
  
  
  
  # CO grouping level
  LO_lvl <- 'CO_ID'
  LO_subset <- sort(unique(df_subset$CO_ID))
  
  # # CC grouping level
  # LO_lvl <- 'CC_ID'
  # LO_subset <- sort(unique(df_subset$CC_ID))
  
  
  # minimum number of observations
  userSelection_minObs <- 1
  
}else{
  
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
      df_subset <- data_raw100_assessment_training
      break
    } else if(userSelection_filter == 1){
      # filter on the CO01 LOs
      LO_ID_range <- data_raw100_assessment_training$CO_ID == "CO01"
      df_subset <- data_raw100_assessment_training[LO_ID_range, ]
      break
    } else if(userSelection_filter == 2){
      # filter on the CO02 LOs
      LO_ID_range <- data_raw100_assessment_training$CO_ID == "CO02"
      df_subset <- data_raw100_assessment_training[LO_ID_range, ]
      break
    } else if(userSelection_filter == 3){
      # filter on the CO03 LOs
      LO_ID_range <- data_raw100_assessment_training$CO_ID == "CO03"
      df_subset <- data_raw100_assessment_training[LO_ID_range, ]
      break
    } else if(userSelection_filter == 4){
      # filter on the CO04 LOs
      LO_ID_range <- data_raw100_assessment_training$CO_ID == "CO04"
      df_subset <- data_raw100_assessment_training[LO_ID_range, ]
      break
    } else if(userSelection_filter == 5){
      # filter on the CO05 LOs
      LO_ID_range <- data_raw100_assessment_training$CO_ID == "CO05"
      df_subset <- data_raw100_assessment_training[LO_ID_range, ]
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
  
  
} #end option selection







######### Probability Matrix ##########
#Create a prob matrix which will record the fraction of an individual's
#   assessment score for a particular LO: prob(assessment score|LO)
#   (e.g., if they earn "Proficient" to 2 of 3 items for LO 01.03
#   then 2/3 will be recorded)
#
## build LO level feature vectors (FV)
# construct column names for feature vectors
proficiency_levels <- tibble(value = c(4,3,2,1,NA),
                             label = c("4_Proficient", "3_Developing",
                                       "2_Emerging", "1_Insufficient Evidence", "pct submitted"),
                                       # "0_No Attempt", "0_Did Not Attempt",
                                       # "0_No Submission"),
                             rubric_column =  c("Proficient", "Developing",
                                                "Emerging", "Insufficient Evidence", NA))
                                                # c("No Attempt", "Did Not Attempt", "No Submission")))


# make list of FV name
FV_names <- character()
for (LO in LO_subset) {
  FV_names <- c(FV_names, paste0(LO, " - ", proficiency_levels$label))
}


# construct student FV table
stu_LO_FV <- tibble("User ID" = USER_IDS)

# append feature vector names to the main table (init. values to 0)
for (i in 1:length(FV_names)) {
  stu_LO_FV <- stu_LO_FV %>% add_column(!!FV_names[i] := 0)
}
rm(i)

# drop 'pct submitted' from the list of FV names
prof_FVs <- !str_detect(string = FV_names,
                        pattern = "pct submitted$")
FV_names_prof <- FV_names[prof_FVs]
# save location of 'pct submitted' columns
pct_FVs <- str_detect(string = FV_names,
                      pattern = "pct submitted$")





# ..v2. With constructed missing assessment percentage  ####

# create place to store the missing assessment items
missing_cnt <- tibble('User ID' = as.character(),
                      'Missing item count' = as.integer(),
                      'Percent submitted' = as.numeric())

for (i in 1:length(USER_IDS)) {
  cur_stu <- USER_IDS[i]  #store the current student ID
  cur_stu_row <- (stu_LO_FV[,1] == cur_stu)

  # extract the cur_stu's LO data
  cur_stu_LOs <- df_subset[df_subset$"User ID" == cur_stu, ]

  # identify the total number of items the cur_stu is missing
  cur_missing_cnt <- sum(cur_stu_LOs$`Rubric Column` == "Did Not Attempt", na.rm = T) +
    sum(cur_stu_LOs$`Rubric Column` == "No Attempt", na.rm = T) +
    sum(cur_stu_LOs$`Rubric Column` == "No Submission", na.rm = T)
  # cur_missing_cnt <- sum(!cur_stu_LOs$item_submitted_TF) # alt equiv option

  # calculate total missing submission percentage
  pct_submitted <- (nrow(cur_stu_LOs) - cur_missing_cnt)/nrow(cur_stu_LOs)
  
  # add the student and their total missing LO items to missing_cnt
  missing_cnt <- missing_cnt %>% add_case('User ID' = cur_stu,
                                          'Missing item count' = cur_missing_cnt,
                                          'Percent submitted' = pct_submitted)





    # loop through all the levels of the selected LO grouping
    for (LO in LO_subset){
      cur_assessments <- cur_stu_LOs[cur_stu_LOs[[LO_lvl]] == LO, "Rubric Column"]
      # LO_value_sum <- 0.0
      if(nrow(cur_assessments) > 0){

        ### find missing submission percentage for LO-groups
          # extract the cur_stu's LO data
        if (LO_lvl == "CO_ID") {
          cur_stu_LO_subset <- cur_stu_LOs[cur_stu_LOs$CO_ID == LO, ]
        }else if (LO_lvl == "CC_ID") {
          cur_stu_LO_subset <- cur_stu_LOs[cur_stu_LOs$CC_ID == LO, ]
        }else if (LO_lvl == "LO_ID") {
          cur_stu_LO_subset <- cur_stu_LOs[cur_stu_LOs$LO_ID == LO, ]
        }else{
          return()
        }
        
          # identify the number of items the cur_stu was missing
          cur_missing_cnt <- 
            sum(cur_stu_LO_subset$`Rubric Column` == "Did Not Attempt", na.rm = T) +
            sum(cur_stu_LO_subset$`Rubric Column` == "No Attempt", na.rm = T) +
            sum(cur_stu_LO_subset$`Rubric Column` == "No Submission", na.rm = T)
          # cur_missing_cnt <- sum(!cur_stu_LOs$item_submitted_TF) # alt equiv option
          
          # calculate submitted percentage
          pct_submitted <- (nrow(cur_stu_LO_subset) - cur_missing_cnt)/nrow(cur_stu_LO_subset)
          
          # save missing submission percentage into FV
          cur_colName <- paste0(LO, " - ",
                                proficiency_levels$label[proficiency_levels$label == 'pct submitted'])
          stu_LO_FV[cur_stu_row, cur_colName] <- pct_submitted
        
        
        ### loop through the recorded proficencies for the current LO
        for (j in 1:length(cur_assessments$`Rubric Column`)){
          level <- cur_assessments$`Rubric Column`[[j]]

          # LO_value_sum <- LO_value_sum +
          #   proficiency_levels[proficiency_levels$rubric_column == level, "value"]

          # convert proficency level into the column name format for this specific LO
          is_prof_lvl <- (proficiency_levels$rubric_column == level)&
                          (!is.na(proficiency_levels$rubric_column == level))
          if(sum(is_prof_lvl) > 0){
            cur_colName <- paste0(LO, " - ",
                                  proficiency_levels$label[is_prof_lvl])

          # increment the count for this LO's profiency level
          stu_LO_FV[cur_stu_row, cur_colName] <- (stu_LO_FV[cur_stu_row, cur_colName] + 1)
          }  
        }

        # calc probability for these LOs
        cur_dims <- str_detect(string = FV_names_prof, pattern = paste0("^", LO))
          #take the FV without the ID col, then extract only the dims with one of the 4 proficiency levels, then only extract the four specific values for the current student and LO proficiency levels. Divide the result by the number of assessments
        stu_LO_FV[,-1][prof_FVs][cur_stu_row, cur_dims] <-
          stu_LO_FV[,-1][prof_FVs][cur_stu_row, cur_dims]/nrow(cur_assessments)
      } # end IF
    } # end LO FOR


  #| print completion progress to console   ####
  #durring first iteration, create progress status variables for main processing loop
  if(i == 1)
  {
    iCount <- 0 #initialize loop counter for completion updates
    pct <- 0  #initialize percentage complete tracker

  }else{
    #print function
    updateVars <- DisplayPercentComplete(dataFrame = as.data.frame(USER_IDS),
                                         iCount, pct, displayText = "Feature vector completion: ")

    #update status variables (for next iteration)
    iCount <- updateVars$iCount
    pct <- updateVars$pct

    #print update
    cat(updateVars$toPrint)
  }


}


######## find and drop students who have zero assessments for one or more LO grouping levels ####

# examine the percent submitted columns only (include the user id column)
z <- data.table::as.data.table(stu_LO_FV[, c(TRUE, pct_FVs)])

# Find the student IDs for those students who are missing all assessments for one or more LO groupings
stu_to_drop <- z[ z[, do.call(pmin, .SD) == 0, .SDcols=names(stu_LO_FV[, c(TRUE, pct_FVs)])] ][, 1]
stu_to_drop <- as.tibble(stu_to_drop)

# save the rows associated with the students to drop
stu_LO_FV.dropped_IDs <- stu_LO_FV[stu_LO_FV$`User ID` %in%  stu_to_drop$`User ID`, ]
# filter out the rows associated with the students to drop
stu_LO_FV <- stu_LO_FV[!(stu_LO_FV$`User ID` %in%  stu_to_drop$`User ID`), ]





######### Save data to file #########
##Save feature vector to file ####
message("\nSaving Feature vector files.\n")
inc_COs <- unique(df_subset$CO_ID) %>% str_sort() %>% str_c(collapse = " ")  # string of COs included in data used

#write to CSV file
write_csv(path = file.path("output", paste0("110v2_stuFeatureVector-", LO_lvl ,"_grouping-", inc_COs, ".csv")),
          x = stu_LO_FV, col_names = T)
#write to RData file
save(stu_LO_FV, LO_lvl, stu_LO_FV.dropped_IDs,
     file = file.path("output", paste0("110v2_stuFeatureVector-", LO_lvl ,"_grouping-", inc_COs, ".RData")),
     precheck = TRUE, compress = TRUE)

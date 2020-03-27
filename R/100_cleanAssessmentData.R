## ===================================================== ##
# Title:        Clean Assessment Data ####
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
#   2019.09.20.   
#     * Updates to combine LOs into higher level groupings
#   2019.10.07.   
#     * Code the user selection of a minimum number of observations at the selected LO grouping level (i.e., at least three observations at the CC level)
#     * allow users to select what level of LO grouping they want (LO, CC, CO)
#     * code the LO grouping function
#     * update the Feature Vector calculation in light of the selected grouping level
#   2020.02.19.   
#     * several minor fixes and updates in the past week
#     * labeled the first column of the feature vector as "userID" (it was blank)
#     * extracted code for building feature vectors to 110_buildFeatureVectors.R
#     
#                   
# Feature wishlist:  (*: planned but not complete)
#     * add functionality to the user selection of the threshold number of data pointsto include
## ===================================================== ##



######### Clean the environment ########## 
rm(list=ls())


######### Internal functions ########## 



######### Setup ##########
#load required packages
require(tidyverse)
require(lubridate)
require(readxl)

#Load funtions 
source(paste0(getwd(), "/R/functions/DisplayPercentComplete.R"))


######### Read Data ##########
# _import the LO mapping data ####
LO_mapping <- read_xlsx(#path = file.choose(), 
                        path = "C:\\Users\\Taylor Williams\\Dropbox (Personal)\\_Purdue (DB)\\__Milestones\\3_Dissertation (TW DB)\\132 data + info\\ENGR132_Sp18_assessment_plan (update1.01).xlsx",
                        sheet = "CGtoLOtoActivityMapping", 
                        col_names = T)
LO_mapping <- as_tibble(LO_mapping)




# # _import BlackBoard LO data in CSV format ####
# data_raw_complete <- read_csv(#file = file.choose(),
#                                 file = "C:\\Users\\Taylor Williams\\Dropbox (Personal)\\_Purdue (DB)\\__Milestones\\3_Dissertation (TW DB)\\132 data + info\\_2 cleaned data\\_assessment data. 132 sp18 deID data. complete. cleaned 2019.09.12\\_132 deID data. complete. clean. 2019.09.12.csv")
# _import BlackBoard LO data with missing rows added (from 060) ####
load(file = file.path("output", paste0("060v2_assessmentData_wMissing+.RData")))


# _import training set of Student IDs ####
# training_ids <- read_csv(#file = file.choose(),
  # file = "C:\\Users\\Taylor Williams\\Dropbox (Personal)\\_Purdue (DB)\\__Milestones\\3_Dissertation (TW DB)\\132 data + info\\_2 cleaned data\\2020.02.17. 80-20 data split\\050_studentIDs_trainingSet_80pct.csv")
load(file = file.path("output", paste0("050_studentsAndSplit.RData")))




######### Clean Data ##########
df <- as_tibble(data_raw060v2.1)


# .extract and add the numeric versions of the LO, CC, and CO ####
LO_mapping <- add_column(.data = LO_mapping, 
                         "LO_ID" = NA,
                         "CC_ID" = NA,
                         "CO_ID" = NA)
for (i in 1:nrow(LO_mapping)) {
  LO_mapping$LO_ID[i]     <- str_extract(string = LO_mapping$`LO# Learning Objective`[i],
                                         pattern = "\\d{2}\\.\\d{2}")
  LO_mapping$CC_ID[i] <- str_extract(string = LO_mapping$LO_Map[i],
                                     pattern = "\\d{2}\\.\\d{2}")
  LO_mapping$CO_ID[i]     <- str_extract(string = LO_mapping$`CO# Course Goal`[i],
                                         pattern = "CO\\d{2}")
}

# extract the numeric LO IDs
LO_ID <- str_match(string = df$`Rubric Row`, 
                   pattern = "\\d{2}\\.\\d{2}")[,1]

# (boolean) determine if the student submitted anything for the item, save in new column
item_submitted_TF <- !str_detect(string = df$`Rubric Column`, 
                                pattern = "(No Attempt)|(Did Not Attempt)|(No Submission)")



# build working dataframe and add columns for LO/CC/CO IDs and a boolean value for if the item was submitted 
df <- df %>% add_column("LO_ID" = as.character(LO_ID),
                  "CC_ID" = as.character(NA),
                  "CO_ID" = as.character(NA),
                  "item_submitted_TF" = as.logical(item_submitted_TF))


# .loop to lookup CC_ID and CO_ID from the LO_mapping data ####
for (i in 1:nrow(df)) {
  # df$LO_ID[i]     <- str_extract(string = LO_mapping$`LO# Learning Objective`[i],
  #                                        pattern = "\\d{2}\\.\\d{2}")
  # Look in the map for the CC and CO that match the LO and save to the current working data frame
  df$CC_ID[i] <- LO_mapping$CC_ID[LO_mapping$LO_ID == df$LO_ID[i]][1]
  df$CO_ID[i] <- LO_mapping$CO_ID[LO_mapping$LO_ID == df$LO_ID[i]][1]

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

##### #####
### .for regraded items, only retain the most recent grade ####

# convert all timestamp data into a more usable form using lubridate package
df$`Attempt Date` <- dmy_hms(df$`Attempt Date`)
df$`First Graded Date` <- dmy_hms(df$`First Graded Date`)
df$`Last Graded Date` <- dmy_hms(df$`Last Graded Date`)

#Create a place to save a backup of any duplicated items
data_raw100_dup_items_backup <- tibble()



####Search for duplicated (regraded) LO items in the raw data ####
for (user_ID in stu_sections$`User ID`) {
  
  #Identify the duplicated items for this user 
  dup   <-  duplicated(df[df$`User ID` == user_ID, ]$assmt_item_ID)
  dup2  <-  duplicated(df[df$`User ID` == user_ID, ]$assmt_item_ID, fromLast = T)
  duplicated_items <- (df[df$`User ID` == user_ID, ][(dup | dup2),])
  
  #Continue if duplicated items were identified
  if(nrow(duplicated_items) > 1){
    
    #Save a backup of the duplicated items
    data_raw100_dup_items_backup <- data_raw100_dup_items_backup %>% 
      bind_rows(duplicated_items)
    
    # Identify and then retain only the most recent grade value for any re-graded items
    for (item_ID in unique(duplicated_items$assmt_item_ID)) {
      #isolate a single set of entries for a regraded item
      cur_items <- duplicated_items[duplicated_items$assmt_item_ID == item_ID, ] 
      
      #identify the most recent entry (and only the first of any duplicates)
      cur_items_keep <- cur_items[cur_items$`Last Graded Date` == max(cur_items$`Last Graded Date`), ][1,]
      
      #update the data frame
      df <- df %>% anti_join(cur_items, ) %>%     #remove the duplicated items 
        bind_rows(cur_items_keep)   #add back the most receint item
      
      print(paste0(user_ID, ": ", which(stu_sections$`User ID` == user_ID), " of ", nrow(stu_sections), ". ", nrow(duplicated_items), " duplicates."))
    }
  }
}

#remove any rows that were added more than once
data_raw100_dup_items_backup <- distinct(data_raw100_dup_items_backup)

# put working data frame into a more descriptively named one
data_raw100_assessment_all <- df

#extract the training data points into a separate data frame
data_raw100_assessment_training <- 
  data_raw100_assessment_all[data_raw100_assessment_all$`User ID` %in% 
                               stu_sections$`User ID`[stu_sections$training_set==T], ] 
#extract the verification data points into a separate data frame
data_raw100_assessment_verification <- 
  data_raw100_assessment_all[data_raw100_assessment_all$`User ID` %in% 
                               stu_sections$`User ID`[stu_sections$training_set==F], ] 




##Save assessment data to file ####
message("\nSaving assessment files.\n")

#write to CSV file
write_csv(path = file.path("output", paste0("100_assessmentData_complete.csv")),
          x = data_raw100_assessment_all)
#write to RData file
save(data_raw100_assessment_all, stu_sections, data_raw100_dup_items_backup,
     data_raw100_assessment_verification, data_raw100_assessment_training,
     file = file.path("output", paste0("100_assessmentData.RData")),
     precheck = TRUE, compress = TRUE)


# 
# 
# ## comparing defined vs used LOs ###
# LOs_in_map <- as.tibble(sort(unique(LO_mapping$LO_ID)))
# LOs_in_assessment <- as.tibble(sort(unique(df$LO_ID)))
# 
# LOs_unused <- anti_join(LOs_in_map, LOs_in_assessment)
# 
# 
# 
# # get list of all student IDs in the dataset
# student_ids_all <- unique(df$"User ID")
# 

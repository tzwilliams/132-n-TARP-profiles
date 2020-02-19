## ===================================================== ##
# Title:         ####
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

#extract and add the numeric versions of the LO, CC, and CO
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



# _import BlackBoard LO data in CSV format ####
data_raw_complete <- read_csv(#file = file.choose(),
                                file = "C:\\Users\\Taylor Williams\\Dropbox (Personal)\\_Purdue (DB)\\__Milestones\\3_Dissertation (TW DB)\\132 data + info\\_2 cleaned data\\_assessment data. 132 sp18 deID data. complete. cleaned 2019.09.12\\_132 deID data. complete. clean. 2019.09.12.csv")



# _import training set of Student IDs ####
training_ids <- read_csv(#file = file.choose(),
  file = "C:\\Users\\Taylor Williams\\Dropbox (Personal)\\_Purdue (DB)\\__Milestones\\3_Dissertation (TW DB)\\132 data + info\\_2 cleaned data\\2020.02.17. 80-20 data split\\050_studentIDs_trainingSet_80pct.csv")

  #filter on the training data points (Assuming the training ids are in a column named 'x')
data_raw_assessment <- data_raw_complete[data_raw_complete$`User ID` %in% training_ids$x, ] 


######### export Student IDs to file ##########
#Save Student IDs to file ####
message("\nSaving Student IDs.\n")

student_IDs <- as.data.frame( unique(data_raw_assessment$`User ID`) )

# Rename column where names is "user_ID"
names(student_IDs)[names(student_IDs) == "unique(data_raw_assessment$`User ID`)"] <- "user_ID"

#write to CSV file
write_csv(path = file.path("output", paste0("100_studentIDsUsed.csv")),
          x = student_IDs)




######### Clean Data ##########
# extract the numeric LO IDs
LO_ID <- str_match(string = data_raw_assessment$`Rubric Row`, pattern = "\\d{2}\\.\\d{2}")[,1]

# build working dataframe and add LO_ID column
df <- as_tibble(data_raw_assessment)
df <- df %>% add_column("LO_ID" = LO_ID)

# lookup CC_ID and CO_ID from th LO_mapping data
df <- add_column(.data = df,
                 "CC_ID" = NA,
                 "CO_ID" = NA)
for (i in 1:nrow(df)) {
  # df$LO_ID[i]     <- str_extract(string = LO_mapping$`LO# Learning Objective`[i],
  #                                        pattern = "\\d{2}\\.\\d{2}")
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




##Save assessment data to file ####
message("\nSaving assessment files.\n")

#write to CSV file
write_csv(path = file.path("output", paste0("100_assessmentData.csv")),
          x = df)
#write to RData file
save(df, student_IDs, 
     file = file.path("output", paste0("100_assessmentData.RData")),
     precheck = TRUE, compress = TRUE)




## comparing defined vs used LOs #### 
LOs_in_map <- as.tibble(sort(unique(LO_mapping$LO_ID)))
LOs_in_assessment <- as.tibble(sort(unique(df$LO_ID)))

LOs_unused <- anti_join(LOs_in_map, LOs_in_assessment)



# get list of all student IDs in the dataset
student_ids_all <- unique(df$"User ID")


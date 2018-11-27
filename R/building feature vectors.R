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
#                   
# Feature wishlist:  (*: planned but not complete)
#     
## ===================================================== ##



######### Clean the environment ########## 
# rm(list=ls())   


######### Internal functions ########## 



######### Setup ##########
#load required packages
require("tidyverse")
require("readxl")


## Read data ####
# import the LO mapping data
LO_mapping <- readxl::read_xlsx(#path = file.choose(), 
                                path = "C:\\Users\\TaylorWilliams\\OneDrive\\Documents\\_Purdue (OD)\\__Milestones\\2_Prelim (OD)\\132 data + info\\ENGR132_Sp18_assessment_plan (update1).xlsx",
                                sheet = "CGtoLOtoActivityMapping", 
                                col_names = T)
LO_mapping <- as_tibble(LO_mapping)

#extract and add the numeric versions of the LO, LO_map, and CO
LO_mapping <- tibble::add_column(.data = LO_mapping, 
                                 "LO_ID" = NA,
                                 "LO_map_ID" = NA,
                                 "CO_ID" = NA)
for (i in 1:nrow(LO_mapping)) {
  LO_mapping$LO_ID[i] = str_extract(string = LO_mapping$`LO# Learning Objective`[i],
                                    pattern = "^/d{2}/./d{2}")
}



# import BB LO data file
data_raw <- read_csv(file.choose())

# extract the numeric LO IDs
LO_ID <- str_match(string = data_raw$`Rubric Row`, pattern = "\\d{2}\\.\\d{2}")[,1]
# LO_ID <- format(sort(round(as.numeric(LO_ID), digits=2)), nsmall = 2)
# build working dataframe
df <- data.frame(data_raw, LO_ID, stringsAsFactors=FALSE)
# df <- cbind(data_raw, LO_ID)



# filter on the 01.xx LOs
LO_ID_range <- str_detect(string = df$LO_ID, pattern = "^01\\.\\d{2}")
df_subset <- df[LO_ID_range, ]
LO_subset <- unique(df_subset$LO_ID)


# get list of student IDs
student_ids <- unique(df$User.ID)


# build LO level feature vector
proficiency_levels <- data.frame(value = c(4,3,2,1,0),
                                 label = c("4_Proficient", "3_Developing", 
                                            "2_Emerging", "1_Insufficient Evidence", 
                                            "0_No Attempt"), 
                                 rubric_column =  c("Proficient", "Developing", 
                                                    "Emerging", "Insufficient Evidence", 
                                                    "No Attempt"), 
                                 rubric_column_alt =  c("Proficient", "Developing", 
                                                    "Emerging", "Insufficient Evidence", 
                                                    "Did Not Attempt"))
stu_LO_FV <- data.frame(student_id = student_ids)
FV_names <- character()
for (LO in LO_subset) {
  FV_names <- c(FV_names, paste0(LO, " - ", proficiency_levels$label))
}



# # for each student subset the LOs and calculate average LO proficiency (TW Note: for now I'm ignoring the point value and only looking at the proficiency)
# for (cur_stu in student_ids) {
#   cur_stu_LOs <- df_subset[df_subset$User.ID == cur_stu, ]
#   
#   for (LO in LO_subset){
#     cur_assessment <- cur_stu_LOs[cur_stu_LOs$LO_ID == LO, "Rubric.Column"]
#     LO_value_sum <- 0
#     for (i in 1:length(cur_assessment)){
#       level <- cur_assessment[i]
#       # LO_value_sum <- LO_value_sum + 
#         # proficiency_levels[proficiency_levels$rubric_column == level, "value"]
#       # proficiency_levels[proficiency_levels$rubric_column == level, "value"]
#       
#       # stu_LO_FV[cur_stu, FV_names == paste0(LO, " - ", level)]
#     }
#     LO_avg <- LO_value_sum/i
#     
#     
#   }
#   
# }



######### Probability Matrix ##########
#Create a prob matrix which will record the fraction of an individual's
#   assessment score for a particular LO: prob(assessment score|LO) 
#   (e.g., if they earn "Proficient" to 2 of 3 items for LO 01.03 
#   then 2/3 will be recorded)

#Initialize the matrix with a column of the user IDs in the dataset
probMatrixRaw <- df %>% distinct(User.ID)


#creating columns for the probMatrixRaw.  Initializing all probabilities to 0
for (FV_field in FV_names) {
  probMatrixRaw <- tibble::add_column(.data = probMatrixRaw, 
                                      !!(FV_field) := 0)
}



#find the sum total of LOs assosciated with each LO code
#construct dataframe to hold totals, one row per code found in coded_Qs, initalize count to zero
codeTotalQs <- data.frame("Code"=levels(coded_Qs$Code1), "Total"=0)

for(i in 1:nlevels(coded_Qs$Code1)){
  #store the string for the current code 
  codeTotalQs$Total[i] <- sum(coded_Qs$Code1==levels(coded_Qs$Code1)[i])
}




# 
# 
# str_match(string = names(stu_LO_FV)[2:ncol(stu_LO_FV)], 
#           pattern = "(?<=_).*")


# build Category feature vector

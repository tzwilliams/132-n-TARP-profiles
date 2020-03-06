## ===================================================== ##
# Title:        Processing Missing Data  ####
# Project:      132 n-TARP Profiles
#               https://github.com/tzwilliams/
# 
# Copyright 2020 Taylor Williams
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
# 
#       [extract from dissertation prposal document, section 3.4.1.1] The data include a no attempt rating when students did not submit a solution to a problem. I am assuming no attempt does not lie on the same proficiency continuum as the other assessment levels since there are many reasons why a student might not submit an answer beyond their not being proficient (for example, a family emergency or prioritizing a party over homework). Since no attempt is not measuring the same latent construct of proficiency it needs to be treated differently and is one of the reasons why this data set is considered complex. To find a meaningful and usable value from the no attempt data, I will calculate the frequency that a student did not attempt assessment items within each course objective group.  Said another way, for students who have missing submissions I will not discard the no attempt data or isolate it; rather, I will create a new feature associated with each LO to record the observed probability that the student submitted anything for that LO during the semester.  Then, only considering those assessments that were turned in, I will calculate the observed probabilities of each assessment proficiency level for that student.  In this way, I will end up with five features associated with each LO—four features capturing the students’ assessed proficiency levels and one feature capturing the students’ propensity to complete assessments on a per-LO level. Missing assessment data will be assigned to the no attempt level during data pre-processing.
# 
# Package dependancies: 
#
# Changelog:
#   2020.02.24. init code
#                   
# Feature wishlist:  (*: planned but not complete)
#     * 
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
# inport student lists
load(file = file.path("output", paste0("050_studentsAndSplit.RData")))  # list of students still enrolled at the end of the course


# _import BlackBoard LO data in CSV format ####
data_raw_orig <- read_csv(#file = file.choose(),
  file = "C:\\Users\\Taylor Williams\\Dropbox (Personal)\\_Purdue (DB)\\__Milestones\\3_Dissertation (TW DB)\\132 data + info\\_2 cleaned data\\_assessment data. 132 sp18 deID data. complete. cleaned 2019.09.12\\_132 deID data. complete. clean. 2019.09.12.csv")



# get unique list of items that should have been assessed for each student
unique_items <- unique(data_raw_orig[c('Rubric Row', 'Rubric Title')])



# find complete list of Student IDs
# student_IDs_complete <- as.data.frame( unique(data_raw_orig$`User ID`) )
student_IDs_complete <- as.data.frame( unique(stu_sections) )


# New ID column for unique items
data_raw060v2 <- data_raw_orig %>% 
  mutate(assmt_item_ID = paste(`Rubric Title`, "---", `Rubric Row`))


# add in missing rows (missing item/user pairs) and mark the assessment as "No Submission"
# data_raw060v2 <- data_raw060v2 %>% expand(`User ID`, assmt_item_ID)
data_raw060v2.1 <- data_raw060v2 %>% complete(`User ID`, assmt_item_ID, 
                                              fill = list(`Rubric Column` = "No Submission"))

# intersect(y = data_raw060v2.1, x = data_raw060v2, by = "User ID")






######### Save data to file #########
##Save assessment data to file ####
message("\nSaving Feature vector files.\n")

#write to CSV file
write_csv(path = file.path("output", paste0("060v2_assessmentData_wMissing.csv")),
          x = data_raw060v2.1, col_names = T)
#write to RData file
save(data_raw_orig, data_raw060v2.1, unique_items,
     student_IDs_complete,
     file = file.path("output", paste0("060v2_assessmentData_wMissing+.RData")),
     precheck = TRUE, compress = TRUE)

## ===================================================== ##
# Title:        Split data into training and verification sets ####
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
# Description:  Randomly split the user ID list into two sets. One for training and one for verification
# 
# Package dependancies: 
#
# Changelog:
#   2020.02.??.   Initial code
#   2020.02.19.   Added header and other documentation
#     
#                   
# Feature wishlist:  (*: planned but not complete)
#     * The script is very incomplete, almost all initialization section still need to be added
## ===================================================== ##


######### Clean the environment ########## 
rm(list=ls())


######### Internal functions ########## 



######### Setup ##########
#load required packages


######### Read Data ##########
# _import the LO mapping data ####
finalGradesPath <- "C:\\Users\\Taylor Williams\\Dropbox (Personal)\\_Purdue (DB)\\__Milestones\\3_Dissertation (TW DB)\\132 data + info\\_1 raw data\\132 deID data (sp18)\\ENGR132_Sp18_SemesterGrades_v5_deID.xlsx"


sections <- c("Bb014", "Bb013", "Bb012", "Bb011", "Bb010", "Bb009", "Bb008", 
              "Bb007", "Bb006", "Bb005", "Bb004", "Bb003", "Bb002", "Bb001")
stu_Bb001 <- read_xlsx(path = finalGradesPath, range = "Bb001!D7:D150", col_names = "User ID") %>% 
  drop_na() %>%
  mutate(section = 'Bb001')
stu_Bb002 <- read_xlsx(path = finalGradesPath, range = "Bb002!D7:D150", col_names = "User ID") %>% 
  drop_na() %>%
  mutate(section = 'Bb002')
stu_Bb003 <- read_xlsx(path = finalGradesPath, range = "Bb003!D7:D150", col_names = "User ID") %>% 
  drop_na() %>%
  mutate(section = 'Bb003')
stu_Bb004 <- read_xlsx(path = finalGradesPath, range = "Bb004!D7:D150", col_names = "User ID") %>% 
  drop_na() %>%
  mutate(section = 'Bb004')
stu_Bb005 <- read_xlsx(path = finalGradesPath, range = "Bb005!D7:D150", col_names = "User ID") %>% 
  drop_na() %>%
  mutate(section = 'Bb005')
stu_Bb006 <- read_xlsx(path = finalGradesPath, range = "Bb006!D7:D150", col_names = "User ID") %>% 
  drop_na() %>%
  mutate(section = 'Bb006')
stu_Bb007 <- read_xlsx(path = finalGradesPath, range = "Bb007!D7:D150", col_names = "User ID") %>% 
  drop_na() %>%
  mutate(section = 'Bb007')
stu_Bb008 <- read_xlsx(path = finalGradesPath, range = "Bb008!D7:D150", col_names = "User ID") %>% 
  drop_na() %>%
  mutate(section = 'Bb008')
stu_Bb009 <- read_xlsx(path = finalGradesPath, range = "Bb009!D7:D150", col_names = "User ID") %>% 
  drop_na() %>%
  mutate(section = 'Bb009')
stu_Bb010 <- read_xlsx(path = finalGradesPath, range = "Bb010!D7:D150", col_names = "User ID") %>% 
  drop_na() %>%
  mutate(section = 'Bb010')
stu_Bb011 <- read_xlsx(path = finalGradesPath, range = "Bb011!D7:D150", col_names = "User ID") %>% 
  drop_na() %>%
  mutate(section = 'Bb011')
stu_Bb012 <- read_xlsx(path = finalGradesPath, range = "Bb012!D7:D150", col_names = "User ID") %>% 
  drop_na() %>%
  mutate(section = 'Bb012')
stu_Bb013 <- read_xlsx(path = finalGradesPath, range = "Bb013!D7:D150", col_names = "User ID") %>% 
  drop_na() %>%
  mutate(section = 'Bb013')
stu_Bb014 <- read_xlsx(path = finalGradesPath, range = "Bb014!D7:D150", col_names = "User ID") %>% 
  drop_na() %>%
  mutate(section = 'Bb014')

stu_sections <- union(stu_Bb014,stu_Bb013) %>% union(stu_Bb012) %>% 
  union(stu_Bb011) %>% union(stu_Bb010) %>% union(stu_Bb009) %>% 
  union(stu_Bb008) %>% union(stu_Bb007) %>% union(stu_Bb006) %>% 
  union(stu_Bb005) %>% union(stu_Bb004) %>% union(stu_Bb003) %>% 
  union(stu_Bb002) %>% union(stu_Bb001)

rm(list = c('stu_Bb014', 'stu_Bb013', 'stu_Bb012', 'stu_Bb011', 'stu_Bb010', 'stu_Bb009', 
            'stu_Bb008', 'stu_Bb007', 'stu_Bb006', 'stu_Bb005', 'stu_Bb004', 'stu_Bb003', 
            'stu_Bb002', 'stu_Bb001'))


######### Split the students into training and verification sets ######### 
## set the seed to make partition reproducible
set.seed(123)

## 80/20 split
spliter <- runif(length(stu_sections$`User ID`), 
                min = 0, 
                max = 1)
ID_split_training <- stu_sections$`User ID`[spliter <= 0.80, ]
ID_split_verification <- stu_sections$`User ID`[spliter < 0.20, ]






######### save IDs for data subsets to file ######### 
write.csv(x = ID_split_training, file = file.path("output", paste0("050_studentList-training.csv")))
write.csv(x = ID_split_verification, file = file.path("output", paste0("050_studentList-verification.csv")))
save(ID_split_training, ID_split_verification,
     stu_sections, sections, 
     file = file.path("output", paste0("050_studentsAndSplit.RData")),
     precheck = TRUE, compress = TRUE)

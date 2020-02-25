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






######### Split the students into training and verification sets ######### 


spliter <- runif(length(student_IDs$user_ID), 
                min = 0, 
                max = 1)
training <- student_IDs[spliter <= 0.80, ]
verification <- student_IDs[spliter < 0.20, ]
view(training)




######### save IDs for data subsets to file ######### 
write.csv(x = training, file = file.choose())
write.csv(x = verification, file = file.choose())

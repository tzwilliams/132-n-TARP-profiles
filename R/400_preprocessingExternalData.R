 
## ===================================================== ##
# Title:        Preprocessing Data Not Used in Profile Formation ####
# Project:      132 n-TARP Profiles
#               https://github.com/tzwilliams/
#               
# Authors:      Taylor Williams 
#
# Affiliation:  Purdue University
# 
# Description:  Scripts in the 400s focus on cleaning the data that was not used in profile construction. Once cleaned, this data will be used in the profile interpretation.
# 
# Package dependencies: 
# 
# Input stack: 
#
# Changelog:
#     
#                   
# Feature wishlist:  (*: planned but not complete)
#     *
## ===================================================== ##

######################################################################
######################################################################
########################## Skeleton structure ########################
############################### follows ##############################
######################################################################
######################################################################
######################################################################


######### Clean the environment ########## 
rm(list=ls())
# ## alt: Clean the environment except required variables########## 
# rm(list = setdiff(ls(), c(""))


######### Internal functions ########## 



######### Setup ##########
#load required packages
require(tidyverse)
require(readxl)

#Load funtions 
source(file.path(getwd(), "R", "functions", "DisplayPercentComplete.R"))
source(file.path(getwd(), "R", "functions", "file-structure-functions.R"))


######### Read Data ##########
if(!exists("filenamePrefix")) filenamePrefix <- NULL
if(!exists("dataFolderPath")) dataFolderPath <- NULL
if(!exists("filenameFV")) filenameFV <- NULL

## get data file locations from user ####
#Locate the CLEAN probability matrix (feature vector) file
filenameFV <- 
  SelectFile(prompt = "*****Select the CLEAN PROBABILITY MATRIX (feature vector) file (probably a `110_`)*****\n    (The file picker window may have opened in the background.  Check behind this window if you do not see it.)\n",
             defaultFilename = "110v2_stuFeatureVector-",
             # filenamePrefix = ifelse(exists("filenamePrefix") & !is.null(filenamePrefix),
             #                         yes = filenamePrefix, no = ""),
             fileTypeMatrix = matrix(c("RData", ".RData", "CSV", ".csv", "All files", ".*"),
                                     3, 2, byrow = TRUE),
             dataFolderPath = ifelse(exists("dataFolderPath") & !is.null(dataFolderPath),
                                     yes = dataFolderPath, no = ""))



#load in the data based on the type of data file provided
if(grepl(x = filenameFV, pattern = "\\.RData$")){
  objs <- load(file = filenameFV, verbose = T)
  probMatrix <- stu_LO_FV
  # probMatrix <- rownames_to_column(probMatrix)
  # names(probMatrix)[1] <- "User ID"
}else if(grepl(x = filenameFV, pattern = "\\.(csv|CSV)$")){
  probMatrix <- read_csv(file = filenameFV)
  names(probMatrix)[1] <- "User ID"
}else {
  message("Invalid Data Filetype.")
  break
}




######### Main ##########














######### Save data to file #########
message("\nSaving files.\n")

#write to CSV file
##Save dataFrame of profiles and percentage students in each profile
write.csv(file = file.path("output", paste0("300_profileDistribution-", n_vectors ,"_criteria.csv")),
          x = unique_assignment_counts, row.names = F)

##Save dataFrame of cluster assignments of individual users
write.csv(file = file.path("output", paste0("300_clusterAssignmentsAllUsers-", n_vectors ,"_criteria.csv")),
          x = cluster_assignments, row.names = T)
write.csv(file = file.path("output", paste0("300_clusterAssignmentsAllUsersCollapsed-", n_vectors ,"_criteria.csv")),
          x = cluster_assignments_collapsed, row.names = F)

#write to RData file
save(unique_assignment_counts, cluster_assignments, all_assignments,
     file = file.path("output", paste0("300_profiles-", n_vectors ,"_criteria.RData")),
     precheck = TRUE, compress = TRUE)


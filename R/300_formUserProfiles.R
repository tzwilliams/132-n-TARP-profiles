## ===================================================== ##
# Title:        Form User Profiles ####
# Project:      132 n-TARP Profiles
#               https://github.com/tzwilliams/
#               
# Authors:      Doipayan Roy (principal author), Taylor Williams (made modifications
#               to fix a bug and export important data)
#
# Affiliation:  Purdue University
# 
# Description:  Calculate projection values of user parameter vectors onto random
#               projection directions
# 
# Package dependancies: 
#
# Changelog:
#     
#                   
# Feature wishlist:  (*: planned but not complete)
#     *
## ===================================================== ##




######### Clean the environment ########## 
rm(list=ls())
# ## Clean the environment except required variables########## 
# rm(list = setdiff(ls(), c("course", "path_files", "path_output", "probMatrix",
#                   

######### Internal functions ########## 



######### Setup ##########
#load required packages
require(tidyverse)
require(readxl)

#Load funtions 
source(paste0(getwd(), "/R/functions/DisplayPercentComplete.R"))


######### Read Data ##########


##Read file with W values for projection directions
#to del# W_values <-  read.csv(paste0(path_output, "minW_Threshold_", course, ".csv"), row.names = 1)
#read the MIN_W and THRESHOLD data file
prompt <- "*****Select the MIN_W and THRESHOLD data file*****\n    (The file picker window may have opened in the background.  Check behind this window if you do not see it.)\n"
cat("\n", prompt, "\n\n")
filename <- tcltk::tk_choose.files(caption = prompt,
                                   default = file.path(getwd(),
                                                       "output",
                                                       "40_minW_and_threshold.RData"),
                                   filter = matrix(c("RData", ".RData",
                                                     "CSV", ".csv",
                                                     "All files", ".*"),
                                                   3, 2, byrow = TRUE),
                                   multi = FALSE)
#load in the data based on the type of data file provided
if(grepl(x = filename, pattern = "\\.RData$"))
{
  load(file = filename)

}else if(grepl(x = filename, pattern = "\\.(csv|CSV)$"))
{
  minW_RandVec_sort <- read_csv(file = filename)

}else
{
  message("Invalid Data Filetype.")
  return
}



##Read file containing projection directions
#to del# projection_directions <- read.csv(paste0(path_output, "randomVectors_",
                                         # course, ".csv"), row.names = 1)
#read the BEST RANDOM PROJECTIONS data file
# prompt <- "*****Select the BEST RANDOM PROJECTIONS data file*****\n    (The file picker window may have opened in the background.  Check behind this window if you do not see it.)\n"
# cat("\n", prompt, "\n\n")
# filename <- tcltk::tk_choose.files(caption = prompt,
#                                    default = file.path(getwd(),
#                                                        "output",
#                                                        "40_best_RP_names.RData"),
#                                    filter = matrix(c("RData", ".RData",
#                                                      "CSV", ".csv",
#                                                      "All files", ".*"),
#                                                    3, 2, byrow = TRUE),
#                                    multi = FALSE)
# #load in the data based on the type of data file provided
# if(grepl(x = filename, pattern = "\\.RData$"))
# {
#   load(file = filename)
#   projection_directions <- sortedCandidateNames
#     
# }else if(grepl(x = filename, pattern = "\\.(csv|CSV)$"))
# {
#   projection_directions <- read_csv(file = filename)
#   
# }else
# {
#   message("Invalid Data Filetype.")
#   return
# }
#read the PROJECTIONS data file
prompt <- "*****Select the PROJECTIONS data file*****\n    (The file picker window may have opened in the background.  Check behind this window if you do not see it.)\n"
cat("\n", prompt, "\n\n")
filename <- tcltk::tk_choose.files(caption = prompt,
                                   default = file.path(getwd(),
                                                       "output",
                                                       "30_projections.RData"),
                                   filter = matrix(c("RData", ".RData",
                                                     "CSV", ".csv",
                                                     "All files", ".*"),
                                                   3, 2, byrow = TRUE),
                                   multi = FALSE)
#load in the data based on the type of data file provided
if(grepl(x = filename, pattern = "\\.RData$"))
{
  load(file = filename)
  projection_directions <- projection
  
}else if(grepl(x = filename, pattern = "\\.(csv|CSV)$"))
{
  projection_directions <- read_csv(file = filename)
}else
{
  message("Invalid Data Filetype.")
  return
}

#read the CLEAN probability matrix CSV file
prompt <- "*****Select the CLEAN PROBABILITY MATRIX file*****\n    (The file picker window may have opened in the background.  Check behind this window if you do not see it.)\n"
cat("\n", prompt)
filename <- tcltk::tk_choose.files(caption = prompt,
                                   default = file.path(getwd(),
                                                       "output",
                                                       "10_passForwardData_CPM.RData"),
                                   filter = matrix(c("RData", ".RData",
                                                     "CSV", ".csv",
                                                     "All files", ".*"),
                                                   3, 2, byrow = TRUE),
                                   multi = FALSE)
#load in the data based on the type of data file provided
if(grepl(x = filename, pattern = "\\.RData$"))
{
  objs <- load(file = filename, verbose = T)
  probMatrix <- stu_LO_FV
  probMatrix <- rownames_to_column(probMatrix)
  names(probMatrix)[1] <- "userID"
  
}else if(grepl(x = filename, pattern = "\\.(csv|CSV)$"))
{
  probMatrix <- read_csv(file = filename)
  
}else
{
  message("Invalid Data Filetype.")
  break
}

##Number of students in course. Required for calculating percentage of students in profiles
n_learners <- length(probMatrix$userID)

##Take the number of best directions to consider for profiling
n_directions <- readline(prompt = "Please enter the number of best projection directions to consider for profiling : \n")
n_directions <- as.integer(n_directions)

##Extract the names of the best projection directions
minW_RandVec_sort <- rownames_to_column(minW_RandVec_sort)
names(minW_RandVec_sort)[1] <- "RV_id"

best_direction_names <- minW_RandVec_sort$RV_id[1:n_directions]
# best_direction_names <- gsub(x = best_direction_names, pattern = "RP1D", replacement = "RV")



##Form a dataFrame of n_directions columns and column = i is ith best projection direction
best_directions <- c()
# best_directions <- minW_RandVec_sort$RV_id[1:n_directions]
for(i in 1:n_directions)
{
  best_directions <- cbind(best_directions, 
                           projection_directions[, best_direction_names[i]])
}
best_directions <- as.data.frame(best_directions)
##Name columns using names of corresponding directions
colnames(best_directions) <- best_direction_names

##Form a list of W values for best projection directions
best_direction_thresholds <- minW_RandVec_sort[1:n_directions, "Group Threshold"]




#########updates looking good till here 2020.02.05 16:11:11. ######
#########
#########
#########

##Loop over learners and find cluster assignment along each best direction
cluster_assignments <- c()
for(i in 1:nrow(probMatrix))
{
  ##User parameter vector
  user_params <- as.matrix(probMatrix[i,])
  cluster <- c()
  for(j in 1:n_directions)
  {
    ##Calculate the projection value along direction j
    proj <- as.numeric(user_params %*% best_directions[,j])
    ##If proj > threshold, cluster = 2. Else, cluster = 1
    if(proj > best_direction_thresholds[j])
    {
      cluster <- c(cluster, 2)
    } else
    {
      cluster <- c(cluster, 1)
    }
  }
  ##Save cluster assignment of each learner by row
  cluster_assignments <- rbind(cluster_assignments, cluster)
}

##Convert cluster_assignment to dataFrame, name rows with corresponding user_id and columns with corresponding direction
cluster_assignments <- as.data.frame(cluster_assignments)
rownames(cluster_assignments) <- rownames(probMatrix)
colnames(cluster_assignments) <- paste0("cluster_", best_direction_names)

##Initialize a dataFrame for saving the profiles of every user
all_assignments <- c()
all_assignments$student_id <- rownames(cluster_assignments)
all_assignments$profile <- rep(NA, length(all_assignments$student_id))

##Convert cluster assignments to strings of length = n_directions. These strings are usage profiles
cluster_assignments_collapsed <- c()
for(i in 1:nrow(cluster_assignments))
{
  cluster_assignments_collapsed <- c(cluster_assignments_collapsed, 
                                     paste0(as.character(cluster_assignments[i, ]), collapse = ""))
}
##Convert all_assignments to dataFrame
all_assignments <- as.data.frame(all_assignments)
##Find set of all profiles observed in course
unique_assignments <- unique(unique_assignments)

##Now, count the number of times each profiles occurs...
unique_assignment_counts <- c()
temp <- c()
for(i in 1:length(unique_assignments))
{
  temp <- c(temp, sum(cluster_assignments_collapsed == unique_assignments[i]))
}

##Form a dataFrame containing profiles, number and percentage of learners in a profile 
unique_assignment_counts$Cluster.Assignment <- unique_assignments
unique_assignment_counts$Number.Students <- temp
##Calculate percentage of learners for each profile
unique_assignment_counts$Percentage.Students <- (temp / n_learners) * 100
##Convert list to dataFrame and sort profiles in decreasing order of Percentage.Students
unique_assignment_counts <- as.data.frame(unique_assignment_counts)
unique_assignment_counts <- unique_assignment_counts[order(unique_assignment_counts$Number.Students, decreasing = T), ]

##Save dataFrame of profiles and percentage students in each profile
write.csv(unique_assignment_counts, paste0(path_output, "profileDistribution_", course, ".csv"))


##Save dataFrame of cluster assignments of individual users
write.csv(cluster_assignments, paste0(path_output, "clusterAssignmentsAllUsers_", course, ".csv"))
write.csv(cluster_assignments_collapsed, paste0(path_output, "clusterAssignmentsAllUsersCollapsed_", course, ".csv"))


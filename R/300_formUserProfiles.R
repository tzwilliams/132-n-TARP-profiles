## ===================================================== ##
# Title:        Form User Profiles ####
# Project:      132 n-TARP Profiles
#               https://github.com/tzwilliams/
#               
# Authors:      Taylor Williams 
#
# Affiliation:  Purdue University
# 
# Description:  Identify and save the n-tarp profiles
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


##Read file with W values for projection vectors
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
  projection_values <- projection
  projection_values <- rownames_to_column(projection_values)
  names(projection_values)[1] <- "userID"
  
}else if(grepl(x = filename, pattern = "\\.(csv|CSV)$"))
{
  projection_values <- read_csv(file = filename)
  names(projection_values)[1] <- "userID"
  
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
  names(probMatrix)[1] <- "userID"
  
}else
{
  message("Invalid Data Filetype.")
  break
}

##Number of students in course. Required for calculating percentage of students in profiles
n_learners <- length(probMatrix$userID)

##Take the number of best vectors to consider for profiling
n_vectors <- readline(prompt = "Please enter the number of best projection vectors to consider for profiling : \n")
n_vectors <- as.integer(n_vectors)

##Extract the names of the best projection vectors
if(names(minW_RandVec_sort)[1] != "RV_id"){
  minW_RandVec_sort <- rownames_to_column(minW_RandVec_sort)
  names(minW_RandVec_sort)[1] <- "RV_id"
}
  
best_vector_names <- minW_RandVec_sort$RV_id[1:n_vectors]


##Form a dataFrame of n_vectors columns where column = i is all of the user projection values
proj_vals_for_criteria <- c()



for(i in 1:n_vectors)
{
  proj_vals_for_criteria <- cbind(proj_vals_for_criteria,
                                  projection_values[, best_vector_names[i]])
}

##Name columns and rows using names of corresponding vectors
proj_vals_for_criteria <- as.data.frame(proj_vals_for_criteria)
colnames(proj_vals_for_criteria) <- best_vector_names
rownames(proj_vals_for_criteria) <- probMatrix$userID

##Form a list of W values for best projection vectors
best_vector_thresholds <- minW_RandVec_sort[1:n_vectors, "Group Threshold"]





##Loop over learners and find cluster assignment along each best candidate vector
cluster_assignments <- c()
for(i in 1:n_learners)  #loop over all users
{
  ##User parameter vector (assuming column 1 is row name)
  user_cluster <- c()
  for(j in 1:n_vectors)
  {
    #del#Calculate the projection value along vector j
    #   proj <- as.numeric(user_params %*% best_vectors[,j])
     
    ##If proj >= threshold, user_cluster = 2. Else, user_cluster = 1
    if(proj_vals_for_criteria[i, best_vector_names[j]] >= best_vector_thresholds[j])
    {
      user_cluster <- c(user_cluster, 2)
    } else
    {
      user_cluster <- c(user_cluster, 1)
    }
  }
  ##Save cluster assignment of each learner by row
  cluster_assignments <- rbind(cluster_assignments, user_cluster)
}

##Convert cluster_assignment to dataFrame, name rows with corresponding user_id and columns with corresponding vector
cluster_assignments <- as.data.frame(cluster_assignments)
rownames(cluster_assignments) <- probMatrix$userID
colnames(cluster_assignments) <- paste0("cluster_", best_vector_names)




##Initialize a dataFrame for saving the profiles of every user
all_assignments <- c()
all_assignments$user_id <- rownames(cluster_assignments)
all_assignments$profile <- rep(NA, length(all_assignments$user_id))

##Convert cluster assignments to strings of length = n_vectors. These strings are usage profiles
cluster_assignments_collapsed <- c()
for(i in 1:nrow(cluster_assignments))
{
  cluster_assignments_collapsed <- c(cluster_assignments_collapsed,
                                     paste0(as.character(cluster_assignments[i, ]), collapse = ""))
}
##Convert all_assignments to dataFrame
all_assignments <- as.data.frame(all_assignments)
all_assignments$profile <- cluster_assignments_collapsed

##Find set of all profiles observed in course
unique_assignments <- unique(all_assignments$profile)



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
          x = all_assignments, row.names = F) 

#write to RData file
save(unique_assignment_counts, cluster_assignments, cluster_assignments_collapsed,
     file = file.path("output", paste0("300_profiles-", n_vectors ,"_criteria.RData")),
     precheck = TRUE, compress = TRUE)


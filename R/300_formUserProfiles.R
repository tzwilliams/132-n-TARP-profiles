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
# Input stack:
#               110v2_stuFeatureVector
#               40_minW_and_threshold.RData
#               30_projections.RData
#
# Package dependencies: 
#
# Changelog:
#     2021.07.2x.   commented out the file loading code--a bug with grepl is suspected.  Workaround: manually load the 3 RData files
#                   
# Feature wishlist:  (*: planned; /: started; x: complete)
#     [*] re-enable file loading
## ===================================================== ##




######### Clean the environment ########## 
# rm(list=ls())
# ## Clean the environment except required variables
# rm(list = setdiff(ls(), c("course", "path_files", "path_output", "probMatrix",
#                   

######### Internal functions ########## 



######### Setup ##########
#load required packages
require(tidyverse)
require(readxl)

#Load functions 
source(file.path(getwd(), "R", "functions", "DisplayPercentComplete.R"))
source(file.path(getwd(), "R", "functions", "file-structure-functions.R"))


######### Read Data ##########
# if(!exists("filenamePrefix")) filenamePrefix <- NULL
# if(!exists("dataFolderPath")) dataFolderPath <- NULL
# if(!exists("filenameFV")) filenameFV <- NULL
# 
# ## get data file locations from user
# #Locate the CLEAN probability matrix (feature vector) file
#   filenameFV <- 
#     SelectFile(prompt = "*****Select the CLEAN PROBABILITY MATRIX (feature vector) file (probably a `110_`)*****\n    (The file picker window may have opened in the background.  Check behind this window if you do not see it.)\n",
#              defaultFilename = "110v2_stuFeatureVector-",
#              # filenamePrefix = ifelse(exists("filenamePrefix") & !is.null(filenamePrefix),
#              #                         yes = filenamePrefix, no = ""),
#              fileTypeMatrix = matrix(c("RData", ".RData", "CSV", ".csv", "All files", ".*"),
#                                      3, 2, byrow = TRUE),
#              dataFolderPath = ifelse(exists("dataFolderPath") & !is.null(dataFolderPath),
#                                      yes = dataFolderPath, no = ""))
#   
# 
# 
# #load in the data based on the type of data file provided
# if(grepl(x = filenameFV, pattern = "\\.RData$")){
#   objs <- load(file = filenameFV, verbose = T)
  probMatrix <- stu_LO_FV
#   # probMatrix <- rownames_to_column(probMatrix)
#   # names(probMatrix)[1] <- "User ID"
# }else if(grepl(x = filenameFV, pattern = "\\.(csv|CSV)$")){
#   probMatrix <- read_csv(file = filenameFV)
#   names(probMatrix)[1] <- "User ID"
# }else {
#   message("Invalid Data Filetype.")
#   break
# }
# 
# 
# 
# 
# #read the MIN_W and THRESHOLD data file
# filenameMinW <-
#   SelectFile(prompt = "*****Select the MIN_W and THRESHOLD data file (probably a `40_`)*****\n    (The file picker window may have opened in the background.  Check behind this window if you do not see it.)\n",
#              defaultFilename = "40_minW_and_threshold.RData",
#              # filenamePrefix = ifelse(exists("filenamePrefix") & !is.null(filenamePrefix),
#              #                         yes = filenamePrefix, no = ""),
#              fileTypeMatrix = matrix(c("RData", ".RData", "CSV", ".csv", "All files", ".*"),
#                                      3, 2, byrow = TRUE),
#              dataFolderPath = ifelse(exists("dataFolderPath") & !is.null(dataFolderPath),
#                                      yes = dataFolderPath, no = ""))
# 
# 
# #load in the data based on the type of data file provided
# if(grepl(x = filenameMinW, pattern = "\\.RData$")){
#   load(file = filenameMinW)
# }else if(grepl(x = filenameMinW, pattern = "\\.(csv|CSV)$")){
#   minW_RandVec_sort <- read_csv(file = filenameMinW)
# }else{
#   message("Invalid Data Filetype.")
#   break
# }
# 
# 
# 
# 
# #read the PROJECTIONS data file
# filenameProj <-
#   SelectFile(prompt = "*****Select the PROJECTIONS data file (probably a `30_`)*****\n    (The file picker window may have opened in the background.  Check behind this window if you do not see it.)\n",
#              defaultFilename = "30_projections.RData",
#              # filenamePrefix = ifelse(exists("filenamePrefix") & !is.null(filenamePrefix),
#              #                         yes = filenamePrefix, no = ""),
#              fileTypeMatrix = matrix(c("RData", ".RData", "CSV", ".csv", "All files", ".*"),
#                                      3, 2, byrow = TRUE),
#              dataFolderPath = ifelse(exists("dataFolderPath") & !is.null(dataFolderPath),
#                                      yes = dataFolderPath, no = ""))
# 
# 
# #load in the data based on the type of data file provided
# if(grepl(x = filenameProj, pattern = "\\.RData$")){
#   load(file = filenameProj)
  projection_values <- projection
  projection_values <- rownames_to_column(projection_values)
  names(projection_values)[1] <- "User ID"
# rm(projection)
# # }else if(grepl(x = filenameProj, pattern = "\\.(csv|CSV)$")){
# #   projection_values <- read_csv(file = filenameProj)
# #   names(projection_values)[1] <- "User ID"
# }else{
#   message("Invalid Data Filetype.")
#   break
# }





######### Main ##########

##count number of students  ####
  #(Required for calculating percentage of students in profiles)
n_learners <- length(probMatrix$`User ID`)

##Request number of best vectors to include in the profiles ####
# n_vectors <- 10 #default
n_vectors <- readline(prompt = "Please enter how many projection vectors you want to include in the profiles: \n")
n_vectors <- as.integer(n_vectors)

##Extract the names of the top projection vectors ####
if(names(minW_RandVec_sort)[1] != "RV_id"){
  minW_RandVec_sort <- rownames_to_column(minW_RandVec_sort)
  names(minW_RandVec_sort)[1] <- "RV_id"
}
  
best_vector_names <- minW_RandVec_sort$RV_id[1:n_vectors]


## Extract user's projection values each top projection vector ####
# dataFrame of n_vectors columns where column = i is the user's projection values
proj_vals_for_criteria <- c()
for(i in 1:n_vectors)
{
  proj_vals_for_criteria <- cbind(proj_vals_for_criteria,
                                  projection_values[, best_vector_names[i]])
}

#Name columns and rows using names of corresponding vectors
proj_vals_for_criteria <- as.data.frame(proj_vals_for_criteria)
colnames(proj_vals_for_criteria) <- best_vector_names
rownames(proj_vals_for_criteria) <- probMatrix$`User ID`


##list of group separation thresholds for the best projection vectors ####
best_vector_thresholds <- minW_RandVec_sort[1:n_vectors, "Group Threshold"]

best_vector_thresholds <- as.data.frame(best_vector_thresholds)
rownames(best_vector_thresholds) <- best_vector_names



##Find cluster assignment for each user (for each best candidate vector)  ####
cluster_assignments <- c()
for(i in 1:n_learners)  #loop over all users
{
  ##User parameter vector (assuming column 1 is row name)
  user_cluster <- c()  #initialize vector (or erase last user's values)
  for(j in 1:n_vectors)
  {
    #del#Calculate the projection value along vector j
    #   proj <- as.numeric(user_params %*% best_vectors[,j])
     
    ##If proj >= threshold, user_cluster = 2. Else, user_cluster = 1
    if(proj_vals_for_criteria[i, best_vector_names[j]] >= best_vector_thresholds[j, 1])
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

## Convert cluster_assignment to dataFrame, name rows with corresponding user_id and columns with corresponding vector
cluster_assignments <- as.data.frame(cluster_assignments)
rownames(cluster_assignments) <- probMatrix$`User ID`
colnames(cluster_assignments) <- paste0("cluster_", best_vector_names)




## Save the profiles of each user ####
### Initialize a dataFrame
all_assignments <- c()
all_assignments$`User ID` <- rownames(cluster_assignments)
all_assignments$profile <- rep(NA, length(all_assignments$`User ID`))

## Convert cluster assignments to strings of length = n_vectors. These strings are the user profiles
cluster_assignments_collapsed <- c()
for(i in 1:nrow(cluster_assignments))
{
  cluster_assignments_collapsed <- c(cluster_assignments_collapsed,
                                     paste0(as.character(cluster_assignments[i, ]), collapse = ""))
}

##Convert all_assignments to dataFrame
all_assignments <- as.data.frame(all_assignments)
all_assignments$profile <- cluster_assignments_collapsed
cluster_assignments_collapsed <- all_assignments



## profile stats  ####
##Find set of all profiles observed in the data
unique_assignments <- unique(all_assignments$profile)



##Now, count the number of times each profiles occurs...
unique_assignment_counts <- c()
temp <- c()
for(i in 1:length(unique_assignments))
{
  temp <- c(temp, sum(cluster_assignments_collapsed == unique_assignments[i]))
}



##Form a variable storing profiles, number, and percentage of learners in a profile
unique_assignment_counts$Cluster.Assignment <- unique_assignments
unique_assignment_counts$Number.Students <- temp
##Calculate percentage of learners for each profile
unique_assignment_counts$Percentage.Students <- (temp / n_learners)
##Convert list to dataFrame and sort profiles in decreasing order of Percentage.Students
unique_assignment_counts <- as.data.frame(unique_assignment_counts)
unique_assignment_counts <- 
  unique_assignment_counts[order(unique_assignment_counts$Number.Students, decreasing = T), ]



##Assign short name to profiles according to profile size
unique_assignment_counts <- 
  unique_assignment_counts %>% add_column(short_name = NA)

for (i in 1:nrow(unique_assignment_counts)) {
  unique_assignment_counts$short_name[i] <- paste0("P", i)
}

##Add short profile name to student profile assignment ####
all_assignments <- add_column(all_assignments, profile_name = NA)

for (i in 1:nrow(all_assignments)) {
  all_assignments$profile_name[i] <- 
    unique_assignment_counts$short_name[ unique_assignment_counts$Cluster.Assignment == 
                                           all_assignments$profile[i] ]
}



######### Save data to file #########
message("\nSaving files.\n")

#write to CSV file
##Save dataFrame of profiles and percentage students in each profile
write_csv(file = file.path("output", paste0("300_profileDistribution-", n_vectors ,"_criteria.csv")),
          x = unique_assignment_counts, col_names = T)


##Save dataFrame of cluster assignments of individual users
write.csv(file = file.path("output", paste0("300_clusterAssignmentsAllUsersExpanded-", n_vectors ,"_criteria.csv")),
          x = cluster_assignments, row.names = T)
write_csv(file = file.path("output", paste0("300_clusterAssignmentsAllUsersCollapsed-", n_vectors ,"_criteria.csv")),
          x = all_assignments, col_names = T)

#write to RData file
save(unique_assignment_counts, cluster_assignments, all_assignments, 
     file = file.path("output", paste0("300_profiles-", n_vectors ,"_criteria.RData")),
     precheck = TRUE, compress = TRUE)

